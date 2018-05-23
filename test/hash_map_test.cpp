
#include <robin_hood/hash_map.hpp>

#include <unordered_map>
#include <vector>
#include <chrono>
#include <random>
#include <sstream>
#include <iostream>

#if __has_include(<cxxabi.h>)
#	include <cxxabi.h>
static thread_local int robin_hood_demangle_status;
#	define ROBIN_HOOD_TEST_DEMANGLE(...) (abi::__cxa_demangle(__VA_ARGS__, 0, 0, &robin_hood_demangle_status))
#else
#	define ROBIN_HOOD_TEST_DEMANGLE(...) (__VA_ARGS__)
#endif

using default_random = std::default_random_engine;

const char *const indent = "  ";

struct assertion_failure : std::runtime_error {
	using _base_type = std::runtime_error;
	using _base_type::_base_type;
};

#ifdef assert
#	undef assert
#endif
static constexpr bool stop_on_first_failure = true;
static std::vector<std::pair<assertion_failure, std::string>> failed_assertions{};
static std::size_t assertions_passed = 0;
#define assert(C) do { if (!(C)) throw assertion_failure("'" #C "'"); ++assertions_passed; } while (false)

template <class Map0, class Map1>
void assert_submap(const Map0& map0, const Map1& map1)
{
	for (const auto& pair : map0)
	{
		auto it = map1.find(pair.first);
		assert(it != map1.end() &&
			"key in a submap is in a supermap");
		assert(it->second == pair.second &&
			"value in a submap equals the value in a supermap");
	}
}

template <class Map0, class Map1>
void assert_maps_equivalent(const Map0& map0, const Map1& map1)
{
	assert(map0.size() == map1.size() &&
		"equivalent maps have equal sizes");
	assert_submap(map0, map1);
	assert_submap(map1, map0);
}

// This wrapper type makes a type non-copyable because that's a use case we would like to support
template <class T>
struct move_only
{
	T _value;
	/*implicit*/ move_only(T value) : _value(std::move(value)) {}
	move_only() : _value{} {}
	move_only(move_only&& other) = default;
	move_only(const move_only& other) = delete;

	operator const T&() const { return _value; }
	operator T&() & { return _value; }
	operator T&&() && { return std::move(_value); }

	move_only& operator =(const move_only& other) = delete;
	move_only& operator =(move_only&& other) = default;

	auto operator ==(const move_only& other) const
	{ return _value == other._value; }

	// Support comparison with underlying (potentially copyable) type
	friend auto operator ==(const move_only& lhs, const T& rhs)
	{ return lhs._value == rhs; }
	friend auto operator ==(const T& lhs, const move_only& rhs)
	{ return lhs == rhs._value; }

	// Support streaming for debugging purposes
	template <class Char, class Traits>
	friend decltype(auto) operator<<(std::basic_ostream<Char, Traits>& s, const move_only& self) {
		return s << self._value;
	}
};

namespace std
{
	template <class T>
	struct hash<move_only<T>>
	{
		std::hash<T> _hash;
		auto operator()(const move_only<T>& value) const
		{ return _hash(value._value); }
	};
}

// We need a way to remove this non-copyable wrapper because the STL types we use as references assume copyability
template <class T> struct unwrap_move_only_result { using type = T; };
template <class T> struct unwrap_move_only_result<move_only<T>> { using type = T; };
template <class T> using unwrap_move_only_result_t = typename unwrap_move_only_result<T>::type;

template <class Key, class Value, class Hash, class Test, class... Args>
void test_map_operations(const Test& test, const Args&... args)
{
	std::ostringstream test_name_stream;
	test_name_stream << ROBIN_HOOD_TEST_DEMANGLE(typeid(Test).name()) << " with [\n" <<
		indent << "Key = " << ROBIN_HOOD_TEST_DEMANGLE(typeid(Key).name()) << ",\n" <<
		indent << "Value = " << ROBIN_HOOD_TEST_DEMANGLE(typeid(Value).name()) << ",\n" <<
		indent << "Hash = " << ROBIN_HOOD_TEST_DEMANGLE(typeid(Hash).name()) << "]";
	std::string test_name = test_name_stream.str();
	std::clog << "Running test " << test_name << std::endl;

	// The approach we take to testing an operation is simple:
	// we perform the same operation on a reference implementation
	// (std::unordered_map<...> in this case) and verify that
	// the resulting maps are equivalent.  The only difference is
	// that because std::unordered_map does not support move-only
	// types, we have unwrap those in the reference case.

	std::unordered_map<unwrap_move_only_result_t<Key>, unwrap_move_only_result_t<Value>, Hash> ref;
	test(ref, args...);

	try {
		robin_hood::hash_map<Key, Value, Hash> can;
		test(can, args...);
		assert_maps_equivalent(ref, can);
	} catch (assertion_failure failure) {
		if (stop_on_first_failure)
			throw;
		failed_assertions.emplace_back(failure, test_name);
	} catch (...) {
		if (stop_on_first_failure)
			throw;
		failed_assertions.emplace_back(assertion_failure("unexpected exception thrown"), test_name);
	}
}

template <class Clock = std::chrono::high_resolution_clock, class F>
auto time_ms(const F& f)
{
	using namespace std::chrono;

	typename Clock::time_point stop, start = Clock::now();
	std::size_t executions = 0;
	do {
		f();
		++executions;
	} while ((stop = Clock::now()) - start < seconds(1));

	using duration_type = duration<double, std::milli>;
	return duration_cast<duration_type>(stop - start).count() / executions;
}

template <class Key = int, class Value = int, class Hash, class Test, class... Args>
void test_map_performance(const Test& test, const Args&... args)
{
	std::ostringstream test_name_stream;
	test_name_stream << ROBIN_HOOD_TEST_DEMANGLE(typeid(Test).name()) << " with [\n" <<
		indent << "Key = " << ROBIN_HOOD_TEST_DEMANGLE(typeid(Key).name()) << ",\n" <<
		indent << "Value = " << ROBIN_HOOD_TEST_DEMANGLE(typeid(Value).name()) << ",\n" <<
		indent << "Hash = " << ROBIN_HOOD_TEST_DEMANGLE(typeid(Hash).name()) << "]";
	std::string test_name = test_name_stream.str();
	std::clog << "Running test " << test_name << std::endl;

	// Note that there is no exception handling here, because it's pointless to test performance before correctness.

	std::unordered_map<Key, Value, Hash> ref;
	auto ref_ms = time_ms([&]{
		test(ref, args...);
	});

	robin_hood::hash_map<Key, Value, Hash> can;
	auto can_ms = time_ms([&]{
		test(can, args...);
	});

	// Sanity check
	assert_maps_equivalent(ref, can);

	if (can_ms > ref_ms) {
		std::stringstream description;
		description << "Performance regression of " <<
			static_cast<int>(round(100 * (can_ms / ref_ms - 1))) << '%' <<
			" (" << can_ms << " ms" << " / " << ref_ms << " ms" << ")";
		std::clog << description.str() << "\n";
		failed_assertions.emplace_back(assertion_failure(description.str()), test_name);
	} else {
		std::stringstream description;
		description << "Speedup of " <<
			static_cast<int>(round(100 * (ref_ms / can_ms))) << '%' <<
			" (" << ref_ms << " ms" << " / " << can_ms << " ms" << ")";
		std::clog << description.str() << "\n";
	}
	std::clog << std::endl;
}

struct empty_map_test
{
	template <class Map>
	void operator()(Map& map) const
	{
		assert(map.empty() &&
			"default-constructed map is empty");
	}
};

template <class Random = default_random>
struct insert_map_test
{
	template <class Map, class Seed, class KeyDistribution, class ValueDistribution>
	void operator()(Map& map, std::size_t count, const Seed& seed, KeyDistribution kd, ValueDistribution vd) const
	{
		Random random(seed ^ typeid(*this).hash_code());
		for (std::size_t i = 0; i < count; ++i)
		{
			auto key = kd(random);
			auto value = vd(random);
			map.insert(std::make_pair(key, value));
		}
	}
};

template <class Random = default_random>
struct emplace_map_test
{
	template <class Map, class Seed, class KeyDistribution, class ValueDistribution>
	void operator()(Map& map, std::size_t count, const Seed& seed, KeyDistribution kd, ValueDistribution vd) const
	{
		Random random(seed ^ typeid(*this).hash_code());
		for (std::size_t i = 0; i < count; ++i)
		{
			auto key = kd(random);
			auto value = vd(random);
			map.emplace(std::move(key), std::move(value));
		}
	}
};

template <class Random = default_random>
struct find_map_test
{
	emplace_map_test<Random> generator{};
	template <class Map, class Seed, class KeyDistribution, class ValueDistribution>
	void operator()(Map& map, std::size_t count, const Seed& seed, KeyDistribution kd, ValueDistribution vd) const
	{
		generator(map, count, seed, kd, vd);

		Random random(seed ^ typeid(*this).hash_code());
		for (std::size_t i = 0; i < count; ++i)
		{
			auto key = kd(random);
			auto it = map.find(key);
			assert((it == map.end() || it->first == key) &&
				"finding a key either fails or finds the key");
		}
	}
};

template <class Random = default_random>
struct move_assign_map_test
{
	emplace_map_test<Random> generator{};
	template <class Map, class Seed, class KeyDistribution, class ValueDistribution>
	void operator()(Map& map, std::size_t count, const Seed& seed, KeyDistribution kd, ValueDistribution vd) const
	{
		Map other;
		generator(other, count, seed, kd, vd);
		map = std::move(other);
	}
};

template <class Random = default_random>
struct copy_assign_map_test
{
	emplace_map_test<Random> generator{};
	template <class Map, class Seed, class KeyDistribution, class ValueDistribution>
	void operator()(Map& map, std::size_t count, const Seed& seed, KeyDistribution kd, ValueDistribution vd) const
	{
		Map other;
		generator(other, count, seed, kd, vd);
		map = other;
	}
};

template <class Random = default_random>
struct swap_map_test
{
	emplace_map_test<Random> generator{};
	template <class Map, class Seed, class KeyDistribution, class ValueDistribution>
	void operator()(Map& map, std::size_t count, const Seed& seed, KeyDistribution kd, ValueDistribution vd) const
	{
		Map other;
		generator(other, count, seed, kd, vd);
		swap(map, other);
	}
};

template <class Random = default_random>
struct erase_map_test
{
	emplace_map_test<Random> generator{};
	template <class Map, class Seed, class KeyDistribution, class ValueDistribution>
	void operator()(Map& map, std::size_t count, const Seed& seed, KeyDistribution kd, ValueDistribution vd) const
	{
		Random random(seed ^ typeid(*this).hash_code());
		generator(map, count, seed, kd, vd);
		for (std::size_t i = 0; i < count; ++i)
			map.erase(kd(random));
	}
};

template <class Random = default_random>
struct erase_all_map_test
{
	emplace_map_test<Random> generator{};
	template <class Map, class Seed, class KeyDistribution, class ValueDistribution>
	void operator()(Map& map, std::size_t count, const Seed& seed, KeyDistribution kd, ValueDistribution vd) const
	{
		generator(map, count, seed, kd, vd);
		for (auto it = map.begin(); it != map.end();)
			it = map.erase(it);
	}
};

template <class Random = default_random>
struct clear_map_test
{
	emplace_map_test<Random> generator{};
	template <class Map, class Seed, class KeyDistribution, class ValueDistribution>
	void operator()(Map& map, std::size_t count, const Seed& seed, KeyDistribution kd, ValueDistribution vd) const
	{
		generator(map, count, seed, kd, vd);
		map.clear();
	}
};

template <class Random = default_random>
struct move_construct_map_test
{
	erase_map_test<Random> generator{};
	template <class Map, class Seed, class KeyDistribution, class ValueDistribution>
	void operator()(Map& map, std::size_t count, const Seed& seed, KeyDistribution kd, ValueDistribution vd) const
	{
		generator(map, count, seed, kd, vd);
		auto other = std::move(map);
		map.clear();
		map = std::move(other);
	}
};

template <class Random = default_random>
struct copy_construct_map_test
{
	erase_map_test<Random> generator{};
	template <class Map, class Seed, class KeyDistribution, class ValueDistribution>
	void operator()(Map& map, std::size_t count, const Seed& seed, KeyDistribution kd, ValueDistribution vd) const
	{
		generator(map, count, seed, kd, vd);
		auto other = map;
		map.clear();
		map = std::move(other);
	}
};

template <class Random = default_random>
struct equal_map_test
{
	erase_map_test<Random> generator{};
	template <class Map, class Seed, class KeyDistribution, class ValueDistribution>
	void operator()(Map& map, std::size_t count, const Seed& seed, KeyDistribution kd, ValueDistribution vd) const
	{
		Random random(seed ^ typeid(*this).hash_code());

		generator(map, count, seed, kd, vd);
		auto other = map;
		assert(map == other && other == map &&
			"copy-constructed maps are equal");

		std::uniform_int_distribution<std::size_t> id{0, map.size() - 1};

		// Test changing a value
		other = map;
		if (!other.empty())
		{
			auto it = other.begin();
			std::advance(it, id(random));
			auto previous_value = it->second;
			do it->second = vd(random);
			while (it->second == previous_value);
			assert(map != other && other != map &&
				"maps are unequal after changing a value in one map");
		}

		// Test erasing a value
		other = map;
		if (!other.empty())
		{
			auto it = other.begin();
			std::advance(it, id(random));
			other.erase(it);
			assert(map != other && other != map &&
				"maps are unequal after erasing a value in one map");
		}

		// Test inserting a value
		other = map;
		auto key = kd(random);
		if (other.find(key) == other.end())
		{
			other.emplace(key, vd(random));
			assert(map != other && other != map &&
				"maps are unequal after inserting a value in one map");
		}
	}
};

template <class Distribution>
struct move_only_distribution
{
	Distribution _distribution;

	using result_type = move_only<typename Distribution::result_type>;

	template <class... Args>
	explicit move_only_distribution(Args&&... args) :
		_distribution(std::forward<Args>(args)...)
	{}

	//move_only_distribution(const move_only_distribution&) = default;
	move_only_distribution(const move_only_distribution& other) :
		move_only_distribution(other._distribution)
	{}
	//move_only_distribution(move_only_distribution&&) = default;
	move_only_distribution(move_only_distribution&& other) :
		move_only_distribution(std::move(other._distribution))
	{}

	template <class Random>
	result_type operator()(Random& random)
	{ return _distribution(random); }
};

template <class Distribution>
auto make_move_only_distribution(const Distribution& d)
{
	return move_only_distribution<Distribution>(d);
}

template <class T>
struct worst_possible_hash
{
	std::size_t operator()(const T&) const
	{
		return 0;
	}
};

struct test_string_distribution
{
	using result_type = std::string;

	template <class Random>
	result_type operator()(Random& random)
	{ return std::to_string(random()); }
};

template <template <class> class Hash, class KeyDistribution, class ValueDistribution>
void test_move_only_map_operations(std::size_t count, std::size_t seed, const KeyDistribution& kd, const ValueDistribution& vd)
{
	using key_type = typename KeyDistribution::result_type;
	using value_type = typename ValueDistribution::result_type;
	using hasher = Hash<key_type>;

	test_map_operations<key_type, value_type, hasher>(empty_map_test{});
	test_map_operations<key_type, value_type, hasher>(emplace_map_test<>{}, count, seed, kd, vd);
	test_map_operations<key_type, value_type, hasher>(find_map_test<>{}, count, seed, kd, vd);
	test_map_operations<key_type, value_type, hasher>(move_assign_map_test<>{}, count, seed, kd, vd);
	test_map_operations<key_type, value_type, hasher>(swap_map_test<>{}, count, seed, kd, vd);
	test_map_operations<key_type, value_type, hasher>(erase_map_test<>{}, count, seed, kd, vd);
	test_map_operations<key_type, value_type, hasher>(clear_map_test<>{}, count, seed, kd, vd);
	test_map_operations<key_type, value_type, hasher>(move_construct_map_test<>{}, count, seed, kd, vd);
}

template <template <class> class Hash, class KeyDistribution, class ValueDistribution>
void test_all_map_operations(std::size_t count, std::size_t seed, const KeyDistribution& kd, const ValueDistribution& vd)
{
	using key_type = typename KeyDistribution::result_type;
	using value_type = typename ValueDistribution::result_type;
	using hasher = Hash<key_type>;

	test_move_only_map_operations<Hash>(count, seed,
		make_move_only_distribution(kd),
		make_move_only_distribution(vd));
	//test_move_only_map_operations<Hash>(count, seed, kd, vd);

	test_map_operations<key_type, value_type, hasher>(insert_map_test<>{}, count, seed, kd, vd);
	test_map_operations<key_type, value_type, hasher>(copy_assign_map_test<>{}, count, seed, kd, vd);
	test_map_operations<key_type, value_type, hasher>(erase_all_map_test<>{}, count, seed, kd, vd);
	test_map_operations<key_type, value_type, hasher>(copy_construct_map_test<>{}, count, seed, kd, vd);
	test_map_operations<key_type, value_type, hasher>(equal_map_test<>{}, count, seed, kd, vd);
}

template <template <class> class Hash, class Key, class ValueDistribution>
void test_all_map_operations_with_integral_key(std::size_t count, std::size_t seed, const ValueDistribution& vd)
{
	test_all_map_operations<Hash>(count, seed,
		std::uniform_int_distribution<Key>{}, vd);
}

template <template <class> class Hash, class Key, class ValueDistribution>
void test_all_map_operations_with_real_key(std::size_t count, std::size_t seed, const ValueDistribution& vd)
{
	test_all_map_operations<Hash>(count, seed,
		std::uniform_real_distribution<Key>{}, vd);
}

template <template <class> class Hash, class ValueDistribution>
void test_all_map_operations_with_all_key_types(std::size_t count, std::size_t seed, const ValueDistribution& vd)
{
	test_all_map_operations_with_integral_key<Hash, signed char>(count, seed, vd);
	test_all_map_operations_with_integral_key<Hash, unsigned char>(count, seed, vd);
	//test_all_map_operations_with_integral_key<Hash, signed short>(count, seed, vd);
	//test_all_map_operations_with_integral_key<Hash, unsigned short>(count, seed, vd);
	//test_all_map_operations_with_integral_key<Hash, signed int>(count, seed, vd);
	//test_all_map_operations_with_integral_key<Hash, unsigned int>(count, seed, vd);
	//test_all_map_operations_with_integral_key<Hash, signed long>(count, seed, vd);
	//test_all_map_operations_with_integral_key<Hash, unsigned long>(count, seed, vd);
	//test_all_map_operations_with_integral_key<Hash, signed long long>(count, seed, vd);
	test_all_map_operations_with_integral_key<Hash, unsigned long long>(count, seed, vd);

	test_all_map_operations_with_real_key<Hash, float>(count, seed, vd);
	//test_all_map_operations_with_real_key<Hash, double>(count, seed, vd);
	//test_all_map_operations_with_real_key<Hash, long double>(count, seed, vd);

	test_all_map_operations<Hash>(count, seed, test_string_distribution{}, vd);
}

template <template <class> class Hash, class Value>
void test_all_map_operations_with_all_key_types_and_integral_values(std::size_t count, std::size_t seed)
{
	test_all_map_operations_with_all_key_types<Hash>(count, seed,
		std::uniform_int_distribution<Value>{});
}

template <template <class> class Hash, class Value>
void test_all_map_operations_with_all_key_types_and_real_values(std::size_t count, std::size_t seed)
{
	test_all_map_operations_with_all_key_types<Hash>(count, seed,
		std::uniform_real_distribution<Value>{});
}


template <template <class> class Hash = std::hash>
void test_all_map_operations_with_fundamental_types(std::size_t count, std::size_t seed)
{
	test_all_map_operations_with_all_key_types_and_integral_values<Hash, signed char>(count, seed);
	test_all_map_operations_with_all_key_types_and_integral_values<Hash, unsigned char>(count, seed);
	//test_all_map_operations_with_all_key_types_and_integral_values<Hash, signed short>(count, seed);
	//test_all_map_operations_with_all_key_types_and_integral_values<Hash, unsigned short>(count, seed);
	//test_all_map_operations_with_all_key_types_and_integral_values<Hash, signed int>(count, seed);
	//test_all_map_operations_with_all_key_types_and_integral_values<Hash, unsigned int>(count, seed);
	//test_all_map_operations_with_all_key_types_and_integral_values<Hash, signed long>(count, seed);
	//test_all_map_operations_with_all_key_types_and_integral_values<Hash, unsigned long>(count, seed);
	//test_all_map_operations_with_all_key_types_and_integral_values<Hash, signed long long>(count, seed);
	test_all_map_operations_with_all_key_types_and_integral_values<Hash, unsigned long long>(count, seed);

	test_all_map_operations_with_all_key_types_and_real_values<Hash, float>(count, seed);
//	test_all_map_operations_with_all_key_types_and_real_values<Hash, double>(count, seed);
//	test_all_map_operations_with_all_key_types_and_real_values<Hash, long double>(count, seed);

	test_all_map_operations_with_all_key_types<Hash>(count, seed, test_string_distribution{});
}

template <template <class> class Hash, class Test>
void test_performance_with_all_types(const Test& test, std::size_t count, std::size_t seed)
{
	// We don't care about the char key case because one should just
	// use an array at that point, but anything larger is fair game.
	test_map_performance<signed short, unsigned char, Hash<char>>(test, count, seed,
		std::uniform_int_distribution<char>{},
		std::uniform_int_distribution<unsigned char>{});
	test_map_performance<int, unsigned char, Hash<int>>(test, count, seed,
		std::uniform_int_distribution<int>{},
		std::uniform_int_distribution<unsigned char>{});
	test_map_performance<int, std::string, Hash<int>>(test, count, seed,
		std::uniform_int_distribution<int>{},
		test_string_distribution{});
	test_map_performance<std::string, unsigned char, Hash<std::string>>(test, count, seed,
		test_string_distribution{},
		std::uniform_int_distribution<unsigned char>{});
	test_map_performance<std::string, std::string, Hash<std::string>>(test, count, seed,
		test_string_distribution{},
		test_string_distribution{});
}
template <template <class> class Hash = std::hash>
void test_performance_of_all_operations_with_all_types(std::size_t count, std::size_t seed)
{
	test_performance_with_all_types<Hash>(insert_map_test<>{}, count, seed);
	test_performance_with_all_types<Hash>(emplace_map_test<>{}, count, seed);
	test_performance_with_all_types<Hash>(find_map_test<>{}, count, seed);
	test_performance_with_all_types<Hash>(erase_map_test<>{}, count, seed);
	test_performance_with_all_types<Hash>(erase_all_map_test<>{}, count, seed);
}

int main(int argc, char *argv[])
{
	std::size_t seed = 0;

#ifndef NDEBUG
	// Test correctness...
	// ... with std::hash
	for (auto count : {16, 256, 65536})
		for (std::size_t i = 0; i < 5; ++i)
			test_all_map_operations_with_fundamental_types(count, ++seed);
	// ... with the worst possible hash function
	for (auto count : {16, 256})
		for (std::size_t i = 0; i < 5; ++i)
			test_all_map_operations_with_fundamental_types<worst_possible_hash>(count, ++seed);
#else
	// Test performance (only with a reasonable hash function)
	test_performance_of_all_operations_with_all_types<std::hash>(1000000, seed++);
#endif

	// Check for failures
	if (failed_assertions.empty())
		std::clog << "All " << assertions_passed << " assertions passed!" << std::endl;
	else
		std::cerr << std::endl;
	for (const auto& failure : failed_assertions)
		std::cerr << "FAILURE: " << failure.first.what() << " in " << failure.second << std::endl;

	return 0;
}
