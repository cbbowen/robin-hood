
#ifndef ROBIN_HOOD_HASH_MAP
#define ROBIN_HOOD_HASH_MAP

#include <functional>
#include <memory>

#include "hash_table.hpp"

namespace robin_hood {

namespace detail
{
	struct first_f
	{
		template <class Key, class Value>
		Key operator()(std::pair<Key, Value>&& p) const
		{ return std::move(p.first); }

		template <class Key, class Value>
		const Key& operator()(const std::pair<Key, Value>& p) const
		{ return p.first; }
	};

	template <class Key, class Value>
	struct key_of_unconstructed<first_f, std::pair<Key, Value>,
		const Key&, const Value&>
	{
		static const Key& get(const first_f&, const Key& key, const Value&)
		{
			return key;
		}
	};

	template <class Key, class Value, class... KArgs, class... VArgs>
	struct key_of_unconstructed<first_f, std::pair<Key, Value>, const std::piecewise_construct_t&,
		const std::tuple<KArgs&&...>&, const std::tuple<VArgs&&...>&>
	{
		static decltype(auto) get(const first_f&, const std::piecewise_construct_t&,
			const std::tuple<KArgs&&...>& key_args, const std::tuple<VArgs&&...>&)
		{
			return std::apply([](auto&&... args) -> decltype(auto) {
				// Conceptually, this should be equivalent to
				// return Key(std::forward<KArgs>(args)...);
				// but we may also be able to avoid constructing the key
				using inner_unconstructed_key = key_of_unconstructed<identity_f, Key, const KArgs&...>;
				return inner_unconstructed_key::get(identity_f{}, std::forward<KArgs>(args)...);
			}, key_args);
		}
	};
}

// hash_map differs from unordered_map in one rather important way.
// `value_type` is `std::pair<K, T>` rather than `std::pair<const K, T>`.
// There are a few reasons for this, but the primary one is correctness.
// Without this, we can't support move-only types without using
// unions in a way that is technically undefined behavior (see
// http://llvm.org/svn/llvm-project/libcxx/trunk/include/unordered_map)
// For the most part, this interface change shouldn't be a problem, but
// it means we have to add an extra restriction to the use of the hash_map:
// The key cannot be changed in a way that affects equality.  Really, this
// is a restriction also imposed by std::unorderd_map, but is built into
// the interface type explicitly there.
template <class Key, class T,
	class Hash = std::hash<Key>,
	class KeyEqual = std::equal_to<Key>,
	class Alloc = std::allocator<std::pair<Key, T>>>
class hash_map :
	public hash_table<std::pair<Key, T>, detail::first_f,
		Hash, KeyEqual, Alloc>
{
	using base_type = hash_table<std::pair<Key, T>, detail::first_f, Hash, KeyEqual, Alloc>;

public:
	using key_type = Key;
	using mapped_type = T;
	using value_type = typename base_type::value_type;
	using size_type = typename base_type::size_type;
	using difference_type = typename base_type::difference_type;
	using hasher = typename base_type::hasher;
	using key_equal = typename base_type::key_equal;
	using allocator_type = typename base_type::allocator_type;
	using const_reference = typename base_type::const_reference;
	using reference = typename base_type::reference;
	using const_pointer = typename base_type::const_pointer;
	using pointer = typename base_type::pointer;
	using const_iterator = typename base_type::const_iterator;
	using iterator = typename base_type::iterator;

	// TODO: Implement constructors (http://en.cppreference.com/w/cpp/container/unordered_map/unordered_map)

	const mapped_type& at(const key_type& key) const
	{
		auto it = this->find(key);
		if (it == this->end())
			throw std::out_of_range("key not found");
		return it->second;
	}

	mapped_type& at(const key_type& key)
	{
		auto it = this->find(key);
		if (it == this->end())
			throw std::out_of_range("key not found");
		return it->second;
	}

	mapped_type& operator[](const key_type& key)
	{
		return this->emplace(key, std::piecewise_construct,
			std::forward_as_tuple(key), std::forward_as_tuple())->second;
	}

	mapped_type& operator[](key_type&& key)
	{
		return this->emplace(key, std::piecewise_construct,
			std::forward_as_tuple(std::move(key)), std::forward_as_tuple())->second;
	}

	size_type count(const key_type& key) const
	{
		return this->find(key) != this->end() ? 1 : 0;
	}

	std::pair<const_iterator, const_iterator> equal_range(const key_type& key) const
	{
		auto first = this->find(key),
			last = first;
		if (last != this->end())
			++last;
		return {first, last};
	}

	std::pair<iterator, iterator> equal_range(const key_type& key)
	{
		auto first = this->find(key),
			last = first;
		if (last != this->end())
			++last;
		return {first, last};
	}

private:
	template <class K, class M>
	std::pair<iterator, bool> _insert_or_assign(K&& key, M&& value)
	{
		auto it = this->find(key);
		if (it == this->end())
			return {emplace_hint(it, std::forward<K>(key), std::forward<M>(value)), true};

		it->second = std::forward<M>(value);
		return {it, false};
	}

	template <class K, class M>
	iterator _insert_or_assign(const const_iterator& hint, K&& key, M&& value)
	{
		auto it = this->find(key);
		if (it == this->end())
			return emplace_hint(hint, std::forward<K>(key), std::forward<M>(value));

		it->second = std::forward<M>(value);
		return it;
	}

public:
	template <class M>
	auto insert_or_assign(const key_type& key, M&& value)
	{ return _insert_or_assign(key, std::forward<M>(value)); }

	template <class M>
	auto insert_or_assign(key_type&& key, M&& value)
	{ return _insert_or_assign(std::move(key), std::forward<M>(value)); }

	template <class M>
	auto insert_or_assign(const const_iterator& hint, const key_type& key, M&& value)
	{ return _insert_or_assign(hint, key, std::forward<M>(value)); }

	template <class M>
	auto insert_or_assign(const const_iterator& hint, key_type&& key, M&& value)
	{ return _insert_or_assign(hint, std::move(key), std::forward<M>(value)); }
};

} // namespace robin_hood

#endif
