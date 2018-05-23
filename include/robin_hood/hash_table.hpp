
#ifndef ROBIN_HOOD_HASH_TABLE
#define ROBIN_HOOD_HASH_TABLE

#include <algorithm>
#include <functional>
#include <limits>
#include <utility>
#include <memory>
#include <cstring> // for std::memcpy
#include <cassert>

// There are a few cases here where providing hints can help speed up
// tight loops when profile-guided optimization isn't in use.
#ifdef __GNUC__
#	define ROBIN_HOOD_DETAIL_LIKELY(Expr) __builtin_expect((Expr), true)
#	define ROBIN_HOOD_DETAIL_UNLIKELY(Expr) __builtin_expect((Expr), false)
#else
#	define ROBIN_HOOD_DETAIL_LIKELY(Expr) (Expr)
#	define ROBIN_HOOD_DETAIL_UNLIKELY(Expr) (Expr)
#endif

// We'd like to give restrict hints to the compiler for a couple of methods
#if defined(__GNUC__) || defined(__clang__)
#	define ROBIN_HOOD_DETAIL_RESTRICT __restrict__
#elif defined(_MSC_VER)
#	define ROBIN_HOOD_DETAIL_RESTRICT __restrict
#else
	// Many compilers support the C keyword even in C++
#	define ROBIN_HOOD_DETAIL_RESTRICT restrict
#endif

// Trace logging for debugging
#define ROBIN_HOOD_DETAIL_LOG(...) do; while (false)
//#define ROBIN_HOOD_DETAIL_LOG(...) (std::clog << __VA_ARGS__ << std::endl)

namespace robin_hood {

namespace detail {
	// Traits type for key extraction functors
	struct must_construct {};

	// Must satisfy:
	// `key_of_unconstructed<decltype(f), T, decltype(args)...>::get(f, args...) == f(T(args...))`
	template <class F, class Value, class... Args>
	struct key_of_unconstructed : must_construct
	{
		static decltype(auto) get(const F& f, Args... args)
		{
			ROBIN_HOOD_DETAIL_LOG("key_of_unconstructed<" << typeid(F).name() << ">::get(..)" <<
				" called with " << sizeof...(Args) << " arguments");
			return f(Value(static_cast<decltype(args)&&>(args)...));
		}
	};

	// Identity functor, used for key extraction in hash_set
	struct identity_f
	{
		template <class T>
		T operator()(T&& x) const
		{ return std::forward<T>(x); }
	};

	template <class Key>
	struct key_of_unconstructed<identity_f, Key, const Key&>
	{
		static const Key& get(const identity_f&, const Key& key)
		{ return key; }
	};

	template <class T>
	using storage_t = std::aligned_storage_t<sizeof(T), alignof(T)>;

	template <class T, class Max>
	using difference_type_for =
		std::conditional_t<sizeof(T) < 1, std::int8_t,
		std::conditional_t<sizeof(T) < 2, std::int16_t,
		std::conditional_t<sizeof(T) < 4, std::int32_t,
		std::conditional_t<sizeof(T) < 8, std::int64_t, Max>>>>;
}

// This implementation employs open addressing with a Robin Hood replacement scheme.
//
// Performance: profiling indicates that the cost of most operations is effectively a single
// cache miss, which we aren't likely to beat without leveraging application-level knowledge
// about query coherence.
//
// Iterator invalidation: most of the interface is identical to std::unordered_set and
// std::unordered_map, but one notable place where this is not true is iterator invalidation.
// Unlike the standard containers, all non-const operations may invalidate any iterators.
// This is unavoidable without a substantial performance impact owing to the Robin Hood
// replacement scheme.
//
// Buckets: it doesn't make sense to think about an open-addressing hash table in terms of
// buckets, so this portion of the `std::unordered_*` interface is entirely absent.
template <class T,
	// Rather than storing a separate key, we make the key be a function of the value.
	// This can be used to get the same behavior as std::hash_map by using an std::pair
	// as the value and std::get<0> as the key function.  It can also provide the same
	// interface as std::hash_set by using the identity function.
	class KeyExtract,
	class KeyHash = std::hash<std::decay_t<std::result_of_t<KeyExtract(T)>>>,
	class KeyEqual = std::equal_to<std::decay_t<std::result_of_t<KeyExtract(T)>>>,
	class Alloc = std::allocator<T>>
class hash_table
{
	static_assert(std::is_move_constructible<T>::value,
		"T must be move-constructible");

	using allocator_traits = std::allocator_traits<Alloc>;

public:
	using allocator_type = Alloc;
	using key_extract = KeyExtract;
	using hasher = KeyHash;
	using key_equal = KeyEqual;

	using value_type = T;
	using key_type = std::decay_t<std::result_of_t<KeyExtract(T)>>;

	using size_type = typename allocator_traits::size_type;
	using difference_type = detail::difference_type_for<key_type,
		typename allocator_traits::difference_type>;
	using const_pointer = typename allocator_traits::const_pointer;
	using pointer = typename allocator_traits::pointer;
	using const_reference = T const&;
	using reference = T&;

private:
	// dib that will be used to flag empty entries.
	// For performance reasons (see _emplace_unsafe, _find,
	// and _erase), this value should be less than 0.
	static constexpr difference_type _empty_dib = -1;
	static bool _dib_is_empty(difference_type dib)
	{
		// This can be more efficient than checking equality
		return dib < 0;
	}

	struct entry
	{
		using allocator_type = typename Alloc::template rebind<entry>::other;

		// Value first to help with alignment.  Bear in mind that
		// this is only constructed if the entry is non-empty.  This
		// avoids the requirement that T be default-constructible.
		detail::storage_t<T> value_storage;

		// Distance from preferred location.  The invariant we will
		// maintain is that the dib of an entry will never exceed
		// that of the previous entry by more than 1.  In such a case
		// we can swap the entries (robbing the low dib one to help
		// the high dib one), thereby restablishing the invariant.
		// Furthermore, this implies that subject to the general
		// constraints of an open-addressing hash table, we minimize
		// the maximum dib.  In other words, we use the order with
		// the best worst-case performance!
		difference_type dib;

		// Because we read entry::dib without always constructing entry,
		// we need to ensure it's trivial to avoid undefined behavior.
		static_assert(std::is_trivial<difference_type>::value,
			"difference_type must be trivial");

		// This type must be default-constructible and trivially so
		// (as checked by the static_assert below)
		entry() = default;

		// // Movable (but not copyable) so that sorting works
		// entry(entry&& other) :
		// 	dib(std::exchange(other.dib, _empty_dib))
		// {
		// 	// TODO: This doesn't use Alloc::construct/destroy
		// 	if (!_dib_is_empty(dib)) {
		// 		new (&value()) T(std::move(other.value()));
		// 		other.value().~T();
		// 	}
		// }
		// entry& operator =(entry&& other)
		// {
		// 	// TODO: This doesn't use Alloc::construct/destroy
		// 	if (_dib_is_empty(dib))
		// 	{
		// 		if (_dib_is_empty(other.dib))
		// 			return *this;
		// 		new (&value()) T(std::move(other.value()));
		// 		other.value().~T();
		// 	}
		// 	else if (_dib_is_empty(other.dib)) {
		// 		new (&other.value()) T(std::move(value()));
		// 		value().~T();
		// 	} else {
		// 		std::swap(value(), other.value());
		// 	}
		// 	std::swap(dib, other.dib);
		// 	return *this;
		// }

		T& value() { return *reinterpret_cast<T*>(&value_storage); }
		T const& value() const { return *reinterpret_cast<T const*>(&value_storage); }

		template <class... Args>
		void give_value(allocator_type& alloc, difference_type dib, Args&&... args)
		{
			std::allocator_traits<allocator_type>::construct(alloc, &value(), std::forward<Args>(args)...);
			this->dib = dib;
		}

		void take_value_unsafe(allocator_type& alloc)
		{
			std::allocator_traits<allocator_type>::destroy(alloc, &value());
		}

		void take_value(allocator_type& alloc)
		{
			assert(dib == _empty_dib);
			take_value_unsafe(alloc);
		}
	};

	static_assert(std::is_trivially_default_constructible_v<entry>,
		"entry type must be trivially default-constructible");

	// We'll need to rebind the allocator since we're not allocating T directly
	using entry_allocator_type = typename entry::allocator_type;
	using entry_allocator_traits = std::allocator_traits<entry_allocator_type>;
	using entry_pointer = typename entry_allocator_traits::pointer;

	// TODO: Take advantage of the empty base optimization
	entry_allocator_type _alloc{};
	key_extract _key_extract{};
	hasher _hash_function{};
	key_equal _key_eq{};

	// Table of entries
	entry_pointer _first = nullptr, _last = _first;
	// Number of non-empty entries in the table
	size_type _size = 0;
	// Personally, I would prefer load factor to be specified
	// by a rational, but for consistency with the standard
	// library, we'll stick with a float.
	float _max_load_factor = 0.67f; // Emperically chosen

public:
	allocator_type get_allocator() const noexcept { return _alloc; }
	const hasher& hash_function() const noexcept { return _hash_function; }
	const key_equal& key_eq() const noexcept { return _key_eq; }
	size_type max_size() const noexcept { return entry_allocator_traits::max_size(_alloc); }
	size_type bucket_count() const noexcept { return _last - _first; }
	size_type size() const noexcept { return _size; }
	bool empty() const noexcept { return !size(); }

private:
	entry_pointer _probe(entry_pointer p) const noexcept
	{
		// Simple linear probing because cache effects greatly
		// outweigh the theoretical benefits of other methods.
		++p;
		return ROBIN_HOOD_DETAIL_UNLIKELY(p == _last) ? _first : p;
	}

	bool _probe_insert(
		entry_pointer ROBIN_HOOD_DETAIL_RESTRICT& ROBIN_HOOD_DETAIL_RESTRICT p,
		difference_type& ROBIN_HOOD_DETAIL_RESTRICT dib,
		const key_type& ROBIN_HOOD_DETAIL_RESTRICT key) const noexcept
	{
		// This is an optimization to avoid unnecessary key
		// equality checks in tables with many collisions.
		// However, profiling indicates that it's not a win
		// compared to the optimization below, and it doesn't
		// make sense to enable both because it results in
		// extra (unpredictable) branches.
		//while (dib < p->dib)
		//{
		//	p = _probe(p);
		//	++dib;
		//}

		// In theory, we could use binary search below, which would be
		// O(log dib) instead of O(dib), but in practice this is slower
		// than linear search even for very full hash tables.

		// Find either the next empty slot or one to rob.
		// By choosing -1 for _empty_dib, we can combine these:
		// dib <= p->dib && p->dib != _empty_dib
		while (dib <= p->dib)
		{
			if (// Similar to the dib check optimization above
				// but may still be beneficial, particularly
				// when equality checking is slow
				dib == p->dib &&
				_key_eq(key, _key_extract(p->value())))
				return false;

			p = _probe(p);
			++dib;
		}

		return true;
	}

	void _propogate_insert(
		entry_pointer ROBIN_HOOD_DETAIL_RESTRICT p,
		difference_type dib,
		value_type&& ROBIN_HOOD_DETAIL_RESTRICT value)
	{
		ROBIN_HOOD_DETAIL_LOG("_propogate_insert(p, dib, value)");

		// Propagate everything forward while robbing from
		// the rich (low dib) to help the poor (high dib)
		// TODO: Can this loop be optimized further (one branch)
		// by taking advantage of the structure invariants?
		while (!_dib_is_empty(p->dib))
		{
			if (dib > p->dib)
			{
				std::swap(value, p->value());
				std::swap(dib, p->dib);
			}

			p = _probe(p);
			++dib;
		}

		// Construct the last entry in-place by moving from value
		p->give_value(_alloc, dib, std::move(value));
	}

	template <class... Args>
	void _propogate_insert(
		entry_pointer ROBIN_HOOD_DETAIL_RESTRICT p,
		difference_type dib,
		Args&&... args)
	{
		ROBIN_HOOD_DETAIL_LOG("_propogate_insert(p, dib, args...)");

		// Conceptually, this should be equivalent to
		// _propogate_insert(p, dib, value_type(std::forward<Args>(args)...));
		// except that we may be able to perform one fewer move.

		// Propagate everything forward while robbing from
		// the rich (low dib) to help the poor (high dib)
		// TODO: Can this loop be optimized further (one branch)
		// by taking advantage of the structure invariants?
		while (!_dib_is_empty(p->dib))
		{
			if (dib > p->dib)
			{
				auto other_value = std::exchange(p->value(), value_type(std::forward<Args>(args)...));
				auto other_dib = std::exchange(p->dib, dib);
				return _propogate_insert(_probe(p), ++other_dib, std::move(other_value));
			}

			p = _probe(p);
			++dib;
		}

		// Construct the only entry in-place by forwarding args
		p->give_value(_alloc, dib, std::forward<Args>(args)...);
	}

	template <class... Args>
	std::pair<entry_pointer, bool> _emplace_unsafe_at_key(const key_type& key, Args&&... args)
	{
		ROBIN_HOOD_DETAIL_LOG("_emplace_unsafe_at_key(" << key << ")");

		auto p = _first + _hash_function(key) % bucket_count();
		difference_type dib = 0;

		// Find the first valid insertion point
		// or determine if the key is not unique
		auto result = _probe_insert(p, dib, key);

		// Perform the actual insertion with propogation
		if (result)
			_propogate_insert(p, dib, std::forward<Args>(args)...);

		return {p, result};
	}

	// Tries to insert a value into the table.  Returns a pointer
	// to the entry in the table with the same key and a flag
	// indicating whether or not it is new.
	// Precondition: there exists an empty entry in the table.
	// Note: this _does not_ update _size!  This is to avoid
	// repeatedly incrementing it (e.g. during a rehash).
	template <class... Args>
	std::pair<entry_pointer, bool> _emplace_unsafe(Args&&... args)
	{
		// Take advantage of key_extract_traits when it is available,
		// so that we can avoid constructing value in some cases.
		using unconstructed_key_extract = detail::key_of_unconstructed<key_extract, value_type, const Args&...>;
		if constexpr (std::is_base_of_v<detail::must_construct, unconstructed_key_extract>)
		{
			value_type value(std::forward<Args>(args)...);
			decltype(auto) key = _key_extract(value);
			return _emplace_unsafe_at_key(key, std::move(value));
		}
		else
		{
			decltype(auto) key = unconstructed_key_extract::get(_key_extract, static_cast<const Args&>(args)...);
			return _emplace_unsafe_at_key(key, std::forward<Args>(args)...);
		}
	}

	// Find a pointer to the entry in a table with a given key.
	entry_pointer _find(const key_type& key) const
	{
		ROBIN_HOOD_DETAIL_LOG("_find(" << key << ")");

		// For correctness, we only need to ensure the bucket_count is nonzero here,
		// but while we're doing the check anyway, we might as well rule out the
		// empty case too.
		if (ROBIN_HOOD_DETAIL_UNLIKELY(empty()))
			return _last;

		auto hash = _hash_function(key);
		auto p = _first + hash % bucket_count();
		difference_type dib = 0;
		// By choosing -1 for _empty_dib, we can combine these:
		// dib <= p->dib && p->dib != _empty_dib
		while (dib <= p->dib)
		{
			if (_key_eq(key, _key_extract(p->value())))
				return p;
			p = _probe(p);
			++dib;
		}

		return _last;
	}

	// Erase an entry at a pointer into the table.
	void _erase(entry_pointer p)
	{
		entry_pointer q;
		// By choosing -1 for _empty_dib, we can combine these:
		// q->dib != _empty_dib && q->dib != 0
		while ((q = _probe(p))->dib > 0)
		{
			// We re-use the entries rather than destroying them here
			// to avoid invoking the destructor on value unecessarily
			p->value() = std::move(q->value());
			p->dib = q->dib - 1;
			p = q;
		}

		p->dib = _empty_dib;
		p->take_value(_alloc);
		--_size;
	}

	struct _raw_table
	{
		using iterator = entry_pointer;

		entry_pointer _first, _last;
		std::reference_wrapper<entry_allocator_type> _alloc;

		_raw_table(entry_allocator_type& alloc, entry_pointer first, entry_pointer last) noexcept :
			_first(first), _last(last), _alloc(alloc)
		{
		}

		_raw_table(_raw_table&& other) :
			_raw_table(other._alloc,
				std::exchange(other._first, nullptr),
				std::exchange(other._last, nullptr))
		{
		}

		_raw_table& operator =(_raw_table&& other)
		{
			std::swap(_first, other._first);
			std::swap(_last, other._last);
			std::swap(_alloc, other._alloc);
		}

		~_raw_table()
		{
			if (_first)
				entry_allocator_traits::deallocate(_alloc, _first, _last - _first);
		}

		bool empty() const { return _last == _first; }
		size_type size() const { return _last - _first; }
		iterator begin() { return _first; }
		iterator end() { return _last; }
	};

	// Replace the entire table with a new table.  This is used
	// to concisely implement both `_rehash` and `_deallocate`.  It is
	// marked unsafe because it leaves dib's in unspecified states.
	auto _exchange_raw_table_unsafe(entry_pointer new_first, entry_pointer new_last)
	{
		return _raw_table(_alloc,
			std::exchange(_first, new_first),
			std::exchange(_last, new_last));
	}

	// Invoke a predicate on and destroy each value currently in the
	// table. This is used to concisely implement `clear`.
	template <class Pred>
	void _consume(Pred&& pred)
	{
		for (auto p = _first; ROBIN_HOOD_DETAIL_UNLIKELY(p != _last); ++p)
		{
			if (!_dib_is_empty(p->dib))
			{
				pred(std::move(p->value()));
				p->dib = _empty_dib;
				p->take_value(_alloc);
			}
		}
		_size = 0;
	}

	// Deallocate everything allocated with the allocator.  Intended for
	// use before destroying the allocator (e.g. in the destructor).
	void _deallocate()
	{
		for (auto& e : _exchange_raw_table_unsafe(nullptr, nullptr))
			if (!_dib_is_empty(e.dib))
				// Table entries are considered invalid at this
				// point, so we don't need to update the dib
				e.take_value_unsafe(_alloc);
	}

	// Precondition: new_bucket_count >= size()
	void _rehash(size_type new_bucket_count)
	{
		auto new_first = new_bucket_count ? entry_allocator_traits::allocate(_alloc, new_bucket_count, _first) : nullptr,
			new_last = new_first + new_bucket_count;
		for (auto p = new_first; p != new_last; ++p)
			p->dib = _empty_dib;

		// This works, but it performs more moves than potentially necessary.
		// However, moves are generally cheap, so this outperforms the method
		// below in our benchmarks.
		for (auto& e : _exchange_raw_table_unsafe(new_first, new_last))
		{
			if (!_dib_is_empty(e.dib))
			{
				_emplace_unsafe(std::move(e.value()));

				// Table entries are considered invalid at this
				// point, so we don't need to update the dib
				e.take_value_unsafe(_alloc);
			}
		}

		// // Instead, we can figure out where (most) of the values belong a priori
		// // TODO: This is currently slower than the method above, but can be made
		// // more efficient by taking advantage of the fact that the moves only need
		// // to consider one case of `entry`'s move constructor.
		// auto end = std::remove_if(_first, _last, [](const auto& e)
		// { return _dib_is_empty(e.dib); });

		// auto get_entry_bucket = [&](const entry& e)
		// {
		// 	assert(e.dib != _empty_dib);
		// 	return _hash_function(_key_extract(e.value())) % new_bucket_count;
		// };

		// std::sort(_first, end, [=](const auto& l, const auto& r)
		// { return get_entry_bucket(l) < get_entry_bucket(r); });

		// auto it = _first;
		// for (size_type index = 0; ROBIN_HOOD_DETAIL_UNLIKELY(it != end); ++it, ++index)
		// {
		// 	size_type new_index = get_entry_bucket(*it);
		// 	index = std::max({index, new_index});
		// 	if (ROBIN_HOOD_DETAIL_UNLIKELY(index >= new_bucket_count))
		// 		break;
		// 	new_first[index].give_value(_alloc, index - new_index, std::move(it->value()));
		// }

		// // Handle the stragglers after swapping out buffers using standard emplacement
		// auto old_table = _exchange_raw_table_unsafe(new_first, new_last);
		// for (; it != end; ++it)
		// 	_emplace_unsafe(std::move(it->value()));
		// // Destroy old values
		// for (auto it = old_table.begin(); it != end; ++it)
		// 	// Table entries are considered invalid at this
		// 	// point, so we don't need to update the dib
		// 	it->take_value_unsafe(_alloc);
	}

	auto _actual_max_load_factor() const noexcept
	{
		return std::min({1.f, max_load_factor()});
	}

	// The size threshold for a bucket count
	auto _size_for_rehash(size_type bucket_count) const noexcept
	{
		return bucket_count * _actual_max_load_factor();
	}

	void _rehash_before_insert(size_type count)
	{
		auto new_size = size() + count;
		if (ROBIN_HOOD_DETAIL_UNLIKELY(new_size >= _size_for_rehash(bucket_count())))
		{
			// A growth factor as high as 2 is suboptimal for a vector, but there is
			// a lot more work involved in growing a hash table, so a larger growth
			// factor makes sense in this context
			const auto growth_factor = 2;

			// Prime sizes would be ideal here, but we can get a lot of the benefit
			// simply by ensuring that the bucket count is always odd
			_rehash(1 | static_cast<size_type>((growth_factor * new_size) / _actual_max_load_factor()));
		}
	}

	template <class... Args>
	auto _emplace(Args&&... args)
	{
		// Rehash if required
		_rehash_before_insert(1);

		auto result = _emplace_unsafe(std::forward<Args>(args)...);

		// Update _size
		if (result.second)
			++_size;

		return result;
	}

	// Base class for const_iterator and iterator that handles the heavy lifting.
	// We employ the Curiously Recurring Template Pattern here to get as much of
	// the functionality in this class as possible.  This makes the iterator and
	// const_iterator classes very simple facades.
	template <class Iterator>
	struct _iterator_base
	{
	private:
		Iterator& derived() noexcept
		{ return static_cast<Iterator&>(*this); }

		const Iterator& derived() const noexcept
		{ return static_cast<const Iterator&>(*this); }

		// This is mutable because we may need to advance it during canonicalization
		mutable entry_pointer _current;

	protected:
		friend class hash_table;

		// Requires: dereferencable
		entry_pointer& current() const noexcept
		{
			while (_dib_is_empty(_current->dib))
				++_current;
			return _current;
		}

		// Requires: _current <= p
		bool canonicalize_up_to(entry_pointer p) const noexcept
		{
			for (; _current < p; ++_current)
				if (!_dib_is_empty(_current->dib))
					return false;
			return true;
		}

		_iterator_base(entry_pointer current) noexcept :
			_current(current)
		{}

		template <class OtherIterator>
		_iterator_base(const _iterator_base<OtherIterator>& other) noexcept :
			_iterator_base(other._current)
		{}

	public:
		using difference_type = typename hash_table::difference_type;
		using value_type = typename hash_table::value_type;
		using iterator_category = std::forward_iterator_tag;

		decltype(auto) operator ->() const noexcept
		{ return &(*derived()); }

		Iterator& operator++() noexcept
		{
			++current();
			return derived();
		}

		Iterator operator++(int) noexcept
		{
			auto result = derived();
			++*this;
			return result;
		}

		template <class OtherIterator>
		bool operator ==(const _iterator_base<OtherIterator>& other) const noexcept
		{
			// The most common use case (and especially the most common bottleneck case)
			// is in a for loop where we are iterating over more than one value, so we
			// optimize for this case
			return ROBIN_HOOD_DETAIL_LIKELY(_current <= other._current) ?
				canonicalize_up_to(other._current) :
				other.canonicalize_up_to(_current);
		}

		template <class OtherIterator>
		bool operator !=(const _iterator_base<OtherIterator>& other) const noexcept
		{ return !(*this == other); }

	protected:
		template <class OtherIterator>
		decltype(auto) _assign(const _iterator_base<OtherIterator>& other) noexcept
		{
			_current = other._current;
			return derived();
		}
	};

public:
	struct iterator :
		public _iterator_base<iterator>
	{
		friend class hash_table;

		using base_type = _iterator_base<iterator>;
		using base_type::base_type;
		using pointer = typename hash_table::pointer;
		using reference = typename hash_table::reference;

		reference operator *() const noexcept
		{ return this->current()->value(); }

		decltype(auto) operator =(const iterator& other) noexcept
		{ return this->_assign(other); }
	};

	struct const_iterator :
		public _iterator_base<const_iterator>
	{
		friend class hash_table;

		using base_type = _iterator_base<const_iterator>;
		using base_type::base_type;
		using pointer = typename hash_table::const_pointer;
		using reference = typename hash_table::const_reference;

		const_iterator(const iterator& it) noexcept :
			base_type(it)
		{}

		reference operator *() const noexcept
		{ return this->current()->value(); }

		decltype(auto) operator =(const const_iterator& other) noexcept
		{ return this->_assign(other); }

		decltype(auto) operator =(const iterator& other) noexcept
		{ return this->_assign(other); }
	};

	explicit hash_table(
		const KeyExtract& key_extract = KeyExtract{},
		const KeyHash& hash_function = KeyHash{},
		const KeyEqual& key_eq = KeyEqual{},
		const Alloc& alloc = Alloc{}) :
		_alloc(alloc),
		_key_extract(key_extract),
		_hash_function(hash_function),
		_key_eq(key_eq)
	{}

	hash_table(const hash_table& other) :
		hash_table(other._key_extract, other._hash_function, other._key_eq,
			entry_allocator_traits::select_on_container_copy_construction(other._alloc))
	{
		// Performing a range insertion works and isn't terrible, but it is
		// inefficient in a couple of ways:
		// First, there is overhead in figuring out where all the entries belong.
		// Second, the iterators aren't random access, so we have to walk through
		// the other hash table before resizing this one appropriately.
		//insert(other.begin(), other.end());

		// Instead, we exactly duplicate the structure of the other table
		_rehash(other.bucket_count());
		if constexpr (std::is_trivially_copyable_v<value_type>)
		{
			std::memcpy(other._first, other._last, _first);
		}
		else
		{
			auto q = _first;
			for (auto p = other._first; p != other._last; ++p, ++q)
				if (!_dib_is_empty(p->dib))
					// TODO: This needs to cleanup correctly if an exception is thrown here
					q->give_value(_alloc, p->dib, p->value());
		}
		_size = other._size;
	}

	hash_table(hash_table&& other) :
		_alloc(std::move(other._alloc)),
		_key_extract(std::move(other._key_extract)),
		_hash_function(std::move(other._hash_function)),
		_key_eq(std::move(other._key_eq)),
		_first(std::move(other._first)),
		_last(std::move(other._last)),
		_size(std::move(other._size)),
		_max_load_factor(std::move(other._max_load_factor))
	{
		// Simply move everything and then ensure the other table can't access it
		other._first = other._last = nullptr;
		other._size = 0;
	}

	~hash_table()
	{
		_deallocate();
	}

	const_iterator begin() const noexcept
	{ return {_first}; }

	const_iterator end() const noexcept
	{ return {_last}; }

	iterator begin() noexcept
	{ return {_first}; }

	iterator end() noexcept
	{ return {_last}; }

	float load_factor() const noexcept
	{ return size() / static_cast<float>(bucket_count()); }

	float max_load_factor() const noexcept
	{ return _max_load_factor; }

	void max_load_factor(float value)
	{
		_max_load_factor = value;

		// Rehash if required
		_rehash_before_insert(0);
	}

	void rehash(size_type new_bucket_count)
	{
		// Don't do anything if we would immediately need to rehash
		if (ROBIN_HOOD_DETAIL_UNLIKELY(size() >= _size_for_rehash(new_bucket_count)))
			return;

		_rehash(new_bucket_count);
	}

	void reserve(size_type count)
	{
		// Don't bother shrinking in reserve.  If you need to control
		// bucket_count at a level that fine, then use rehash insead.
		if (ROBIN_HOOD_DETAIL_UNLIKELY(count < _size_for_rehash(bucket_count())))
			return;

		rehash(1 + count / _actual_max_load_factor());
	}

	template <class... Args>
	std::pair<iterator, bool> emplace(Args&&... args)
	{
		auto [p, success] = _emplace(std::forward<Args>(args)...);
		return {{p}, success};
	}

	template <class... Args>
	iterator emplace_hint(const const_iterator& hint, Args&&... args)
	{
		auto [p, success] = _emplace(std::forward<Args>(args)...);
		return {p};
	}

	auto insert(value_type&& value)
	{ return emplace(std::move(value)); }

	auto insert(const value_type& value)
	{ return emplace(value); }

	auto insert(const const_iterator& hint, value_type&& value)
	{ return insert(std::move(value)).first; }

	auto insert(const const_iterator& hint, const value_type& value)
	{ return insert(value).first; }

	template <class FwdIt>
	void insert(FwdIt first, FwdIt last)
	{
		// TODO: If the iterators are not random access, this
		// will iterate through twice, which could cause issues
		// with mutating iterators.  That said, the standard
		// containers and algorithms make the same assumption.

		// Rehash if required and perform direct insertions
		auto count = std::distance(first, last);
		_rehash_before_insert(count);
		for (; first != last; ++first)
			_emplace_unsafe(*first);
		_size += count;

		// TODO: It would be possible to compute all the table
		// indices here before insertion, which could be more
		// performant, especially for large types.  This is
		// actually fairly easy to do if the table is already
		// empty, as would be the case when rehashing.
	}

	void insert(std::initializer_list<value_type> ilist)
	{
		insert(ilist.begin(), ilist.end());
	}

	iterator erase(const_iterator it)
	{
		_erase(it.current());

		// We don't need to do anything to advance the iterator here
		// because either a value was moved in to replace the one that
		// was erased or the entry pointed to is now empty (in which case
		// canonicalization will advance it to the next value on deref)
		return it;
	}

	size_type erase(const key_type& key)
	{
		auto p = _find(key);
		if (p == _last)
			return 0;
		_erase(p);
		return 1;
	}

	void clear()
	{
		_consume([](T&&){});
	}

	const_iterator find(const key_type& key) const
	{
		return {_find(key)};
	}

	iterator find(const key_type& key)
	{
		return {_find(key)};
	}

	bool operator ==(const hash_table& other) const
	{
		if (size() != other.size())
			return false;

		auto last = end();
		for (auto&& value : other)
		{
			auto it = find(_key_extract(value));
			// TODO: This double-compares key in the set and map
			// cases, which might be optimized out and might not.
			// It would be better (though less maintainable) to
			// tightly integrate this check into the find, by
			// observing that a == b implies k(a) == k(b).
			if (it == last || *it != value)
				return false;
		}

		return true;
	}

	bool operator !=(const hash_table& other) const
	{ return !(*this == other); }

	friend void swap(hash_table& left, hash_table& right)
		noexcept(
			(allocator_traits::is_always_equal::value ||
			noexcept(std::swap(std::declval<key_extract&>(), std::declval<key_extract&>()))) &&
			noexcept(std::swap(std::declval<hasher&>(), std::declval<hasher&>())) &&
			noexcept(std::swap(std::declval<key_equal&>(), std::declval<key_equal&>())))
	{
		using std::swap;
		if constexpr (entry_allocator_traits::propagate_on_container_swap::value)
			swap(left._alloc, right._alloc);
		swap(left._key_extract, right._key_extract);
		swap(left._hash_function, right._hash_function);
		swap(left._key_eq, right._key_eq);
		swap(left._first, right._first);
		swap(left._last, right._last);
		swap(left._size, right._size);
		swap(left._max_load_factor, right._max_load_factor);
	}

	hash_table& operator =(const hash_table& other)
	{
		if constexpr (entry_allocator_traits::propagate_on_container_copy_assignment::value)
		{
			// Deallocate table before replacing allocator
			_deallocate();
			_alloc = other._alloc;
		}

		_key_extract = other._key_extract;
		_hash_function = other._hash_function;
		_key_eq = other._key_eq;
		_max_load_factor = other._max_load_factor;

		clear();
		insert(other.begin(), other.end());

		return *this;
	}

	hash_table& operator =(hash_table&& other)
	{
		using std::swap;

		if constexpr (entry_allocator_traits::propagate_on_container_move_assignment::value)
		{
			// Deallocate table before replacing allocator
			_deallocate();
			_alloc = std::move(other._alloc);
		}

		_key_extract = std::move(other._key_extract);
		_hash_function = std::move(other._hash_function);
		_key_eq = std::move(other._key_eq);
		swap(_size, other._size);
		swap(_max_load_factor, other._max_load_factor);

		swap(_first, other._first);
		swap(_last, other._last);

		return *this;
	}
};

} // namespace robin_hood

#undef ROBIN_HOOD_DETAIL_LIKELY
#undef ROBIN_HOOD_DETAIL_UNLIKELY
#undef ROBIN_HOOD_DETAIL_RESTRICT

#endif
