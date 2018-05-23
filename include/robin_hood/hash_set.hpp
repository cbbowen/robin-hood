
#ifndef ROBIN_HOOD_HASH_SET
#define ROBIN_HOOD_HASH_SET

#include <functional>
#include <memory>

#include "hash_table.hpp"

namespace robin_hood {

template <class T,
	class Hash = std::hash<T>,
	class Equal = std::equal_to<T>,
	class Alloc = std::allocator<T>>
class hash_set :
	public hash_table<T,detail::identity_f, Hash, Equal, Alloc>
{
	using base_type = detail::identity_f, Hash, Equal, Alloc;

public:
	// TODO: Implement constructors (http://en.cppreference.com/w/cpp/container/unordered_set/unordered_set)
};

} // namespace robin_hood

#endif
