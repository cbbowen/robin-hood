
# Robin Hood Hash Table

Performant implementation of a [Robin Hood hash table](https://en.wikipedia.org/wiki/Hash_table#Robin_Hood_hashing).

## Interface

`robin_hood::hash_map` and `robin_hood::hash_set` are intended to be drop-in replacements for `std::unordered_map` or `std::unordered_set` with a few notable exceptions:

* Iterator invalidation: Unlike the standard containers, all non-const operations may invalidate any iterators.  This is unavoidable without a substantial performance impact owing to the Robin Hood replacement scheme.

* Buckets: It doesn't make sense to think about an open-addressing hash table in terms of buckets, so this portion of the standard interface is entirely absent.

* Move-only types: Unlike the standard library associative containers, the ones in this library fully support types which cannot be copied (like `unique_ptr`).

Note that not every member function has been implemented yet.  However, the most common ones have, and the rest are on the way.

## Performance

Open-addressing hash tables have the potential to be faster than the separate chaining variant effectively required of the standard library types.  I have observed speedups of 1.0x to 2.1x compared to libstdc++ (1.2x to 2.5x compared to libc++) on my machine!  However, open-addressing approaches are more reliant on having a good hash function, and gains will depend on the key and value types and operations being measured.  You can compare for yourself by running

```sh
make run CONFIG=release
```

in the `test` directory.  Please open an issue if you encounter any performance regressions!

In addition to the benchmarks, there is a growing battery of unit tests that can be run using

```sh
make run
```

in the `test` directory.
