#pragma once

#include <cstddef>
#include <memory>
#include <vector>

enum State
{
    EMPTY,
    BUSY,
    DIVIDED
};

namespace pool {

class Pool
{
public:
    Pool(unsigned min_p, unsigned max_p)
        : N(1 << min_p)
        , blocks_count(1 << (max_p - min_p))
        , m_storage(1 << max_p)
        , m_tree(2 * blocks_count)
    {
    }

    void * allocate(size_t k);

    void deallocate(const void * ptr);

private:
    static constexpr size_t npos = static_cast<size_t>(-1);

    std::pair<size_t, size_t> find_empty_place(size_t i, size_t l, size_t r, size_t n) const;

    void add(size_t i, size_t l, size_t r, size_t pos, size_t n);

    void del(size_t i, size_t l, size_t r, size_t pos);

    const size_t N, blocks_count;
    std::vector<std::byte> m_storage;
    std::vector<State> m_tree;
};

Pool * create_pool(unsigned min_p, unsigned max_p);

void * allocate(Pool & pool, std::size_t n);

void deallocate(Pool & pool, const void * ptr);

} // namespace pool

class PoolAllocator
{
public:
    PoolAllocator(const unsigned min_p, const unsigned max_p)
        : m_pool(create_pool(min_p, max_p))
    {
    }

    static pool::Pool * create_pool(const unsigned min_p, const unsigned max_p)
    {
        return pool::create_pool(min_p, max_p);
    }

    void * allocate(const std::size_t n)
    {
        return pool::allocate(*m_pool, n);
    }

    void deallocate(const void * ptr)
    {
        pool::deallocate(*m_pool, ptr);
    }

private:
    std::unique_ptr<pool::Pool> m_pool;
};
