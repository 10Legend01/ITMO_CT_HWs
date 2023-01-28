#include "pool.h"

#include <cstddef>
#include <iostream>

using std::size_t;

namespace pool {

std::pair<size_t, size_t> Pool::find_empty_place(const size_t i, const size_t l, const size_t r, const size_t n) const
{
    if (m_tree[i] == BUSY || r - l < n) {
        return std::make_pair(npos, npos);
    }
    if (r - l == n) {
        if (m_tree[i] == EMPTY) {
            return std::make_pair(l, r - l);
        }
        return std::make_pair(npos, npos);
    }
    if (m_tree[i] == EMPTY) {
        return std::make_pair(l, r - l);
    }
    const size_t m = (l + r) / 2;
    const auto left = find_empty_place(i * 2 + 1, l, m, n);
    if (left.second == n) {
        return left;
    }
    const auto right = find_empty_place(i * 2 + 2, m, r, n);
    if (right.second < left.second) {
        return right;
    }
    return left;
}

void Pool::add(const size_t i, const size_t l, const size_t r, const size_t pos, const size_t n)
{
    if (pos < l || r <= pos) {
        return;
    }
    if (r - l == n) {
        m_tree[i] = BUSY;
        return;
    }
    if (m_tree[i] == EMPTY) {
        m_tree[i] = DIVIDED;
    }
    const size_t m = (l + r) / 2;
    add(i * 2 + 1, l, m, pos, n);
    add(i * 2 + 2, m, r, pos, n);
}

void Pool::del(const size_t i, const size_t l, const size_t r, const size_t pos)
{
    if (pos < l || r <= pos) {
        return;
    }
    if (m_tree[i] == BUSY) {
        m_tree[i] = EMPTY;
        return;
    }
    if (r - l == 1) {
        return;
    }
    const size_t m = (l + r) / 2;
    del(i * 2 + 1, l, m, pos);
    del(i * 2 + 2, m, r, pos);
    if (m_tree[i * 2 + 1] == EMPTY && m_tree[i * 2 + 2] == EMPTY) {
        m_tree[i] = EMPTY;
    }
}

void * Pool::allocate(const size_t n)
{
    size_t k = 1;
    while (k < n) {
        k *= 2;
    }
    k = (std::max(N, k) + N - 1) / N;
    const std::pair<size_t, int> pr = find_empty_place(0, 0, blocks_count, k);
    if (pr.first != npos) {
        add(0, 0, blocks_count, pr.first, k);
        return &m_storage[pr.first * N];
    }
    throw std::bad_alloc{};
}

void Pool::deallocate(const void * ptr)
{
    auto bytes = static_cast<const std::byte *>(ptr);
    const auto begin = &m_storage[0];
    if (bytes >= begin) {
        const size_t offset = (bytes - begin) / N;
        if (offset < blocks_count) {
            del(0, 0, blocks_count, offset);
            return;
        }
    }
    throw std::bad_alloc{};
}

Pool * create_pool(unsigned min_p, unsigned max_p)
{
    return new Pool(min_p, max_p);
}

void * allocate(Pool & pool, const size_t n)
{
    return pool.allocate(n);
}

void deallocate(Pool & pool, const void * ptr)
{
    pool.deallocate(ptr);
}

} // namespace pool
