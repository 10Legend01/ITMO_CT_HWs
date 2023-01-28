#pragma once

#include <algorithm>
#include <list>
#include <ostream>

template <class Key, class KeyProvider, class Allocator>
class Cache
{
public:
    template <class... AllocArgs>
    Cache(const std::size_t cache_size, AllocArgs &&... alloc_args)
        : m_max_top_size(cache_size)
        , m_max_low_size(cache_size)
        , m_alloc(std::forward<AllocArgs>(alloc_args)...)
    {
    }

    std::size_t size() const
    {
        return m_queue_top.size() + m_queue_low.size();
    }

    bool empty() const
    {
        return m_queue_top.empty() && m_queue_low.empty();
    }

    template <class T>
    T & get(const Key & key);

    std::ostream & print(std::ostream & strm) const;

    friend std::ostream & operator<<(std::ostream & strm, const Cache & cache)
    {
        return cache.print(strm);
    }

private:
    const std::size_t m_max_top_size;
    const std::size_t m_max_low_size;
    Allocator m_alloc;
    std::list<KeyProvider *> m_queue_top;
    std::list<KeyProvider *> m_queue_low;
};

template <class T, class Allocator>
void destroy(Allocator & alloc, T * ptr)
{
    alloc.template destroy<T>(reinterpret_cast<void *>(ptr));
}

template <class KeyProvider, class Key>
auto findKey(std::list<KeyProvider *> & queue, Key & key)
{
    return std::find_if(queue.begin(), queue.end(), [&key](const KeyProvider * elem) {
        return *elem == key;
    });
}

template <class Key, class KeyProvider, class Allocator>
template <class T>
inline T & Cache<Key, KeyProvider, Allocator>::get(const Key & key)
{
    auto it = findKey(m_queue_top, key);
    if (it != m_queue_top.end()) {
        m_queue_top.splice(m_queue_top.begin(), m_queue_top, it);
    }
    else {
        it = findKey(m_queue_low, key);
        if (it != m_queue_low.end()) {
            m_queue_low.splice(m_queue_low.begin(), m_queue_low, it);
            const auto a = m_queue_low.front();
            m_queue_low.pop_front();
            if (m_max_top_size == m_queue_top.size()) {
                m_queue_low.splice(m_queue_low.end(), m_queue_top, std::prev(m_queue_top.end()));
            }
            m_queue_top.push_front(a);
        }
        else {
            if (m_max_low_size == m_queue_low.size()) {
                destroy(m_alloc, m_queue_low.front());
                m_queue_low.pop_front();
            }
            m_queue_low.push_back(m_alloc.template create<T>(key));
            return *static_cast<T *>(m_queue_low.back());
        }
    }
    return *static_cast<T *>(m_queue_top.front());
}

template <class Key, class KeyProvider, class Allocator>
inline std::ostream & Cache<Key, KeyProvider, Allocator>::print(std::ostream & strm) const
{
    strm << "Priority: \n";
    for (const auto it : m_queue_top) {
        strm << "data: " << it->data << "\n";
    }
    strm << "\nRegular: \n";
    for (const auto it : m_queue_low) {
        strm << "data: " << it->data << "\n";
    }
    strm << "\n";
    return strm;
}
