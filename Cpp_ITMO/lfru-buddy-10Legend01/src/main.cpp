#include "allocator.h"
#include "cache.h"

#include <iostream>
#include <string>

namespace {

struct String
{
    std::string data;
    bool marked = false;

    String(const std::string & key)
        : data(key)
    {
    }

    bool operator==(const std::string & other) const
    {
        return data == other;
    }

    friend std::ostream & operator<<(std::ostream & strm, const String & s)
    {
        return strm << s.data << std::boolalpha << "{" << s.marked << "}";
    }
};

using TestCache = Cache<std::string, String, AllocatorWithPool>;

} // anonymous namespace

int main()
{
    /*
    AllocatorWithPool allocator{0, 2};
    uint16_t * first = allocator.create<uint16_t>();
    allocator.create<uint8_t>();
    allocator.destroy<uint16_t>(reinterpret_cast<void *>(first));
    allocator.create<uint8_t>();
    allocator.create<uint16_t>();
     */
    TestCache cache(9, 4, 10);
    std::string line;
    while (std::getline(std::cin, line)) {
        if (line.length() == 0)
            break;
        auto & s = cache.get<String>(line);
        if (s.marked) {
            std::cout << "known" << std::endl;
        }
        std::cout << s << std::endl;

        s.marked = true;
        std::cout << "\n"
                  << cache;
    }
    std::cout << "\n"
              << cache;
    return 0;
}
