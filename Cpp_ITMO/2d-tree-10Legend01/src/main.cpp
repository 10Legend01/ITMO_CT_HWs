#include "primitives.h"

#include <iostream>

int main()
{
    kdtree::PointSet ps("D:\\Users\\Legend\\2d-tree-10Legend01\\test\\etc\\test1.dat");

    std::cout << ps << std::endl;
    std::cout << ps.size() << std::endl;

    auto n = ps.nearest(Point(.4, .4));
    std::cout << *n << std::endl;
    auto range = ps.range(Rect(Point(0.2, 0.2), Point(.8, .8)));

    //auto it = ps.begin();
    //auto end = ps.end();
    auto it = range.first;
    auto end = range.second;

    std::cout << std::distance(it, end) << std::endl;

    while (it != end) {
        std::cout << *it;
        ++it;
    }

    //std::vector<Point> v;
    //v.push_back(Point(0.4, 0.4));
    //v.push_back(Point(0.4, 0.7));
    //std::swap(v[0], v[1]);
    /*
    std::cout << (Point(0.4, 0.4) != Point(0.4, 0.4) ? "true" : "false");
    std::cout << Point(0.4, 0.4) << std::endl;

    kdtree::PointSet ps;
    ps.put(Point(0.4, 0.4));
    ps.put(Point(0.2, 0.6));
    ps.put(Point(0.1, 0.5));
    ps.put(Point(0.5, 0.5));
    ps.put(Point(0.8, 0.8));

    //std::cout << (it != end ? "true" : "false");

    std::cout << ps << std::endl;
*/
    return 0;
}
