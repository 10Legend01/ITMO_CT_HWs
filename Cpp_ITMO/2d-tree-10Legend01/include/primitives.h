#pragma once

#include <iostream>
#include <limits>
#include <memory>
#include <optional>
#include <set>
#include <vector>

class Point
{
public:
    Point(double x, double y);

    double x() const;
    double y() const;
    double distance(const Point &) const;

    bool operator<(const Point &) const;
    bool operator>(const Point &) const;
    bool operator<=(const Point &) const;
    bool operator>=(const Point &) const;
    bool operator==(const Point &) const;
    bool operator!=(const Point &) const;

    friend std::ostream & operator<<(std::ostream &, const Point &);

private:
    const double X, Y;
};

class Rect
{
public:
    Rect(const Point & left_bottom, const Point & right_top);

    double xmin() const;
    double ymin() const;
    double xmax() const;
    double ymax() const;
    double distance(const Point & p) const;

    bool contains(const Point & p) const;
    bool intersects(const Rect &) const;

private:
    Point lb, rt;
};

class iteratorTree
{
private:
    std::vector<const Point *> vector;
    //std::vector<const Point *> vector;
    //Point a = new Point(1,1);

public:
    using iterator_category = std::forward_iterator_tag;
    using difference_type = std::ptrdiff_t;
    using value_type = Point;
    using pointer = const value_type *;
    using reference = const value_type &;

    iteratorTree(const std::vector<const Point *> & init = {})
    {
        std::move(init.begin(), init.end(), std::back_inserter(vector));
    }

    iteratorTree(const std::set<Point> & s)
    {
        for (auto i = s.begin(); i != s.end(); i++) {
            vector.emplace_back(i.operator->());
            //*vector.back() = Point(1,0);
            //const Point * p = vector.back();
            //*p = Point{1,2};
        }
    }

    reference operator*() const
    {
        return *vector.back();
    }

    pointer operator->() const
    {
        return vector.back();
    }

    iteratorTree & operator++()
    {
        vector.pop_back();
        return *this;
    }

    iteratorTree operator++(int)
    {
        auto tmp = *this;
        operator++();
        return tmp;
    }

    friend bool operator==(const iteratorTree & lhs, const iteratorTree & rhs)
    {
        return lhs.vector == rhs.vector;
    }

    friend bool operator!=(const iteratorTree & lhs, const iteratorTree & rhs)
    {
        return !(lhs == rhs);
    }
};

namespace rbtree {

class PointSet
{
public:
    using iterator = iteratorTree;

    PointSet(const std::string & filename = {});

    bool empty() const;
    std::size_t size() const;
    void put(const Point &);
    bool contains(const Point &) const;

    // second iterator points to an element out of range
    std::pair<iterator, iterator> range(const Rect &) const;
    iterator begin() const;
    iterator end() const;

    std::optional<Point> nearest(const Point &) const;
    // second iterator points to an element out of range
    std::pair<iterator, iterator> nearest(const Point &, std::size_t) const;

    friend std::ostream & operator<<(std::ostream &, const PointSet &);

private:
    std::set<Point> s;

    static constexpr double MAX_DOUBLE = std::numeric_limits<double>::max();
    const PointSet::iterator END = PointSet::iterator();
};

} // namespace rbtree

namespace kdtree {

class PointSet
{
public:
    class iterator
    {
    private:
        std::vector<std::shared_ptr<Point>> vector;

    public:
        using iterator_category = std::forward_iterator_tag;
        using difference_type = std::ptrdiff_t;
        using value_type = Point;
        using pointer = value_type *;
        using reference = value_type &;

        iterator(const std::vector<std::shared_ptr<Point>> & init = {})
        {
            std::move(init.begin(), init.end(), std::back_inserter(vector));
        }

        reference operator*() const
        {
            return vector.back().operator*();
        }

        pointer operator->() const
        {
            return vector.back().operator->();
        }

        std::shared_ptr<Point> getShared() const
        {
            return vector.back();
        }

        iterator & operator++()
        {
            vector.pop_back();
            return *this;
        }

        iterator operator++(int)
        {
            auto tmp = *this;
            operator++();
            return tmp;
        }

        friend bool operator==(const iterator & lhs, const iterator & rhs)
        {
            return lhs.vector == rhs.vector;
        }

        friend bool operator!=(const iterator & lhs, const iterator & rhs)
        {
            return !(lhs == rhs);
        }
    };

    PointSet(const std::string & filename = {});

    bool empty() const;
    std::size_t size() const;
    void put(const Point &);
    bool contains(const Point &) const;

    std::pair<iterator, iterator> range(const Rect &) const;
    iterator begin() const;
    iterator end() const;

    std::optional<Point> nearest(const Point &) const;
    std::pair<iterator, iterator> nearest(const Point &, std::size_t) const;

    friend std::ostream & operator<<(std::ostream &, const PointSet &);

private:
    struct Node
    {
        std::shared_ptr<Point> p;
        size_t parent = npos,
               left = npos,
               right = npos;
        bool vertical;

        Node(const std::shared_ptr<Point> & point)
            : p(point)
        {
        }
    };

    std::vector<Node> l;

    static constexpr size_t npos = std::numeric_limits<std::size_t>::max();
    static constexpr double MAX_DOUBLE = std::numeric_limits<double>::max();
    const PointSet::iterator END = PointSet::iterator();
};

} // namespace kdtree
