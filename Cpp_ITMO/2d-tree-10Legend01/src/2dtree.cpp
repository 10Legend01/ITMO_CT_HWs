#include "primitives.h"

#include <algorithm>
#include <cmath>
#include <fstream>
#include <vector>

Point::Point(double x, double y)
    : X(x)
    , Y(y)
{
}

double Point::x() const
{
    return X;
}
double Point::y() const
{
    return Y;
}
double Point::distance(const Point & p) const
{
    return std::sqrt(std::pow(p.x() - X, 2) + std::pow(p.y() - Y, 2));
}

bool Point::operator<(const Point & p) const
{
    return X < p.x() || (X == p.x() && Y < p.y());
}
bool Point::operator>(const Point & p) const
{
    return X > p.x() || (X == p.x() && Y > p.y());
}
bool Point::operator<=(const Point & p) const
{
    return !(*this > p);
}
bool Point::operator>=(const Point & p) const
{
    return !(*this < p);
}
bool Point::operator==(const Point & p) const
{
    return X == p.x() && Y == p.y();
}
bool Point::operator!=(const Point & p) const
{
    return !(*this == p);
}

std::ostream & operator<<(std::ostream & strm, const Point & p)
{
    return strm << "{" << p.x() << ", " << p.y() << "}";
}

Rect::Rect(const Point & left_bottom, const Point & right_top)
    : lb(left_bottom)
    , rt(right_top)
{
}

double Rect::xmin() const
{
    return lb.x();
}
double Rect::ymin() const
{
    return lb.y();
}
double Rect::xmax() const
{
    return rt.x();
}
double Rect::ymax() const
{
    return rt.y();
}
double Rect::distance(const Point & p) const
{
    if (contains(p)) {
        return 0;
    }
    if (xmin() <= p.x() && p.x() <= xmax()) {
        return std::min(p.distance(Point(p.x(), ymin())), p.distance(Point(p.x(), ymax())));
    }
    if (ymin() <= p.y() && p.y() <= ymax()) {
        return std::min(p.distance(Point(ymin(), p.y())), p.distance(Point(ymax(), p.y())));
    }
    return std::min({p.distance(lb), p.distance(rt), p.distance(Point(xmin(), ymax())), p.distance(Point(xmax(), ymin()))});
}

bool Rect::contains(const Point & p) const
{
    return xmin() <= p.x() && p.x() <= xmax() && ymin() <= p.y() && p.y() <= ymax();
}
bool Rect::intersects(const Rect & r) const
{
    return !(ymax() < r.ymin() || ymin() > r.ymax() || xmin() > r.xmax() || xmax() < r.xmin());
}

namespace rbtree {

PointSet::PointSet(const std::string & filename)
{
    if (filename.empty()) {
        return;
    }
    try {
        std::ifstream fs(filename);

        double x, y;
        while (fs) {
            fs >> x >> y;
            if (fs.fail()) {
                break;
            }
            PointSet::put(Point(x, y));
        }
    }
    catch (...) {
        std::cout << "Can't read " << filename << ".\n";
    }
}

bool PointSet::empty() const
{
    return s.empty();
}
std::size_t PointSet::size() const
{
    return s.size();
}
void PointSet::put(const Point & p)
{
    s.insert(p);
}
bool PointSet::contains(const Point & p) const
{
    return s.find(p) != s.end();
}
std::pair<PointSet::iterator, PointSet::iterator> PointSet::range(const Rect & r) const
{
    std::vector<const Point *> v;
    for (auto it = begin(); it != end(); ++it) {
        if (r.contains(*it)) {
            v.emplace_back(it.operator->());
        }
    }
    return {PointSet::iterator(v), PointSet::end()};
}
PointSet::iterator PointSet::begin() const
{
    return PointSet::iterator(s);
}
PointSet::iterator PointSet::end() const
{
    return END;
}
std::optional<Point> PointSet::nearest(const Point & p) const
{
    if (empty()) {
        return std::optional<Point>();
    }
    double curr_dist = MAX_DOUBLE;
    PointSet::iterator curr;
    for (auto i = PointSet::begin(); i != PointSet::end(); ++i) {
        if (p.distance(*i) < curr_dist) {
            curr_dist = p.distance(*i);
            curr = i;
        }
    }
    return std::optional<Point>(*curr);
}
std::pair<PointSet::iterator, PointSet::iterator> PointSet::nearest(const Point & p, std::size_t k) const
{
    if (k == 0) {
        return {PointSet::end(), PointSet::end()};
    }
    k = std::min(k, PointSet::size());
    std::vector<const Point *> v(k);
    std::vector<double> d(k, MAX_DOUBLE);
    for (auto i = begin(); i != end(); ++i) {
        size_t m = k;
        double curr = p.distance(*i);
        for (size_t j = 0; j < k; ++j) {
            if (curr < d[j]) {
                m = j;
                break;
            }
        }
        for (size_t j = m; j < k; ++j) {
            if (d[m] < d[j]) {
                m = j;
            }
        }
        if (m < k) {
            v[m] = i.operator->();
            d[m] = curr;
        }
    }
    return {PointSet::iterator(v), PointSet::end()};
}
std::ostream & operator<<(std::ostream & strm, const PointSet & ps)
{
    for (auto i = ps.begin(); i != ps.end(); ++i) {
        strm << *i << "\n";
    }
    return strm;
}

} // namespace rbtree

namespace kdtree {

//bool ycomp(const Point * p1, const Point * p2)
bool ycomp(Point *& p1, Point *& p2)
{
    return p1->y() < p2->y();
    //return p1.y() < p2.y();
}

//bool xcomp(const Point * p1, const Point * p2)
bool xcomp(Point *& p1, Point *& p2)
{
    return p1->x() < p2->x();
    //return p1.x() < p2.x();
}

PointSet::PointSet(const std::string & filename)
{
    if (filename.empty()) {
        return;
    }
    std::vector<Point> inc;
    try {
        std::ifstream fs(filename);
        double x, y;
        while (fs) {
            fs >> x >> y;
            if (fs.fail()) {
                break;
            }
            inc.emplace_back(Point(x, y));
        }
    }
    catch (...) {
        std::cout << "Can't read " << filename << ".\n";
        return;
    }
    std::vector<Point *> v(inc.size());
    for (size_t i = 0; i < inc.size(); i++) {
        v[i] = &inc[i];
    }
    std::sort(v.begin(), v.end(), xcomp);
    //auto q = v.begin().operator*();
    //Point *& a = q;
    for (size_t i = 1; i < v.size();) {
        if (*v[i - 1] == *v[i]) {
            v.erase(std::next(v.begin(), i));
        }
        else {
            i++;
        }
    }
    std::vector<std::pair<size_t, bool>> zn;
    zn.emplace_back(std::make_pair(v.size(), true));
    while (!v.empty()) {
        size_t right = zn.back().first;
        if (right <= 2) {
            while (right != 0) {
                PointSet::put(*v[0]);
                v.erase(v.begin());
                right--;
            }
            zn.pop_back();
            continue;
        }
        auto comp = zn.back().second ? ycomp : xcomp;
        size_t m = right / 2 + 1;
        PointSet::put(*v[m]);
        v.erase(std::next(v.begin(), m));
        zn.back().first -= m;
        zn.back().second = !zn.back().second;
        zn.emplace_back(std::make_pair(m - 1, zn[zn.size() - 1].second));
        std::sort(v.begin(), std::next(v.begin(), m), comp);
        std::sort(std::next(v.begin(), m), std::next(v.begin(), right - 1), comp);
    }
}

bool PointSet::empty() const
{
    return l.empty();
}
std::size_t PointSet::size() const
{
    return l.size();
}
void PointSet::put(const Point & p)
{
    if (contains(p)) {
        return;
    }
    l.emplace_back(Node(std::make_shared<Point>(p)));
    if (size() == 1) {
        l.front().vertical = true;
        return;
    }
    size_t curr = 0;
    while (true) {
        bool left = l[curr].vertical ? p.x() <= l[curr].p->x() : p.y() <= l[curr].p->y();
#define route(left) (left ? l[curr].left : l[curr].right)
        if (route(left) == npos) {
            route(left) = l.size() - 1;
            l.back().parent = curr;
            l.back().vertical = !l[curr].vertical;
            break;
        }
        curr = route(left);
    }
}
bool PointSet::contains(const Point & p) const
{
    if (empty()) {
        return false;
    }
    size_t curr = 0;
    while (curr != npos) {
        if (p == l[curr].p.operator*()) {
            return true;
        }
        bool left = l[curr].vertical ? p.x() <= l[curr].p->x() : p.y() <= l[curr].p->y();
        curr = route(left);
    }
    return false;
}

std::pair<PointSet::iterator, PointSet::iterator> PointSet::range(const Rect & r) const
{
    std::vector<std::shared_ptr<Point>> v;
    for (auto it = PointSet::begin(); it != PointSet::end(); ++it) {
        if (r.contains(*it)) {
            v.emplace_back(it.getShared());
        }
    }
    return {PointSet::iterator(v), PointSet::end()};
}
PointSet::iterator PointSet::begin() const
{
    std::vector<std::shared_ptr<Point>> v;
    size_t curr = 0;
    std::vector<bool> f(size(), false);
    while (true) {
        if (l[curr].left != npos && !f[l[curr].left]) {
            curr = l[curr].left;
            continue;
        }
        if (l[curr].right != npos && !f[l[curr].right]) {
            curr = l[curr].right;
            continue;
        }
        v.emplace_back(l[curr].p);
        if (l[curr].left != npos) {
            f[l[curr].left] = false;
        }
        if (l[curr].right != npos) {
            f[l[curr].right] = false;
        }
        if (l[curr].parent == npos) {
            break;
        }
        f[curr] = true;
        curr = l[curr].parent;
    }
    return PointSet::iterator(v);
}
PointSet::iterator PointSet::end() const
{
    return END;
}

std::optional<Point> PointSet::nearest(const Point & p) const
{
    if (empty()) {
        return std::optional<Point>();
    }
    double curr_dist = MAX_DOUBLE;
    PointSet::iterator curr;
    for (auto i = PointSet::begin(); i != PointSet::end(); ++i) {
        if (p.distance(*i) < curr_dist) {
            curr_dist = p.distance(*i);
            curr = i;
        }
    }
    return std::optional<Point>(*curr);
}
std::pair<PointSet::iterator, PointSet::iterator> PointSet::nearest(const Point & p, std::size_t k) const
{
    if (k == 0) {
        return {PointSet::end(), PointSet::end()};
    }
    k = std::min(k, PointSet::size());
    std::vector<std::shared_ptr<Point>> v(k);
    std::vector<double> d(k, MAX_DOUBLE);
    for (auto i = PointSet::begin(); i != PointSet::end(); ++i) {
        size_t m = k;
        double curr = p.distance(*i);
        for (size_t j = 0; j < k; ++j) {
            if (curr < d[j]) {
                m = j;
                break;
            }
        }
        for (size_t j = m; j < k; ++j) {
            if (d[m] < d[j]) {
                m = j;
            }
        }
        if (m < k) {
            v[m] = i.getShared();
            d[m] = curr;
        }
    }
    return {PointSet::iterator(v), PointSet::end()};
}

std::ostream & operator<<(std::ostream & strm, const PointSet & ps)
{
    for (auto i = ps.begin(); i != ps.end(); ++i) {
        strm << *i << "\n";
    }
    return strm;
}

} // namespace kdtree
