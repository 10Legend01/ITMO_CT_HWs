#include <gtest/gtest.h>
#include "primitives.h"
#include "test_iterator.h"

#include <algorithm>
#include <iostream>
#include <fstream>
#include <set>

template <typename T>
class PointSetTest : public ::testing::Test {
    public:
        void load_data(const std::string & filename)
        {
            try {
                std::ifstream fs(filename);
                ASSERT_TRUE(fs.is_open()) << filename << " wasn't opened";

                double x, y;
                while (fs) {
                    fs >> x >> y;
                    if (fs.fail()) {
                        break;
                    }
                    m_set.put(Point(x, y));
                }

                // std::cout << "Loaded " << m_set.size() << " points.\n";
            } catch(...) {
                std::cout << "Can't read " << filename << ".\n";
            }
        }

        void check_size(size_t correct_size)
        {
            auto it = m_set.begin();
            auto end = m_set.end();
            ASSERT_NE(it, end);
            size_t s = 0;
            while (it != end) {
                ++s;
                ++it;
            }
            ASSERT_EQ(s, correct_size);
        }

        using point_set_t = T;
        using iterator_t = typename T::iterator;
        using set_t = std::set<Point>;

        template<typename ITER>
        set_t to_set(const std::pair<ITER, ITER> & range)
        {
            set_t res;
            std::copy(range.first, range.second, std::inserter(res, res.begin()));
            return res;
        }

        void contains(const set_t & s, const Point & p)
        {
            ASSERT_NE(std::find(s.begin(), s.end(), p), s.end());
        }

        T m_set;

        T& not_empty_container()
        {
            if (m_sample.empty()) {
                m_sample.put(Point(0.1, 0.1));
                m_sample.put(Point(0.13, 0.008));
                m_sample.put(Point(0.11, 0.8));
                m_sample.put(Point(0.1, 0.34));
                m_sample.put(Point(0.5, 0.9));
                m_sample.put(Point(0.8, 0.3));
            }

            return m_sample;
        }

        T m_sample;
};

using TestTypes = ::testing::Types<rbtree::PointSet, kdtree::PointSet>;
//using TestTypes = ::testing::Types<kdtree::PointSet>;
TYPED_TEST_SUITE(PointSetTest, TestTypes);

TEST(PointSetTest, Point)
{
    ASSERT_EQ(Point(1., 2.), Point(1., 2.));
    ASSERT_NE(Point(1., 2.), Point(5., 4.));
    ASSERT_DOUBLE_EQ(Point(0, 0).distance(Point(1, 0)), 1.);
    ASSERT_DOUBLE_EQ(Point(0, 0).distance(Point(0, 1)), 1.);
    ASSERT_DOUBLE_EQ(Point(0, 4).distance(Point(3, 0)), 5.);
}

TEST(PointSetTest, Rect)
{
    Rect r(Point(1., 1.), Point(2., 2.));
    ASSERT_DOUBLE_EQ(r.xmin(), 1.);
    ASSERT_DOUBLE_EQ(r.ymin(), 1.);
    ASSERT_DOUBLE_EQ(r.xmax(), 2.);
    ASSERT_DOUBLE_EQ(r.ymax(), 2.);
    ASSERT_DOUBLE_EQ(r.distance(Point(1., 1.)), 0.);
    ASSERT_DOUBLE_EQ(r.distance(Point(1.5, 1.5)), 0.);
    ASSERT_DOUBLE_EQ(r.distance(Point(0., 1.)), 1.);
    ASSERT_DOUBLE_EQ(r.distance(Point(0., 1.5)), 1.);
    ASSERT_DOUBLE_EQ(r.distance(Point(2., 3.)), 1.);
    ASSERT_DOUBLE_EQ(r.distance(Point(4., 1.2)), 2.);
    ASSERT_DOUBLE_EQ(r.distance(Point(1.1, -1)), 2.);
    ASSERT_TRUE(r.contains(Point(1.5, 1.5)));
    ASSERT_FALSE(r.contains(Point(.9, 1.5)));
    ASSERT_TRUE(r.intersects(Rect(Point(0., 0.), Point(1.5, 1.5))));
    ASSERT_TRUE(r.intersects(Rect(Point(0.5, 0.5), Point(3.5, 3.5))));
    ASSERT_FALSE(r.intersects(Rect(Point(2.1, 0.1), Point(3.5, 1.9))));
}

TYPED_TEST(PointSetTest, ForwardIterator)
{
    //this->load_data("test/etc/test2.dat");
    this->load_data("D:\\Users\\Legend\\2d-tree-10Legend01\\test\\etc\\test2.dat");
    auto & p = this->m_set;

    auto s1 = this->to_set(std::make_pair(p.begin(), p.end()));
    auto s2 = this->to_set(std::make_pair(p.begin(), p.end()));

    ASSERT_EQ(s1.size(), 120);
    ASSERT_EQ(s2.size(), 120);

    ASSERT_TRUE(s1 == s2);

    auto it1 = p.begin(), it2 = p.begin(), end = p.end();
    while (it1 != end) {
        ASSERT_DOUBLE_EQ(it1->x(), it2->x());
        ASSERT_DOUBLE_EQ(it1->y(), it2->y());
        auto it2_copy {it2};
        auto prev_it = it1++;
        ASSERT_NE(prev_it, it1);
        ASSERT_EQ(it2_copy, it2);
        ASSERT_EQ(*it2_copy, *it2);
        ++it2;
    }
    ASSERT_EQ(it2, end);
}

TYPED_TEST(PointSetTest, PointSetMethods)
{
    auto & p = this->m_set;
    ASSERT_TRUE(p.empty());
    ASSERT_EQ(p.size(), 0);

    Point a(0., 0.), b(1., 1.), c(.5, .5);
    p.put(a);
    p.put(b);
    p.put(c);
    ASSERT_FALSE(p.empty());
    ASSERT_EQ(p.size(), 3);
    this->check_size(3);
    ASSERT_TRUE(p.contains(a));
    ASSERT_TRUE(p.contains(b));
    ASSERT_TRUE(p.contains(c));
    ASSERT_FALSE(p.contains(Point(0.5,0)));
}

TYPED_TEST(PointSetTest, PointSetBasicSearch)
{
    auto & ps_write = this->m_set;
    Point a(0., 0.), b(1., 1.), c(.5, .5);
    ps_write.put(a);
    ps_write.put(b);
    ps_write.put(c);
    this->check_size(3);

    const auto & ps_read = this->m_set;
    auto n = ps_read.nearest(Point(.4, .4));
    ASSERT_TRUE(n.has_value());
    ASSERT_EQ(c, *n);

    auto range = ps_read.range(Rect(Point(0.3, 0.3), Point(.7, .7)));
    auto s = this->to_set(range);
    ASSERT_EQ(s.size(), 1);
    this->contains(s, c);
}

TYPED_TEST(PointSetTest, PointSetNearest0)
{
    //this->load_data("test/etc/test0.dat");
    this->load_data("D:\\Users\\Legend\\2d-tree-10Legend01\\test\\etc\\test0.dat");
    auto & p = this->m_set;
    this->check_size(5);

    auto n = p.nearest(Point(.74, .29));
    ASSERT_TRUE(n.has_value());
    ASSERT_EQ(Point(0.725, 0.338), *n);
    this->check_size(5);
}

TYPED_TEST(PointSetTest, PointSetNearest1)
{
    //this->load_data("test/etc/test2.dat");
    this->load_data("D:\\Users\\Legend\\2d-tree-10Legend01\\test\\etc\\test2.dat");
    auto & p = this->m_set;
    this->check_size(120);

    auto n = p.nearest(Point(.712, .567));
    ASSERT_TRUE(n.has_value());
    ASSERT_EQ(Point(0.718, 0.555), *n);
    this->check_size(120);
}

TYPED_TEST(PointSetTest, PointSetNearest1B)
{
    //this->load_data("test/etc/test2.dat");
    this->load_data("D:\\Users\\Legend\\2d-tree-10Legend01\\test\\etc\\test2.dat");
    auto & p = this->m_set;
    this->check_size(120);

    auto n = p.nearest(Point(.712, .567));
    ASSERT_TRUE(n.has_value());
    ASSERT_EQ(Point(0.718, 0.555), *n);
    this->check_size(120);
}

TYPED_TEST(PointSetTest, PointSetRange0)
{
    //this->load_data("test/etc/test1.dat");
    this->load_data("D:\\Users\\Legend\\2d-tree-10Legend01\\test\\etc\\test1.dat");
    auto & p = this->m_set;
    this->check_size(20);

    auto range = p.range(Rect(Point(0.634, 0.276), Point(.818, .42)));

    auto s = this->to_set(range);
    ASSERT_EQ(s.size(), 3);
    this->contains(s, Point(0.655, 0.382));
    this->contains(s, Point(0.725, 0.311));
    this->contains(s, Point(0.794, 0.299));
    this->check_size(20);
}

TYPED_TEST(PointSetTest, PointSetRange0FromFile)
{
    using point_set_t = typename TestFixture::point_set_t;
    //point_set_t p("test/etc/test1.dat");
    point_set_t p("D:\\Users\\Legend\\2d-tree-10Legend01\\test\\etc\\test1.dat");

    auto range = p.range(Rect(Point(0.634, 0.276), Point(.818, .42)));

    auto s = this->to_set(range);
    ASSERT_EQ(s.size(), 3);
    this->contains(s, Point(0.655, 0.382));
    this->contains(s, Point(0.725, 0.311));
    this->contains(s, Point(0.794, 0.299));
}

TYPED_TEST(PointSetTest, PointSetRange1)
{
    //this->load_data("test/etc/test2.dat");
    this->load_data("D:\\Users\\Legend\\2d-tree-10Legend01\\test\\etc\\test2.dat");
    auto & p = this->m_set;
    this->check_size(120);

    auto range = p.range(Rect(Point(0., 0.), Point(1., 1.)));

    auto s = this->to_set(range);
    ASSERT_EQ(s.size(), 120);
    auto it = p.begin();
    auto end = p.end();
    while (it != end) {
        this->contains(s, *it);
        ++it;
    }

    range = p.range(Rect(Point(0., 0.), Point(0., 0.)));
    s = this->to_set(range);
    ASSERT_EQ(s.size(), 0);
    this->check_size(120);
}

TYPED_TEST(PointSetTest, PointSetRange1B)
{
    //this->load_data("test/etc/test2.dat");
    this->load_data("D:\\Users\\Legend\\2d-tree-10Legend01\\test\\etc\\test2.dat");
    auto & p = this->m_set;
    this->check_size(120);

    auto range = p.range(Rect(Point(0., 0.), Point(1., 1.)));

    auto s = this->to_set(range);
    ASSERT_EQ(s.size(), 120);
    auto it = p.begin();
    auto end = p.end();
    while (it != end) {
        this->contains(s, *it);
        ++it;
    }

    range = p.range(Rect(Point(0., 0.), Point(0., 0.)));
    s = this->to_set(range);
    ASSERT_EQ(s.size(), 0);
    this->check_size(120);
}

TYPED_TEST(PointSetTest, PointSetNearestK1)
{
    //this->load_data("test/etc/test2.dat");
    this->load_data("D:\\Users\\Legend\\2d-tree-10Legend01\\test\\etc\\test2.dat");
    const auto & p = this->m_set;
    this->check_size(120);

    auto range = p.nearest(Point(.386, .759), 3);
    auto s = this->to_set(range);
    ASSERT_EQ(s.size(), 3);
    this->contains(s, Point(0.376, 0.767));
    this->contains(s, Point(0.409, 0.754));
    this->contains(s, Point(0.408, 0.728));

    range = p.nearest(Point(.386, .759), 0);
    s = this->to_set(range);
    ASSERT_EQ(s.size(), 0);

    range = p.nearest(Point(.386, .759), 120);
    s = this->to_set(range);
    ASSERT_EQ(s.size(), 120);

    range = p.nearest(Point(.386, .759), 210);
    s = this->to_set(range);
    ASSERT_EQ(s.size(), 120);
    this->contains(s, Point(0.376, 0.767));
    this->contains(s, Point(0.409, 0.754));
    this->contains(s, Point(0.408, 0.728));

    this->check_size(120);
}

TYPED_TEST(PointSetTest, PointSetNearestK1B)
{
    //this->load_data("test/etc/test2.dat");
    this->load_data("D:\\Users\\Legend\\2d-tree-10Legend01\\test\\etc\\test2.dat");
    auto & p = this->m_set;
    this->check_size(120);

    auto range = p.nearest(Point(.386, .759), 3);
    auto s = this->to_set(range);
    ASSERT_EQ(s.size(), 3);
    this->contains(s, Point(0.376, 0.767));
    this->contains(s, Point(0.409, 0.754));
    this->contains(s, Point(0.408, 0.728));

    range = p.nearest(Point(.386, .759), 0);
    s = this->to_set(range);
    ASSERT_EQ(s.size(), 0);

    range = p.nearest(Point(.386, .759), 120);
    s = this->to_set(range);
    ASSERT_EQ(s.size(), 120);

    range = p.nearest(Point(.386, .759), 210);
    s = this->to_set(range);
    ASSERT_EQ(s.size(), 120);
    this->contains(s, Point(0.376, 0.767));
    this->contains(s, Point(0.409, 0.754));
    this->contains(s, Point(0.408, 0.728));

    this->check_size(120);
}

TYPED_TEST(PointSetTest, RangeForwardIterator)
{
    //this->load_data("test/etc/test2.dat");
    this->load_data("D:\\Users\\Legend\\2d-tree-10Legend01\\test\\etc\\test2.dat");
    auto & p = this->m_set;
    this->check_size(120);

    auto range1 = p.range(Rect(Point(0., 0.), Point(1., 1.)));
    auto range3 = p.range(Rect(Point(0.3, 0.3), Point(.7, .7)));
    auto range2 = p.range(Rect(Point(0., 0.), Point(1., 1.)));

    auto s2 = this->to_set(range2);
    auto s3 = this->to_set(range3);
    auto s1 = this->to_set(range1);

    ASSERT_EQ(s1.size(), 120);
    ASSERT_EQ(s2.size(), 120);
    ASSERT_EQ(s1, s2);

    auto [it1, end] = p.range(Rect(Point(0., 0.), Point(1., 1.)));
    auto it2 {it1};

    while (it1 != end) {
        ASSERT_DOUBLE_EQ(it1->x(), it2->x());
        ASSERT_DOUBLE_EQ(it1->y(), it2->y());
        auto prev_it = it1++;
        ASSERT_NE(prev_it, it1);
        ++it2;
    }
    ASSERT_EQ(it2, end);
}

TYPED_TEST(PointSetTest, NearestForwardIterator)
{
    //this->load_data("test/etc/test2.dat");
    this->load_data("D:\\Users\\Legend\\2d-tree-10Legend01\\test\\etc\\test2.dat");
    auto & p = this->m_set;
    this->check_size(120);

    auto range1 = p.nearest(Point(.386, .759), 120);
    auto range3 = p.nearest(Point(.386, .759), 7);
    auto range2 = p.nearest(Point(.386, .759), 120);

    auto s1 = this->to_set(range1);
    auto s3 = this->to_set(range3);
    auto s2 = this->to_set(range2);

    ASSERT_EQ(s1.size(), 120);
    ASSERT_EQ(s2.size(), 120);
    ASSERT_EQ(s3.size(), 7);
    ASSERT_EQ(s1, s2);

    auto [it1, end] = p.nearest(Point(.386, .759), 120);
    auto it2 {it1};

    while (it1 != end) {
        ASSERT_DOUBLE_EQ(it1->x(), it2->x());
        ASSERT_DOUBLE_EQ(it1->y(), it2->y());
        auto prev_it = it1++;
        ASSERT_NE(prev_it, it1);
        ++it2;
    }
}

TYPED_TEST(PointSetTest, NearestPointSetCopy)
{
    //this->load_data("test/etc/test2.dat");
    this->load_data("D:\\Users\\Legend\\2d-tree-10Legend01\\test\\etc\\test2.dat");
    auto & p = this->m_set;
    this->check_size(120);

    auto copy_p {p}; // NOLINT: point set copy is required indeed

    auto range = p.nearest(Point(.386, .759), 120);
    auto range_copy = copy_p.nearest(Point(.386, .759), 120);

    auto s = this->to_set(range);
    auto s_copy = this->to_set(range_copy);

    ASSERT_EQ(s.size(), 120);
    ASSERT_EQ(s_copy.size(), 120);
    ASSERT_EQ(s, s_copy);
}

TYPED_TEST(PointSetTest, MultiThreadIteratorAccess)
{
    //this->load_data("test/etc/test2.dat");
    this->load_data("D:\\Users\\Legend\\2d-tree-10Legend01\\test\\etc\\test2.dat");
    auto & p = this->m_set;
    this->check_size(120);

    using iterator_t = typename TestFixture::iterator_t;

    std::vector<iterator_test::Job<iterator_t>> jobs;
    size_t count = 100;
    double step = 1. / count;
    for (size_t i = 0; i < count; ++i) {
        double l = i * step;
        jobs.emplace_back([&p, l]() { return p.range(Rect(Point(0., 0.), Point(l, l))); }, 
                            iterator_test::test_multipass<iterator_t>);
        jobs.emplace_back([&p, l, i]() { return p.nearest(Point(l, l), i); }, 
                            iterator_test::test_multipass<iterator_t>);
    }
    iterator_test::run_multithread<iterator_t>(jobs);
}

TYPED_TEST(PointSetTest, MultiThreadIteratorAccessLoadFromFile)
{
    using point_set_t = typename TestFixture::point_set_t;
    //point_set_t p("test/etc/test2.dat");
    point_set_t p("D:\\Users\\Legend\\2d-tree-10Legend01\\test\\etc\\test2.dat");

    using iterator_t = typename TestFixture::iterator_t;

    std::vector<iterator_test::Job<iterator_t>> jobs;
    size_t count = 100;
    double step = 1. / count;
    for (size_t i = 0; i < count; ++i) {
        double l = i * step;
        jobs.emplace_back([&p, l, i]() { return p.nearest(Point(l, l), i); }, 
                            iterator_test::test_multipass<iterator_t>);
        jobs.emplace_back([&p, l]() { return p.range(Rect(Point(0., 0.), Point(l, l))); }, 
                            iterator_test::test_multipass<iterator_t>);
    }
    iterator_test::run_multithread<iterator_t>(jobs);
}

using TypesToTest = ::testing::Types<PointSetTest<rbtree::PointSet>, PointSetTest<kdtree::PointSet>>;
INSTANTIATE_TYPED_TEST_SUITE_P(KDTree, IteratorTest, TypesToTest);
