#pragma once

#include <map>
#include <string>
#include <vector>

class Searcher
{
public:
    using Filename = std::string; // or std::filesystem::path

    // index modification
    void add_document(const Filename & filename, std::istream & strm);

    void remove_document(const Filename & filename);

    // queries
    class DocIterator
    {
    private:
        std::vector<const Filename *> vector;

    public:
        using iterator_category = std::forward_iterator_tag;
        using difference_type = std::ptrdiff_t;
        using value_type = const Filename;
        using pointer = value_type *;
        using reference = value_type &;

        void add(const Filename * f)
        {
            vector.emplace_back(f);
        }

        reference operator*() const
        {
            return *vector.back();
        }

        pointer operator->() const
        {
            return vector.back();
        }

        DocIterator & operator++()
        {
            vector.pop_back();
            return *this;
        }

        DocIterator operator++(int)
        {
            auto tmp = *this;
            operator++();
            return tmp;
        }

        friend bool operator==(const DocIterator & lhs, const DocIterator & rhs)
        {
            return lhs.vector == rhs.vector;
        }

        friend bool operator!=(const DocIterator & lhs, const DocIterator & rhs)
        {
            return !(lhs == rhs);
        }
    };

    class BadQuery : public std::exception
    {
    private:
        std::string m_error;

    public:
        BadQuery(const std::string & error)
            : m_error("Search query syntax error: " + error)
        {
        }


        const char* what() const noexcept
        {
            return m_error.c_str();
        }

    };

    std::pair<DocIterator, DocIterator> search(const std::string & query) const;

private:
    std::vector<ptrdiff_t> parse_phrase(const std::vector<std::string> & vector, const std::vector<ptrdiff_t> & a) const;
    std::vector<ptrdiff_t> parse_str(const std::string & s, const std::vector<ptrdiff_t> & a) const;

    std::map<std::string, std::map<size_t, std::vector<ptrdiff_t>>> m_index;

    std::map<size_t, std::string> m_names;
    std::vector<ptrdiff_t> vector_names;

    size_t m_doc = 0;

    const Searcher::DocIterator END = Searcher::DocIterator();
};
