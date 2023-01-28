#include "searcher.h"

#include <algorithm>
#include <iostream>

std::string trim(const std::string & s)
{
    size_t a, b;
    for (a = 0; a < s.length(); ++a) {
        if (!std::ispunct(s[a])) {
            break;
        }
    }
    if (a == s.length()) {
        return "";
    }
    for (b = s.length() - 1; 0 < b; --b) {
        if (!std::ispunct(s[b])) {
            break;
        }
    }
    return s.substr(a, b - a + 1);
}

void Searcher::add_document(const Searcher::Filename & filename, std::istream & strm)
{
    remove_document(filename);
    m_names.insert({m_doc, filename});
    size_t pos = 0;
    std::string s = "";
    unsigned char c = strm.get();
    while (strm.good()) {
        if (std::isspace(c)) {
            s = trim(s);
            if (!s.empty()) {
                m_index[s][m_doc].emplace_back(pos++);
                s = "";
            }
        }
        else {
            if ('A' <= c && c <= 'Z') {
                c += 32;
            }
            s += c;
        }
        c = strm.get();
    }
    s = trim(s);
    if (!s.empty()) {
        m_index[s][m_doc].emplace_back(pos);
    }
    vector_names.emplace_back(m_doc++);
}

void Searcher::remove_document(const Searcher::Filename & filename)
{
    auto n_it = m_names.begin();
    while (n_it != m_names.end()) {
        if (n_it->second == filename) {
            break;
        }
        n_it++;
    }
    if (n_it == m_names.end()) {
        return;
    }
    size_t doc = n_it->first;
    std::vector<std::string> ers;
    for (auto i = m_index.begin(); i != m_index.end(); ++i) {
        i->second.erase(doc);
        if (i->second.empty()) {
            ers.emplace_back(i->first);
        }
    }
    for (const std::string & i : ers) {
        m_index.erase(i);
    }
    vector_names.erase(vector_names.begin() + std::distance(m_names.begin(), n_it));
    m_names.erase(doc);
}

std::vector<ptrdiff_t> intersection(const std::vector<ptrdiff_t> & a, const std::vector<ptrdiff_t> & b)
{
    std::vector<ptrdiff_t> c;
    size_t i = 0, j = 0;
    while (i < a.size() && j < b.size()) {
        if (a[i] == b[j]) {
            c.emplace_back(a[i]);
            i++;
            j++;
        }
        else if (a[i] < b[j]) {
            i++;
        }
        else {
            j++;
        }
    }
    return c;
}

std::vector<ptrdiff_t> Searcher::parse_phrase(const std::vector<std::string> & vector, const std::vector<ptrdiff_t> & a) const
{
    if (vector.empty()) {
        throw BadQuery("didn't found words in the phrase with '\"'.");
    }
    if (vector.size() == 1) {
        return parse_str(vector[0], a);
    }
    auto fi_iter = m_index.find(vector[0]);
    if (fi_iter == m_index.end()) {
        return {};
    }
    auto first = fi_iter->second;
    std::vector<ptrdiff_t> ans;
    size_t curr_doc = 0;
    for (auto i = first.begin(); i != first.end(); ++i) {
        ptrdiff_t doc = i->first;
        while (curr_doc < a.size() && a[curr_doc] < doc) {
            curr_doc++;
        }
        if (curr_doc == a.size()) {
            break;
        }
        if (a[curr_doc] > doc) {
            continue;
        }
        curr_doc++;
        bool good = true;
        std::vector<ptrdiff_t> inter1 = i->second;
        for (size_t k = 1; k < vector.size(); ++k) {
            auto it = m_index.find(vector[k]);
            if (it == m_index.end()) {
                return {};
            }
            auto curr_map = it->second;
            auto curr = curr_map.find(doc);
            if (curr == curr_map.end()) {
                good = false;
                break;
            }
            std::vector<ptrdiff_t> inter2 = curr->second;
            for (size_t j = 0; j < inter2.size(); ++j) {
                inter2[j] -= k;
            }
            inter1 = intersection(inter1, inter2);
        }
        if (!good || inter1.empty()) {
            continue;
        }
        ans.emplace_back(doc);
    }
    return ans;
}

std::vector<ptrdiff_t> Searcher::parse_str(const std::string & s, const std::vector<ptrdiff_t> & a) const
{
    auto fi_iter = m_index.find(s);
    if (fi_iter == m_index.end()) {
        return {};
    }
    auto first = fi_iter->second;
    std::vector<ptrdiff_t> ans;
    size_t curr_doc = 0;
    for (auto i = first.begin(); i != first.end(); ++i) {
        ptrdiff_t doc = i->first;
        while (curr_doc < a.size() && a[curr_doc] < doc) {
            curr_doc++;
        }
        if (curr_doc == a.size()) {
            break;
        }
        if (a[curr_doc] > doc) {
            continue;
        }
        ans.emplace_back(doc);
    }
    return ans;
}

bool empty_text(const std::string & query)
{
    size_t a;
    for (a = 0; a < query.length(); ++a) {
        if (!std::ispunct(query[a]) && !std::isspace(query[a])) {
            break;
        }
    }
    if (a == query.length()) {
        return true;
    }
    return false;
}

std::pair<Searcher::DocIterator, Searcher::DocIterator> Searcher::search(const std::string & query) const
{
    if (empty_text(query)) {
        throw BadQuery("didn't found words in the text.");
    }
    std::vector<ptrdiff_t> vector = vector_names;
    std::string s = "";
    bool phrase = false;
    std::vector<std::string> vector_phrase;
    for (size_t i = 0; i < query.length(); ++i) {
        unsigned char c = query[i];
        if (c == '"') {
            s = trim(s);
            if (phrase) {
                if (!s.empty()) {
                    vector_phrase.emplace_back(s);
                    s = "";
                }
                vector = parse_phrase(vector_phrase, vector);
                phrase = false;
                vector_phrase.clear();
            }
            else {
                if (!s.empty()) {
                    vector = parse_str(s, vector);
                    s = "";
                }
                phrase = true;
            }
        }
        else if (std::isspace(c)) {
            s = trim(s);
            if (s.empty()) {
                continue;
            }
            if (phrase) {
                vector_phrase.emplace_back(s);
            }
            else {
                vector = parse_str(s, vector);
            }
            s = "";
        }
        else {
            if ('A' <= c && c <= 'Z') {
                c += 32;
            }
            s += c;
        }
    }
    if (phrase) {
        throw BadQuery("didn't found '\"'.");
    }
    s = trim(s);
    if (!s.empty()) {
        vector = parse_str(s, vector);
    }
    Searcher::DocIterator start = END;
    for (size_t i = 0; i < vector.size(); ++i) {
        start.add(&m_names.find(vector[i])->second);
    }
    return {start, END};
}
