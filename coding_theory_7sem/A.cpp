#include <iostream>
#include <vector>
#include <algorithm>
#include <queue>
#include <random>
#include <set>
#include <map>

using namespace std;

class LinearCodeViterbi {
public:
    int n{}, k{};
    vector<vector<bool>> GT;

    vector<vector<pair<bool, int>>> nodes;
    vector<int> start_pos_layers;

    std::mt19937 gen{228};

    LinearCodeViterbi() {
        cin >> n >> k;

        vector<vector<bool>> G = vector<vector<bool>>(k, vector<bool>(n));
        GT = vector<vector<bool>>(n, vector<bool>(k));
        for (int i = 0; i < k; i++) {
            for (int j = 0; j < n; j++) {
                bool a;
                cin >> a;
                G[i][j] = a;
                GT[j][i] = a;
            }
        }

        // Структуры для хранения начала и конца активных элементов
        // Хранится в виде <столбец, строка>
        queue<pair<int, int>> starts;
        vector<pair<int, int>> ends;

        // нужно привести к МСФ - начала строк различны и все концы строк различны
        // Первый этап привода к минимальной спэновой форме матрицы через метод гаусса
        for (int i = 0, j = 0; i < k && j < n; i++, j++) {
            int new_i;
            do {
                new_i = i;
                // Ищем первую строку (без первых зафиксированных строк), где есть единица на этом столбце
                while (new_i < k && !G[new_i][j]) {
                    new_i++;
                }
                // Если не нашли единицу на столбце, то переходим к следующему столбцу
                if (new_i >= k) {
                    j++;
                }
            } while (new_i >= k && j < n);
            // Если мы уже вышли за возможные столбцы при поиске единицы, то выходим
            if (j >= n) {
                break;
            }
            // запоминаем столбец начала активных элементов этой строки (первая единица строки)
            starts.emplace(j, i);
            // Если нашли единицу не на той строке, которой ожидали, то переносим единицу наверх, чтобы построить лесенку
            if (i != new_i) {
                for (int q = 0; q < n; q++) {
                    G[i][q] = G[i][q] ^ G[new_i][q];
                }
            }
            // Избавляемся от всех единиц снизу
            for (new_i = i + 1; new_i < k; new_i++) {
                if (G[new_i][j]) {
                    for (int q = 0; q < n; q++) {
                        G[new_i][q] = G[new_i][q] ^ G[i][q];
                    }
                }
            }
        }

        // Второй этап привода к МСФ

        // Здесь нам нужно хранить множество еще не измененных строк, так как мы не хотим просто перемещать первую
        // найденную единицу сверху вниз, как это сделано на первом этапе (снизу вверх), т.к. будет выглядеть некрасиво
        // Создание своего компаратора, чтобы идти с конца к началу.
        // Мы это делаем, чтобы найти первую единицу снизу, и убрать все единицы сверху в том же столбце,
        // так мы гарантируем, что начала строк не изменятся, когда их концы будут меняться.
        auto cmp = [](int a, int b) {
            return b < a;
        };
        set<int, decltype(cmp)> rows(cmp);
        for (int i = 0; i < k; i++) {
            rows.insert(i);
        }
        for (int j = n - 1; j >= 0; j--) {
            int i = -1;
            for (auto row: rows) {
                if (G[row][j]) {
                    i = row;
                    break;
                }
            }
            if (i == -1) {
                continue;
            }
            ends.emplace_back(j, i);
            rows.erase(i);
            // Здесь мы удаляем все единицы в этом столбце сверху от выбранной строки. Снизу не смотрим, т.к. там одни нули на активных строках
            for (int l = 0; l < i; l++) {
                if (G[l][j]) {
                    for (int q = 0; q < n; q++) {
                        G[l][q] = G[l][q] ^ G[i][q];
                    }
                }
            }
        }

        // На всякий случай добавлены пустышки (несуществующие строки), чтобы случайно не вылетел алгоритм на пустых структурах данных
        starts.emplace(n, k);
        ends.emplace_back(n, k);
        // Сортируем концы активных элементов строк по столбцам в порядке убывания, чтобы в конце был самый низкий номер столбца
        // т.к. в векторе удаляются элементы с конца, удлять будем в порядке возрастания
        sort(ends.begin(), ends.end(), [](const pair<int, int> &x, const pair<int, int> &y) {
            return x.first > y.first;
        });


        // Строим решетку

        // Узлы решетки, узлы идут в порядке построения новых слоев
        nodes.emplace_back();
        // Началы индексов каждого следующего слоя решетки
        start_pos_layers.push_back(1);
        map<vector<bool>, int> past_layer = {{{}, 0}};
        // Храним множество номеров строк с активными элементами на рассматриваемом столбце
        set<int> actives_G;
        for (int i = 0; i < n; i++) {
            vector<bool> active_elements_row;
            for (int active_row: actives_G) {
                active_elements_row.emplace_back(G[active_row][i]);
            }

            bool have_new = starts.front().first == i;
            if (have_new) {
                active_elements_row.emplace_back(G[starts.front().second][i]);
            }
            bool have_old = ends.back().first == i;

            map<vector<bool>, int> cur_layer;
            for (const auto &node: past_layer) {
                auto actives_node = node.first;
                vector<bool> actives_new_node;
                int j = 0;
                // Избавляемся от значения со старой строки (теперь уже неактивного элемента)
                for (int active_G: actives_G) {
                    if (have_old && ends.back().second == active_G) {
                        j++;
                        continue;
                    }
                    actives_new_node.push_back(actives_node[j++]);
                }
                if (have_new) {
                    actives_node.emplace_back(false);
                    actives_new_node.emplace_back(false);
                }

                // Считаем значение ребра путем перемножения вектора активных значений в узле с вектором активных
                // элементов столбца порождающей матрицы
                bool edge_value = false;
                for (size_t q = 0; q < actives_node.size(); q++) {
                    edge_value ^= actives_node[q] && active_elements_row[q];
                }

                // Ищем существующий узел, если есть, то добавляем к нему новое ребро, иначе создаем узел с новым ребром
                auto it = cur_layer.find(actives_new_node);
                if (it != cur_layer.end()) {
                    nodes[it->second].emplace_back(edge_value, node.second);
                    if (have_new) {
                        actives_new_node.back() = true;
                        nodes[cur_layer[actives_new_node]].emplace_back(!edge_value, node.second);
                    }
                } else {
                    nodes.push_back({{edge_value, node.second}});
                    cur_layer.insert({actives_new_node, nodes.size() - 1});
                    if (have_new) {
                        actives_new_node.back() = true;
                        nodes.push_back({{!edge_value, node.second}});
                        cur_layer.insert({actives_new_node, nodes.size() - 1});
                    }
                }
            }

            if (have_new) {
                actives_G.insert(starts.front().second);
                starts.pop();
            }
            if (have_old) {
                actives_G.erase(ends.back().second);
                ends.pop_back();
            }

            past_layer = cur_layer;
            start_pos_layers.push_back((int) nodes.size());
        }
    }

    vector<bool> encode(vector<bool> &inp) {
        // Реализуем несистематическое кодирование, то есть считаем закодированный вектор через перемножение
        // информационного вектора с порождающей матрицей
        vector<bool> ans(n);
        for (int i = 0; i < n; i++) {
            bool res = false;
            for (int j = 0; j < k; j++) {
                res ^= inp[j] && GT[i][j];
            }
            ans[i] = res;
        }
        return ans;
    }

    vector<bool> decode(vector<double> &inp) {
        // Реализуем динамическое программирование, считаем наименьшую возможную ошибку для узла и запоминаем из
        // какого узла прошлого слоя взяли минимальное значение
        pair<bool, int> from_node;
        vector<pair<double, pair<bool, int>>> dist(start_pos_layers.back());

        for (int l = 0; l < n; l++) {
            for (int node_id = start_pos_layers[l]; node_id < start_pos_layers[l + 1]; node_id++) {
                double min_error = 1e9;
                for (const auto node: nodes[node_id]) {
                    // Корреляционная метрика. Максимум правдободобия по скалярному произведению
                    double cur_error = dist[node.second].first + inp[l] * (node.first ? 1 : 0);
                    if (cur_error < min_error) {
                        min_error = cur_error;
                        from_node = node;
                    }
                }
                dist[node_id] = {min_error, from_node};
            }
        }

        // Строим путь
        auto cur = start_pos_layers.back() - 1;
        vector<bool> ans(n);
        for (int i = n - 1; i >= 0; i--) {
            ans[i] = dist[cur].second.first;
            cur = dist[cur].second.second;
        }
        return ans;
    }

    void simulate() {
        double noise_value;
        int simulations_count, max_errors_count;
        cin >> noise_value >> simulations_count >> max_errors_count;

        // sigma^2 = N0/2
        // sigma = sqrt(N0/2)
        // Eb = Es/R = a^2/(k/n) = n/k, поставим a=1
        // noise_value = 10 * lg(Eb/N0)

        // Eb/N0 = 10^(noise_value / 10)
        // 1/N0 = 10^(noise_value / 10) * k / n
        // N0 = 1 / (10^(noise_value / 10) * k / n)
        // sigma = sqrt(1/2 / (10^(noise_value / 10) * k / n))
        normal_distribution<> distribution{0, sqrt(0.5 / (pow(10, noise_value / 10) * k / n))};

        int errors_count = 0;
        int i = 0;
        while (i++ < simulations_count && errors_count < max_errors_count) {
            auto rand_vector = vector<bool>(k);
            for (int j = 0; j < k; j++) {
                rand_vector[j] = gen() % 2;
            }
            auto test = encode(rand_vector);
            vector<double> encoded;

            // y_i = a(2*x_i − 1) + n_i
            // a - уже поставили a=1
            // x_i - значение бита (0 или 1)
            // n_i - значение из нормального распределения
            for (auto value: test) {
                encoded.emplace_back((value ? -1.0 : 1.0) + distribution(gen));
            }
            auto check = decode(encoded);
            if (test != check) {
                errors_count++;
            }
        }
        cout << (double) errors_count / i << endl;
    }
};

int main() {
    freopen("input.txt", "r", stdin);
    freopen("output.txt", "w", stdout);

    auto viterbi = LinearCodeViterbi();

    cout << 1 << ' ';
    for (int i = 1; i < viterbi.start_pos_layers.size(); i++) {
        cout << viterbi.start_pos_layers[i] - viterbi.start_pos_layers[i - 1] << ' ';
    }
    cout << endl;

    string cmd;
    while (cin >> cmd) {
        if (cmd == "Encode") {
            vector<bool> input(viterbi.k);
            for (int i = 0; i < viterbi.k; ++i) {
                bool inp;
                cin >> inp;
                input[i] = inp;
            }
            for (bool i: viterbi.encode(input)) {
                cout << i << " ";
            }
            cout << endl;
        } else if (cmd == "Decode") {
            vector<double> input(viterbi.n);
            for (int i = 0; i < viterbi.n; ++i) {
                cin >> input[i];
            }
            for (bool i: viterbi.decode(input)) {
                cout << i << " ";
            }
            cout << endl;
        } else if (cmd == "Simulate") {
            viterbi.simulate();
        }
    }
}
