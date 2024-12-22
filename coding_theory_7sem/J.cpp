#include <iostream>
#include <vector>
#include <algorithm>
#include <random>

using namespace std;

uniform_real_distribution<> distribution(0, 1);

class BchCodeBerlekampMassey {
public:
    int n{}, d{}, r, k;
    vector<int> pow_to_poly, poly_to_pow;
    vector<bool> g;
    vector<vector<int>> Mul;

    std::mt19937 gen{228};

    BchCodeBerlekampMassey() {
        int p;
        cin >> n >> p >> d;

        // Ищем 2^m через p - примитивный многочлен поля GF(2^m), который определенно имеет x^m в виде самого "высокого" бита
        int top_border = 1;
        for (int i = p; 1 < i; i /= 2) {
            top_border *= 2;
        }

        // Строим таблицу примитивных элементов поля GF(2^m)
        // a^0 = 1
        // a^i = (a^(i-1)*x) mod p
        // Нам нужно запомнить переходы из многочлена в степень 'i' и наоборот с помощью векторов,
        // чтобы корректно проводить операции умножения и сложения над многочленами поля:
        // Умножение: a^i * a^j = a^(i+j)
        // Сложение: a^i + a^j = (y_(q-1)*x^(q-1) + ... + y_(0)*x^0) + (u_(q-1)*x^(q-1) + ... + u_(1)*x^0) =
        // = ((y_(q-1) + u_(q-1))*x^(q-1) + (y_(0) + u_(0))*x^0) = a^z, где y_(i) и u_(i) равны 0 или 1 при 0<=i<=q-1
        // Исключение - многочлен '0' который не имеет зафиксированной степени, поэтому его степень будет -1
        pow_to_poly.push_back(1);
        poly_to_pow = vector<int>(n + 1);
        poly_to_pow[0] = -1;
        for (int i = 1; i < n; i++) {
            pow_to_poly.push_back(2 * pow_to_poly[i - 1]);
            if (top_border <= pow_to_poly[i]) {
                pow_to_poly[i] ^= p;
            }
            poly_to_pow[pow_to_poly[i]] = i;
        }

        // Строим циклотомические классы C_i при 1<=i<=d-1 относительно умножения на 2 по модулю n,
        // строим только уникальные циклотомические классы для дальнейшего использования
        // Циклотомический класс по модулю N над GF(2) - это множество {s, 2*s, 2^2*s, ..., 2^(t-1)*s}, где 2^t*s = s mod n
        vector<vector<int>> cyc_classes;
        vector<int> used(n);
        used[0] = true;
        for (int i = 1; i < d; i++) {
            if (used[i]) {
                continue;
            }
            cyc_classes.emplace_back();
            int j = i;
            while (!used[j]) {
                used[j] = true;
                cyc_classes.back().emplace_back(j);
                j = (2 * j) % n;
            }
        }

        // Формула для получения g(x) = НОК(M_i(x), 1 = b<=i<=b+d-2 = d-1)
        // b = 1, т.к. по условию нужно реализовать построение **двоичного кода БЧХ в узком смысле**
        // M_i(x) = Mul_{j∈C_i}(x-a^j) - минимальный многочлен элемента i
        // По определению минимальные многочлены или взаимно просты, или совпадают,
        // тогда g(x) - перемножение уникальных минимальных многочленов,
        // уникальные минимальные многочлены образовываются из уникальных циклотомических классов.

        // Вид многочлена в векторе min_polynom на примере:
        // min_polynom[j] = i, где a^i*x^j - то есть значение = степень примитивного элемента a,
        // min_polynom[j] = -1, то 0*x^j, то есть элемента x^j не существует
        // Пример: [0, 0, -1, 2] = 1 + x + a^2*x^3
        vector<int> min_polynom;
        for (auto &cyc_class: cyc_classes) {
            min_polynom.clear();
            min_polynom.emplace_back(0);
            // Строим алгоритм перемножения многочленов вида (x-a^j)
            // Нам не нужно запоминать знак, - или +, т.к. по итогу все значения в векторах будут либо -1 (нет x^i), либо 0 (есть x^i),
            // а в нашем GF(2): -x^i = +x^i
            for (int power: cyc_class) {
                min_polynom.emplace_back(0);
                for (auto i = min_polynom.size() - 2; i > 0; i--) {
                    // Проверка исключения, что степень примитивного элемента существует, то есть степень x существует
                    if (min_polynom[i] != -1) {
                        min_polynom[i] = (min_polynom[i] + power) % n;
                        if (min_polynom[i - 1] != -1) {
                            min_polynom[i] = poly_to_pow[
                                    pow_to_poly[min_polynom[i]] ^ pow_to_poly[min_polynom[i - 1]]
                            ];
                        }
                    } else if (min_polynom[i - 1] != -1) {
                        min_polynom[i] = min_polynom[i - 1];
                    }
                }
                min_polynom[0] = (min_polynom[0] + power) % n;
            }

            // Перемножение матриц
            if (g.empty()) {
                for (auto value: min_polynom) {
                    g.emplace_back(value == 0);
                }
            } else {
                vector<bool> new_g(g.size() + min_polynom.size() - 1);
                for (int i = 0; i < min_polynom.size(); i++) {
                    if (min_polynom[i] != 0) {
                        continue;
                    }
                    for (int j = 0; j < g.size(); j++) {
                        new_g[i + j] = new_g[i + j] ^ g[j];
                    }
                }
                g = new_g;
            }
        }

        r = g.size() - 1;
        k = n - r;

        // Предпосчитаем перемножения многочленов в поле, чтобы использовать при декодинге
        Mul = vector<vector<int>>(n + 1, vector<int>(n + 1));
        for (int i = 1; i <= n; i++) {
            for (int j = 1; j <= i; j++) {
                Mul[j][i] = Mul[i][j] = pow_to_poly[(poly_to_pow[i] + poly_to_pow[j]) % n];
            }
        }
    }

    vector<bool> encode(vector<bool> &inp) {
        // По условию нам нужно реализовать систематическое кодирование:
        // c(x) = x^r*m(x) + (-x^r*m(x) mod g(x)), где m(x) - входной информационный многочлен
        vector<bool> c(n);
        for (int i = r, j = 0; i < n; i++, j++) {
            c[i] = inp[j];
        }

        // Реализация поиска остатка от деления многочлена на многочлен
        // Поиск -x^r*m(x) mod g(x) = x^r*m(x) mod g(x)
        for (auto i = c.size() - 1; i >= r; i--) {
            if (c[i]) {
                for (int j = 0; j <= r; j++) {
                    c[i - j] = c[i - j] ^ g[r - j];
                }
            }
        }

        // Добавление исходной x^r*m(x)
        for (int i = r, j = 0; i < n; i++, j++) {
            c[i] = inp[j];
        }

        return c;
    }

    vector<bool> decode(vector<bool> &inp) {
        auto ans = inp;

        // Вычисление синдрома через схему Горнера
        // В этом алгоритме раскрываем скобочки:
        // S_i = y0 + a^(i+1)*(y1 + a^(i+1)*(y2 + a^(i+1)*(...))) =
        // = y0*(a^(i+1) * 0) + y1*(a^(i+1) * 1) + y2*(a^(i+1) * 2) + ...
        vector<int> s(d);
        for (int j = 0; j < d; j++) {
            for (int i = 0; i < inp.size(); i++) {
                if (inp[i]) {
                    s[j] ^= pow_to_poly[(i * j) % n];
                }
            }
        }

        // Алгоритм Бэрлекэмпа-Месси
        // Итеративно строим последовательность РСЛОС

        // Длина фильтра РСЛОС (регистр сдвига с линейной обратной связью)
        int L = 0;
        // Многочлен локаторов ошибок (многочлен связей?)
        vector<int> lambda = {1};
        // Многочлен компенсации невязки
        vector<int> B = {1};
        int mem_r = 0;
        for (int _r = 1; _r < d; _r++) {
            // delta - невязка
            int delta = 0;
            for (int j = 0; j <= L; j++) {
                delta ^= Mul[lambda[j]][s[_r - j]];
            }
            if (delta != 0) {
                // Временная лямбда, чтобы пока не менять текущую
                auto T = lambda;
                for (auto i = lambda.size(); i < _r - mem_r + B.size(); i++) {
                    T.push_back(0);
                }
                for (int i = 0; i < B.size(); i++) {
                    T[_r - mem_r + i] ^= Mul[delta][B[i]];
                }
                if (2 * L < _r) {
                    // Увеличиваем длину регистра
                    B = lambda;
                    int delta_reverse = pow_to_poly[(n - poly_to_pow[delta]) % n];
                    for (int &i: B) {
                        i = Mul[delta_reverse][i];
                    }
                    L = _r - L;
                    mem_r = _r;
                }
                // Изменили лямбду на новую
                lambda = T;
            }
        }

        // Если не выполняется условие, то исправить ошибки не можем, и возвращаем неправильную строку
        if (L != lambda.size() - 1) {
            return ans;
        }

        // Ищем позиции ошибок lambda(a^-i) == 0 при 0<=i<n и меняем значение в этом случае
        for (int i = 0; i < n; i++) {
            int res = 0;
            for (int j = 0; j < lambda.size(); j++) {
                res ^= Mul[pow_to_poly[(j * i) % n]][lambda[j]];
            }
            if (res == 0) {
                int j = (n - i) % n;
                ans[j] = !inp[j];
            }
        }

        return ans;
    }

    void simulate() {
        double noise_value;
        int simulations_count, max_errors_count;
        cin >> noise_value >> simulations_count >> max_errors_count;

        int errors_count = 0;
        int i = 0;
        while (i++ < simulations_count && errors_count < max_errors_count) {
            auto rand_vector = vector<bool>(k);
            for (int j = 0; j < k; j++) {
                rand_vector[j] = gen() % 2;
            }
            auto test = encode(rand_vector);
            vector<bool> encoded;
            for (auto value: test) {
                // Просто проверяем, что случайное вещественное число от 0 до 1 больше уровня шума
                // Если больше, то бит не меняется, если меньше, то бит инвертируется
                encoded.emplace_back(noise_value < distribution(gen) ? value : !value);
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

    auto berlekamp_massey = BchCodeBerlekampMassey();

    cout << berlekamp_massey.k << '\n';
    for (auto b: berlekamp_massey.g) {
        cout << (b ? 1 : 0) << ' ';
    }
    cout << '\n';

    string cmd;
    while (cin >> cmd) {
        if (cmd == "Encode") {
            vector<bool> input(berlekamp_massey.k);
            for (int i = 0; i < berlekamp_massey.k; ++i) {
                bool inp;
                cin >> inp;
                input[i] = inp;
            }
            for (bool i: berlekamp_massey.encode(input)) {
                cout << i << " ";
            }
            cout << endl;
        } else if (cmd == "Decode") {
            vector<bool> input(berlekamp_massey.n);
            for (int i = 0; i < berlekamp_massey.n; ++i) {
                bool inp;
                cin >> inp;
                input[i] = inp;
            }
            for (bool i: berlekamp_massey.decode(input)) {
                cout << i << " ";
            }
            cout << endl;
        } else if (cmd == "Simulate") {
            berlekamp_massey.simulate();
        }
    }
}
