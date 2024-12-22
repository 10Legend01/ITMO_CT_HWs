#include <iostream>
#include <vector>
#include <algorithm>
#include <random>

using namespace std;

uniform_real_distribution<> distribution(0, 1);

class ReedSolomonCodeSugiyama {
public:
    int n{}, d{}, r, k;
    vector<int> pow_to_poly, poly_to_pow;
    vector<int> g;
    vector<vector<int>> Mul;

    std::mt19937 gen{410};

    uniform_int_distribution<> new_element;

    ReedSolomonCodeSugiyama() {
        // Код Рида-Соломона - Код БЧХ длины n = q-1 над GF(q)
        int p;
        cin >> n >> p >> d;

        new_element = uniform_int_distribution<int>(1, n);

        int top_border = 1;
        for (int i = p; 1 < i; i /= 2) {
            top_border *= 2;
        }

        // Вычисляем поле
        // Сложение и вычитание определено через XOR: a^i + a^j = a^i - a^j = a^i xor a^j
        // Умножение и деление через сложение и вычитание степеней соответственно по модулю n
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

        // Для кодов Рида-Соломона в узком смысле g(x) = mul_{i=1..d-1}(x-a^i)
        // Все минимальные многочлены взаимно просты, т.к. минимальный многочлен для a in GF(q) над GF(q): M_a(x) = x - a
        // В g хранятся степени поля: g[i] = j <=> a^j*x^i для j>=0; g[i] = -1 <=> x^i не существует
        g = {0};
        for (int power = 1; power < d; power++) {
            g.emplace_back(0);
            for (auto i = g.size() - 2; i > 0; i--) {
                if (g[i] != -1) {
                    g[i] = (g[i] + power) % n;
                    if (g[i - 1] != -1) {
                        g[i] = poly_to_pow[
                                pow_to_poly[g[i]] ^ pow_to_poly[g[i - 1]]
                        ];
                    }
                } else if (g[i - 1] != -1) {
                    g[i] = g[i - 1];
                }
            }
            g[0] = (g[0] + power) % n;
        }
        // Переводим из степеней в значения
        for (int &i: g) {
            if (i == -1) {
                i = 0;
            } else {
                i = pow_to_poly[i];
            }
        }

        r = g.size() - 1;
        k = n - r;

        Mul = vector<vector<int>>(n + 1, vector<int>(n + 1));
        for (int i = 1; i <= n; i++) {
            for (int j = 1; j <= i; j++) {
                Mul[j][i] = Mul[i][j] = pow_to_poly[(poly_to_pow[i] + poly_to_pow[j]) % n];
            }
        }
    }

    static void normalize_poly(vector<int> &a) {
        while (!a.empty() && a.back() == 0) {
            a.pop_back();
        }
    }

    vector<int> mul_polys(const vector<int> &a, const vector<int> &b) {
        vector<int> res(a.size() + b.size() - 1);
        for (int i = 0; i < a.size(); ++i) {
            if (a[i]) {
                for (int j = 0; j < b.size(); ++j) {
                    res[i + j] ^= Mul[a[i]][b[j]];
                }
            }
        }
        normalize_poly(res);
        return res;
    }

    vector<int> add_polys(const vector<int> &big, const vector<int> &small) {
        if (big.size() < small.size()) {
            return add_polys(small, big);
        }
        vector<int> res = big;
        for (int i = (int) small.size() - 1; 0 <= i; --i) {
            res[i] ^= small[i];
        }
        normalize_poly(res);
        return res;
    }

    pair<vector<int>, vector<int>> div_polys(const vector<int> &a, const vector<int> &b) {
        vector<int> result, rest = a;

        for (int i = (int) rest.size() - 1; (int) b.size() - 1 <= i; i--) {
            if (rest[i]) {
                int mul_on = pow_to_poly[(poly_to_pow[rest[i]] - poly_to_pow[b.back()] + n) % n];
                for (int j = 0; j <= b.size() - 1; j++) {
                    rest[i - j] ^= Mul[b[(int) b.size() - 1 - j]][mul_on];
                }
                result.push_back(mul_on);
            } else {
                result.push_back(0);
            }
        }

        reverse(result.begin(), result.end());
        normalize_poly(rest);
        return {result, rest};
    }

    int poly_eval(vector<int> &poly, int x) {
        int res = 0;
        for (int i = 0; i < poly.size(); i++) {
            res ^= Mul[pow_to_poly[(i * poly_to_pow[x]) % n]][poly[i]];
        }
        return res;
    }

    vector<int> encode(const vector<int> &inp) {
        // Систематическое кодирование: c = x^r*y + (x^r*y mod g)
        vector<int> c(n);
        for (int i = r, j = 0; i < n; i++, j++) {
            c[i] = inp[j];
        }

        auto pre_c = div_polys(c, g).second;

        for (int i = 0; i < r; i++) {
            if (pre_c.size() == i) {
                break;
            }
            c[i] = pre_c[i];
        }

        return c;
    }

    vector<int> decode(const vector<int> &inp) {
        auto ans = inp;

        // Вычисляем синдром S = (S_0, ..., S_(d-1)) через схему Горнера
        // S_i = y0 + a^(i+1)*(y1 + a^(i+1)*(y2 + a^(i+1)*(...)))
        // S_i = sum_{l=1..omega}(y_l * X_l^(b+l))
        vector<int> s(d - 1);
        for (int j = 1; j < d; j++) {
            for (int i = 0; i < inp.size(); i++) {
                if (inp[i]) {
                    s[j - 1] ^= pow_to_poly[(poly_to_pow[inp[i]] + i * j) % n];
                }
            }
        }

        // Алгоритм Сугиямы
        vector<int> u(1, 1), u_prime, a = s, b(d);
        normalize_poly(a);
        b[d - 1] = 1;
        while ((int) a.size() - 1 >= (d - 1) / 2) {
            auto res = div_polys(b, a);
            auto q = std::move(res.first), _r = std::move(res.second);
            auto v = add_polys(mul_polys(q, u), u_prime);
            u_prime = std::move(u);
            u = std::move(v);
            b = std::move(a);
            a = std::move(_r);
        }

        // Ключевое уравнение - gamma(x) = lambda(x) * S(x) mod x^(d-1) - многочлен значений ошибок
        // lambda(x) = mul_{l=1..omega}(1-X_l*x) = 1 + sum_{l=1..omega}(lambda_l * x^l) - многочлен локаторов
        auto gamma = a, lambda = u;
        if (lambda[0] > 1) {
            // Нормализуем так, чтобы lambda(0) = 1
            // constant(x) = lambda(x) / lambda'(x) = gamma(x) / gamma'(x)
            auto constant = pow_to_poly[(n - poly_to_pow[lambda[0]])];
            for (auto &value: gamma) {
                value = Mul[constant][value];
            }
            for (auto &value: lambda) {
                value = Mul[constant][value];
            }
        }

        // По процедуре Ченя ищем корни X многочлена lambda(x) = mul_{l=1..omega}(1 - X_l*x)
        // Перебираем a^i: 0<=i<n, и если lambda((a^i)^-1) = 0, то i - позиция ошибки
        vector<int> errors_pos;
        for (int i = 0; i < n; i++) {
            int i_inv = (n - i) % n;
            auto res = poly_eval(lambda, pow_to_poly[i_inv]);
            if (res == 0) {
                errors_pos.push_back(i);
            }
        }

        // Ищем значения ошибок методом Форни:
        // Y_l = X_i^-1 * gamma(X_i^-1) / mul_{j != i}(1 - X_j*X_i^-1), 0<=i<omega
        for (int i = 0; i < errors_pos.size(); i++) {
            int X_i_inv = pow_to_poly[(n - errors_pos[i]) % n];

            int num = poly_eval(gamma, X_i_inv);
            num = Mul[num][X_i_inv];

            int div = 1;
            for (int j = 0; j < errors_pos.size(); j++) {
                if (i == j) {
                    continue;
                }
                int X_j = pow_to_poly[errors_pos[j]];
                div = Mul[div][1 ^ Mul[X_j][X_i_inv]];
            }

            int e = pow_to_poly[(poly_to_pow[num] - poly_to_pow[div] + n) % n];
            ans[errors_pos[i]] ^= e;
        }

        return ans;
    }

    void simulate() {
        double noise_value;
        int simulations_count, max_errors_count;
        cin >> noise_value >> simulations_count >> max_errors_count;

        int errors_count = 0;
        int i = 0;
        while (i < simulations_count && errors_count < max_errors_count) {
            auto rand_vector = vector<int>(k);
            for (int j = 0; j < k; j++) {
                rand_vector[j] = new_element(gen);
            }
            auto test = encode(rand_vector);
            vector<int> encoded = test;
            for (auto & value: encoded) {
                if (distribution(gen) < noise_value) {
                    value ^= new_element(gen);
                }
            }
            auto check = decode(encoded);
            if (test != check) {
                errors_count++;
            }
            i++;
        }
        cout << (double) errors_count / i << endl;
    }
};

int main() {
    freopen("input.txt", "r", stdin);
    freopen("output.txt", "w", stdout);

    auto reed_solomon = ReedSolomonCodeSugiyama();

    cout << reed_solomon.k << '\n';
    for (auto value: reed_solomon.g) {
        cout << value << ' ';
    }
    cout << '\n';

    string cmd;
    while (cin >> cmd) {
        if (cmd == "Encode") {
            vector<int> input(reed_solomon.k);
            for (int i = 0; i < reed_solomon.k; ++i) {
                cin >> input[i];
            }
            for (auto i: reed_solomon.encode(input)) {
                cout << i << " ";
            }
            cout << endl;
        } else if (cmd == "Decode") {
            vector<int> input(reed_solomon.n);
            for (int i = 0; i < reed_solomon.n; ++i) {
                cin >> input[i];
            }
            for (auto i: reed_solomon.decode(input)) {
                cout << i << " ";
            }
            cout << endl;
        } else if (cmd == "Simulate") {
            reed_solomon.simulate();
        }
    }
}
