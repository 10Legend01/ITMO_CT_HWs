#include "monty_hall.h"

#include "random_gen.h"

double winning_probability(const unsigned long runs) {
    if (runs == 0) return 0;
    unsigned long wins = 0;
    for (unsigned long i = 0; i < runs; ++i) {
        if (get_random_number() * 3 < 2) wins++;
    }
    return static_cast<double>(wins) / runs;
}
