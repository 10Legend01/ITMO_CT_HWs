#include "monty_hall.h"

#include <iostream>
#include <string>

int main(int argc, char ** argv)
{
    unsigned long runs = 1000000;
    if (argc > 1) {
        runs = std::stoul(argv[1]);
    }
    std::cout << winning_probability(runs) << std::endl;
}
