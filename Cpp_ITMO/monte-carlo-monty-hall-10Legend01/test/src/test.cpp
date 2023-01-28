#include "monty_hall.h"

#include <gtest/gtest.h>

#include <cmath>

TEST(MonteCarloTest, zero_iterations)
{
    EXPECT_FALSE(std::isnan(winning_probability(0)));
}

TEST(MonteCarloTest, monty_hall_test)
{
    EXPECT_NEAR(winning_probability(10000), 2./3, 1.e-2);
    EXPECT_NEAR(winning_probability(1000000), 2./3, 1.e-3);
}
