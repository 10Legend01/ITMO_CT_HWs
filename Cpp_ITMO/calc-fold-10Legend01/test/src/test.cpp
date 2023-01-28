#include "calc.h"

#include <gtest/gtest.h>

TEST(Calc, err)
{
    testing::internal::CaptureStderr();
    EXPECT_DOUBLE_EQ(0, process_line(0, "fix"));
    EXPECT_EQ("Unknown operation fix\n", testing::internal::GetCapturedStderr());
    testing::internal::CaptureStderr();
    EXPECT_DOUBLE_EQ(11, process_line(11, "sqrt"));
    EXPECT_EQ("Unknown operation sqrt\n", testing::internal::GetCapturedStderr());
    testing::internal::CaptureStderr();
    EXPECT_DOUBLE_EQ(17, process_line(17, "\\ 11"));
    EXPECT_EQ("Unknown operation \\ 11\n", testing::internal::GetCapturedStderr());
}

TEST(Calc, set)
{
    EXPECT_DOUBLE_EQ(0, process_line(0, "0"));
    EXPECT_DOUBLE_EQ(0, process_line(0, "0000"));
    EXPECT_DOUBLE_EQ(0, process_line(101, "0"));
    EXPECT_DOUBLE_EQ(13, process_line(0, "13"));
    EXPECT_DOUBLE_EQ(5, process_line(99, "5."));
    EXPECT_DOUBLE_EQ(0.05625, process_line(1113, "0.05625"));
    EXPECT_DOUBLE_EQ(1234567890.0, process_line(1, "1234567890"));
    testing::internal::CaptureStderr();
    EXPECT_DOUBLE_EQ(1, process_line(1, "12345678900000"));
    EXPECT_EQ("Argument isn't fully parsed, suffix left: '0000'\n", testing::internal::GetCapturedStderr());
    testing::internal::CaptureStderr();
    EXPECT_DOUBLE_EQ(99, process_line(99, "5 "));
    EXPECT_EQ("Argument parsing error at 1: ' '\n", testing::internal::GetCapturedStderr());
    testing::internal::CaptureStderr();
    EXPECT_DOUBLE_EQ(0, process_line(0, "0.001 11"));
    EXPECT_EQ("Argument parsing error at 5: ' 11'\n", testing::internal::GetCapturedStderr());
}

TEST(Calc, add)
{
    EXPECT_DOUBLE_EQ(7, process_line(0, "+7"));
    EXPECT_DOUBLE_EQ(7, process_line(5, "+ 2"));
    EXPECT_DOUBLE_EQ(7, process_line(5, "+ \t\t   2"));
    EXPECT_DOUBLE_EQ(2.34, process_line(1.5, "+ 0.84"));
    testing::internal::CaptureStderr();
    EXPECT_DOUBLE_EQ(9, process_line(9, "+    12345678900000"));
    EXPECT_EQ("Argument isn't fully parsed, suffix left: '0000'\n", testing::internal::GetCapturedStderr());
    testing::internal::CaptureStderr();
    EXPECT_DOUBLE_EQ(99, process_line(99, "+ 1 "));
    EXPECT_EQ("Argument parsing error at 3: ' '\n", testing::internal::GetCapturedStderr());
    testing::internal::CaptureStderr();
    EXPECT_DOUBLE_EQ(5, process_line(5, "+ 1 2"));
    EXPECT_EQ("Argument parsing error at 3: ' 2'\n", testing::internal::GetCapturedStderr());
    testing::internal::CaptureStderr();
    EXPECT_DOUBLE_EQ(0, process_line(0, "+0.001 11"));
    EXPECT_EQ("Argument parsing error at 6: ' 11'\n", testing::internal::GetCapturedStderr());
    testing::internal::CaptureStderr();
    EXPECT_DOUBLE_EQ(99, process_line(99, "+ -"));
    EXPECT_EQ("Argument parsing error at 2: '-'\nNo argument for a binary operation\n", testing::internal::GetCapturedStderr());
    testing::internal::CaptureStderr();
    EXPECT_DOUBLE_EQ(113, process_line(113, "+"));
    EXPECT_EQ("No argument for a binary operation\n", testing::internal::GetCapturedStderr());
    testing::internal::CaptureStderr();
    EXPECT_DOUBLE_EQ(113, process_line(113, "+) 10"));
    EXPECT_EQ("Argument parsing error at 1: ') 10'\nNo argument for a binary operation\n", testing::internal::GetCapturedStderr());
}

TEST(Calc, sub)
{
    EXPECT_DOUBLE_EQ(-11, process_line(0, "- 11"));
    EXPECT_DOUBLE_EQ(0, process_line(0, "-0"));
    EXPECT_DOUBLE_EQ(0, process_line(3, "-3"));
    EXPECT_DOUBLE_EQ(-3, process_line(7, "-10"));
    EXPECT_DOUBLE_EQ(-12344.6789, process_line(1, "- 12345.67890"));
    testing::internal::CaptureStderr();
    EXPECT_DOUBLE_EQ(77, process_line(77, "- 123a3"));
    EXPECT_EQ("Argument parsing error at 5: 'a3'\n", testing::internal::GetCapturedStderr());
}

TEST(Calc, mul)
{
    EXPECT_DOUBLE_EQ(0, process_line(0, "* 0"));
    EXPECT_DOUBLE_EQ(0, process_line(0, "*131"));
    EXPECT_DOUBLE_EQ(0, process_line(99, "* 0"));
    EXPECT_DOUBLE_EQ(8, process_line(2, "* 4"));
    EXPECT_DOUBLE_EQ(-16, process_line(-4, "*4"));
}

TEST(Calc, div)
{
    EXPECT_DOUBLE_EQ(0, process_line(0, "/ 11"));
    testing::internal::CaptureStderr();
    EXPECT_DOUBLE_EQ(11, process_line(11, "/ 0"));
    EXPECT_EQ("Bad right argument for division: 0\n", testing::internal::GetCapturedStderr());
    EXPECT_DOUBLE_EQ(3, process_line(6, "/ 2"));
    EXPECT_DOUBLE_EQ(0.7, process_line(7, "/ 10"));
    EXPECT_DOUBLE_EQ(0.3333333333333333, process_line(1, "/ 3"));
    EXPECT_DOUBLE_EQ(-0.5, process_line(-2, "/ 4"));
    EXPECT_DOUBLE_EQ(100, process_line(10, "/ 0.1"));
}

TEST(Calc, rem)
{
    EXPECT_DOUBLE_EQ(0, process_line(0, "% 3"));
    EXPECT_DOUBLE_EQ(0, process_line(4, "%4"));
    EXPECT_DOUBLE_EQ(0, process_line(-24, "%4"));
    EXPECT_DOUBLE_EQ(-3, process_line(-13, "%5"));
    testing::internal::CaptureStderr();
    EXPECT_DOUBLE_EQ(-1, process_line(-1, "%0"));
    EXPECT_EQ("Bad right argument for remainder: 0\n", testing::internal::GetCapturedStderr());
}

TEST(Calc, neg)
{
    EXPECT_DOUBLE_EQ(0, process_line(0, "_"));
    EXPECT_DOUBLE_EQ(1, process_line(-1, "_"));
    testing::internal::CaptureStderr();
    EXPECT_DOUBLE_EQ(1, process_line(1, "_ "));
    EXPECT_EQ("Unexpected suffix for a unary operation: ' '\n", testing::internal::GetCapturedStderr());
    testing::internal::CaptureStderr();
    EXPECT_DOUBLE_EQ(1, process_line(1, "_1"));
    EXPECT_EQ("Unexpected suffix for a unary operation: '1'\n", testing::internal::GetCapturedStderr());
}

TEST(Calc, pow)
{
    EXPECT_DOUBLE_EQ(0, process_line(0, "^1"));
    EXPECT_DOUBLE_EQ(0, process_line(0, "^2"));
    EXPECT_DOUBLE_EQ(1, process_line(119, "^0"));
    EXPECT_DOUBLE_EQ(37, process_line(37, "^1"));
    EXPECT_DOUBLE_EQ(25, process_line(-5, "^2"));
    EXPECT_DOUBLE_EQ(-27, process_line(-3, "^3"));
    EXPECT_DOUBLE_EQ(5, process_line(25, "^0.5"));
}

TEST(Calc, sqrt)
{
    EXPECT_DOUBLE_EQ(1, process_line(1, "SQRT"));
    EXPECT_DOUBLE_EQ(0.7, process_line(0.49, "SQRT"));
    EXPECT_DOUBLE_EQ(5, process_line(25, "SQRT"));
    testing::internal::CaptureStderr();
    EXPECT_DOUBLE_EQ(-1, process_line(-1, "SQRT"));
    EXPECT_EQ("Bad argument for SQRT: -1\n", testing::internal::GetCapturedStderr());
}

TEST(Calc, fold_noop)
{
    testing::internal::CaptureStderr();
    EXPECT_DOUBLE_EQ(3, process_line(3, "() 1 2 3"));
    EXPECT_FALSE(testing::internal::GetCapturedStderr().empty());
}

TEST(Calc, fold_wrong_op)
{
    testing::internal::CaptureStderr();
    EXPECT_DOUBLE_EQ(3, process_line(3, "(_) 1 2 3"));
    EXPECT_FALSE(testing::internal::GetCapturedStderr().empty());
    testing::internal::CaptureStderr();
    EXPECT_DOUBLE_EQ(9, process_line(9, "(1) 1 2 3"));
    EXPECT_FALSE(testing::internal::GetCapturedStderr().empty());
    testing::internal::CaptureStderr();
    EXPECT_DOUBLE_EQ(11, process_line(11, "(() 1 2 3"));
    EXPECT_FALSE(testing::internal::GetCapturedStderr().empty());
    testing::internal::CaptureStderr();
    EXPECT_DOUBLE_EQ(19, process_line(19, "(S) 5"));
    EXPECT_FALSE(testing::internal::GetCapturedStderr().empty());
}

TEST(Calc, fold_no_args)
{
    testing::internal::CaptureStderr();
    EXPECT_DOUBLE_EQ(1, process_line(1, "(+)"));
    EXPECT_FALSE(testing::internal::GetCapturedStderr().empty());
    testing::internal::CaptureStderr();
    EXPECT_DOUBLE_EQ(3, process_line(3, "(/)   "));
    EXPECT_FALSE(testing::internal::GetCapturedStderr().empty());
}

TEST(Calc, fold_wrong_brackets)
{
    testing::internal::CaptureStderr();
    EXPECT_DOUBLE_EQ(1, process_line(1, ")+( 1 2 3"));
    EXPECT_FALSE(testing::internal::GetCapturedStderr().empty());
    testing::internal::CaptureStderr();
    EXPECT_DOUBLE_EQ(1, process_line(1, "((+) 1 2 3"));
    EXPECT_FALSE(testing::internal::GetCapturedStderr().empty());
}

TEST(Calc, fold_add)
{
    EXPECT_DOUBLE_EQ(34579.8345, process_line(0, "(+) 1 2 34567.8345 9"));
    EXPECT_DOUBLE_EQ(11, process_line(10, "(+) 1"));
    EXPECT_DOUBLE_EQ(13, process_line(10, "(+) 1                                                                            2"));
    EXPECT_DOUBLE_EQ(2, process_line(1, "(+) 0 0 0 0 1"));
    EXPECT_DOUBLE_EQ(4.321, process_line(0, "(+) 0 1 1.1 1.11 1.111"));
    EXPECT_DOUBLE_EQ(0, process_line(-12, "(+) 1 1 2 3 5"));
    testing::internal::CaptureStderr();
    EXPECT_DOUBLE_EQ(37, process_line(37, "(+) 1 2 3 a 4 5 6"));
    EXPECT_FALSE(testing::internal::GetCapturedStderr().empty());
    testing::internal::CaptureStderr();
    EXPECT_DOUBLE_EQ(11, process_line(11, "(+ ) 1"));
    EXPECT_FALSE(testing::internal::GetCapturedStderr().empty());
}

TEST(Calc, fold_sub)
{
    // ((((0 - 1) - 2) - 34567.8345) - 9)
    EXPECT_DOUBLE_EQ(-34579.8345, process_line(0, "(-) 1 2 34567.8345 9"));
    EXPECT_DOUBLE_EQ(9, process_line(10, "(-) 1"));
    EXPECT_DOUBLE_EQ(0, process_line(1, "(-) 0 0 0 0 1"));
    EXPECT_DOUBLE_EQ(-4.321, process_line(0, "(-) 0 1 1.1 1.11 1.111"));
    EXPECT_DOUBLE_EQ(0, process_line(12, "(-)1 1 2 3 5 "));
    testing::internal::CaptureStderr();
    EXPECT_DOUBLE_EQ(11, process_line(11, "(- 1"));
    EXPECT_FALSE(testing::internal::GetCapturedStderr().empty());
    testing::internal::CaptureStderr();
    EXPECT_DOUBLE_EQ(17, process_line(17, "(-_ 1"));
    EXPECT_FALSE(testing::internal::GetCapturedStderr().empty());
    testing::internal::CaptureStderr();
    EXPECT_DOUBLE_EQ(3, process_line(3, "(-1"));
    EXPECT_FALSE(testing::internal::GetCapturedStderr().empty());
}

TEST(Calc, fold_mul)
{
    EXPECT_DOUBLE_EQ(0, process_line(0, "(*) 1 2 3.333"));
    EXPECT_DOUBLE_EQ(119.988, process_line(1, "(*) 15 2.4 3.333"));
    EXPECT_DOUBLE_EQ(2, process_line(2, "(*) 0.5 2 0.5 2"));
    EXPECT_DOUBLE_EQ(1024, process_line(2, "(*) 4 8 16"));
    EXPECT_DOUBLE_EQ(15.001, process_line(150.01, "(*) 1 1 0.1 1"));
    EXPECT_DOUBLE_EQ(62.2, process_line(31.1, "(*)    1          2                             "));
    testing::internal::CaptureStderr();
    EXPECT_DOUBLE_EQ(2, process_line(2, "((*) 0.5 2"));
    EXPECT_FALSE(testing::internal::GetCapturedStderr().empty());
}

TEST(Calc, fold_pow)
{
    EXPECT_DOUBLE_EQ(65536, process_line(2, "(^) 2 2 2 2"));
    EXPECT_DOUBLE_EQ(0, process_line(0, "(^) 3 5 10"));
    EXPECT_DOUBLE_EQ(1, process_line(1, "(^)10 100"));
    EXPECT_DOUBLE_EQ(1, process_line(42, "(^) 0 0 0 0 0"));
    EXPECT_DOUBLE_EQ(3723.875, process_line(15.5, "(^) 3 1 1 1"));
    testing::internal::CaptureStderr();
    EXPECT_DOUBLE_EQ(7, process_line(7, "((^)) 10 100"));
    EXPECT_FALSE(testing::internal::GetCapturedStderr().empty());
    testing::internal::CaptureStderr();
    EXPECT_DOUBLE_EQ(1, process_line(1, "(^* 10 100"));
    EXPECT_FALSE(testing::internal::GetCapturedStderr().empty());
}

TEST(Calc, fold_div)
{
    EXPECT_DOUBLE_EQ(2, process_line(1024, "(/) 4 8 16"));
    EXPECT_DOUBLE_EQ(128, process_line(2, "(/) 1 0.5 0.25 0.125"));
    EXPECT_DOUBLE_EQ(5, process_line(5, "(/) 1 1 1 1 1"));
    testing::internal::CaptureStderr();
    EXPECT_DOUBLE_EQ(5, process_line(5, "(/) 1,2,3,4,5"));
    EXPECT_FALSE(testing::internal::GetCapturedStderr().empty());
    EXPECT_DOUBLE_EQ(10.533333333333333, process_line(1264, "(/) 12 4 5 0.5"));
    testing::internal::CaptureStderr();
    EXPECT_DOUBLE_EQ(11, process_line(11, "(/) 1 0"));
    EXPECT_EQ("Bad right argument for division: 0\n", testing::internal::GetCapturedStderr());
    testing::internal::CaptureStderr();
    EXPECT_DOUBLE_EQ(13, process_line(13, "(/) 4 5 1xx"));
    EXPECT_FALSE(testing::internal::GetCapturedStderr().empty());
}

TEST(Calc, fold_rem)
{
    EXPECT_DOUBLE_EQ(2, process_line(15, "(%) 8 5"));
    EXPECT_DOUBLE_EQ(3, process_line(469, "(%) 123 93 11 4"));
    EXPECT_DOUBLE_EQ(0, process_line(1173, "(%) 173 17 16"));
    EXPECT_DOUBLE_EQ(0, process_line(1173, "(%) 173 17 16 16"));
    testing::internal::CaptureStderr();
    EXPECT_DOUBLE_EQ(-1, process_line(-1, "(%)1 0"));
    EXPECT_EQ("Bad right argument for remainder: 0\n", testing::internal::GetCapturedStderr());
    testing::internal::CaptureStderr();
    EXPECT_DOUBLE_EQ(7, process_line(7, "(((((%))))) 10 100"));
    EXPECT_FALSE(testing::internal::GetCapturedStderr().empty());
}
