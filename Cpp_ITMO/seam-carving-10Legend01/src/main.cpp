#include "Image.h"
#include "SeamCarver.h"

#include <cmath>
#include <fstream>
#include <gtest/gtest.h>
#include <iostream>

TEST(RTests, RegressionTest)
{
    const std::vector<Image::Pixel> col0 = {Image::Pixel(152, 150, 150), Image::Pixel(152, 150, 150), Image::Pixel(150, 150, 150)};
    const std::vector<Image::Pixel> col1 = {Image::Pixel(152, 150, 150), Image::Pixel(154, 150, 150), Image::Pixel(152, 150, 150)};
    const std::vector<Image::Pixel> col2 = {Image::Pixel(152, 150, 150), Image::Pixel(150, 150, 150), Image::Pixel(150, 150, 150)};
    const std::vector<Image::Pixel> col3 = {Image::Pixel(152, 150, 150), Image::Pixel(154, 150, 150), Image::Pixel(152, 150, 150)};

    Image image({col0, col1, col2, col3});
    SeamCarver carver(image);

    EXPECT_DOUBLE_EQ(2.0, carver.GetPixelEnergy(0, 0));
    EXPECT_DOUBLE_EQ(2.0, carver.GetPixelEnergy(1, 0));
    EXPECT_DOUBLE_EQ(0.0, carver.GetPixelEnergy(2, 0));
    EXPECT_DOUBLE_EQ(2.0, carver.GetPixelEnergy(3, 0));

    EXPECT_DOUBLE_EQ(2.0, carver.GetPixelEnergy(0, 1));
    EXPECT_DOUBLE_EQ(2.0, carver.GetPixelEnergy(1, 1));
    EXPECT_DOUBLE_EQ(2.0, carver.GetPixelEnergy(2, 1));
    EXPECT_DOUBLE_EQ(2.0, carver.GetPixelEnergy(3, 1));

    EXPECT_DOUBLE_EQ(0.0, carver.GetPixelEnergy(0, 2));
    EXPECT_DOUBLE_EQ(2.0, carver.GetPixelEnergy(1, 2));
    EXPECT_DOUBLE_EQ(2.0, carver.GetPixelEnergy(2, 2));
    EXPECT_DOUBLE_EQ(2.0, carver.GetPixelEnergy(3, 2));

    std::vector<size_t> seam = carver.FindVerticalSeam();

    ASSERT_EQ(3, seam.size());
    EXPECT_EQ(2, seam.at(0));
    EXPECT_EQ(1, seam.at(1));
    EXPECT_EQ(0, seam.at(2));

    carver.RemoveVerticalSeam(seam);

    ASSERT_EQ(3, carver.GetImageWidth());
    ASSERT_EQ(3, carver.GetImageHeight());

    image = Image({col1, col2, col3, col0});
    carver = SeamCarver(image);

    seam = carver.FindVerticalSeam();

    ASSERT_EQ(3, seam.size());
    EXPECT_EQ(1, seam.at(0));
    EXPECT_EQ(2, seam.at(1));
    EXPECT_EQ(3, seam.at(2));
}

std::vector<std::vector<Image::Pixel>> ReadImageFromCSV(std::ifstream & input)
{
    size_t width, height;
    input >> width >> height;
    std::vector<std::vector<Image::Pixel>> table;
    for (size_t columnId = 0; columnId < width; ++columnId) {
        std::vector<Image::Pixel> column;
        for (size_t rowId = 0; rowId < height; ++rowId) {
            size_t red, green, blue;
            input >> red >> green >> blue;
            column.emplace_back(red, green, blue);
        }
        table.emplace_back(std::move(column));
    }
    return table;
}

void WriteImageToCSV(const SeamCarver & carver, std::ofstream & output)
{
    const size_t width = carver.GetImageWidth();
    const size_t height = carver.GetImageHeight();
    output << width << " " << height << "\n";
    const Image & image = carver.GetImage();
    for (size_t columnId = 0; columnId < width; ++columnId) {
        for (size_t rowId = 0; rowId < height; ++rowId) {
            const Image::Pixel & pixel = image.GetPixel(columnId, rowId);
            output << pixel.m_red << " " << pixel.m_green << " " << pixel.m_blue << std::endl;
        }
    }
}

int main(int argc, char * argv[])
{
    // Check command line arguments
    const size_t expectedAmountOfArgs = 3;
    if (argc != expectedAmountOfArgs) {
        std::cout << "Wrong amount of arguments. Provide filenames as arguments. See example below:\n";
        std::cout << "seam-carving data/tower.csv data/tower_updated.csv" << std::endl;
        return 0;
    }
    // Check csv file
    std::ifstream inputFile(argv[1]);
    if (!inputFile.good()) {
        std::cout << "Can't open source file " << argv[1] << ". Verify that the file exists." << std::endl;
    }
    else {
        auto imageSource = ReadImageFromCSV(inputFile);
        SeamCarver carver(std::move(imageSource));
        std::cout << "Image: " << carver.GetImageWidth() << "x" << carver.GetImageHeight() << std::endl;
        const size_t pixelsToDelete = 150;
        for (size_t i = 0; i < pixelsToDelete; ++i) {
            std::vector<size_t> seam = carver.FindVerticalSeam();
            carver.RemoveVerticalSeam(seam);
            std::cout << "width = " << carver.GetImageWidth() << ", height = " << carver.GetImageHeight() << std::endl;
        }
        std::ofstream outputFile(argv[2]);
        WriteImageToCSV(carver, outputFile);
        std::cout << "Updated image is written to " << argv[2] << "." << std::endl;
    }
    //::testing::InitGoogleTest(&argc, argv);
    //return RUN_ALL_TESTS();
    return 0;
}
