#include "SeamCarver.h"

#include <algorithm>
#include <cmath>
#include <vector>

#define GetEnergy(x, y, transpose) (transpose ? GetPixelEnergy(y, x) : GetPixelEnergy(x, y))

SeamCarver::SeamCarver(Image image)
    : m_image(std::move(image))
    , width(m_image.m_table.size())
    , height(width == 0 ? 0 : m_image.m_table[0].size())
{
}

const Image & SeamCarver::GetImage() const
{
    return m_image;
}

size_t SeamCarver::GetImageWidth() const
{
    return width;
}

size_t SeamCarver::GetImageHeight() const
{
    return height;
}

double SeamCarver::GetPixelEnergy(size_t columnId, size_t rowId) const
{
    const size_t x = columnId, y = rowId,
                 xLeft = x == 0 ? width - 1 : x - 1,
                 xRight = x == width - 1 ? 0 : x + 1,
                 yUp = y == 0 ? height - 1 : y - 1,
                 yDown = y == height - 1 ? 0 : y + 1;
    return std::sqrt(
            std::pow(m_image.GetPixel(xLeft, y).m_red - m_image.GetPixel(xRight, y).m_red, 2) +
            std::pow(m_image.GetPixel(xLeft, y).m_green - m_image.GetPixel(xRight, y).m_green, 2) +
            std::pow(m_image.GetPixel(xLeft, y).m_blue - m_image.GetPixel(xRight, y).m_blue, 2) +
            std::pow(m_image.GetPixel(x, yUp).m_red - m_image.GetPixel(x, yDown).m_red, 2) +
            std::pow(m_image.GetPixel(x, yUp).m_green - m_image.GetPixel(x, yDown).m_green, 2) +
            std::pow(m_image.GetPixel(x, yUp).m_blue - m_image.GetPixel(x, yDown).m_blue, 2));
}

SeamCarver::Seam SeamCarver::FindSeam(const bool & transpose) const
{
    const size_t curr_width = transpose ? height : width,
                 curr_height = transpose ? width : height;
    if (curr_height <= 1) {
        return Seam(curr_width);
    }
    std::vector<std::vector<double>> energy_map(curr_width, std::vector<double>(curr_height));
    for (size_t y = 0; y < curr_height; ++y) {
        energy_map[curr_width - 1][y] = GetEnergy(curr_width - 1, y, transpose);
    }
    for (size_t x = curr_width - 2, npos = -1; x != npos; --x) {
        energy_map[x][0] = GetEnergy(x, 0, transpose) + std::min(energy_map[x + 1][0], energy_map[x + 1][1]);
        for (size_t y = 1; y < curr_height - 1; ++y) {
            energy_map[x][y] = GetEnergy(x, y, transpose) + std::min({energy_map[x + 1][y - 1], energy_map[x + 1][y], energy_map[x + 1][y + 1]});
        }
        energy_map[x][curr_height - 1] = GetEnergy(x, curr_height - 1, transpose) + std::min(energy_map[x + 1][curr_height - 2], energy_map[x + 1][curr_height - 1]);
    }
    size_t curr_y = std::distance(energy_map[0].begin(), std::min_element(energy_map[0].begin(), energy_map[0].end()));
    Seam seam;
    seam.push_back(curr_y);
    for (size_t x = 1; x < curr_width; ++x) {
        if (curr_y == 0) {
            curr_y = energy_map[x][curr_y] < energy_map[x][curr_y + 1] ? curr_y : curr_y + 1;
        }
        else if (curr_y == curr_height - 1) {
            curr_y = energy_map[x][curr_y - 1] < energy_map[x][curr_y] ? curr_y - 1 : curr_y;
        }
        else {
            if (energy_map[x][curr_y - 1] < energy_map[x][curr_y] && energy_map[x][curr_y - 1] < energy_map[x][curr_y + 1]) {
                curr_y--;
            }
            else if (energy_map[x][curr_y + 1] < energy_map[x][curr_y]) {
                curr_y++;
            }
        }
        seam.push_back(curr_y);
    }
    return seam;
}

SeamCarver::Seam SeamCarver::FindHorizontalSeam() const
{
    return FindSeam(false);
}

SeamCarver::Seam SeamCarver::FindVerticalSeam() const
{
    return FindSeam(true);
}

void SeamCarver::RemoveHorizontalSeam(const Seam & seam)
{
    for (size_t x = 0; x < width; ++x) {
        m_image.m_table[x].erase(std::next(m_image.m_table[x].begin(), seam[x]));
    }
    if (--height == 0) {
        width = 0;
    }
}

void SeamCarver::RemoveVerticalSeam(const Seam & seam)
{
    for (size_t y = 0; y < height; ++y) {
        for (size_t x = seam[y] + 1; x < width; ++x) {
            m_image.m_table[x - 1][y] = m_image.m_table[x][y];
        }
    }
    m_image.m_table.pop_back();
    if (--width == 0) {
        height = 0;
    }
}
