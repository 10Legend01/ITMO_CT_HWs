#include <string>
#include <fstream>

int main(int argc, char * argv[])
{
    bool flagC = false,
         flagD = false,
         flagU = false,
         flagFile = false,
         good = true;
    std::ifstream *inputFile;
    for (int i = 1; i < argc; ++i) {
        const std::string arg = static_cast<std::string>(argv[i]);
        if (arg == "--count") {
            flagC = true;
        }
        else if (arg == "--repeated") {
            flagD = true;
        }
        else if (arg == "--unique") {
            flagU = true;
        }
        else if (argv[i][0] == '-') {
            for (size_t j = 1; j < arg.length(); ++j) {
                if (argv[i][j] == 'c') {
                    flagC = true;
                }
                else if (argv[i][j] == 'd') {
                    flagD = true;
                }
                else if (argv[i][j] == 'u') {
                    flagU = true;
                }
                else {
                    good = false;
                }
            }
        }
        else {
            if (flagFile) {
                good = false;
            }
            flagFile = true;
            inputFile = new std::ifstream(arg);
        }
    }
    if (flagFile && inputFile->good())
    return 0;
}
