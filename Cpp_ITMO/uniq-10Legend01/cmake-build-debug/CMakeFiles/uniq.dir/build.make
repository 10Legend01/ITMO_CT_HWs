# CMAKE generated file: DO NOT EDIT!
# Generated by "MinGW Makefiles" Generator, CMake Version 3.17

# Delete rule output on recipe failure.
.DELETE_ON_ERROR:


#=============================================================================
# Special targets provided by cmake.

# Disable implicit rules so canonical targets will work.
.SUFFIXES:


# Disable VCS-based implicit rules.
% : %,v


# Disable VCS-based implicit rules.
% : RCS/%


# Disable VCS-based implicit rules.
% : RCS/%,v


# Disable VCS-based implicit rules.
% : SCCS/s.%


# Disable VCS-based implicit rules.
% : s.%


.SUFFIXES: .hpux_make_needs_suffix_list


# Command-line flag to silence nested $(MAKE).
$(VERBOSE)MAKESILENT = -s

# Suppress display of executed commands.
$(VERBOSE).SILENT:


# A target that is always out of date.
cmake_force:

.PHONY : cmake_force

#=============================================================================
# Set environment variables for the build.

SHELL = cmd.exe

# The CMake executable.
CMAKE_COMMAND = "C:\Program Files\JetBrains\CLion 2020.2\bin\cmake\win\bin\cmake.exe"

# The command to remove a file.
RM = "C:\Program Files\JetBrains\CLion 2020.2\bin\cmake\win\bin\cmake.exe" -E rm -f

# Escaping for special characters.
EQUALS = =

# The top-level source directory on which CMake was run.
CMAKE_SOURCE_DIR = D:\Users\Legend\uniq-10Legend01

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = D:\Users\Legend\uniq-10Legend01\cmake-build-debug

# Include any dependencies generated for this target.
include CMakeFiles/uniq.dir/depend.make

# Include the progress variables for this target.
include CMakeFiles/uniq.dir/progress.make

# Include the compile flags for this target's objects.
include CMakeFiles/uniq.dir/flags.make

CMakeFiles/uniq.dir/src/main.cpp.obj: CMakeFiles/uniq.dir/flags.make
CMakeFiles/uniq.dir/src/main.cpp.obj: CMakeFiles/uniq.dir/includes_CXX.rsp
CMakeFiles/uniq.dir/src/main.cpp.obj: ../src/main.cpp
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=D:\Users\Legend\uniq-10Legend01\cmake-build-debug\CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Building CXX object CMakeFiles/uniq.dir/src/main.cpp.obj"
	C:\mingw-w64\x86_64-8.1.0-win32-seh-rt_v6-rev0\mingw64\bin\g++.exe  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles\uniq.dir\src\main.cpp.obj -c D:\Users\Legend\uniq-10Legend01\src\main.cpp

CMakeFiles/uniq.dir/src/main.cpp.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/uniq.dir/src/main.cpp.i"
	C:\mingw-w64\x86_64-8.1.0-win32-seh-rt_v6-rev0\mingw64\bin\g++.exe $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E D:\Users\Legend\uniq-10Legend01\src\main.cpp > CMakeFiles\uniq.dir\src\main.cpp.i

CMakeFiles/uniq.dir/src/main.cpp.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/uniq.dir/src/main.cpp.s"
	C:\mingw-w64\x86_64-8.1.0-win32-seh-rt_v6-rev0\mingw64\bin\g++.exe $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S D:\Users\Legend\uniq-10Legend01\src\main.cpp -o CMakeFiles\uniq.dir\src\main.cpp.s

# Object files for target uniq
uniq_OBJECTS = \
"CMakeFiles/uniq.dir/src/main.cpp.obj"

# External object files for target uniq
uniq_EXTERNAL_OBJECTS =

uniq.exe: CMakeFiles/uniq.dir/src/main.cpp.obj
uniq.exe: CMakeFiles/uniq.dir/build.make
uniq.exe: CMakeFiles/uniq.dir/linklibs.rsp
uniq.exe: CMakeFiles/uniq.dir/objects1.rsp
uniq.exe: CMakeFiles/uniq.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=D:\Users\Legend\uniq-10Legend01\cmake-build-debug\CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Linking CXX executable uniq.exe"
	$(CMAKE_COMMAND) -E cmake_link_script CMakeFiles\uniq.dir\link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
CMakeFiles/uniq.dir/build: uniq.exe

.PHONY : CMakeFiles/uniq.dir/build

CMakeFiles/uniq.dir/clean:
	$(CMAKE_COMMAND) -P CMakeFiles\uniq.dir\cmake_clean.cmake
.PHONY : CMakeFiles/uniq.dir/clean

CMakeFiles/uniq.dir/depend:
	$(CMAKE_COMMAND) -E cmake_depends "MinGW Makefiles" D:\Users\Legend\uniq-10Legend01 D:\Users\Legend\uniq-10Legend01 D:\Users\Legend\uniq-10Legend01\cmake-build-debug D:\Users\Legend\uniq-10Legend01\cmake-build-debug D:\Users\Legend\uniq-10Legend01\cmake-build-debug\CMakeFiles\uniq.dir\DependInfo.cmake --color=$(COLOR)
.PHONY : CMakeFiles/uniq.dir/depend

