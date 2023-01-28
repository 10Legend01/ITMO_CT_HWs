# CMake generated Testfile for 
# Source directory: D:/Users/Legend/monte-carlo-monty-hall-10Legend01
# Build directory: D:/Users/Legend/monte-carlo-monty-hall-10Legend01/cmake-build-debug
# 
# This file includes the relevant testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
add_test(tests "D:/Users/Legend/monte-carlo-monty-hall-10Legend01/cmake-build-debug/test/runUnitTests.exe")
set_tests_properties(tests PROPERTIES  _BACKTRACE_TRIPLES "D:/Users/Legend/monte-carlo-monty-hall-10Legend01/CMakeLists.txt;96;add_test;D:/Users/Legend/monte-carlo-monty-hall-10Legend01/CMakeLists.txt;0;")
subdirs("googletest")
subdirs("test")
