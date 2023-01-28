cmake_minimum_required(VERSION 3.13)

set(PROJECT_NAME uniq_test)
project(${PROJECT_NAME})

# Data files
file(GLOB INPUT_FILES ${PROJECT_SOURCE_DIR}/etc/*.txt)
list(JOIN INPUT_FILES " " TEST_DATA)

# Tests
enable_testing()
add_test(
    NAME uniq
    COMMAND sh -c "${PROJECT_SOURCE_DIR}/test.sh $<TARGET_FILE:uniq> ${TEST_DATA}"
    )
add_test(
    NAME uniq_c
    COMMAND sh -c "${PROJECT_SOURCE_DIR}/test-c.sh $<TARGET_FILE:uniq> ${TEST_DATA}"
    )
add_test(
    NAME uniq_d
    COMMAND sh -c "${PROJECT_SOURCE_DIR}/test-d.sh $<TARGET_FILE:uniq> ${TEST_DATA}"
    )
add_test(
    NAME uniq_u
    COMMAND sh -c "${PROJECT_SOURCE_DIR}/test-u.sh $<TARGET_FILE:uniq> ${TEST_DATA}"
    )
add_test(
    NAME uniq_cd
    COMMAND sh -c "${PROJECT_SOURCE_DIR}/test-cd.sh $<TARGET_FILE:uniq> ${TEST_DATA}"
    )
add_test(
    NAME uniq_cu
    COMMAND sh -c "${PROJECT_SOURCE_DIR}/test-cu.sh $<TARGET_FILE:uniq> ${TEST_DATA}"
    )
add_test(
    NAME uniq_cdu
    COMMAND sh -c "${PROJECT_SOURCE_DIR}/test-cdu.sh $<TARGET_FILE:uniq> ${TEST_DATA}"
    )