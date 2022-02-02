cmake_minimum_required(VERSION 3.9)
include("cmake/Aardvark.cmake")

project(CeresNative)

# set(Ceres_DIR ${CMAKE_LIBRARY_PATH}../../.vcpkg/vcpkg/packages/ceres_x64-osx-release)
find_package(Ceres CONFIG REQUIRED)

set(CMAKE_BUILD_TYPE, "Release")
if(UNIX)
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -O3 -ffunction-sections -fdata-sections -fvisibility=hidden")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -O3 -ffunction-sections -fdata-sections -fvisibility=hidden")
endif()



set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
if(APPLE)
    set(CMAKE_SHARED_LINKER_FLAGS "-O3 -flto=thin -dead_strip")
elseif(UNIX)
    set(CMAKE_SHARED_LINKER_FLAGS "-O3 -Wl,--gc-sections -Wl,--strip-all")
endif()


add_library(CeresNative SHARED CeresNative.h CeresNative.cpp)

# include_directories(~/vcpkg/packages/ceres_arm64-osx/include)
target_link_libraries(CeresNative PRIVATE ceres Ceres::ceres)

SET(CMAKE_INSTALL_PREFIX "${CMAKE_CURRENT_SOURCE_DIR}/../../libs/Native/Ceres/") 
install(TARGETS CeresNative DESTINATION ${OS}/${ARCH})