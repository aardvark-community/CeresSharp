#!/bin/bash

OS=`uname -s`

VCPKG_TRIPLET=""
ARCH=""
ARCH_FLAGS=""

a="/$0"; a=${a%/*}; a=${a#/}; a=${a:-.}; BASEDIR=$(cd "$a"; pwd)

rm -dfr .vcpkg
mkdir .vcpkg
git clone https://github.com/Microsoft/vcpkg.git ./.vcpkg/vcpkg --depth 1

if [ "$OS" = "Darwin" ];
then
    echo "MacOS"
    if [ "$1" = "x86_64" ]; then
        VCPKG_TRIPLET="x64-osx-release"
        ARCH="x86_64"
    elif [ "$1" = "arm64" ]; then
        VCPKG_TRIPLET="arm64-osx"
        ARCH="arm64"
        echo "set(VCPKG_BUILD_TYPE release)" >> .vcpkg/vcpkg/triplets/community/arm64-osx.cmake
    else
        ARCH=`uname -m`
        if [ "$ARCH" = "x86_64" ]; then
            VCPKG_TRIPLET="x64-osx-release"
        elif [ "$ARCH" ]; then
            VCPKG_TRIPLET="arm64-osx"
            echo "set(VCPKG_BUILD_TYPE release)" >> .vcpkg/vcpkg/triplets/community/arm64-osx.cmake
        fi
    fi

    ARCH_FLAGS="-DCMAKE_OSX_ARCHITECTURES=$ARCH"

else
    echo "Linux"
    VCPKG_TRIPLET="x64-linux-release"
fi

./.vcpkg/vcpkg/bootstrap-vcpkg.sh
export VCPKG_NUGET_REPOSITORY="https://github.com/aardvark-community/CeresSharp"
./.vcpkg/vcpkg/vcpkg install ceres --triplet $VCPKG_TRIPLET --binarysource='clear;nuget,Github,readwrite'


rm -dfr src/CeresNative/build
cmake -S src/CeresNative/ -B src/CeresNative/build $ARCH_FLAGS \
    -DCMAKE_TOOLCHAIN_FILE="$BASEDIR/.vcpkg/vcpkg/scripts/buildsystems/vcpkg.cmake" \
    -DVCPKG_TARGET_TRIPLET=$VCPKG_TRIPLET \
    -DCMAKE_BUILD_TYPE=Release

cmake --build src/CeresNative/build --config Release --target install