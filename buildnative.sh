#!/bin/bash

OS=`uname -s`

VCPKG_TRIPLET=""
ARCH=""
ARCH_FLAGS=""

if [ "$OS" = "Darwin" ];
then
    echo "MacOS"
    if [ "$1" = "x86_64" ]; then
        VCPKG_TRIPLET="x64-osx-release"
        ARCH="x86_64"
    elif [ "$1" = "arm64" ]; then
        VCPKG_TRIPLET="arm64-osx"
        ARCH="arm64"
    else
        ARCH=`uname -m`
        if [ "$ARCH" = "x86_64" ]; then
            VCPKG_TRIPLET="x64-osx-release"
        elif [ "$ARCH" ]; then
            VCPKG_TRIPLET="arm64-osx"
        fi
    fi

    ARCH_FLAGS="-DCMAKE_OSX_ARCHITECTURES=$ARCH"

else
    echo "Linux"
    VCPKG_TRIPLET="x64-linux-release"
fi

a="/$0"; a=${a%/*}; a=${a#/}; a=${a:-.}; BASEDIR=$(cd "$a"; pwd)

rm -dfr .vcpkg
mkdir .vcpkg
git clone https://github.com/Microsoft/vcpkg.git ./.vcpkg/vcpkg --depth 1
./.vcpkg/vcpkg/bootstrap-vcpkg.sh
./.vcpkg/vcpkg/vcpkg install ceres --triplet $VCPKG_TRIPLET


rm -dfr src/CeresNative/build
cmake -S src/CeresNative/ -B src/CeresNative/build $ARCH_FLAGS \
    -DCMAKE_TOOLCHAIN_FILE="$BASEDIR/.vcpkg/vcpkg/scripts/buildsystems/vcpkg.cmake" \
    -DVCPKG_TARGET_TRIPLET=$VCPKG_TRIPLET \
    -DCMAKE_BUILD_TYPE=Release

cmake --build src/CeresNative/build --config Release --target install