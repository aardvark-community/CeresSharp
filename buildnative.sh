#!/bin/bash

OS=`uname -s`

VCPKG_TRIPLET=""
ARCH=""
ARCH_FLAGS=""
DEBUG=""

a="/$0"; a=${a%/*}; a=${a#/}; a=${a:-.}; BASEDIR=$(cd "$a"; pwd)

mono .config/nuget.exe setApiKey -Source GitHub $GITHUB_TOKEN -NonInteractive

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
        DEBUG="--debug"
    else
        ARCH=`uname -m`
        if [ "$ARCH" = "x86_64" ]; then
            VCPKG_TRIPLET="x64-osx-release"
        elif [ "$ARCH" = "arm64"]; then
            VCPKG_TRIPLET="arm64-osx"
            DEBUG="--debug"
        fi
    fi

    ARCH_FLAGS="-DCMAKE_OSX_ARCHITECTURES=$ARCH"

else
    echo "Linux"
    VCPKG_TRIPLET="x64-linux-release"
fi

./.vcpkg/vcpkg/bootstrap-vcpkg.sh
export VCPKG_NUGET_REPOSITORY=https://github.com/aardvark-community/CeresSharp
./.confog
./.vcpkg/vcpkg/vcpkg install ceres --triplet $VCPKG_TRIPLET --binarysource='clear;nuget,GitHub,readwrite;nugettimeout,1000' $DEBUG


rm -dfr src/CeresNative/build
cmake -S src/CeresNative/ -B src/CeresNative/build $ARCH_FLAGS \
    -DCMAKE_TOOLCHAIN_FILE="$BASEDIR/.vcpkg/vcpkg/scripts/buildsystems/vcpkg.cmake" \
    -DVCPKG_TARGET_TRIPLET=$VCPKG_TRIPLET \
    -DCMAKE_BUILD_TYPE=Release

cmake --build src/CeresNative/build --config Release --target install