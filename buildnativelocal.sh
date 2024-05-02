#!/bin/bash

OS=`uname -s`

VCPKG_TRIPLET=""
ARCH=""
ARCH_FLAGS=""

a="/$0"; a=${a%/*}; a=${a#/}; a=${a:-.}; BASEDIR=$(cd "$a"; pwd)

mkdir .vcpkg
git clone https://github.com/Microsoft/vcpkg.git ./.vcpkg/vcpkg 
# cd ./.vcpkg/vcpkg
# git reset --hard e52999ee1a61bfea654733712288c5d4469d38bc
# cd ../..

PACKAGE=ceres[suitesparse,lapack,cxsparse,eigensparse] 
if [ "$OS" = "Darwin" ];
then
    PACKAGE=ceres
    echo "MacOS"
    if [ "$1" = "x86_64" ]; then
        VCPKG_TRIPLET="x64-osx-release"
        ARCH="x86_64"
    elif [ "$1" = "arm64" ]; then
        VCPKG_TRIPLET="arm64-osx"
        ARCH="arm64"
    else
        ARCH=`uname -m | tail -1`
        if [ "$ARCH" = "x86_64" ]; then
            VCPKG_TRIPLET="x64-osx-release"
        elif [ "$ARCH" = "arm64" ]; then
            VCPKG_TRIPLET="arm64-osx"
        fi
    fi

    ARCH_FLAGS="-DCMAKE_OSX_ARCHITECTURES=$ARCH"

else
    echo "Linux"
    VCPKG_TRIPLET="x64-linux-release"
fi

./.vcpkg/vcpkg/bootstrap-vcpkg.sh
./.vcpkg/vcpkg/vcpkg install gflags $PACKAGE --triplet $VCPKG_TRIPLET 

rm -dfr src/CeresNative/build
cmake -S src/CeresNative/ -B src/CeresNative/build $ARCH_FLAGS \
    -DCMAKE_TOOLCHAIN_FILE="$BASEDIR/.vcpkg/vcpkg/scripts/buildsystems/vcpkg.cmake" \
    -DVCPKG_TARGET_TRIPLET=$VCPKG_TRIPLET \
    -DCMAKE_BUILD_TYPE=Release

cmake --build src/CeresNative/build --config Release --target install