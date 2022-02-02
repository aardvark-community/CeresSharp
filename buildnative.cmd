@echo off

mkdir .vcpkg
git clone https://github.com/Microsoft/vcpkg.git ./.vcpkg/vcpkg --depth 1
.vcpkg\vcpkg\bootstrap-vcpkg.bat
.vcpkg\vcpkg\vcpkg.exe install ceres --triplet x64-windows-release

cmake -S src\CeresNative/ -B src\CeresNative\build \
    -DCMAKE_TOOLCHAIN_FILE="%~dp0\.vcpkg\vcpkg\scripts\buildsystems\vcpkg.cmake" \
    -DVCPKG_TARGET_TRIPLET=x64-windows-release\
    -DCMAKE_BUILD_TYPE=Release

cmake --build src\CeresNative/build --config Release --target install