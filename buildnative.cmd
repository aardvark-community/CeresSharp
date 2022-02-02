@echo off

git clone https://github.com/Microsoft/vcpkg.git ./.vcpkg/vcpkg --depth 1
cmd /C ".vcpkg\vcpkg\bootstrap-vcpkg.bat -disableMetrics"
.vcpkg\vcpkg\vcpkg.exe install ceres --triplet x64-windows

cmake -S src\CeresNative -B src\CeresNative\build -DCMAKE_TOOLCHAIN_FILE="%~dp0\.vcpkg\vcpkg\scripts\buildsystems\vcpkg.cmake"  -DCMAKE_BUILD_TYPE=Release

cmake --build src\CeresNative/build --config Release --target install