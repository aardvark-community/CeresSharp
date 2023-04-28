@echo off


git clone https://github.com/Microsoft/vcpkg.git ./.vcpkg/vcpkg
cd .\.vcpkg\vcpkg
git reset --hard e52999ee1a61bfea654733712288c5d4469d38bc
cd ..\..

cmd /C ".vcpkg\vcpkg\bootstrap-vcpkg.bat -disableMetrics"

.vcpkg\vcpkg\vcpkg.exe install ceres[suitesparse] --triplet x64-windows-static-md

cmake -S src\CeresNative -B src\CeresNative\build -DCMAKE_TOOLCHAIN_FILE="%~dp0\.vcpkg\vcpkg\scripts\buildsystems\vcpkg.cmake" -DVCPKG_TARGET_TRIPLET=x64-windows-static-md -DCMAKE_BUILD_TYPE=Debug

cmake --build src\CeresNative/build --config Debug --target install
REM xcopy .vcpkg\vcpkg\installed\x64-windows-static-md\bin\ .\libs\Native\Ceres\windows\AMD64\ /Y