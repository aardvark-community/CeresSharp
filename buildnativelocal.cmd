@echo off


git clone https://github.com/Microsoft/vcpkg.git ./.vcpkg/vcpkg
REM cd .\.vcpkg\vcpkg
REM git reset --hard e52999ee1a61bfea654733712288c5d4469d38bc
REM cd ..\..

cmd /C ".vcpkg\vcpkg\bootstrap-vcpkg.bat -disableMetrics"

copy /Y .vcpkg\vcpkg\triplets\community\x64-windows-static-md.cmake .vcpkg\vcpkg\triplets\community\x64-windows-static-md-mod.cmake
echo set(VCPKG_CXX_FLAGS "${VCPKG_CXX_FLAGS} -D_DISABLE_CONSTEXPR_MUTEX_CONSTRUCTOR ") >> .vcpkg\vcpkg\triplets\community\x64-windows-static-md-mod.cmake
echo set(VCPKG_C_FLAGS "${VCPKG_C_FLAGS} -D_DISABLE_CONSTEXPR_MUTEX_CONSTRUCTOR ") >> .vcpkg\vcpkg\triplets\community\x64-windows-static-md-mod.cmake

.vcpkg\vcpkg\vcpkg.exe install ceres[suitesparse] --triplet x64-windows-static-md-mod

rmdir /q /s src\CeresNative\build
cmake -S src\CeresNative -B src\CeresNative\build -DCMAKE_TOOLCHAIN_FILE="%~dp0\.vcpkg\vcpkg\scripts\buildsystems\vcpkg.cmake" -DVCPKG_TARGET_TRIPLET=x64-windows-static-md-mod -DCMAKE_BUILD_TYPE=Release

cmake --build src\CeresNative/build --config Release --target install
xcopy .vcpkg\vcpkg\installed\x64-windows-static-md-mod\bin .\libs\Native\Ceres\windows\AMD64\ /Y