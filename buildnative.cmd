@echo off

.config/nuget.exe setApiKey -Source GitHub $GITHUB_TOKEN -NonInteractive
cmd /C "rmdir /S /Q .vcpkg"

git clone https://github.com/Microsoft/vcpkg.git ./.vcpkg/vcpkg
REM cd .\.vcpkg\vcpkg
REM git reset --hard e52999ee1a61bfea654733712288c5d4469d38bc
REM cd ..\..

copy .vcpkg\vcpkg\triplets\community\x64-windows-static-md.cmake .vcpkg\vcpkg\triplets\community\x64-windows-static-md-rel.cmake
echo set(VCPKG_BUILD_TYPE release) >> .vcpkg\vcpkg\triplets\community\x64-windows-static-md-rel.cmake

cmd /C ".vcpkg\vcpkg\bootstrap-vcpkg.bat -disableMetrics"

SET VCPKG_NUGET_REPOSITORY=https://github.com/aardvark-community/CeresSharp
.vcpkg\vcpkg\vcpkg.exe install ceres[suitesparse] --triplet x64-windows-static-md-rel --binarysource="clear;nuget,Github,readwrite;nugettimeout,1000"

cmake -S src\CeresNative -B src\CeresNative\build -DCMAKE_TOOLCHAIN_FILE="%~dp0\.vcpkg\vcpkg\scripts\buildsystems\vcpkg.cmake" -DVCPKG_TARGET_TRIPLET=x64-windows-static-md-rel -DCMAKE_BUILD_TYPE=Release

cmake --build src\CeresNative/build --config Release --target install
xcopy .vcpkg\vcpkg\installed\x64-windows-static-md-rel\bin\ .\libs\Native\Ceres\windows\AMD64\ /Y