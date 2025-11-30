@echo off

.config/nuget.exe setApiKey -Source GitHub $GITHUB_TOKEN -NonInteractive
cmd /C "rmdir /S /Q .vcpkg"

git clone https://github.com/Microsoft/vcpkg.git ./.vcpkg/vcpkg
pushd .vcpkg\vcpkg
git reset --hard 5bf0c55239da398b8c6f450818c9e28d36bf9966
popd

cmd /C ".vcpkg\vcpkg\bootstrap-vcpkg.bat -disableMetrics"


copy /Y .vcpkg\vcpkg\triplets\community\x64-windows-static-md-release.cmake .vcpkg\vcpkg\triplets\x64-windows-static-md-mod.cmake
echo set(VCPKG_CXX_FLAGS "${VCPKG_CXX_FLAGS} -D_DISABLE_CONSTEXPR_MUTEX_CONSTRUCTOR ") >> .vcpkg\vcpkg\triplets\x64-windows-static-md-mod.cmake
echo set(VCPKG_C_FLAGS "${VCPKG_C_FLAGS} -D_DISABLE_CONSTEXPR_MUTEX_CONSTRUCTOR ") >> .vcpkg\vcpkg\triplets\x64-windows-static-md-mod.cmake

SET VCPKG_NUGET_REPOSITORY=https://github.com/aardvark-community/CeresSharp
.vcpkg\vcpkg\vcpkg.exe install ceres[suitesparse] --triplet x64-windows-static-md-mod --binarysource="clear;nuget,GitHub,readwrite;nugettimeout,1000"

cmake -S src\CeresNative -B src\CeresNative\build -DCMAKE_TOOLCHAIN_FILE="%~dp0\.vcpkg\vcpkg\scripts\buildsystems\vcpkg.cmake" -DVCPKG_TARGET_TRIPLET=x64-windows-static-md-mod -DCMAKE_BUILD_TYPE=Release

cmake --build src\CeresNative/build --config Release --target install
if exist .vcpkg\vcpkg\installed\x64-windows-static-md-mod\bin xcopy .vcpkg\vcpkg\installed\x64-windows-static-md-mod\bin .\libs\Native\Ceres\windows\AMD64\ /Y