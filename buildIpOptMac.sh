#!/bin/zsh
set -e

# Build IPOPT for macOS with MUMPS statically linked
# This script creates a self-contained libipopt.dylib

# Usage: buildIpOptMac.sh [arch]
#   arch: arm64 or x86_64 (default: x86_64)

ARCH="${1:-x86_64}"

a="/$0"; a=${a%/*}; a=${a#/}; a=${a:-.}; BASEDIR=$(cd "$a"; pwd)

# Read IpOpt version from version file
IPOPT_VERSION_NUMBER=$(cat "$BASEDIR/ipopt-version.txt" | tr -d '[:space:]')
IPOPT_VERSION="releases/${IPOPT_VERSION_NUMBER}"

if [ "$ARCH" = "x86_64" ]; then
    OUTPUT_DIR="$BASEDIR/libs/Native/IpOptSharp/mac/AMD64"
elif [ "$ARCH" = "arm64" ]; then
    OUTPUT_DIR="$BASEDIR/libs/Native/IpOptSharp/mac/ARM64"
else
    echo "Error: Unknown architecture: $ARCH"
    echo "Usage: $0 [x86_64|arm64]"
    exit 1
fi

BUILD_DIR="/tmp/ipopt-build-$$"
INSTALL_DIR="/tmp/ipopt-install-$$"

echo "==> Building IPOPT for macOS ${ARCH}"
echo "==> IPOPT version: ${IPOPT_VERSION}"
echo "==> Build directory: ${BUILD_DIR}"
echo "==> Install directory: ${INSTALL_DIR}"
echo "==> Output directory: ${OUTPUT_DIR}"

mkdir -p "${BUILD_DIR}"
mkdir -p "${OUTPUT_DIR}"
cd "${BUILD_DIR}"

# Download MUMPS third-party package
echo "==> Downloading MUMPS..."
curl -L https://github.com/coin-or-tools/ThirdParty-Mumps/archive/refs/tags/releases/3.0.5.tar.gz -o mumps.tar.gz
tar xzf mumps.tar.gz
cd ThirdParty-Mumps-releases-3.0.5

# Get MUMPS source
echo "==> Fetching MUMPS source..."
./get.Mumps

# Configure and build MUMPS
echo "==> Building MUMPS..."
./configure \
  --prefix="${INSTALL_DIR}" \
  CFLAGS="-arch ${ARCH}" \
  CXXFLAGS="-arch ${ARCH}" \
  FFLAGS="-arch ${ARCH} -fallow-argument-mismatch -fPIC" \
  --disable-dependency-tracking \
  --enable-static \
  --disable-shared

make -j$(sysctl -n hw.ncpu)
make install

cd "${BUILD_DIR}"

# Download IPOPT
echo "==> Cloning IPOPT..."
git clone --depth 1 --branch "${IPOPT_VERSION}" https://github.com/coin-or/Ipopt.git
cd Ipopt

# Configure IPOPT with static MUMPS
echo "==> Configuring IPOPT..."
mkdir build && cd build

../configure \
  --prefix="${INSTALL_DIR}" \
  CFLAGS="-arch ${ARCH}" \
  CXXFLAGS="-arch ${ARCH}" \
  FFLAGS="-arch ${ARCH} -fallow-argument-mismatch" \
  --disable-dependency-tracking \
  --with-mumps \
  --with-mumps-lflags="-L${INSTALL_DIR}/lib -lcoinmumps -framework Accelerate" \
  --with-mumps-cflags="-I${INSTALL_DIR}/include/coin-or/mumps" \
  --disable-linear-solver-loader

# Build and install
echo "==> Building IPOPT..."
make -j$(sysctl -n hw.ncpu)
make install

echo "==> Copying libraries to output directory..."
cd "${INSTALL_DIR}/lib"

# Find and copy the actual library file, renaming to libipopt.dylib
if [ -L libipopt.dylib ]; then
    ACTUAL_FILE=$(readlink libipopt.dylib)
    cp -f "$ACTUAL_FILE" "${OUTPUT_DIR}/libipopt.dylib"
elif [ -f libipopt.dylib ]; then
    cp -f libipopt.dylib "${OUTPUT_DIR}/libipopt.dylib"
else
    # Find the versioned library (e.g., libipopt.3.dylib)
    VERSIONED_LIB=$(ls libipopt.*.dylib | head -n 1)
    if [ -n "$VERSIONED_LIB" ]; then
        cp -f "$VERSIONED_LIB" "${OUTPUT_DIR}/libipopt.dylib"
    else
        echo "Error: Could not find libipopt library"
        exit 1
    fi
fi

# Make libipopt self-contained by fixing paths
cd "${OUTPUT_DIR}"
install_name_tool -id "@rpath/libipopt.dylib" libipopt.dylib

# Check what dependencies we have
echo "==> Library dependencies:"
otool -L libipopt.dylib

# Copy gfortran libraries (required for IpOpt)
echo "==> Copying gfortran runtime libraries..."

# Find gfortran - use FC if set, otherwise try to find it via brew
if [ -n "$FC" ] && [ -x "$FC" ]; then
    GFORTRAN_CMD="$FC"
elif command -v gfortran >/dev/null 2>&1; then
    GFORTRAN_CMD="gfortran"
else
    # Try to find gfortran from brew
    GFORTRAN_CMD=$(find $(brew --prefix gcc 2>/dev/null || echo /usr/local)/bin -name 'gfortran-*' | head -n 1)
fi

if [ -z "$GFORTRAN_CMD" ] || [ ! -x "$GFORTRAN_CMD" ]; then
    echo "Error: Could not find gfortran"
    exit 1
fi

GFORTRAN_LIB=$($GFORTRAN_CMD -print-file-name=libgfortran.dylib)
GFORTRAN_DIR=$(dirname "${GFORTRAN_LIB}")

echo "==> Using gfortran from: ${GFORTRAN_DIR}"

# Copy required runtime libraries
cp "${GFORTRAN_DIR}/libgfortran.5.dylib" .
cp "${GFORTRAN_DIR}/libquadmath.0.dylib" .
cp "${GFORTRAN_DIR}/libgcc_s.1.1.dylib" .

# Fix paths in libipopt to use @loader_path
install_name_tool -change "@rpath/libgfortran.5.dylib" "@loader_path/libgfortran.5.dylib" libipopt.dylib || true
install_name_tool -change "@rpath/libquadmath.0.dylib" "@loader_path/libquadmath.0.dylib" libipopt.dylib || true
install_name_tool -change "@rpath/libgcc_s.1.1.dylib" "@loader_path/libgcc_s.1.1.dylib" libipopt.dylib || true

# Fix paths in libgfortran
install_name_tool -id "@rpath/libgfortran.5.dylib" libgfortran.5.dylib
install_name_tool -change "@rpath/libquadmath.0.dylib" "@loader_path/libquadmath.0.dylib" libgfortran.5.dylib || true
install_name_tool -change "@rpath/libgcc_s.1.1.dylib" "@loader_path/libgcc_s.1.1.dylib" libgfortran.5.dylib || true

# Fix paths in libquadmath
install_name_tool -id "@rpath/libquadmath.0.dylib" libquadmath.0.dylib
install_name_tool -change "@rpath/libgcc_s.1.1.dylib" "@loader_path/libgcc_s.1.1.dylib" libquadmath.0.dylib || true

# Fix paths in libgcc_s
install_name_tool -id "@rpath/libgcc_s.1.1.dylib" libgcc_s.1.1.dylib

# Re-sign all libraries
codesign --force --sign - *.dylib 2>/dev/null || true

# Remove sIPOPT and other optional libraries if present
rm -f libsipopt*.dylib 2>/dev/null || true
rm -f libipoptamplinterface*.dylib 2>/dev/null || true

echo ""
echo "==> Build complete!"
echo "==> Libraries copied to: ${OUTPUT_DIR}"
echo ""
echo "Files:"
ls -lh "${OUTPUT_DIR}"/*.dylib

# Cleanup
echo ""
echo "==> Cleaning up build and install directories..."
rm -rf "${BUILD_DIR}"
rm -rf "${INSTALL_DIR}"

echo "==> Done!"
