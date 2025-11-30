#!/bin/bash
set -e

# Build IPOPT for Linux with MUMPS statically linked
# This script creates a self-contained libipopt.so

a="/$0"; a=${a%/*}; a=${a#/}; a=${a:-.}; BASEDIR=$(cd "$a"; pwd)

# Read IpOpt version from version file
IPOPT_VERSION_NUMBER=$(cat "$BASEDIR/ipopt-version.txt" | tr -d '[:space:]')
IPOPT_VERSION="releases/${IPOPT_VERSION_NUMBER}"

OUTPUT_DIR="$BASEDIR/libs/Native/IpOptSharp/linux/AMD64"
BUILD_DIR="/tmp/ipopt-build-$$"
INSTALL_DIR="/tmp/ipopt-install-$$"

echo "==> Building IPOPT for Linux x64"
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
  FFLAGS="-fallow-argument-mismatch -fPIC" \
  --disable-dependency-tracking \
  --enable-static \
  --disable-shared

make -j$(nproc)
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
  FFLAGS="-fallow-argument-mismatch" \
  --disable-dependency-tracking \
  --with-mumps \
  --with-mumps-lflags="-L${INSTALL_DIR}/lib -lcoinmumps -llapack -lblas -lgfortran -lm -lquadmath" \
  --with-mumps-cflags="-I${INSTALL_DIR}/include/coin-or/mumps" \
  --disable-linear-solver-loader

# Build and install
echo "==> Building IPOPT..."
make -j$(nproc)
make install

echo "==> Copying libraries to output directory..."
cd "${INSTALL_DIR}/lib"

# Find and copy the actual library file, renaming to libipopt.so
if [ -L libipopt.so ]; then
    ACTUAL_FILE=$(readlink -f libipopt.so)
    cp -f "$ACTUAL_FILE" "${OUTPUT_DIR}/libipopt.so"
elif [ -f libipopt.so ]; then
    cp -f libipopt.so "${OUTPUT_DIR}/libipopt.so"
else
    # Find the versioned library (e.g., libipopt.so.3)
    VERSIONED_LIB=$(ls libipopt.so.* 2>/dev/null | head -n 1)
    if [ -n "$VERSIONED_LIB" ]; then
        cp -f "$VERSIONED_LIB" "${OUTPUT_DIR}/libipopt.so"
    else
        echo "Error: Could not find libipopt library"
        exit 1
    fi
fi

cd "${OUTPUT_DIR}"

# Check what dependencies we have
echo "==> Library dependencies:"
ldd libipopt.so || true

# Copy gfortran runtime libraries if needed
if ldd libipopt.so | grep -q libgfortran; then
    echo "==> Copying gfortran runtime libraries..."

    # Find gfortran library locations
    GFORTRAN_PATH=$(ldd libipopt.so | grep libgfortran | awk '{print $3}')
    QUADMATH_PATH=$(ldd libipopt.so | grep libquadmath | awk '{print $3}')
    LIBGCC_PATH=$(ldd libipopt.so | grep libgcc_s | awk '{print $3}')

    # Copy libraries
    if [ -n "$GFORTRAN_PATH" ] && [ -f "$GFORTRAN_PATH" ]; then
        cp "$GFORTRAN_PATH" ./libgfortran.so.5
    fi
    if [ -n "$QUADMATH_PATH" ] && [ -f "$QUADMATH_PATH" ]; then
        cp "$QUADMATH_PATH" ./libquadmath.so.0
    fi
    if [ -n "$LIBGCC_PATH" ] && [ -f "$LIBGCC_PATH" ]; then
        cp "$LIBGCC_PATH" ./libgcc_s.so.1
    fi

    # Set RPATH to $ORIGIN so libraries load from same directory
    echo "==> Setting RPATH to \$ORIGIN..."
    patchelf --set-rpath '$ORIGIN' libipopt.so 2>/dev/null || echo "patchelf not available, skipping RPATH"

    if [ -f libgfortran.so.5 ]; then
        patchelf --set-rpath '$ORIGIN' libgfortran.so.5 2>/dev/null || true
    fi
    if [ -f libquadmath.so.0 ]; then
        patchelf --set-rpath '$ORIGIN' libquadmath.so.0 2>/dev/null || true
    fi

    echo "==> Fixed library paths:"
    ldd libipopt.so || true
fi

# Strip the library
echo "==> Stripping debug symbols..."
strip --strip-unneeded libipopt.so || true
if [ -f libgfortran.so.5 ]; then
    strip --strip-unneeded libgfortran.so.5 || true
fi
if [ -f libquadmath.so.0 ]; then
    strip --strip-unneeded libquadmath.so.0 || true
fi
if [ -f libgcc_s.so.1 ]; then
    strip --strip-unneeded libgcc_s.so.1 || true
fi

# Remove sIPOPT and other optional libraries if present
rm -f libsipopt*.so* 2>/dev/null || true
rm -f libipoptamplinterface*.so* 2>/dev/null || true

echo ""
echo "==> Build complete!"
echo "==> Libraries copied to: ${OUTPUT_DIR}"
echo ""
echo "Files:"
ls -lh "${OUTPUT_DIR}"/libipopt.so

# Cleanup
echo ""
echo "==> Cleaning up build and install directories..."
rm -rf "${BUILD_DIR}"
rm -rf "${INSTALL_DIR}"

echo "==> Done!"
