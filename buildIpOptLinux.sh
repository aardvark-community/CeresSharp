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

# Detect GCC version and set appropriate FFLAGS
# The -fallow-argument-mismatch flag is only available in GCC 10+
# For older GCC versions (like GCC 7 on Ubuntu 18.04), this flag is not needed
# as older gfortran was more lenient about type mismatches
GCC_VERSION=$(gcc -dumpversion | cut -d. -f1)
echo "==> Detected GCC major version: ${GCC_VERSION}"

if [ "${GCC_VERSION}" -ge 10 ]; then
    FFLAGS_EXTRA="-fallow-argument-mismatch"
    echo "==> Using -fallow-argument-mismatch flag for GCC ${GCC_VERSION}"
else
    FFLAGS_EXTRA=""
    echo "==> GCC ${GCC_VERSION} does not need -fallow-argument-mismatch flag"
fi

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
if [ -n "${FFLAGS_EXTRA}" ]; then
    ./configure \
      --prefix="${INSTALL_DIR}" \
      FFLAGS="${FFLAGS_EXTRA} -fPIC" \
      --disable-dependency-tracking \
      --enable-static \
      --disable-shared
else
    ./configure \
      --prefix="${INSTALL_DIR}" \
      FFLAGS="-fPIC" \
      --disable-dependency-tracking \
      --enable-static \
      --disable-shared
fi

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

if [ -n "${FFLAGS_EXTRA}" ]; then
    ../configure \
      --prefix="${INSTALL_DIR}" \
      FFLAGS="${FFLAGS_EXTRA}" \
      --disable-dependency-tracking \
      --with-mumps \
      --with-mumps-lflags="-L${INSTALL_DIR}/lib -lcoinmumps -llapack -lblas -lgfortran -lm -lquadmath" \
      --with-mumps-cflags="-I${INSTALL_DIR}/include/coin-or/mumps" \
      --disable-linear-solver-loader
else
    ../configure \
      --prefix="${INSTALL_DIR}" \
      --disable-dependency-tracking \
      --with-mumps \
      --with-mumps-lflags="-L${INSTALL_DIR}/lib -lcoinmumps -llapack -lblas -lgfortran -lm -lquadmath" \
      --with-mumps-cflags="-I${INSTALL_DIR}/include/coin-or/mumps" \
      --disable-linear-solver-loader
fi

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

# Copy runtime libraries that libipopt.so depends on
echo "==> Copying runtime libraries..."

# Find library locations from ldd output
GFORTRAN_PATH=$(ldd libipopt.so | grep libgfortran | awk '{print $3}')
QUADMATH_PATH=$(ldd libipopt.so | grep libquadmath | awk '{print $3}')
LIBGCC_PATH=$(ldd libipopt.so | grep libgcc_s | awk '{print $3}')
BLAS_PATH=$(ldd libipopt.so | grep libblas | awk '{print $3}')
LAPACK_PATH=$(ldd libipopt.so | grep liblapack | awk '{print $3}')

# Copy gfortran runtime libraries
# Ubuntu 18.04 has libgfortran.so.4, Ubuntu 20.04+ has libgfortran.so.5
if [ -n "$GFORTRAN_PATH" ] && [ -f "$GFORTRAN_PATH" ]; then
    GFORTRAN_SONAME=$(basename "$GFORTRAN_PATH")
    cp "$GFORTRAN_PATH" "./$GFORTRAN_SONAME"
    echo "==> Copied $GFORTRAN_SONAME"
fi
if [ -n "$QUADMATH_PATH" ] && [ -f "$QUADMATH_PATH" ]; then
    QUADMATH_SONAME=$(basename "$QUADMATH_PATH")
    cp "$QUADMATH_PATH" "./$QUADMATH_SONAME"
    echo "==> Copied $QUADMATH_SONAME"
fi
if [ -n "$LIBGCC_PATH" ] && [ -f "$LIBGCC_PATH" ]; then
    LIBGCC_SONAME=$(basename "$LIBGCC_PATH")
    cp "$LIBGCC_PATH" "./$LIBGCC_SONAME"
    echo "==> Copied $LIBGCC_SONAME"
fi

# Copy BLAS and LAPACK libraries
if [ -n "$BLAS_PATH" ] && [ -f "$BLAS_PATH" ]; then
    BLAS_SONAME=$(basename "$BLAS_PATH")
    cp "$BLAS_PATH" "./$BLAS_SONAME"
    echo "==> Copied $BLAS_SONAME"
fi
if [ -n "$LAPACK_PATH" ] && [ -f "$LAPACK_PATH" ]; then
    LAPACK_SONAME=$(basename "$LAPACK_PATH")
    cp "$LAPACK_PATH" "./$LAPACK_SONAME"
    echo "==> Copied $LAPACK_SONAME"
fi

# Set RPATH to $ORIGIN so libraries load from same directory
echo "==> Setting RPATH to \$ORIGIN..."
patchelf --set-rpath '$ORIGIN' libipopt.so 2>/dev/null || echo "patchelf not available, skipping RPATH"

# Patch all copied libraries (use nullglob to handle case when no files match)
shopt -s nullglob
for lib in libgfortran.so.* libquadmath.so.* libgcc_s.so.* libblas.so.* liblapack.so.*; do
    patchelf --set-rpath '$ORIGIN' "$lib" 2>/dev/null || true
done
shopt -u nullglob

echo "==> Fixed library paths:"
ldd libipopt.so || true

# Strip the library
echo "==> Stripping debug symbols..."
strip --strip-unneeded libipopt.so || true
# Strip all bundled libraries (use nullglob to handle case when no files match)
shopt -s nullglob
for lib in libgfortran.so.* libquadmath.so.* libgcc_s.so.* libblas.so.* liblapack.so.*; do
    strip --strip-unneeded "$lib" || true
done
shopt -u nullglob

# Remove sIPOPT and other optional libraries if present
rm -f libsipopt*.so* 2>/dev/null || true
rm -f libipoptamplinterface*.so* 2>/dev/null || true

echo ""
echo "==> Build complete!"
echo "==> Libraries copied to: ${OUTPUT_DIR}"
echo ""
echo "Files:"
ls -lh "${OUTPUT_DIR}"/*.so*

# Cleanup
echo ""
echo "==> Cleaning up build and install directories..."
rm -rf "${BUILD_DIR}"
rm -rf "${INSTALL_DIR}"

echo "==> Done!"
