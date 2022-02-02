#!/bin/sh
brew install glog gflags eigen suite-sparse
echo "${pwd}"
rm -dfr cerestmp
git clone https://github.com/ceres-solver/ceres-solver.git ./cerestmp -b 2.0.0 --depth 1
cmake S ./cerestmp -B ./cerestmp/bb -DBUILD_TESTING=OFF -DBUILD_EXAMPLES=OFF -DCMAKE_INSTALL_PREFIX=./ceres 
cmake --build cerestmp/bb -j4
cmake --install cerestmp/bb
rm -dfr cerestmp
