### 0.9.46
* IpOpt netstandard2.0
* builder extensions for IpOpt

### 0.9.45
* MacOS/Linux builds for IpOpt should now truly be self-contained.

### 0.9.44
* updated build system
* added IpOptSharp

### 0.9.43
* added `AddCostFunction` overloads with 5,6,7 blocks

### 0.9.42
* build CeresNative dependencies with `_DISABLE_CONSTEXPR_MUTEX_CONSTRUCTOR`

### 0.9.41
* workaround for https://stackoverflow.com/questions/78598141/first-stdmutexlock-crashes-in-application-built-with-latest-visual-studio

### 0.9.40
* tried to fix https://stackoverflow.com/questions/78598141/first-stdmutexlock-crashes-in-application-built-with-latest-visual-studio

### 0.9.39
* updated build

### 0.9.38
* updated dependencies (Aardvark.Base)

### 0.9.37
* added `AddCostFunction` overloads for 5-8 parameter blocks
* added `TrySolve` returning whether or not the solution is usable and converged

### 0.9.36
* added MaxIterations/Tolerances to CeresBundleIteration 
* updated Ceres packages and added glfags

### 0.9.35
* fixed camera covariance orientation

### 0.9.34
* fixed covariance problem

### 0.9.33
* fixed distortion

### 0.9.32
* Covariance estimation index fix

### 0.9.31
* Covariance estimated using CovarianceAlgorithmType::DENSE_SVD

### 0.9.30
* index-checks in cOptimizePhotonetwork

### 0.9.29
* covariances

### 0.9.28
* fixed differential pose problem

### 0.9.27
* useDifferentialPoses flag

### 0.9.26
* nonmonotonic flag for PhotoNetwork optimize

### 0.9.25
* resdiual weights 

### 0.9.24
* fixed points for PhotoNetwork v2

### 0.9.23
* fixed points for PhotoNetwork

### 0.9.22
* explicit distortions 

### 0.9.21
* mingw suitesparse for windows

### 0.9.20
* added sparse features 

### 0.9.19
* num_threads is now ProcessorCount

### 0.9.18
* PhotoNetwork fixed

### 0.9.17
* PhotoNetwork distortion

### 0.9.16
* PhotoNetwork properly handling aspect

### 0.9.15
* fixed parameter blocks in PhotoNetwork optimize.

### 0.9.14 
* usable PhotoNetwork optimize

### 0.9.13 
* initial PhotoNetwork optimize

### 0.9.12
* updated to Aardvark.Base 5.2

### 0.9.11
* Aardvark.Build version

### 0.9.11-prerelease0001
* initial Aardvark.Build version 

### 0.9.10
* started RELEASE_NOTES.md