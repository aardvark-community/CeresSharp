# CeresSharp

[![Publish](https://github.com/aardvark-community/CeresSharp/actions/workflows/publish.yml/badge.svg)](https://github.com/aardvark-community/CeresSharp/actions/workflows/publish.yml)
[![Version](https://img.shields.io/nuget/vpre/Ceres)](https://www.nuget.org/packages/Ceres/)
[![Downloads](https://img.shields.io/nuget/dt/Ceres)](https://www.nuget.org/packages/Ceres/)

Simple Ceres wrapper written in F# including aardvark-style native dependencies

## Building Locally

### Prerequisites

* `cmake`
* `dotnet >= 6.0`
* C/C++ compiler

### Steps
1. setup environment variable `GITHUB_TOKEN` with a token having at least read-access to packages on github (see [here](https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/creating-a-personal-access-token) how to create a token)
2. run `buildnative`
3. `dotnet tool restore`
4. `dotnet paket restore`
5. `dotnet build src/Ceres.sln`

## Pushing Packages

New Packages can be created by simply adding a new version in `RELEASE_NOTES.md` and pushing the file to the `master` branch.
The GitHub [CI Action](https://github.com/aardvark-community/CeresSharp/actions/workflows/publish.yml) will take care of building native code and publishing packages to NuGet/GitHub Packages

