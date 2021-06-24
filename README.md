# FTObjectLibrary

[![Build Status](https://github.com/trixi-framework/FTObjectLibrary/workflows/CI/badge.svg)](https://github.com/trixi-framework/FTObjectLibrary/actions?query=workflow%3ACI)
[![Coveralls](https://coveralls.io/repos/github/trixi-framework/FTObjectLibrary/badge.svg?branch=main)](https://coveralls.io/github/trixi-framework/FTObjectLibrary?branch=main)
[![Codecov](https://codecov.io/gh/trixi-framework/FTObjectLibrary/branch/main/graph/badge.svg)](https://codecov.io/gh/trixi-framework/FTObjectLibrary)
[![License: MIT](https://img.shields.io/badge/License-MIT-success.svg)](https://opensource.org/licenses/MIT)

FTObjectLibrary provides a collection of reference counted Fortran 2003 classes
to facilitate writing generic object oriented Fortran programs. Reference
counting is implemented to assist with memory management so that the lifespans
of objects are properly maintained and are so that objects are deleted only
when no other references are made to them.

**NOTE: This repository is in an experimental stage and may undergo breaking
changes at any time.**


FTObjectLibrary tries, as much as the maturity of Fortran compilers allow, to
use the new F2003/2008 features to make generic programming possible. The LCD
for the library is gfortran, and as modern features get implemented in the
compiler, FTObjectLibrary will be updated to include those features. In the meantime, there
are a few workarounds that exist in the code.
 
The library includes three categories of classes:

* Value classes
* Container classes
* Error reporting and testing classes

Value classes include the base class, FTObject and at the current time, a subclass, FTValue.

- FTObject is the base class that implements the reference counting mechanism and other functions that should be overridden in subclasses. It the base class for all classes in the FTObjectLibrary library. You will usually not allocate objects of this class. Instead you will create your own subclasses of it that have data and procedures as needed.
- FTValue is a wrapper class that allows storage of real, integer, character and logical values, which can then be stored in containers.

Container classes let you store any subclass of the base class FTObject in them. This makes it easy to store, for instance, a linked list of linked lists, or an array of dictionaries.

Included in the library are the following standard container classes:

- FTStack is a subclass of FTLinkedList that adds the usual push, pop, and peek routines.
- FTSparseMatrix associates a double index (i,j) to an FTObject. Basically this is a two dimensional sparse matrix of pointers to FTObjects.
- FTMultiIndexTable associates an integer array keys(:) to an FTObject. Basically this is an m--dimensional sparse matrix of pointers to FTObjects.
- FTDictionary is an ``associative container'', that associates a string to another FTObject. 
- FTValueDictionary is a subclass of FTDictionary that has additional methods to store and retrieve
values.
- FTMutableObjectArray is a mutable one-dimensional array class that can store any FTObject
- FTStringSet is different as it encapsulates set operations like set membership, union, intersection, difference, but only for strings. Although other objects cannot be added to string sets, StringSets can be added to other containers

 The library also contains classes for testing (FTAssertions, TestSuiteManagerClass) and for reporting errors through the FTException class.

## Documentation

Documentation can be found in the linked pages, and in the user's guide *FTObjectLibrary.pdf* found in the Docs directory.

## Examples

Examples can be found in the Examples directory and in the Testing directory. The examples include a simple reverse Polish caclulator using a stack, and another showing the use of a linked list. The testing directory includes tests that can be run on the library, which themselves serve as examples of the use of all of the classes.

## Building the Library

The library (.a) can be built with either CMake or default make.

### Spack
You can install FTObjectLibrary using the [Spack package manager](https://spack.io). 
To install the FTObjectLibrary with Spack,
```
git clone https://github.com/spack/spack.git ~/spack
source ~/spack/share/spack/setup-env.sh
spack install ftobjectlibrary@main
```
Once installed, FTObjectLibrary can be added to your environment using
```
spack load ftobjectlibrary
```


### CMake
To install the FTObjectLibrary with CMake,
```
mkdir build && cd build
cmake -DCMAKE_INSTALL_PREFIX=/path/to/install ../
make
make install
```

### make

cd to the directory "makeLibrary" and type
```
make
```

That will create the necessary files in that directory, which can be moved to somewhere else as desired.

### Updating Spack Packages
When new releases of FTObjectLibrary are made, you can contribute the new release to the spack packages.
Before making an update to the Spack package, you should familiarize yourself with [the Spack packaging guide](https://spack.readthedocs.io/en/latest/packaging_guide.html)


To add a new FTObjectLibrary release to Spack,
1. Fork the Spack repository on Github to your personal account or the trixi-framework organization account.

2. Install and initialize spack on your local system from your fork
```
git clone https://github.com/YOUR GITHUB ACCOUNT/spack.git ~/spack
source ~/spack/share/spack/setup-env.sh
```

3. Open the FTObjectLibrary package for editing
```
spack edit ftobjectlibrary
```

4. Add a new `version` metadata item to the ftobjectlibrary package. The first argument is the version name as it will appear in the spack package manager. Use the `tag` argument to specify the name of the tag as it appears in the FTObjectLibrary repository. As an example, a new version line for `v1.0.1` is shown below.
```
    version('v1.0.1', tag='v1.0.1')
```

5. (Optional) If you would like to be noted as a maintainer, add your github handle to the maintainers list. Maintainers will be notified if Spack users experience issues installing FTObjectLibrary and when modifications are being made to the FTObjectLibrary package in Spack.
```
    maintainers = ['schoonovernumerics','your-github-handle']
```

6. When you are finished editing, save the package file. You can verify the new version is registered in your local repository by obtaining a `spec` for FTObjectLibrary at the new version you've added.
```
spack spec ftobjectlibrary@v1.0.1
```

7. Test to make sure the installation works
```
spack install ftobjectlibrary@v1.0.1
```

8. Run the `spack style` command to ensure that you are meeting Spack's style requirements.

9. When ready, commit your changes and push them to your Github repository.

10. Open a Pull Request with `github.com/spack/spack` to merge your changes with the `spack/spack/develop` branch.

## Authors
FTObjectLibrary was initiated by
[David A. Kopriva](https://www.math.fsu.edu/~kopriva/), who is also the principal developer.
The full list of contributors can be found in [AUTHORS.md](AUTHORS.md).


## License and contributing
FTObjectLibrary is licensed under the MIT license (see [LICENSE.md](LICENSE.md)).
