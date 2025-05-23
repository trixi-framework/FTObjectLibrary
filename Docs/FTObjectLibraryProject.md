project: FTObjectLibrary
src_dir: ../Source
output_dir: ./HTMLdocs
summary: A Fortran library for reference counted object and container classes.
author: David A. Kopriva
author_description: Department of Mathematics, The Florida State University
email: kopriva@math.fsu.edu
website: https://www.math.fsu.edu/~kopriva
graph: true 
project_github: https://github.com/trixi-framework/FTObjectLibrary
source: false
predocmark:>

# Overview

 FTObjectLibrary provides a collection of reference counted Fortran 2003 classes to 
 facilitate writing generic object oriented Fortran programs. Reference counting
 is implemented to assist with memory management so that the lifespans of objects
 are properly maintained and are so that objects are deleted only when no other references are made to them.
 
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

# Documentation

Documentation can be found in the [user's guide](UsersGuide.md).

# Examples

Examples can be found in the [Examples](../Examples) directory and in the [Testing](../Testing) directory. The examples include a simple reverse Polish calculator using a stack, and another showing the use of a linked list. The testing directory includes tests that can be run on the library, which themselves serve as examples of the use of all of the classes.

# Building the Library

The library (.a) can be built with either CMake or default make.

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
