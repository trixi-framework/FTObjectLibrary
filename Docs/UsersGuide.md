# Introduction

FTObjectLibrary provides a collection of reference counted Fortran 2003
classes to facilitate writing generic object oriented Fortran programs.
Reference counting is implemented to assist with memory management so
that the lifespans of objects are properly maintained and are so that
objects are deleted only when no other references are made to them.

The library includes three categories of classes:

- Value classes

- Container classes

- Error reporting and testing classes

### Value Classes 

Value classes include the base class, FTObject and at the current time,
a subclass, FTValue.

- **FTObject**

  FTObject is the base class that implements the reference counting
  mechanism and other functions that should be overridden in subclasses.
  It the base class for all classes in the FTObjectLibrary library. You
  will usually not allocate objects of this class. Instead you will
  create your own subclasses of it that have data and procedures as
  needed.

- **FTValue**

  FTValue is a wrapper class that allows storage of real, integer,
  character and logical values, which can then be stored in containers.

You can create your own value classes by extending FTObject and store
instances of those classes in the containers.

### Container Classes 

Container classes let you store any subclass of the base class FTObject
in them. This makes it easy to store, for instance, a linked list of
linked lists, or an array of dictionaries. Included in the library are
the following container classes:

- **FTLinkedList**

  FTLinkedList is an implementation of a doubly (and, optionally,
  circular) linked list.

- **FTStack**

  FTStack is a subclass of FTLinkedList that adds the usual push, pop,
  and peek routines.

- **FTSparseMatrix**

  FTSparseMatrix associates a double index (i,j) to an FTObject.
  Basically this is a two dimensional sparse matrix of pointers to
  FTObjects.

- **FTMultiIndexTable**

  FTMultiIndexTable associates an integer array keys(:) to an FTObject.
  Basically this is an m--dimensional sparse matrix of pointers to
  FTObjects.

- **FTDictionary**

  FTDictionary is an "associative container", that associates a string
  to another FTObject.

- **FTValueDictionary**

  FTValueDictionary is a subclass of FTDictionary that has additional
  methods to store and retrieve values.

- **FTMutableObjectArray**

  A mutable one-dimensional array class that can store any FTObject.

### Error Reporting and Testing Classes

The library also contains classes for testing (FTAssertions,
TestSuiteManagerClass) and for reporting errors through the FTException
class.

# Examples

To give an idea about how to use the library, we show how to use a
dictionary to store a real value and a string that describes the value.
In the snippet of code below, we prepare the dictionary for objects to
be added to it by the init() method. We then create two values that are
added to the dictionary.

             SUBROUTINE constructDictionary(dict)
                     CLASS(FTDictionary), POINTER :: dict
                     CLASS(FTObject)    , POINTER :: obj
                     CLASS(FTValue)     , POINTER :: v
                     
                     ALLOCATE(dict)
                     CALL dict % initWithSize(64)
                 
                     ALLOCATE(v)
                     CALL v % initWithValue(3.14159)
                     obj => v
                     CALL dict % addObjectForKey(obj,``Pi'')
                     CALL releaseFTValue(v)
                 
                     ALLOCATE(v)
                     CALL v % initWithValue(``Ratio of circumference to diameter'')
                     obj => v
                     CALL dict % addObjectForKey(obj,"definition")
                     CALL releaseFTValue(v)
             END SUBROUTINE constructDictionary

Notice that in the subroutine we have allocated memory for the
dictionary "dict" and two FTValue objects. The dictionary is returned to
the calling procedure and can be accessed from that point through the
actual argument. The two value objects would normally be expected to be
deallocated when leaving the subroutine, since otherwise they would not
be accessible outside of the procedure. It would be dangerous if we
deallocated the two objects that we created and then try to use them
later through the dictionary. This is where a systematic approach to
memory management comes in. When we allocate and initialize an object,
we assume ownership of it. When we add it to the dictionary, it assumes
partial ownership. So instead of deallocating the two value objects, we
relinquish ownership by way of the releaseFTValue() procedure, leaving
only the dictionary to be responsible for deallocating them when it does
not need them any more.

We use the dictionary as shown in the next snippet of code:

                CLASS(FTDictionary), POINTER  :: dict
                CLASS(FTValue)     , POINTER  :: v
                REAL                          :: pi
                CALL constructDictionary(dict)
                
                v   => valueFromObject(dict % objectForKey("Pi"))
                pi  = v % realValue()
                
                v   => valueFromObject(dict % objectForKey(``definition''))
                PRINT *, "The num pi = ", pi," is defined as", TRIM(v % stringValue())
                CALL  releaseFTDictionary(dict)

In this snippet, the values for the two keys "Pi" and "definition" are
retrieved and then used.
The function valueFromObject() converts the generic object to the
specific FTValue type. (If we try to convert something that is not an
FTValue, the method returns an ASSOCIATED(v) == .FALSE. pointer.)
Finally, the dictionary is released, which will release all of its
objects and then deallocate all of those objects that it is sole owner
of, which in this example is both the value and definition of $\pi$, and
then DEALLOCATE it if it is no longer owned (referenced) by another
object, which is the case here.

In this way, any object that inherits from FTObject can be stored in a
container, including other containers, making these containers quite
generic.

# Memory Management

FTObjectLibrary uses a manual retain-release memory management model
known as reference counting. A description of this model can be found at
[https:developer.apple.com/library/mac/#documentation/cocoa/Conceptual/MemoryMgmt/Articles/mmRules.html#apple_ref/doc/uid/20000994-BAJHFBGH](https:developer.apple.com/library/mac/#documentation/cocoa/Conceptual/MemoryMgmt/Articles/mmRules.html#apple_ref/doc/uid/20000994-BAJHFBGH){.uri}.
The basic idea is that a pointer object exists as long as someone "owns"
it. When no one owns it any more, the object is automatically
deallocated upon release. This approach makes it easy to not have to
keep track of when to deallocate pointers, and ensures that a pointer
that is in use is not prematurely deallocated. In long, complex,
programs, this mechanism automatically keeps track of which objects
(pointers) that have been allocated need to be deallocated, and when.

Ownership rules are as follows:

- If you allocate and initialize an object, you own it.

- You own an object if you call the retain() subroutine on that object.

- You may inherit the ownership of an object by calling a method that
  creates (allocates and initializes) it.

- When you no longer need an object (or are going out of scope) you must
  release it using the releaseXXX() subroutine, where XXX refers to the
  specific name of the extended type.

- You must neither relinquish ownership, nor deallocate a pointer object
  that you do not own.

- You generally do not own objects returned as pointers by functions or
  subroutines.

*Fundamentally, if you call an init() method on an object, you should
call a releaseXXX() in the same scope.*

In the example below, the main program creates a linked list to store
points in, a "point" object (e.g. that stores x,y,z values), and adds
the point object to the linked list.

          PROGRAM main
             CLASS(FTLinkedList), POINTER :: list ! Subclass of FTObject
             CLASS(Point)       , POINTER :: pnt  ! Subclass of FTObject
             CLASS(FTObject)    , POINTER :: obj
             
             ALLOCATE(list)
             CALL list % init() ! main now owns the list
             
             ALLOCATE(pnt)
             CALL pnt % initWithXYZ(0.0,0.0,0.0) ! main now owns pnt
             
             obj => pnt
             CALL list % add(obj) !list also owns pnt
             CALL releasePoint(pnt) ! main gives up ownership to pnt
             .
             .
             .
             ! we're done with the list, it will deallocate pnt since the list is the last owner.
             ! It will also deallocate itself since main is the last owner.
             CALL releaseFTLinkedList(list) 

          END PROGRAM main

# Class Descriptions

## FTObject

FTObject defines the basic methods that are essential for reference
counted objects. FTObject is generally not going to be instantiated by
itself, but rather it will be subclassed and you will work with
instances of the subclasses. FTValue, for instance, is a subclass of
FTObject. Otherwise, pointers of type FTObject that point to instances
of subclasses will be stored in the container classes.

### Tasks

- **init()**

  Initializes an object and any memory that it needs to allocate, etc.
  Should be implemented in subclasses.The base class implementation does
  nothing but increase the reference count of the object.

- **printDescription(iUnit)**

  Prints a description of the object to a specified file unit. The base
  class implementation does nothing but print "FTObject"

- **copy()**

  Creates a copy (pointer) to the object of CLASS(FTObject) sourced with
  the object.

- **retain()**

  Increases the reference count of the object. Any procedure or object
  that retain()'s an object gains an ownership stake in that object.
  *This procedure is not overridable.*

- **releaseFTObject()**

  Decreases the reference count of a base class object pointer. To be
  called only by objects or procedures that have ownership in an object
  pointer, i.e., for which init() or retain() have been called.

- **isUnreferenced()**

  Test to see if there are no more owners of an object. Usually this is
  of interest only for debugging purposes. *This procedure is not
  overridable.*

- **refCount()**

  Returns the number of owners of an object. Usually this is of interest
  only for debugging purposes. *This procedure is not overridable.*

### Subclassing FTObject

In general, subclasses of FTObject implement

- **init()**

- **The destructor (FINAL)**

- **printDescription()**

- **release(self)**

**Implementing init()**

The init() procedure performs subclass specific operations to initialize
an object.

Subclasses that override init() *must* include a call to the super class
method. For example, if "Subclass" EXTENDS(FTObject), overriding init()
looks like

          SUBROUTINE initSubclass(self) 
             IMPLICIT NONE
             CLASS(Subclass) :: self
             
             CALL self % FTObject % init()
             Allocate and initialize all member objects
             ... Other Subclass specific code
          END SUBROUTINE initSubclass

You can have multiple initializers for an object, so it is also
worthwhile understanding the concept of the "designated initializer".
Generally speaking this is the initializer that has the most arguments.
It is also the only one that includes the call to the super class init
procedure. For example, the designated initializer for a "point" class
would be the one that takes the (x,y,z) values.

          SUBROUTINE initPointWithXYZ(self,x,y,z) 
             IMPLICIT NONE
             CLASS(Subclass) :: self
             
             CALL self % FTObject % init()
             self % x = x
             self % y = y
             self % z = z
          END SUBROUTINE initPointWithXYZ

Other initializers might be the default, and the initializer that takes
an array of length three. They will do nothing but call the designated
initializer. The default initializer sets the location to the origin, or
some other reasonable value.

          SUBROUTINE initPoint(self) 
             IMPLICIT NONE
             CLASS(Subclass) :: self
             
             call self % initPointWithXYZ(0.0,0.0,0.0)
          END SUBROUTINE initPoint

The array initializer is

          SUBROUTINE initPointWithArray(self,w) 
             IMPLICIT NONE
             CLASS(Subclass) :: self
             REAL            :: w(3)
             
             CALL self % initPointWithXYZ(w(1),w(2),w(3))
          END SUBROUTINE initPointWithArray

**Implementing the destructor**

The destructor reverses the operations done in the init() procedure. It
releases and deallocates any pointers that it owns. For example, if
"Subclass" EXTENDS(FTObject) then overriding destruct looks like

          SUBROUTINE destructSubclass(self) 
             IMPLICIT NONE
             TYPE(Subclass) :: self
             .
             Release and deallocate (if necessary) all member objects
             .
          END SUBROUTINE destructSubclass

The destructor is to be declared as a FINAL procedure for the type, and
hence is not called directly. It is called automatically by Fortran when
an object is deallocated or goes out of scope.

**Implementing printDescription(iUnit)**

printDescription is a method whose existence is to support debugging.
Call printDescription(iUnit) on any objects owned by self for a
cascading of what is stored in the object.

**Implementing releaseXXX(self)**

The release subroutine will call the base class releaseFTObject which
will, in turn, release all objects that it owns. If the object itself is
no longer referenced, it will deallocate itself. If the subclass is
going to be subclassed again, use the CLASS specifier, otherwise, we
TYPE to work only on that specific subclass.

          SUBROUTINE releaseSubclass(self)  
             IMPLICIT NONE
             TYPE(Subclass) , POINTER :: self
             CLASS(FTObject), POINTER :: obj
             obj => self
             CALL releaseFTObject(self = obj)
             IF ( .NOT. ASSOCIATED(obj) )     THEN
                self => NULL() 
             END IF      
          END SUBROUTINE releaseSubclass

It is best to name the release procedures consistently. For instance,
all the FTObjectLibrary classes declare their release procedure by using
the name of the extended type, for example releaseFTDictionary or
releaseFTValueDictionary.

**Converting an object from the base to a subclass**

Container classes and the copy function return pointers to a
CLASS(FTObject). To use any subclass features one must "cast" or convert
a pointer to a pointer to the subclass. We like to have a specific cast
routine to do this as painlessly as possible. Each subclass should
include a function like this:

          FUNCTION subclassFromSuperclass(obj) RESULT(cast)
             IMPLICIT NONE  
             CLASS(FTObject), POINTER :: obj
             CLASS(Subclass), POINTER :: cast
             cast => NULL()
             SELECT TYPE (e => obj)
                TYPE is (Subclass)
                   cast => e
                CLASS DEFAULT
             END SELECT
          END FUNCTION subclassFromSuperclass

You saw an example above in the "valueFromObject(obj)" function.

## FTValue

FTValue is an immutable class to
store primitive values: integer, real, double precision, logical,
character. (To Add: complex)

### Usage

- Initialization

                  TYPE(FTValue) :: r, i, s, l, d

                  CALL r % initWithValue(3.14)
                  CALL i % initWithValue(6)
                  CALL d % initWithValue(3.14d0)
                  CALL l % initWithValue(.true.)
                  CALL s % initWithValue("A string")

- Destruction

                  CALL releaseFTValue(r)    !For Pointers

- Accessors

                  real = r % realValue()
                  int  = i % integerValue()
                  doub = d % doublePrecisionValue()
                  logc = l % logicalValue()
                  str  = s % stringValue()

- Description

                  str = v % description()
                  call v % printDescription(unit)

- Converting a base FTObject to an FTValue

                  CLASS(FTValue) , POINTER :: v
                  CLASS(FTObject), POINTER :: obj
                  v => valueFromObject(obj)

The class will attempt to convert between the different types:

                CALL r % initWithReal(3.14)
                print *, r % stringValue()

                Logical variables rules:

                real, doublePrecision, integer values
                logicalValue = .FALSE. if input = 0
                logicalValue = .TRUE.  if input /= 0

String values can be converted to numeric types. If the string is not a
numeric, $Huge(x)$ will be returned, where $x$ is of the requested type.

## Linked Lists

A linked list is a dynamic container that links the objects added to it
in a chain. The chain can be of any length and objects can be added or
removed at any time and at any location in the chain. Linked lists are
useful when you don't know how many objects will be stored. They are
fast to add or delete objects from, but they are slow to access any
given object. To access objects, you start at the beginning of the chain
and then follow the chain it until you reach the desired entry. You use
a linked list when sequential access is more typical than random access.
Special classes of linked lists are singly linked lists, which can be
followed in only one direction from the start, double linked lists that
can be followed in either direction, and circular lists where the ends
are connected.

### FTLinkedList

FTLinkedList is a container class that stores objects in a doubly linked
list. Optionally, the list can be made circular. As usual, FTLinkedList
inherits from FTObjectClass.

**Definition (Subclass of FTObject):**

                   TYPE(FTLinkedList) :: list

**Usage:**

- Initialization

                         CLASS(FTLinkedList), POINTER :: list
                         ALLOCATE(list)
                         CALL list % init()

- Adding an object

                         CLASS(FTLinkedList), POINTER :: list
                         CLASS(FTObject)    , POINTER :: obj
          
                         obj => r                ! r is subclass of FTObject
                         CALL list % Add(obj)    ! Pointer is retained by list
                         CALL release(r)         ! If control is no longer wanted in this scope.

- Combining lists

                         CLASS(FTLinkedList), POINTER :: list, listToAdd
                         CLASS(FTObject)    , POINTER :: obj
                         .
                         .
                         .
                         CALL list % addObjectsFromList(listToAdd)

- Inserting objects

                         CLASS(FTLinkedList)      , POINTER :: list
                         CLASS(FTObject)          , POINTER :: obj
                         CLASS(FTLinkedListRecord), POINTER :: record
          
                         obj => r                            ! r is subclass of FTObject
                         CALL list % insertObjectAfterRecord(obj,record) ! Pointer is retained by list
                         CALL release(r)                ! If caller wants to reliquish ownership

  OR

                         CLASS(FTLinkedList)      , POINTER :: list
                         CLASS(FTObject)          , POINTER :: obj, otherObject
                         CLASS(FTLinkedListRecord), POINTER :: record
          
                         obj => r                            ! r is subclass of FTObject
                         CALL list % insertObjectAfterObject(obj,otherObject) ! Pointer is retained by list
                         CALL release(r)                ! If caller wants to reliquish ownership

- Removing objects

                         CLASS(FTLinkedList), POINTER :: list
                         CLASS(FTObject)    , POINTER :: obj
                         obj => r                 ! r is subclass of FTObject
                         CALL list % remove(obj)

- Getting an array of all of the objects

                         CLASS(FTLinkedList)         , POINTER :: list
                         CLASS(FTMutableObjectArray) , POINTER :: array
                         array => list % allObjects() ! Array has refCount = 1

- Making a linked list circular or not. The default is not circular.

                        LOGICAL :: c = .true.
                        CALL list % makeCircular( c )

- Checking to see if a linked list circular or not

                        LOGICAL :: c 
                        c = list % isCircular()

- Counting the number of objects in the list

                             n = list % count()

- Reversing the order of the list

                             CALL list % reverse()

- Destruction

                             CALL  releaseFTLinkedList(list) ! If list is a pointer

### FTLinkedListIterator

Linked lists must be navigated in order along the chain. This is usually
accomplished with the help of an *iterator* to navigate linked lists.
FTObjectLibrary includes a class called FTLinkedListIterator for
stepping through (iterating) a linked list to access its entries.

**Definition (Subclass of FTObject):**

                   TYPE(FTLinkedListIterator) :: iterator

**Usage:**

- Initialization

                    CLASS(FTLinkedListIterator), POINTER :: iterator
                    CLASS(FTLinkedList)        , POINTER :: list
                    ALLOCATE(iterator)
                    CALL iterator % init()
                    .
                    .
                    .

  Or

                    CLASS(FTLinkedList)        , POINTER :: list
                    CLASS(FTLinkedListIterator), POINTER :: iterator
                    ALLOCATE(iterator)
                    CALL iterator % initWithFTLinkedList(list) ! Increases refCount of list
                    .
                    .
                    .

- Setting the iterator to the beginning of the list

                     CALL iterator % setToStart()

- Testing if the iterator is at the end of the list

                     IF( iterator % isAtEnd() )

- Iterating through the list and accessing contained objects

                    CLASS(FTObject), POINTER :: obj
                    CALL iterator % setToStart()
                    DO WHILE (.NOT.iterator % isAtEnd())
                        obj => iterator % object()              ! if the object is wanted
                        recordPtr => iterator % currentRecord() ! if the record is wanted
                        
                        !Do something with object or record

                        CALL iterator % moveToNext() ! FORGET THIS CALL AND YOU GET AN INFINITE LOOP!
                    END DO

- Destruction

                     CALL  releaseFTLinkedListIterator(iterator)    ! If a pointer

## Stacks

A stack is a data structure that enforces last-in/first-out access to
the objects stored in it. One puts a new object onto the top of the
stack with a *push* operation, and pulls off the object at the top of
the stack with a *pop* operation. Usually one has the option to just
look at the top of the stack with a *peek* operation that doesn't remove
the top object. You would implement a Reverse Polish calculator with a
stack, for instance.

**Definition (Subclass of FTLinkedListClass):**

                TYPE(FTStack) :: stack

**Usage:**

- Initialization

                 ALLOCATE(stack) ! If stack is a pointer
                 CALL stack  %  init()

- Destruction

                 CALL  releaseFTStack(stack)    ! If stack is a pointer

- Pushing an object onto the stack

                 TYPE(FTObject) :: obj
                 obj => r1
                 CALL stack % push(obj)

- Peeking at the top of the stack

                 obj => stack % peek() ! No change of ownership
                 SELECT TYPE(obj)
                    TYPE is (*SubclassType*)
                       ! Do something with obj as subclass
                    CLASS DEFAULT
                       ! Problem with casting
                 END SELECT

- Popping the top of the stack

                 obj => stack % pop() ! Ownership transferred to caller.
                                      ! Call releaseFTObject(obj) when done with it

## Object Arrays

Fortran has pointers to arrays, but not arrays of pointers. To do the
latter, one creates a wrapper derived type and creates an array of that
wrapper type. Fortran arrays are great, but they are of fixed length,
and they don't easily implement reference counting to keep track of
memory. For that, we have the FTMutableObjectArray. Performance reasons
dictate that you will use regular arrays for numeric types and the like,
but for generic objects we would use an Object Array.

You initialize a FTMutableObjectArray with the number of objects that
you expect it to hold. However, it can re-size itself if necessary. To
be efficient, it adds more than one entry at a time given by the
"chunkSize", which you can choose for yourself. (The default is 10.)

**Definition (Subclass of FTObject):**

                TYPE(FTMutableObjectArray) :: array

**Usage:**

- Initialization

            CLASS(FTMutableObjectArray)  :: array
            INTEGER                      :: N = 11
            CALL array % initWithSize(N)

- Destruction

                 CALL  releaseFTMutableObjectArray(array) !If array is a pointer

- Adding an object

                 TYPE(FTObject) :: obj
                 obj => r1
                 CALL array % addObject(obj)

- Removing an object

                 TYPE(FTObject) :: obj
                 CALL array % removeObjectAtIndex(i)

- Accessing an object

                 TYPE(FTObject) :: obj
                 obj => array % objectAtIndex(i)

- Replacing an object

                 TYPE(FTObject) :: obj
                 obj => r1
                 CALL array % replaceObjectAtIndexWithObject(i,obj)

- Setting the chunk size

                 CALL array % setChunkSize(size)

- Getting the chunk size

                 i = array % chunkSize(size)

- Finding the number of items in the array

                 n =  array % count()

- Finding the actual allocated size of the array

                 n =  array % allocatedSize()

- Converting a base class pointer to an object array

                 Array =>  objectArrayFromObject(obj)

## Sparse Matrices

Hash tables are data structures designed to enable storage and fast
retrieval of key-value pairs. An example of a key-value pair is a
variable name ("gamma") and its associated value ("1.4"). The table
itself is typically an array. The location of the value in a hash table
associated with a key, $k$, is specified by way of a *hash function*,
$H(k)$. In the case of a variable name and value, the hash function
would convert the name into an integer that tells where to find the
associated value in the table.

A very simple example of a hash table is, in fact, a singly dimensioned
array. The key is the array index and the value is what is stored at
that index. Multiple keys can be used to identify data; a two
dimensional array provides an example of where two keys are used to
access memory and retrieve the value at that location. If we view a
singly dimensioned array as a special case of a hash table, its hash
function is just the array index, $H(j)=j$. A doubly dimensioned array
could be (and often is) stored columnwise as a singly dimensioned array
by creating a hash function that maps the two indices to a single
location in the array, e.g., $H(i,j) = i + j*N$, where $N$ is the range
of the first index, $i$.

Two classes are included in FTObjectLibrary. The first, FTSparseMatrix,
works with an ordered pair, (i,j), as the keys. The second,
FTMultiIndexTable, uses an array of integers as the keys.

Both classes include enquiry functions to see of an object exists for
the given keys. Otherwise, the function that returns an object for a
given key will return an UNASSOCIATED pointer if there is no object for
the key. Be sure to retain any object returned by the objectForKeys
methods if you want to keep it beyond the lifespan of the matrix or
table. For example,

               TYPE(FTObject) :: obj
               obj => matrix % objectForKeys(i,j)
               IF ( ASSOCIATED(OBJ) ) THEN
                 CALL obj % retain()
                     ! Cast obj to something useful
               ELSE
                 ! Perform some kind of error recovery
               END IF 

### FTSparseMatrix

The sparse matrix included in the FTObjectLibrary is very simple in that
it has a predefined hash function with two keys, $(i,j)$. You will
initialize the matrix with the number of rows.

**Definition (Subclass of FTObject):**

                TYPE(FTSparseMatrix) :: matrix

**Usage:**

- Initialization

            CLASS(FTSparseMatrix)  :: matrix
            INTEGER                :: N = 11 ! Number of rows
            CALL matrix % initWithSize(N)

- Destruction

                 CALL  releaseFTSparseMatrix(matrix)      !If matrix is a pointer

- Adding an object

                 TYPE(FTObject) :: obj
                 matrix % addObjectForKeys(obj,i,j)

- Accessing an object

                 TYPE(FTObject) :: obj
                 obj => matrix % objectForKeys(i,j)

- Checking if an entry exists

                 LOGICAL :: exists
                 exists = matrix % containsKeys(i,j)

### FTMultiIndexTable

An extension (not in the subclass sense) of the sparse matrix class is
the MultiIndexTable. It uses an integer array of keys instead of just an
(i,j) pair. A multiIndexTable can be used, for instance, to determine if
the four node ids of a face of an element match those of another face of
another element.

**Definition (Subclass of FTObject):**

                TYPE(FTMultiIndexTable) :: table

**Usage:**

- Initialization

            CLASS(FTMultiIndexTable)  :: table
            INTEGER                   :: N = 11 ! maximum over all keys
            CALL table % initWithSize(N)

- Destruction

                 CALL  releaseFTMultiIndexTable(table)      !If table is a pointer

- Adding an object

                 TYPE(FTObject) :: obj
                 INTEGER        :: keys(m) ! m = # keys
                 matrix % addObjectForKeys(obj,keys)

- Accessing an object

                 TYPE(FTObject) :: obj
                 INTEGER        :: keys(m) ! m = # keys
                 obj => matrix % objectForKeys(keys)

- Checking if an entry exists

                 LOGICAL :: exists
                 INTEGER :: keys(m) ! m = # keys
                 exists = matrix % containsKeys(keys)

## Dictionaries

A dictionary is a special case of a hash table that stores *key-value
pairs*. It is an example of what is called an "associative container".
In the implementation of FTObjectLibrary, the value can be any subclass
of FTObject and the key is a character variable. The library includes
the base dictionary that can store and retrieve any subclass of
FTObject. It also includes a subclass that is designed to store and
retrieve FTValue objects.

### FTDictionary

**Definition (Subclass of FTObject):**

                TYPE(FTDictionary) :: dict

**Usage:**

- Initialization

            CLASS(FTDictionary)  :: dict
            INTEGER              :: N = 16 ! Should be a power of two.
            CALL dict % initWithSize(N)

- Destruction

                 CALL releaseFTDictionary(dict)       !If dict is a pointer

- Adding a key-object pair

                    CLASS(FTDictionary), POINTER :: dict
                    CLASS(FTObject)    , POINTER :: obj
                    CHARACTER(LEN=M)             :: key
                    obj => r                            ! r is subclass of FTObject
                    CALL dict % addObjectForKey(obj,key)

- Accessing an object

                 TYPE(FTObject) :: obj
                 obj => dict % objectForKey(key)

- Converting a base class pointer to a dictionary

                 dict =>  dictionaryFromObject(obj)

- Getting all of the keys (The target of the pointer must be deallocated
  by the caller)

                 CHARACTER(LEN=FTDICT_KWD_STRING_LENGTH), POINTER :: keys(:)
                 keys =>  dict % allKeys()

- Getting all of the objects

                 CLASS(FTMutableObjectArray), POINTER :: objectArray
                 objectArray =>  dict % allObjects() ! The array is owned by the caller.

### FTValueDictionary

FTValueDictionary adds methods to store and retrieve FTValue objects.

**Definition (Subclass of FTDictionary):**

                TYPE(FTValueDictionary) :: dict

**Usage:**

- Adding a value

          CALL dict % addValueForKey(1,"integer")
          CALL dict % addValueForKey(3.14,"real")
          CALL dict % addValueForKey(98.6d0,"double")
          CALL dict % addValueForKey(.true.,"logical")
          CALL dict % addValueForKey("Hello World","string")

- Accessing a value

           i = dict % integerValueForKey("integer")
           r = dict % realValueForKey("real")
           d = dict % doublePrecisionValueForKey("double")
           l = dict % logicalValueForKey("logical")
           s = dict % stringValueForKey("string")

  Note that the FTValue class will do type conversion, so you can also
  access something like

           i = dict % integerValueForKey("real")
           s = dict % stringValueForKey("real")

- Converting an FTDictionary to an FTValueDictionary

                   valueDict => valueDictionaryFromDictionary(dict)

- Converting an FTObject to an FTValueDictionary

                   valueDict => valueDictionaryFromObject(obj)

# String Sets

A StringSet implements set operations on strings. An FTStringSet is an
unordered group of unique (case dependent) strings. It is primarily used
to find whether or not a given string is a member of a set of strings.
The class also includes usual set operations of intersection, union and
difference. To enumerate over the items in a set, use the strings()
procedure to return a pointer to an array of the strings in the set, and
then loop over that array.

An empty set can be initialized, to which strings can be added, or a set
can be initialized with an array of strings. The string length must be
FTDICT\_KWD\_STRING_LENGTH or less.

**Definition (Subclass of FTObject):**

                TYPE(FTStringSet) :: set

**Usage:**

- Initialization

            CLASS(FTStringSet)  :: set
            INTEGER              :: N = 16 ! Should be a power of two.
            CALL dict % initFTStringSet(FTStringSetSize = N)

            CLASS(FTStringSet)  :: set
            CHARACTER(LEN=*)    :: strings(:)
            CALL dict % initWithStrings(strings)

- Destruction

                 CALL releaseFTStringSet(set)       !If set is a pointer

- Adding a string

                    CHARACTER(LEN=M)  :: str
                    CALL set % addString(str)

- Testing for inclusion

                 LOGICAL :: test
                 CHARACTER(LEN=M) :: str
                 test = set % containsString(str)

- Finding the union of two sets (Release when through)

                 unionSet =>  set1 % unionWithSet(set2)
                 ...
                 call ReleaseFTStringSet(unionSet)

- Finding the intersection of two sets (Release when through)

                 intersectionSet =>  set1 % intersectionWithSet(set2)
                 ...
                 call ReleaseFTStringSet(intersectionSet)

- Finding the difference of two sets (Release when through)

                 differenceSet =>  set1 % setFromDifference(set2)
                 ...
                 call ReleaseFTStringSet(differenceSet)

- Converting a base class pointer to a set

                 set =>  FTStringSetFromObject(obj)

- Getting all of the strings (The target of the pointer must be
  deallocated by the caller)

                 CHARACTER(LEN=FTDICT_KWD_STRING_LENGTH), POINTER :: strings(:)
                 strings =>  set % strings()
                 ...
                 deallocate(strings)

# Advanced Classes: Testing and Error Reporting

The library includes classes for testing and reporting errors. Errors
are reported through instances of the FTException class. Testing is made
semi-automatic through the TestSuiteManager class and procedures defined
in the FTAssertions module.

## Assertions

An assertion is a true-false statement that you expect to be true.
Assertions are used to test for exceptional situations (AKA "Failures")
in a code. For example, knowing that density always must be positive,
you might assert that fact before using it, and if the result is false
generate an error. With FTObjectLibrary that would be

                CALL assert(rho > 0,``Density must be positive'')

Fortran does not have an assertion mechanism, and so pretty much
everyone writes their own. There are a couple of open source projects
available, but one never knows how actively they will be maintained. In
the grand Fortran tradition of writing one's own, FTObjectLibrary has an
(incomplete) assertion module.

To use assertions, you will USE the FTAssertions module and initialize
the assertions system by calling

                CALL initializeSharedAssertionsManager

During the course of your program, the sharedAssertionsManager will keep
track of the success or failure of the assertions that you make. You can
enquire at any time how many assertions have failed and how many
assertions have been made with the two enquiry functions

         INTEGER FUNCTION numberOfAssertionFailures()
         INTEGER FUNCTION numberOfAssertions()

You can get a summary of the assertions by calling the subroutine

         SUBROUTINE SummarizeFTAssertions(title,iUnit)  
            IMPLICIT NONE
            CHARACTER(LEN=*)                        :: title
            INTEGER                                 :: iUnit

When you are done, you finalize the sharedAssertionsManager with

                CALL finalizeSharedAssertionsManager

So how do you make assertions? FTObjectLibrary supplies two subroutines
that post failures to the sharedAssertionsManager. The first takes a
LOGICAL variable

          SUBROUTINE assert(test,msg)  
             IMPLICIT NONE
             CHARACTER(LEN=*), OPTIONAL :: msg
             LOGICAL                    :: test

The second tests equality through the overloaded subroutine assertEqual,
which allows a variety of argument type listed below:

           INTERFACE assertEqual
             MODULE PROCEDURE assertEqualTwoIntegers
             MODULE PROCEDURE assertEqualTwoIntegerArrays1D
             MODULE PROCEDURE assertEqualTwoIntegerArrays2D
             MODULE PROCEDURE assertWithinToleranceTwoReal
             MODULE PROCEDURE assertWithinToleranceTwoRealArrays1D
             MODULE PROCEDURE assertWithinToleranceTwoRealArrays2D
             MODULE PROCEDURE assertWithinToleranceTwoDouble
             MODULE PROCEDURE assertWithinToleranceTwoDoubleArrays1D
             MODULE PROCEDURE assertWithinToleranceTwoDoubleArrays2D
             MODULE PROCEDURE assertEqualTwoLogicals
             MODULE PROCEDURE assertEqualString
          END INTERFACE assertEqual

The individual calls have the signatures

          SUBROUTINE assertEqualTwoIntegers(expectedValue,actualValue,msg)  
             IMPLICIT NONE  
             INTEGER, INTENT(in)        :: expectedValue,actualValue
             CHARACTER(LEN=*), OPTIONAL :: msg
             
          SUBROUTINE assertEqualTwoIntegerArrays1D(expectedValue,actualValue)  
             IMPLICIT NONE  
             INTEGER, INTENT(in)    , DIMENSION(:)            :: expectedValue,actualValue
             
          SUBROUTINE assertEqualTwoIntegerArrays2D(expectedValue,actualValue)  
             IMPLICIT NONE  
             INTEGER, INTENT(in)    , DIMENSION(:,:)          :: expectedValue,actualValue
             
          SUBROUTINE assertWithinToleranceTwoReal(x,y,tol,absTol,msg)  
             IMPLICIT NONE  
             REAL, INTENT(in)           :: x,y,tol
             REAL, INTENT(IN), OPTIONAL :: absTol
             CHARACTER(LEN=*), OPTIONAL :: msg
             
          SUBROUTINE assertWithinToleranceTwoRealArrays1D(expectedValue,actualValue,tol,absTol,msg)  
             IMPLICIT NONE  
             REAL, INTENT(IN), DIMENSION(:) :: expectedValue,actualValue
             REAL, INTENT(IN)               :: tol
             REAL, INTENT(IN), OPTIONAL     :: absTol
             CHARACTER(LEN=*), OPTIONAL     :: msg
             
          SUBROUTINE assertWithinToleranceTwoRealArrays2D(expectedValue,actualValue,tol)  
             IMPLICIT NONE  
             REAL, INTENT(IN), DIMENSION(:,:) :: expectedValue,actualValue
             REAL, INTENT(IN)                 :: tol
             
          SUBROUTINE assertWithinToleranceTwoDouble(expectedValue,actualValue,tol,absTol,msg)  
             IMPLICIT NONE  
             DOUBLE PRECISION, INTENT(in) :: expectedValue,actualValue,tol
             REAL, INTENT(IN), OPTIONAL   :: absTol
             CHARACTER(LEN=*), OPTIONAL   :: msg
             
          SUBROUTINE assertWithinToleranceTwoDoubleArrays1D(expectedValue,actualValue,tol,absTol,msg)  
             IMPLICIT NONE  
             DOUBLE PRECISION, INTENT(IN), DIMENSION(:) :: expectedValue,actualValue
             DOUBLE PRECISION, INTENT(IN)               :: tol
             REAL, INTENT(IN), OPTIONAL                 :: absTol
             CHARACTER(LEN=*), OPTIONAL                 :: msg
             
          SUBROUTINE assertWithinToleranceTwoDoubleArrays2D(expectedValue,actualValue,tol,abstol)  
             IMPLICIT NONE  
             DOUBLE PRECISION, INTENT(IN), DIMENSION(:,:) :: expectedValue,actualValue
             DOUBLE PRECISION, INTENT(IN)                 :: tol
             REAL, INTENT(IN), OPTIONAL :: absTol
             
          SUBROUTINE assertEqualString(expectedValue,actualValue,msg)
             IMPLICIT NONE
             CHARACTER(LEN=*)           :: expectedValue,actualValue
             CHARACTER(LEN=*), OPTIONAL :: msg
             
          SUBROUTINE assertEqualTwoLogicals(expectedValue,actualValue,msg)  
             IMPLICIT NONE  
             LOGICAL, INTENT(in)        :: expectedValue,actualValue
             CHARACTER(LEN=*), OPTIONAL :: msg

Notice that you can only check the equality of two floating point
numbers to within some tolerance. One can either specify just a relative error tolerance, tol, or a relative and optional absolute tolerance, tol and absTol.

## Testing

FTObjectLibrary also includes a testing suite with which you can create
a suite of tests to make sure your codes are working and stay working.
The tests are managed by an instance of the class. It is designed to be
used with minimal fuss. You

1.  Initialize the test suite

2.  Add test subroutines

3.  Have the testSuiteManager perform the tests

4.  Finalize the test suite manager

An example of running a suite of tests is the following:

          TYPE(TestSuiteManager) :: testSuite
          
          EXTERNAL :: FTDictionaryClassTests
          EXTERNAL :: FTExceptionClassTests
          EXTERNAL :: FTValueClassTests
          EXTERNAL :: FTValueDictionaryClassTests
          EXTERNAL :: FTLinkedListClassTests
          EXTERNAL :: StackClassTests
          EXTERNAL :: MutableArrayClassTests
          EXTERNAL :: HashTableTests

          CALL testSuite % init()
          
          CALL testSuite % addTestSubroutineWithName(FTValueClassTests,"FTValueClass Tests")
          CALL testSuite % addTestSubroutineWithName(FTDictionaryClassTests,"FTDictionaryClass Tests")
          CALL testSuite % addTestSubroutineWithName(FTValueDictionaryClassTests,"FTValueDictionaryClass Tests")
          CALL testSuite % addTestSubroutineWithName(FTLinkedListClassTests,"FTLinkedListClass Tests")
          CALL testSuite % addTestSubroutineWithName(StackClassTests,"StackClass Tests")
          CALL testSuite % addTestSubroutineWithName(MutableArrayClassTests,"Mutable Array Tests")
          CALL testSuite % addTestSubroutineWithName(HashTableTests,"Hash Table Tests")
          CALL testSuite % addTestSubroutineWithName(FTExceptionClassTests,"FTExceptionClass Tests")

          CALL testSuite % performTests()

The test subroutines have no arguments. The interface is

          ABSTRACT INTERFACE
             SUBROUTINE testSuiteFunction()
             END SUBROUTINE testSuiteFunction
          END INTERFACE

The test functions should USE the FTAssertions module as in the previous
section. You don't have to do any reporting code in your tests, however.
Reporting is managed by the testSuiteManager at the end of performTests. Look at the Testing directory in the repository for examples on how to set up testing.

**Definition:**

                TYPE(TestSuiteManager) :: tester

**Usage:**

- Initialization

          CALL tester % init()

- Creating a test Create subroutines with the interface

            ABSTRACT INTERFACE
              SUBROUTINE testSuiteFunction(optData)
                 CHARACTER(LEN=1), POINTER, OPTIONAL :: optData(:) 
              END SUBROUTINE testSuiteFunction
            END INTERFACE

  that (typically) includes unit test calls. The optData is optional data that can be included as part
  of the test, if necessary. The Fortran TRANSFER function can be used to encode and decode the data, so pretty much any data can be transferred generically.

- Adding a test case

               CALL tester % addTestSubroutineWithName(SubroutineName, description)

  where

  - SubroutineName = a subroutine with the interface as above, and

  - description = a CHARACTER(LEN=128) character string that names the
    test

- Setting the output location

               CALL tester % setOutputUnit(iUnit)

- Running tests

               CALL tester % performTests()

## Exceptions

An FTException object provides a way to pass generic information about
an exceptional situation. Methods for dealing with exceptions are
defined in the SharedExceptionManagerModule module.

An FTException object wraps:

- A severity indicator

- A name for the exception

- An optional dictionary that contains whatever information is deemed
  necessary.

  It is expected that classes will define exceptions that use instances
  of the FTException Class.

Defined constants:

             FT_ERROR_NONE    = 0
             FT_ERROR_WARNING = 1
             FT_ERROR_FATAL   = 2

**Usage:**

- Initialization

                  e  %  initFTException(severity,exceptionName,infoDictionary)

                  Plus the convenience initializers, which automatically 
                  create a FTValueDictionary with a single key called "message":

                  e % initWarningException(msg = "message")
                  e % initFatalException(msg = "message")

- Setting components Create subroutines with the interface

                  e  %  setInfoDictionary(infoDictionary)
