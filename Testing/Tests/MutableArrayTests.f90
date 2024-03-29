! MIT License
!
! Copyright (c) 2010-present David A. Kopriva and other contributors: AUTHORS.md
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in all
! copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.
!
! FTObjectLibrary contains code that, to the best of our knowledge, has been released as
! public domain software:
! * `b3hs_hash_key_jenkins`: originally by Rich Townsend,
! https://groups.google.com/forum/#!topic/comp.lang.fortran/RWoHZFt39ng, 2005
!
! --- End License

!
!////////////////////////////////////////////////////////////////////////
!
!      MutableArrayTests.f90
!      Created: June 12, 2013 10:46 AM
!      By: David Kopriva
!
!      This subroutine tests and shows how to use the FTMutableObjectArray
!      class.
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE MutableArrayClassTests
         USE FTAssertions
         USE FTMutableObjectArrayClass
         USE FTValueClass
         IMPLICIT NONE
!
!        ------------
!        Declarations
!        ------------
!
         TYPE (FTMutableObjectArray) , POINTER :: array
         CLASS (FTMutableObjectArray), POINTER :: arrayPtr

         INTEGER                    :: i
         INTEGER, DIMENSION(10)     :: values         = [(i,i=1,10)]
         INTEGER, DIMENSION(10)     :: modifiedValues = [1,2,3,4,22,6,7,9,10,11]

         CLASS(FTObject), POINTER   :: obj
         TYPE (FTValue) , POINTER   :: v
!
!        ------------------------------------------------------------
!        Initialize an empty array with an initial size of 10 objects
!        ------------------------------------------------------------
!
         ALLOCATE(array)
         CALL array % initwithsize(10)
         CALL FTAssertEqual(0,array % COUNT(),"Initial array count")
         CALL array % setChunkSize(chunkSize = 15)
         CALL FTAssertEqual(expectedValue = 15, &
                            actualValue   = array % chunkSize(), &
                            msg           = "Chunk size set to 15")
         CALL array % setChunkSize(chunkSize = 10) !Set back to default for later tests
!
!        --------------
!        Check its type
!        --------------
!
         CALL FTAssertEqual(expectedValue = "FTMutableObjectArray",&
                            actualValue   = array % className(),   &
                            msg = "Test class name on mutable object array")
!
!        --------------------------------------------------------
!        Add objects to the array
!        Normally we should check the deallocation status
!        when we release an object, but we know we have just
!        added it to the array, so the array will have ownership.
!        Ditto on any releases below.
!        --------------------------------------------------------
!
         DO i = 1, 10
            ALLOCATE(v)
            CALL v % initwithValue(values(i))
            obj => v
            CALL array % addObject(obj)
            CALL FTAssertEqual( 2, v % refCount(), "Adding object adds ownership" )
            CALL releaseFTValue(v)
         END DO
         CALL FTAssertEqual(10, array % COUNT(), "Number of objects in array is equal to number of objects added")
!
!        ---------------------------------------
!        Exercise output routine to a dummy file
!        ---------------------------------------
!
         OPEN(UNIT = 7, STATUS = 'SCRATCH')
         CALL array % printDescription(7) ! To print the array, if desired.
         CLOSE(7)
!
!        -----------------------------
!        Check the values in the array
!        -----------------------------
!
         DO i = 1, 10
            obj => array % objectAtIndex(i)   ! Get the object
            v   => valueFromObject(obj)       ! Convert it to a value.
            CALL FTAssert(test = ASSOCIATED(v),msg = "Object not found at index")
            IF ( ASSOCIATED(v) )     THEN
               CALL FTAssertEqual(values(i),v % integerValue(),"Object values")
            END IF
         END DO
!
!        ---------------------------------------------------
!        Replace the object at index 5
!        The object that was there will be deallocated since
!        its sole owner is the array.
!        ---------------------------------------------------
!
         ALLOCATE(v)
         CALL v % initwithValue(22)
         obj => v
         CALL array % replaceObjectAtIndexWithObject(5,obj)
         CALL FTAssertEqual(2,v % refCount(),"Replacement refCount")
         CALL releaseFTValue(v)
!
!        ----------------------
!        Get the replaced value
!        ----------------------
!
         obj => array % objectAtIndex(5)
         v   => valueFromObject(obj = obj)
         CALL FTAssertEqual(22, v % integerValue(),"Replacement value")
         CALL FTAssertEqual(1, v % refCount(),"Refcount after main release")
!
!        ------------------------------------------
!        Add another value - this should reallocate
!        ------------------------------------------
!
         ALLOCATE(v)
         CALL v % initWithValue(11)
         obj => v
         CALL array % addObject(obj)
         CALL releaseFTValue(v)
         CALL FTAssertEqual(11, array % COUNT(), "Number of objects in array is increased")
         CALL FTAssertEqual(20, array % allocatedSize(),"Memory increased by chunk size")
!
!        -----------------------------------------------
!        Remove an item from the array. For this test we
!        will keep ownership of the item to make sure
!        that it is removed properly.
!        -----------------------------------------------
!
         obj => array % objectAtIndex(8)
         CALL obj % retain()
         CALL array % removeObjectAtIndex(8)
         CALL FTAssertEqual(10, array % COUNT(), "Item deleted count")
         CALL FTAssertEqual(1, obj % refcount(), "Refcount after removal")
         CALL releaseFTObject(obj)
         IF ( .NOT. ASSOCIATED(obj) )     THEN
            CALL FTAssert(.true., "Object properly deallocated")
         ELSE
            CALL FTAssert(.FALSE., "Object properly deallocated")
         END IF
!
!        -----------------------------------
!        Check the values in the array again
!        -----------------------------------
!
         DO i = 1, 10
            obj => array % objectAtIndex(i)
            v   => valueFromObject(obj)
            CALL FTAssertEqual(modifiedValues(i),v % integerValue(),"Object values after deletion")
         END DO
!
!        -------
!        Casting
!        -------
!
         obj      => array
         arrayPtr => objectArrayFromObject(obj)
         CALL FTAssert(ASSOCIATED(arrayPtr),msg = "association by casting with function call")
         IF ( ASSOCIATED(arrayPtr) )     THEN
            CALL FTAssertEqual(expectedValue = array % COUNT(),    &
                               actualValue   = arrayPtr % COUNT(), &
                               msg           = "Cast by function pointer")
         ELSE 
            CALL FTAssert(.FALSE.,msg = "casting of array pointer failed") 
         END IF 
         arrayPtr => NULL()
         CALL castToMutableObjectArray(obj,arrayPtr)
         CALL FTAssert(ASSOCIATED(arrayPtr),msg = "association by casting with subroutine call")
         IF ( ASSOCIATED(arrayPtr) )     THEN
            CALL FTAssertEqual(expectedValue = array % COUNT(),    &
                               actualValue   = arrayPtr % COUNT(), &
                               msg           = "Cast by subroutine call")
         ELSE 
            CALL FTAssert(.FALSE.,msg = "casting of array pointer failed") 
         END IF 
!
!        -------------------------------------------------------------
!        Release array contents
!        -------------------------------------------------------------
!
         CALL releaseFTMutableObjectArray(array)

      END SUBROUTINE MutableArrayClassTests
