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
         TYPE(FTMutableObjectArray) :: array
         INTEGER                    :: i
         INTEGER, DIMENSION(10)     :: values         = [(i,i=1,10)]
         INTEGER, DIMENSION(10)     :: modifiedValues = [1,2,3,4,22,6,7,9,10,11]

         CLASS(FTObject), POINTER   :: obj
         CLASS(FTValue) , POINTER   :: v
!
!        ------------------------------------------------------------
!        Initialize an empty array with an initial size of 10 objects
!        ------------------------------------------------------------
!
         CALL array % initwithsize(10)
         CALL FTAssertEqual(0,array % COUNT(),"Initial array count")
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
            CALL v % release()
         END DO
         CALL FTAssertEqual(10, array % COUNT(), "Number of objects in array is equal to number of objects added")
!
!        -----------------------------
!        Check the values in the array
!        -----------------------------
!
!         CALL array % printDescription(6) ! To print the array, if desired.
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
         CALL v % release()
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
         CALL v % release()
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
         CALL obj % release()
         IF ( obj % isUnreferenced() )     THEN
            DEALLOCATE(obj)
            CALL FTAssert(.true., "Object properly deallocated") 
         ELSE
            CALL FTAssert(.FALSE., "Object properly deallocated") 
         END IF 
!
!        -----------------------------------
!        Check the values in the array again
!        -----------------------------------
!
!         CALL array % printDescription(6)
         DO i = 1, 10
            obj => array % objectAtIndex(i)
            CALL cast(obj,v)
            CALL FTAssertEqual(modifiedValues(i),v % integerValue(),"Object values after deletion")
         END DO
!
!        -------------------------------------------------------------
!        Release array contents
!        array is not a pointer, so there is no need to deallocate it.
!        -------------------------------------------------------------
!
         CALL array % release()
         
      END SUBROUTINE MutableArrayClassTests
