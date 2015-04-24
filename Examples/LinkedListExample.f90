!
!////////////////////////////////////////////////////////////////////////
!
!      demonstrateLinkedList.f90
!      Created: July 29, 2014 at 12:50 PM 
!      By: David Kopriva  
!
!////////////////////////////////////////////////////////////////////////
!
   MODULE linkedListDemonstrationModule
      CONTAINS 
      
      SUBROUTINE demonstrateLinkedList
         USE FTLinkedListClass  
         USE FTLinkedListIteratorClass
         USE FTValueClass
         
         IMPLICIT NONE
         CLASS(FTLinkedList)        , POINTER :: list
         CLASS(FTLinkedListIterator), POINTER :: iterator
         CLASS(FTValue)             , POINTER :: v
         CLASS(FTObject)            , POINTER :: obj, objToDelete
         
         INTEGER, DIMENSION(6)                :: values = [1,3,5,7,9,11]
         INTEGER                              :: j
!
!        --------------------------------
!        Allocate and initialize the list
!        --------------------------------
!
         ALLOCATE(list)
         CALL list % init()
!
!        ------------------------------------------
!        Add some values to the list. Any subclass
!        of FTObject can be added to the list, 
!        including other lists.
!        Let the list take ownership of the objects.
!        -------------------------------------------
!
         DO j = 1, 6
            ALLOCATE(v)
            CALL v % initWithValue(values(j))
            obj => v
            CALL list % add(obj)
            CALL v % release()
         END DO  
         PRINT *, "There are ", list % COUNT(), " records in the list"
!
!        --------------------------------------------------
!        Iterate through the list and print the values.
!        Tag (point) the entry with value = 5 to use later.
!        --------------------------------------------------
!
         ALLOCATE(iterator)
         CALL iterator % initWithFTLinkedList(list)
         CALL iterator % setToStart()
         
         PRINT *, "Values in the list are:"
         DO WHILE( .NOT.iterator % isAtEnd() )
         
            obj => iterator % object()
            v => valueFromObject(obj)
            PRINT *, v % integerValue()
            
            IF ( v % integerValue() == 5 ) objToDelete => obj
            
            CALL iterator % moveToNext() ! DON'T FORGET THIS CALL !
         END DO
!
!        ---------------------------------------
!        Insert a value after the tagged object.
!        ---------------------------------------
!
         ALLOCATE(v)
         CALL v % initWithValue(99)
         obj => v
         CALL list % insertObjectAfterObject(obj,objToDelete)
         PRINT *, "After adding 99 after value 5, the values in the list are:"
         CALL list % printDescription(iUnit = 6)
!
!        ------------------------
!        Delete the tagged object
!        ------------------------
!
         CALL list % remove(objToDelete)
         PRINT *, "After Deleting the value 5, the values in the list are:"
         CALL list % printDescription(iUnit = 6)
!
!        ----------------
!        Reverse the list
!        ----------------
!
         CALL list % reverse()
         PRINT *, "After reversing the list:"
         CALL list % printDescription(iUnit = 6)
!
!        --------
!        Clean up
!        --------
!
         CALL iterator % release()
         IF(iterator % isUnreferenced()) DEALLOCATE(iterator)
         
         CALL list % release()
         IF(list % isUnreferenced()) DEALLOCATE(list)
         
      END SUBROUTINE demonstrateLinkedList
   END MODULE linkedListDemonstrationModule
