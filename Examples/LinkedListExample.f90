! MIT License

! Copyright (c) 2010-present David A. Kopriva and other contributors: AUTHORS.md

! Permission is hereby granted, free of charge, to any person obtaining a copy  
! of this software and associated documentation files (the "Software"), to deal  
! in the Software without restriction, including without limitation the rights  
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell  
! copies of the Software, and to permit persons to whom the Software is  
! furnished to do so, subject to the following conditions:

! The above copyright notice and this permission notice shall be included in all  
! copies or substantial portions of the Software.

! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR  
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE  
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER  
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,  
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE  
! SOFTWARE.

! FTObjectLibrary contains code that, to the best of our knowledge, has been released as
! public domain software:
! * `b3hs_hash_key_jenkins`: originally by Rich Townsend, 
! https://groups.google.com/forum/#!topic/comp.lang.fortran/RWoHZFt39ng, 2005
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
            ALLOCATE(v); CALL v % initWithValue(values(j))
            obj => v
            CALL list % add(obj)
            CALL release(v)
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
         
         PRINT *, "Values in the list are:"
         DO WHILE( .NOT.iterator % isAtEnd() )
         
            v   => valueFromObject(iterator % object())
            PRINT *, v % integerValue()
            
            IF ( v % integerValue() == 5 ) objToDelete => iterator % object()
            
            CALL iterator % moveToNext() ! DON'T FORGET THIS CALL !
         END DO
!
!        ---------------------------------------
!        Insert a value after the tagged object.
!        ---------------------------------------
!
         ALLOCATE(v); CALL v % initWithValue(99); obj => v
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
         CALL release(iterator)
         CALL release(list)
                  
      END SUBROUTINE demonstrateLinkedList
   END MODULE linkedListDemonstrationModule
