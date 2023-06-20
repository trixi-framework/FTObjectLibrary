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
!      FTLinkedListUnitTests.f90
!      Created: January 15, 2013 5:25 PM 
!      By: David Kopriva
!
!      Create a linked list of FTValue objects. The class can store
!      any objects inheriting from FTObject. For instance, one could
!      have a linked list whose first record is a linked list and second
!      is a value.
!
!      The FTLinkedListIterator class, used for stepping through a linked
!      list, is also and tested demonstrated here.
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE FTLinkedListClassTests  
         IMPLICIT NONE

         CALL basicTests
         CALL TestDeletingObjects
         CALL TestAppendingLists
         CALL TestInsertingObjects
         
      END SUBROUTINE FTLinkedListClassTests
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE basicTests
         USE FTAssertions
         USE FTValueClass
         USE FTLinkedListClass
         USE FTLinkedListIteratorClass
         IMPLICIT NONE
!
!        ------------------------------------------------------
!        r1,r2,r3 are three object pointers to be stored in the
!        linked list
!        ------------------------------------------------------
!
         TYPE(FTValue), POINTER :: r1, r2, r3 
!
!        ------------------------------------------------------------
!        We need a pointer of the base class type to pass to the list
!        ------------------------------------------------------------
!
         CLASS(FTObject), POINTER :: objectPtr
!
!        -------------------------------------------------
!        Here we will define the list as a pointer, but it
!        can be a non-pointer, too, like the iterator.
!        -------------------------------------------------
!
         CLASS (FTLinkedList)       , POINTER :: list, listPtr
         TYPE (FTLinkedListIterator), POINTER :: iterator
         
         INTEGER                      :: i
         REAL                         :: singleTol = 2*EPSILON(1.0e0)
!
!        -------------------------------------------------------
!        Allocate and initialize the linked list. Remember that
!        init initializes the object with reference count of one
!        and ownership to this calling subroutine.
!        -------------------------------------------------------
!
         ALLOCATE(list)
         CALL list % init()
         CALL FTAssertEqual(expectedValue = "FTLinkedList", &
                            actualValue   = list % className(), &
                            msg           = "Class name test for linked list")
!
!        --------------------------------------------------------------------------
!        Test Reference counting.
!           Retaining an object implies that this subroutine wants to
!           share ownership of an object. (In fact, it already does, so the retain
!           is redundant.)
!           Releasing an object implies that the caller releases its share of
!           the object. Note that, because of the init call, we still own the
!           object after the retain+release. Finally, remember to balance init/retains
!           and releases: init + # retains = # releases
!        --------------------------------------------------------------------------
!
         CALL FTAssertEqual(1,list % refCount(),"Reference counting: Initial object refCount")
         CALL list % retain()
         CALL FTAssertEqual(2,list % refCount(), "Reference counting: Test retain")
         CALL releaseFTLinkedList(list)
         CALL FTAssertEqual(1,list % refCount(),"Reference counting: test release")
!
!        ---------------------------------------------------------------------------------
!        A linked list that is just initialized has no items in it, so its COUNT is zero. 
!        ---------------------------------------------------------------------------------
!
         CALL FTAssertEqual(0,list % COUNT(),"Initial list size")
!
!        ------------------------------------------------------------------
!        Add some items to the linked list.
!        As we add items the list's count increases.
!        Also, the list takes a share of ownership of the object added
!        so the reference count of the object is increased. Once we
!        add an object to the list, we may not need it anymore, in which case 
!        we reliquish our share of the ownership by releasing it. We should
!        check to see if the object needs to be deallocated for safety.
!
!        The first item that we add is an FTValueObject that represents an
!        integer.
!        ------------------------------------------------------------------
!
         ALLOCATE(r1)
         CALL r1 % initWithValue(1)
         objectPtr => r1
         CALL list % add(objectPtr)
         CALL FTAssertEqual(1,list % COUNT(),"List size after adding one object")
!         
         CALL FTAssertEqual(2,r1 % refCount(),&
         "Reference counting: Stored object should have reference count increased")
         CALL releaseFTValue(r1)
            
         CALL FTAssertEqual(1,objectPtr % refCount(),&
         "Reference counting: Stored object should have reference count decreased")
!
!        ---------------------------------------------------------------------
!        The second item in the list is an FTValueObject that stores a string.
!        We will use r2 later in the subroutine, so we don't give up ownership
!        at this time.
!        ---------------------------------------------------------------------
!
         ALLOCATE(r2)
         CALL r2 % initWithValue("r2 is a string")
         objectPtr => r2
         CALL list % add(objectPtr)
         CALL FTAssertEqual(2,list % COUNT(),"List size after adding second object")
!
!        -------------------------------------
!        The final object we add stores a real
!        -------------------------------------
!
         ALLOCATE(r3)
         CALL r3 % initWithValue(3.14)
         objectPtr => r3
         CALL list % add(objectPtr)
         CALL FTAssertEqual(3,list % COUNT(),"List size after adding third object")
         CALL releaseFTValue(r3)
!
!        -------
!        Casting
!        -------
!
         objectPtr => list
         CALL cast(objectPtr, listPtr)
         CALL FTAssert(ASSOCIATED(list,listPtr),msg = "Cast by subroutine call" )
         listPtr => linkedListFromObject(obj = objectPtr)
         CALL FTAssert(ASSOCIATED(list,listPtr),msg = "Cast by function call" )
!
!        ---------------------------------------------------------------------------------
!        Check integrity of stored objects. We iterate
!        through the linked list with an iterator. The
!        iterator is initialized with the list, which
!        is why we have defined the list as a pointer.
!        Note that the init function gives us ownership
!        of the iterator, and the iterator takes an ownership
!        stake in the list. When we get to the end of this subroutine,
!        since we are the sole owner of the iterator (we could check with  % shouldDestruct),
!        we must destruct the iterator. Since the iterator is the sole owner of
!        the list after we release it, it will destruct the list.
!        ---------------------------------------------------------------------------------
!
         ALLOCATE(iterator)
         CALL iterator % initWithFTLinkedList(list)
         CALL FTAssertEqual(2,list % refCount(),"Ref count increase on addition of list to iterator")
         CALL FTAssertEqual(expectedValue = "FTLinkedListIterator", &
                            actualValue   = iterator % className(), &
                            msg           = "Class name test for linked list")
         CALL FTAssert(ASSOCIATED(list, iterator % linkedList()),msg = "List stored vs list returned")
!
!        -------------------------------------------
!        Iterate through the list from the beginning
!        -------------------------------------------
!
         CALL iterator % setToStart() !(This is automatically done in the init)
         i = 1
         
         DO WHILE (.NOT.iterator % isAtEnd() )
!
!           --------------------------------------------------------------------------
!           Get a pointer to the iterator's current object. Just pointing does not
!           imply ownership, so if we wanted to save the reference to the object after
!           destructing the iterator and list, we'd have to retain it.
!           --------------------------------------------------------------------------
!
            objectPtr => iterator % object()
            
            SELECT TYPE(v=>objectPtr)
               TYPE IS (FTValue)
                 SELECT CASE(i)
                     CASE(1)  
                        CALL FTAssertEqual(1,v % integerValue(),"First item is integer value")
                     CASE(2)
                        CALL FTAssertEqual("r2 is a string",v % stringValue(14),"Second item is string value")
                     CASE(3)
                        CALL FTAssertEqual(3.14,v % realValue(),singleTol,"Third item in list is real value")
                  END SELECT
               CLASS DEFAULT
                  CALL FTAssert(.false.,"Unknown type stored in linked list")
            END SELECT 
            CALL iterator % moveToNext()
            i = i + 1
         END DO
!
!        ---------------------------------------------------
!        Delete the object r2 from the list
!        The number of items should be decreased by one, and
!        what were r1 and r3 should still be there.
!        ---------------------------------------------------
!
         objectPtr => r2
         CALL list % remove(objectPtr)
         CALL FTAssertEqual(2,list % COUNT(),"List has two objects after removing one")
         CALL FTAssertEqual(1,r2 % refCount(),msg = "Refcount after removing object")
         
         CALL iterator % setToStart()
         i = 1
         
         DO WHILE (.NOT.iterator % isAtEnd())
            objectPtr => iterator % object()
            SELECT TYPE(v=>objectPtr)
               TYPE IS (FTValue)
                 SELECT CASE(i)
                     CASE(1)  
                        CALL FTAssertEqual(1,v % integerValue(),"First item in list doesn't have proper value")
                     CASE(3)
                        CALL FTAssertEqual(3.14,v % realValue(),singleTol,"third item in list doesn't have proper value")
                  END SELECT
               CLASS DEFAULT
                  CALL FTAssert(.false.,"Known type stored in linked list")
            END SELECT 
            CALL iterator % moveToNext()
            i = i + 1
         END DO
!
!        ------------------------------------------------------
!        By releasing an object we promise not to reference it. 
!        Otherwise, it is possible to get an undefined pointer.
!        ------------------------------------------------------
!
         CALL releaseFTLinkedList(list)
         CALL FTAssertEqual(1,list % refCount(),"Ref count decrease on release")
!
!        -------------------------------------------------------------------
!        Normally we would now check if the list should be deallocated. But 
!        since we know that it's refCount = 1, we won't and simply nullify 
!        the pointer. Note that the list itself is retained by the iterator.
!        -------------------------------------------------------------------
!
         list => NULL() 
!
!        ------------------------------------------------------------------------------
!        Clean up iterator. Its refCount, since we know we are the only owners, should
!        cause it to be destructed. On release, the iterator will deallocate the list
!        since it is the last owner.
!        ------------------------------------------------------------------------------
!
         CALL releaseFTLinkedListIterator(iterator)
         
      END SUBROUTINE basicTests
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE TestAppendingLists  
         USE FTAssertions
         USE FTValueClass
         USE FTLinkedListClass
         USE FTLinkedListIteratorClass
         IMPLICIT NONE
!         
         TYPE (FTValue)             , POINTER :: v 
         CLASS(FTObject)            , POINTER :: objectPtr
         CLASS(FTLinkedList)        , POINTER :: list1, list2
         TYPE (FTMutableObjectArray), POINTER :: array
         
         TYPE (FTLinkedListIterator), POINTER :: iterator
         INTEGER                              :: j, N
         
         ALLOCATE(iterator)
!
!        ----------------------------------------------
!        Create the two lists that will be concatenated
!        ----------------------------------------------
!
         ALLOCATE(list1,list2)
         CALL list1 % init()
         CALL list2 % init()
!
!        -----------------------------
!        Add some objects to the lists
!        -----------------------------
!
         DO j = 1, 5
            ALLOCATE(v)
            CALL v % initWithValue(j)
            objectPtr => v
            CALL list1 % add(objectPtr)
            CALL releaseFTValue(v)
         END DO
         
         DO j = 6, 10
            ALLOCATE(v)
            CALL v % initWithValue(j)
            objectPtr => v
            CALL list2 % add(objectPtr)
            CALL releaseFTValue(v)
         END DO
!
!        -----------------------------------
!        Add the elements of list2 to list 1
!        -----------------------------------
!
         CALL list1 % addObjectsFromList(list2)
         CALL FTAssertEqual(10, list1 % COUNT(),"Append list increases list size")
!
!        -------------------------------------------
!        See that the new list contains the old one.
!        Note that objects are owned by both lists.
!        -------------------------------------------
!
         CALL iterator % initWithFTLinkedList(list1)
         j = 1
         DO WHILE (.NOT.iterator % isAtEnd())
            v => valueFromObject(iterator % object())
            CALL FTAssertEqual(j, v % integerValue(),"Item value stored properly")
            IF ( j >= 6 )     THEN
               objectPtr => iterator % object()
               CALL FTAssertEqual(2, objectPtr % refCount(), "Records owned by two lists")
            END IF 
            CALL iterator % moveToNext()
            j = j + 1
         END DO
!
!        --------------------------------------------------
!        Now delete the list2. Its contents should still be
!        in list1
!        --------------------------------------------------
!
         CALL releaseFTLinkedList(list2)
         CALL FTAssertEqual(.TRUE., .NOT. ASSOCIATED(list2),"List has only one owner and should deallocate on release")
!
!        --------------------------------------------------
!        List1 should have its contents plus the other list
!        with refCount 1
!        --------------------------------------------------
!
         CALL iterator % setToStart()
         j = 1
         DO WHILE (.NOT.iterator % isAtEnd())
         
            v         => valueFromObject(iterator % object())
            CALL FTAssertEqual(j, v % integerValue(),"Item value stored properly after release of added list")
            CALL FTAssertEqual(1, v % refCount(),"Item value pointed to by list refCount")
            
            objectPtr => iterator % object()
            CALL FTAssertEqual(1, objectPtr % refCount(), "Objects owned by one list")
            CALL iterator % moveToNext()
            j = j + 1
         END DO
!
!        -------------------------------------
!        Create an object array from the list.
!        -------------------------------------
!
         array => list1 % allObjects()
         DO j = 1, array % COUNT()
            v => valueFromObject(array % objectAtIndex(j))
            CALL FTAssertEqual(j, v % integerValue(),"Item value stored in array created from list")
            objectPtr => array % objectAtIndex(j)
            CALL FTAssertEqual(2, objectPtr % refCount(), "Objects owned by one list and one array")
         END DO
         CALL releaseFTMutableObjectArray(array)
         CALL FTAssert(test = .NOT. ASSOCIATED(array),msg = "Array unreferenced")
!
!        -------------------------------------------
!        Now reverse the list and iterate through it
!        -------------------------------------------
!
         CALL list1 % reverse()
         N = list1 % COUNT()
         j = N
         CALL iterator % setToStart
         DO WHILE (.NOT.iterator % isAtEnd())
         
            v => valueFromObject(iterator % object())
            CALL FTAssertEqual(j, v % integerValue(),"Item value stored properly after release of added list")
            CALL FTAssertEqual(1, v % refCount(),"Item value pointed to by list refCount")
            
            objectPtr => iterator % object()
            CALL FTAssertEqual(1, objectPtr % refCount(), "Objects owned by one list")
            CALL iterator % moveToNext()
            j = j - 1
         END DO
!
!        --------
!        Clean up
!        --------
!
         CALL releaseFTLinkedList(list1)
         CALL releaseFTLinkedListIterator(iterator)
         
      END SUBROUTINE TestAppendingLists
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE TestDeletingObjects
         USE FTLinkedListClass
         USE FTLinkedListIteratorClass
         USE FTValueClass
         USE FTAssertions
         IMPLICIT NONE  
!
!        -----------------------------------------------
!        Add a number of objects to a list and then test
!        out removing them
!        -----------------------------------------------
!
         TYPE (FTValue)           , POINTER   :: v 
         CLASS(FTObject)          , POINTER   :: obj
         CLASS(FTLinkedList)      , POINTER   :: list
         CLASS(FTLinkedListRecord), POINTER   :: recordPtr
         
         TYPE(FTLinkedListIterator), POINTER :: iterator
         INTEGER                             :: j
!
!        ----------------------------------------------
!        Create the two lists that will be concatenated
!        ----------------------------------------------
!
         ALLOCATE(list)
         CALL list % init()
!
!        -----------------------------
!        Add some objects to the lists
!        -----------------------------
!
         DO j = 1, 6
            ALLOCATE(v)
            CALL v % initWithValue(j)
            obj => v
            CALL list % add(obj)
            CALL releaseFTValue(v)
         END DO
!
!        ------------------------------------------------------------
!        Create an iterator on the list and remove an "interior" item
!        ------------------------------------------------------------
!
         ALLOCATE(iterator)
         CALL iterator % initwithFTLinkedList(list)
!
!        ---------------
!        Delete the tail
!        ---------------
!
         CALL iterator % setToStart()
         j = 1
         DO WHILE( .NOT.iterator % isAtEnd() )
            IF ( j == 6 )     THEN
               v => valueFromObject(iterator % object())
               CALL FTAssertEqual(6,v % integerValue(),"Value of object to be deleted")
               CALL iterator % removeCurrentRecord() 
            END IF  
            CALL iterator % moveToNext()
            j = j + 1
         END DO
         
         CALL FTAssertEqual(5,list % COUNT(),"Count after deletion of tail object")
!!
!!        -----------------------
!!        Delete the third record
!!        -----------------------
!!
         CALL iterator % setToStart()
         j = 1
         DO WHILE( .NOT.iterator % isAtEnd() )
            IF ( j == 3 )     THEN
               v   => valueFromObject(iterator % object())
               CALL FTAssertEqual(3,v % integerValue(),"Value of object to be deleted")
               CALL iterator % removeCurrentRecord() 
               EXIT 
            END IF  
            CALL iterator % moveToNext()
            j = j + 1
         END DO
        
         CALL FTAssertEqual(4,list % COUNT(),"Count after deletion of middle object")
         v   => valueFromObject(iterator % object())
         CALL FTAssertEqual(4,v % integerValue(),"Value of current object after deleting")
!
!        ---------------------------------
!        Make sure connections are correct
!        ---------------------------------
!
         recordPtr => iterator % currentRecord()
         v         => valueFromObject(recordPtr % previous % recordObject)
         CALL FTAssertEqual(2,v % integerValue(), "Value of previous object after deleting")
         v         => valueFromObject(recordPtr % next % recordObject)
         CALL FTAssertEqual(5,v % integerValue(), "Value of next object after deleting")
!
!        -----------------------
!        Delete the first record
!        -----------------------
!
         CALL iterator % setToStart()
         CALL iterator % removeCurrentRecord()
         CALL FTAssertEqual(3, list % COUNT(), "count after deleting head")
         v   => valueFromObject(iterator % object())
         CALL FTAssertEqual(2,v % integerValue(),"Value of current head after deleting")
!
!        --------
!        Clean up
!        --------
!
         CALL releaseFTLinkedList(list)
         CALL releaseFTLinkedListIterator(iterator)

      END SUBROUTINE TestDeletingObjects
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE TestInsertingObjects  
         USE FTLinkedListClass
         USE FTLinkedListIteratorClass
         USE FTValueClass
         USE FTAssertions
         IMPLICIT NONE
         
         TYPE (FTValue)           , POINTER   :: v 
         CLASS(FTObject)          , POINTER   :: obj, savObj
         CLASS(FTLinkedList)      , POINTER   :: list
         INTEGER                              :: j
         TYPE(FTLinkedListIterator), POINTER  :: iterator
         
         ALLOCATE(list)
         CALL list % init()
!
!        -----------------------------
!        Add some objects to the lists
!        and save one of them
!        -----------------------------
!
         DO j = 1, 6
            ALLOCATE(v)
            CALL v % initWithValue(j)
            obj => v
            
            IF ( j == 3 )     THEN
               savObj => obj 
            END IF 

            CALL list % add(obj)
            CALL releaseFTValue(v)
         END DO
!
!        ------------------
!        Add the new object
!        ------------------
!
         ALLOCATE(v)
         CALL v % initWithValue(99)
         obj => v
         CALL list % insertObjectAfterObject(obj = obj,after = savObj)
         savObj => obj
         CALL releaseFTValue(v)
         CALL FTAssertEqual(expectedValue = 7,actualValue = list % count(),msg = "Length after adding object")
!
!        ---------------------------------
!        Make sure it's in the right place
!        ---------------------------------
!
         ALLOCATE(iterator)
         CALL iterator % initwithFTLinkedList(list)
         CALL releaseFTLinkedList(list)
         
         CALL iterator % setToStart()
         DO j = 1, 3 
            CALL iterator % moveToNext() 
         END DO 
         obj => iterator % object()
         CALL FTAssert(ASSOCIATED(obj, savObj),msg = "Location of inserted object test")
         
         CALL releaseFTLinkedListIterator(iterator)
         
      END SUBROUTINE TestInsertingObjects
