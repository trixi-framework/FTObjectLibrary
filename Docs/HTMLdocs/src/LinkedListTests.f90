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
!
!        ------------
!        Basic tests 
!        ------------
!
         CALL basicTests
!
!        --------------------------------------
!        Do more with deleting objects in lists
!        --------------------------------------
!
         CALL TestDeletingObjects
!
!        ---------------------------------
!        Now test appending lists to lists
!        ---------------------------------
!
         CALL testAppendingLists
         
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
         CLASS(FTValue), POINTER :: r1, r2, r3 
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
         CLASS(FTLinkedList)        , POINTER :: list
         CLASS(FTLinkedListIterator), POINTER :: iterator
         
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
         CALL release(list)
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
         CALL release(r1)
            
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
         CALL release(r3)
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
         CALL release(list)
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
         CALL release(iterator)
!
!        --------------------------------------------------------------------------
!        At this point, the iterator should not have a linked list associated with 
!        it. Check to make sure.
!        --------------------------------------------------------------------------
!
!         list => iterator % linkedList()
!         test = ASSOCIATED(list)
!         CALL FTAssert(.NOT.test,"List pointer nullified")
         
      END SUBROUTINE basicTests
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE testAppendingLists  
         USE FTAssertions
         USE FTValueClass
         USE FTLinkedListClass
         USE FTLinkedListIteratorClass
         IMPLICIT NONE
!         
         CLASS(FTValue)             , POINTER :: v 
         CLASS(FTObject)            , POINTER :: objectPtr
         CLASS(FTLinkedList)        , POINTER :: list1, list2
         CLASS(FTMutableObjectArray), POINTER :: array
         
         CLASS(FTLinkedListIterator), POINTER :: iterator
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
            CALL release(v)
         END DO
         
         DO j = 6, 10
            ALLOCATE(v)
            CALL v % initWithValue(j)
            objectPtr => v
            CALL list2 % add(objectPtr)
            CALL release(v)
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
         CALL release(list2)
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
            CALL FTAssertEqual(j, v % integerValue(),"Item value stored properly afer release of added list")
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
         CALL release(array)
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
            CALL FTAssertEqual(j, v % integerValue(),"Item value stored properly afer release of added list")
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
         CALL release(list1)
         CALL release(iterator)

         
      END SUBROUTINE testAppendingLists
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
         CLASS(FTValue)           , POINTER   :: v 
         CLASS(FTObject)          , POINTER   :: obj
         CLASS(FTLinkedList)      , POINTER   :: list
         CLASS(FTLinkedListRecord), POINTER   :: recordPtr
         
         CLASS(FTLinkedListIterator), POINTER :: iterator
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
            CALL release(v)
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
         CALL release(iterator)
         CALL release(list)

      END SUBROUTINE TestDeletingObjects