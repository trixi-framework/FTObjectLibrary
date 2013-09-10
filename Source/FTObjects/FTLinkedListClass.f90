!
!////////////////////////////////////////////////////////////////////////
!
!      FTLinkedListClass.f90
!      Created: January 7, 2013 2:56 PM 
!      By: David Kopriva  
!
!
!
!////////////////////////////////////////////////////////////////////////
!
!@mark -
!
!>      FTLinkedListRecord is the record type (object and next) for the
!>      LinkedList class.
!>      
!>      One will generally not instantiate a record oneself. They are 
!>      created automatically when one adds an object to a linked list.
!>
      Module FTLinkedListRecordClass 
      USE FTObjectClass
      IMPLICIT NONE 
!
!     -----------------------------
!     Record class for linked lists
!     -----------------------------
!
      TYPE, EXTENDS(FTObject) :: FTLinkedListRecord
      
         CLASS(FTObject)          , POINTER :: recordObject
         CLASS(FTLinkedListRecord), POINTER :: next, previous
!
!        ========         
         CONTAINS
!        ========
!
         PROCEDURE :: initWithObject
         PROCEDURE :: destruct => destructFTLinkedListRecord
         
      END TYPE FTLinkedListRecord
!
!     ----------
!     Procedures
!     ----------
!
      CONTAINS 
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE initWithObject(self,obj) 
         IMPLICIT NONE 
         CLASS(FTLinkedListRecord) :: self
         CLASS(FTObject), POINTER  :: obj
!
!        -------------------------------
!        Always call the superclass init
!        -------------------------------
!
         CALL self % FTObject % init
!
!        ------------------------
!        Subclass initializations
!        ------------------------
!
         CALL obj % retain()
         
         self % recordObject => obj
         self % next         => NULL()
         self % previous     => NULL()
         
      END SUBROUTINE initWithObject
!
!////////////////////////////////////////////////////////////////////////
!
!< The destructor must only be called from within subclass destructors
!
      SUBROUTINE destructFTLinkedListRecord(self) 
         IMPLICIT NONE
         CLASS(FTLinkedListRecord) :: self
         
         IF ( ASSOCIATED(self % recordObject) )     THEN
            CALL self % recordObject % release()
            IF ( self % recordObject % isUnreferenced() )     THEN
               DEALLOCATE(self % recordObject)
               self % recordObject => NULL()
            END IF
         END IF 
         self % next => NULL()
         self % previous => NULL()
!
!        ------------------------------------------
!        Always call the superclass destructor here
!        at the end of the subclass destructor.
!        ------------------------------------------
!
         CALL self % FTObject % destruct()
        
      END SUBROUTINE destructFTLinkedListRecord     
      
      END MODULE FTLinkedListRecordClass  
!@mark -
!
!
!     --------------------------------------------------
!     Implements the basics of a linked list of objects
!     --------------------------------------------------
!
!>
!> FTLinkedList is a container class that stores objects in a linked list.
!> 
!>
!      FTLinkedList:
!
!>      Inherits from FTObjectClass
!>
!>      *Definition (Subclass of FTObject):*
!>
!>               TYPE(FTLinkedList) :: list
!>
!>      Usage:
!>
!>         *Initialization*
!>
!>               CLASS(FTLinkedList), POINTER :: list
!>               ALLOCATE(list)
!>               CALL list % init
!>
!>         *Adding objects*
!>
!>               CLASS(FTLinkedList), POINTER :: list, listToAdd
!>               CLASS(FTObject)    , POINTER :: objectPtr
!>
!>               objectPtr => r                ! r is subclass of FTObject
!>               CALL list % Add(objectPtr)    ! Pointer is retained by list
!>               CALL objectPtr % release()    ! If caller reliquishes ownership
!>
!>               CALL list % addObjectsFromList(listToAdd)
!>
!>         *Inserting objects*
!>
!>               CLASS(FTLinkedList)      , POINTER :: list
!>               CLASS(FTObject)          , POINTER :: objectPtr
!>               CLASS(FTLinkedListRecord), POINTER :: record
!>
!>               objectPtr => r                            ! r is subclass of FTObject
!>               CALL list % insertAfter(objectPtr,record) ! Pointer is retained by list
!>               CALL objectPtr % release()                ! If caller reliquishes ownership
!>
!>         *Removing objects*
!>
!>               CLASS(FTLinkedList), POINTER :: list
!>               CLASS(FTObject)    , POINTER :: objectPtr
!>               objectPtr => r                 ! r is subclass of FTObject
!>               CALL list % remove(objectPtr)
!>
!>         *Counting the number of objects in the list*
!>
!>               n = list % count()
!>
!>         *Destruction*
!>         
!>               CALL list % release()
!>               IF ( list % isUnreferenced() )     THEN ! If list is a pointer
!>                  DEALLOCATE(list)
!>                  list => NULL()
!>               END IF
!>
      Module FTLinkedListClass
!      
      USE FTLinkedListRecordClass
      IMPLICIT NONE 
!
!     -----------------
!     Class object type
!     -----------------
!
      TYPE, EXTENDS(FTObject) :: FTLinkedList
      
         CLASS(FTLinkedListRecord), POINTER :: head, tail
         INTEGER                            :: nRecords
         LOGICAL                            :: isCircular_
!
!        ========         
         CONTAINS
!        ========
!
         PROCEDURE :: init             => initFTLinkedList
         PROCEDURE :: add              => addObject
         PROCEDURE :: insert           => insertObjectAfter
         PROCEDURE :: remove           => removeObject
         PROCEDURE :: reverse          => reverseLinkedList
         PROCEDURE :: removeRecord     => removeLinkedListRecord
         PROCEDURE :: destruct         => destructFTLinkedList
         PROCEDURE :: count            => numberOfRecords
         PROCEDURE :: description      => FTLinkedListDescription
         PROCEDURE :: printDescription => printFTLinkedListDescription
         PROCEDURE :: addObjectsFromList
         PROCEDURE :: makeCircular
         PROCEDURE :: isCircular
         
      END TYPE FTLinkedList
      
      INTERFACE cast
         MODULE PROCEDURE castObjectToLinkedList
      END INTERFACE cast
      
!
!     ----------
!     Procedures
!     ----------
!
      CONTAINS 
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE initFTLinkedList(self) 
         IMPLICIT NONE 
         CLASS(FTLinkedList) :: self
!
!        -------------------------------
!        Always call the superclass init
!        -------------------------------
!
         CALL self % FTObject % init()
!
!        --------------------------------------
!        Then call the subclass initializations
!        --------------------------------------
!
         self % nRecords    = 0
         self % isCircular_ = .FALSE.
         
         self % head => NULL(); self % tail => NULL()
         
      END SUBROUTINE initFTLinkedList
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE addObject(self,obj)
         IMPLICIT NONE 
         CLASS(FTLinkedList)                :: self
         CLASS(FTObject)          , POINTER :: obj
         CLASS(FTLinkedListRecord), POINTER :: newRecord
         
         ALLOCATE(newRecord)
         CALL newRecord % initWithObject(obj)
         
         IF ( .NOT.ASSOCIATED(self % head) )     THEN
            self % head => newRecord
         ELSE
            self % tail % next   => newRecord
            newRecord % previous => self % tail
         END IF
         
         self % tail => newRecord
         self % nRecords = self % nRecords + 1
         
      END SUBROUTINE addObject 
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE addObjectsFromList(self,list)
         IMPLICIT NONE 
         CLASS(FTLinkedList)                :: self
         CLASS(FTLinkedList)      , POINTER :: list
         CLASS(FTLinkedListRecord), POINTER :: recordPtr
         
         IF ( .NOT.ASSOCIATED( self % head ) )     THEN
            self % head => list % head
            self % tail => list % head
         ELSE
            self % tail % next     => list % head
            list % head % previous => self % tail
            self % tail            => list % tail
         END IF

         recordPtr => list % head

         DO WHILE(ASSOCIATED( recordPtr ))
            CALL recordPtr % retain()
            self % nRecords = self % nRecords + 1
            recordPtr => recordPtr % next
         END DO
         
      END SUBROUTINE addObjectsFromList 
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE insertObjectAfter(self,obj,after)
         IMPLICIT NONE 
         CLASS(FTLinkedList)                :: self
         CLASS(FTObject)          , POINTER :: obj
         CLASS(FTLinkedListRecord), POINTER :: newRecord
         CLASS(FTLinkedListRecord), POINTER :: after
         
         ALLOCATE(newRecord)
         CALL newRecord % initWithObject(obj)
         
         newRecord % next     => after % next
         newRecord % previous => after
         after % next     => newRecord
         
         IF ( .NOT.ASSOCIATED( newRecord % next ) )     THEN
            self % tail => newRecord 
         END IF 
         
         self % nRecords = self % nRecords + 1
         
      END SUBROUTINE insertObjectAfter 
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE makeCircular(self,circular)  
         IMPLICIT NONE  
         CLASS(FTLinkedList) :: self
         LOGICAL             :: circular
         
         IF ( circular )     THEN
            self % head % previous => self % tail
            self % tail % next     => self % head
            self % isCircular_ = .TRUE.
         ELSE
            self % head % previous => NULL()
            self % tail % next     => NULL()
            self % isCircular_ = .FALSE.
         END IF 
      END SUBROUTINE makeCircular
!
!//////////////////////////////////////////////////////////////////////// 
! 
      LOGICAL FUNCTION isCircular(self)  
         IMPLICIT NONE  
         CLASS(FTLinkedList) :: self
         isCircular = self % isCircular_
      END FUNCTION isCircular
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE removeObject1(self,obj)
         IMPLICIT NONE 
         CLASS(FTLinkedList)                :: self
         CLASS(FTObject)          , POINTER :: obj
         
         CLASS(FTLinkedListRecord), POINTER :: current, previous
                  
         IF ( .NOT.ASSOCIATED(self % head) )     RETURN
         
         current  => self % head
         previous => NULL()
!
!        -------------------------------------------------------------
!        Find the object in the list by a linear search and remove it.
!        It will be deallocated if necessary.
!        -------------------------------------------------------------
!
         DO WHILE (ASSOCIATED(current))
            IF ( ASSOCIATED(current % recordObject,obj) )     THEN
            
               IF ( ASSOCIATED(previous) )     THEN
                  previous % next => current % next
               ELSE
                  self % head => current % next
               END IF 
               
               IF ( ASSOCIATED(current,self % tail) )     THEN
                  self % tail => previous 
               END IF 
               
               CALL current % release()
               IF ( current%isUnreferenced() )     THEN
                  DEALLOCATE(current)
                  current => NULL()
               END IF 
               
               self % nRecords = self % nRecords - 1
               EXIT
            END IF 
            
            previous => current
            current  => current % next
         END DO
         
      END SUBROUTINE removeObject1 
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE removeObject(self,obj)
         IMPLICIT NONE 
         CLASS(FTLinkedList)                :: self
         CLASS(FTObject)          , POINTER :: obj
         
         CLASS(FTLinkedListRecord), POINTER :: current, previous
                  
         IF ( .NOT.ASSOCIATED(self % head) )     RETURN
         
         current  => self % head
         previous => NULL()
!
!        -------------------------------------------------------------
!        Find the object in the list by a linear search and remove it.
!        It will be deallocated if necessary.
!        -------------------------------------------------------------
!
         DO WHILE (ASSOCIATED(current))
         
            IF ( ASSOCIATED(current % recordObject,obj) )     THEN
               CALL self % removeRecord(current)
               EXIT
            END IF 
            
            previous => current
            current  => current % next
         END DO
         
      END SUBROUTINE removeObject 
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE removeLinkedListRecord(self,listRecord)
         IMPLICIT NONE 
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(FTLinkedList)                :: self
         CLASS(FTLinkedListRecord), POINTER :: listRecord
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(FTLinkedListRecord), POINTER :: previous, next
                  
         previous => listRecord % previous
         next     => listRecord % next
         
         IF ( ASSOCIATED(listRecord, self % head) )     THEN
            self % head => next
            IF ( ASSOCIATED(next) )     THEN
               self % head % previous => NULL() 
            END IF  
         END IF 
         
         IF ( ASSOCIATED(listRecord, self % tail) )     THEN
            self % tail => previous
            IF ( ASSOCIATED(previous) )     THEN
               self % tail % next => NULL() 
            END IF  
         END IF 
         
         IF ( ASSOCIATED(previous) .AND. ASSOCIATED(next) )     THEN
            previous % next => next
            next % previous => previous 
         END IF 
               
         CALL listRecord % release()
         IF ( listRecord % isUnreferenced() )     THEN
            DEALLOCATE(listRecord)
            listRecord => NULL()
         END IF 
         
         self % nRecords = self % nRecords - 1
         
      END SUBROUTINE removeLinkedListRecord 
!
!//////////////////////////////////////////////////////////////////////// 
! 
      INTEGER FUNCTION numberOfRecords(self)  
         IMPLICIT NONE  
         CLASS(FTLinkedList) :: self
          numberOfRecords = self % nRecords
      END FUNCTION numberOfRecords     
!
!////////////////////////////////////////////////////////////////////////
!
!< The destructor must only be called from within the destructors of subclasses
!> It is automatically called by release().
!
      SUBROUTINE destructFTLinkedList(self) 
         IMPLICIT NONE
         CLASS(FTLinkedList)                :: self
         CLASS(FTLinkedListRecord), POINTER :: listRecord, tmp

         IF(.NOT.ASSOCIATED(self % head)) THEN
            CALL self % FTObject % destruct()
            RETURN
         END IF

         listRecord => self % head
         DO WHILE (ASSOCIATED(listRecord))
            tmp => listRecord % next

            CALL listRecord % release()

            IF(listRecord % isUnreferenced()) THEN
               DEALLOCATE(listRecord)
               listRecord => NULL()
            END IF
            self % nRecords = self % nRecords - 1
            listRecord => tmp
         END DO

         self % head => NULL(); self % tail => NULL()
!
!        ------------------------------------------
!        Always call the superclass destructor here
!        at the end of the subclass destructor.
!        ------------------------------------------
!
         CALL self % FTObject % destruct()

      END SUBROUTINE destructFTLinkedList
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION FTLinkedListDescription(self)  
         IMPLICIT NONE  
         CLASS(FTLinkedList)                         :: self
         CLASS(FTLinkedListRecord), POINTER          :: listRecord
         CHARACTER(LEN=DESCRIPTION_CHARACTER_LENGTH) :: FTLinkedListDescription
         
         
         FTLinkedListDescription = ""
         IF(.NOT.ASSOCIATED(self % head)) RETURN
         
         listRecord              => self % head
         FTLinkedListDescription = TRIM(listRecord % recordObject % description())
         listRecord              => listRecord % next

         DO WHILE (ASSOCIATED(listRecord))
            FTLinkedListDescription = TRIM(FTLinkedListDescription) // &
                                       CHAR(13) // &
                                       TRIM(listRecord % recordObject % description())
            listRecord => listRecord % next
         END DO
      END FUNCTION FTLinkedListDescription    
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE printFTLinkedListDescription(self,iUnit)  
         IMPLICIT NONE  
         CLASS(FTLinkedList)                         :: self
         INTEGER                                     :: iUnit
         CLASS(FTLinkedListRecord), POINTER          :: listRecord
         
         
         IF(.NOT.ASSOCIATED(self % head)) RETURN
         
         listRecord              => self % head

         DO WHILE (ASSOCIATED(listRecord))
            CALL listRecord % recordObject % printDescription(iUnit)
            listRecord => listRecord % next
            IF ( self % isCircular_ )     THEN
               IF ( ASSOCIATED(listRecord,self % head) )     THEN
                  EXIT 
               END IF  
            END IF 
         END DO
         
      END SUBROUTINE printFTLinkedListDescription
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE reverseLinkedList(self)
!
!     ----------------------------------------------
!     Returns a retained copy of the list that runs 
!     in the opposite order
!     ----------------------------------------------
!
         IMPLICIT NONE 
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(FTLinkedList) :: self
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(FTLinkedListRecord), POINTER :: current, tmp, next => NULL()
         
         IF(.NOT.ASSOCIATED(self % head)) RETURN
         
         IF ( self % isCircular_ )     THEN
            self % head % previous => NULL()
            self % tail % next     => NULL() 
         END IF
         
         current  => self % head

         DO WHILE (ASSOCIATED(current))
            tmp                => current % next
            current % next     => current % previous
            current % previous => tmp
            current            => tmp
         END DO
         
         tmp => self % head
         self % head => self % tail
         self % tail => tmp
         
         IF ( self % isCircular_ )     THEN
            CALL self % makeCircular(.TRUE.) 
         END IF 
         
      END SUBROUTINE reverseLinkedList
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE castObjectToLinkedList(obj,cast) 
!
!     -----------------------------------------------------
!     Cast the base class FTObject to the LinkedList class
!     -----------------------------------------------------
!
         IMPLICIT NONE  
         CLASS(FTObject)    , POINTER :: obj
         CLASS(FTLinkedList), POINTER :: cast
         
         cast => NULL()
         SELECT TYPE (e => obj)
            TYPE is (FTLinkedList)
               cast => e
            CLASS DEFAULT
               
         END SELECT
         
      END SUBROUTINE castObjectToLinkedList
!
      END MODULE FTLinkedListClass
!
!@mark -
!
!//////////////////////////////////////////////////////////////////////// 
! 
!>      An object for stepping through a linked list.
!>
!>      Definition (Subclass of FTObject):
!>         TYPE(FTLinkedListIterator) :: list
!>
!>      Usage:
!>
!>         *Initialization*
!>
!>               CLASS(FTLinkedList)        , POINTER :: list
!>               CLASS(FTLinkedListIterator), POINTER :: iterator
!>               ALLOCATE(iterator)
!>               CALL iterator % initWithFTLinkedList(list)
!>
!>         *Accessors*
!>
!>               ptr => iterator % list()
!>               ptr => iterator % object()
!>               ptr => iterator % currentRecord()
!>
!>         *Iterating*
!>
!>               CLASS(FTObject), POINTER :: objectPtr
!>               CALL iterator % setToStart
!>               DO WHILE (.NOT.iterator % isAtEnd())
!>                  objectPtr => iterator % object()        ! if the object is wanted
!>                  recordPtr => iterator % currentRecord() ! if the record is wanted
!>                  
!>                   Do something with object or record
!>
!>                  CALL iterator % moveToNext()
!>               END DO
!>
!>         *Destruction*
!>         
!>               CALL iterator % release()
!>               IF ( iterator % isUnreferenced() )     THEN ! if it is a pointer
!>                  DEALLOCATE(iterator) 
!>                  iterator => null()
!>               END IF
!
!//////////////////////////////////////////////////////////////////////// 
! 
      Module FTLinkedListIteratorClass
      USE FTLinkedListClass
      IMPLICIT NONE
!
!     -----------------
!     Class object type
!     -----------------
!
      TYPE, EXTENDS(FTObject) :: FTLinkedListIterator
         CLASS(FTLinkedList)      , POINTER :: list
         CLASS(FTLinkedListRecord), POINTER :: current
!
!        ========         
         CONTAINS
!        ========
!
         PROCEDURE :: init => initEmpty
         PROCEDURE :: initWithFTLinkedList
         PROCEDURE :: destruct => destructIterator
         PROCEDURE :: isAtEnd => FTLinkedListIsAtEnd
         PROCEDURE :: object  => FTLinkedListObject
         PROCEDURE :: currentRecord  => FTLinkedListCurrentRecord
         PROCEDURE :: linkedList => returnLinkedList
         PROCEDURE :: setLinkedList
         PROCEDURE :: setToStart
         PROCEDURE :: moveToNext
         PROCEDURE :: removeCurrentRecord
      END TYPE FTLinkedListIterator
!
!     ----------
!     Procedures
!     ----------
!
      CONTAINS 
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE initEmpty(self) 
         IMPLICIT NONE 
         CLASS(FTLinkedListIterator)  :: self
         CLASS(FTLinkedList), POINTER :: list
!
!        --------------------------------------------
!        Always call the superclass initializer first
!        --------------------------------------------
!
         CALL self % FTObject % init()
!
!        ----------------------------------------------
!        Then call the initializations for the subclass
!        ----------------------------------------------
!
         self % list    => NULL()
         self % current => NULL()
         
      END SUBROUTINE initEmpty   
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE initWithFTLinkedList(self,list) 
         IMPLICIT NONE 
         CLASS(FTLinkedListIterator)  :: self
         CLASS(FTLinkedList), POINTER :: list
!
!        --------------------------------------------
!        Always call the superclass initializer first
!        --------------------------------------------
!
         CALL self % FTObject % init()
!
!        ----------------------------------------------
!        Then call the initializations for the subclass
!        ----------------------------------------------
!
         self % list    => NULL()
         self % current => NULL()
         CALL self % setLinkedList(list)
         
      END SUBROUTINE initWithFTLinkedList   
!
!////////////////////////////////////////////////////////////////////////
!
!< The destuctor must not be called except at the end of destructors of
! subclasses.
!
      SUBROUTINE destructIterator(self)
          IMPLICIT NONE 
          CLASS(FTLinkedListIterator) :: self
          
          IF ( ASSOCIATED(self % list) )     THEN
             CALL self % list % release()
             IF(self % list % isUnreferenced()) THEN
                DEALLOCATE(self % list)
                self % list => NULL()
             END IF
          END IF 
          
          self % current => NULL()
!
!        ------------------------------------------
!        Always call the superclass destructor here
!        at the end of the subclass destructor.
!        ------------------------------------------
!
          CALL self % FTObject % destruct
          
!          PRINT *, "Linked list iterator destructed"
      END SUBROUTINE destructIterator
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE setToStart(self) 
         IMPLICIT NONE 
         CLASS(FTLinkedListIterator)  :: self
         self % current => self % list % head
      END SUBROUTINE setToStart 
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE moveToNext(self) 
         IMPLICIT NONE 
         CLASS(FTLinkedListIterator)  :: self
         
         IF ( ASSOCIATED(self % current) )     THEN
            self % current => self % current % next
         ELSE 
            self % current => NULL() 
         END IF 
         !^ 
         
         IF ( ASSOCIATED(self % current, self % list % head) )     THEN
            self % current => NULL() 
         END IF 
         
      END SUBROUTINE moveToNext 
!
!////////////////////////////////////////////////////////////////////////
!
      LOGICAL FUNCTION FTLinkedListIsAtEnd(self)
         IMPLICIT NONE 
         CLASS(FTLinkedListIterator)  :: self
         IF ( ASSOCIATED(self % current) )     THEN
            FTLinkedListIsAtEnd = .false.
         ELSE
            FTLinkedListIsAtEnd = .true.
         END IF
      END FUNCTION FTLinkedListIsAtEnd   
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE setLinkedList(self,list)
         IMPLICIT NONE 
         CLASS(FTLinkedListIterator)  :: self
         CLASS(FTLinkedList), POINTER :: list
!
!        -----------------------------------
!        Remove current list if there is one
!        -----------------------------------
!
         IF ( ASSOCIATED(list) )     THEN
         
            IF ( ASSOCIATED(self % list, list) )     THEN
               CALL self % setToStart()
            ELSE IF( ASSOCIATED(self % list) )     THEN
               CALL self % list % release()
               IF ( self % list % isUnreferenced() )     THEN
                  DEALLOCATE(self % list) 
               END IF 
               self % list => list
               CALL self % list % retain()
               CALL self % setToStart
            ELSE
               self % list => list
               CALL self % list % retain()
               CALL self % setToStart
            END IF 
            
         ELSE
         
            IF( ASSOCIATED(self % list) )     THEN
               CALL self % list % release()
               IF ( self % list % isUnreferenced() )     THEN
                  DEALLOCATE(self % list) 
               END IF 
            END IF 
            self % list => NULL()
            
         END IF
         
      END SUBROUTINE setLinkedList   
!
!////////////////////////////////////////////////////////////////////////
!
      FUNCTION returnLinkedList(self) RESULT(o)
         IMPLICIT NONE 
         CLASS(FTLinkedListIterator)  :: self
         CLASS(FTLinkedList), POINTER     :: o
         o => self % list
      END FUNCTION returnLinkedList 
!
!////////////////////////////////////////////////////////////////////////
!
      FUNCTION FTLinkedListObject(self) RESULT(o)
         IMPLICIT NONE 
         CLASS(FTLinkedListIterator)  :: self
         CLASS(FTObject), POINTER     :: o
         o => self % current % recordObject
      END FUNCTION FTLinkedListObject 
!
!////////////////////////////////////////////////////////////////////////
!
      FUNCTION FTLinkedListCurrentRecord(self) RESULT(o)
         IMPLICIT NONE 
         CLASS(FTLinkedListIterator)        :: self
         CLASS(FTLinkedListRecord), POINTER :: o
         o => self % current
      END FUNCTION FTLinkedListCurrentRecord 
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE removeCurrentRecord(self)  
         IMPLICIT NONE  
         CLASS(FTLinkedListIterator)        :: self
         CLASS(FTLinkedListRecord), POINTER :: r, n
         r => self % current
         n => self % current % next
         
         CALL self % list % removeRecord(r)
         self % current => n
         
      END SUBROUTINE removeCurrentRecord
      
      END MODULE FTLinkedListIteratorClass   