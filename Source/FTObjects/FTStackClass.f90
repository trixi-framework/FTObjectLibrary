!
!////////////////////////////////////////////////////////////////////////
!
!      FTStackClass.f90
!      Created: January 25, 2013 12:56 PM 
!      By: David Kopriva  
!
!      Inherits from FTLinkedListClass : FTObjectClass
!
!      Definition (Subclass of FTLinkedListClass):
!         TYPE(FTStack) :: list
!
!      *Usage:
!
!         *Initialization*
!
!            ALLOCATE(stack) ! If stack is a pointer
!            CALL stack  %  init
!
!         *Destruction*
!            CALL stack  %  release
!            IF ( stack  %  isUnreferenced() )     THEN
!               DEALLOCATE(stack) ! If stack is a pointer
!               stack => NULL()
!            END IF 
!
!         *Pushing an object onto the stack*
!
!            TYPE(FTObject) :: objectPtr
!            objectPtr => r1
!            CALL stack % push(objectPtr)
!
!         *Peeking at the top of the stack*
!
!            objectPtr => stack % peek() ! No change of ownership
!            SELECT TYPE(objectPtr)
!               TYPE is (*SubclassType*)
!                  … Do something with ObjectPtr as subclass
!               CLASS DEFAULT
!                  … Problem with casting
!            END SELECT
!
!         *Popping the top of the stack*
!
!            objectPtr => stack % pop() ! Ownership transferred to caller
!            SELECT TYPE(objectPtr)
!               TYPE is (*SubclassType*)
!                  … Do something with ObjectPtr as subclass
!               CLASS DEFAULT
!                  … Problem with casting
!            END SELECT
!
!////////////////////////////////////////////////////////////////////////
!
      Module FTStackClass
      USE FTLinkedListClass
      IMPLICIT NONE
      
      TYPE, EXTENDS(FTLinkedList) :: FTStack
!
!        ========         
         CONTAINS
!        ========
!
         PROCEDURE :: init             => initFTStack
         PROCEDURE :: printDescription => printStackDescription
         PROCEDURE :: push
         PROCEDURE :: pop
         PROCEDURE :: peek
      END TYPE FTStack
!
!     ----------
!     Procedures
!     ----------
!
!     ========
      CONTAINS
!     ========
!
!
!------------------------------------------------
!> Public, generic name: init()
!>
!> Initialize the stack.
!------------------------------------------------
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE initFTStack(self) 
         IMPLICIT NONE 
         CLASS(FTStack) :: self
!
!        --------------------------------------------
!        Call the initializer of the superclass first
!        --------------------------------------------
!
         CALL self % FTLinkedList % init()
!
!        ---------------------------------
!        Then initialize ivars of subclass 
!        ---------------------------------
!
         !None to intialize
         
      END SUBROUTINE initFTStack
!
!     -----------------------------------
!     push: Push an object onto the stack
!     -----------------------------------
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE push(self,obj)
!
!        ----------------------------------
!        Add object to the head of the list
!        ----------------------------------
!
         IMPLICIT NONE 
         CLASS(FTStack)                     :: self
         CLASS(FTObject)          , POINTER :: obj
         CLASS(FTLinkedListRecord), POINTER :: newRecord => NULL()
         CLASS(FTLinkedListRecord), POINTER :: tmp       => NULL()
         
         ALLOCATE(newRecord)
         CALL newRecord % initWithObject(obj)
         
         IF ( .NOT.ASSOCIATED(self % head) )     THEN
            self % head => newRecord
            self % tail => newRecord
         ELSE
            tmp                => self % head
            self % head        => newRecord
            self % head % next => tmp
            tmp  % previous    => newRecord
         END IF
         self % nRecords = self % nRecords + 1
         
      END SUBROUTINE push
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION peek(self)
!
!        -----------------------------------------
!        Return the object at the head of the list
!        ** No change of ownership **
!        -----------------------------------------
!
         IMPLICIT NONE 
         CLASS(FTStack)           :: self
         CLASS(FTObject), POINTER :: peek
         
         IF ( .NOT. ASSOCIATED(self % head) )     THEN
            peek => NULL()
            RETURN 
         END IF 

         peek => self % head % recordObject

      END FUNCTION peek    
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE pop(self,p)
!
!        ---------------------------------------------------
!        Remove the head of the list and return the object
!        that it points to. Calling routine gains ownership 
!        of the object.
!        ---------------------------------------------------
!
         IMPLICIT NONE  
         CLASS(FTStack)                     :: self
         CLASS(FTObject)          , POINTER :: p
         CLASS(FTLinkedListRecord), POINTER :: tmp => NULL()
         
         IF ( .NOT. ASSOCIATED(self % head) )     THEN
            p => NULL()
            RETURN 
         END IF 
            
         p => self % head % recordObject
         IF(.NOT.ASSOCIATED(p)) RETURN 
         CALL p % retain()
         
         tmp => self % head
         self % head => self % head % next
         
         CALL tmp % release()
         IF( tmp % isUnreferenced())     THEN
            DEALLOCATE(tmp)
            tmp => NULL()
         END IF
         self % nRecords = self % nRecords - 1

      END SUBROUTINE pop
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION stackFromObject(obj) RESULT(cast)
!
!     -----------------------------------------------------
!     Cast the base class FTObject to the LinkedList class
!     -----------------------------------------------------
!
         IMPLICIT NONE  
         CLASS(FTObject), POINTER :: obj
         CLASS(FTStack) , POINTER :: cast
         
         cast => NULL()
         SELECT TYPE (e => obj)
            TYPE is (FTStack)
               cast => e
            CLASS DEFAULT
               
         END SELECT
         
      END FUNCTION stackFromObject
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE printStackDescription(self, iUnit) 
         IMPLICIT NONE 
         CLASS(FTStack) :: self
         INTEGER        :: iUnit
         
         CALL self % FTLinkedList % printDescription(iUnit = iUnit)
         
      END SUBROUTINE printStackDescription
    
      END Module FTStackClass    