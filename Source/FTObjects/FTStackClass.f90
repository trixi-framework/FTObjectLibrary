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
!      FTStackClass.f90
!      Created: January 25, 2013 12:56 PM 
!      By: David Kopriva  
!
!>Inherits from FTLinkedListClass : FTObjectClass
!>
!>##Definition (Subclass of FTLinkedListClass):
!>   TYPE(FTStack) :: list
!>
!>#Usage:
!>
!>##Initialization
!>
!>      ALLOCATE(stack)  If stack is a pointer
!>      CALL stack  %  init()
!>
!>##Destruction
!>      CALL releaseFTStack(stack) [Pointers]
!>
!>##Pushing an object onto the stack
!>
!>      TYPE(FTObject) :: objectPtr
!>      objectPtr => r1
!>      CALL stack % push(objectPtr)
!>
!>##Peeking at the top of the stack
!>
!>      objectPtr => stack % peek()  No change of ownership
!>      SELECT TYPE(objectPtr)
!>         TYPE is (*SubclassType*)
!>            … Do something with ObjectPtr as subclass
!>         CLASS DEFAULT
!>            … Problem with casting
!>      END SELECT
!>
!>##Popping the top of the stack
!>
!>      objectPtr => stack % pop()  Ownership transferred to caller
!>      SELECT TYPE(objectPtr)
!>         TYPE is (*SubclassType*)
!>            … Do something with ObjectPtr as subclass
!>         CLASS DEFAULT
!>            … Problem with casting
!>      END SELECT
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
         PROCEDURE :: className        => stackClassName
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
         !None to initialize
         
      END SUBROUTINE initFTStack
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE releaseFTStack(self)  
         IMPLICIT NONE
         TYPE(FTStack)  , POINTER :: self
         CLASS(FTObject), POINTER :: obj
            
         IF(.NOT. ASSOCIATED(self)) RETURN
       
         obj => self
         CALL release(obj) 
         IF(.NOT.ASSOCIATED(obj)) self => NULL()
      END SUBROUTINE releaseFTStack
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
         CLASS(FTObject)          , POINTER :: p, obj
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
         
         obj => tmp
         CALL release(obj)
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
!
!//////////////////////////////////////////////////////////////////////// 
! 
!      -----------------------------------------------------------------
!> Class name returns a string with the name of the type of the object
!>
!>  ### Usage:
!>
!>        PRINT *,  obj % className()
!>        if( obj % className = "FTStack")
!>
      FUNCTION stackClassName(self)  RESULT(s)
         IMPLICIT NONE  
         CLASS(FTStack)                             :: self
         CHARACTER(LEN=CLASS_NAME_CHARACTER_LENGTH) :: s
         
         IF(self % refCount() .ge. 0) CONTINUE
         s = "FTStack"
 
      END FUNCTION stackClassName
    
      END Module FTStackClass    
