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
!      FTDataClass.f90
!      Created: July 11, 2013 2:00 PM 
!      By: David Kopriva  
!
!>FTData defines a subclass of FTObject to contain immutable
!>generic data, including derived types. 
!>
!>The initializer
!>copies the data and takes ownership of that copy. FTData
!>gives a way to use derived types without having to subclass
!>FTObject.
!
!////////////////////////////////////////////////////////////////////////
!
      Module FTDataClass 
      USE FTObjectClass
      IMPLICIT NONE
!
!     ---------
!     Constants
!     ---------
!
      INTEGER, PARAMETER          :: DATA_CLASS_TYPE_LENGTH = 32
!
!     ---------------------
!     Class type definition
!     ---------------------
!
      TYPE, EXTENDS(FTObject) :: FTData
         PRIVATE 
         CHARACTER(LEN=DATA_CLASS_TYPE_LENGTH) :: dataType
         CHARACTER(LEN=1), POINTER             :: dataStorage(:) 
         INTEGER                               :: dataSize
!
!        ========         
         CONTAINS 
!        ========
!         
         PROCEDURE, PUBLIC :: initWithDataOfType
         PROCEDURE, PUBLIC :: storedData
         PROCEDURE, PUBLIC :: storedDataSize
         PROCEDURE, PUBLIC :: className => dataClassName
         FINAL             :: destructData
      END TYPE FTData
      
      CONTAINS 
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE initWithDataOfType(self, genericData, dataType)  
         IMPLICIT NONE  
         CLASS(FTData)    :: self
         CHARACTER(LEN=*) :: dataType
         CHARACTER(LEN=1) :: genericData(:)
         
         INTEGER          :: dataSize
          
          CALL self % FTObject % init()
          
          dataSize = SIZE(genericData)
          ALLOCATE(self % dataStorage(dataSize))
          
          self % dataStorage = genericData
          self % dataType    = dataType
          self % dataSize    = dataSize
          
      END SUBROUTINE initWithDataOfType
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE destructData(self) 
         IMPLICIT NONE
         TYPE(FTData)  :: self
         
         DEALLOCATE( self % dataStorage)
         
      END SUBROUTINE destructData
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE releaseFTData(self)  
         IMPLICIT NONE
         TYPE(FTData)  , POINTER :: self
         CLASS(FTObject), POINTER :: obj
         
         IF(.NOT. ASSOCIATED(self)) RETURN
        
         obj => self
         CALL release(obj) 
         IF(.NOT.ASSOCIATED(obj)) self => NULL()
      END SUBROUTINE releaseFTData
!@mark -
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION storedData(self)  RESULT(d)
         IMPLICIT NONE  
         CLASS(FTData)             :: self
         CHARACTER(LEN=1), POINTER :: d(:)
         d => self % dataStorage
      END FUNCTION storedData
!
!//////////////////////////////////////////////////////////////////////// 
! 
      INTEGER FUNCTION storedDataSize(self)
         IMPLICIT NONE  
         CLASS(FTData)    :: self
         storedDataSize = self % dataSize
      END FUNCTION storedDataSize
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION dataType(self)  RESULT(t)
         IMPLICIT NONE  
         CLASS(FTData)    :: self
         CHARACTER(LEN=DATA_CLASS_TYPE_LENGTH) :: t
         t = self % dataType
      END FUNCTION dataType
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION dataFromObject(obj) RESULT(cast)
!
!     -----------------------------------------------------
!     Cast the base class FTObject to the FTException class
!     -----------------------------------------------------
!
         IMPLICIT NONE  
         CLASS(FTObject) , POINTER :: obj
         CLASS(FTData)   , POINTER :: cast
         
         cast => NULL()
         SELECT TYPE (e => obj)
            TYPE is (FTData)
               cast => e
            CLASS DEFAULT
               
         END SELECT
         
      END FUNCTION dataFromObject
!
!//////////////////////////////////////////////////////////////////////// 
! 
!      -----------------------------------------------------------------
!> Class name returns a string with the name of the type of the object
!>
!>  ### Usage:
!>
!>        PRINT *,  obj % className()
!>        if( obj % className = "FTData")
!>
      FUNCTION dataClassName(self)  RESULT(s)
         IMPLICIT NONE  
         CLASS(FTData)                              :: self
         CHARACTER(LEN=CLASS_NAME_CHARACTER_LENGTH) :: s
         
         s = "FTData"
         IF( self % refCount() >= 0 ) CONTINUE 
 
      END FUNCTION dataClassName
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION dataIsOfType(self, dataType)  RESULT(t)
         IMPLICIT NONE  
         CLASS(FTData)                         :: self
         CHARACTER(LEN=DATA_CLASS_TYPE_LENGTH) :: dataType
         LOGICAL                               :: t
         
         IF ( dataType == self % dataType )     THEN
            t = .TRUE. 
         ELSE 
            t = .FALSE. 
         END IF 
      END FUNCTION dataIsOfType
      
      END Module FTDataClass
