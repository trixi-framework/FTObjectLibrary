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
!      FTValueDictionary.f90
!      Created: February 6, 2013 8:54 AM 
!      By: David Kopriva  
!
!>
!> The FTValueDictionary subclass of FTDictionary adds convenient methods
!> to easily add fundamental (Real, integer,…) values to a dictionary.
!>
!> As a subclass, all other methods are still available.
!>
!>#Usage:
!>#Adding a value
!>
!>     CALL dict % addValueForKey(1,"integer")
!>     CALL dict % addValueForKey(3.14,"real")
!>     CALL dict % addValueForKey(98.6d0,"double")
!>     CALL dict % addValueForKey(.true.,"logical")
!>     CALL dict % addValueForKey("Hello World","string")
!>#Accessing a value
!>     i = dict % integerValueForKey("integer")
!>     r = dict % realValueForKey("real")
!>     d = dict % doublePrecisionValueForKey("double")
!>     l = dict % logicalValueForKey("logical")
!>     s = dict % stringValueForKey("string")
!>#Converting an FTDictionary to an FTValueDictionary
!>            valueDict => valueDictionaryFromDictionary(dict)
!>#Converting an FTObject to an FTValueDictionary
!>            valueDict => valueDictionaryFromObject(obj)
!>#Destruction
!>      call releaseFTValueDictionary(dict) [Pointers]
!
!////////////////////////////////////////////////////////////////////////
!
      Module FTValueDictionaryClass
      USE ISO_FORTRAN_ENV
      USE FTDictionaryClass
      USE FTValueClass
      IMPLICIT NONE
      
      TYPE, EXTENDS(FTDictionary) :: FTValueDictionary
!
!        --------      
         CONTAINS
!        --------
!
!        -------
!        Setters
!        -------
!
         PROCEDURE, PRIVATE :: addRealValueForKey
         PROCEDURE, PRIVATE :: addDoublePrecisionValueForKey
         PROCEDURE, PRIVATE :: addIntegerValueForKey
         PROCEDURE, PRIVATE :: addStringValueForKey
         PROCEDURE, PRIVATE :: addLogicalValueForKey
         GENERIC, PUBLIC    :: addValueForKey => addRealValueForKey,  &
                                      addDoublePrecisionValueForKey,  &
                                      addIntegerValueForKey,          &
                                      addStringValueForKey,           &
                                      addLogicalValueForKey
#ifdef _has_Quad
         PROCEDURE, PRIVATE :: addQuadValueForKey
         GENERIC, PUBLIC    :: addValueForKey => addQuadValueForKey
#endif
!
!        -------
!        Getters
!        -------
!
         PROCEDURE :: realValueForKey
         PROCEDURE :: doublePrecisionValueForKey
#ifdef _has_Quad
         PROCEDURE :: quadValueForKey
#endif
         PROCEDURE :: integerValueForKey
         PROCEDURE :: stringValueForKey
         PROCEDURE :: logicalValueForKey
!
!        -------------
!        Introspection
!        -------------
!
         PROCEDURE :: className => valueDictionaryClassName         
      END TYPE FTValueDictionary
!      
      CONTAINS  
!@mark -
!
!------------------------------------------------
!> Public, generic name: release(self)
!>
!> Call release(self) on an object to release control
!> of an object. If its reference count is zero, then 
!> it is deallocated.
!------------------------------------------------
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE releaseFTValueDictionary(self)  
         IMPLICIT NONE
         TYPE(FTValueDictionary) , POINTER :: self
         CLASS(FTObject)         , POINTER :: obj
         
         IF(.NOT. ASSOCIATED(self)) RETURN
         
         obj => self
         CALL releaseFTObject(self = obj)
         IF ( .NOT. ASSOCIATED(obj) )     THEN
            self => NULL() 
         END IF      
      END SUBROUTINE releaseFTValueDictionary
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE addIntegerValueForKey(self,i,key)
         IMPLICIT NONE
         CLASS(FTValueDictionary) :: self
         INTEGER                  :: i
         CHARACTER(LEN=*)         :: key
         CLASS(FTValue), POINTER  :: v   => NULL()
         CLASS(FTObject), POINTER :: obj => NULL()
         
         ALLOCATE(v)
         CALL v % initWithValue(i)
         obj => v
         CALL self % addObjectforKey(obj,key)
         CALL release(obj)
      END SUBROUTINE addIntegerValueForKey
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE addRealValueForKey(self,r,key)
         IMPLICIT NONE
         CLASS(FTValueDictionary) :: self
         REAL                     :: r
         CHARACTER(LEN=*)         :: key
         CLASS(FTValue), POINTER  :: v   => NULL()
         CLASS(FTObject), POINTER :: obj => NULL()
         
         ALLOCATE(v)
         CALL v % initWithValue(r)
         obj => v
         CALL self % addObjectforKey(obj,key)
         CALL release(obj)
      END SUBROUTINE addRealValueForKey
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE addDoublePrecisionValueForKey(self,r,key)
         IMPLICIT NONE
         CLASS(FTValueDictionary) :: self
         DOUBLE PRECISION         :: r
         CHARACTER(LEN=*)         :: key
         CLASS(FTValue), POINTER  :: v   => NULL()
         CLASS(FTObject), POINTER :: obj => NULL()
         
         ALLOCATE(v)
         CALL v % initWithValue(r)
         obj => v
         CALL self % addObjectforKey(obj,key)
         CALL release(obj)
      END SUBROUTINE addDoublePrecisionValueForKey
!
!//////////////////////////////////////////////////////////////////////// 
! 
#ifdef _has_Quad
      SUBROUTINE addQuadValueForKey(self,r,key)
         IMPLICIT NONE
         CLASS(FTValueDictionary) :: self
         REAL(KIND=SELECTED_REAL_KIND(QUAD_DIGITS))       :: r
         CHARACTER(LEN=*)         :: key
         CLASS(FTValue), POINTER  :: v   => NULL()
         CLASS(FTObject), POINTER :: obj => NULL()
         
         ALLOCATE(v)
         CALL v % initWithValue(r)
         obj => v
         CALL self % addObjectforKey(obj,key)
         CALL release(v)
      END SUBROUTINE addQuadValueForKey
#endif
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE addStringValueForKey(self,s,key)
         IMPLICIT NONE
         CLASS(FTValueDictionary) :: self
         CHARACTER(LEN=*)         :: s
         CHARACTER(LEN=*)         :: key
         CLASS(FTValue), POINTER  :: v   => NULL()
         CLASS(FTObject), POINTER :: obj => NULL()
         
         ALLOCATE(v)
         CALL v % initWithValue(s)
         obj => v
         CALL self % addObjectforKey(obj,key)
         CALL release(obj)
      END SUBROUTINE addStringValueForKey
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE addLogicalValueForKey(self,l,key)
         IMPLICIT NONE
         CLASS(FTValueDictionary) :: self
         LOGICAL                  :: l
         CHARACTER(LEN=*)         :: key
         CLASS(FTValue), POINTER  :: v   => NULL()
         CLASS(FTObject), POINTER :: obj => NULL()
         
         ALLOCATE(v)
         CALL v % initWithValue(l)
         obj => v
         CALL self % addObjectforKey(obj,key)
         CALL release(obj)
      END SUBROUTINE addLogicalValueForKey
!@mark -
!
!//////////////////////////////////////////////////////////////////////// 
! 
      REAL FUNCTION realValueForKey(self,key)  
         IMPLICIT NONE  
         CLASS(FTValueDictionary) :: self
         CHARACTER(LEN=*)         :: key
         
         CLASS(FTValue) , POINTER :: v   => NULL()
         CLASS(FTObject), POINTER :: obj => NULL()
         
         obj => self % objectForKey(key)
         IF ( ASSOCIATED(obj) )     THEN
            v => valueFromObject(obj)
            realValueForKey = v % realValue()
         ELSE
            realValueForKey = HUGE(realValueForKey)
         END IF 
         
      END FUNCTION realValueForKey    
!
!//////////////////////////////////////////////////////////////////////// 
! 
      INTEGER FUNCTION integerValueForKey(self,key)  
         IMPLICIT NONE  
         CLASS(FTValueDictionary) :: self
         CHARACTER(LEN=*)         :: key
         
         CLASS(FTValue) , POINTER :: v   => NULL()
         CLASS(FTObject), POINTER :: obj => NULL()
         
         obj => self % objectForKey(key)
         IF ( ASSOCIATED(obj) )     THEN
            v => valueFromObject(obj)
            integerValueForKey = v % integerValue()
         ELSE
            integerValueForKey = HUGE(integerValueForKey)
         END IF 
         
      END FUNCTION integerValueForKey    
!
!//////////////////////////////////////////////////////////////////////// 
! 
      DOUBLE PRECISION FUNCTION doublePrecisionValueForKey(self,key)  
         IMPLICIT NONE  
         CLASS(FTValueDictionary) :: self
         CHARACTER(LEN=*)         :: key
         
         CLASS(FTValue) , POINTER :: v   => NULL()
         CLASS(FTObject), POINTER :: obj => NULL()
         
         obj => self % objectForKey(key)
         IF ( ASSOCIATED(obj) )     THEN
            v => valueFromObject(obj)
            doublePrecisionValueForKey = v % doublePrecisionValue()
         ELSE
            doublePrecisionValueForKey = HUGE(doublePrecisionValueForKey)
         END IF 
         
      END FUNCTION doublePrecisionValueForKey    
!
!//////////////////////////////////////////////////////////////////////// 
! 
#ifdef _has_Quad
      REAL(KIND=SELECTED_REAL_KIND(QUAD_DIGITS)) FUNCTION quadValueForKey(self,key)  
         IMPLICIT NONE  
         CLASS(FTValueDictionary) :: self
         CHARACTER(LEN=*)         :: key
         
         CLASS(FTValue) , POINTER :: v   => NULL()
         CLASS(FTObject), POINTER :: obj => NULL()
         
         obj => self % objectForKey(key)
         IF ( ASSOCIATED(obj) )     THEN
            v => valueFromObject(obj)
            quadValueForKey = v % quadValue()
         ELSE
            quadValueForKey = HUGE(quadValueForKey)
         END IF 
         
      END FUNCTION quadValueForKey  
#endif  
!
!//////////////////////////////////////////////////////////////////////// 
! 
      LOGICAL FUNCTION logicalValueForKey(self,key)  
         IMPLICIT NONE  
         CLASS(FTValueDictionary) :: self
         CHARACTER(LEN=*)         :: key
         
         CLASS(FTValue) , POINTER :: v   => NULL()
         CLASS(FTObject), POINTER :: obj => NULL()
         
         obj => self % objectForKey(key)
         IF ( ASSOCIATED(obj) )     THEN
            v => valueFromObject(obj)
            logicalValueForKey = v % logicalValue()
         ELSE 
            logicalValueForKey = .FALSE.
         END IF 
         
      END FUNCTION logicalValueForKey    
!
!//////////////////////////////////////////////////////////////////////// 
! 
       FUNCTION stringValueForKey(self,key)  
         IMPLICIT NONE  
         CLASS(FTValueDictionary)       :: self
         CHARACTER(LEN=*)               :: key
         CHARACTER(LEN=:), ALLOCATABLE  :: stringValueForKey
         
         CLASS(FTValue) , POINTER :: v   => NULL()
         CLASS(FTObject), POINTER :: obj => NULL()
         
         obj => self % objectForKey(key)
         IF ( ASSOCIATED(obj) )     THEN
            v => valueFromObject(obj)
            stringValueForKey = v % stringValue()
         ELSE 
            stringValueForKey = "" 
         END IF 
         
      END FUNCTION stringValueForKey    
!@mark -
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE castDictionaryToValueDictionary(dict,valueDict) 
!
!     -----------------------------------------------------
!     Cast a FTDictionary to a FTValueDictionary as a
!     subroutine call.
!     -----------------------------------------------------
!
         IMPLICIT NONE  
         CLASS(FTDictionary)     , POINTER :: dict
         CLASS(FTValueDictionary), POINTER :: valueDict
         
         valueDict => NULL()
         SELECT TYPE (dict)
            TYPE is (FTValueDictionary)
               valueDict => dict
            CLASS DEFAULT
               
         END SELECT
         
      END SUBROUTINE castDictionaryToValueDictionary
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE castObjectToValueDictionary(obj,valueDict) 
!
!     -----------------------------------------------------------
!     Cast the base class FTObject to the FTValueDictionary class
!     -----------------------------------------------------------
!
         IMPLICIT NONE  
         CLASS(FTValueDictionary), POINTER :: valueDict
         CLASS(FTObject)         , POINTER :: obj
         
         valueDict => NULL()
         SELECT TYPE (obj)
            TYPE is (FTValueDictionary)
               valueDict => obj
            CLASS DEFAULT
               
         END SELECT
         
      END SUBROUTINE castObjectToValueDictionary
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION valueDictionaryFromDictionary(dict) RESULT(valueDict)
!
!     -----------------------------------------------------
!     Cast FTDictionary to the FTValueDictionary class as
!     a function call.
!     -----------------------------------------------------
!
         IMPLICIT NONE  
         CLASS(FTDictionary)     , POINTER :: dict
         CLASS(FTValueDictionary), POINTER :: valueDict
         
         valueDict => NULL()
         SELECT TYPE (dict)
            TYPE is (FTValueDictionary)
               valueDict => dict
            CLASS DEFAULT
               
         END SELECT
         
      END FUNCTION valueDictionaryFromDictionary
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION valueDictionaryFromObject(obj) RESULT(valueDict)
!
!     -----------------------------------------------------------
!     Cast the base class FTObject to the FTValueDictionary class
!     -----------------------------------------------------------
!
         IMPLICIT NONE  
         CLASS(FTValueDictionary), POINTER :: valueDict
         CLASS(FTObject)         , POINTER :: obj
         
         valueDict => NULL()
         SELECT TYPE (obj)
            TYPE is (FTValueDictionary)
               valueDict => obj
            CLASS DEFAULT
               
         END SELECT
         
      END FUNCTION valueDictionaryFromObject
!
!//////////////////////////////////////////////////////////////////////// 
! 
!      -----------------------------------------------------------------
!> Class name returns a string with the name of the type of the object
!>
!>  ### Usage:
!>
!>        PRINT *,  obj % className()
!>        if( obj % className = "FTValueDictionary")
!>
      FUNCTION valueDictionaryClassName(self)  RESULT(s)
         IMPLICIT NONE  
         CLASS(FTValueDictionary)                   :: self
         CHARACTER(LEN=CLASS_NAME_CHARACTER_LENGTH) :: s
         
         s = "FTValueDictionary"
         IF( self % refCount() >= 0) CONTINUE 
 
      END FUNCTION valueDictionaryClassName
 
      
      END Module FTValueDictionaryClass    