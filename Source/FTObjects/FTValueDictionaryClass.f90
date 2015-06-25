!
!////////////////////////////////////////////////////////////////////////
!
!      FTValueDictionary.f90
!      Created: February 6, 2013 8:54 AM 
!      By: David Kopriva  
!
!> The FTValueDictionary subclass of FTDictionary adds convenient methods
!> to easily add fundamental (Real, integer,…) values to a dicitonary.
!> As a subclass, all other methods are still available.
!>
! Usage:
!
!     CALL dict % addValueForKey(1,"integer")
!     CALL dict % addValueForKey(3.14,"real")
!     CALL dict % addValueForKey(98.6d0,"double")
!     CALL dict % addValueForKey(.true.,"logical")
!     CALL dict % addValueForKey("Hello World","string")
!
!     i = dict % integerValueForKey("integer")
!     r = dict % realValueForKey("real")
!     d = dict % doublePrecisionValueForKey("double")
!     l = dict % logicalValueForKey("logical")
!     s = dict % stringValueForKey("string",15)
!
!////////////////////////////////////////////////////////////////////////
!
      Module FTValueDictionaryClass
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
!
!        -------
!        Getters
!        -------
!
         PROCEDURE :: realValueForKey
         PROCEDURE :: doublePrecisionValueForKey
         PROCEDURE :: integerValueForKey
         PROCEDURE :: stringValueForKey
         PROCEDURE :: logicalValueForKey
         
      END TYPE FTValueDictionary
!
         
!      INTERFACE cast
!         MODULE PROCEDURE castObjectToValueDictionary
!      END INTERFACE cast
!      
      CONTAINS  
!@mark -
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
         CALL v % release()
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
         CALL v % release()
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
         CALL v % release()
      END SUBROUTINE addDoublePrecisionValueForKey
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
         CALL v % release()
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
         CALL v % release()
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
            CALL cast(obj,v)
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
            CALL cast(obj,v)
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
            CALL cast(obj,v)
            doublePrecisionValueForKey = v % doublePrecisionValue()
         ELSE
            doublePrecisionValueForKey = HUGE(doublePrecisionValueForKey)
         END IF 
         
      END FUNCTION doublePrecisionValueForKey    
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
            CALL cast(obj,v)
            logicalValueForKey = v % logicalValue()
         ELSE 
            logicalValueForKey = .FALSE.
         END IF 
         
      END FUNCTION logicalValueForKey    
!
!//////////////////////////////////////////////////////////////////////// 
! 
       FUNCTION stringValueForKey(self,key,requestedLength)  
         IMPLICIT NONE  
         CLASS(FTValueDictionary)       :: self
         CHARACTER(LEN=*)               :: key
         INTEGER                        :: requestedLength
         CHARACTER(LEN=requestedLength) :: stringValueForKey
         
         CLASS(FTValue) , POINTER :: v   => NULL()
         CLASS(FTObject), POINTER :: obj => NULL()
         
         obj => self % objectForKey(key)
         IF ( ASSOCIATED(obj) )     THEN
            CALL cast(obj,v)
            stringValueForKey = v % stringValue(requestedLength)
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
!     Cast the base class FTObject to the FTException class
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
         
         obj => NULL()
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
!     Cast the base class FTObject to the FTException class
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
         
         obj => NULL()
         SELECT TYPE (obj)
            TYPE is (FTValueDictionary)
               valueDict => obj
            CLASS DEFAULT
               
         END SELECT
         
      END FUNCTION valueDictionaryFromObject
 
      
      END Module FTValueDictionaryClass    