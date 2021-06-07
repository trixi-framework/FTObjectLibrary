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
!      FTStringSet.f90
!
!>FTStringSet is a class for an unordered collection of strings. Use a FTStringSet
!>to store strings as an alternative to arrays when the order is not important, but
!>testing for membership is.
!>
!>##Definition
!>           TYPE(FTStringSet) :: varName
!>#Usage
!>##Initialization
!>       CLASS(FTStringSet)  :: FTStringSet
!>       integer             :: N = 11
!>       logical             :: cs = .true.
!>       CALL FTStringSet % initFTStringSet(N,cs)
!>
!>       CLASS(FTStringSet)  :: FTStringSet
!>       CHARACTER(LEN=*)    :: strings(:)
!>       CALL FTStringSet % initWithStrings(strings)
!>
!>#Destruction
!>      CALL FTStringSet  %  destuct() [Non Pointers]
!>      CALL releaseFTStringSet(stringSet) [Pointers]
!>#Adding Strings
!>         CALL set % addString(str)
!>#Testing membership:
!>      if(set % containsString(str))     THEN
!>#Getting an array of members
!>      CHARACTER(LEN=FTDICT_KWD_STRING_LENGTH) ,DIMENSION(:), POINTER :: s
!>      s => set % strings
!>      ... do something ...
!>      DEALLOCATE(s)
!>#Set operations, union, intersection, difference
!>      newSet => set1 % unionWithSet(set2)
!>      ... do something ...
!>      call releaseFTStringSet(newSet)
!>
!>      newSet => set1 % intersectionWithSet(set2)
!>      ... do something ...
!>      call releaseFTStringSet(newSet)
!>
!>      newSet => set1 % setFromDifference(set2)
!>      ... do something ...
!>      call releaseFTStringSet(newSet)
!
!
!////////////////////////////////////////////////////////////////////////
!
      MODULE FTStringSetClass
      USE FTObjectClass
      USE FTDictionaryClass
      IMPLICIT NONE
      
      TYPE, EXTENDS(FTObject) ::  FTStringSet
         TYPE(FTDictionary), PRIVATE  :: dict
!
!        --------
         CONTAINS
!        --------
!
         PROCEDURE, PUBLIC :: initFTStringSet
         PROCEDURE, PUBLIC :: initWithStrings
         
         FINAL             :: destructFTStringSet
         
         PROCEDURE, PUBLIC :: addString
         PROCEDURE, PUBLIC :: containsString
         PROCEDURE, PUBLIC :: strings
         PROCEDURE, PUBLIC :: unionWithSet
         PROCEDURE, PUBLIC :: intersectionWithSet
!         PROCEDURE, PUBLIC :: setIsCaseSensitive
!         PROCEDURE, PUBLIC :: isCaseSensitive 
         PROCEDURE, PUBLIC :: setFromDifference
         PROCEDURE, PUBLIC :: isEmpty
         PROCEDURE, PUBLIC :: count => stringCount
         
         PROCEDURE, PUBLIC :: printDescription => printFTStringSet
         PROCEDURE, PUBLIC :: className        => FTStringSetClassName
!         
      END TYPE 
         
!      INTERFACE cast
!         MODULE PROCEDURE castToFTStringSet
!      END INTERFACE cast
!
!     ======== 
      CONTAINS  
!     ======== 
!
!
!//////////////////////////////////////////////////////////////////////// 
! 
!>
!> Designated initializer. Initializes the amount of storage, but
!> the FTStringSet remains empty.
!>
!> *Usage
!>       CLASS(FTStringSet)  :: FTStringSet
!>       integer             :: N = 11
!>       logical             :: cs = .true.
!>       CALL FTStringSet % initFTStringSet(N)
!>
      SUBROUTINE initFTStringSet( self, FTStringSetSize )    
         IMPLICIT NONE  
         CLASS( FTStringSet) :: self
         INTEGER             :: FTStringSetSize
         
         CALL self % FTObject % init()
         
         CALL self % dict % initWithSize(sze = FTStringSetSize)
!         self % dict % isCaseSensitive = caseSensitive
         
      END SUBROUTINE initFTStringSet
!
!//////////////////////////////////////////////////////////////////////// 
! 
!>
!>  initializer. Initializes the amount of storage from the strings passed
!> *Usage
!>       CLASS(FTStringSet)  :: FTStringSet
!>       CHARACTER(LEN=*)    :: strings(:)
!>       CALL FTStringSet % initWithStrings(strings)
!>
      SUBROUTINE initWithStrings( self, strings )    
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS( FTStringSet) :: self
         CHARACTER(LEN=*)    :: strings(:)
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER                  :: stringCount, i, dictSize
         
         stringCount = SIZE(strings)
         dictSize    = 2**EXPONENT(REAL(stringCount)) ! Makes it a factor of two
         
         CALL self % initFTStringSet(FTStringSetSize = dictSize)
         
         DO i = 1, stringCount 
            CALL self % addString(str = STRINGs(i))
         END DO 
         
      END SUBROUTINE initWithStrings
!
!//////////////////////////////////////////////////////////////////////// 
! 
!>
!> Destructor for the class. This is called automatically when the
!> reference count reaches zero. Do not call this yourself on pointers
!>
      SUBROUTINE destructFTStringSet(self)  
         IMPLICIT NONE
         TYPE( FTStringSet) :: self

      END SUBROUTINE destructFTStringSet
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
      SUBROUTINE releaseFTStringSet(self)  
         IMPLICIT NONE
         TYPE(FTStringSet) , POINTER :: self
         CLASS(FTObject)   , POINTER :: obj
                  
         obj => self
         CALL release(self = obj)
         IF ( .NOT. ASSOCIATED(obj) )     THEN
            NULLIFY(self)
         END IF      
      END SUBROUTINE releaseFTStringSet
!
!//////////////////////////////////////////////////////////////////////// 
! 
      INTEGER FUNCTION stringCount(self)  
         IMPLICIT NONE  
         CLASS(FTStringSet) :: self
         stringCount = self % dict % COUNT()
      END FUNCTION stringCount
!
!//////////////////////////////////////////////////////////////////////// 
! 
!      -----------------------------------------------------------------
!> AddString adds a string to the set if it is not already present
!>
!>### Usage:
!>        CALL set % addString(str)
!>
      SUBROUTINE AddString(self,str)  
         IMPLICIT NONE
         CLASS(FTStringSet)          :: self
         CHARACTER(LEN=*)            :: str
         CLASS(FTObject)   , POINTER :: obj
         
         IF(self % dict % containsKey(key = str))     RETURN 
         
         ALLOCATE(obj)
         CALL self % dict % addObjectForKey(object = obj,key = str)
         CALL release(obj)
         
      END SUBROUTINE AddString
!
!//////////////////////////////////////////////////////////////////////// 
! 
!      -----------------------------------------------------------------
!> containsString returns .TRUE. if the set contains the string, .FALSE. 
!> otherwise.
!>
!>### Usage:
!>        if(set % containsString(str))
!>
      LOGICAL FUNCTION containsString(self,str)
         IMPLICIT NONE  
         CLASS(FTStringSet) :: self
         CHARACTER(LEN=*)   :: str
         
         containsString = self % dict % containsKey(key = str)
         
      END FUNCTION containsString
!
!//////////////////////////////////////////////////////////////////////// 
! 
!      -----------------------------------------------------------------
!> strings returns a pointer to an array of strings that are in the set.
!> Deallocate this array when done with it. 
!>
!>### Usage:
!>
!>      CHARACTER(LEN=FTDICT_KWD_STRING_LENGTH) ,DIMENSION(:), POINTER :: s
!>      s => set % strings
!>      ... do something ...
!>      DEALLOCATE(s)
!>
      FUNCTION strings(self)  RESULT(s)
          IMPLICIT NONE  
          CLASS(FTStringSet)                                             :: self
          CHARACTER(LEN=FTDICT_KWD_STRING_LENGTH) ,DIMENSION(:), POINTER :: s
          
          s => self % dict % allKeys()
          
      END FUNCTION strings
!
!//////////////////////////////////////////////////////////////////////// 
! 
!> unionWithSet returns a pointer to a new set that is the union of two sets.
!> the new set has reference count of 1. Release when done.
!>
!>### Usage:
!>
!>      newSet => set1 % unionWithSet(set2)
!>      ... do something ...
!>      call releaseFTStringSet(newSet)
!
      FUNCTION unionWithSet(self,set)  RESULT(newSet)
         IMPLICIT NONE  
         CLASS(FTStringSet)                                             :: self, set
         CHARACTER(LEN=FTDICT_KWD_STRING_LENGTH) ,DIMENSION(:), POINTER :: s1, s2
         INTEGER                                                        :: i
         TYPE(FTStringSet), POINTER                                     :: newSet
         
         ALLOCATE(newSet)
         s1 => self % strings()
         CALL newSet % initWithStrings(strings = s1)
         DEALLOCATE(s1)
         
         s2 => set % strings()
         DO i = 1, SIZE(s2) 
            CALL newSet % addString(str = s2(i)) 
         END DO 
         DEALLOCATE(s2)
         
      END FUNCTION unionWithSet
!
!//////////////////////////////////////////////////////////////////////// 
! 
!> intersectionWithSet returns a pointer to a new set that is the intersection of two sets.
!> the new set has reference count of 1. Release when done.
!>
!>### Usage:
!>
!>      newSet => set1 % intersectionWithSet(set2)
!>      ... do something ...
!>      call releaseFTStringSet(newSet)
!
      FUNCTION intersectionWithSet(self, set)  RESULT(newSet)
         IMPLICIT NONE  
         CLASS(FTStringSet)                                             :: self, set
         CHARACTER(LEN=FTDICT_KWD_STRING_LENGTH) ,DIMENSION(:), POINTER :: strArray
         INTEGER                                                        :: i
         TYPE(FTStringSet), POINTER                                     :: newSet
         
        
         ALLOCATE(newSet)
         CALL newSet % initFTStringSet(FTStringSetSize = 16)
         
         strArray => self % strings()
         IF(.NOT.ASSOCIATED(strArray)) RETURN 
        
         DO i = 1, SIZE(strArray) 
            IF ( set % containsString(str = strArray(i)) )     THEN
               CALL newSet % addString(str = strArray(i))
            END IF 
         END DO 
         DEALLOCATE(strArray)
         
      END FUNCTION intersectionWithSet
!
!//////////////////////////////////////////////////////////////////////// 
! 
!> setFromDifference returns a pointer to a new set that is the difference of two sets.
!> $$A - B = \{x: x \in A \;\rm{ and }\; x\notin B\}$$
!> the new set has reference count of 1. Release when done.
!>
!>### Usage:
!>
!>      newSet => set1 % setFromDifference(set2)
!>      ... do something ...
!>      call releaseFTStringSet(newSet)
!
      FUNCTION setFromDifference(self, set)  RESULT(newSet)
         IMPLICIT NONE  
         CLASS(FTStringSet)                                             :: self, set
         CHARACTER(LEN=FTDICT_KWD_STRING_LENGTH) ,DIMENSION(:), POINTER :: strArray
         INTEGER                                                        :: i
         TYPE(FTStringSet), POINTER                                     :: newSet
         
         ALLOCATE(newSet)
         CALL newSet % initFTStringSet(FTStringSetSize = MAX(self % count(),8))
         
         IF(self % count() ==0 )     RETURN

         strArray => self % strings()
         IF(.NOT.ASSOCIATED(strArray)) RETURN 
         
         DO i = 1, SIZE(strArray) 
            IF ( .NOT.set % containsString(str = strArray(i)) )     THEN
               CALL newSet % addString(str = strArray(i))
            END IF 
         END DO 
         DEALLOCATE(strArray)
         
      END FUNCTION setFromDifference
!
!//////////////////////////////////////////////////////////////////////// 
! 
      LOGICAL FUNCTION isEmpty(self)  
         IMPLICIT NONE  
         CLASS(FTStringSet) :: self
         isEmpty = .TRUE.
         IF(self % count() > 0)   isEmpty = .FALSE. 
      END FUNCTION isEmpty
!
!//////////////////////////////////////////////////////////////////////// 
! 
!      SUBROUTINE setIsCaseSensitive(self,sensitive)  
!         IMPLICIT NONE  
!         CLASS(FTStringSet) :: self
!         LOGICAL            :: sensitive
!         self % dict % isCaseSensitive = sensitive
!      END SUBROUTINE setIsCaseSensitive
!
!//////////////////////////////////////////////////////////////////////// 
! 
!      LOGICAL FUNCTION isCaseSensitive(self)  
!         IMPLICIT NONE  
!         CLASS(FTStringSet) :: self
!         isCaseSensitive = self % dict % isCaseSensitive
!      END FUNCTION isCaseSensitive
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE printFTStringSet(self,iUnit)  
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(FTStringSet)          :: self
         INTEGER                     :: iUnit
!
!        ---------------
!        Local Variables
!        ---------------
!
         INTEGER                                          :: i
         CHARACTER(LEN=FTDICT_KWD_STRING_LENGTH), POINTER :: keys(:)
         
         keys => self % dict % allKeys()
         DO i = 1, SIZE(keys) 
            PRINT *, TRIM(keys(i)) 
         END DO 
         DEALLOCATE(keys)
         
      END SUBROUTINE printFTStringSet

!
!---------------------------------------------------------------------------
!> Generic Name: cast
!> 
!> Cast a pointer to the base class to an FTStringSet pointer 
!---------------------------------------------------------------------------
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION FTStringSetFromObject(obj) RESULT(cast)
!
!     -----------------------------------------------------
!     Cast the base class FTObject to the FTException class
!     -----------------------------------------------------
!
         IMPLICIT NONE  
         CLASS(FTObject)            , POINTER :: obj
         CLASS(FTStringSet), POINTER :: cast
         
         cast => NULL()
         SELECT TYPE (e => obj)
            TYPE is (FTStringSet)
               cast => e
            CLASS DEFAULT
               
         END SELECT
         
      END FUNCTION FTStringSetFromObject
!
!//////////////////////////////////////////////////////////////////////// 
! 
!      -----------------------------------------------------------------
!> Class name returns a string with the name of the type of the object
!>
!>### Usage:
!>
!>        PRINT *,  obj % className()
!>        if( obj % className = "FTStringSet")
!>
      FUNCTION FTStringSetClassName(self)  RESULT(s)
         IMPLICIT NONE  
         CLASS(FTStringSet)                :: self
         CHARACTER(LEN=CLASS_NAME_CHARACTER_LENGTH) :: s
         
         s = "FTStringSet"
 
      END FUNCTION FTStringSetClassName

      
      END MODULE  FTStringSetClass    