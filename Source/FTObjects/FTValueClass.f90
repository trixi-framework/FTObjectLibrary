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
!
!> A not completely F2003/2008 version of an immutable class
!> to store primitive values: integer, real, double precision, logical,
!> character. (To Add: complex)
!>
!> This version does not use CLASS(*) or deferred length strings
!> so that it can be used with gfortran 4.7/4.8
!>
!> Usage:
!> ------
!>
!> - Initialization
!>
!>            TYPE(FTValue) :: r, i, s, l, d
!>
!>            CALL r % initValue(3.14)
!>            CALL i % initValue(6)
!>            CALL d % initValue(3.14d0)
!>            CALL l % initValue(.true.)
!>            CALL s % initValue("A string")
!>
!> - Destruction 
!>
!>            call releaseFTValue(r) [Pointers]
!>
!> - Accessors
!>
!>            real = r % realValue()
!>            int  = i % integerValue()
!>            doub = d % doublePrecisionValue()
!>            logc = l % logicalValue()
!>            str  = s % stringValue()
!>
!> - Description
!>
!>            str = v % description()
!>            call v % printDescription(unit)
!>
!> - Casting
!>
!>            CLASS(FTVALUE) , POINTER :: v
!>            CLASS(FTObject), POINTER :: obj
!>            call cast(obj,v)
!>      
!> The class will attempt to convert between the different types:
!>
!>            CALL r % initWithReal(3.14)
!>            print *, r % stringValue()
!>
!>            Logical variables rules:
!>
!>            real, doublePrecision, integer values
!>               logicalValue = .FALSE. if input = 0
!>               logicalValue = .TRUE.  if input /= 0
!>
!> String values can be converted to numeric types. If the string is
!> not a numeric, Huge(1) will be returned, for integers and NaN for reals.
!>      
!<
! FTValueClass.f90
! Created: January 9, 2013 12:20 PM 
!> @author David Kopriva  
!<
!
!////////////////////////////////////////////////////////////////////////
!
      Module FTValueClass
      USE IEEE_ARITHMETIC
      USE ISO_FORTRAN_ENV
      USE FTOLConstants
      USE FTObjectClass
      IMPLICIT NONE
!
!     ----------------
!     Public constants
!     ----------------
!
      INTEGER          , PARAMETER :: FTVALUE_NOT_INTEGER         = HUGE(1)
      REAL             , PARAMETER :: FTVALUE_NOT_REAL            = HUGE(1.0)
      DOUBLE PRECISION , PARAMETER :: FTVALUE_NOT_DOUBLEPRECISION = HUGE(1.0D0)
      INTEGER          , PARAMETER :: FTVALUE_STRING_LENGTH       = 512 ! Until vary length strings are available.
      
      INTEGER, PARAMETER :: FT_REAL_KIND             = SELECTED_REAL_KIND(6)
      INTEGER, PARAMETER :: FT_DOUBLE_PRECISION_KIND = SELECTED_REAL_KIND(15)
!
!     -----------------
!     Private constants
!     -----------------
!
      INTEGER, PARAMETER, PRIVATE :: FTVALUECLASS_INTEGER = 1, FTVALUECLASS_REAL   = 2, &
                                     FTVALUECLASS_DOUBLE  = 3, FTVALUECLASS_STRING = 4, &
                                     FTVALUECLASS_LOGICAL = 5, FTVALUECLASS_QUAD   = 6
!
!     ---------------------
!     Class type definition 
!     ---------------------
!
      TYPE, EXTENDS(FTObject) :: FTValue
         PRIVATE
         INTEGER                       :: valueType
         CHARACTER(LEN=1), ALLOCATABLE :: valueStorage(:) 
!
!        ========         
         CONTAINS
!        ========
!
!        --------------
!        Initialization
!        --------------
!
         PROCEDURE, PRIVATE :: initWithReal
         PROCEDURE, PRIVATE :: initWithDoublePrecision
         PROCEDURE, PRIVATE :: initWithString
         PROCEDURE, PRIVATE :: initWithLogical
         PROCEDURE, PRIVATE :: initWithInteger
         GENERIC  , PUBLIC  :: initWithValue => initWithReal,   initWithDoublePrecision, &
                                                initWithString, initWithLogical,         &
                                                initWithInteger
#ifdef _has_Quad
         PROCEDURE, PRIVATE :: initWithQuad
         GENERIC  , PUBLIC  :: initWithValue => initWithQuad
#endif
!
!        -----------
!        Destruction
!        -----------
!
         FINAL :: destructValue
!
!        -------
!        Getters
!        -------
!
         PROCEDURE :: realValue
         PROCEDURE :: doublePrecisionValue
#ifdef _has_Quad
         PROCEDURE :: quadValue
#endif
         
         PROCEDURE :: stringvalueR
         PROCEDURE :: stringValueA
         GENERIC   :: stringValue => stringvalueR, stringValueA
         PROCEDURE :: logicalValue
         PROCEDURE :: integerValue
!
!        -----------
!        Description
!        -----------
!
         PROCEDURE :: description      => FTValueDescription
         PROCEDURE :: printDescription => printValueDescription
         PROCEDURE :: className        => valueClassName
!
!        ----------
!        Comparison
!        ----------
!
!         PROCEDURE, PRIVATE :: isEqualTo => isEqualToFTValue

         
      END TYPE FTValue

      INTERFACE cast
         MODULE PROCEDURE castToValue
      END INTERFACE cast

!     ----------
!     Procedures
!     ----------
!
      CONTAINS 
!@mark -
!
!
!------------------------------------------------
!> Public, generic name: initwithValue()
!>
!> Initialize the value object with a real number
!------------------------------------------------
!
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE initWithReal(self,v) 
         IMPLICIT NONE
         CLASS(FTValue) :: self
         REAL           :: v
         INTEGER        :: dataLength
         
         CALL self % FTObject % init()
         
         dataLength = SIZE(TRANSFER(v,self % valueStorage))
         ALLOCATE(self % valueStorage(dataLength))
         self % valueStorage = TRANSFER(v,self % valueStorage)
         
         self % valueType = FTVALUECLASS_REAL
         
      END SUBROUTINE initWithReal
!
!-----------------------------------------------
!> Public, generic name: initwithValue()
!>
!> Initialize the value object with a double 
!> precision number
!-----------------------------------------------
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE initWithDoublePrecision(self,v) 
         IMPLICIT NONE
         CLASS(FTValue)  :: self
         DoublePrecision :: v
         INTEGER         :: dataLength
         
         CALL self % FTObject % init
         
         dataLength = SIZE(TRANSFER(v,self % valueStorage))
         ALLOCATE(self % valueStorage(dataLength))
         self % valueStorage = TRANSFER(v,self % valueStorage)
         
         self % valueType = FTVALUECLASS_DOUBLE
         
      END SUBROUTINE initWithDoublePrecision
!
!---------------------------------------------------
!> Public, generic name: initwithValue()
!>
!> Initialize the value object with a quad precision
!> number
!---------------------------------------------------
!
!
!////////////////////////////////////////////////////////////////////////
!
#ifdef _has_Quad

      SUBROUTINE initWithQuad(self,v) 
         IMPLICIT NONE
         CLASS(FTValue)                    :: self
         REAL(KIND=SELECTED_REAL_KIND(QUAD_DIGITS)) :: v
         INTEGER                           :: dataLength
         
         CALL self % FTObject % init()
         
         dataLength = SIZE(TRANSFER(v,self % valueStorage))
         ALLOCATE(self % valueStorage(dataLength))
         self % valueStorage = TRANSFER(v,self % valueStorage)
         
         self % valueType = FTVALUECLASS_QUAD
         
      END SUBROUTINE initWithQuad
#endif
!
!-----------------------------------------------
!> Public, generic name: initwithValue()
!>
!> Initialize the value object with an 
!> integer number
!-----------------------------------------------
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE initWithInteger(self,v) 
         IMPLICIT NONE
         CLASS(FTValue) :: self
         INTEGER        :: v
         INTEGER        :: dataLength
         
         CALL self % FTObject % init
         
         dataLength = SIZE(TRANSFER(v,self % valueStorage))
         ALLOCATE(self % valueStorage(dataLength))
         self % valueStorage = TRANSFER(v,self % valueStorage)
         
         self % valueType = FTVALUECLASS_INTEGER
        
      END SUBROUTINE initWithInteger
!
!-----------------------------------------------
!> Public, generic name: initwithValue()
!>
!> Initialize the value object with a logical
!-----------------------------------------------
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE initWithLogical(self,v) 
         IMPLICIT NONE
         CLASS(FTValue) :: self
         LOGICAL        :: v
         INTEGER        :: dataLength
         
         CALL self % FTObject % init
         
         dataLength = SIZE(TRANSFER(v,self % valueStorage))
         ALLOCATE(self % valueStorage(dataLength))
         self % valueStorage = TRANSFER(v,self % valueStorage)
         
         self % valueType = FTVALUECLASS_LOGICAL   
        
      END SUBROUTINE initWithLogical
!
!-----------------------------------------------
!> Public, generic name: initwithValue()
!>
!> Initialize the value object with a string
!-----------------------------------------------
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE initWithString(self,v) 
         IMPLICIT NONE
         CLASS(FTValue)  :: self
         CHARACTER(LEN=*):: v
         INTEGER         :: dataLength
         
         CALL self % FTObject % init
         
         dataLength = LEN_TRIM(v)
         ALLOCATE(self % valueStorage(dataLength))
         self % valueStorage = TRANSFER(trim(v),self % valueStorage)
         
         self % valueType = FTVALUECLASS_STRING
         
      END SUBROUTINE initWithString
!@mark -
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE releaseFTValue(self)  
         IMPLICIT NONE
         TYPE(FTValue)  , POINTER :: self
         CLASS(FTObject), POINTER :: obj
           
         IF(.NOT. ASSOCIATED(self)) RETURN
        
         obj => self
         CALL release(obj) 
         IF(.NOT.ASSOCIATED(obj)) self => NULL()
      END SUBROUTINE releaseFTValue
!
!------------------------------------------------
!> Public, generic name: destruct()
!>
!> Destructor for the class.
!------------------------------------------------
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE destructValue(self) 
         IMPLICIT NONE
         TYPE(FTValue)  :: self
         
         IF(ALLOCATED(self % valueStorage)) DEALLOCATE(self % valueStorage)
      END SUBROUTINE destructValue
! 
!@mark -
!
!---------------------------------------------------------------
!> Get the real value stored in the object, or convert the value
!> in the object to a real if it is of a different type.
!---------------------------------------------------------------
!
!////////////////////////////////////////////////////////////////////////
!
      REAL FUNCTION realValue(self)
         IMPLICIT NONE 
         CLASS(FTValue)   :: self
         INTEGER          :: iErr
         
         INTEGER                               :: i
         DOUBLE PRECISION                      :: d
         LOGICAL                               :: l
         CHARACTER(LEN= FTVALUE_STRING_LENGTH) :: s, tmpString
         
         SELECT CASE (self % valueType)
            CASE (FTVALUECLASS_INTEGER)
               i         = TRANSFER(self % valueStorage, i)
               realValue = REAL(i)
            CASE (FTVALUECLASS_DOUBLE)
               d         = TRANSFER(self % valueStorage, d)
               realValue = REAL(d)
            CASE (FTVALUECLASS_REAL)
                realValue = TRANSFER(self % valueStorage, realValue)
            CASE (FTVALUECLASS_STRING)
               tmpString = TRANSFER(self % valueStorage, tmpString)
               s         = tmpString(1:SIZE(self % valueStorage))
               READ(s,*,IOSTAT = iErr) realValue
               IF (iErr /= 0)     THEN
                  realValue = IEEE_VALUE(realValue,IEEE_QUIET_NAN)
               END IF
            CASE (FTVALUECLASS_LOGICAL)
               l = TRANSFER(self % valueStorage, l)
               IF ( l )     THEN
                  realValue = 1.0
               ELSE
                  realValue = 0.0
               END IF
         END SELECT
         
      END FUNCTION realValue   
!
!---------------------------------------------------------------------------
!> Get the double precision value stored in the object, or convert the value
!> in the object to a double precision if it is of a different type.
!---------------------------------------------------------------------------
!
!////////////////////////////////////////////////////////////////////////
!
      DOUBLE PRECISION FUNCTION doublePrecisionValue(self)
         IMPLICIT NONE 
         CLASS(FTValue)  :: self
         INTEGER         :: iErr
         
         REAL                                  :: r
         INTEGER                               :: i
         LOGICAL                               :: l
         CHARACTER(LEN= FTVALUE_STRING_LENGTH) :: s, tmpString
         
         SELECT CASE (self % valueType)
            CASE (FTVALUECLASS_INTEGER)
               i                    = TRANSFER(self % valueStorage, i)
               doublePrecisionValue = DBLE(i)
            CASE (FTVALUECLASS_REAL)
               r                    = TRANSFER(self % valueStorage, r)
               doublePrecisionValue = DBLE(r)
            CASE (FTVALUECLASS_DOUBLE)
                doublePrecisionValue = TRANSFER(self % valueStorage, doublePrecisionValue)
            CASE (FTVALUECLASS_STRING)
               tmpString = TRANSFER(self % valueStorage, tmpString)
               s         = tmpString(1:SIZE(self % valueStorage))
               READ(s,*,IOSTAT = iErr) doublePrecisionValue
               IF (iErr /= 0)     THEN
                  doublePrecisionValue = IEEE_VALUE(doublePrecisionValue,IEEE_QUIET_NAN)
               END IF
            CASE (FTVALUECLASS_LOGICAL)
               l = TRANSFER(self % valueStorage, l)
               IF ( l )     THEN
                  doublePrecisionValue = 1.0d0
               ELSE
                  doublePrecisionValue = 0.0d0
               END IF
         END SELECT
         
      END FUNCTION doublePrecisionValue   
!
!---------------------------------------------------------------------------
!> Get the double precision value stored in the object, or convert the value
!> in the object to a double precision if it is of a different type.
!---------------------------------------------------------------------------
!
!////////////////////////////////////////////////////////////////////////
!
#ifdef _has_Quad
      DOUBLE PRECISION FUNCTION quadValue(self)
         IMPLICIT NONE 
         CLASS(FTValue)  :: self
         INTEGER         :: iErr
         
         REAL                                  :: r
         INTEGER                               :: i
         LOGICAL                               :: l
         CHARACTER(LEN= FTVALUE_STRING_LENGTH) :: s, tmpString
         
         SELECT CASE (self % valueType)
            CASE (FTVALUECLASS_INTEGER)
               i                    = TRANSFER(self % valueStorage, i)
               quadValue = REAL(A = i, KIND = SELECTED_REAL_KIND(QUAD_DIGITS))
            CASE (FTVALUECLASS_REAL)
               r                    = TRANSFER(self % valueStorage, r)
               quadValue = REAL(A = r, KIND = SELECTED_REAL_KIND(QUAD_DIGITS))
            CASE (FTVALUECLASS_DOUBLE)
                quadValue = TRANSFER(self % valueStorage, quadValue)
            CASE (FTVALUECLASS_STRING)
               tmpString = TRANSFER(self % valueStorage, tmpString)
               s         = tmpString(1:SIZE(self % valueStorage))
               READ(s,*,IOSTAT = iErr) quadValue
               IF (iErr /= 0)     THEN
                  quadValue = IEEE_VALUE(quadValue,IEEE_QUIET_NAN)
               END IF
            CASE (FTVALUECLASS_LOGICAL)
               l = TRANSFER(self % valueStorage, l)
               IF ( l )     THEN
                  quadValue = 1.0
               ELSE
                  quadValue = 0.0
               END IF
         END SELECT
         
      END FUNCTION quadValue 
#endif  
!
!---------------------------------------------------------------------------
!> Get the integer value stored in the object, or convert the value
!> in the object to an integer if it is of a different type.
!---------------------------------------------------------------------------
!
!////////////////////////////////////////////////////////////////////////
!
      INTEGER FUNCTION integerValue(self)
         IMPLICIT NONE 
         CLASS(FTValue)  :: self
         INTEGER         :: iErr
         
         REAL                                  :: r
         INTEGER                               :: i
         DOUBLE PRECISION                      :: d
         LOGICAL                               :: l
         CHARACTER(LEN= FTVALUE_STRING_LENGTH) :: s, tmpString
         
         SELECT CASE (self % valueType)
            CASE (FTVALUECLASS_INTEGER)
               integerValue = TRANSFER(self % valueStorage, i)
            CASE (FTVALUECLASS_DOUBLE)
               d            = TRANSFER(self % valueStorage, d)
               integerValue = INT(d)
            CASE (FTVALUECLASS_REAL)
               r            = TRANSFER(self % valueStorage, r)
               integerValue = INT(r)
            CASE (FTVALUECLASS_STRING)
               tmpString = TRANSFER(self % valueStorage, tmpString)
               s         = tmpString(1:SIZE(self % valueStorage))
               READ(s,*,IOSTAT = iErr) integerValue
               IF (iErr /= 0)     THEN
                  integerValue = HUGE(1)
               END IF
            CASE (FTVALUECLASS_LOGICAL)
               l = TRANSFER(self % valueStorage, l)
               IF ( l )     THEN
                  integerValue = 1
               ELSE
                  integerValue = 0
               END IF
         END SELECT
         
      END FUNCTION integerValue   
!
!---------------------------------------------------------------------------
!> Get the logical value stored in the object, or convert the value
!> in the object to a logical if it is of a different type.
!---------------------------------------------------------------------------
!
!////////////////////////////////////////////////////////////////////////
!
      LOGICAL FUNCTION logicalValue(self)
         IMPLICIT NONE 
         CLASS(FTValue)  :: self
         
         REAL                                  :: r
         INTEGER                               :: i
         DOUBLE PRECISION                      :: d
         LOGICAL                               :: l
         CHARACTER(LEN= FTVALUE_STRING_LENGTH) :: s, tmpString
         
         SELECT CASE (self % valueType)
            CASE (FTVALUECLASS_INTEGER)
               i = TRANSFER(self % valueStorage, i)
               IF ( i /= 0 )     THEN
                  logicalValue = .true.
               ELSE
                  logicalValue = .false.
               END IF
            CASE (FTVALUECLASS_DOUBLE)
               d = TRANSFER(self % valueStorage, d)
               IF ( d /= 0.0d0 )     THEN
                  logicalValue = .true.
               ELSE
                  logicalValue = .false.
               END IF
            CASE (FTVALUECLASS_REAL)
               r = TRANSFER(self % valueStorage, r)
               IF ( r /= 0.0 )     THEN
                  logicalValue = .true.
               ELSE
                  logicalValue = .false.
               END IF
            CASE (FTVALUECLASS_STRING)
               tmpString = TRANSFER(self % valueStorage, tmpString)
               s         = tmpString(1:SIZE(self % valueStorage))
               IF ( TRIM(s) == ".true." .OR. TRIM(s) == ".false." .OR. &
                    TRIM(s) == ".TRUE." .OR. TRIM(s) == ".FALSE.")     THEN
                  READ(s,*) logicalValue
               ELSE
                  logicalValue = .false.
               END IF
            CASE (FTVALUECLASS_LOGICAL)
               logicalValue = TRANSFER(self % valueStorage, l)
         END SELECT
         
      END FUNCTION logicalValue   
!
!---------------------------------------------------------------------------
!> Get the string value stored in the object, or convert the value
!> in the object to a string of that length if it is of a different type.
!> It is overloaded as stringValue().
!---------------------------------------------------------------------------
!
!////////////////////////////////////////////////////////////////////////
!
      FUNCTION stringValueA(self) RESULT(s)
         IMPLICIT NONE 
         CLASS(FTValue)                 :: self
         CHARACTER(LEN=:), ALLOCATABLE  :: s
 
         CHARACTER(LEN= FTVALUE_STRING_LENGTH) :: tmpString
         
         REAL                                  :: r
         INTEGER                               :: i
         DOUBLE PRECISION                      :: d
         LOGICAL                               :: l

          SELECT CASE (self % valueType)
            CASE (FTVALUECLASS_INTEGER)
               i = TRANSFER(self % valueStorage, i)
               WRITE(tmpString,*) i
               s = TRIM(ADJUSTL(tmpString))
           CASE (FTVALUECLASS_DOUBLE)
               d = TRANSFER( self % valueStorage, d)
               WRITE(tmpString,*) d
               s = TRIM(ADJUSTL(tmpString))
            CASE (FTVALUECLASS_REAL)
               r = TRANSFER(self % valueStorage, r)
               WRITE(tmpString,*) r
               s = TRIM(ADJUSTL(tmpString))
            CASE (FTVALUECLASS_STRING)
               tmpString = TRANSFER(self % valueStorage, tmpString)
               s         = tmpString(1:SIZE(self % valueStorage))
            CASE (FTVALUECLASS_LOGICAL)
               l = TRANSFER(self % valueStorage, l)
               IF ( l )     THEN
                  s = ".true."
               ELSE
                  s = ".false."
               END IF
         END SELECT
         
      END FUNCTION stringValueA
!
!---------------------------------------------------------------------------
!> Get the string value of length requestedLength stored in the object, or 
!> convert the value in the object to a string of that length if it is of a 
!> different type. This version is deprecated in favor of the stringValueA
!> function that uses allocated strings. This version is included only so
!> that other code that uses this library doesn't break. It is overloaded
!> as stringValue(requestedLength).
!---------------------------------------------------------------------------
!
!////////////////////////////////////////////////////////////////////////
!
      FUNCTION stringValueR(self,requestedLength) RESULT(s)
         IMPLICIT NONE 
         CLASS(FTValue)                 :: self
         INTEGER                        :: requestedLength
         CHARACTER(LEN=requestedLength) :: s
 
         CHARACTER(LEN= FTVALUE_STRING_LENGTH) :: tmpString
         
         REAL                                  :: r
         INTEGER                               :: i
         DOUBLE PRECISION                      :: d
         LOGICAL                               :: l

          SELECT CASE (self % valueType)
            CASE (FTVALUECLASS_INTEGER)
               i = TRANSFER(self % valueStorage, i)
               WRITE(tmpString,*) i
               s = TRIM(ADJUSTL(tmpString))
           CASE (FTVALUECLASS_DOUBLE)
               d = TRANSFER( self % valueStorage, d)
               WRITE(tmpString,*) d
               s = TRIM(ADJUSTL(tmpString))
            CASE (FTVALUECLASS_REAL)
               r = TRANSFER(self % valueStorage, r)
               WRITE(tmpString,*) r
               s = TRIM(ADJUSTL(tmpString))
            CASE (FTVALUECLASS_STRING)
               tmpString = TRANSFER(self % valueStorage, tmpString)
               s         = tmpString(1:SIZE(self % valueStorage))
            CASE (FTVALUECLASS_LOGICAL)
               l = TRANSFER(self % valueStorage, l)
               IF ( l )     THEN
                  s = ".true."
               ELSE
                  s = ".false."
               END IF
         END SELECT
         
      END FUNCTION stringValueR   
!@mark -
!
!---------------------------------------------------------------------------
!> Returns the description of the value. In this case, it returns the 
!> stringValue() of the object. 
!---------------------------------------------------------------------------
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION FTValueDescription(self)  
         IMPLICIT NONE  
         CLASS(FTValue)      :: self
         CHARACTER(LEN=DESCRIPTION_CHARACTER_LENGTH) :: FTValueDescription
         
         FTValueDescription =  self % stringValue()
         
      END FUNCTION FTValueDescription
!
!---------------------------------------------------------------------------
!> Prints the description of the value to unit iUnit. In this case, it prints  
!> the stringValue() of the object. 
!---------------------------------------------------------------------------
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE printValueDescription(self,iUnit)  
         IMPLICIT NONE 
         CLASS(FTValue) :: self
         INTEGER        :: iUnit
         
         WRITE(iUnit,*) TRIM(self % description())

      END SUBROUTINE printValueDescription    
!
!---------------------------------------------------------------------------
!> Generic Name: cast
!> 
!> Cast a pointer to the base class to an FTValue pointer 
!---------------------------------------------------------------------------
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE castToValue(obj,cast) 
!
!     -----------------------------------------------------
!     Cast the base class FTObject to the FTValue class
!     -----------------------------------------------------
!
         IMPLICIT NONE  
         CLASS(FTObject), POINTER :: obj
         CLASS(FTValue) , POINTER :: cast
         
         cast => NULL()
         SELECT TYPE (e => obj)
            TYPE is (FTValue)
               cast => e
            CLASS DEFAULT
               
         END SELECT
         
      END SUBROUTINE castToValue
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION valueFromObject(obj) RESULT(cast)
!
!     -----------------------------------------------------
!     Cast the base class FTObject to the FTValue class
!     -----------------------------------------------------
!
         IMPLICIT NONE  
         CLASS(FTObject), POINTER :: obj
         CLASS(FTValue) , POINTER :: cast
         
         cast => NULL()
         SELECT TYPE (e => obj)
            TYPE is (FTValue)
               cast => e
            CLASS DEFAULT
               
         END SELECT
         
      END FUNCTION valueFromObject
!
!//////////////////////////////////////////////////////////////////////// 
! 
!      -----------------------------------------------------------------
!> Class name returns a string with the name of the type of the object
!>
!>  ### Usage:
!>
!>        PRINT *,  obj % className()
!>        if( obj % className = "FTValue")
!>
      FUNCTION valueClassName(self)  RESULT(s)
         IMPLICIT NONE  
         CLASS(FTValue)                             :: self
         CHARACTER(LEN=CLASS_NAME_CHARACTER_LENGTH) :: s
         
         s = "FTValue"
         IF(self % refCount() >= 0)     CONTINUE 
      END FUNCTION valueClassName

      END MODULE FTValueClass   
