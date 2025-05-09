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
!      FTValueClassTests.f90
!      Created: January 15, 2013 5:13 PM 
!      By: David Kopriva  
!
!     Demonstrate and test the components of 
!     the FTValue class. An FTValue is a wrapper
!     to a
!         double precision
!         real
!         logical
!         string of length FTVALUE_STRING_LENGTH
!         integer
!         
!     These values can then be stored in one of the collection
!     classes.
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE FTValueClassTests
         USE FTValueClass
         USE FTAssertions
         IMPLICIT NONE 
!
!        -------------------------------------------------------
!        FTValue is the class to test here and the instance is v
!        It will be a pointer if it is to be added to a 
!        container.
!        -------------------------------------------------------
!
         TYPE(FTValue)  , POINTER :: v
         CLASS(FTObject), POINTER :: obj
         CLASS(FTValue) , POINTER :: vFromObj
!
!        -------------------------------------------
!        Some values to convert into FTValue objects
!        -------------------------------------------
!
         REAL                                          :: r = 3.14, x
         REAL(KIND=KIND(1.0d0))                        :: d, dd
         INTEGER                                       :: i = 666, j
         DOUBLE PRECISION                              :: doubleTol = 2*EPSILON(1.0d0), y
         REAL                                          :: singleTol = 2*EPSILON(1.0e0)
         CHARACTER(LEN=DESCRIPTION_CHARACTER_LENGTH)   :: s
         LOGICAL                                       :: logicVal, lArray(2) = [.false., .true.]
         INTEGER                                       :: logicalToInt(2)     = [0,1]
         REAL                                          :: logicalToReal(2)    = [0.0, 1.0]
         DOUBLE PRECISION                              :: logicalToDbl(2)     = [0.0d0, 1.0d0]
         CHARACTER(LEN = 7)                            :: logicalToStr(2)     = [".false.", ".true. "]
!
!        --------------------------------------------
!        Create an object storing a real value
!        init*** returns an FTObject with ownership
!        given to this calling routine (refCount =1).
!        --------------------------------------------
!
         ALLOCATE(v)
         CALL v % initWithValue(r)

!        -----------------------
!        Make sure it is a value
!        -----------------------
!
         CALL FTAssertEqual(expectedValue = v % className(), &
                            actualValue   = "FTValue",  &
                            msg           = "Class name for FTValue")
!
!        ----------------------------------------------------------------------------------
!        Description returns a string that represents an object. Here it is just 
!        the number represented as a string, but for more complex objects like linked lists
!        it could be arbitrarily complicated
!        ----------------------------------------------------------------------------------
!
         s = v % description()
!
!        -----------------------------
!        Test to make sure it is right
!        -----------------------------
!
         CALL FTAssertEqual("3.140000",s(1:8),"Compare description for real value")
!
!        --------------------------
!        Also test the string value
!        --------------------------
!
         s = v % stringValue()
         CALL FTAssertEqual("3.140000",s(1:8),"Compare string value for real value using allocated string")
         s = v % stringValue(8)
         CALL FTAssertEqual("3.140000",s(1:8),"Compare string value for real value using requestedLength string")

!
!        --------------------------------------------------------------------------
!        Test Reference counting.
!           Retaining an object implies that this routine wants to
!           share ownership of an object. (In fact, it already does, so the retain
!           is redundant.)
!           Releasing an object implies that the caller releases its share of
!           the object. Note that, because of the init call, we still own the
!           object after the retain+release.
!        --------------------------------------------------------------------------
!
         CALL FTAssertEqual(1,v % refCount(),"Reference counting: Initial object refCount")
         CALL v % retain()
         CALL FTAssertEqual(2,v % refCount(),"Reference counting: Retain count increase")
         CALL releaseFTValue(v)
         CALL FTAssertEqual(1,v % refCount(),"Reference counting: retain count decrease")
!
!        -----------------------------------------------------------------
!        Test storage of the real value. An FTObject can return any one of 
!        the basic variable types, appropriately modified. Make sure that
!        each is correct.
!        ----------------------------------------------------------------
!
         CALL FTAssertEqual(3.14,v % realValue(),singleTol,msg="Real storage to real")
         CALL FTAssertEqual(3,v % integerValue(),"Integer return for real object")
         CALL FTAssertEqual(DBLE(3.14),v % doublePrecisionValue(),doubleTol,msg="Double return for real object")
         s = v % stringValue()
         CALL FTAssertEqual("3.140000",s(1:8),"String return for real object")
         CALL FTAssertEqual(.true.,v % logicalValue(),"Logical return for real object")
!
!        ----------------------------------------------------------------
!        Destruction.
!        ----------------------------------------------------------------
!
         CALL releaseFTValue(v)
         CALL FTAssert(test = .NOT.ASSOCIATED(v),msg = "Final release deletes object 1")
!
!        --------------------------------------
!        Now do the same with an integer number
!        --------------------------------------
!
         ALLOCATE(v)
         CALL v % initWithValue(i)
         CALL FTAssert(.NOT. v % isUnreferenced(),msg = "Referenced object should return false for isUnreferenced")
         
         CALL FTAssertEqual(666.0,v % realValue(),singleTol,msg="Integer storage to real")
         CALL FTAssertEqual(666,v % integerValue(),"Integer storage to integer")
         CALL FTAssertEqual(DBLE(666.0),v % doublePrecisionValue(),doubleTol,msg="Integer storage to double")
         CALL FTAssertEqual("666",v % stringValue(),"Integer storage to string")
         CALL FTAssertEqual(.true.,v % logicalValue(),"Integer storage to logical")
!
!        ------------------------------------------
!        We are done with this value, so release it
!        ------------------------------------------
!
         CALL releaseFTValue(v)
         CALL FTAssert(test = .NOT.ASSOCIATED(v),msg = "Final release deletes object 2")
!
!        -------------------------------
!        Store a double precision number
!        -------------------------------
!
         d = 1.0d0/3.0d0
         ALLOCATE(v)
         CALL v % initWithValue(d)
         
         CALL FTAssertEqual(REAL(d),v % realValue(),singleTol,msg="Double storage to real")
         CALL FTAssertEqual(0,v % integerValue(),"Double storage to integer")
         CALL FTAssertEqual(d,v % doublePrecisionValue(),doubleTol,msg="Double storage to double")
         s = v % stringValue()
         CALL FTAssertEqual("0.33333333333333",s(1:16),"Double storage to string")
         CALL FTAssertEqual(.true.,v % logicalValue(),"Double storage to logical")
!
!        -----------------------------------------------
!        We are done with this value, too, so release it
!        -----------------------------------------------
!
         CALL releaseFTValue(v)
         CALL FTAssert(test = .NOT.ASSOCIATED(v),msg = "Final release deletes object 3")
!
!        -------------------------------------------------------
!        Save a string and read it as numeric and logical values
!        -------------------------------------------------------
!
         ALLOCATE(v)
         CALL v % initWithValue("3.14")
         x = v % realValue()
         CALL FTAssertEqual(3.14e0,x,singleTol,msg="String storage to real")
         CALL FTAssert(.NOT. v % logicalValue(),msg = "String not logical should be false")
         CALL releaseFTValue(v)
         CALL FTAssert(test = .NOT.ASSOCIATED(v),msg = "Final release deletes object 4")

         ALLOCATE(v)
         CALL v % initWithValue("3567")
         j = v % integerValue()
         CALL FTAssertEqual(3567,j,"String storage to integer")
         CALL releaseFTValue(v)
         CALL FTAssert(test = .NOT.ASSOCIATED(v),msg = "Final release deletes object 5")
         
         ALLOCATE(v)
         CALL v % initWithValue("3.141592653589793")
         dd = v % doublePrecisionValue()
         CALL FTAssertEqual(3.141592653589793d0,dd,doubleTol,msg="String storage to real")
         CALL releaseFTValue(v)
         CALL FTAssert(test = .NOT.ASSOCIATED(v),msg = "Final release deletes object 6")
         
         ALLOCATE(v)
         CALL v % initWithValue(".true.")
         CALL FTAssert(v % logicalValue(),msg = ".true. converted to logical")
         CALL releaseFTValue(v)
         
         DO j = 1,2 
            ALLOCATE(v)
            CALL v % initWithValue(lArray(j))
            CALL FTAssertEqual(expectedValue = lArray(j), &
                               actualValue   = v % logicalValue(), &
                               msg           = "matching logical input.")
            CALL FTAssertEqual(expectedValue = logicalToInt(j), &
                               actualValue = v % integerValue(), &
                               msg = "Integer value from logical:"// logicalToStr(j))
            CALL FTAssertEqual(expectedValue = logicalToStr(j),                    &
                               actualValue = v % stringValue(), &
                               msg = "String value from logical:"// logicalToStr(j))
            CALL FTAssertEqual(expectedValue = logicalToReal(j),                    &
                               actualValue = v % realValue(),                       &
                               relTol      = 2.0*EPSILON(x),                        &
                               msg = "Real value from logical "// logicalToStr(j))
            CALL FTAssertEqual(expectedValue = logicalToDbl(j),                     &
                               actualValue = v % doublePrecisionValue(),            &
                               relTol      = 2.0*EPSILON(d),                        &
                               msg = "Double value from logical "// logicalToStr(j))
            CALL releaseFTValue(v)
         END DO
!
!        -------------------
!        Test absolute error
!        -------------------
!
         CALL FTAssertEqual(expectedValue = 1.01d-8,    &
                            actualValue   = 1.0d-8,     &
                            relTol        = 0.0d0,      &
                            absTol        = 1.0d-4,     &
                            msg = "Absolute Error test")
         CALL FTAssertEqual(expectedValue = 1.01d-8,    &
                            actualValue   = 1.0d-8,     &
                            relTol        = 1.0d-4,     &
                            absTol        = 1.0d-4,     &
                            msg = "Absolute Error test")
!
!        -----------------------
!        Test casting of objects
!        -----------------------
!
         ALLOCATE(v)
         CALL v % initWithValue("stringValue")
         obj => v
         
         CALL castToValue(obj, vFromObj)
         CALL FTAssert(ASSOCIATED(vFromObj),msg = "Cast value from object as subroutine")
         CALL FTAssertEqual(expectedValue = "stringValue", &
                            actualValue   = vFromObj % stringValue(), &
                            msg           = "Check that cast is correct")
!
!        ---------------
!        Test bad values
!        ---------------
!
         x = v % realValue()
         CALL FTAssert(test = IEEE_IS_NAN(x),msg = "Real value from non number string is NAN")
 
         y = v % doublePrecisionValue()
         CALL FTAssert(test = IEEE_IS_NAN(y),msg = "double value from non number string is NAN")
        
         i = v % integerValue()
         CALL FTAssertEqual(expectedValue = HUGE(1), &
                            actualValue   = I,         &
                            msg           = "Non-integer string conversion")

         logicVal = v % logicalValue()
         CALL FTAssert(.NOT. logicVal,msg = "Logic value of non logical string is false.")
         CALL releaseFTValue(v)
         
      END SUBROUTINE FTValueClassTests   
