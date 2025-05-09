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
!      FTValueDictionaryTests.f90
!      Created: February 6, 2013 9:41 AM 
!      By: David Kopriva  
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE FTValueDictionaryClassTests
         USE FTValueDictionaryClass
         USE FTAssertions
         IMPLICIT NONE  
         
         TYPE(FTValueDictionary)           :: dict, dict2
         CLASS(FTValueDictionary), POINTER :: dict3, valDict
         CLASS(FTDictionary)     , POINTER :: plainDict
         CLASS(FTObject)         , POINTER :: obj
!
!        -----------------------
!        Example values and keys
!        -----------------------
!
         CHARACTER(LEN=FTDICT_KWD_STRING_LENGTH), DIMENSION(4) :: keys         = ["first ","second","third ","fourth"]
         INTEGER                                , DIMENSION(4) :: intValues    = [1,2,3,4]
         REAL                                   , DIMENSION(4) :: realValues   = [1.1, 2.1, 3.1, 4.1]
         DOUBLE PRECISION                       , DIMENSION(4) :: dbleValues   = [1.1d0, 2.1d0, 3.1d0, 4.1d0]
         CHARACTER(LEN=FTDICT_KWD_STRING_LENGTH), DIMENSION(4) :: stringValues = ['1', '2', '3', '4']
         INTEGER                                               :: i, s
         REAL                                                  :: x,r
         DOUBLE PRECISION                                      :: xd
         LOGICAL                                               :: lgcal
         CHARACTER(LEN=FTDICT_KWD_STRING_LENGTH)               :: sValue
!
!        -------------------------------------------------------
!        Initialize the dictionary. We set it up with
!        64 elements. The size should be large enough
!        so that there are not a lot of name collisions, but not
!        so big as to use up massive amounts of memory. The size
!        should be a power of two. 
!        -------------------------------------------------------
!
         CALL dict % initWithSize(64)
!
!        ----------------------------------
!        Make sure it is a value dictionary
!        ----------------------------------
!
         CALL FTAssertEqual(expectedValue = dict % className(), &
                            actualValue   = "FTValueDictionary",  &
                            msg           = "Class name for FTValuedictionary")
!
!        -----------------------------------------
!        Add the keys and values to the dictionary
!        -----------------------------------------
!
         DO i = 1, 4
            CALL dict % addValueForKey(intValues(i),keys(i))
         END DO
!
!        ------------------
!        Get them back out 
!        ------------------
!
         DO i = 1,4
            s = dict % integerValueForKey(keys(i))
            CALL FTAssertEqual(s,intValues(i),"Value for key as integer ")
         END DO
!
!        -----------------------
!        Get them out as strings
!        -----------------------
!
         DO i = 1,4
            sValue = dict % stringValueForKey(keys(i))
            CALL FTAssertEqual(sValue,stringValues(i),"Value for key as string ")
         END DO
         DO i = 1,4
            sValue = dict % stringValueForKey(keys(i),6)
            CALL FTAssertEqual(sValue,stringValues(i),"Value for key as string ")
         END DO
!
!        ------------------------
!        Get them out as logicals
!        ------------------------
!
         DO i = 1,4
            lgcal = dict % logicalValueForKey(key = keys(i))
            CALL FTAssertEqual(lgcal,.TRUE.,"Value for key as logical ")
         END DO
!
!        ---------------------
!        Redo with real values
!        ---------------------
!
         CALL dict2 % initWithSize(64)
!
!        -----------------------------------------
!        Add the keys and values to the dictionary
!        -----------------------------------------
!
         DO i = 1, 4
            CALL dict2 % addValueForKey(realValues(i),keys(i))
         END DO
!
!        ------------------
!        Get them back out 
!        ------------------
!
         DO i = 1,4
            x = dict2 % realValueForKey(keys(i))
            CALL FTAssertEqual(x,realValues(i),2*EPSILON(x),msg="Value for key as real ")
         END DO
!
!        -------------------------------------
!        Get them back out as double precision
!        -------------------------------------
!
         DO i = 1,4
            xd = dict2 % doublePrecisionValueForKey(keys(i))
            CALL FTAssertEqual(xd,dbleValues(i),DBLE(2*EPSILON(x)),msg="Value for key as double ")
         END DO
!
!        -----------------------------
!        Add a logical and get  it out
!        -----------------------------
!
         lgcal = .FALSE.
         CALL dict2 % addValueForKey(lgcal,"logical")
         CALL FTAssert(test = .NOT.dict2 % logicalValueForKey("logical"), &
                                                         msg = "Add and get logical")
!
!        ---------------------------
!        Add a double and get it out
!        ---------------------------
!
         xd = 3.1d0
         CALL dict2 % addValueForKey(xd,"double")
         CALL FTAssertEqual(expectedValue = xd, &
                            actualValue   = dict2 % doublePrecisionValueForKey("double"), &
                            relTol        = 2*EPSILON(xd))
!
!        ----------------------------------
!        Getting something that's not there
!        ----------------------------------
!
         r = dict2 % realValueForKey("bologna")
         CALL FTAssertEqual(expectedValue = HUGE(r), &
                            actualValue   = r,       &
                            relTol        = 2*EPSILON(r))
         xd = dict2 % doublePrecisionValueForKey("bologna")
         CALL FTAssertEqual(expectedValue = HUGE(xd), &
                            actualValue   = xd,       &
                            relTol        = 2*EPSILON(xd))
         i = dict2 % integerValueForKey("bologna")
         CALL FTAssertEqual(expectedValue = HUGE(i), &
                            actualValue   = i)
         lgcal = dict2 % logicalValueForKey("bologna")
         CALL FTAssert(.NOT. lgcal,msg = "Retrieve nonexistent logical")
         sValue = dict2 % stringValueForKey("bologna")
         CALL FTAssertEqual(expectedValue = "", &
                            actualValue   = sValue)
!
!        -----------------------
!        Check superclass method
!        -----------------------
!
         CALL FTAssert( dict2 % containsKey("first")   ,msg = "dictionary contains key")
         CALL FTAssert(.NOT. dict2 % containsKey("bob"),msg = "dictionary does not contain key")
!
!        --------------------------
!        Some pointer casting tests
!        --------------------------
!
         ALLOCATE(dict3)
         CALL dict3 % init()
         CALL dict3 % addValueForKey("dict3","name")
         
         obj => dict3
         valDict => valueDictionaryFromObject(obj)
         CALL FTAssert(ASSOCIATED(valDict),msg = "Cast object to dictionary")
         
         valDict => NULL()
         CALL castObjectToValueDictionary(obj,valDict)
         CALL FTAssert(ASSOCIATED(valDict),msg = "Cast object to dictionary by alternate")
         
         valDict   => NULL()
         plainDict => dict3
         CALL FTAssert(ASSOCIATED(plainDict),msg = "Point dictionary to value dictionary")
         
         valDict => valueDictionaryFromDictionary(plainDict)
         CALL FTAssert(ASSOCIATED(valDict),msg = "Cast dictionary to valuedictionary")
         CALL FTAssert(valDict  % containsKey(key = "name"),msg = "Test integrity of casting")

         CALL castDictionaryToValueDictionary(plainDict,valDict)
         CALL FTAssert(ASSOCIATED(valDict),msg = "Cast dictionary to valuedictionary as subroutine call ")
         CALL FTAssert(valDict  % containsKey(key = "name"),msg = "Test integrity of casting")
         
         CALL releaseFTValueDictionary(dict3)
         CALL FTAssert(.NOT.ASSOCIATED(dict3),msg = "Release dictionary should deallocate")
         
         
      END SUBROUTINE FTValueDictionaryClassTests    
