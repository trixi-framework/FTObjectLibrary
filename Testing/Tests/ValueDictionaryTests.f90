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
         
         TYPE(FTValueDictionary)      :: dict, dict2
!
!        -----------------------
!        Example values and keys
!        -----------------------
!
         CHARACTER(LEN=FTDICT_KWD_STRING_LENGTH), DIMENSION(4) :: keys         = ["first ","second","third ","fourth"]
         INTEGER                                , DIMENSION(4) :: intValues    = [1,2,3,4]
         REAL                                   , DIMENSION(4) :: realValues   = [1.1, 2.1, 3.1, 4.1]
         CHARACTER(LEN=FTDICT_KWD_STRING_LENGTH), DIMENSION(4) :: stringValues = ['1', '2', '3', '4']
         INTEGER                                               :: i, s
         REAL                                                  :: x
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
            sValue = dict % stringValueForKey(keys(i),8)
            CALL FTAssertEqual(sValue,stringValues(i),"Value for key as string ")
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
            CALL FTAssertEqual(x,realValues(i),2*EPSILON(x),"Value for key as real ")
         END DO
!
!        -----------------------
!        Check superclass method
!        -----------------------
!
         CALL FTAssert( dict2 % containsKey("first")   ,msg = "dictionary contains key")
         CALL FTAssert(.NOT. dict2 % containsKey("bob"),msg = "dictionary does not contain key")         
         
      END SUBROUTINE FTValueDictionaryClassTests    
