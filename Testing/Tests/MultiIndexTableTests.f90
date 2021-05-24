! MIT License

! Copyright (c) 2010-present David A. Kopriva and other contributors: AUTHORS.md

! Permission is hereby granted, free of charge, to any person obtaining a copy  
! of this software and associated documentation files (the "Software"), to deal  
! in the Software without restriction, including without limitation the rights  
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell  
! copies of the Software, and to permit persons to whom the Software is  
! furnished to do so, subject to the following conditions:

! The above copyright notice and this permission notice shall be included in all  
! copies or substantial portions of the Software.

! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR  
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE  
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER  
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,  
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE  
! SOFTWARE.

! FTObjectLibrary contains code that, to the best of our knowledge, has been released as
! public domain software:
! * `b3hs_hash_key_jenkins`: originally by Rich Townsend, 
! https://groups.google.com/forum/#!topic/comp.lang.fortran/RWoHZFt39ng, 2005
!
!////////////////////////////////////////////////////////////////////////
!
!      MultiIndexTableTests.f90
!      Created: May 27, 2015 at 5:46 PM 
!      By: David Kopriva  
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE MultiIndexTableTests
         USE FTAssertions
         USE FTObjectClass
         USE FTValueClass
         USE FTMultiIndexTableClass
         IMPLICIT NONE
         
         INTEGER                  :: e
         TYPE (FTValue) , POINTER :: v
         CLASS(FTObject), POINTER :: obj
         
         TYPE(FTMultiIndexTable) :: table
         INTEGER, DIMENSION(3)   :: unsorted3 = [5,3,1]
         INTEGER, DIMENSION(3)   :: sorted3   = [1,3,5]
         INTEGER, DIMENSION(4)   :: unsorted4 = [7,5,3,1]
         INTEGER, DIMENSION(4)   :: sorted4   = [1,3,5,7]
         INTEGER, DIMENSION(5)   :: unsorted5 = [9,7,5,3,1]
         INTEGER, DIMENSION(5)   :: sorted5   = [1,3,5,7,9]
         
         CHARACTER(LEN=5)        :: strs(4) = ["one  ", "two  ", "three", "four "]
         CHARACTER(LEN=5)        :: str
         INTEGER, DIMENSION(4,4) :: keys
         INTEGER                 :: j
         LOGICAL                 :: test
!
!        ----------------------
!        Test sorting procedure
!        ----------------------
!
         CALL sortKeysAscending(keys = unsorted3)
         e = MAXVAL(ABS(unsorted3 - sorted3))
         CALL FTAssertEqual(expectedValue = 0,actualValue = e,msg = "Sort of length three array")
         
         CALL sortKeysAscending(keys = unsorted4)
         e = MAXVAL(ABS(unsorted4 - sorted4))
         CALL FTAssertEqual(expectedValue = 0,actualValue = e,msg = "Sort of length four array")
         
         CALL sortKeysAscending(keys = unsorted5)
         e = MAXVAL(ABS(unsorted5 - sorted5))
         CALL FTAssertEqual(expectedValue = 0,actualValue = e,msg = "Sort of length five array")
!
!        -----------------
!        Create Test table
!        -----------------
!
         keys(:,1) = [1,3,5,6]
         keys(:,2) = [5,3,2,1]
         keys(:,3) = [3,6,8,9]
         keys(:,4) = [4,6,7,2]
         
         CALL table % initWithSize(N = 10) ! Says first item of the multiIndex array is at most 10
!
!        -----------------------
!        Add values to the table
!        -----------------------
!
         DO j = 1, 4
            ALLOCATE(v)
            CALL v % initWithValue(strs(j))
            obj => v
            CALL table % addObjectForKeys(obj = obj,keys = keys(:,j))
            CALL releaseFTValue(v)
         END DO
!
!        ---------------------------
!        Test if keys exist in table
!        ---------------------------
!
         DO j = 1, 4
            test = table % containsKeys(keys(:,j))
            CALL FTAssert(test, msg = "Keys found in table")
         END DO
!
!        -------------------------------
!        Retrieve objects from the table          
!        -------------------------------
!
         DO j = 1, 4
            v   => valueFromObject(table % objectForKeys(keys(:,j)))
            str = v % stringValue(5)
            CALL FTAssertEqual(expectedValue = strs(j),actualValue = str,msg = "object retrieval")
         END DO
         
      END SUBROUTINE MultiIndexTableTests
