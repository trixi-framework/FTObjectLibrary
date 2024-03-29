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
!      StringSetTests.f90
!      Created: March 9, 2020 at 10:20 AM 
!      By: David Kopriva  
!
!////////////////////////////////////////////////////////////////////////
!
!
!////////////////////////////////////////////////////////////////////////
!
   SUBROUTINE FTStringSetTests
      USE FTStringSetClass
      USE FTAssertions
      IMPLICIT NONE 
      
      TYPE(FTStringSet)           :: set1, set2
      TYPE(FTStringSet) , POINTER :: intersectionSet, differenceSet, setPtr
      CLASS(FTStringSet), POINTER :: unionSet
      CHARACTER(LEN=5)            :: s1(5) = ['one  ','two  ','three','four ','five ']
      CHARACTER(LEN=5)            :: s2(5) = ['one  ','two  ','six  ','seven','eight']
      CHARACTER(LEN=5)            :: s3(6) = ['one  ','two  ','three','four ','five ','six  ']
      CHARACTER(LEN=5)            :: unionStrings(8) = ['one  ','two  ','three','four ','five ','six  ','seven','eight']
      CHARACTER(LEN=5)            :: intersectionStrings(2) = ['one  ','two  ']
      CHARACTER(LEN=5)            :: differenceStrings1(3) = ['three','four ','five ']
      CHARACTER(LEN=5)            :: differenceStrings2(3) = ['six  ','seven','eight']
      INTEGER                     :: i
      LOGICAL                     :: test
      CLASS(FTObject), POINTER    :: obj

      CHARACTER(LEN=FTDICT_KWD_STRING_LENGTH) ,DIMENSION(:), POINTER :: keys
!
!     -------------------------------------
!     Test size of set after initialization
!     -------------------------------------
!
      CALL set1 % initWithStrings(strings = s1)
      CALL FTAssertEqual(expectedValue = SIZE(s1),     &
                         actualValue   =  set1 % COUNT(), &
                         msg           = "First set count")
      CALL FTAssert(test = .NOT.set1 % isEmpty(),msg = 'Test non empty test for empty')
      CALL FTAssertEqual(expectedValue = "FTStringSet",      &
                         actualValue   = set1 % className(), &
                         msg           = "Class name should be string set")
!
!     -------------------------------------------
!     Test size of string array returned from set
!     -------------------------------------------
!
      keys => set1 % strings()
      CALL FTAssertEqual(expectedValue = SIZE(s1),actualValue = SIZE(keys),msg = "Returned strings size count")
!
!     -------------------------------
!     Test all strings are in the set
!     -------------------------------
!
      test = stringIsInArray('one  ',s1)
      CALL FTAssert(test = test,msg = 'StringIsInArray test')
      DO i = 1, SIZE(s1) 
         CALL FTAssert(test = stringIsInArray(s1(i),keys),msg = "Set returned strings includes original strings") 
         CALL FTAssert(test = set1 % containsString(str = s1(i)),msg = "Set1 contains string of original array")
      END DO 
      DEALLOCATE(keys)
      
      CALL set2 % initWithStrings(strings = s2)
      CALL FTAssertEqual(expectedValue = SIZE(s2),     &
                         actualValue =  set2 % COUNT(), &
                         msg = "Second set count")
!
!     -------------------------
!     Check for FALSE positives
!     -------------------------
!
      CALL FTAssert(test = .NOT.set1 % containsString(str = 'blork'),msg = "Set1 should not contain string")
!
!     --------------------------
!     Check for case sensitivity
!     --------------------------
!
      CALL FTAssert(test = .NOT.set1 % containsString(str = 'ONE'),msg = 'Case sensitivity')
!
!     -------------------------------------------
!     Test size of string array returned from set
!     -------------------------------------------
!
      keys => set2 % strings()
      CALL FTAssertEqual(expectedValue = SIZE(s2),actualValue = SIZE(keys),msg = "Returned strings size count")
!
!     -------------------------------
!     Test all strings are in the set
!     -------------------------------
!
      test = stringIsInArray('eight',s2)
      CALL FTAssert(test = test,msg = 'StringIsInArray test set 2')
      DO i = 1, SIZE(s2) 
         CALL FTAssert(test = stringIsInArray(s2(i),keys),msg = "Set returned strings includes original strings") 
         CALL FTAssert(test = set2 % containsString(str = s2(i)),msg = "Set2 contains string of original array")
      END DO 
      DEALLOCATE(keys)
!
!     ------------------
!     Test union of sets
!     ------------------
!
      unionSet => set1 % unionWithSet(set = set2)
      CALL FTAssertEqual(expectedValue = SIZE(unionStrings),&
                         actualValue   = unionSet % COUNT(), &
                         msg           = "union set count")
      DO i = 1, SIZE(unionStrings) 
         CALL FTAssert(test = unionSet % containsString(str = unionStrings(i)),msg = "unionSet contains string of original array")
      END DO 
!
!     ------------------------
!     Test casting of pointers
!     ------------------------
!
      obj => unionSet
      setPtr => FTStringSetFromObject(obj)
      CALL FTAssertEqual(expectedValue = SIZE(unionStrings),&
                         actualValue   = unionSet % COUNT(), &
                         msg           = "union set count")
      
      CALL releaseFTStringSet(self = unionSet)
      CALL FTAssert(test = .NOT.ASSOCIATED(unionSet),msg = 'Release of pointer to set')
!
!     ----------------------
!     Test set intersections
!     ----------------------
!
      intersectionSet => set1 % intersectionWithSet(set = set2)
      CALL FTAssert(test = ASSOCIATED(intersectionSet),msg = 'Intersection pointer association')
      CALL FTAssertEqual(expectedValue = SIZE(intersectionStrings), &
                         actualValue = intersectionSet % count(), &
                         msg = 'Intersection size')
      DO i = 1, SIZE(intersectionStrings) 
         CALL FTAssert(test = intersectionSet % containsString(str = intersectionStrings(i)),msg = "Set contains intersection")
      END DO 
      CALL releaseFTStringSet(self = intersectionSet)
!
!     --------------------------------
!     Test difference between two sets
!     --------------------------------
!
      differenceSet => set1 % setFromDifference(set = set2)
      CALL FTAssert(test = ASSOCIATED(differenceSet),msg = 'Intersection pointer association')
      CALL FTAssertEqual(expectedValue = SIZE(differenceStrings1), &
                         actualValue = differenceSet % count(), &
                         msg = 'Difference set1 size')
      DO i = 1, SIZE(differenceStrings1) 
         CALL FTAssert(test = differenceSet % containsString(str = differenceStrings1(i)),msg = "Set 1 contains difference")
      END DO 
      CALL releaseFTStringSet(self = differenceSet)
      
      differenceSet => set2 % setFromDifference(set = set1)
      CALL FTAssert(test = ASSOCIATED(differenceSet),msg = 'Intersection pointer association')
      CALL FTAssertEqual(expectedValue = SIZE(differenceStrings2), &
                         actualValue = differenceSet % count(), &
                         msg = 'Difference set2 size')
      DO i = 1, SIZE(differenceStrings2) 
         CALL FTAssert(test = differenceSet % containsString(str = differenceStrings2(i)),msg = "Set 2 contains difference")
      END DO 
      CALL releaseFTStringSet(self = differenceSet)
!
!     --------------------
!     Test adding to a set
!     --------------------
!
      CALL set1 % addString(str = 'six')
      CALL FTAssertEqual(expectedValue = SIZE(s3),actualValue = set1 % count(),msg = 'Size of set after add')
      DO i = 1, SIZE(s3) 
         CALL FTAssert(test = set1 % containsString(str = s3(i)),msg = "Appended set contains new string")
      END DO
!
!    ========
     CONTAINS 
!    ========
      LOGICAL FUNCTION stringIsInArray(str,array)  
         IMPLICIT NONE  
         CHARACTER(LEN=*)               :: str
         CHARACTER(LEN=*) ,DIMENSION(:) :: array
         INTEGER                        :: i
         
         stringIsInArray = .FALSE.
         DO i = 1, SIZE(array) 
            IF ( TRIM(str) == TRIM(array(i)) )     THEN
               stringIsInArray = .TRUE.
               EXIT   
            END IF 
         END DO 
      
      END FUNCTION stringIsInArray

   END SUBROUTINE FTStringSetTests
