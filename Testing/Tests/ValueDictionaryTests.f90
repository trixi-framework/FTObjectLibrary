!
!////////////////////////////////////////////////////////////////////////
!
!      FTValueDicitonaryTests.f90
!      Created: February 6, 2013 9:41 AM 
!      By: David Kopriva  
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE FTValueDictionaryClassTests
         USE FTValueDictionaryClass
         USE FTAssertions
         IMPLICIT NONE  
         
         TYPE(FTValueDictionary)  :: dict
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
!        ------------------------------------------------------------
!        The dictionary is not a pointer, so we need only
!        to call release for it to release (and here, deallocate) its
!        objects
!        ------------------------------------------------------------
!
         CALL dict % release()
!
!        ---------------------
!        Redo with real values
!        ---------------------
!
         CALL dict % initWithSize(64)
!
!        -----------------------------------------
!        Add the keys and values to the dictionary
!        -----------------------------------------
!
         DO i = 1, 4
            CALL dict % addValueForKey(realValues(i),keys(i))
         END DO
!
!        ------------------
!        Get them back out 
!        ------------------
!
         DO i = 1,4
            x = dict % realValueForKey(keys(i))
            CALL FTAssertEqual(x,realValues(i),2*EPSILON(x),"Value for key as real ")
         END DO
!
!        ------------------------------------------------------------
!        The dictionary is not a pointer, so we need only
!        to call release for it to release (and here, deallocate) its
!        objects
!        ------------------------------------------------------------
!
         CALL dict % release()
         
         
      END SUBROUTINE FTValueDictionaryClassTests    