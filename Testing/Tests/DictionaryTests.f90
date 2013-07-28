!
!////////////////////////////////////////////////////////////////////////
!
!      DictionaryTests.f90
!      Created: January 29, 2013 9:37 AM 
!      By: David Kopriva  
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE FTDictionaryClassTests
         USE FTDictionaryClass
         USE FTValueClass
         USE FTAssertions
         IMPLICIT NONE  
         
         TYPE(FTDictionary)       :: dict
         CLASS(FTObject), POINTER :: obj
         CLASS(FTValue) , POINTER :: v
         
         CHARACTER(LEN=FTDICT_KWD_STRING_LENGTH), DIMENSION(4) :: keys   = ["first ","second","third ","fourth"]
         CHARACTER(LEN=FTDICT_KWD_STRING_LENGTH), DIMENSION(4) :: values = ["one  ","two  ","three","four "]
         CHARACTER(LEN=FTDICT_KWD_STRING_LENGTH)               :: s, msg
         INTEGER                                               :: i

         CALL dict%initWithSize(64)
!
!        -----------------------------------------
!        Add the keys and values to the dictionary
!        -----------------------------------------
!
         DO i = 1, 4
            ALLOCATE(v)
            CALL v%initWithValue(values(i))
            obj => v
            CALL dict%addObjectForKey(obj,keys(i))
            CALL v % release()
            CALL assertEqual(1,v%refCount(),"Reference Counting: Addition of object and release")
            CALL assertEqual(i,dict%count(),"Adding to dictionary object count")
         END DO
!
!        ------------------
!        Get them back out 
!        ------------------
!
         DO i = 1,4
            obj => dict%objectForKey(keys(i))
            CALL cast(obj,v)
            IF ( ASSOCIATED(v) )     THEN
               s = v%stringValue(FTDICT_KWD_STRING_LENGTH)
               CALL assertEqual(s,values(i),"Value for key")
            ELSE
               msg = "Value for key "//TRIM(values(i))// " not of correct type"
               CALL assert(.false.,msg)
            END IF 
         END DO
         
         CALL dict % release()
         
      END SUBROUTINE FTDictionaryClassTests    