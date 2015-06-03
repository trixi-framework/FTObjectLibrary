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
         
         TYPE(FTDictionary)                               :: dict
         CLASS(FTObject)                        , POINTER :: obj
         CLASS(FTValue)                         , POINTER :: v
         CLASS(FTMutableObjectArray)            , POINTER :: storedObjects
         CHARACTER(LEN=FTDICT_KWD_STRING_LENGTH), POINTER :: storedKeys(:)

         
         CHARACTER(LEN=FTDICT_KWD_STRING_LENGTH), DIMENSION(4) :: keys   = ["first ","second","third ","fourth"]
         CHARACTER(LEN=FTDICT_KWD_STRING_LENGTH), DIMENSION(4) :: values = ["one  ","two  ","three","four "]
         CHARACTER(LEN=FTDICT_KWD_STRING_LENGTH)               :: s, msg, storedKey, sExpected, sActual
         INTEGER                                               :: i

         CALL dict % initWithSize(64)
!
!        -----------------------------------------
!        Add the keys and values to the dictionary
!        -----------------------------------------
!
         DO i = 1, 4
            ALLOCATE(v)
            CALL v % initWithValue(values(i))
            obj => v
            CALL dict % addObjectForKey(obj,keys(i))
            CALL v % release()
            CALL FTAssertEqual(1,v % refCount(),"Reference Counting: Addition of object and release")
            CALL FTAssertEqual(i,dict % count(),"Adding to dictionary object count")
         END DO
!
!        ------------------
!        Get them back out 
!        ------------------
!
         DO i = 1,4
            obj => dict % objectForKey(keys(i))
            v   => valueFromObject(obj)
            IF ( ASSOCIATED(v) )     THEN
               s = v % stringValue(FTDICT_KWD_STRING_LENGTH)
               CALL FTAssertEqual(values(i),s,"Value for key in dictionary class")
            ELSE
               msg = "Value for key "//TRIM(values(i))// " not of correct type"
               CALL FTAssert(.false.,msg)
            END IF 
         END DO
!
!        -------------------------
!        Find the keys and objects
!        -------------------------
!
         storedKeys    => dict % AllKeys()
         storedObjects => dict % AllObjects()
         
         DO i = 1, 4
            storedKey = storedKeys(i)
            obj       => dict % objectForKey(storedKey)
            v         => valueFromObject(obj)
            sExpected = v % stringValue(FTDICT_KWD_STRING_LENGTH)
            
            obj      => storedObjects % objectAtIndex(indx = i)
            v        => valueFromObject(obj)
            sActual  =  v % stringValue(FTDICT_KWD_STRING_LENGTH)
            
            CALL FTAssertEqual(sExpected, sActual,"String for stored key")
         END DO   
!
!        ---------------
!        Clean up memory
!        ---------------
!
         DEALLOCATE(storedKeys)
         CALL storedObjects % release()
         IF ( storedObjects % isUnreferenced() )     THEN
            DEALLOCATE(storedObjects) 
         END IF 
         CALL dict % release()
         
      END SUBROUTINE FTDictionaryClassTests    