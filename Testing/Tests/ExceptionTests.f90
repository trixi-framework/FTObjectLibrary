!
!////////////////////////////////////////////////////////////////////////
!
!      FTExceptionTests.f90
!      Created: February 6, 2013 6:16 PM 
!      By: NocturnalAviationSoftware  
!
!////////////////////////////////////////////////////////////////////////
! 
      MODULE ExceptionWrapper
         CONTAINS
         FUNCTION testException()
            USE FTExceptionClass
            USE FTValueDictionaryClass
            IMPLICIT NONE  
            CLASS(FTException)      , POINTER :: testException
            CLASS(FTValueDictionary), POINTER :: userDictionary
            CLASS(FTDictionary)     , POINTER :: ptr
            CLASS(FTObject)         , POINTER :: obj
            REAL                              :: r = 3.1416
            
            ALLOCATE(userDictionary)
            CALL userDictionary % initWithSize(64)
            CALL userDictionary % addValueForKey("An error has occurred","message")
            CALL userDictionary % addValueForKey(r,"value")
            
            ALLOCATE(testException)
            ptr => userDictionary
            CALL testException % initFTException(FT_ERROR_FATAL, &
                                 exceptionName   = "FTTestException", &
                                 infoDictionary  = ptr)
            CALL releaseFTValueDictionary(userDictionary)

         END FUNCTION testException
      END MODULE ExceptionWrapper 
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE subroutineThatThrowsError 
         USE SharedExceptionManagerModule
         USE FTExceptionClass
         USE ExceptionWrapper
         IMPLICIT NONE 
         
         CLASS(FTException) , POINTER :: exception
         CLASS(FTObject)    , POINTER :: obj
         
         exception => testException()
         CALL throw(exception)
         CALL releaseFTException(exception)
         
      END SUBROUTINE subroutineThatThrowsError    
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE FTExceptionClassTests
         USE FTValueDictionaryClass
         USE SharedExceptionManagerModule
         USE FTAssertions
         IMPLICIT NONE
         
         CLASS(FTException)      , POINTER       :: e
         CLASS(FTDictionary)     , POINTER       :: d
         CLASS(FTValueDictionary), POINTER       :: userDictionary
         REAL                                    :: r
         CHARACTER(LEN=FTDICT_KWD_STRING_LENGTH) :: msg
         REAL                                    :: singleTol = 2*EPSILON(1.0e0)

         CALL initializeFTExceptions
         CALL subroutineThatThrowsError
         
         IF ( catch("FTTestException") )     THEN
            e              => errorObject()
            d              => e % infoDictionary()
            userDictionary => valueDictionaryFromDictionary(dict = d)
            
            msg = userDictionary % stringValueForKey("message",FTDICT_KWD_STRING_LENGTH)
            CALL FTAssertEqual("An error has occurred",msg,"String for key: message")

            r   = userDictionary % realValueForKey("value")
            CALL FTAssertEqual(3.1416,r,singleTol,"Value for key: value")
         END IF 
         
         CALL destructFTExceptions
         
      END SUBROUTINE FTExceptionClassTests   
