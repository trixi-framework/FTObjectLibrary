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
            TYPE (FTValueDictionary), POINTER :: userDictionary
            CLASS(FTDictionary)     , POINTER :: ptr
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
         
         TYPE (FTException) , POINTER :: exception
         
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
         
         CLASS(FTException)      , POINTER       :: e, ePtr
         CLASS(FTDictionary)     , POINTER       :: d
         CLASS(FTValueDictionary), POINTER       :: userDictionary
         CLASS(FTValue)          , POINTER       :: vGood, vBad
         CLASS(FTObject)         , POINTER       :: obj
         REAL                                    :: r
         CHARACTER(LEN=FTDICT_KWD_STRING_LENGTH) :: msg
         REAL                                    :: singleTol = 2*EPSILON(1.0e0)
!
!        -------------
!        Initial State
!        -------------
!
         CALL FTAssert(.NOT. catch(),msg = "Uninitiated error stack has no exceptions")
         CALL FTAssert(.NOT. catch("FTTestException"),msg = "Uninitiated error stack has no exceptions")
!
!        ------------------
!        Testing Exceptions
!        ------------------
!
         ALLOCATE(e)
         CALL e % initWarningException(msg = "Warning: Danger Will Robinson (Designed Uncaught)")
         CALL FTAssertEqual(expectedValue = FT_ERROR_WARNING, &
                            actualValue   = e % severity(),  &
                            msg           = "Warning error level match")
         CALL throw(e)
         CALL releaseFTException(e)
         
         ALLOCATE(e)
         CALL e % initFatalException(msg = "I'm Sorry, I can't do that Dave  (Designed Uncaught)")
         CALL FTAssertEqual(expectedValue = FT_ERROR_FATAL, &
                            actualValue   = e % severity(),  &
                            msg           = "Fatal error level match")
         CALL throw(e)
         CALL releaseFTException(e)

         ALLOCATE(e)
         ALLOCATE(vGood, vBad)
         CALL vGood % initWithValue(777)
         CALL vBad  % initWithValue(555)
         
         CALL e % initAssertionFailureException(msg                 = "TAF:Designed to fail!", &
                                                expectedValueObject = vGood, &
                                                ObservedValueObject = vBad,  &
                                                level               = FT_ERROR_WARNING)
         CALL releaseFTValue(vBad)
         CALL releaseFTValue(vGood)
         
         CALL FTAssertEqual(expectedValue = FT_ERROR_WARNING, &
                            actualValue    = e % severity(),  &
                            msg = "Assertion error level match")
                            
         d => e % infoDictionary()
         userDictionary => valueDictionaryFromDictionary(d)
         CALL FTAssert(ASSOCIATED(userDictionary), &
                       msg = "Conversion of dictionary to valueDictionary")
         
         CALL FTAssertEqual(expectedValue = "TAF:Designed to fail!", &
                            actualValue = userDictionary % stringValueForKey(key = "message",requestedLength = 21),&
                            msg = "Message set for assertion failure")
         
         CALL FTAssertEqual(expectedValue = 777, &
                            actualValue = userDictionary % integerValueForKey("expectedValue"),&
                            msg = "Expected value set for assertion failure")
         
         CALL FTAssertEqual(expectedValue = 555, &
                            actualValue = userDictionary % integerValueForKey("observedValue"),&
                            msg = "Observed value set for assertion failure")
         obj => e
         CALL cast(obj,ePtr)
         CALL FTAssert(ASSOCIATED(ePtr),msg = "Test casting of exception")
         ePtr => NULL()
         ePtr => exceptionFromObject(obj)
         CALL FTAssert(ASSOCIATED(ePtr),msg = "Test casting of exception by function")
         
         CALL throw(e)
         CALL releaseFTException(e)
!
!        -----------------------------
!        Testing the exception manager
!        -----------------------------
!         
         CALL initializeFTExceptions ! This call is redundant, because it is called by catch.
         
         CALL FTAssert(.NOT. catch("FTTestException"),msg = "Uninitiated error stack has no exceptions")
         CALL subroutineThatThrowsError
         
         CALL FTAssertEqual(expectedValue = 4, &
                            actualValue   = errorCount(), &
                            msg           = "Number of errors")
         CALL FTAssertEqual(expectedValue = FT_ERROR_FATAL, &
                            actualValue   = maximumErrorSeverity(), &
                            msg           = "Error severity")
         
         e => peekLastException()
         CALL FTAssert(ASSOCIATED(e),msg = "Peek at exception should be associated")
         
         IF ( catch("FTTestException") )     THEN
            e              => errorObject()
            d              => e % infoDictionary()
            userDictionary => valueDictionaryFromDictionary(dict = d)
            
            CALL FTAssertEqual(expectedValue = "FTException", &
                               actualValue = e % className(), &
                               msg = "Class name for FTException")
                               
            msg = userDictionary % stringValueForKey("message",FTDICT_KWD_STRING_LENGTH)
            CALL FTAssertEqual("An error has occurred",msg,"String for key: message")

            r   = userDictionary % realValueForKey("value")
            CALL FTAssertEqual(3.1416,r,singleTol,msg = "Value for key: value")
            
         END IF 
!
!        -------------
!        Throw and pop
!        -------------
!
         e => popLastException() ! Will be the assert exception thrown
         CALL FTAssertEqual(expectedValue = FT_ERROR_WARNING, &
                            actualValue    = e % severity(),  &
                            msg = "Popped error level match")
         CALL releaseFTException(e)
         
         CALL destructFTExceptions
         
      END SUBROUTINE FTExceptionClassTests   
