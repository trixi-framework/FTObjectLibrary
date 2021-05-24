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
         
         TYPE (FTException) , POINTER :: exception
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
