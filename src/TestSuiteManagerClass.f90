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
!      TestSuiteModule.f90
!      Created: February 21, 2013 11:21 AM 
!      By: David Kopriva  
!
!> The TestSuiteManager class defines methods to easily
!> put together and run a suite of unit tests. 
!>
!>
!> The tests are managed by an instance of the
!>**TestSuiteManager** class. It is designed to be used with minimal fuss. You
!>
!>- Initialize the test suite
!>- Add test subroutines
!>- Have the testSuiteManager perform the tests
!>- Finalize the test suite manager
!>
!># Usage: #
!>
!>##Definition
!>
!>      TYPE(TestSuiteManager) :: testSuite
!>
!>##Initialization
!>         call testSuite % init()
!>
!>##Creating a test ###
!>
!>   A test is a subroutine with interface
!>
!>         ABSTRACT INTERFACE
!>            SUBROUTINE testSuiteSubroutine()
!>               CHARACTER(LEN=1), POINTER, OPTIONAL :: optData(:) 
!>            END SUBROUTINE testSuiteSubroutine
!>         END INTERFACE
!>   
!>   that (typically) includes unit test calls. You add
!>   a test suite function by the add subroutine
!>   
!>         CALL testSuite % addTestSubroutineWithName(SubroutineName, description)
!>
!>   where 
!>
!> - SubroutineName = a subroutine with the interface as above, and 
!> - description = a CHARACTER(LEN=128) character string that names the test
!>   
!> Alternately optional data acan be added to the call
!>
!>   CALL testSuite % addTestSubroutineWithName(SubroutineName, description, optData)
!>
!> where OptData is 
!>
!>     CHARACTER(LEN=1), POINTER :: optData(:)
!>
!> The optional data can contain anything if it is encoded using the TRANSFER function. This is a standard
!> trick in fortran.
!>
!>##Setting the output location ###
!>   Set the unit to which the output is written by
!>
!>         CALL testSuite % setOutputUnit(iUnit)
!>
!>##Running tests ###
!>   To run the tests call
!>
!>         CALL testSuite % performTests() OR
!>         CALL testSuite % performTests(numFailed)
!>   
!>##Finalizing the test suite ###
!>   When done, call
!>
!>         CALL finalizeSharedAssertionsManager

!
!////////////////////////////////////////////////////////////////////////
!
      Module TestSuiteManagerClass
      USE FTAssertions
      IMPLICIT NONE
      PRIVATE 
            
      ABSTRACT INTERFACE
         SUBROUTINE testSuiteFunction(optData)
            CHARACTER(LEN=1), POINTER, OPTIONAL :: optData(:) 
         END SUBROUTINE testSuiteFunction
      END INTERFACE

      TYPE TestCaseRecord
         LOGICAL                                       :: passed
         CHARACTER(LEN=128)                            :: testName
         TYPE(FTAssertionsManager)   , POINTER         :: assertionsManager
         PROCEDURE(testSuiteFunction), POINTER, NOPASS :: TestSubroutine
         CHARACTER(LEN=1), POINTER                     :: optData(:) 
         TYPE(TestCaseRecord), POINTER                 :: next
      END TYPE TestCaseRecord
      
      TYPE, PUBLIC :: TestSuiteManager
         INTEGER                       :: numberOfTests
         INTEGER                       :: stdOut = 6
         TYPE(TestCaseRecord), POINTER :: testCasesHead => NULL()
         TYPE(TestCaseRecord), POINTER :: testCasesTail => NULL()
         CONTAINS
         PROCEDURE :: init     => initializeTestSuiteManager
         FINAL     :: finalizeTestSuiteManager
         PROCEDURE :: addTestSubroutineWithName
         PROCEDURE :: performTests
         PROCEDURE :: setOutputUnit
      END TYPE TestSuiteManager
!
!     ========      
      CONTAINS
!     ========      
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE initializeTestSuiteManager(self)
         IMPLICIT NONE
         CLASS(TestSuiteManager) :: self

            self % testCasesHead => NULL()
            self % testCasesTail => NULL()
            self % numberOfTests = 0
            
      END SUBROUTINE initializeTestSuiteManager
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE setOutputUnit(self,iUnit)
         IMPLICIT NONE 
         CLASS(TestSuiteManager) :: self
         INTEGER                 :: iUnit
         self % stdOut = iUnit
      END SUBROUTINE setOutputUnit    
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE addTestSubroutineWithName(self,testSubroutine ,testName, optData)
         IMPLICIT NONE
         CLASS(TestSuiteManager)             :: self
         EXTERNAL                            :: testSubroutine
         CHARACTER(LEN=*)                    :: testName
         CHARACTER(LEN=1), POINTER, OPTIONAL :: optData(:) 
         TYPE(TestCaseRecord), POINTER       :: newTestCase
         
         INTERFACE
            SUBROUTINE testSubroutine(optData)
               CHARACTER(LEN=1), POINTER, OPTIONAL :: optData(:) 
            END SUBROUTINE  testSubroutine
         END INTERFACE
         
         ALLOCATE(newTestCase)
         newTestCase % testName     = TRIM(ADJUSTL(testName))
         newTestCase % TestSubroutine => testSubroutine
         IF ( PRESENT(optData) )     THEN
            newTestCase % optData => optData 
         ELSE 
            newTestCase % optData => NULL() 
         END IF 
          
         newTestCase % next         => NULL()
         newTestCase % passed       = .TRUE.
         self % numberOfTests       = self % numberOfTests + 1
         
         IF ( ASSOCIATED(self % testCasesHead) )     THEN
            self % testCasesTail % next => newTestCase
            self % testCasesTail      => newTestCase 
         ELSE
            self % testCasesHead => newTestCase
            self % testCasesTail => newTestCase
         END IF 
         
      END SUBROUTINE addTestSubroutineWithName    
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE finalizeTestSuiteManager(self)
         IMPLICIT NONE
         TYPE(TestSuiteManager)        :: self
         TYPE(TestCaseRecord), POINTER :: tmp, current
         
         IF ( .NOT.ASSOCIATED(self % testCasesHead) )     THEN
           RETURN 
         END IF 
         
         current => self % testCasesHead
         DO WHILE (ASSOCIATED(tmp))
            tmp => current % next
            
            IF(ASSOCIATED(current % assertionsManager)) THEN
               DEALLOCATE(current % assertionsManager)
               DEALLOCATE(current % optData)
            END IF 
            
            DEALLOCATE(current)
            current => tmp
         END DO

         self % testCasesHead => NULL()
         self % testCasesTail => NULL()
         self % numberOfTests = 0
         
      END SUBROUTINE finalizeTestSuiteManager
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE performTests(self, numberOfFailedTestsRet)  
          IMPLICIT NONE  
!
!         ---------
!         Arguments
!         ---------
!
          CLASS(TestSuiteManager)             :: self
          INTEGER                  , OPTIONAL :: numberOfFailedTestsRet
!
!         ---------------
!         Local variables
!         ---------------
!
          TYPE(TestCaseRecord)     , POINTER  :: current
          TYPE(FTAssertionsManager), POINTER  :: sharedManager
          INTEGER                             :: maxMessageLength, numberOfFailedTests
          
          numberOfFailedTests = 0
          maxMessageLength    = 0
          
          WRITE(self % stdOut,*)
          WRITE(self % stdOut,*) "                   ////////////////////////////////"
          WRITE(self % stdOut,*) "                   ////    Begin Test Suites   ////"
          WRITE(self % stdOut,*) "                   ////////////////////////////////"
          WRITE(self % stdOut,*)
          
          current => self % testCasesHead
          DO WHILE (ASSOCIATED(current))
          
            CALL initializeSharedAssertionsManager
            sharedManager               => sharedAssertionsManager()
            current % assertionsManager => sharedManager
            
            IF ( ASSOCIATED(current % optData) )     THEN
               CALL current % TestSubroutine(current % optData)
            ELSE 
               CALL current % TestSubroutine
            END IF 
            
            IF ( sharedManager % numberOfAssertionFailures() /= 0 )     THEN
               numberOfFailedTests = numberOfFailedTests + 1 
               current % passed = .FALSE.
            END IF 
               
            CALL sharedManager % SummarizeAssertions(current % testName,self % stdOut)
            CALL detachSharedAssertionsManager
            
            maxMessageLength = MAX(maxMessageLength,LEN_TRIM(current % testName))
            
            current => current % next
          END DO
        
          WRITE(self % stdOut,*)
          WRITE(self % stdOut,*) "   **********************************************************"
          WRITE(self % stdOut,*) "                     Summary of failed test suites:"
          WRITE(self % stdOut,'(i6,A,i3)')  numberOfFailedTests," suite(s) failed out of ", self % numberOfTests 
          WRITE(self % stdOut,*) "   **********************************************************"
          
          WRITE(self % stdOut,*)
          WRITE(self % stdOut,*) "                   ////////////////////////////////////"
          WRITE(self % stdOut,*) "                   ////    Test Suites Completed   ////"
          WRITE(self % stdOut,*) "                   ////////////////////////////////////"
          WRITE(self % stdOut,*)
        
!
!         ------------------
!         Test matrix output
!         ------------------
!
          WRITE(self % stdOut,*)
          WRITE(self % stdOut,*) "////////////////////////////////"
          WRITE(self % stdOut,*) "////   Test Status Matrix   ////"
          WRITE(self % stdOut,*) "////////////////////////////////"
          WRITE(self % stdOut,*)
        
          current => self % testCasesHead
          DO WHILE (ASSOCIATED(current))
            
            IF ( current % passed )     THEN
               WRITE(self % stdOut,*) current % testName(1:maxMessageLength), " ... Passed"
            ELSE 
               WRITE(self % stdOut,*) current % testName(1:maxMessageLength), " ... F A I L E D"
            END IF 
            
            current => current % next
          END DO
        
          IF(PRESENT(numberOfFailedTestsRet)) numberOfFailedTestsRet = numberOfFailedTests
          
      END SUBROUTINE performTests    
      
      END Module TestSuiteManagerClass    