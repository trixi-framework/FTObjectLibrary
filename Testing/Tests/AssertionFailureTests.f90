!
!////////////////////////////////////////////////////////////////////////
!
!      AssertionFailureTests.f90
!      Created: June 22, 2023 at 8:32 AM 
!      By: David Kopriva  
!
!////////////////////////////////////////////////////////////////////////
!
   SUBROUTINE AssertionFailureTests
!
!  ------------------------------------------------------------------
!  Bypass the ususal testing framework and make assertions that
!  fail. Then check the list of failures to make sure they are right.
!  Do an error stop only if any tests fail to fail.
!  ------------------------------------------------------------------
!
      USE FTAssertions
      IMPLICIT NONE
      
      TYPE(FTAssertionsManager), POINTER :: assertionsManager
      INTEGER                            :: numberUnexpectedFailures
      
      CHARACTER(LEN=FT_ASSERTION_STRING_LENGTH) :: expected, actual
!
!     -----
!     Setup
!     -----
!
      numberUnexpectedFailures = 0
      assertionsManager => sharedAssertionsManager()
      IF ( .NOT. ASSOCIATED(assertionsManager) )     THEN
         CALL initializeSharedAssertionsManager
         assertionsManager => sharedAssertionsManager()
      END IF 
!
!     ----------------
!     Logical failures
!     ----------------
!
      CALL FTAssert(.FALSE.,msg = "Assert1")
      CALL DoTest(1,                              &
                  "Logical assertion failed: ",   &
                  "True",                         &
                  "False",                        &
                  "Assert1",                      &
                   numberUnexpectedFailures)

      CALL FTAssert(.FALSE.)
      CALL DoTest(2,                              &
                  "Logical assertion failed: ",   &
                  "True",                         &
                  "False",                        &
                  "",                             &
                   numberUnexpectedFailures)
!
!     ------------
!     String tests
!     ------------
!
      CALL FTAssertEqual(expectedValue = "Bob", actualValue = "Fred", msg = "String comp")
      CALL DoTest(3,                            &
                  "String equality failed: ",   &
                  "Bob",                        &
                  "Fred",                       &
                  "String comp",                &
                   numberUnexpectedFailures)
                   
      CALL FTAssertEqual(expectedValue = "Bob", actualValue = "Fred")
      CALL DoTest(4,                            &
                  "String equality failed: ",   &
                  "Bob",                        &
                  "Fred",                       &
                  "",                           &
                   numberUnexpectedFailures)
!
!     -------------
!     INTEGER tests
!     -------------
!
      CALL FTAssertEqual(expectedValue = 1, actualValue = 2, msg = "Integer comp")
      WRITE(expected,*) 1
      expected = TRIM(ADJUSTL(expected))
      WRITE(actual,*)   2
      actual = TRIM(ADJUSTL(actual))
      CALL DoTest(5,                            &
                  "Integer equality failed: ",  &
                   expected,                    &
                   actual,                      &
                  "Integer comp",               &
                   numberUnexpectedFailures)
      CALL FTAssertEqual(expectedValue = 1, actualValue = 2)
      CALL DoTest(6,                            &
                  "Integer equality failed: ",  &
                   expected,                    &
                   actual,                      &
                  "",                           &
                   numberUnexpectedFailures)
!
!     ----------
!     real tests
!     ----------
!
      CALL FTAssertEqual(expectedValue = 1.0, actualValue = 2.0, relTol = 1.0e-4, msg = "Real comp")
      WRITE(expected,*) 1.0
      expected = TRIM(ADJUSTL(expected))
      WRITE(actual,*)   2.0
      actual = TRIM(ADJUSTL(actual))
      CALL DoTest(7,                            &
                  "Real equality failed: ",     &
                   expected,                    &
                   actual,                      &
                  "Real comp",                  &
                   numberUnexpectedFailures)
      CALL FTAssertEqual(expectedValue = 1.0, actualValue = 2.0, relTol = 1.0e-4)
      CALL DoTest(8,                            &
                  "Real equality failed: ",     &
                   expected,                    &
                   actual,                      &
                  "",                           &
                   numberUnexpectedFailures)
!
!     ------------
!     Double tests
!     ------------
!
      CALL FTAssertEqual(expectedValue = 1.0d0, actualValue = 2.0d0, relTol = 1.0d-4, msg = "Double comp")
      WRITE(expected,*) 1.0d0
      expected = TRIM(ADJUSTL(expected))
      WRITE(actual,*)   2.0d0
      actual = TRIM(ADJUSTL(actual))
      CALL DoTest(9,                                        &
                  "Double Precision equality failed: ",     &
                   expected,                                &
                   actual,                                  &
                  "Double comp",                            &
                   numberUnexpectedFailures)
      CALL FTAssertEqual(expectedValue = 1.0d0, actualValue = 2.0d0, relTol = 1.0d-4)
      CALL DoTest(10,                                       &
                  "Double Precision equality failed: ",     &
                   expected,                                &
                   actual,                                  &
                  "",                                       &
                   numberUnexpectedFailures)
!
!     ---------
!     Finish up
!     ---------
!
      IF ( numberUnexpectedFailures > 0 )     THEN
         ERROR STOP "Failure in Assertions testing"
      END IF 
       
   END SUBROUTINE AssertionFailureTests
!
!//////////////////////////////////////////////////////////////////////// 
! 
   SUBROUTINE DoTest(n, aType, expectedValue, actualValue, msg, runningCount)  
      USE FTAssertions
      IMPLICIT NONE
      
      TYPE(FTAssertionsManager), POINTER :: assertionsManager
      INTEGER                            :: n, runningCount
      CHARACTER(LEN=*)                   :: expectedValue, actualValue, aType
      CHARACTER(LEN=*)                   :: msg
      INTEGER                            :: errorCode

      assertionsManager => sharedAssertionsManager()
      
      IF ( numberOfAssertionFailures(assertionsManager) /= n )     THEN
         runningCount = runningCount + 1
      ELSE
         errorCode = SelfTestAssertion(assertionNumber = n,             &
                                       assertionType   = aType,         &
                                       expected        = expectedValue, &
                                       actual          = actualValue,   &
                                       msg             = msg)
         IF (errorCode /= 0) THEN 
            PRINT *, "Assert ", n, " Failed with error code: ", errorCode
            runningCount = runningCount + 1
         END IF
      END IF
      
   END SUBROUTINE DoTest
