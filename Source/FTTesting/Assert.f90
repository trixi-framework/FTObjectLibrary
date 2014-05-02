!
!////////////////////////////////////////////////////////////////////////
!
! Assert.f90
! Created: February 21, 2013 2:34 PM 
! By: David Kopriva  
!
!> Assertions are functions that return true or false
!! that can be placed in a program to test whether
!! a predicate is true.
!!
!! To use the assertions module, it must be initialized,
!! usually in the main program. When it is no longer needed,
!! it is finalized. Assertions are posted to the module as they
!! are called, and can be summarized later at an appropriate time.
!!
!! ### Initialization ###
!!
!!      CALL initializeSharedAssertionsManager
!!
!! ### Finalization ###
!!
!!      CALL finalizeSharedAssertionsManager
!!
!! ### Asserting ###
!!
!!      CALL assertEqual(expectedValue,resultValue,message)
!!
!! ### Summarizing Assertions ###
!!
!!      CALL SummarizeFTAssertions(title,unit)
!!
!! ### Additional enquiry functions ###
!!
!!      INTEGER :: nf, nA
!!       nF = numberOfAssertionFailures()
!!       nA = numberOfAssertions()
!<
!
!////////////////////////////////////////////////////////////////////////
!
      Module FTAssertions
      USE ComparisonsModule
      IMPLICIT NONE
      PRIVATE
!
!     ------
!     Public
!     ------
!
      INTEGER, PARAMETER, PUBLIC :: FT_ASSERTION_STRING_LENGTH = 128
      
      INTERFACE assertEqual
         MODULE PROCEDURE assertEqualTwoIntegers,assertEqualTwoIntegerArrays1D, &
         assertEqualTwoIntegerArrays2D
!         MODULE PROCEDURE assertEqualTwoIntegerArrays1D
!         MODULE PROCEDURE assertEqualTwoIntegerArrays2D
         MODULE PROCEDURE assertWithinToleranceTwoReal
         MODULE PROCEDURE assertWithinToleranceTwoRealArrays1D
         MODULE PROCEDURE assertWithinToleranceTwoRealArrays2D
         MODULE PROCEDURE assertWithinToleranceTwoDouble
         MODULE PROCEDURE assertWithinToleranceTwoDoubleArrays1D
         MODULE PROCEDURE assertWithinToleranceTwoDoubleArrays2D
         MODULE PROCEDURE assertEqualTwoLogicals
         MODULE PROCEDURE assertEqualString
      END INTERFACE assertEqual
      
      PUBLIC :: assertEqual, initializeSharedAssertionsManager, finalizeSharedAssertionsManager
      PUBLIC :: SummarizeFTAssertions, numberOfAssertionFailures, assert
!
!     -------
!     Private
!     -------
!
      TYPE FTAssertionFailureRecord
         CHARACTER(LEN=FT_ASSERTION_STRING_LENGTH) :: msg, expected, actual
         TYPE(FTAssertionFailureRecord), POINTER   :: next
      END TYPE FTAssertionFailureRecord
      
      LOGICAL :: isInitialized_ = .false.
      INTEGER :: numberOfTests_, numberOfAssertionFailures_
!
!     ---------------------------------------------------------------------
!     Assertion failures are stored in a linked list managed by the manager
!     to be independent of any dependencies
!     ---------------------------------------------------------------------
!
      TYPE(FTAssertionFailureRecord), POINTER :: failureListHead => NULL()
      TYPE(FTAssertionFailureRecord), POINTER :: failureListTail => NULL()

!     ========      
      CONTAINS
!     ========
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE initializeSharedAssertionsManager  
         IMPLICIT NONE
!
!        --------------------------------------------------
!        The manager is allowed only once to be initialized
!        per run.
!        --------------------------------------------------
!
         IF ( .NOT.isInitialized_ )     THEN
            isInitialized_             = .true.
            numberOfTests_             = 0
            numberOfAssertionFailures_ = 0
            NULLIFY(failureListHead, failureListTail)
         END IF
         
      END SUBROUTINE initializeSharedAssertionsManager
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE finalizeSharedAssertionsManager
         IMPLICIT NONE
         TYPE(FTAssertionFailureRecord), POINTER :: tmp, current
         
         IF ( .NOT.ASSOCIATED(failureListHead) )     THEN
             isInitialized_ = .false.
           RETURN 
         END IF 
         
         current => failureListHead
         DO WHILE (ASSOCIATED(tmp))
            tmp => current%next
            DEALLOCATE(current)
            current => tmp
         END DO
         
         numberOfTests_    = 0
         numberOfAssertionFailures_ = 0
         NULLIFY(failureListHead, failureListTail)
        
      END SUBROUTINE finalizeSharedAssertionsManager    
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE addAssertionFailureForParameters(msg,expected,actual)
         IMPLICIT NONE  
         CHARACTER(LEN=*)                        :: msg, expected, actual
         TYPE(FTAssertionFailureRecord), POINTER :: newFailure
         
         ALLOCATE(newFailure)
         newFailure%msg      = TRIM(ADJUSTL(msg))
         newFailure%expected = TRIM(ADJUSTL(expected))
         newFailure%actual   = TRIM(ADJUSTL(actual))
         newFailure%next     => NULL()
         
         IF ( ASSOCIATED(failureListTail) )     THEN
            failureListTail%next => newFailure
            failureListTail=> failureListTail%next
         ELSE
            failureListHead => newFailure
            failureListTail => newFailure
         END IF 
      END SUBROUTINE addAssertionFailureForParameters
!
!//////////////////////////////////////////////////////////////////////// 
! 
     SUBROUTINE SummarizeFTAssertions(title,iUnit)  
        IMPLICIT NONE
        CHARACTER(LEN=*)                        :: title
        INTEGER                                 :: iUnit
        TYPE(FTAssertionFailureRecord), POINTER :: current
        
        WRITE(iUnit,*)
        WRITE(iUnit,*) "   -------------------------------------------------------------"
        WRITE(iUnit,*) "   Summary of failed tests for test suite ",TRIM(title)
        WRITE(iUnit,*)  "   ",numberOfAssertionFailures_," failures out of ", numberOfTests_," tests." 
        WRITE(iUnit,*) "   -------------------------------------------------------------"
         
         IF ( .NOT.ASSOCIATED(failureListHead) )     THEN
             isInitialized_ = .false.
           RETURN 
         END IF 
         
         current => failureListHead
         DO WHILE (ASSOCIATED(current))
            WRITE(iUnit,*) "   ",TRIM(current%msg)," failure: Expected [",TRIM(current%expected),"], Got [",TRIM(current%actual),"]"
            current => current%next
         END DO
         
         WRITE(iUnit,*)
         
     END SUBROUTINE SummarizeFTAssertions    
!
!//////////////////////////////////////////////////////////////////////// 
! 
     INTEGER FUNCTION numberOfAssertions()
        IMPLICIT NONE  
        numberOfAssertions = numberOfTests_
     END FUNCTION numberOfAssertions    
!
!//////////////////////////////////////////////////////////////////////// 
! 
     INTEGER FUNCTION numberOfAssertionFailures()
        IMPLICIT NONE  
        numberOfAssertionFailures = numberOfAssertionFailures_
     END FUNCTION numberOfAssertionFailures  
!@mark -
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE assert(test,msg)  
         IMPLICIT NONE
         CHARACTER(LEN=*), OPTIONAL :: msg
         LOGICAL                    :: test
         
         numberOfTests_ = numberOfTests_ + 1
         IF ( .NOT.test )     THEN
            CALL addAssertionFailureForParameters(msg,"True","False")
            numberOfAssertionFailures_ = numberOfAssertionFailures_ + 1
         END IF 
      END SUBROUTINE assert      
!@mark -
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE assertEqualTwoIntegers(i,j,msg)  
         IMPLICIT NONE  
         INTEGER, INTENT(in)        :: i, j
         CHARACTER(LEN=*), OPTIONAL :: msg

         CHARACTER(LEN=FT_ASSERTION_STRING_LENGTH) :: expected,actual
         
         numberOfTests_ = numberOfTests_ + 1
         IF ( .NOT.isEqual(i,j) )     THEN
            WRITE(expected,*) i
            WRITE(actual,*) j
            CALL addAssertionFailureForParameters(msg,expected,actual)
            numberOfAssertionFailures_ = numberOfAssertionFailures_ + 1
         END IF 
         
      END SUBROUTINE assertEqualTwoIntegers    
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE assertEqualTwoIntegerArrays1D(a,b)  
         IMPLICIT NONE  
         INTEGER, INTENT(in)    , DIMENSION(:)            :: a, b
         
         numberOfTests_ = numberOfTests_ + 1
         IF ( .NOT.isEqual(a,b) )     THEN
             
            numberOfAssertionFailures_ = numberOfAssertionFailures_ + 1
         END IF 
         
      END SUBROUTINE assertEqualTwoIntegerArrays1D
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE assertEqualTwoIntegerArrays2D(a,b)  
         IMPLICIT NONE  
         INTEGER, INTENT(in)    , DIMENSION(:,:)          :: a, b
         
         numberOfTests_ = numberOfTests_ + 1
         IF ( .NOT.isEqual(a,b) )     THEN
             
            numberOfAssertionFailures_ = numberOfAssertionFailures_ + 1
         END IF 
         
      END SUBROUTINE assertEqualTwoIntegerArrays2D
!@mark -
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE assertWithinToleranceTwoReal(x,y,tol,msg)  
         IMPLICIT NONE  
         REAL, INTENT(in)           :: x,y,tol
         CHARACTER(LEN=*), OPTIONAL :: msg

         CHARACTER(LEN=FT_ASSERTION_STRING_LENGTH) :: expected,actual
        
         numberOfTests_ = numberOfTests_ + 1
         IF ( .NOT.isEqual(x,y,tol) )     THEN
            WRITE(expected,*) x
            WRITE(actual,*) y
            CALL addAssertionFailureForParameters(msg,expected,actual)
            numberOfAssertionFailures_ = numberOfAssertionFailures_ + 1
         END IF 
         
      END SUBROUTINE assertWithinToleranceTwoReal    
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE assertWithinToleranceTwoRealArrays1D(a,b,tol,msg)  
         IMPLICIT NONE  
         REAL, INTENT(IN), DIMENSION(:) :: a, b
         REAL, INTENT(IN)               :: tol
         CHARACTER(LEN=*), OPTIONAL     :: msg
         INTEGER                        :: k
         
         CHARACTER(LEN=FT_ASSERTION_STRING_LENGTH) :: expected,actual
         
         numberOfTests_ = numberOfTests_ + 1
         IF ( .NOT.isEqual(a,b,tol) )     THEN
            DO k = 1, SIZE(a)
               WRITE(expected,*) a(k)
               WRITE(actual,*)   b(k)
               CALL addAssertionFailureForParameters(msg,expected,actual)
               numberOfAssertionFailures_ = numberOfAssertionFailures_ + 1
            END DO  
         END IF 
         
      END SUBROUTINE assertWithinToleranceTwoRealArrays1D
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE assertWithinToleranceTwoRealArrays2D(a,b,tol)  
         IMPLICIT NONE  
         REAL, INTENT(IN), DIMENSION(:,:) :: a, b
         REAL, INTENT(IN)                 :: tol
         
         numberOfTests_ = numberOfTests_ + 1
         IF ( .NOT.isEqual(a,b,tol) )     THEN
             
            numberOfAssertionFailures_ = numberOfAssertionFailures_ + 1
         END IF 
         
      END SUBROUTINE assertWithinToleranceTwoRealArrays2D
!@mark -
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE assertWithinToleranceTwoDouble(x,y,tol,msg)  
         IMPLICIT NONE  
         DOUBLE PRECISION, INTENT(in) :: x,y,tol
         CHARACTER(LEN=*), OPTIONAL   :: msg

         CHARACTER(LEN=FT_ASSERTION_STRING_LENGTH) :: expected,actual
         
          numberOfTests_ = numberOfTests_ + 1
        IF ( .NOT.isEqual(x,y,tol) )     THEN
            WRITE(expected,*) x
            WRITE(actual,*) y
            CALL addAssertionFailureForParameters(msg,expected,actual)
            numberOfAssertionFailures_ = numberOfAssertionFailures_ + 1
         END IF 
         
      END SUBROUTINE assertWithinToleranceTwoDouble    
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE assertWithinToleranceTwoDoubleArrays1D(a,b,tol,msg)  
         IMPLICIT NONE  
         DOUBLE PRECISION, INTENT(IN), DIMENSION(:) :: a, b
         DOUBLE PRECISION, INTENT(IN)               :: tol
         CHARACTER(LEN=*), OPTIONAL                 :: msg
         INTEGER                                    :: code
         INTEGER                                    :: k
         
         CHARACTER(LEN=FT_ASSERTION_STRING_LENGTH) :: expected,actual,eMsg
         
         numberOfTests_ = numberOfTests_ + 1
         IF ( .NOT.isEqual(a,b,tol,code) )     THEN
            eMsg = TRIM(msg) // "---" // TRIM(compareCodeStrings(code))
            DO k = 1, SIZE(a)
               WRITE(expected,*) a(k)
               WRITE(actual,*)   b(k)
               CALL addAssertionFailureForParameters(eMsg,expected,actual)
               numberOfAssertionFailures_ = numberOfAssertionFailures_ + 1
            END DO  
         END IF 
         
      END SUBROUTINE assertWithinToleranceTwoDoubleArrays1D
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE assertWithinToleranceTwoDoubleArrays2D(a,b,tol)  
         IMPLICIT NONE  
         DOUBLE PRECISION, INTENT(IN), DIMENSION(:,:) :: a, b
         DOUBLE PRECISION, INTENT(IN)                 :: tol
         INTEGER                         :: code
         
         numberOfTests_ = numberOfTests_ + 1
         IF ( .NOT.isEqual(a,b,tol,code) )     THEN
             
             numberOfAssertionFailures_ = numberOfAssertionFailures_ + 1
        END IF 
         
      END SUBROUTINE assertWithinToleranceTwoDoubleArrays2D
!@mark -
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE assertEqualString(s1,s2,msg)
         IMPLICIT NONE
         CHARACTER(LEN=*)           :: s1,s2
         CHARACTER(LEN=*), OPTIONAL :: msg
         
         numberOfTests_ = numberOfTests_ + 1
         IF ( .NOT.isEqual(s1,s2) )     THEN
            CALL addAssertionFailureForParameters(msg,s1,s2)
            numberOfAssertionFailures_ = numberOfAssertionFailures_ + 1
         END IF 
         
      END SUBROUTINE assertEqualString
!@mark -
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE assertEqualTwoLogicals(i,j,msg)  
         IMPLICIT NONE  
         LOGICAL, INTENT(in)        :: i, j
         CHARACTER(LEN=*), OPTIONAL :: msg

         CHARACTER(LEN=FT_ASSERTION_STRING_LENGTH) :: expected,actual
         
         numberOfTests_ = numberOfTests_ + 1
         IF ( .NOT.(i .EQV. j) )     THEN
            WRITE(expected,*) i
            WRITE(actual,*) j
            CALL addAssertionFailureForParameters(msg,expected,actual)
            numberOfAssertionFailures_ = numberOfAssertionFailures_ + 1
         END IF 
         
      END SUBROUTINE assertEqualTwoLogicals    
       
      END Module FTAssertions    
