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
!!      CALL FTAssertEqual(expectedValue,resultValue,message)
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
      
      TYPE FTAssertionsManager
         PRIVATE
          
         INTEGER                                 :: numberOfTests_
         INTEGER                                 :: numberOfAssertionFailures_
         TYPE(FTAssertionFailureRecord), POINTER :: failureListHead => NULL()
         TYPE(FTAssertionFailureRecord), POINTER :: failureListTail => NULL()
!
!        ========
         CONTAINS
!        ========
!         
         PROCEDURE, PUBLIC  :: init
         PROCEDURE, PUBLIC  :: finalize
         PROCEDURE, PUBLIC  :: numberOfAssertionFailures
         PROCEDURE, PUBLIC  :: numberOfAssertions
         PROCEDURE, PUBLIC  :: summarizeAssertions
         
      END TYPE FTAssertionsManager
      
      PUBLIC :: FTAssertionsManager
      
      INTERFACE FTAssertEqual
         MODULE PROCEDURE assertEqualTwoIntegers
         MODULE PROCEDURE assertEqualTwoIntegerArrays1D
         MODULE PROCEDURE assertEqualTwoIntegerArrays2D
         MODULE PROCEDURE assertWithinToleranceTwoReal
         MODULE PROCEDURE assertWithinToleranceTwoRealArrays1D
         MODULE PROCEDURE assertWithinToleranceTwoRealArrays2D
         MODULE PROCEDURE assertWithinToleranceTwoDouble
         MODULE PROCEDURE assertWithinToleranceTwoDoubleArrays1D
         MODULE PROCEDURE assertWithinToleranceTwoDoubleArrays2D
         MODULE PROCEDURE assertEqualTwoLogicals
         MODULE PROCEDURE assertEqualString
      END INTERFACE FTAssertEqual
      
      PUBLIC :: FTAssertEqual
      PUBLIC :: initializeSharedAssertionsManager, finalizeSharedAssertionsManager
      PUBLIC :: FTAssert, sharedAssertionsManager, numberOfAssertionFailures, numberOfAssertions
!
!     -------
!     Private
!     -------
!
      TYPE FTAssertionFailureRecord
         CHARACTER(LEN=FT_ASSERTION_STRING_LENGTH) :: msg, expected, actual
         TYPE(FTAssertionFailureRecord), POINTER   :: next
      END TYPE FTAssertionFailureRecord
!
!     -------------------------
!     Shared Assertions manager
!     -------------------------
!
      TYPE(FTAssertionsManager), POINTER, PRIVATE  :: sharedManager
!
!     ========      
      CONTAINS
!     ========
!@mark -
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION sharedAssertionsManager()
         IMPLICIT NONE  
         TYPE(FTAssertionsManager), POINTER :: sharedAssertionsManager
         sharedAssertionsManager => sharedManager 
      END FUNCTION sharedAssertionsManager
! 
!//////////////////////////////////////////////////////////////////////// 
! 
      INTEGER FUNCTION numberOfAssertions(self) 
         IMPLICIT NONE  
         CLASS(FTAssertionsManager) :: self
         numberOfAssertions = self % numberOfTests_
      END FUNCTION numberOfAssertions
! 
!//////////////////////////////////////////////////////////////////////// 
! 
      INTEGER FUNCTION numberOfAssertionFailures(self) 
         IMPLICIT NONE  
         CLASS(FTAssertionsManager) :: self
         numberOfAssertionFailures = self % numberOfAssertionFailures_
      END FUNCTION numberOfAssertionFailures
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE init(self)  
         IMPLICIT NONE
         CLASS(FTAssertionsManager) :: self
         
         self % numberOfTests_             = 0
         self % numberOfAssertionFailures_ = 0
         NULLIFY(self % failureListHead, self % failureListTail)
         
      END SUBROUTINE init
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE finalize(self)
         IMPLICIT NONE
         CLASS(FTAssertionsManager)              :: self
         TYPE(FTAssertionFailureRecord), POINTER :: tmp, current
         
         IF ( .NOT.ASSOCIATED(self % failureListHead) ) RETURN 
!
!        ------------------------------
!        Delete linked list of failures
!        ------------------------------
!
         current => self % failureListHead
         DO WHILE (ASSOCIATED(tmp))
            tmp => current % next
            DEALLOCATE(current)
            current => tmp
         END DO
         
         self % numberOfTests_    = 0
         self % numberOfAssertionFailures_ = 0
         NULLIFY(self % failureListHead, self % failureListTail)
        
      END SUBROUTINE finalize    
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
         IF ( ASSOCIATED(sharedManager) )     RETURN 
         
         ALLOCATE(sharedManager)
         CALL sharedManager % init()
         
      END SUBROUTINE initializeSharedAssertionsManager
! 
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE finalizeSharedAssertionsManager 
         IMPLICIT NONE
         
         IF(ASSOCIATED(sharedManager)) CALL sharedManager % finalize()
         
      END SUBROUTINE finalizeSharedAssertionsManager
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE addAssertionFailureForParameters(msg,expected,actual)
         IMPLICIT NONE  
         CHARACTER(LEN=*)                        :: msg, expected, actual
         TYPE(FTAssertionFailureRecord), POINTER :: newFailure
         
         ALLOCATE(newFailure)
         newFailure % msg      = TRIM(ADJUSTL(msg))
         newFailure % expected = TRIM(ADJUSTL(expected))
         newFailure % actual   = TRIM(ADJUSTL(actual))
         newFailure % next     => NULL()
         
         IF ( ASSOCIATED(sharedManager % failureListTail) )     THEN
            sharedManager % failureListTail % next => newFailure
            sharedManager % failureListTail        => sharedManager % failureListTail % next
         ELSE
            sharedManager % failureListHead => newFailure
            sharedManager % failureListTail => newFailure
         END IF 
         
         sharedManager % numberOfAssertionFailures_ = sharedManager % numberOfAssertionFailures_ + 1
         
      END SUBROUTINE addAssertionFailureForParameters
!
!//////////////////////////////////////////////////////////////////////// 
! 
     SUBROUTINE summarizeAssertions(self,title,iUnit)  
        IMPLICIT NONE
        CLASS(FTAssertionsManager)              :: self
        CHARACTER(LEN=*)                        :: title
        INTEGER                                 :: iUnit
        TYPE(FTAssertionFailureRecord), POINTER :: current
        
        WRITE(iUnit,*)
        WRITE(iUnit,*) "   -------------------------------------------------------------"
        WRITE(iUnit,*) "   Summary of failed tests for test suite ",TRIM(title)
        WRITE(iUnit,*)  "   ",self % numberOfAssertionFailures()," failures out of ", &
                              self % numberOfAssertions()," tests." 
        WRITE(iUnit,*) "   -------------------------------------------------------------"
                  
         current => self % failureListHead
         DO WHILE (ASSOCIATED(current))
            WRITE(iUnit,*) "   ",TRIM(current % msg),&
                               " failure: Expected [",TRIM(current % expected),&
                               "], Got [",TRIM(current % actual),"]"
            current => current % next
         END DO
         
         WRITE(iUnit,*)
         
     END SUBROUTINE summarizeAssertions    

!@mark -
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE FTAssert(test,msg)  
         IMPLICIT NONE
         CHARACTER(LEN=*), OPTIONAL :: msg
         LOGICAL                    :: test
         
         IF(.NOT.ASSOCIATED(sharedManager)) THEN
            CALL initializeSharedAssertionsManager
         END IF 
         
        sharedManager % numberOfTests_ = sharedManager % numberOfTests_ + 1
        IF ( .NOT.test )     THEN
            CALL addAssertionFailureForParameters(msg,"True","False")
         END IF 
      END SUBROUTINE FTAssert      
!@mark -
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE assertEqualTwoIntegers(expectedValue,actualValue,msg)  
         IMPLICIT NONE  
         INTEGER, INTENT(in)        :: expectedValue,actualValue
         CHARACTER(LEN=*), OPTIONAL :: msg

         CHARACTER(LEN=FT_ASSERTION_STRING_LENGTH) :: expected,actual
         
         IF(.NOT.ASSOCIATED(sharedManager)) THEN
            CALL initializeSharedAssertionsManager
         END IF 
         
        sharedManager % numberOfTests_ = sharedManager % numberOfTests_ + 1
         IF ( .NOT.isEqual(expectedValue,actualValue) )     THEN
            WRITE(expected,*) expectedValue
            WRITE(actual,*) actualValue
            CALL addAssertionFailureForParameters(msg,expected,actual)
         END IF 
         
      END SUBROUTINE assertEqualTwoIntegers    
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE assertEqualTwoIntegerArrays1D(a,b)  
         IMPLICIT NONE  
         INTEGER, INTENT(in)    , DIMENSION(:)            :: a, b
         
         IF(.NOT.ASSOCIATED(sharedManager)) THEN
            CALL initializeSharedAssertionsManager
         END IF 
         
         sharedManager % numberOfTests_ = sharedManager % numberOfTests_ + 1
         IF ( .NOT.isEqual(a,b) )     THEN
             PRINT *, "assertEqualTwoIntegerArrays1D not implemented"
         END IF 
         
      END SUBROUTINE assertEqualTwoIntegerArrays1D
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE assertEqualTwoIntegerArrays2D(a,b)  
         IMPLICIT NONE  
         INTEGER, INTENT(in)    , DIMENSION(:,:)          :: a, b
         
         IF(.NOT.ASSOCIATED(sharedManager)) THEN
            CALL initializeSharedAssertionsManager
         END IF 
         
         sharedManager % numberOfTests_ = sharedManager % numberOfTests_ + 1
         IF ( .NOT.isEqual(a,b) )     THEN
             PRINT *, "assertEqualTwoIntegerArrays2D not implemented"
         END IF 
         
      END SUBROUTINE assertEqualTwoIntegerArrays2D
!@mark -
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE assertWithinToleranceTwoReal(expectedValue,actualValue,tol,msg)  
         IMPLICIT NONE  
         REAL, INTENT(in)           :: expectedValue,actualValue,tol
         CHARACTER(LEN=*), OPTIONAL :: msg

         CHARACTER(LEN=FT_ASSERTION_STRING_LENGTH) :: expectedS,actualS
         
         IF(.NOT.ASSOCIATED(sharedManager)) THEN
            CALL initializeSharedAssertionsManager
         END IF 
         
         sharedManager % numberOfTests_ = sharedManager % numberOfTests_ + 1
         IF ( .NOT.isEqual(expectedValue,actualValue,tol) )     THEN
            WRITE(expectedS,*) expectedValue
            WRITE(actualS,*) actualValue
            CALL addAssertionFailureForParameters(msg,expectedS,actualS)
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
         
         IF(.NOT.ASSOCIATED(sharedManager)) THEN
            CALL initializeSharedAssertionsManager
         END IF 
         
         sharedManager % numberOfTests_ = sharedManager % numberOfTests_ + 1
         IF ( .NOT.isEqual(a,b,tol) )     THEN
            DO k = 1, SIZE(a)
               WRITE(expected,*) a(k)
               WRITE(actual,*)   b(k)
               CALL addAssertionFailureForParameters(msg,expected,actual)
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
         
         IF(.NOT.ASSOCIATED(sharedManager)) THEN
            CALL initializeSharedAssertionsManager
         END IF 
         
         sharedManager % numberOfTests_ = sharedManager % numberOfTests_ + 1
         IF ( .NOT.isEqual(a,b,tol) )     THEN
             PRINT *, "assertWithinToleranceTwoRealArrays2D not implemented"
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
         
         IF(.NOT.ASSOCIATED(sharedManager)) THEN
            CALL initializeSharedAssertionsManager
         END IF 
         
         sharedManager % numberOfTests_ = sharedManager % numberOfTests_ + 1
        IF ( .NOT.isEqual(x,y,tol) )     THEN
            WRITE(expected,*) x
            WRITE(actual,*) y
            CALL addAssertionFailureForParameters(msg,expected,actual)
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
         
         IF(.NOT.ASSOCIATED(sharedManager)) THEN
            CALL initializeSharedAssertionsManager
         END IF 
         
         sharedManager % numberOfTests_ = sharedManager % numberOfTests_ + 1
         IF ( .NOT.isEqual(a,b,tol,code) )     THEN
            eMsg = TRIM(msg) // "---" // TRIM(compareCodeStrings(code))
            DO k = 1, SIZE(a)
               WRITE(expected,*) a(k)
               WRITE(actual,*)   b(k)
               CALL addAssertionFailureForParameters(eMsg,expected,actual)
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
         
         IF(.NOT.ASSOCIATED(sharedManager)) THEN
            CALL initializeSharedAssertionsManager
         END IF 
         
         sharedManager % numberOfTests_ = sharedManager % numberOfTests_ + 1
         IF ( .NOT.isEqual(a,b,tol,code) )     THEN
             PRINT *, "assertWithinToleranceTwoDoubleArrays2D not implemented"
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
         
         IF(.NOT.ASSOCIATED(sharedManager)) THEN
            CALL initializeSharedAssertionsManager
         END IF 
         
         sharedManager % numberOfTests_ = sharedManager % numberOfTests_ + 1
         IF ( .NOT.isEqual(s1,s2) )     THEN
            CALL addAssertionFailureForParameters(msg,s1,s2)
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
         
         IF(.NOT.ASSOCIATED(sharedManager)) THEN
            CALL initializeSharedAssertionsManager
         END IF 
         
         sharedManager % numberOfTests_ = sharedManager % numberOfTests_ + 1
         IF ( .NOT.(i .EQV. j) )     THEN
            WRITE(expected,*) i
            WRITE(actual,*) j
            CALL addAssertionFailureForParameters(msg,expected,actual)
         END IF 
         
      END SUBROUTINE assertEqualTwoLogicals    
       
      END Module FTAssertions    
