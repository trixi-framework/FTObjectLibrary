!
!////////////////////////////////////////////////////////////////////////
!
! Assert.f90
! Created: February 21, 2013 2:34 PM 
! By: David Kopriva  
!
!! Assertions are functions that return true or false
!! that can be placed in a program to test whether
!! a predicate is true.
!!
!!An assertion is a true-false statement that you expect to be true. Assertions are used
!!to test for exceptional situations (AKA ``Failures'') in a code. For example, knowing that density
!!always must be positive, you might assert that fact before using it, and if the result is false 
!!generate an error. With FTObjectLibrary that would be
!!
!!        	CALL assert(rho > 0,``Density must be positive'')
!!
!!
!!Fortran does not have an assertion mechanism, and so pretty much everyone writes their own. 
!!There are a couple of open source projects available, but one never knows how actively they will be 
!!maintained. In the grand Fortran tradition of writing one's own, FTObjectLibrary has
!!an (incomplete) assertion module. 
!!
!!##Initialization
!!
!!To use assertions, you will USE the FTAssertions module and initialize the assertions system by calling
!!
!!        	CALL initializeSharedAssertionsManager
!!
!!##Inquiry Functions
!!During the course of your program, the sharedAssertionsManager will keep track of the 
!!success or failure of the assertions that you make. You can enquire at any time how many assertions
!!have failed and how many assertions have been made with the two enquiry functions
!!
!!     INTEGER FUNCTION numberOfAssertionFailures()
!!     INTEGER FUNCTION numberOfAssertions()
!!##Summary Output
!!You can get a summary of the assertions by calling the subroutine
!!
!!     SUBROUTINE SummarizeFTAssertions(title,iUnit)  
!!        IMPLICIT NONE
!!        CHARACTER(LEN=*)                        :: title
!!        INTEGER                                 :: iUnit
!!##Finalization
!!When you are done, you finalize the sharedAssertionsManager with
!!
!!        	CALL finalizeSharedAssertionsManager
!!##Posting Assertions
!! FTObjectLibrary supplies two subroutines that post 
!!failures to the sharedAssertionsManager. The first takes a LOGICAL variable
!!
!!      SUBROUTINE assert(test,msg)  
!!         IMPLICIT NONE
!!         CHARACTER(LEN=*), OPTIONAL :: msg
!!         LOGICAL                    :: test
!!
!!The second tests equality through the overloaded subroutine assertEqual, which allows a variety 
!!of argument type listed below:
!!
!!       INTERFACE assertEqual
!!         MODULE PROCEDURE assertEqualTwoIntegers
!!         MODULE PROCEDURE assertEqualTwoIntegerArrays1D
!!         MODULE PROCEDURE assertEqualTwoIntegerArrays2D
!!         MODULE PROCEDURE assertWithinToleranceTwoReal
!!         MODULE PROCEDURE assertWithinToleranceTwoRealArrays1D
!!         MODULE PROCEDURE assertWithinToleranceTwoRealArrays2D
!!         MODULE PROCEDURE assertWithinToleranceTwoDouble
!!         MODULE PROCEDURE assertWithinToleranceTwoDoubleArrays1D
!!         MODULE PROCEDURE assertWithinToleranceTwoDoubleArrays2D
!!         MODULE PROCEDURE assertEqualTwoLogicals
!!         MODULE PROCEDURE assertEqualString
!!      END INTERFACE assertEqual
!!
!!The individual calls have the signatures
!!
!!      SUBROUTINE assertEqualTwoIntegers(expectedValue,actualValue,msg)  
!!         IMPLICIT NONE  
!!         INTEGER, INTENT(in)        :: expectedValue,actualValue
!!         CHARACTER(LEN=*), OPTIONAL :: msg
!!         
!!      SUBROUTINE assertEqualTwoIntegerArrays1D(expectedValue,actualValue)  
!!         IMPLICIT NONE  
!!         INTEGER, INTENT(in)    , DIMENSION(:)            :: expectedValue,actualValue
!!         
!!      SUBROUTINE assertEqualTwoIntegerArrays2D(expectedValue,actualValue)  
!!         IMPLICIT NONE  
!!         INTEGER, INTENT(in)    , DIMENSION(:,:)          :: expectedValue,actualValue
!!      SUBROUTINE assertWithinToleranceTwoReal(x,y,tol,msg)  
!!         IMPLICIT NONE  
!!         REAL, INTENT(in)           :: x,y,tol
!!         CHARACTER(LEN=*), OPTIONAL :: msg
!!         
!!      SUBROUTINE assertWithinToleranceTwoRealArrays1D(expectedValue,actualValue,tol,msg)  
!!         IMPLICIT NONE  
!!         REAL, INTENT(IN), DIMENSION(:) :: expectedValue,actualValue
!!         REAL, INTENT(IN)               :: tol
!!         CHARACTER(LEN=*), OPTIONAL     :: msg
!!         
!!      SUBROUTINE assertWithinToleranceTwoRealArrays2D(expectedValue,actualValue,tol)  
!!         IMPLICIT NONE  
!!         REAL, INTENT(IN), DIMENSION(:,:) :: expectedValue,actualValue
!!         REAL, INTENT(IN)                 :: tol
!!         
!!      SUBROUTINE assertWithinToleranceTwoDouble(expectedValue,actualValue,tol,msg)  
!!         IMPLICIT NONE  
!!         DOUBLE PRECISION, INTENT(in) :: expectedValue,actualValue,tol
!!         CHARACTER(LEN=*), OPTIONAL   :: msg
!!         
!!      SUBROUTINE assertWithinToleranceTwoDoubleArrays1D(expectedValue,actualValue,tol,msg)  
!!         IMPLICIT NONE  
!!         DOUBLE PRECISION, INTENT(IN), DIMENSION(:) :: expectedValue,actualValue
!!         DOUBLE PRECISION, INTENT(IN)               :: tol
!!         CHARACTER(LEN=*), OPTIONAL                 :: msg
!!         
!!      SUBROUTINE assertWithinToleranceTwoDoubleArrays2D(expectedValue,actualValue,tol)  
!!         IMPLICIT NONE  
!!         DOUBLE PRECISION, INTENT(IN), DIMENSION(:,:) :: expectedValue,actualValue
!!         DOUBLE PRECISION, INTENT(IN)                 :: tol
!!         
!!      SUBROUTINE assertEqualString(expectedValue,actualValue,msg)
!!         IMPLICIT NONE
!!         CHARACTER(LEN=*)           :: expectedValue,actualValue
!!         CHARACTER(LEN=*), OPTIONAL :: msg
!!         
!!      SUBROUTINE assertEqualTwoLogicals(expectedValue,actualValue,msg)  
!!         IMPLICIT NONE  
!!         LOGICAL, INTENT(in)        :: expectedValue,actualValue
!!         CHARACTER(LEN=*), OPTIONAL :: msg
!!
!!Notice that you can only check the equality of two floating point numbers to within some tolerance.
!
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
      PUBLIC :: detachSharedAssertionsManager
!
!     -------
!     Private
!     -------
!
      TYPE FTAssertionFailureRecord
         CHARACTER(LEN=FT_ASSERTION_STRING_LENGTH) :: msg, expected, actual
         CHARACTER(LEN=FT_ASSERTION_STRING_LENGTH) :: assertionType
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
      SUBROUTINE detachSharedAssertionsManager 
         IMPLICIT NONE
!
!     --------------------------------------------------------------------------
!     To create a new sharedAssertionsManager, 
!     call this procedure after storing a pointer to the sharedAssertionsManager
!     and before initializing again.
!     --------------------------------------------------------------------------
!
         sharedManager => NULL()
      END SUBROUTINE detachSharedAssertionsManager
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
      SUBROUTINE addAssertionFailureForParameters(msg,expected,actual, assertionType)
         IMPLICIT NONE  
         CHARACTER(LEN=*)                        :: msg, expected, actual, assertionType
         TYPE(FTAssertionFailureRecord), POINTER :: newFailure
         
         ALLOCATE(newFailure)
         newFailure % msg           = TRIM(msg)
         newFailure % expected      = TRIM(ADJUSTL(expected))
         newFailure % actual        = TRIM(ADJUSTL(actual))
         newFailure % assertionType = assertionType
         newFailure % next          => NULL()
         
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
        WRITE(iUnit,*) "   Summary of failed tests for test suite: ",TRIM(title)
        WRITE(iUnit,'(3x,i3,A,i5,A)')  self % numberOfAssertionFailures()," failures out of ", &
                              self % numberOfAssertions()," assertions." 
        WRITE(iUnit,*) "   -------------------------------------------------------------"
                  
         current => self % failureListHead
         DO WHILE (ASSOCIATED(current))
            WRITE(iUnit,*) "   ",TRIM(current % assertionType)
            WRITE(iUnit,*) "      ",TRIM(current % msg)
            WRITE(iUnit,*) "      ","Expected [",TRIM(current % expected),&
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
            IF ( PRESENT(msg) )     THEN
               CALL addAssertionFailureForParameters(msg,"True","False","Logical assertion failed: ")
            ELSE 
               CALL addAssertionFailureForParameters("","True","False","Logical assertion failed: ")
            END IF 
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
            IF ( PRESENT(msg) )     THEN
               CALL addAssertionFailureForParameters(msg,expected,actual,"Integer equality failed: ")
            ELSE 
               CALL addAssertionFailureForParameters("",expected,actual,"Integer equality failed: ")
            END IF 
         END IF 
         
      END SUBROUTINE assertEqualTwoIntegers    
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE assertEqualTwoIntegerArrays1D(expectedValue,actualValue)  
         IMPLICIT NONE  
         INTEGER, INTENT(in)    , DIMENSION(:)     :: expectedValue,actualValue
!         CHARACTER(LEN=FT_ASSERTION_STRING_LENGTH) :: expected,actual
         
         IF(.NOT.ASSOCIATED(sharedManager)) THEN
            CALL initializeSharedAssertionsManager
         END IF 
         
         sharedManager % numberOfTests_ = sharedManager % numberOfTests_ + 1
         IF ( .NOT.isEqual(expectedValue,actualValue) )     THEN
             
             PRINT *, "assertEqualTwoIntegerArrays1D not implemented"
         END IF 
         
      END SUBROUTINE assertEqualTwoIntegerArrays1D
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE assertEqualTwoIntegerArrays2D(expectedValue,actualValue)  
         IMPLICIT NONE  
         INTEGER, INTENT(in)    , DIMENSION(:,:)          :: expectedValue,actualValue
         
         IF(.NOT.ASSOCIATED(sharedManager)) THEN
            CALL initializeSharedAssertionsManager
         END IF 
         
         sharedManager % numberOfTests_ = sharedManager % numberOfTests_ + 1
         IF ( .NOT.isEqual(expectedValue,actualValue) )     THEN
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
            IF ( PRESENT(msg) )     THEN
               CALL addAssertionFailureForParameters(msg,expectedS,actualS,"Real equality failed: ")
            ELSE 
               CALL addAssertionFailureForParameters("",expectedS,actualS,"Real equality failed: ")
            END IF 
         END IF 
         
      END SUBROUTINE assertWithinToleranceTwoReal    
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE assertWithinToleranceTwoRealArrays1D(expectedValue,actualValue,tol,msg)  
         IMPLICIT NONE  
         REAL, INTENT(IN), DIMENSION(:) :: expectedValue,actualValue
         REAL, INTENT(IN)               :: tol
         CHARACTER(LEN=*), OPTIONAL     :: msg
         INTEGER                        :: k
         
         CHARACTER(LEN=FT_ASSERTION_STRING_LENGTH) :: expected,actual
         
         IF(.NOT.ASSOCIATED(sharedManager)) THEN
            CALL initializeSharedAssertionsManager
         END IF 
         
         sharedManager % numberOfTests_ = sharedManager % numberOfTests_ + 1
         IF ( .NOT.isEqual(expectedValue,actualValue,tol) )     THEN
            DO k = 1, SIZE(expectedValue)
               WRITE(expected,*) expectedValue(k)
               WRITE(actual,*)   actualValue(k)
               IF ( PRESENT(msg) )     THEN
                  CALL addAssertionFailureForParameters(msg,expected,actual,"Real Array equality failed: ")
               ELSE 
                  CALL addAssertionFailureForParameters("",expected,actual,"Real Array equality failed: ")
               END IF 
            END DO  
         END IF 
         
      END SUBROUTINE assertWithinToleranceTwoRealArrays1D
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE assertWithinToleranceTwoRealArrays2D(expectedValue,actualValue,tol)  
         IMPLICIT NONE  
         REAL, INTENT(IN), DIMENSION(:,:) :: expectedValue,actualValue
         REAL, INTENT(IN)                 :: tol
         
         IF(.NOT.ASSOCIATED(sharedManager)) THEN
            CALL initializeSharedAssertionsManager
         END IF 
         
         sharedManager % numberOfTests_ = sharedManager % numberOfTests_ + 1
         IF ( .NOT.isEqual(expectedValue,actualValue,tol) )     THEN
             PRINT *, "assertWithinToleranceTwoRealArrays2D not implemented"
         END IF 
         
      END SUBROUTINE assertWithinToleranceTwoRealArrays2D
!@mark -
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE assertWithinToleranceTwoDouble(expectedValue,actualValue,tol,msg)  
         IMPLICIT NONE  
         DOUBLE PRECISION, INTENT(in) :: expectedValue,actualValue,tol
         CHARACTER(LEN=*), OPTIONAL   :: msg

         CHARACTER(LEN=FT_ASSERTION_STRING_LENGTH) :: expected,actual
         
         IF(.NOT.ASSOCIATED(sharedManager)) THEN
            CALL initializeSharedAssertionsManager
         END IF 
         
         sharedManager % numberOfTests_ = sharedManager % numberOfTests_ + 1
         IF ( .NOT.isEqual(expectedValue,actualValue,tol) )     THEN
            WRITE(expected,*) expectedValue
            WRITE(actual,*) actualValue
            IF ( PRESENT(msg) )     THEN
               CALL addAssertionFailureForParameters(msg,expected,actual, "Double Precision equality failed: ")
            ELSE 
               CALL addAssertionFailureForParameters("",expected,actual, "Double Precision equality failed: ")
            END IF 
         END IF 
         
      END SUBROUTINE assertWithinToleranceTwoDouble    
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE assertWithinToleranceTwoDoubleArrays1D(expectedValue,actualValue,tol,msg)  
         IMPLICIT NONE  
         DOUBLE PRECISION, INTENT(IN), DIMENSION(:) :: expectedValue,actualValue
         DOUBLE PRECISION, INTENT(IN)               :: tol
         CHARACTER(LEN=*), OPTIONAL                 :: msg
         INTEGER                                    :: code
         INTEGER                                    :: k
         
         CHARACTER(LEN=FT_ASSERTION_STRING_LENGTH) :: expected,actual,eMsg
         
         IF(.NOT.ASSOCIATED(sharedManager)) THEN
            CALL initializeSharedAssertionsManager
         END IF 
         
         sharedManager % numberOfTests_ = sharedManager % numberOfTests_ + 1
         IF ( .NOT.isEqual(expectedValue,actualValue,tol,code) )     THEN
            IF ( PRESENT(msg) )     THEN
               eMsg = TRIM(msg) // "---" // TRIM(compareCodeStrings(code))
            ELSE 
               eMsg = "---" // TRIM(compareCodeStrings(code))
            END IF 
            
            DO k = 1, SIZE(expectedValue)
               WRITE(expected,*) expectedValue(k)
               WRITE(actual,*)   actualValue(k)
               CALL addAssertionFailureForParameters(eMsg,expected,actual,"Double Precision 1D Array equality failed: ")
            END DO  
         END IF 
         
      END SUBROUTINE assertWithinToleranceTwoDoubleArrays1D
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE assertWithinToleranceTwoDoubleArrays2D(expectedValue,actualValue,tol)  
         IMPLICIT NONE  
         DOUBLE PRECISION, INTENT(IN), DIMENSION(:,:) :: expectedValue,actualValue
         DOUBLE PRECISION, INTENT(IN)                 :: tol
         INTEGER                         :: code
         
         IF(.NOT.ASSOCIATED(sharedManager)) THEN
            CALL initializeSharedAssertionsManager
         END IF 
         
         sharedManager % numberOfTests_ = sharedManager % numberOfTests_ + 1
         IF ( .NOT.isEqual(expectedValue,actualValue,tol,code) )     THEN
             PRINT *, "assertWithinToleranceTwoDoubleArrays2D not implemented"
        END IF 
         
      END SUBROUTINE assertWithinToleranceTwoDoubleArrays2D
!@mark -
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE assertEqualString(expectedValue,actualValue,msg)
         IMPLICIT NONE
         CHARACTER(LEN=*)           :: expectedValue,actualValue
         CHARACTER(LEN=*), OPTIONAL :: msg
         
         IF(.NOT.ASSOCIATED(sharedManager)) THEN
            CALL initializeSharedAssertionsManager
         END IF 
         
         sharedManager % numberOfTests_ = sharedManager % numberOfTests_ + 1
         IF ( .NOT.isEqual(expectedValue,actualValue) )     THEN
            IF ( PRESENT(msg) )     THEN
               CALL addAssertionFailureForParameters(msg,expectedValue,actualValue,"String equality failed: ")
            ELSE 
               CALL addAssertionFailureForParameters("",expectedValue,actualValue,"String equality failed: ")
            END IF 
         END IF 
         
      END SUBROUTINE assertEqualString
!@mark -
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE assertEqualTwoLogicals(expectedValue,actualValue,msg)  
         IMPLICIT NONE  
         LOGICAL, INTENT(in)        :: expectedValue,actualValue
         CHARACTER(LEN=*), OPTIONAL :: msg

         CHARACTER(LEN=FT_ASSERTION_STRING_LENGTH) :: expected,actual
         
         IF(.NOT.ASSOCIATED(sharedManager)) THEN
            CALL initializeSharedAssertionsManager
         END IF 
         
         sharedManager % numberOfTests_ = sharedManager % numberOfTests_ + 1
         IF ( .NOT.(expectedValue .EQV. actualValue) )     THEN
            WRITE(expected,*) expectedValue
            WRITE(actual,*)   actualValue
            IF ( PRESENT(msg) )     THEN
               CALL addAssertionFailureForParameters(msg,expected,actual,"Logical equality failed: ")
            ELSE 
               CALL addAssertionFailureForParameters(msg,expected,actual,"Logical equality failed: ")
            END IF 
         END IF 
         
      END SUBROUTINE assertEqualTwoLogicals    
       
      END Module FTAssertions    
