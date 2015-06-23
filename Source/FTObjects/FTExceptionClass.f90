!
!////////////////////////////////////////////////////////////////////////
!
!      FTExceptionClass.f90
!      Created: January 29, 2013 5:06 PM 
!      By: David Kopriva  
!
!      An FTException object gives a way to pass generic
!      information about an exceptional situation. Methods for
!      dealing with exceptions are defined in the SharedExceptionManagerModule
!      module.
!
!      An FTException object wraps:
!      (1) A severity indicator
!      (2) A name for the exception
!      (3) An optional dictionary that contains whatever information is deemed necessary.
!
!      It is expected that classes will define exceptions that use instances
!      of the FTException Class.
!
!      Defined constants:
!
!         FT_ERROR_NONE    = 0
!         FT_ERROR_WARNING = 1
!         FT_ERROR_FATAL   = 2
!
!      Usage:
!
!         Initialization
!
!            e  %  initFTException(severity,exceptionName,infoDictionary)
!
!            Plus the convenience initializers, which automatically 
!            create a FTValueDictionary with a single key called "message":
!
!            e % initWarningException(msg = "message")
!            e % initFatalException(msg = "message")
!
!            Plus an assertion exception
!
!            e % initAssertionFailureException(msg,expectedValueObject,observedValueObject,level)
!
!         Setting components
!
!            e  %  setInfoDictionary(infoDictionary)
!
!////////////////////////////////////////////////////////////////////////
!
      Module FTExceptionClass
      USE FTStackClass
      USE FTDictionaryClass
      USE FTValueDictionaryClass
      USE FTLinkedListIteratorClass
      IMPLICIT NONE
!
!     ----------------
!     Global constants
!     ----------------
!
      INTEGER, PARAMETER :: FT_ERROR_NONE = 0, FT_ERROR_WARNING = 1, FT_ERROR_FATAL = 2
      INTEGER, PARAMETER :: ERROR_MSG_STRING_LENGTH = 132
      
      CHARACTER(LEN=21), PARAMETER :: FTFatalErrorException       = "FTFatalErrorException"
      CHARACTER(LEN=23), PARAMETER :: FTWarningErrorException     = "FTWarningErrorException"
      CHARACTER(LEN=27), PARAMETER :: FTAssertionFailureException = "FTAssertionFailureException"
!
!     ---------------
!     Error base type
!     ---------------
!
      TYPE, EXTENDS(FTObject) :: FTException
         INTEGER, PRIVATE                                :: severity_
         CHARACTER(LEN=ERROR_MSG_STRING_LENGTH), PRIVATE :: exceptionName_
         CLASS(FTDictionary), POINTER, PRIVATE           :: infoDictionary_ => NULL()
!
!        --------         
         CONTAINS
!        --------         
!
         PROCEDURE :: initFTException
         PROCEDURE :: initWarningException
         PROCEDURE :: initFatalException
         PROCEDURE :: initAssertionFailureException
         PROCEDURE :: destruct => destructException
         PROCEDURE :: setInfoDictionary
         PROCEDURE :: infoDictionary
         PROCEDURE :: exceptionName
         PROCEDURE :: severity
         PROCEDURE :: printDescription => printFTExceptionDescription
      END TYPE FTException
      
      PRIVATE :: releaseInfoDictionary
      
      INTERFACE cast
         MODULE PROCEDURE castToException
      END INTERFACE cast
!
!     ========      
      CONTAINS
!     ========
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE initWarningException(self,msg)  
!
!     ---------------------------------------------
!     A convenience initializer for a warning error 
!     that includes the key "message" in the
!     infoDictionary
!     --------------------------------------------
!
         IMPLICIT NONE
         CLASS(FTException)                     :: self
         CHARACTER(LEN=*)                       :: msg
         
         CLASS(FTValueDictionary), POINTER :: userDictionary => NULL()
         CLASS(FTDictionary)     , POINTER :: dictPtr        => NULL()
            
         ALLOCATE(userDictionary)
         CALL userDictionary % initWithSize(64)
         CALL userDictionary % addValueForKey(msg,"message")
         
         dictPtr => userDictionary
         CALL self % initFTException(severity       = FT_ERROR_WARNING,&
                                     exceptionName  = FTWarningErrorException,&
                                     infoDictionary = dictPtr)
         
         CALL userDictionary % release()
         
      END SUBROUTINE initWarningException
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE initFatalException(self,msg)  
!
!     ---------------------------------------------
!     A convenience initializer for a fatal error 
!     that includes the key "message" in the
!     infoDictionary
!     --------------------------------------------
!
         IMPLICIT NONE
         CLASS(FTException)                     :: self
         CHARACTER(LEN=*)                       :: msg
         
         CLASS(FTValueDictionary), POINTER :: userDictionary => NULL()
         CLASS(FTDictionary)     , POINTER :: dictPtr        => NULL()
            
         ALLOCATE(userDictionary)
         CALL userDictionary % initWithSize(8)
         CALL userDictionary % addValueForKey(msg,"message")
         
         dictPtr => userDictionary
         CALL self % initFTException(severity       = FT_ERROR_FATAL,&
                                     exceptionName  = FTFatalErrorException,&
                                     infoDictionary = dictPtr)
         
         CALL userDictionary % release()
         
      END SUBROUTINE initFatalException
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE initFTException(self,severity,exceptionName,infoDictionary)
!
!     -----------------------------------
!     The main initializer for the class 
!     -----------------------------------
!
         IMPLICIT NONE
         CLASS(FTException)                     :: self
         INTEGER                                :: severity
         CHARACTER(LEN=*)                       :: exceptionName
         CLASS(FTDictionary), POINTER, OPTIONAL :: infoDictionary
         
         CALL self  %  FTObject  %  init()
         
         self  %  severity_        = severity
         self  %  exceptionName_   = exceptionName
         self  %  infoDictionary_  => NULL()
         IF(PRESENT(infoDictionary) .AND. ASSOCIATED(infoDictionary))   THEN 
            CALL self % setInfoDictionary(infoDictionary)
         END IF 
         
      END SUBROUTINE initFTException
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE initAssertionFailureException(self,msg,expectedValueObject,observedValueObject,level)
!
!     ------------------------------------------------
!     A convenience initializer for an assertion error 
!     that includes the keys:
!
!     "message"
!     "expectedValue"
!     "observedValue"
!
!     in the infoDictionary
!
!     ------------------------------------------------
!
         IMPLICIT NONE
         CLASS(FTException)      :: self
         CLASS(FTValue), POINTER :: expectedValueObject, ObservedValueObject
         INTEGER                 :: level
         CHARACTER(LEN=*)        :: msg
         
         CLASS(FTValueDictionary), POINTER :: userDictionary => NULL()
         CLASS(FTDictionary)     , POINTER :: dictPtr        => NULL()
         CLASS(FTObject)         , POINTER :: objectPtr      => NULL()
            
         ALLOCATE(userDictionary)
         CALL userDictionary % initWithSize(8)
         CALL userDictionary % addValueForKey(msg,"message")
         objectPtr => expectedValueObject
         CALL userDictionary % addObjectForKey(object = objectPtr,key = "expectedValue")
         objectPtr => ObservedValueObject
         CALL userDictionary % addObjectForKey(object = objectPtr,key = "observedValue")
         
         dictPtr => userDictionary
         CALL self % initFTException(severity       = level,&
                                     exceptionName  = FTAssertionFailureException,&
                                     infoDictionary = dictPtr)
         
         CALL userDictionary % release()
         
      END SUBROUTINE initAssertionFailureException
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE destructException(self)
         IMPLICIT NONE  
         CLASS(FTException) :: self
         
         IF(ASSOCIATED(self % infoDictionary_))   CALL releaseInfoDictionary(self)

         CALL self % FTObject % destruct()
         
      END SUBROUTINE destructException 
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE setInfoDictionary( self, dict )  
         IMPLICIT NONE
         CLASS(FTException)           :: self
         CLASS(FTDictionary), POINTER :: dict
         
         CALL releaseInfoDictionary(self)
         self  %  infoDictionary_ => dict
         CALL self  %  infoDictionary_  %  retain()
      END SUBROUTINE setInfoDictionary
!
!//////////////////////////////////////////////////////////////////////// 
! 
     FUNCTION infoDictionary(self)
        IMPLICIT NONE  
        CLASS(FTException) :: self
        CLASS(FTDictionary), POINTER :: infoDictionary
        
        infoDictionary => self % infoDictionary_
        
     END FUNCTION infoDictionary
!
!//////////////////////////////////////////////////////////////////////// 
! 
     FUNCTION exceptionName(self)  
        IMPLICIT NONE  
        CLASS(FTException) :: self
        CHARACTER(LEN=ERROR_MSG_STRING_LENGTH) :: exceptionName
        exceptionName = self % exceptionName_
     END FUNCTION exceptionName
!
!//////////////////////////////////////////////////////////////////////// 
! 
     INTEGER FUNCTION severity(self)  
        IMPLICIT NONE  
        CLASS(FTException) :: self
        severity = self % severity_
     END FUNCTION severity    
!
!//////////////////////////////////////////////////////////////////////// 
! 
     SUBROUTINE releaseInfoDictionary(self)  
         IMPLICIT NONE  
         CLASS(FTException) :: self
         
         IF(ASSOCIATED(self % infoDictionary_))     THEN
            CALL self % infodictionary_ % release()
            IF ( self % infodictionary_ % isUnreferenced() )     THEN
               DEALLOCATE(self % infodictionary_)
               self % infodictionary_ => NULL()
            END IF
         END IF
     END SUBROUTINE releaseInfoDictionary
!
!//////////////////////////////////////////////////////////////////////// 
! 
     SUBROUTINE printFTExceptionDescription(self,iUnit)  
        IMPLICIT NONE  
        CLASS(FTException) :: self
        INTEGER            :: iUnit
        
        CLASS(FTDictionary), POINTER :: dict => NULL()
        
        WRITE(iUnit,*) "-------------"
        WRITE(iUnit,*) " "
        WRITE(iUnit,*) "Exception Named: ", TRIM(self  %  exceptionName())
        dict => self % infoDictionary()
        IF(ASSOCIATED(dict)) CALL dict % printDescription(iUnit)
        
     END SUBROUTINE printFTExceptionDescription     
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE castToException(obj,cast) 
!
!     -----------------------------------------------------
!     Cast the base class FTObject to the FTException class
!     -----------------------------------------------------
!
         IMPLICIT NONE  
         CLASS(FTObject)   , POINTER :: obj
         CLASS(FTException), POINTER :: cast
         
         cast => NULL()
         SELECT TYPE (e => obj)
            TYPE is (FTException)
               cast => e
            CLASS DEFAULT
               
         END SELECT
         
      END SUBROUTINE castToException
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION exceptionFromObject(obj) RESULT(cast)
!
!     -----------------------------------------------------
!     Cast the base class FTObject to the FTException class
!     -----------------------------------------------------
!
         IMPLICIT NONE  
         CLASS(FTObject)   , POINTER :: obj
         CLASS(FTException), POINTER :: cast
         
         cast => NULL()
         SELECT TYPE (e => obj)
            TYPE is (FTException)
               cast => e
            CLASS DEFAULT
               
         END SELECT
         
      END FUNCTION exceptionFromObject

      END Module FTExceptionClass
!
!//////////////////////////////////////////////////////////////////////// 
! 
!@mark -
     
      Module SharedExceptionManagerModule
      USE FTExceptionClass
      IMPLICIT NONE  
!
!     --------------------
!     Global error stack  
!     --------------------
!
      CLASS(FTStack)    , POINTER, PRIVATE :: errorStack    => NULL()
      CLASS(FTException), POINTER, PRIVATE :: currentError_ => NULL()
      
      INTERFACE catch
         MODULE PROCEDURE catchAll
         MODULE PROCEDURE catchErrorWithName
      END INTERFACE catch
      
      PRIVATE :: catchAll, catchErrorWithName
!
!     ========      
      CONTAINS
!     ========
!
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE initializeFTExceptions  
         IMPLICIT NONE
         ALLOCATE(errorStack)
         CALL errorStack % init()
         currentError_ => NULL()
      END SUBROUTINE initializeFTExceptions
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE destructFTExceptions  
         IMPLICIT NONE
!  
!        --------------------------------------------------
!        First see if there are any uncaught exceptions and
!        report them if there are.
!        --------------------------------------------------
!
         IF ( catch() )     THEN
           PRINT *
           PRINT *,"   ***********************************"
           IF(errorStack % COUNT() == 1)     THEN
              PRINT *, "   An uncaught exception was raised:"
           ELSE
              PRINT *, "   Uncaught exceptions were raised:"
           END IF
           PRINT *,"   ***********************************"
           PRINT *
                       
         END IF 
!
!        -----------------------
!        Destruct the exceptions
!        -----------------------
!
         IF ( ASSOCIATED(currentError_) )     THEN
            CALL currentError_ % release()
            IF ( currentError_ % isUnreferenced() )     THEN
               DEALLOCATE(currentError_) 
               currentError_ => NULL()
            END IF  
         END IF 
         
         CALL errorStack % release()

         IF ( errorStack % isUnreferenced() )     THEN
            DEALLOCATE(errorStack)
         END IF 
      END SUBROUTINE destructFTExceptions
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE throw(exceptionToThrow)  
         IMPLICIT NONE  
         CLASS(FTException), POINTER :: exceptionToThrow
         CLASS(FTObject)   , POINTER :: ptr => NULL()
         
         IF ( .NOT.ASSOCIATED(errorStack) )     THEN
            CALL initializeFTExceptions 
         END IF 
         
         ptr => exceptionToThrow
         CALL errorStack % push(ptr)
         
      END SUBROUTINE throw
!
!//////////////////////////////////////////////////////////////////////// 
! 
      LOGICAL FUNCTION catchAll()  
         IMPLICIT NONE
         
         IF ( .NOT.ASSOCIATED(errorStack) )     THEN
            CALL initializeFTExceptions 
         END IF 
         
         catchAll = .false.
         IF ( errorStack % count() > 0 )     THEN
            catchAll = .true.
         END IF
         
         IF ( ASSOCIATED(currentError_) )     THEN
            CALL currentError_ % release()
            IF ( currentError_ % isUnreferenced() )     THEN
               DEALLOCATE(currentError_) 
               currentError_ => NULL()
            END IF 
         END IF 
         
      END FUNCTION catchAll
!
!//////////////////////////////////////////////////////////////////////// 
! 
      INTEGER FUNCTION errorCount()  
         IMPLICIT NONE
                  
         IF ( .NOT.ASSOCIATED(errorStack) )     THEN
            CALL initializeFTExceptions 
         END IF 

         errorCount = errorStack % count() 
      END FUNCTION    
!
!//////////////////////////////////////////////////////////////////////// 
! 
      LOGICAL FUNCTION catchErrorWithName(exceptionName)
         IMPLICIT NONE  
         CHARACTER(LEN=*) :: exceptionName
         
         TYPE(FTLinkedListIterator)   :: iterator
         CLASS(FTLinkedList), POINTER :: ptr => NULL()
         CLASS(FTObject)    , POINTER :: obj => NULL()
         CLASS(FTException) , POINTER :: e   => NULL()
         
         catchErrorWithName = .false.
                  
         IF ( .NOT.ASSOCIATED(errorStack) )     THEN
            CALL initializeFTExceptions 
            RETURN 
         END IF 
         
         IF ( errorStack % COUNT() == 0 )     THEN
            RETURN 
         END IF 

         ptr => errorStack
         CALL iterator % initWithFTLinkedList(ptr)
         CALL iterator % setToStart()
         
         DO WHILE (.NOT.iterator % isAtEnd())
            obj => iterator % object()
            CALL cast(obj,e)
            IF ( e % exceptionName() == exceptionName )     THEN
               CALL setCurrentError(e)
               catchErrorWithName = .true.
               CALL errorStack % remove(obj)
               EXIT
           END IF 
           CALL iterator % moveToNext()
         END DO
         
         CALL iterator % destruct
         
      END FUNCTION catchErrorWithName
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION errorObject()  
         IMPLICIT NONE
         CLASS(FTException), POINTER :: errorObject
         
         IF ( .NOT.ASSOCIATED(errorStack) )     THEN
            CALL initializeFTExceptions 
         END IF 
         
         errorObject => currentError_
      END FUNCTION errorObject
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE setCurrentError(e)  
         IMPLICIT NONE  
         CLASS(FTException) , POINTER :: e
!
!        --------------------------------------------------------------
!        Check first to see if there is a current error. Since it
!        is retained, the current one must be released before resetting
!        the pointer.
!        --------------------------------------------------------------
!
         IF ( ASSOCIATED(POINTER = currentError_) )     THEN
            CALL currentError_ % release()
            IF ( currentError_ % isUnreferenced() )     THEN
               DEALLOCATE(currentError_) 
            END IF  
         END IF 
!
!        ------------------------------------
!        Set the pointer and retain ownership
!        ------------------------------------
!
         currentError_ => e
         CALL currentError_ % retain()
         
      END SUBROUTINE setCurrentError
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION popLastException()
         IMPLICIT NONE  
         CLASS(FTException), POINTER :: popLastException
         CLASS(FTObject)   , POINTER :: obj => NULL()
         
         obj => NULL()
         popLastException => NULL()
         IF ( .NOT.ASSOCIATED(errorStack) )     THEN
            CALL initializeFTExceptions 
         ELSE
            CALL errorStack % pop(obj)
            IF(ASSOCIATED(obj)) CALL cast(obj,popLastException)
         END IF 
         
      END FUNCTION popLastException
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION peekLastException()  
         IMPLICIT NONE  
         CLASS(FTException), POINTER :: peekLastException
         CLASS(FTObject)   , POINTER :: obj => NULL()
         
         IF ( .NOT.ASSOCIATED(errorStack) )     THEN
            CALL initializeFTExceptions 
         END IF 
         
         peekLastException => NULL()
         obj => errorStack % peek()
         CALL cast(obj,peekLastException)
         
      END FUNCTION peekLastException
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE printAllExceptions  
         IMPLICIT NONE  
         TYPE(FTLinkedListIterator)   :: iterator
         CLASS(FTLinkedList), POINTER :: list      => NULL()
         CLASS(FTObject)    , POINTER :: objectPtr => NULL()
         CLASS(FTException) , POINTER :: e         => NULL()
           
        list => errorStack
        CALL iterator % initWithFTLinkedList(list)
!
!       ----------------------------------------------------
!       Write out the descriptions of each of the exceptions
!       ----------------------------------------------------
!
        CALL iterator % setToStart
        DO WHILE (.NOT.iterator % isAtEnd())
            objectPtr => iterator % object()
            CALL cast(objectPtr,e)
            CALL e % printDescription(6)
            CALL iterator % moveToNext()
         END DO
         
         CALL iterator % release
         IF ( iterator % isUnreferenced() )     THEN
            !iterator is not a pointer
         END IF
            
      END SUBROUTINE printAllExceptions

      END MODULE SharedExceptionManagerModule    
      