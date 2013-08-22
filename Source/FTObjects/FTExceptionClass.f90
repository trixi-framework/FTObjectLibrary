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
!            e % initFTException(severity,exceptionName,infoDictionary)
!
!         Setting components
!
!            e % setInfoDictionary(infoDictionary)
!
!////////////////////////////////////////////////////////////////////////
!
      Module FTExceptionClass
      USE FTStackClass
      USE FTDictionaryClass
      USE FTLinkedListIteratorClass
      IMPLICIT NONE
!
!     ----------------
!     Global constants
!     ----------------
!
      INTEGER, PARAMETER :: FT_ERROR_NONE = 0, FT_ERROR_WARNING = 1, FT_ERROR_FATAL = 2
      INTEGER, PARAMETER :: ERROR_MSG_STRING_LENGTH = 132
      INTEGER, PARAMETER, PRIVATE :: FTEXCEPTIONCLASS_OBJECT_CODE = 40
!
!     ---------------
!     Error base type
!     ---------------
!
      TYPE, EXTENDS(FTObject) :: FTException
         INTEGER, PRIVATE                                :: severity_
         CHARACTER(LEN=ERROR_MSG_STRING_LENGTH), PRIVATE :: exceptionName_
         CLASS(FTDictionary), POINTER, PRIVATE           :: infoDictionary_
!
!        --------         
         CONTAINS
!        --------         
!
         PROCEDURE :: initFTException
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
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE initFTException(self,severity,exceptionName,infoDictionary)  
         IMPLICIT NONE
         CLASS(FTException)                     :: self
         INTEGER                                :: severity
         CHARACTER(LEN=*)                       :: exceptionName
         CLASS(FTDictionary), POINTER, OPTIONAL :: infoDictionary
         
         CALL self % FTObject % init()
         CALL self % setObjectCode(FTEXCEPTIONCLASS_OBJECT_CODE)
         
         self % severity_        = severity
         self % exceptionName_   = exceptionName
         self % infoDictionary_  => NULL()
         IF(PRESENT(infoDictionary))   CALL self%setInfoDictionary(infoDictionary)
         
      END SUBROUTINE initFTException

!//////////////////////////////////////////////////////////////////////// 
 
      SUBROUTINE destructException(self)
         IMPLICIT NONE  
         CLASS(FTException) :: self
         
         IF(ASSOCIATED(self%infoDictionary_))   CALL releaseInfoDictionary(self)

         CALL self%FTObject%destruct
         
      END SUBROUTINE destructException 
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE setInfoDictionary( self, dict )  
         IMPLICIT NONE
         CLASS(FTException)           :: self
         CLASS(FTDictionary), POINTER :: dict
         
         CALL releaseInfoDictionary(self)
         self % infoDictionary_ => dict
         CALL self % infoDictionary_ % retain()
      END SUBROUTINE setInfoDictionary
!
!//////////////////////////////////////////////////////////////////////// 
! 
     FUNCTION infoDictionary(self)
        IMPLICIT NONE  
        CLASS(FTException) :: self
        CLASS(FTDictionary), POINTER :: infoDictionary
        
        infoDictionary => self%infoDictionary_
        
     END FUNCTION infoDictionary
!
!//////////////////////////////////////////////////////////////////////// 
! 
     FUNCTION exceptionName(self)  
        IMPLICIT NONE  
        CLASS(FTException) :: self
        CHARACTER(LEN=ERROR_MSG_STRING_LENGTH) :: exceptionName
        exceptionName = self%exceptionName_
     END FUNCTION exceptionName
!
!//////////////////////////////////////////////////////////////////////// 
! 
     INTEGER FUNCTION severity(self)  
        IMPLICIT NONE  
        CLASS(FTException) :: self
        severity = self%severity_
     END FUNCTION severity    
!
!//////////////////////////////////////////////////////////////////////// 
! 
     SUBROUTINE releaseInfoDictionary(self)  
         IMPLICIT NONE  
         CLASS(FTException) :: self
         
         IF(ASSOCIATED(self%infoDictionary_))     THEN
            CALL self%infodictionary_%release()
            IF ( self%infodictionary_%isUnreferenced() )     THEN
               DEALLOCATE(self%infodictionary_)
               self%infodictionary_ => NULL()
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
        
        CLASS(FTDictionary), POINTER :: dict
        
        WRITE(iUnit,*) "Exception Named: ", TRIM(self % exceptionName())
        dict => self%infoDictionary()
        CALL dict%printDescription(iUnit)
        
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
      CLASS(FTStack)    , POINTER, PRIVATE :: errorStack
      CLASS(FTException), POINTER, PRIVATE :: currentError_
      
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
         CALL errorStack%init()
         currentError_ => NULL()
      END SUBROUTINE initializeFTExceptions
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE destructFTExceptions  
         IMPLICIT NONE
         TYPE(FTLinkedListIterator)   :: iterator
         CLASS(FTLinkedList), POINTER :: list
         CLASS(FTObject)    , POINTER :: objectPtr
         CLASS(FTException) , POINTER :: e
!  
!        --------------------------------------------------
!        First see if there are any uncaught exceptions and
!        report them if there are.
!        --------------------------------------------------
!
         IF ( catch() )     THEN
           PRINT *
           PRINT *,"***********************************"
           IF(errorStack%COUNT() == 1)     THEN
              PRINT *, "An uncaught exception was raised:"
           ELSE
              PRINT *, "Uncaught exceptions were raised:"
           END IF
           PRINT *,"***********************************"
           PRINT *
           
           CALL printAllExceptions
            
         END IF 
!
!        -----------------------
!        Destruct the exceptions
!        -----------------------
!
         CALL errorStack%destruct()
         DEALLOCATE(errorStack)
         currentError_ => NULL()
      END SUBROUTINE destructFTExceptions
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE throw(thisError)  
         IMPLICIT NONE  
         CLASS(FTException), POINTER :: thisError
         CLASS(FTObject)   , POINTER :: ptr
         
         IF ( .NOT.ASSOCIATED(errorStack) )     THEN
            CALL initializeFTExceptions 
         END IF 
         
         ptr => thisError
         CALL errorStack%push(ptr)
         
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
         IF ( errorStack%count() > 0 )     THEN
            catchAll = .true.
         END IF
         currentError_ => NULL()
      END FUNCTION catchAll
!
!//////////////////////////////////////////////////////////////////////// 
! 
      INTEGER FUNCTION errorCount()  
         IMPLICIT NONE
                  
         IF ( .NOT.ASSOCIATED(errorStack) )     THEN
            CALL initializeFTExceptions 
         END IF 

         errorCount = errorStack%count() 
      END FUNCTION    
!
!//////////////////////////////////////////////////////////////////////// 
! 
      LOGICAL FUNCTION catchErrorWithName(errorName)
         IMPLICIT NONE  
         CHARACTER(LEN=*) :: errorName
         
         TYPE(FTLinkedListIterator)   :: iterator
         CLASS(FTLinkedList), POINTER :: ptr
         CLASS(FTObject)    , POINTER :: obj
         CLASS(FTException) , POINTER :: e
         
         catchErrorWithName = .false.
                  
         IF ( .NOT.ASSOCIATED(errorStack) )     THEN
            CALL initializeFTExceptions 
            RETURN 
         END IF 
         
         IF ( errorStack%COUNT() == 0 )     THEN
            RETURN 
         END IF 

         ptr => errorStack
         CALL iterator%initWithFTLinkedList(ptr)
         CALL iterator%setToStart
         
         DO WHILE (.NOT.iterator%isAtEnd())
            obj => iterator%object()
            CALL cast(obj,e)
            IF ( e%exceptionName() == errorName )     THEN
               currentError_ => e
               catchErrorWithName = .true.
               CALL currentError_%retain()
               CALL errorStack%remove(obj)
               EXIT
           END IF 
           CALL iterator%moveToNext()
         END DO
         
         CALL iterator%destruct
         
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
      FUNCTION popLastException()
         IMPLICIT NONE  
         CLASS(FTException), POINTER :: popLastException
         CLASS(FTObject)   , POINTER :: obj
         
         obj => NULL()
         IF ( .NOT.ASSOCIATED(errorStack) )     THEN
            CALL initializeFTExceptions 
         ELSE
            CALL errorStack%pop(obj)
            CALL cast(obj,popLastException)
         END IF 
         
      END FUNCTION popLastException
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION peekLastException()  
         IMPLICIT NONE  
         CLASS(FTException), POINTER :: peekLastException
         CLASS(FTObject)   , POINTER :: obj
         
         IF ( .NOT.ASSOCIATED(errorStack) )     THEN
            CALL initializeFTExceptions 
         END IF 
                  
         obj => errorStack%peek()
         CALL cast(obj,peekLastException)
         
      END FUNCTION peekLastException
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE printAllExceptions  
         IMPLICIT NONE  
         TYPE(FTLinkedListIterator)   :: iterator
         CLASS(FTLinkedList), POINTER :: list
         CLASS(FTObject)    , POINTER :: objectPtr
         CLASS(FTException) , POINTER :: e
           
        list => errorStack
        CALL iterator%initWithFTLinkedList(list)
!
!       ----------------------------------------------------
!       Write out the descriptions of each of the exceptions
!       ----------------------------------------------------
!
        CALL iterator%setToStart
        DO WHILE (.NOT.iterator%isAtEnd())
            objectPtr => iterator%object()
            CALL cast(objectPtr,e)
            CALL e%printDescription(6)
            CALL iterator%moveToNext()
         END DO
         
         CALL iterator%release
         IF ( iterator%isUnreferenced() )     THEN
            !iterator is not a pointer
         END IF
            
      END SUBROUTINE printAllExceptions

      END MODULE SharedExceptionManagerModule    
      