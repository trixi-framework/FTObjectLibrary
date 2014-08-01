!
!////////////////////////////////////////////////////////////////////////
!
!      SparseMatrixClass.f90
!      Created: July 29, 2013 10:59 AM 
!      By: David Kopriva  
!
!      The sparse matrix stores an FTObject pointer associated
!      with two keys (i,j) as a hash table. The size, N = the range of i.
!
!      * Definition (Subclass of FTObject) *
!
!               TYPE(FTSparseMatrix) :: SparseMatrix
!
!      * Initialization *
!
!               CALL SparseMatrix % initWithSize(N)
!
!      * Destruction *
!
!               CALL SparseMatrix % release()
!
!      * Adding an object *
!
!               CLASS(FTObject), POINTER :: obj
!               CALL SparseMatrix % addObjectForKeys(obj,i,j)
!
!      * Retrieving an object *
!
!               CLASS(FTObject), POINTER :: obj
!               obj => SparseMatrix % objectForKeys(i,j)
!
!            Be sure to retain the object if you want it to live
!            beyond the life of the table.
!
!      * Testing the presence of keys *
!
!               LOGICAL :: exists
!               exists = SparseMatrix % containsKeys(i,j)
!
!////////////////////////////////////////////////////////////////////////
!
      Module FTSparseMatrixData 
      USE FTObjectClass
      IMPLICIT NONE
!
!     ---------------
!     Type definition
!     ---------------
!
      TYPE, EXTENDS(FTObject) :: MatrixData
         INTEGER                  :: key
         CLASS(FTObject), POINTER :: object
!
!        ========
         CONTAINS
!        ========
!
         PROCEDURE :: initWithObjectAndKey
         PROCEDURE :: destruct => destructMatrixData
         
      END TYPE MatrixData
      
      INTERFACE cast
         MODULE PROCEDURE castObjectToMatrixData
      END INTERFACE cast
      
!
!     ========      
      CONTAINS
!     ========
!
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE initWithObjectAndKey(self,object,key)
!
!        ----------------------
!        Designated initializer
!        ----------------------
!
         IMPLICIT NONE
         CLASS(MatrixData)        :: self
         CLASS(FTObject), POINTER :: object
         INTEGER                  :: key
         
         CALL self % FTObject % init()
         
         self % key = key
         self % object => object
         CALL self % object % retain()
         
      END SUBROUTINE initWithObjectAndKey
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE destructMatrixData(self)
         IMPLICIT NONE  
         CLASS(MatrixData) :: self
         
         IF ( ASSOCIATED(self % object) )     THEN
            CALL self % object % release()
            IF ( self % object % isUnreferenced() )     THEN
               DEALLOCATE(self % object)
               self % object => NULL() 
            END IF 
         END IF 
         
         CALL self % FTObject % destruct

      END SUBROUTINE destructMatrixData
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE castObjectToMatrixData(obj,cast)  
         IMPLICIT NONE  
!
!        -----------------------------------------------------
!        Cast the base class FTObject to the FTException class
!        -----------------------------------------------------
!
         CLASS(FTObject)  , POINTER :: obj
         CLASS(MatrixData), POINTER :: cast
         
         cast => NULL()
         SELECT TYPE (e => obj)
            TYPE is (MatrixData)
               cast => e
            CLASS DEFAULT
               
         END SELECT
         
      END SUBROUTINE castObjectToMatrixData
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION matrixDataCast(obj)  RESULT(cast)
         IMPLICIT NONE  
!
!        -----------------------------------------------------
!        Cast the base class FTObject to the FTException class
!        -----------------------------------------------------
!
         CLASS(FTObject)  , POINTER :: obj
         CLASS(MatrixData), POINTER :: cast
         
         cast => NULL()
         SELECT TYPE (e => obj)
            TYPE is (MatrixData)
               cast => e
            CLASS DEFAULT
               
         END SELECT
         
      END FUNCTION matrixDataCast
      
      END Module FTSparseMatrixData
!@mark -
!
!////////////////////////////////////////////////////////////////////////
!
      Module FTSparseMatrixClass
      USE FTObjectClass
      USE FTLinkedListClass
      USE FTLinkedListIteratorClass
      USE FTSparseMatrixData
      IMPLICIT NONE
!
!     ----------------------
!     Class type definitions
!     ----------------------
!
      TYPE FTLinkedListPtr
         CLASS(FTLinkedList), POINTER :: list
      END TYPE FTLinkedListPtr
      PRIVATE :: FTLinkedListPtr
      
      TYPE, EXTENDS(FTObject) :: FTSparseMatrix
         TYPE(FTLinkedListPtr)     , DIMENSION(:), ALLOCATABLE :: table
         TYPE(FTLinkedListIterator), PRIVATE                   :: iterator
!
!        ========
         CONTAINS
!        ========
!
         PROCEDURE :: initWithSize     => initSparseMatrixWithSize
         PROCEDURE :: destruct         => destructSparseMatrix
         PROCEDURE :: containsKeys     => SparseMatrixContainsKeys
         PROCEDURE :: addObjectForKeys => addObjectToSparseMatrixForKeys
         PROCEDURE :: objectForKeys    => objectInSparseMatrixForKeys
         PROCEDURE :: SparseMatrixSize
         
      END TYPE FTSparseMatrix
      
!
!     ========
      CONTAINS
!     ========
!
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE initSparseMatrixWithSize(self,N)  
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(FTSparseMatrix) :: self
         INTEGER            :: N
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER :: j
         
         CALL self % FTObject % init()
         
         ALLOCATE(self % table(N))
         DO j = 1, N
            ALLOCATE(self % table(j) % list)
            CALL self % table(j) % list % init()
         END DO
         
         CALL self % iterator % init()
         
      END SUBROUTINE initSparseMatrixWithSize
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE addObjectToSparseMatrixForKeys(self,obj,i,j)
         IMPLICIT NONE  
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(FTSparseMatrix)       :: self
         CLASS(FTObject), POINTER :: obj
         INTEGER                  :: i,j
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(MatrixData), POINTER :: mData
         CLASS(FTObject)  , POINTER :: ptr
         
         IF ( .NOT.self % containsKeys(i,j) )     THEN
            ALLOCATE(mData)
            CALL mData % initWithObjectAndKey(obj,j)
            ptr => mData
            CALL self % table(i) % list % add(ptr)
            CALL mData % release()
         END IF 
         
      END SUBROUTINE addObjectToSparseMatrixForKeys
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION objectInSparseMatrixForKeys(self,i,j) RESULT(r)
!
!     ---------------------------------------------------------------
!     Returns the stored FTObject for the keys (i,j). Returns NULL()
!     if the object isn't in the table. Retain the object if it needs
!     a strong reference by the caller.
!     ---------------------------------------------------------------
!
         IMPLICIT NONE  
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(FTSparseMatrix)       :: self
         INTEGER                  :: i,j
         CLASS(FTObject), POINTER :: r
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(MatrixData)  , POINTER :: mData
         CLASS(FTObject)    , POINTER :: obj
         CLASS(FTLinkedList), POINTER :: list
         
         r    => NULL()
         IF(.NOT.ALLOCATED(self % table))     RETURN 
         list => self % table(i) % list
         IF(.NOT.ASSOCIATED(list))    RETURN 
         IF (  list % COUNT() == 0 )  RETURN
!
!        ----------------------------
!        Step through the linked list
!        ----------------------------
!
         r => NULL()
         
         CALL self % iterator % setLinkedList(self % table(i) % list)
         DO WHILE (.NOT.self % iterator % isAtEnd())
         
            obj => self % iterator % object()
            CALL cast(obj,mData)
            IF ( mData % key == j )     THEN
               r => mData % object
               EXIT 
            END IF 
            
            CALL self % iterator % moveToNext()
         END DO

      END FUNCTION objectInSparseMatrixForKeys
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION SparseMatrixContainsKeys(self,i,j)  RESULT(r)
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(FTSparseMatrix) :: self
         INTEGER                :: i, j
         LOGICAL                :: r
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(FTObject)    , POINTER :: obj
         CLASS(MatrixData)  , POINTER :: mData
         CLASS(FTLinkedList), POINTER :: list
         
         r = .FALSE.
         IF(.NOT.ALLOCATED(self % table))                RETURN 
         IF(.NOT.ASSOCIATED(self % table(i) % list))     RETURN
         IF ( self % table(i) % list % COUNT() == 0 )    RETURN 
!
!        ----------------------------
!        Step through the linked list
!        ----------------------------
!
         list => self % table(i) % list
         CALL self % iterator % setLinkedList(list)
         CALL self % iterator % setToStart()
         DO WHILE (.NOT.self % iterator % isAtEnd())
         
            obj => self % iterator % object()
            CALL cast(obj,mData)
            IF ( mData % key == j )     THEN
               r = .TRUE.
               RETURN  
            END IF 
            
            CALL self % iterator % moveToNext()
         END DO
         
      END FUNCTION SparseMatrixContainsKeys
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE destructSparseMatrix(self)
         IMPLICIT NONE  
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(FTSparseMatrix) :: self
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER :: j
         
         DO j = 1, SIZE(self % table)
            IF ( ASSOCIATED(self % table(j) % list) )     THEN
               CALL self % table(j) % list % release() 
               IF ( self % table(j) % list % isUnreferenced() )     THEN
                  DEALLOCATE(self % table(j) % list) 
               END IF 
            END IF 
         END DO

         IF(ALLOCATED(self % table))   DEALLOCATE(self % table)

         CALL self % iterator % release()
         
         CALL self % FTObject % destruct()
         
      END SUBROUTINE destructSparseMatrix
!
!//////////////////////////////////////////////////////////////////////// 
! 
      INTEGER FUNCTION SparseMatrixSize(self)  
         IMPLICIT NONE  
         CLASS(FTSparseMatrix) :: self
         IF ( ALLOCATED(self % table) )     THEN
            SparseMatrixSize =  SIZE(self % table)
         ELSE
            SparseMatrixSize = 0
         END IF 
      END FUNCTION SparseMatrixSize
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION SparseMatrixFromObject(obj) RESULT(cast)
!
!     -----------------------------------------------------
!     Cast the base class FTObject to the FTException class
!     -----------------------------------------------------
!
         IMPLICIT NONE  
         CLASS(FTObject)   , POINTER :: obj
         CLASS(FTSparseMatrix), POINTER :: cast
         
         cast => NULL()
         SELECT TYPE (e => obj)
            TYPE is (FTSparseMatrix)
               cast => e
            CLASS DEFAULT
               
         END SELECT
         
      END FUNCTION SparseMatrixFromObject
!
!////////////////////////////////////////////////////////////////////////
!
      INTEGER FUNCTION Hash1( idPair )
         INTEGER, DIMENSION(2) :: idPair
         Hash1 = MAXVAL(idPair)
      END FUNCTION Hash1
!
!////////////////////////////////////////////////////////////////////////
!
      INTEGER FUNCTION Hash2( idPair )
         INTEGER, DIMENSION(2) :: idPair
         Hash2 = MINVAL(idPair)
      END FUNCTION Hash2

      END Module FTSparseMatrixClass
