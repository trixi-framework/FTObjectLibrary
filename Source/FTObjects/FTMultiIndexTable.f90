!
!////////////////////////////////////////////////////////////////////////
!
!      MultiIndexTableClass.f90
!      Created: July 29, 2013 10:59 AM 
!      By: David Kopriva  
!
!      The sparse matrix stores an FTObject pointer associated
!      with two keys (i,j) as a hash table. The size, N = the range of i.
!
!      * Definition (Subclass of FTObject) *
!
!               TYPE(FTMultiIndexTable) :: multiIndexTable
!
!      * Initialization *
!
!               CALL MultiIndexTable % initWithSize(N)
!
!      * Destruction *
!
!               CALL MultiIndexTable % release()
!
!      * Adding an object *
!
!               CLASS(FTObject), POINTER :: obj
!               INTEGER, DIMENSION(dim)  :: keys
!               CALL MultiIndexTable % addObjectForKeys(obj,keys)
!
!      * Retrieving an object *
!
!               CLASS(FTObject), POINTER :: obj
!               INTEGER, DIMENSION(dim)  :: keys
!               obj => MultiIndexTable % objectForKeys(keys)
!
!            Be sure to retain the object if you want it to live
!            beyond the life of the table.
!
!      * Testing the presence of keys *
!
!               LOGICAL :: exists
!               exists = MultiIndexTable % containsKeys(keys)
!
!////////////////////////////////////////////////////////////////////////
!
      Module FTMultiIndexTableData 
      USE FTObjectClass
      IMPLICIT NONE
!
!     ---------------
!     Type definition
!     ---------------
!
      TYPE, EXTENDS(FTObject) :: MultiIndexMatrixData
         INTEGER        , ALLOCATABLE :: key(:)
         CLASS(FTObject), POINTER     :: object
!
!        ========
         CONTAINS
!        ========
!
         PROCEDURE :: initWithObjectAndKeys
         PROCEDURE :: destruct => destructMultiIndexMatrixData
         
      END TYPE MultiIndexMatrixData
      
      INTERFACE cast
         MODULE PROCEDURE castObjectToMultiIndexMatrixData
      END INTERFACE cast
      
!
!     ========      
      CONTAINS
!     ========
!
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE initWithObjectAndKeys(self,object,key)
!
!        ----------------------
!        Designated initializer
!        ----------------------
!
         IMPLICIT NONE
         CLASS(MultiIndexMatrixData) :: self
         CLASS(FTObject), POINTER    :: object
         INTEGER                     :: key(:)
         
         CALL self % FTObject % init()
         
         ALLOCATE(self % key(SIZE(key)))
         self % key = key
         self % object => object
         CALL self % object % retain()
         
      END SUBROUTINE initWithObjectAndKeys
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE destructMultiIndexMatrixData(self)
         IMPLICIT NONE  
         CLASS(MultiIndexMatrixData) :: self
         
         IF ( ASSOCIATED(self % object) )     THEN
            CALL self % object % release()
            IF ( self % object % isUnreferenced() )     THEN
               DEALLOCATE(self % object)
               self % object => NULL() 
            END IF 
         END IF 
         
         IF ( ALLOCATED(self % key) )     THEN
            DEALLOCATE(self % key) 
         END IF 
         
         CALL self % FTObject % destruct()

      END SUBROUTINE destructMultiIndexMatrixData
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE castObjectToMultiIndexMatrixData(obj,cast)  
         IMPLICIT NONE  
!
!        -----------------------------------------------------
!        Cast the base class FTObject to the FTException class
!        -----------------------------------------------------
!
         CLASS(FTObject)  , POINTER :: obj
         CLASS(MultiIndexMatrixData), POINTER :: cast
         
         cast => NULL()
         SELECT TYPE (e => obj)
            TYPE is (MultiIndexMatrixData)
               cast => e
            CLASS DEFAULT
               
         END SELECT
         
      END SUBROUTINE castObjectToMultiIndexMatrixData
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION MultiIndexMatrixDataCast(obj)  RESULT(cast)
         IMPLICIT NONE  
!
!        -----------------------------------------------------
!        Cast the base class FTObject to the FTException class
!        -----------------------------------------------------
!
         CLASS(FTObject)  , POINTER :: obj
         CLASS(MultiIndexMatrixData), POINTER :: cast
         
         cast => NULL()
         SELECT TYPE (e => obj)
            TYPE is (MultiIndexMatrixData)
               cast => e
            CLASS DEFAULT
               
         END SELECT
         
      END FUNCTION MultiIndexMatrixDataCast
      
      END Module FTMultiIndexTableData
!@mark -
!
!////////////////////////////////////////////////////////////////////////
!
      Module FTMultiIndexTableClass
      USE FTObjectClass
      USE FTLinkedListClass
      USE FTLinkedListIteratorClass
      USE FTMultiIndexTableData
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
      
      TYPE, EXTENDS(FTObject) :: FTMultiIndexTable
         TYPE(FTLinkedListPtr)     , DIMENSION(:), ALLOCATABLE :: table
         TYPE(FTLinkedListIterator), PRIVATE                   :: iterator
!
!        ========
         CONTAINS
!        ========
!
         PROCEDURE :: initWithSize     => initMultiIndexTableWithSize
         PROCEDURE :: destruct         => destructMultiIndexTable
         PROCEDURE :: containsKeys     => MultiIndexTableContainsKeys
         PROCEDURE :: addObjectForKeys => addObjectToMultiIndexTableForKeys
         PROCEDURE :: objectForKeys    => objectInMultiIndexTableForKeys
         PROCEDURE :: MultiIndexTableSize
         
      END TYPE FTMultiIndexTable
      
!
!     ========
      CONTAINS
!     ========
!
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE initMultiIndexTableWithSize(self,N)  
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(FTMultiIndexTable) :: self
         INTEGER                  :: N
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
         
      END SUBROUTINE initMultiIndexTableWithSize
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE addObjectToMultiIndexTableForKeys(self,obj,keys)
         IMPLICIT NONE  
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(FTMultiIndexTable) :: self
         CLASS(FTObject), POINTER :: obj
         INTEGER                  :: keys(:)
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(MultiIndexMatrixData), POINTER :: mData
         CLASS(FTObject)            , POINTER :: ptr
         INTEGER                              :: i
         INTEGER                              :: keysCopy(SIZE(keys))
         
         keysCopy = keys
         CALL sortKeysAscending(keysCopy)
         
         i = keysCopy(1)
         IF ( .NOT.self % containsKeys(keysCopy) )     THEN
            ALLOCATE(mData)
            CALL mData % initWithObjectAndKeys(obj,keysCopy)
            ptr => mData
            CALL self % table(i) % list % add(ptr)
            CALL mData % release()
         END IF 
         
      END SUBROUTINE addObjectToMultiIndexTableForKeys
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION objectInMultiIndexTableForKeys(self,keys) RESULT(r)
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
         CLASS(FTMultiIndexTable) :: self
         INTEGER                  :: keys(:)
         CLASS(FTObject), POINTER :: r
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(MultiIndexMatrixData)  , POINTER :: mData
         CLASS(FTObject)              , POINTER :: obj
         CLASS(FTLinkedList)          , POINTER :: list
         INTEGER                                :: i
         INTEGER                                :: keysCopy(SIZE(keys))
  
         keysCopy = keys
         CALL sortKeysAscending(keysCopy)
         
         r => NULL()
         i = keysCopy(1)
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
            IF ( keysMatch(key1 = mData % key,key2 = keysCopy) )     THEN
               r => mData % object
               EXIT 
            END IF 
            
            CALL self % iterator % moveToNext()
         END DO

      END FUNCTION objectInMultiIndexTableForKeys
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION MultiIndexTableContainsKeys(self,keys)  RESULT(r)
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(FTMultiIndexTable) :: self
         INTEGER                  :: keys(:)
         LOGICAL                  :: r
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(FTObject)              , POINTER :: obj
         CLASS(MultiIndexMatrixData)  , POINTER :: mData
         CLASS(FTLinkedList)          , POINTER :: list
         INTEGER                                :: i
         INTEGER                                :: keysCopy(SIZE(keys))

         keysCopy = keys
         CALL sortKeysAscending(keysCopy)
         
         r = .FALSE.
         i = keysCopy(1)
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
            IF ( keysMatch(key1 = mData % key,key2 = keysCopy))     THEN
               r = .TRUE.
               RETURN  
            END IF 
            
            CALL self % iterator % moveToNext()
         END DO
         
      END FUNCTION MultiIndexTableContainsKeys
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE destructMultiIndexTable(self)
         IMPLICIT NONE  
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(FTMultiIndexTable) :: self
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
         
      END SUBROUTINE destructMultiIndexTable
!
!//////////////////////////////////////////////////////////////////////// 
! 
      INTEGER FUNCTION MultiIndexTableSize(self)  
         IMPLICIT NONE  
         CLASS(FTMultiIndexTable) :: self
         IF ( ALLOCATED(self % table) )     THEN
            MultiIndexTableSize =  SIZE(self % table)
         ELSE
            MultiIndexTableSize = 0
         END IF 
      END FUNCTION MultiIndexTableSize
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION MultiIndexTableFromObject(obj) RESULT(cast)
!
!     -----------------------------------------------------
!     Cast the base class FTObject to the FTException class
!     -----------------------------------------------------
!
         IMPLICIT NONE  
         CLASS(FTObject)   , POINTER :: obj
         CLASS(FTMultiIndexTable), POINTER :: cast
         
         cast => NULL()
         SELECT TYPE (e => obj)
            TYPE is (FTMultiIndexTable)
               cast => e
            CLASS DEFAULT
               
         END SELECT
         
      END FUNCTION MultiIndexTableFromObject
!
!//////////////////////////////////////////////////////////////////////// 
! 
      LOGICAL FUNCTION keysMatch(key1,key2)  
         IMPLICIT NONE  
         INTEGER, DIMENSION(:) :: key1, key2
         INTEGER               :: match
         
         keysMatch = .FALSE.
         
         match = MAXVAL(ABS(key1 - key2))
         IF(match == 0) keysMatch = .TRUE.
         
      END FUNCTION keysMatch
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE sortKeysAscending(keys)  
         IMPLICIT NONE
         INTEGER, DIMENSION(:) :: keys
         INTEGER               :: i, j, N, t
         
         N = SIZE(keys)
         
         SELECT CASE ( N )
            CASE( 1 )
               return
            CASE( 2 ) 
               IF ( keys(1) > keys(2) )     THEN
                  t = keys(1)
                  keys(1) = keys(2)
                  keys(2) = t 
               END IF
            CASE DEFAULT 
               DO i = 2, N
                  t = keys(i)
                  j = i
                  DO WHILE( j > 1 .AND. keys(j-1) > t )
                     keys(j) = keys(j-1)
                     j = j - 1 
                  END DO 
                  keys(j) = t
               END DO  
         END SELECT 
          
      END SUBROUTINE sortKeysAscending

      END Module FTMultiIndexTableClass
