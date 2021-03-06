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
!      MultiIndexTableClass.f90
!      Created: July 29, 2013 10:59 AM 
!      By: David Kopriva  
!
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
         FINAL     :: destructMultiIndexMatrixData
         
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
      SUBROUTINE releaseFTMultiIndexMatrixData(self)  
         IMPLICIT NONE
         TYPE(MultiIndexMatrixData), POINTER :: self
         CLASS(FTObject)   , POINTER :: obj
          
         IF(.NOT. ASSOCIATED(self)) RETURN
        
         obj => self
         CALL release(obj) 
         IF(.NOT.ASSOCIATED(obj)) self => NULL()
      END SUBROUTINE releaseFTMultiIndexMatrixData
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE destructMultiIndexMatrixData(self)
         IMPLICIT NONE  
         TYPE(MultiIndexMatrixData) :: self
         
         IF ( ASSOCIATED(self % object) ) CALL releaseFTObject(self % object )
         IF ( ALLOCATED(self % key) )     DEALLOCATE(self % key) 

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
!>The MultiIndexTable stores an FTObject pointer associated
!>with any number of integer keys(:) as a hash table. 
!>
!>#Usage
!>## Definition (Subclass of FTObject)
!>
!>         TYPE(FTMultiIndexTable) :: multiIndexTable
!>
!>##Initialization
!>
!>         CALL MultiIndexTable % initWithSize(N)
!>
!>The size, N = the maximum value of all of the keys.
!>
!>## Destruction
!>
!>         CALL releaseFTMultiIndexTable(MultiIndexTable)     [Pointers]
!>
!>##Adding an object
!>
!>         CLASS(FTObject), POINTER :: obj
!>         INTEGER, DIMENSION(dim)  :: keys
!>         CALL MultiIndexTable % addObjectForKeys(obj,keys)
!>
!>##Retrieving an object
!>
!>         CLASS(FTObject), POINTER :: obj
!>         INTEGER, DIMENSION(dim)  :: keys
!>         obj => MultiIndexTable % objectForKeys(keys)
!>
!>Be sure to retain the object if you want it to live
!>      beyond the life of the table.
!>
!>##Testing the presence of keys
!>
!>         LOGICAL :: exists
!>         exists = MultiIndexTable % containsKeys(keys)
!
!////////////////////////////////////////////////////////////////////////
!
      Module FTMultiIndexTableClass
      USE FTObjectClass
      USE FTLinkedListClass
      USE FTMultiIndexTableData
      IMPLICIT NONE
!
!     ---------------------
!     Class type definition
!     ---------------------
!      
      TYPE, EXTENDS(FTObject) :: FTMultiIndexTable
         CLASS(FTLinkedList), DIMENSION(:), ALLOCATABLE :: table
!
!        ========
         CONTAINS
!        ========
!
         PROCEDURE :: initWithSize     => initMultiIndexTableWithSize
         FINAL     :: destructMultiIndexTable
         PROCEDURE :: containsKeys     => MultiIndexTableContainsKeys
         PROCEDURE :: addObjectForKeys => addObjectToMultiIndexTableForKeys
         PROCEDURE :: objectForKeys    => objectInMultiIndexTableForKeys
         PROCEDURE :: printDescription => printMultiIndexTableDescription
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
            CALL self % table(j) % init()
         END DO
         
      END SUBROUTINE initMultiIndexTableWithSize
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE releaseFTMultiIndexTable(self)  
         IMPLICIT NONE
         TYPE(FTMultiIndexTable), POINTER :: self
         CLASS(FTObject)        , POINTER :: obj
          
         IF(.NOT. ASSOCIATED(self)) RETURN
        
         obj => self
         CALL release(obj) 
         IF(.NOT.ASSOCIATED(obj)) self => NULL()
      END SUBROUTINE releaseFTMultiIndexTable
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
         TYPE(FTMultiIndexTable) :: self
!
!        ---------------
!        Local variables
!        ---------------
!         
         IF(ALLOCATED(self % table))   THEN
            DEALLOCATE(self % table)
         END IF
         
      END SUBROUTINE destructMultiIndexTable
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
         INTEGER                              :: orderedKeys(SIZE(keys))
         
         orderedKeys = keys
         CALL sortKeysAscending(orderedKeys)
         
         i = orderedKeys(1)
         IF ( .NOT.self % containsKeys(orderedKeys) )     THEN
            ALLOCATE(mData)
            CALL mData % initWithObjectAndKeys(obj,orderedKeys)
            ptr => mData
            
            CALL self % table(i) % add(ptr)
            CALL releaseFTObject(ptr)
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
         CLASS(FTLinkedListRecord)    , POINTER :: currentRecord
         INTEGER                                :: i
         INTEGER                                :: orderedKeys(SIZE(keys))
  
         orderedKeys = keys
         CALL sortKeysAscending(orderedKeys)
         
         r => NULL()
         i = orderedKeys(1)
         IF(.NOT.ALLOCATED(self % table))        RETURN 
         IF (  self % table(i) % COUNT() == 0 )  RETURN
!
!        ----------------------------
!        Step through the linked list
!        ----------------------------
!
         r => NULL()
         
         currentRecord => self % table(i) % head
         DO WHILE (ASSOCIATED(currentRecord))
         
            obj => currentRecord % recordObject
            CALL cast(obj,mData)
            IF ( keysMatch(key1 = mData % key,key2 = orderedKeys) )     THEN
               r => mData % object
               EXIT 
            END IF 
            
            currentRecord => currentRecord % next
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
         CLASS(FTLinkedListRecord)    , POINTER :: currentRecord
         INTEGER                                :: i
         INTEGER                                :: orderedKeys(SIZE(keys))

         orderedKeys = keys
         CALL sortKeysAscending(orderedKeys)
         
         r = .FALSE.
         i = orderedKeys(1)
         IF(.NOT.ALLOCATED(self % table))         RETURN 
         IF ( self % table(i) % COUNT() == 0 )    RETURN 
!
!        ----------------------------
!        Step through the linked list
!        ----------------------------
!
         currentRecord => self % table(i) % head
         DO WHILE (ASSOCIATED(currentRecord))
         
            obj => currentRecord % recordObject
            CALL cast(obj,mData)
            IF ( keysMatch(key1 = mData % key,key2 = orderedKeys))     THEN
               r = .TRUE.
               EXIT   
            END IF 
            
            currentRecord => currentRecord % next
         END DO
         
      END FUNCTION MultiIndexTableContainsKeys
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
         CLASS(FTObject)         , POINTER :: obj
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
!
!     ----------------------------------------------------
!     Use an insertion sort for the keys, since the number
!     of them should be small
!     ----------------------------------------------------
!
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
                     IF(j == 1) EXIT 
                  END DO 
                  keys(j) = t
               END DO  
         END SELECT 
          
      END SUBROUTINE sortKeysAscending
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE printMultiIndexTableDescription(self, iUnit)  
         IMPLICIT NONE  
         CLASS(FTMultiIndexTable) :: self
         INTEGER                  :: iUnit
         INTEGER                  :: i
         
         DO i = 1, SIZE(self % table)
            CALL self % table(i) % printDescription(iUnit) 
         END DO 
          
      END SUBROUTINE printMultiIndexTableDescription

      END Module FTMultiIndexTableClass
