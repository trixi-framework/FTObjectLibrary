!
!////////////////////////////////////////////////////////////////////////
!
!      HashTableTests.f90
!      Created: July 29, 2013 2:17 PM 
!      By: David Kopriva  
!
!////////////////////////////////////////////////////////////////////////
! 
      SUBROUTINE HashTableTests  
         USE FTValueClass
         USE FTAssertions
         USE FTHashTableClass
         IMPLICIT NONE
!
!        --------------------------------
!        We will store values in a matrix
!        --------------------------------
!
         TYPE(FTValue)  , POINTER :: v
         CLASS(FTValue) , POINTER :: vTest
         CLASS(FTObject), POINTER :: obj
         
         TYPE(FTHashTable) :: hashTable
         
         INTEGER :: i, j, N
         INTEGER :: h1, h2
         LOGICAL :: t
!
!        -------------------------------------
!        initialize a table with four elements
!        -------------------------------------
!
         N = 4
         CALL hashTable % initWithSize(4)
         CALL assertEqual(N,hashTable % hashTableSize(),"Table size size")
         
         obj => hashTable % objectForKeys(2,3)
         CALL assertEqual(.FALSE.,ASSOCIATED(obj),"Empty table test")
!
!        ---------------------------------------------------------
!        Add an object to the table, retrieve it and then destroy 
!        the table
!        ---------------------------------------------------------
!
         ALLOCATE(v)
         CALL v % initWithValue(42)
         
         obj => v
         CALL hashTable % addObjectForKeys(obj,2,3)
         CALL assertEqual(2,v % refcount(),"Add object to table reference count")
         
         t = hashTable % containsKeys(2,3)
         CALL assertEqual(.TRUE.,t,"contains value for key")
         
         obj => hashTable % objectforKeys(2,3)
         CALL cast(obj,vTest)
         CALL assertEqual(42,vTest % integerValue(),"Table entry retrieval")
         
!         The following fails because hashTable has been destructed. Must check for constructed.
!         CALL hashTable % release()
!         t = hashTable % containsKeys(2,3)
!         CALL assertEqual(.FALSE.,t,"Contains value for key after release")
         
         CALL hashTable % release()
         CALL assertEqual(1, v % refCount(),"Table release object refCount")
         CALL v % release()
         DEALLOCATE(v)
!
!        ---------------------------------------------
!        Now create a hash table with a lot of entries
!        ---------------------------------------------
!
         CALL hashTable % initWithSize(4)  
         DO j = 1,N
            DO i = 1,N
               h1 = Hash1([i,j])
               h2 = Hash2([i,j])
               
               ALLOCATE(v)
               CALL v % initWithValue(i+j)
               obj => v
               CALL hashTable % addObjectForKeys(obj,h1,h2)
               CALL v % release()
                
            END DO   
         END DO
!
!        ------------
!        Get them out
!        ------------
!
         DO j = 1,N
            DO i = 1,N
               h1 = Hash1([i,j])
               h2 = Hash2([i,j])
               obj => hashTable % objectForKeys(h1,h2)
               CALL cast(obj,vTest)
               
               CALL assertEqual(1  , vTest % refCount(),"Table add refCount")
               CALL assertEqual(i+j, vTest % integerValue(), "Table entry value")
               
            END DO   
         END DO
         CALL vTest % retain()
         CALL assertEqual(2  , vTest % refCount(),"Retain refCount")
         
         CALL hashTable % release()
         CALL assertEqual(1, vTest % refCount(),"Table release object refCount")
         CALL vTest % release()
         IF ( vTest % isUnreferenced() )     THEN
            DEALLOCATE(vTest)
         ELSE 
            CALL assert(.FALSE.,"Release object count") 
         END IF 
         
      END SUBROUTINE HashTableTests
