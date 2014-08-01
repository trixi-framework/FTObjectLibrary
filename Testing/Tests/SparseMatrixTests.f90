!
!////////////////////////////////////////////////////////////////////////
!
!      SparseMatrixTests.f90
!      Created: July 29, 2013 2:17 PM 
!      By: David Kopriva  
!
!////////////////////////////////////////////////////////////////////////
! 
      SUBROUTINE SparseMatrixTests  
         USE FTValueClass
         USE FTAssertions
         USE FTSparseMatrixClass
         IMPLICIT NONE
!
!        --------------------------------
!        We will store values in a matrix
!        --------------------------------
!
         CLASS(FTValue) , POINTER :: v
         CLASS(FTValue) , POINTER :: vTest
         CLASS(FTObject), POINTER :: obj
         
         TYPE(FTSparseMatrix) :: SparseMatrix
         
         INTEGER :: i, j, N
         INTEGER :: h1, h2
         LOGICAL :: t
!
!        -------------------------------------
!        initialize a table with four elements
!        -------------------------------------
!
         N = 4
         CALL SparseMatrix % initWithSize(4)
         CALL FTAssertEqual(N,SparseMatrix % SparseMatrixSize(),"Table size size")
         
         obj => SparseMatrix % objectForKeys(2,3)
         CALL FTAssertEqual(.FALSE.,ASSOCIATED(obj),"Empty table test")
!
!        ---------------------------------------------------------
!        Add an object to the table, retrieve it and then destroy 
!        the table
!        ---------------------------------------------------------
!
         ALLOCATE(v)
         CALL v % initWithValue(42)
         
         obj => v
         CALL SparseMatrix % addObjectForKeys(obj,2,3)
         CALL FTAssertEqual(2,v % refcount(),"Add object to table reference count")
         
         t = SparseMatrix % containsKeys(2,3)
         CALL FTAssertEqual(.TRUE.,t,"contains value for key")
         
         obj   => SparseMatrix % objectforKeys(2,3)
         vTest => valueFromObject(obj)
         CALL FTAssertEqual(42,vTest % integerValue(),"Table entry retrieval")

         CALL SparseMatrix % release()
         CALL FTAssertEqual(1, v % refCount(),"Table release object refCount")
         CALL v % release()
         DEALLOCATE(v)
!
!        ---------------------------------------------
!        Now create a hash table with a lot of entries
!        ---------------------------------------------
!
         CALL SparseMatrix % initWithSize(4)  
         DO j = 1,N
            DO i = 1,N
               h1 = Hash1([i,j])
               h2 = Hash2([i,j])
               
               ALLOCATE(v)
               CALL v % initWithValue(i+j)
               obj => v
               CALL SparseMatrix % addObjectForKeys(obj,h1,h2)
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
               h1    =  Hash1([i,j])
               h2    =  Hash2([i,j])
               obj   => SparseMatrix % objectForKeys(h1,h2)
               vTest => valueFromObject(obj)
               
               CALL FTAssertEqual(1  , vTest % refCount(),"Table add refCount")
               CALL FTAssertEqual(i+j, vTest % integerValue(), "Table entry value")
               
            END DO   
         END DO
         CALL vTest % retain()
         CALL FTAssertEqual(2  , vTest % refCount(),"Retain refCount")

         CALL SparseMatrix % release()
         CALL FTAssertEqual(1, vTest % refCount(),"Table release object refCount")
         CALL vTest % release()
         IF ( vTest % isUnreferenced() )     THEN
            DEALLOCATE(vTest)
         ELSE 
            CALL FTAssert(.FALSE.,"Release object count") 
         END IF 
         
      END SUBROUTINE SparseMatrixTests
