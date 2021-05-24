! MIT License

! Copyright (c) 2010-present David A. Kopriva and other contributors: AUTHORS.md

! Permission is hereby granted, free of charge, to any person obtaining a copy  
! of this software and associated documentation files (the "Software"), to deal  
! in the Software without restriction, including without limitation the rights  
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell  
! copies of the Software, and to permit persons to whom the Software is  
! furnished to do so, subject to the following conditions:

! The above copyright notice and this permission notice shall be included in all  
! copies or substantial portions of the Software.

! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR  
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE  
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER  
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,  
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE  
! SOFTWARE.

! FTObjectLibrary contains code that, to the best of our knowledge, has been released as
! public domain software:
! * `b3hs_hash_key_jenkins`: originally by Rich Townsend, 
! https://groups.google.com/forum/#!topic/comp.lang.fortran/RWoHZFt39ng, 2005
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
         CLASS(FTValue) , POINTER :: v
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
         CALL FTAssertEqual(N,hashTable % hashTableSize(),"Table size size")
         
         obj => hashTable % objectForKeys(2,3)
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
         CALL hashTable % addObjectForKeys(obj,2,3)
         CALL FTAssertEqual(2,v % refcount(),"Add object to table reference count")
         
         t = hashTable % containsKeys(2,3)
         CALL FTAssertEqual(.TRUE.,t,"contains value for key")
         
         obj   => hashTable % objectforKeys(2,3)
         vTest => valueFromObject(obj)
         CALL FTAssertEqual(42,vTest % integerValue(),"Table entry retrieval")

         CALL hashTable % destruct()
         CALL FTAssertEqual(1, v % refCount(),"Table release object refCount")
         CALL release(v)
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
               CALL release(v)
                
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
               obj   => hashTable % objectForKeys(h1,h2)
               vTest => valueFromObject(obj)
               
               CALL FTAssertEqual(1  , vTest % refCount(),"Table add refCount")
               CALL FTAssertEqual(i+j, vTest % integerValue(), "Table entry value")
               
            END DO   
         END DO
         CALL vTest % retain()
         CALL FTAssertEqual(2  , vTest % refCount(),"Retain refCount")

         CALL hashTable % destruct()
         CALL FTAssertEqual(1, vTest % refCount(),"Table release object refCount")
         CALL release(vTest)
         IF ( ASSOCIATED(vTest) )     THEN
            CALL FTAssert(.FALSE.,"Release object count") 
         END IF 
         
      END SUBROUTINE HashTableTests
