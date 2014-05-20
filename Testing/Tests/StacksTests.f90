!
!////////////////////////////////////////////////////////////////////////
!
!      StacksTests.f90
!      Created: January 28, 2013 10:11 AM 
!      By: David Kopriva  
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE StackClassTests
         USE FTAssertions
         USE FTStackClass
         USE FTValueClass
         IMPLICIT NONE  
         
         CLASS(FTStack) , POINTER :: stack
         CLASS(FTValue) , POINTER :: r1, r2, r3
         CLASS(FTObject), POINTER :: objectPtr
!
!        ---------------
!        Initializations
!        ---------------
!
         ALLOCATE(stack)
         CALL stack%init
         CALL FTAssertEqual(1,stack%refCount(),"Reference Counting: Initial reference count")
!
!        ------------
!        Add an entry
!        ------------
!
         ALLOCATE(r1)
         CALL r1%initWithValue(3.14d0)
         objectPtr => r1
         CALL stack%push(objectPtr)
         CALL FTAssertEqual(1,stack%COUNT(),"Reference Counting: Initial push")
         
         CALL FTAssertEqual(2,r1%refCount(),"Reference Counting: Reference count on stored object")
         CALL r1%release()
         CALL FTAssertEqual(1,r1%refCount(),"Reference Counting: Release on stored object")
!
!        ------------
!        Second entry
!        ------------
!
         ALLOCATE(r2)
         CALL r2%initWithValue("r2 is a string")
         objectPtr => r2
         CALL stack%push(objectPtr)
         CALL r2%release()
         CALL FTAssertEqual(2,stack%count(),"Reference Counting: Stack size after push")
!
!        -----------
!        Third entry
!        -----------
!
         ALLOCATE(r3)
         CALL r3%initWithValue(17)
         objectPtr => r3
         CALL stack%push(objectPtr)
         CALL FTAssertEqual(3,stack%COUNT(),"Reference Counting: Stack size after push")
         CALL r3%release()
!
!        ------------
!        Peek and pop
!        ------------
!
         objectPtr => stack%peek()
         SELECT TYPE(objectPtr)
            TYPE is (FTValue)
               CALL FTAssertEqual(17,objectPtr%integerValue(),"Interger value stored at top of stack")
            CLASS DEFAULT
               PRINT *, "uncaught cast in stack object"
         END SELECT
         
         CALL stack%pop(objectPtr)
         SELECT TYPE(objectPtr)
            TYPE is (FTValue)
               CALL FTAssertEqual(17,objectPtr%integerValue(),"Incorrect integervalue popped from top of stack")
            CLASS DEFAULT
               CALL FTAssert(.false.,"Known type stored in linked list")
         END SELECT
         CALL FTAssertEqual(2,stack%COUNT(),"Stack count after popping")
!
!        ------------------------
!        Finish up with the stack
!        ------------------------
!
         CALL stack%release
         IF ( stack%isUnreferenced() )     THEN
            DEALLOCATE(stack) 
            stack => NULL()
         END IF 

      END SUBROUTINE StackClassTests    