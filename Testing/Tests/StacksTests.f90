!
!////////////////////////////////////////////////////////////////////////
!
!      StacksTests.f90
!      Created: January 28, 2013 10:11 AM 
!      By: David Kopriva  
!
!////////////////////////////////////////////////////////////////////////
!
      Module StackUnitTestsModule 
      IMPLICIT NONE
      CONTAINS  
 !
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE testStackInit(stack)  
         USE FTAssertions
         USE FTStackClass
         IMPLICIT NONE  
         
         CLASS(FTStack) , POINTER :: stack
!
         CALL FTAssert(test = ASSOCIATED(stack),msg = "testStackPush: Stack not associated. Abort Test")
         IF( .NOT. ASSOCIATED(stack)) RETURN 
         
         CALL stack % init()
         CALL FTAssertEqual(1, stack % refCount(),"testStackInit: Initial reference count")
         CALL FTAssertEqual(0, stack % count(),   "testStackInit: initial size")
         
      END SUBROUTINE testStackInit
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE testStackPush(stack)  
         USE FTAssertions
         USE FTStackClass
         USE FTValueClass
         IMPLICIT NONE  
         
         CLASS(FTStack) , POINTER :: stack
         CLASS(FTValue) , POINTER :: r1
         CLASS(FTObject), POINTER :: objectPtr
         CLASS(FTValue) , POINTER :: rPeek
         
         CALL FTAssert(test = ASSOCIATED(stack),msg = "testStackPush: Stack not associated. Abort Test.")

         ALLOCATE(r1)
         CALL r1 % initWithValue(3.14d0)
         objectPtr => r1
         CALL stack % push(objectPtr)
         CALL FTAssertEqual(1,stack % COUNT(),"testStackPush: Initial push reference count")
         
         CALL FTAssertEqual(2,r1%refCount()  ,"testStackPush: Reference count on stored object")
         
         CALL releaseFTValue(r1)
         CALL FTAssertEqual(1,r1 % refCount(),"testStackPush: Release on stored object")
         
         objectPtr => stack % peek()
         rPeek     => valueFromObject(objectPtr)
         CALL FTAssertEqual(expectedValue = 3.14d0 , &
                            actualValue   = rPeek % doublePrecisionValue()  , &
                            tol           = 1.0d-12, &
                            msg = "testStackPush: Value of stored object")
         
      END SUBROUTINE testStackPush
      
      
      END Module StackUnitTestsModule
! 
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE StackClassTests
         USE FTAssertions
         USE FTStackClass
         USE FTValueClass
         USE StackUnitTestsModule
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
         CALL testStackInit(stack)
!
!        ------------
!        Add an entry
!        ------------
!
         CALL testStackPush(stack)
!
!        ------------
!        Second entry
!        ------------
!
         ALLOCATE(r2)
         CALL r2%initWithValue("r2 is a string")
         objectPtr => r2
         CALL stack%push(objectPtr)
         CALL releaseFTValue(r2)
         CALL FTAssertEqual(2,stack%count(),"StackClassTests: Stack size after second push")
!
!        -----------
!        Third entry
!        -----------
!
         ALLOCATE(r3)
         CALL r3%initWithValue(17)
         objectPtr => r3
         CALL stack%push(objectPtr)
         CALL FTAssertEqual(3,stack%COUNT(),"StackClassTests: Stack size after third push")
         CALL releaseFTValue(r3)
!
!        ------------
!        Peek and pop
!        ------------
!
         objectPtr => stack%peek()
         SELECT TYPE(objectPtr)
            TYPE is (FTValue)
               CALL FTAssertEqual(17,objectPtr%integerValue(),&
               "StackClassTests: Interger value stored at top of stack")
            CLASS DEFAULT
               PRINT *, "uncaught cast in stack object"
         END SELECT
         
         CALL stack%pop(objectPtr)
         SELECT TYPE(objectPtr)
            TYPE is (FTValue)
               CALL FTAssertEqual(17,objectPtr%integerValue(),&
               "StackClassTests: Incorrect integervalue popped from top of stack")
            CLASS DEFAULT
               CALL FTAssert(.false.,"StackClassTests: UnKnown type stored in linked list")
         END SELECT
         CALL FTAssertEqual(2,stack%COUNT(),"StackClassTests: Stack count after popping")
!
!        ------------------------
!        Finish up with the stack
!        ------------------------
!
         CALL releaseFTStack(stack)

      END SUBROUTINE StackClassTests
      