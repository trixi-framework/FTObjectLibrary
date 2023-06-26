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
         
         TYPE (FTStack) , POINTER :: stack
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
         
         TYPE (FTStack) , POINTER :: stack
         TYPE (FTValue) , POINTER :: r1
         CLASS(FTObject), POINTER :: objectPtr
         CLASS(FTValue) , POINTER :: rPeek
         
         CALL FTAssert(test = ASSOCIATED(stack),msg = "testStackPush: Stack not associated. Abort Test.")

         ALLOCATE(r1)
         CALL r1 % initWithValue(3.14d0)
         objectPtr => r1
         CALL stack % push(objectPtr)
         CALL FTAssertEqual(1,stack % COUNT(),"testStackPush: Initial push reference count")
         
         CALL FTAssertEqual(2,r1 % refCount()  ,"testStackPush: Reference count on stored object")
         
         CALL releaseFTValue(r1)
         CALL FTAssertEqual(1,r1 % refCount(),"testStackPush: Release on stored object")
         
         objectPtr => stack % peek()
         rPeek     => valueFromObject(objectPtr)
         CALL FTAssertEqual(expectedValue = 3.14d0 , &
                            actualValue   = rPeek % doublePrecisionValue()  , &
                            relTol        = 1.0d-12, &
                            msg           = "testStackPush: Value of stored object")
         
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
         
         TYPE (FTStack) , POINTER :: stack, castStack
         TYPE (FTValue) , POINTER :: r2, r3
         CLASS(FTObject), POINTER :: objectPtr
!
!        ---------------
!        Initializations
!        ---------------
!
         ALLOCATE(stack)
         CALL testStackInit(stack)
         CALL FTAssertEqual(expectedValue = "FTStack", &
                            actualValue   = stack % className(), &
                            msg           = "Check class name")
         CALL stack % pop(objectPtr)
         CALL FTAssert(.NOT. ASSOCIATED(objectPtr),msg = "Empty stack pop")
         objectPtr => stack % peek()
         CALL FTAssert(.NOT. ASSOCIATED(objectPtr),msg = "Empty stack peek")
         objectPtr => stack
         castStack => stackFromObject(objectPtr)
         CALL FTAssert(ASSOCIATED(castStack, stack ),msg = "Cast of object to stack")
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
         CALL r2 % initWithValue("r2 is a string")
         objectPtr => r2
         CALL stack % push(objectPtr)
         CALL releaseFTValue(r2)
         CALL FTAssertEqual(2,stack % count(),"StackClassTests: Stack size after second push")
!
!        -----------
!        Third entry
!        -----------
!
         ALLOCATE(r3)
         CALL r3 % initWithValue(17)
         objectPtr => r3
         CALL stack % push(objectPtr)
         CALL FTAssertEqual(3,stack % COUNT(),"StackClassTests: Stack size after third push")
         CALL releaseFTValue(r3)
!
!        ------------
!        Peek and pop
!        ------------
!
         objectPtr => stack % peek()
         SELECT TYPE(objectPtr)
            TYPE is (FTValue)
               CALL FTAssertEqual(17,objectPtr%integerValue(),&
               "StackClassTests: Integer value stored at top of stack")
            CLASS DEFAULT
               PRINT *, "uncaught cast in stack object"
         END SELECT
         
         CALL stack % pop(objectPtr)
         SELECT TYPE(objectPtr)
            TYPE is (FTValue)
               CALL FTAssertEqual(17,objectPtr % integerValue(),&
               "StackClassTests: Incorrect integervalue popped from top of stack")
            CLASS DEFAULT
               CALL FTAssert(.false.,"StackClassTests: UnKnown type stored in linked list")
         END SELECT
         CALL FTAssertEqual(2,stack % COUNT(),"StackClassTests: Stack count after popping")
!
!        ------------------------
!        Finish up with the stack
!        ------------------------
!
         CALL releaseFTStack(stack)

      END SUBROUTINE StackClassTests
      
