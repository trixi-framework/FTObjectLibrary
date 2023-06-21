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
!      Main.f90
!      Created: February 22, 2013 6:18 PM 
!      By: NocturnalAviationSoftware  
!!
!////////////////////////////////////////////////////////////////////////
!
      PROGRAM TestObjectsMain 
      USE FTAssertions
      USE TestSuiteManagerClass
      IMPLICIT NONE
      
      TYPE(TestSuiteManager) :: testSuite
      INTEGER                :: numberOfFailedTests
      
      
      EXTERNAL :: FTDictionaryClassTests
      EXTERNAL :: FTExceptionClassTests
      EXTERNAL :: FTValueClassTests
      EXTERNAL :: FTValueDictionaryClassTests
      EXTERNAL :: FTLinkedListClassTests
      EXTERNAL :: StackClassTests
      EXTERNAL :: MutableArrayClassTests
      EXTERNAL :: SparseMatrixTests
      EXTERNAL :: MultiIndexTableTests
      EXTERNAL :: FTStringSetTests
      EXTERNAL :: OptionalDataTest
      EXTERNAL :: ComparisonTests
      
      CHARACTER(LEN=1), POINTER :: optData(:) 
      
!      CALL setvbuf3f(6,2,0) !PGIFortran only
!
!     -----
!     Setup
!     -----
!
      CALL testSuite % init()
      CALL testSuite % setOutputUnit(11)
      CALL FTAssertEqual(expectedValue = 11,                     &
                         actualValue = testSuite % outputUnit(), &
                         msg = "Setting output unit for test suite")
      CALL testSuite % setOutputUnit(6)
      
      CALL testSuite % addTestSubroutineWithName(FTValueClassTests,"FTValueClass Tests")
      CALL testSuite % addTestSubroutineWithName(FTDictionaryClassTests,"FTDictionaryClass Tests")
      CALL testSuite % addTestSubroutineWithName(FTValueDictionaryClassTests,"FTValueDictionaryClass Tests")
      CALL testSuite % addTestSubroutineWithName(FTLinkedListClassTests,"FTLinkedListClass Tests")
      CALL testSuite % addTestSubroutineWithName(StackClassTests,"StackClass Tests")
      CALL testSuite % addTestSubroutineWithName(MutableArrayClassTests,"Mutable Array Tests")
      CALL testSuite % addTestSubroutineWithName(FTExceptionClassTests,"FTExceptionClass Tests")
      CALL testSuite % addTestSubroutineWithName(SparseMatrixTests,"SparseMatrixClass Tests")
      CALL testSuite % addTestSubroutineWithName(MultiIndexTableTests,"MultiIndexTable Tests" )
      CALL testSuite % addTestSubroutineWithName(FTStringSetTests,"String set Tests" )
      CALL testSuite % addTestSubroutineWithName(ComparisonTests,"Comparisons Tests" )
      
      ALLOCATE(optData(2))
      optData(1) = "a"
      optdata(2) = "b"
      CALL testSuite % addTestSubroutineWithName(OptionalDataTest,"Optional Data Tests", optData)
!
!     -------------
!     Run the tests
!     -------------
!
      CALL testSuite % performTests(numberOfFailedTests)
!
!     --------
!     Clean up
!     --------
!
      CALL finalizeTestSuiteManager(testSuite)
!
!     ---------------------------------------
!     Exit with error in case of failed tests
!     ---------------------------------------
!
      IF (numberOfFailedTests .gt. 0) THEN
        ERROR STOP 'At least one test has failed'
      END IF
      
      END PROGRAM TestObjectsMain  
