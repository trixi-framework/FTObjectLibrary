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
      USE TestSuiteManagerClass
      IMPLICIT NONE
      
      TYPE(TestSuiteManager) :: testSuite
      
      
      EXTERNAL :: FTDictionaryClassTests
      EXTERNAL :: FTExceptionClassTests
      EXTERNAL :: FTValueClassTests
      EXTERNAL :: FTValueDictionaryClassTests
      EXTERNAL :: FTLinkedListClassTests
      EXTERNAL :: StackClassTests
      EXTERNAL :: MutableArrayClassTests
      EXTERNAL :: SparseMatrixTests
      EXTERNAL :: MultiIndexTableTests
!      CALL setvbuf3f(6,2,0) !PGIFortran only
!
!     -----
!     Setup
!     -----
!
      CALL testSuite % init()
      
      CALL testSuite % addTestSubroutineWithName(FTValueClassTests,"FTValueClass Tests")
      CALL testSuite % addTestSubroutineWithName(FTDictionaryClassTests,"FTDictionaryClass Tests")
      CALL testSuite % addTestSubroutineWithName(FTValueDictionaryClassTests,"FTValueDictionaryClass Tests")
      CALL testSuite % addTestSubroutineWithName(FTLinkedListClassTests,"FTLinkedListClass Tests")
      CALL testSuite % addTestSubroutineWithName(StackClassTests,"StackClass Tests")
      CALL testSuite % addTestSubroutineWithName(MutableArrayClassTests,"Mutable Array Tests")
      CALL testSuite % addTestSubroutineWithName(FTExceptionClassTests,"FTExceptionClass Tests")
      CALL testSuite % addTestSubroutineWithName(SparseMatrixTests,"SparseMatrixClass Tests")
      CALL testSuite % addTestSubroutineWithName(MultiIndexTableTests,"MultiIndexTable Tests" )
!
!     -------------
!     Run the tests
!     -------------
!
      CALL testSuite % performTests()
!
!     -------
!     Cleanup
!     -------
!
      CALL testSuite % finalize()
      
      END PROGRAM TestObjectsMain  
