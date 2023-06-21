!
!////////////////////////////////////////////////////////////////////////
!
!      ComparisonTests.f90
!      Created: June 21, 2023 at 9:01 AM 
!      By: David Kopriva  
!
!////////////////////////////////////////////////////////////////////////
!
   SUBROUTINE ComparisonTests
      USE ComparisonsModule
      USE FTAssertions
      IMPLICIT NONE
!
!     --------------------
!     True/False functions
!     --------------------
!
      CALL FTAssert(isTrue(.TRUE.),msg = "Test isTrue with true")
      CALL FTAssert(.NOT.isTrue(.FALSE.),msg = "Test isTrue with false")
      CALL FTAssert(isFalse(.FALSE.),msg = "Test isFalse with false")
      CALL FTAssert(.NOT.isFalse(.TRUE.),msg = "Test isFalse with true")
!
!     -----------------
!     Array comparisons
!     -----------------
!
      CALL IntegerComparisonTests
      CALL RealComparisonTests
      CALL DoubleComparisonTests
!
!     -------------
!     A string test
!     -------------
!
      CALL FTAssert(.NOT.isEqual("bob", "fred"), msg = "String inequality test")
      
   END SUBROUTINE ComparisonTests
!
!//////////////////////////////////////////////////////////////////////// 
! 
   SUBROUTINE IntegerComparisonTests  
      USE ComparisonsModule
      USE FTAssertions
      IMPLICIT NONE
      
      INTEGER :: a3(3) = [1, 2, 3], aa3(3) = [1, 4, 3]
      INTEGER :: b2(2) = [1,2]
      
      INTEGER :: a2D(3,2)  = RESHAPE([1, 2, 3, 4, 5, 6], SHAPE = SHAPE(a2D))
      INTEGER :: aa2D(3,2) = RESHAPE([1, 2, 3, 7, 5, 6], SHAPE = SHAPE(aa2D))
      INTEGER :: b2D(2,2)  = RESHAPE([1, 2, 3, 4]      , SHAPE = SHAPE(b2D))
      
      LOGICAL :: test
      
      TYPE(assertInfoArray1D) :: infoIA1D
      TYPE(assertInfoArray2D) :: infoIA2D
!
!     ----------------------------
!     Integer 1D Array comparisons
!     ----------------------------
!
       test = isEqual(a3, a3)
       CALL FTAssert(test,msg = "Integer Array is equal to itself")
       test = isEqual(a3,b2,info = infoIA1D)
       CALL FTAssert(.NOT.test, msg = "Integer Array is equal to shorter array")
       CALL FTAssertEqual(expectedValue = ASSERT_SIZE_DIFFERS,    &
                          actualValue   = infoIA1D % failureType, &
                          msg           = infoIA1D % failureName)
                          
       test = isEqual(a3,aa3,info = infoIA1D)
       CALL FTAssertEqual(expectedValue = ASSERT_VALUES_DIFFER,    &
                          actualValue   = infoIA1D % failureType,  &
                          msg           = infoIA1D % failureName)
       CALL FTAssert(.NOT.test, msg = "Integer Array is equal to different array")
       CALL FTAssert(.NOT.infoIA1D % locations(2),msg = "Location of difference 1D Array")
       CALL FTAssert(infoIA1D % locations(1),msg = "No difference in first location")
!
!      ----------------------
!      Integer 2D comparisons
!      ----------------------
!
       test = isEqual(a2D,a2D)
       CALL FTAssert(test,msg = "Integer 2D Array is equal to itself")
       test = isEqual(a2D,b2D,info = infoIA2D)
       CALL FTAssert(.NOT.test, msg = "Integer 2D Array is equal to shorter array")
       CALL FTAssertEqual(expectedValue = ASSERT_SIZE_DIFFERS,    &
                          actualValue   = infoIA2D % failureType, &
                          msg           = infoIA2D % failureName)
                          
       test = isEqual(a2D,aa2D,info = infoIA2D)
       CALL FTAssertEqual(expectedValue = ASSERT_VALUES_DIFFER,    &
                          actualValue   = infoIA1D % failureType, &
                          msg = infoIA2D % failureName)
       CALL FTAssert(.NOT.test, msg = "Integer Array is equal to different array")
       CALL FTAssert(.NOT.infoIA2D % locations(1,2),msg = "Location of difference 2D Array")
       CALL FTAssert(infoIA2D % locations(2,1), msg = "No difference in (1,2) location")
       
   END SUBROUTINE IntegerComparisonTests
!
!//////////////////////////////////////////////////////////////////////// 
! 
   SUBROUTINE RealComparisonTests  
      USE ComparisonsModule
      USE FTAssertions
      IMPLICIT NONE
      
      REAL :: a3(3) = [1.0, 2.0, 3.0], aa3(3) = [1.0, 4.0, 3.0]
      REAL :: b2(2) = [1,2]
      
      REAL :: a2D(3,2)  = RESHAPE([1.0, 2.0, 3.0, 4.0, 5.0, 6.0], SHAPE = SHAPE(a2D))
      REAL :: aa2D(3,2) = RESHAPE([1.0, 2.0, 3.0, 7.0, 5.0, 6.0], SHAPE = SHAPE(aa2D))
      REAL :: b2D(2,2)  = RESHAPE([1.0, 2.0, 3.0, 4.0]          , SHAPE = SHAPE(b2D))
      
      REAL :: absTol = 1.0e-5, relTol = 1.0e-4
      
      LOGICAL :: test
      
      INTEGER :: infoIA1D
      INTEGER :: infoIA2D
!
!     ----------------------------
!     Integer 1D Array comparisons
!     ----------------------------
!
       test = isEqual(a3, a3, relTol)
       CALL FTAssert(test,msg = "Real Array is equal to itself")
       test = isEqual(a3, b2, relTol, absTol, infoIA1D)
       CALL FTAssert(.NOT.test, msg = "Real Array is equal to shorter array")
       CALL FTAssertEqual(expectedValue = ASSERT_SIZE_DIFFERS,    &
                          actualValue   = infoIA1D, &
                          msg           = "Real array size difference")
                          
       test = isEqual(a3, aa3, relTol, absTol, infoIA1D)
       CALL FTAssertEqual(expectedValue = ASSERT_VALUES_DIFFER,    &
                          actualValue   = infoIA1D,  &
                          msg           = "Real array value difference")
       CALL FTAssert(.NOT.test, msg = "Real Array is equal to different array")
!
!      ----------------------
!      Integer 2D comparisons
!      ----------------------
!
       test = isEqual(a2D, a2D, relTol)
       CALL FTAssert(test,msg = "Real 2D Array is equal to itself")
       test = isEqual(a2D, b2D, relTol, absTol, infoIA2D)
       CALL FTAssert(.NOT.test, msg = "Real 2D Array is equal to shorter array")
       CALL FTAssertEqual(expectedValue = ASSERT_SIZE_DIFFERS,    &
                          actualValue   = infoIA2D, &
                          msg           = "2D Real array size difference")
                          
       test = isEqual(a2D, aa2D, relTol, absTol, infoIA2D)
       CALL FTAssertEqual(expectedValue = ASSERT_VALUES_DIFFER,    &
                          actualValue   = infoIA1D, &
                          msg = "Real 2D array value difference" )
       CALL FTAssert(.NOT.test, msg = "Real Array is equal to different array")
       
   END SUBROUTINE RealComparisonTests
!
!//////////////////////////////////////////////////////////////////////// 
! 
   SUBROUTINE DoubleComparisonTests  
      USE ComparisonsModule
      USE FTAssertions
      IMPLICIT NONE
      
      DOUBLE PRECISION :: a3(3) = [1.0d0, 2.0d0, 3.0d0], aa3(3) = [1.0d0, 4.0d0, 3.0d0]
      DOUBLE PRECISION :: b2(2) = [1,2]
      
      DOUBLE PRECISION :: a2D(3,2)  = RESHAPE([1.0d0, 2.0d0, 3.0d0, 4.0d0, 5.0d0, 6.0d0], SHAPE = SHAPE(a2D))
      DOUBLE PRECISION :: aa2D(3,2) = RESHAPE([1.0d0, 2.0d0, 3.0d0, 7.0d0, 5.0d0, 6.0d0], SHAPE = SHAPE(aa2D))
      DOUBLE PRECISION :: b2D(2,2)  = RESHAPE([1.0d0, 2.0d0, 3.0d0, 4.0d0]              , SHAPE = SHAPE(b2D))
      
      DOUBLE PRECISION :: absTol = 1.0d-9, relTol = 1.0d-8
      
      LOGICAL :: test
      
      INTEGER :: infoIA1D
      INTEGER :: infoIA2D
!
!     ----------------------------
!     Integer 1D Array comparisons
!     ----------------------------
!
       test = isEqual(a3, a3, relTol)
       CALL FTAssert(test,msg = "Double precision Array is equal to itself")
       test = isEqual(a3, b2, relTol, absTol, infoIA1D)
       CALL FTAssert(.NOT.test, msg = "Double precision Array is equal to shorter array")
       CALL FTAssertEqual(expectedValue = ASSERT_SIZE_DIFFERS,    &
                          actualValue   = infoIA1D, &
                          msg           = "Double precision array size difference")
                          
       test = isEqual(a3, aa3, relTol, absTol, infoIA1D)
       CALL FTAssertEqual(expectedValue = ASSERT_VALUES_DIFFER,    &
                          actualValue   = infoIA1D,  &
                          msg           = "Double precision array value difference")
       CALL FTAssert(.NOT.test, msg = "Double precision Array is equal to different array")
!
!      ----------------------
!      Integer 2D comparisons
!      ----------------------
!
       test = isEqual(a2D, a2D, relTol)
       CALL FTAssert(test,msg = "Double precision 2D Array is equal to itself")
       test = isEqual(a2D, b2D, relTol, absTol, infoIA2D)
       CALL FTAssert(.NOT.test, msg = "Double precision 2D Array is equal to shorter array")
       CALL FTAssertEqual(expectedValue = ASSERT_SIZE_DIFFERS,    &
                          actualValue   = infoIA2D, &
                          msg           = "2D Double precision precisionl array size difference")
                         
       test = isEqual(a2D, aa2D, relTol, absTol, infoIA2D)
       CALL FTAssertEqual(expectedValue = ASSERT_VALUES_DIFFER,    &
                          actualValue   = infoIA1D, &
                          msg = "Double precision precisional 2D array value difference" )
       CALL FTAssert(.NOT.test, msg = "Double precision Array is equal to different array")
       
   END SUBROUTINE DoubleComparisonTests

