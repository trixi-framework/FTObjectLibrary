!
!////////////////////////////////////////////////////////////////////////
!
!      OptionalDataTests.f90
!      Created: May 13, 2021 at 9:53 AM 
!      By: David Kopriva  
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE OptionalDataTest(optData)  
         USE FTAssertions
         IMPLICIT NONE  
         CHARACTER(LEN=1), POINTER, OPTIONAL :: optData(:) 
         
         IF ( PRESENT(optData) )     THEN
            CALL FTAssertEqual(expectedValue = "a",actualValue = optData(1)   ,msg = "First character of optional data")
            CALL FTAssertEqual(expectedValue = "b",actualValue = optData(2)   ,msg = "Second character of optional data")
            CALL FTAssertEqual(expectedValue = 2  ,actualValue = SIZE(optData),msg = "Length of optional data array")
         ELSE 
            CALL FTAssertEqual(expectedValue = .TRUE.,actualValue = .FALSE. ,  msg = "PRESENT() of optional data") 
         END IF 
         
      END SUBROUTINE OptionalDataTest
