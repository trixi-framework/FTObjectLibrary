!
!////////////////////////////////////////////////////////////////////////
!
!      DataTests.f90
!      Created: June 21, 2023 at 4:33 PM 
!      By: David Kopriva  
!
!////////////////////////////////////////////////////////////////////////
!
   Module EncodingsModule 
      IMPLICIT NONE
!
!     ========
      CONTAINS 
!     ========
!
! TODO: It might make sense to create a numver of functions like those below
!       as part of the FTDataClass for converting common things to FTData.
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE encodeString(str, enc)
         IMPLICIT NONE
         CHARACTER(LEN=*)              :: str
         CHARACTER(LEN=1), ALLOCATABLE :: enc(:)
         INTEGER                       :: lngth, j
         
         lngth = LEN_TRIM(str)
         ALLOCATE(enc(lngth))
         DO j = 1, lngth 
            enc(j) = str(j:j) 
         END DO 
         
      END SUBROUTINE encodeString
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE decodeString(enc,strOut)
         IMPLICIT NONE  
         CHARACTER(LEN=*)   :: strOut
         CHARACTER(LEN=1)   :: enc(:)
         INTEGER :: j
         
         strOut = ""
         DO j = 1, SIZE(enc) 
            strOut(j:j) = enc(j) 
         END DO 
         
      END SUBROUTINE decodeString

   END Module EncodingsModule
!
!////////////////////////////////////////////////////////////////////////
!
   SUBROUTINE DataTests
      USE EncodingsModule
      USE FTDataClass
      USE FTAssertions
      IMPLICIT NONE
      
      CHARACTER(LEN=1), ALLOCATABLE :: enc(:)
      CLASS(FTData)   , POINTER :: dat, datPtr
      CLASS(FTObject) , POINTER :: obj
      CHARACTER(LEN=1), POINTER :: storedDat(:)
      CHARACTER(LEN=11)         :: outString
      
      CALL encodeString(str = "Data string",enc = enc)
      
      ALLOCATE(dat)
      CALL dat % initWithDataOfType(genericData = enc, dataType = "String")
      CALL FTAssertEqual(expectedValue = "FTData", &
                         actualValue   = dat % className(), &
                         msg           = "Data type class name check")
      CALL FTAssertEqual(expectedValue = SIZE(enc), &
                         actualValue   = dat % storedDataSize(), &
                         msg           = "Data size check")
      CALL FTAssertEqual(expectedValue = "String", &
                         actualValue   = dat % storedDataType(), &
                         msg           = "Data type check")
      CALL FTAssert(dat % dataIsOfType("String"),msg = "Data type inquiry")
!
!     ------------------
!     Accessing the data
!     ------------------
!
      storedDat => dat % storedData()
      CALL decodeString(enc = storedDat, strOut = outString)
      CALL FTAssertEqual(expectedValue = "Data string", &
                         actualValue = outString,       &
                         msg = "Decoding of accessed data")
!
!     -------
!     Casting
!     -------
!
      obj => dat
      datPtr => dataFromObject(obj)
      CALL FTAssert(test = ASSOCIATED(dat,datPtr),msg = "Cast from object") 
!
!     -------
!     Cleanup
!     -------
!
      CALL releaseFTData(dat)
      CALL FTAssert(.NOT. ASSOCIATED(dat),msg = "Destruction of FTData")
      
   END SUBROUTINE DataTests
