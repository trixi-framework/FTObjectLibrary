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
