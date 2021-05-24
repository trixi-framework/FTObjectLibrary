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
!      CalculatorExample.f90
!      Created: August 4, 2014 at 10:36 AM 
!      By: David Kopriva
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE demonstrateCalculator
         USE CalculatorClass
         IMPLICIT NONE  
         TYPE(Calculator) :: calc
         
         CALL calc % init()
!
!        ------------------------------
!        compute a series of operations
!        ------------------------------
!
         CALL calc % enter(1.0d0)
         CALL calc % enter(2.0d0)
         CALL calc % enter("+")
         PRINT *, "1 + 2 = ", calc % readDisplay()
         
         CALL calc % enter(2.0d0)
         CALL calc % enter("*")
         PRINT *, "3 * 2 = ",calc % readDisplay()
         
         CALL calc % enter(2.0d0)
         CALL calc % enter("-")
         PRINT *, "6 - 2 = ",calc % readDisplay()
         
         
         CALL calc % enter(4.0d0)
         CALL calc % enter("-")
         PRINT *, "4 - 4 = ",calc % readDisplay()
!
!        ------------------------
!        compute 3*(4+2*3)-5 = 25
!        ------------------------
!
         CALL calc % clear()
         CALL calc % enter(2.0d0)
         CALL calc % enter(3.0d0)
         CALL calc % enter("*")
         CALL calc % enter(4.0d0)
         CALL calc % enter("+")
         CALL calc % enter(3.0d0)
         CALL calc % enter("*")
         CALL calc % enter(5.0d0)
         CALL calc % enter("-")
         PRINT *, "3*(4+2*3)-5 = ",calc % readDisplay()
         
         CALL calc % destruct()
         
      END SUBROUTINE demonstrateCalculator
