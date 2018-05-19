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
