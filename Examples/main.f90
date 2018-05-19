!
!////////////////////////////////////////////////////////////////////////
!
!      main.f90
!      Created: July 29, 2014 at 12:45 PM 
!      By: David Kopriva  
!
!////////////////////////////////////////////////////////////////////////
!
      PROGRAM main 
         USE linkedListDemonstrationModule
         IMPLICIT NONE
         
         PRINT *
         PRINT *, "*********** Linked list demonstration ***********"
         PRINT *
            CALL demonstrateLinkedList
         PRINT *
         PRINT *, "*********** Calculator demonstration ***********"
         PRINT *
            CALL demonstrateCalculator
      END PROGRAM main
