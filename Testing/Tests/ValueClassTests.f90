!
!////////////////////////////////////////////////////////////////////////
!
!      FTValueClassTests.f90
!      Created: January 15, 2013 5:13 PM 
!      By: David Kopriva  
!
!     Demonstrate and test the components of 
!     the FTValue class
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE FTValueClassTests
         USE FTValueClass
         USE FTAssertions
         IMPLICIT NONE 
!
!        -------------------------------------------------------
!        FTValue is the class to test here and the instance is v
!        It will be a pointer if it is to be added to a 
!        container.
!        -------------------------------------------------------
!
         TYPE(FTValue), POINTER :: v
!
!        -------------------------------------------
!        Some values to convert into FTValue objects
!        -------------------------------------------
!
         REAL                            :: r = 3.14
         REAL(KIND=KIND(1.0d0))          :: d
         LOGICAL                         :: l
         INTEGER                         :: i = 666
         CHARACTER(LEN=:), ALLOCATABLE   :: s
         DOUBLE PRECISION                :: doubleTol = 2*EPSILON(1.0d0)
         REAL                            :: singleTol = 2*EPSILON(1.0e0)
!
!        --------------------------------------------
!        Create an object storing a real value
!        init*** returns an FTObject with ownership
!        given to this calling routine (refCount =1).
!        --------------------------------------------
!
         ALLOCATE(v)
         CALL v % initWithValue(r)
!
!        ----------------------------------------------------------------------------------
!        Description returns a string that represents an object. Here it is just 
!        the number represented as a string, but for more complex objects like linked lists
!        it could be abitrarily complicated
!        ----------------------------------------------------------------------------------
!
         s = v % description()
!
!        -----------------------------
!        Test to make sure it is right
!        -----------------------------
!
         CALL AssertEqual("3.140000",s(1:8),"Compare description for real value")
!
!        --------------------------
!        Also test the string value
!        --------------------------
!
         s = v % stringValue(8)
         CALL AssertEqual("3.140000",s(1:8),"Compare string value for real value")

!
!        --------------------------------------------------------------------------
!        Test Reference counting.
!           Retaining an object implies that this routine wants to
!           share ownership of an object. (In fact, it already does, so the retain
!           is redundant.)
!           Releasing an object implies that the caller releases its share of
!           the object. Note that, because of the init call, we still own the
!           object after the retain+release.
!        --------------------------------------------------------------------------
!
         CALL assertEqual(1,v % refCount(),"Reference counting: Initial object refCount")
         CALL v % retain()
         CALL assertEqual(2,v % refCount(),"Reference counting: Retain count increase")
         CALL v % release()
         CALL assertEqual(1,v % refCount(),"Reference counting: retain count decrease")
!
!        -----------------------------------------------------------------
!        Test storage of the real value. An FTObject can return any one of 
!        the basic variable types, appropriately modified. Make sure that
!        each is correct.
!        ----------------------------------------------------------------
!
         CALL assertEqual(3.14,v % realValue(),singleTol,"Real storage to real")
         CALL assertEqual(3,v % integerValue(),"Integer return for real object")
         CALL assertEqual(DBLE(3.14),v % doublePrecisionValue(),doubleTol,"Double return for real object")
         CALL assertEqual("3.140000",v % stringValue(8),"String return for real object")
         CALL assertEqual(.true.,v % logicalValue(),"Logical return for real object")
!
!        ----------------------------------------------------------------
!        Destruction. When we release an object we must check to see
!        if we are the last owner. It is the last owner's responsibility
!        to destruct an object. Don't forget to nullify the pointer after
!        deallocation.
!        ----------------------------------------------------------------
!
         CALL v % release()
         CALL assertEqual(0,v % refCount(),"Reference counting: Object should be ready to deallocate")
         IF ( v % isUnreferenced() )     THEN
            DEALLOCATE(v)
            v => NULL()
         END IF
!
!        --------------------------------------
!        Now do the same with an integer number
!        --------------------------------------
!
         ALLOCATE(v)
         CALL v % initWithValue(i)
         
         CALL assertEqual(666.0,v % realValue(),singleTol,"Integer storage to real")
         CALL assertEqual(666,v % integerValue(),"Integer storage to integer")
         CALL assertEqual(DBLE(666.0),v % doublePrecisionValue(),doubleTol,"Integer storage to double")
         CALL assertEqual("666",v % stringValue(3),"Integer storage to string")
         CALL assertEqual(.true.,v % logicalValue(),"Integer storage to logical")
!
!        ------------------------------------------
!        We are done with this value, so release it
!        ------------------------------------------
!
         CALL v % release()         
         IF ( v % isUnreferenced() )     THEN
            DEALLOCATE(v)
            v => NULL()
         END IF
!
!        ---------------------------------------
!        Finally store a double precision number
!        ---------------------------------------
!
         d = 1.0d0/3.0d0
         ALLOCATE(v)
         CALL v % initWithValue(d)
         
         CALL assertEqual(REAL(d),v % realValue(),singleTol,"Double storage to real")
         CALL assertEqual(0,v % integerValue(),"Double storage to integer")
         CALL assertEqual(d,v % doublePrecisionValue(),doubleTol,"Double storage to double")
         CALL assertEqual("0.333333333333333",v % stringValue(17),"Double storage to string")
         CALL assertEqual(.true.,v % logicalValue(),"Double storage to logical")
!
!        -----------------------------------------------
!        We are done with this value, too, so release it
!        -----------------------------------------------
!
         CALL v % release()         
         IF ( v % isUnreferenced() )     THEN
            DEALLOCATE(v)
            v => NULL()
         END IF
      END SUBROUTINE FTValueClassTests   
