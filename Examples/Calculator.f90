!
!////////////////////////////////////////////////////////////////////////
!
!      Calculator.f90
!      Created: July 31, 2014 at 9:23 AM 
!      By: David Kopriva 
!
!      A simple toy reverse polish calculator.
!
!        ---------------------------------
!        Example: compute 3*(4+2*3)-5 = 25
!        ---------------------------------
!
!         CALL calc % init()
!         CALL calc % enter(2.0d0)
!         CALL calc % enter(3.0d0)
!         CALL calc % enter("*")
!         CALL calc % enter(4.0d0)
!         CALL calc % enter("+")
!         CALL calc % enter(3.0d0)
!         CALL calc % enter("*")
!         CALL calc % enter(5.0d0)
!         CALL calc % enter("-")
!         PRINT *, "3*(4+2*3)-5 = ",calc % readDisplay()
!         CALL calc % destruct()
!
!
!////////////////////////////////////////////////////////////////////////
!
      Module CalculatorClass
         USE FTObjectClass
         USE FTStackClass
         USE FTValueClass
         
         IMPLICIT NONE
         PRIVATE 
         
         TYPE, EXTENDS(FTObject) :: Calculator
            CLASS(FTStack), POINTER, PRIVATE  :: stack
!
!           ========
            CONTAINS 
!           ========
!
            PROCEDURE, PUBLIC  :: init     => initCalculator
            PROCEDURE, PUBLIC  :: destruct => destructCalculator
            PROCEDURE, PUBLIC  :: clear    => clearCalculator
            PROCEDURE, PUBLIC  :: enterOperation
            PROCEDURE, PUBLIC  :: enterValue
            GENERIC            :: enter     => enterOperation, enterValue
            PROCEDURE, PUBLIC  :: readDisplay
         END TYPE Calculator
         
         PUBLIC :: Calculator
!
!        ========         
         CONTAINS
!        ========         
!         
!
!//////////////////////////////////////////////////////////////////////// 
! 
         SUBROUTINE initCalculator(self)  
            IMPLICIT NONE
            CLASS(Calculator) :: self
            
            CALL self % FTObject % init()
            ALLOCATE(self % stack)
            CALL self % stack % init()
            
         END SUBROUTINE initCalculator
!
!//////////////////////////////////////////////////////////////////////// 
! 
         SUBROUTINE destructCalculator(self)  
            IMPLICIT NONE  
            CLASS(Calculator) :: self
            
            CALL releaseFTStack(self % stack)
            
         END SUBROUTINE destructCalculator
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE releaseCalculator(self)  
         IMPLICIT NONE
         CLASS(Calculator) , POINTER :: self
         CLASS(FTObject)   , POINTER :: obj
         IF(.NOT. ASSOCIATED(self)) RETURN
         obj => self
         CALL releaseFTObject(self = obj)
         IF ( .NOT. ASSOCIATED(obj) )     THEN
            self => NULL() 
         END IF      
      END SUBROUTINE releaseCalculator
!
!//////////////////////////////////////////////////////////////////////// 
! 
         SUBROUTINE clearCalculator(self)  
            IMPLICIT NONE  
            CLASS(Calculator) :: self
            CALL self % stack % removeAllObjects()
         END SUBROUTINE clearCalculator
!
!//////////////////////////////////////////////////////////////////////// 
! 
         SUBROUTINE enterOperation(self,op)  
            IMPLICIT NONE  
            CLASS(Calculator)                   :: self
            CHARACTER(LEN=*)                    :: op
            CLASS(FTObject) , POINTER           :: obj
            CLASS(FTValue)  , POINTER           :: v
            REAL(KIND=FT_DOUBLE_PRECISION_KIND) :: x, y, r
            
            SELECT CASE ( op )
               CASE( "+" ) 
                  x = popValue(self % stack)
                  y = popValue(self % stack)
                  r = x + y
                  CALL self % enterValue(r)
               CASE( "-" )
                  x = popValue(self % stack)
                  y = popValue(self % stack)
                  r = y - x
                  CALL self % enterValue(r)
               CASE( "*" )
                  x = popValue(self % stack)
                  y = popValue(self % stack)
                  r = x*y
                  CALL self % enterValue(r)
               CASE( "/" )
                  x = popValue(self % stack)
                  y = popValue(self % stack)
                  r = x/y
                  CALL self % enterValue(r)
               CASE DEFAULT 
            END SELECT 
            
         END SUBROUTINE enterOperation
!
!//////////////////////////////////////////////////////////////////////// 
! 
         FUNCTION popValue(stack) RESULT(r) 
            IMPLICIT NONE  
            TYPE(FTStack)                       :: stack
            CLASS(FTObject) , POINTER           :: obj
            CLASS(FTValue)  , POINTER           :: v
            REAL(KIND=FT_DOUBLE_PRECISION_KIND) :: r
      
            CALL stack % pop(obj)
            
            v   => valueFromObject(obj)
            r   = v % doublePrecisionValue()
            
            CALL releaseFTValue(v)
            
         END FUNCTION popValue
!
!//////////////////////////////////////////////////////////////////////// 
! 
         SUBROUTINE enterValue(self,v)  
            IMPLICIT NONE  
            CLASS(Calculator)                   :: self
            REAL(KIND=FT_DOUBLE_PRECISION_KIND) :: v
            CLASS(FTValue) , POINTER            :: vValue
            CLASS(FTObject), POINTER            :: obj
            
            ALLOCATE(vValue)
            CALL vValue % initWithValue(v)
           
            obj => vValue
            CALL self % stack % push(obj)
            CALL releaseFTValue(vValue)
            
         END SUBROUTINE enterValue
!
!//////////////////////////////////////////////////////////////////////// 
! 
         FUNCTION readDisplay(self)  RESULT(r)
            IMPLICIT NONE  
            CLASS(Calculator)                   :: self
            REAL(KIND=FT_DOUBLE_PRECISION_KIND) :: r
            CLASS(FTObject), POINTER            :: obj
            
            obj => self % stack % peek()
            SELECT TYPE(obj)
               TYPE is (FTValue)
                  r = obj % doublePrecisionValue() 
               CLASS DEFAULT
                  PRINT *, "Can't read display"
                  r = 0.0
            END SELECT
            
         END FUNCTION readDisplay
      END MODULE CalculatorClass
