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
            
            CALL release(self % stack)
            
            CALL self % FTObject % destruct()
            
         END SUBROUTINE destructCalculator
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE releaseCalculator(self)  
         IMPLICIT NONE
         CLASS(Calculator) , POINTER :: self
         CLASS(FTObject)     , POINTER :: obj
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
            
            CALL release(v)
            
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
            CALL release(vValue)
            
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
