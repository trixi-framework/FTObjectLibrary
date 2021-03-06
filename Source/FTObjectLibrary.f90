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
!      FTObjectLibrary.f90
!      Created: May 8, 2014 at 2:49 PM 
!      By: David Kopriva  
!
!////////////////////////////////////////////////////////////////////////
!
!>A module that simply USEs the entire library modules.
!>
      Module FTObjectLibrary
      
         USE FTAssertions
         USE ComparisonsModule
         USE FTValueDictionaryClass
         USE TestSuiteManagerClass
         USE FTObjectClass
         USE FTDictionaryClass
         USE FTSparseMatrixClass
         USE FTMutableObjectArrayClass
         USE FTStackClass
         USE FTLinkedListClass
         USE FTLinkedListIteratorClass
         USE FTValueClass
         USE FTExceptionClass
        
         IMPLICIT NONE 
      END MODULE FTObjectLibrary
