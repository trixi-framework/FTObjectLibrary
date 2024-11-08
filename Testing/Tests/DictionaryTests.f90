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
!      DictionaryTests.f90
!      Created: January 29, 2013 9:37 AM 
!      By: David Kopriva  
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE FTDictionaryClassTests
         USE FTDictionaryClass
         USE FTValueClass
         USE FTAssertions
         IMPLICIT NONE  
         
         CLASS(FTDictionary)                    , POINTER :: dict, dictFromObj
         CLASS(FTObject)                        , POINTER :: obj
         TYPE (FTValue)                         , POINTER :: v
         TYPE (FTMutableObjectArray)            , POINTER :: storedObjects
         CHARACTER(LEN=FTDICT_KWD_STRING_LENGTH), POINTER :: storedKeys(:)
         TYPE(FTKeyObjectPair)                  , POINTER :: kvPair

         
         CHARACTER(LEN=FTDICT_KWD_STRING_LENGTH), DIMENSION(4) :: keys   = ["first ","second","third ","fourth"]
         CHARACTER(LEN=FTDICT_KWD_STRING_LENGTH), DIMENSION(4) :: values = ["one  ","two  ","three","four "]
         CHARACTER(LEN=FTDICT_KWD_STRING_LENGTH)               :: s, msg, storedKey, sExpected, sActual
         INTEGER                                               :: i
         CHARACTER(LEN=DESCRIPTION_CHARACTER_LENGTH)           :: dictDescription =  " (first,one)"

         ALLOCATE(dict)
         CALL dict % init()
!
!        ----------------------------
!        Make sure it is a dictionary
!        ----------------------------
!
         CALL FTAssertEqual(expectedValue = dict % className(), &
                            actualValue   = "FTDictionary",  &
                            msg           = "Class name for FTDictionary")
!
!        ----------------------
!        Check empty dictionary
!        ----------------------
!
         CALL FTAssertEqual(expectedValue = 0,             &
                            actualValue =  dict % COUNT(), &
                            msg = "Empty dictionary count")
         obj => dict % objectForKey(keys(1))
         CALL FTAssert(test = .NOT.ASSOCIATED(obj),msg = "No object for empty dictionary")
         
!
!        -----------------------------------------
!        Add the keys and values to the dictionary
!        -----------------------------------------
!
         ALLOCATE(v)
         CALL v % initWithValue(values(1))
         obj => v
         CALL dict % addObjectForKey(obj,keys(1))
         CALL releaseFTValue(v)

         CALL FTAssertEqual(expectedValue = TRIM(dictDescription) //NEW_LINE('a'), &
                            actualValue   = dict % description(), &
                            msg           = "Description on a simple dictionary") 
         DO i = 2, 4
            ALLOCATE(v)
            CALL v % initWithValue(values(i))
            obj => v
            CALL dict % addObjectForKey(obj,keys(i))
            CALL releaseFTValue(v)
            CALL FTAssertEqual(1,v % refCount(),"Reference Counting: Addition of object and release")
            CALL FTAssertEqual(i,dict % count(),"Adding to dictionary object count")
         END DO
!
!        -------
!        Casting
!        -------
!
         obj         => dict
         dictFromObj => dictionaryFromObject(obj)
         CALL FTAssertEqual(expectedValue = dictFromObj % className(), &
                            actualValue   = "FTDictionary",  &
                            msg           = "Class name for FTDictionary")
         CALL FTAssert( dictFromObj % containsKey(keys(1)))
         dictFromObj => NULL()
         CALL castToDictionary(obj, dictFromObj)
         CALL FTAssertEqual(expectedValue = dictFromObj % className(), &
                            actualValue   = "FTDictionary",  &
                            msg           = "Class name for FTDictionary")
         CALL FTAssert( dictFromObj % containsKey(keys(1)))
!
!        ------------------
!        Get them back out 
!        ------------------
!
         DO i = 1,4
            obj => dict % objectForKey(keys(i))
            v   => valueFromObject(obj)
            IF ( ASSOCIATED(v) )     THEN
               s = v % stringValue(FTDICT_KWD_STRING_LENGTH)
               CALL FTAssertEqual(values(i),s,"Value for key in dictionary class")
            ELSE
               msg = "Value for key "//TRIM(values(i))// " not of correct type"
               CALL FTAssert(.false.,msg)
            END IF
         END DO
!
!        -------------------------
!        Find the keys and objects
!        -------------------------
!
         storedKeys    => dict % AllKeys()
         storedObjects => dict % AllObjects()
         
         DO i = 1, 4
            storedKey = storedKeys(i)
            obj       => dict % objectForKey(storedKey)
            v         => valueFromObject(obj)
            sExpected = v % stringValue(FTDICT_KWD_STRING_LENGTH)
            
            obj      => storedObjects % objectAtIndex(indx = i)
            v        => valueFromObject(obj)
            sActual  =  v % stringValue(FTDICT_KWD_STRING_LENGTH)
            
            CALL FTAssertEqual(sExpected, sActual,"String for stored key")
         END DO   
!
!        ----------------------
!        Check inquiry routines
!        ----------------------
!
         CALL FTAssert( dict % containsKey("first")    ,msg = "dictionary contains key")
         CALL FTAssert( .NOT. dict % containsKey("bob"),msg = "dictionary doesn't contain key")
!
!        ---------------
!        Clean up memory
!        ---------------
!
         DEALLOCATE(storedKeys)
         CALL releaseFTMutableObjectArray(storedObjects)
         CALL releaseFTDictionary(dict)
         CALL FTAssert(.NOT.ASSOCIATED(dict),"Released dictionary should have been deallocated")
!
!        -------------------------
!        Key-Value Pair operations
!        -------------------------
!
         ALLOCATE(kvPair)
         ALLOCATE(v)
         CALL v % initWithValue(1024)
         obj => v
         CALL kvPair % initWithObjectAndKey(obj,"magna")
         CALL releaseFTValue(v)
         CALL FTAssertEqual(expectedValue = "magna",actualValue = kvPair % key())
         obj => kvPair % object()
         v => valueFromObject(obj)
         CALL FTAssertEqual(expectedValue = 1,actualValue = v % refCount())
         CALL FTAssertEqual(expectedValue = 1024,actualValue = v % integerValue())
         CALL releaseFTKeyObjectPair(kvPair)
         CALL FTAssert(.NOT.ASSOCIATED(kvPair),msg = "Final deletion of key-object pair")
         
      END SUBROUTINE FTDictionaryClassTests    