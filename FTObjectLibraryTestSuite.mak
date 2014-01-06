F90 = /usr/local/bin/gfortran
SourcePath = /Users/kopriva/Documents/Research/FortranCode/LibrarySource
FFLAGS = -O
##########################
# Object Files for build #
##########################

OBJS = \
Assert.o \
Comparisons.o \
Constants.o \
DictionaryTests.o \
ExceptionTests.o \
FTDataClass.o \
FTDictionaryClass.o \
FTExceptionClass.o \
FTHashTableClass.o \
FTLinkedListClass.o \
FTObjectArrayClass.o \
FTObjectClass.o \
FTStackClass.o \
FTValueClass.o \
FTValueDictionaryClass.o \
Hash.o \
HashTableTests.o \
LinkedListTests.o \
MutableArrayTests.o \
StacksTests.o \
TestMain.o \
TestSuiteManagerClass.o \
ValueClassTests.o \
ValueDictionaryTests.o \

FTObjectLibraryTestSuite : $(OBJS)
	 ${F90}  -o $@ $(OBJS)

#######################################
# Object dependencies and compilation #
#######################################
Assert.o : ${SourcePath}/FTObjectLibrary/Source/FTTesting/Assert.f90 \
Comparisons.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${SourcePath}/FTObjectLibrary/Source/FTTesting/Assert.f90

Comparisons.o : ${SourcePath}/FTObjectLibrary/Source/FTTesting/Comparisons.f90
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${SourcePath}/FTObjectLibrary/Source/FTTesting/Comparisons.f90

Constants.o : ${SourcePath}/FTObjectLibrary/Source/Foundation/Constants.f90
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${SourcePath}/FTObjectLibrary/Source/Foundation/Constants.f90

DictionaryTests.o : ${SourcePath}/FTObjectLibrary/Testing/Tests/DictionaryTests.f90 \
FTValueClass.o \
Assert.o \
FTDictionaryClass.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${SourcePath}/FTObjectLibrary/Testing/Tests/DictionaryTests.f90

ExceptionTests.o : ${SourcePath}/FTObjectLibrary/Testing/Tests/ExceptionTests.f90 \
FTExceptionClass.o \
FTExceptionClass.o \
FTValueDictionaryClass.o \
Assert.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${SourcePath}/FTObjectLibrary/Testing/Tests/ExceptionTests.f90

FTDataClass.o : ${SourcePath}/FTObjectLibrary/Source/FTObjects/FTDataClass.f90 \
FTObjectClass.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${SourcePath}/FTObjectLibrary/Source/FTObjects/FTDataClass.f90

FTDictionaryClass.o : ${SourcePath}/FTObjectLibrary/Source/FTObjects/FTDictionaryClass.f90 \
FTObjectArrayClass.o \
FTLinkedListClass.o \
FTObjectClass.o \
FTLinkedListClass.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${SourcePath}/FTObjectLibrary/Source/FTObjects/FTDictionaryClass.f90

FTExceptionClass.o : ${SourcePath}/FTObjectLibrary/Source/FTObjects/FTExceptionClass.f90 \
FTLinkedListClass.o \
FTDictionaryClass.o \
FTStackClass.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${SourcePath}/FTObjectLibrary/Source/FTObjects/FTExceptionClass.f90

FTHashTableClass.o : ${SourcePath}/FTObjectLibrary/Source/FTObjects/FTHashTableClass.f90 \
FTLinkedListClass.o \
FTObjectClass.o \
FTLinkedListClass.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${SourcePath}/FTObjectLibrary/Source/FTObjects/FTHashTableClass.f90

FTLinkedListClass.o : ${SourcePath}/FTObjectLibrary/Source/FTObjects/FTLinkedListClass.f90 \
FTObjectArrayClass.o \
FTObjectClass.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${SourcePath}/FTObjectLibrary/Source/FTObjects/FTLinkedListClass.f90

FTObjectArrayClass.o : ${SourcePath}/FTObjectLibrary/Source/FTObjects/FTObjectArrayClass.f90 \
FTObjectClass.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${SourcePath}/FTObjectLibrary/Source/FTObjects/FTObjectArrayClass.f90

FTObjectClass.o : ${SourcePath}/FTObjectLibrary/Source/FTObjects/FTObjectClass.f90
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${SourcePath}/FTObjectLibrary/Source/FTObjects/FTObjectClass.f90

FTStackClass.o : ${SourcePath}/FTObjectLibrary/Source/FTObjects/FTStackClass.f90 \
FTLinkedListClass.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${SourcePath}/FTObjectLibrary/Source/FTObjects/FTStackClass.f90

FTValueClass.o : ${SourcePath}/FTObjectLibrary/Source/FTObjects/FTValueClass.f90 \
FTObjectClass.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${SourcePath}/FTObjectLibrary/Source/FTObjects/FTValueClass.f90

FTValueDictionaryClass.o : ${SourcePath}/FTObjectLibrary/Source/FTObjects/FTValueDictionaryClass.f90 \
FTValueClass.o \
FTDictionaryClass.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${SourcePath}/FTObjectLibrary/Source/FTObjects/FTValueDictionaryClass.f90

Hash.o : ${SourcePath}/FTObjectLibrary/Source/FTObjects/Hash.f90
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${SourcePath}/FTObjectLibrary/Source/FTObjects/Hash.f90

HashTableTests.o : ${SourcePath}/FTObjectLibrary/Testing/Tests/HashTableTests.f90 \
FTHashTableClass.o \
Assert.o \
FTValueClass.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${SourcePath}/FTObjectLibrary/Testing/Tests/HashTableTests.f90

LinkedListTests.o : ${SourcePath}/FTObjectLibrary/Testing/Tests/LinkedListTests.f90 \
FTValueClass.o \
FTLinkedListClass.o \
Assert.o \
FTLinkedListClass.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${SourcePath}/FTObjectLibrary/Testing/Tests/LinkedListTests.f90

MutableArrayTests.o : ${SourcePath}/FTObjectLibrary/Testing/Tests/MutableArrayTests.f90 \
FTObjectArrayClass.o \
Assert.o \
FTValueClass.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${SourcePath}/FTObjectLibrary/Testing/Tests/MutableArrayTests.f90

StacksTests.o : ${SourcePath}/FTObjectLibrary/Testing/Tests/StacksTests.f90 \
FTValueClass.o \
Assert.o \
FTStackClass.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${SourcePath}/FTObjectLibrary/Testing/Tests/StacksTests.f90

TestMain.o : ${SourcePath}/FTObjectLibrary/Testing/TestMain.f90 \
TestSuiteManagerClass.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${SourcePath}/FTObjectLibrary/Testing/TestMain.f90

TestSuiteManagerClass.o : ${SourcePath}/FTObjectLibrary/Source/FTTesting/TestSuiteManagerClass.f90 \
Assert.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${SourcePath}/FTObjectLibrary/Source/FTTesting/TestSuiteManagerClass.f90

ValueClassTests.o : ${SourcePath}/FTObjectLibrary/Testing/Tests/ValueClassTests.f90 \
Assert.o \
FTValueClass.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${SourcePath}/FTObjectLibrary/Testing/Tests/ValueClassTests.f90

ValueDictionaryTests.o : ${SourcePath}/FTObjectLibrary/Testing/Tests/ValueDictionaryTests.f90 \
Assert.o \
FTValueDictionaryClass.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ ${SourcePath}/FTObjectLibrary/Testing/Tests/ValueDictionaryTests.f90

