FC = gfortran

FFLAGS = -cpp
FFLAGS += -std=f2008 # Enforce Fortran 2008 standard
FFLAGS += -pedantic # Be very pendatic
FFLAGS += -Werror # All warnings should be treated as errors
FFLAGS += -fimplicit-none # For implicit none everywhere to detect errors
FFLAGS += -Wall -Wextra # Enable many useful warnings
FFLAGS += -Wcharacter-truncation -Wimplicit-interface -Wimplicit-procedure # Even more warnings
FFLAGS += -Wno-compare-reals # Must be allowed since it is used *properly* in several places
FFLAGS += -Wno-unused-dummy-argument # Must be allowed for some OOP style functions
FFLAGS += -Wno-intrinsic-shadow # Must be allowed for COUNT
FFLAGS += -Wno-implicit-interface # Must be allowed for b3hs_hash_key_jenkins
FFLAGS += -Wno-implicit-procedure # Must be allowed for testAppendingLists

##########################
# Object Files for build #
##########################

OBJS = \
Assert.o \
Calculator.o \
CalculatorExample.o \
Comparisons.o \
FTDataClass.o \
FTDictionaryClass.o \
FTExceptionClass.o \
FTLinkedListClass.o \
FTMultiIndexTable.o \
FTObjectArrayClass.o \
FTObjectClass.o \
FTObjectLibrary.o \
FTOLConstants.o \
FTSparseMatrixClass.o \
FTStackClass.o \
FTValueClass.o \
FTValueDictionaryClass.o \
Hash.o \
LinkedListExample.o \
main.o \
TestSuiteManagerClass.o \

Examples : $(OBJS)
	 ${FC}  -o $@ $(OBJS)

#######################################
# Object dependencies and compilation #
#######################################
Assert.o : ../Source/FTTesting/Assert.f90 \
Comparisons.o \
FTOLConstants.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ ../Source/FTTesting/Assert.f90

Calculator.o : Calculator.f90 \
FTValueClass.o \
FTObjectClass.o \
FTStackClass.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ Calculator.f90

CalculatorExample.o : CalculatorExample.f90 \
Calculator.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ CalculatorExample.f90

Comparisons.o : ../Source/FTTesting/Comparisons.f90 \
FTOLConstants.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ ../Source/FTTesting/Comparisons.f90

FTDataClass.o : ../Source/FTObjects/FTDataClass.f90 \
FTObjectClass.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ ../Source/FTObjects/FTDataClass.f90

FTDictionaryClass.o : ../Source/FTObjects/FTDictionaryClass.f90 \
FTObjectArrayClass.o \
FTLinkedListClass.o \
FTObjectClass.o \
FTLinkedListClass.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ ../Source/FTObjects/FTDictionaryClass.f90

FTExceptionClass.o : ../Source/FTObjects/FTExceptionClass.f90 \
FTDictionaryClass.o \
FTValueDictionaryClass.o \
FTStackClass.o \
FTLinkedListClass.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ ../Source/FTObjects/FTExceptionClass.f90

FTLinkedListClass.o : ../Source/FTObjects/FTLinkedListClass.f90 \
FTObjectArrayClass.o \
FTObjectClass.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ ../Source/FTObjects/FTLinkedListClass.f90

FTMultiIndexTable.o : ../Source/FTObjects/FTMultiIndexTable.f90 \
FTObjectClass.o \
FTLinkedListClass.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ ../Source/FTObjects/FTMultiIndexTable.f90

FTObjectArrayClass.o : ../Source/FTObjects/FTObjectArrayClass.f90 \
FTObjectClass.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ ../Source/FTObjects/FTObjectArrayClass.f90

FTObjectClass.o : ../Source/FTObjects/FTObjectClass.f90
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ ../Source/FTObjects/FTObjectClass.f90

FTObjectLibrary.o : ../Source/FTObjectLibrary.f90 \
FTValueDictionaryClass.o \
FTSparseMatrixClass.o \
FTExceptionClass.o \
FTObjectArrayClass.o \
FTObjectClass.o \
FTStackClass.o \
TestSuiteManagerClass.o \
FTLinkedListClass.o \
Assert.o \
FTDictionaryClass.o \
FTLinkedListClass.o \
FTValueClass.o \
Comparisons.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ ../Source/FTObjectLibrary.f90

FTOLConstants.o : ../Source/Foundation/FTOLConstants.f90
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ ../Source/Foundation/FTOLConstants.f90

FTSparseMatrixClass.o : ../Source/FTObjects/FTSparseMatrixClass.f90 \
FTLinkedListClass.o \
FTObjectClass.o \
FTLinkedListClass.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ ../Source/FTObjects/FTSparseMatrixClass.f90

FTStackClass.o : ../Source/FTObjects/FTStackClass.f90 \
FTLinkedListClass.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ ../Source/FTObjects/FTStackClass.f90

FTValueClass.o : ../Source/FTObjects/FTValueClass.f90 \
FTOLConstants.o \
FTObjectClass.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ ../Source/FTObjects/FTValueClass.f90

FTValueDictionaryClass.o : ../Source/FTObjects/FTValueDictionaryClass.f90 \
FTValueClass.o \
FTDictionaryClass.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ ../Source/FTObjects/FTValueDictionaryClass.f90

Hash.o : ../Source/FTObjects/Hash.f90
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ ../Source/FTObjects/Hash.f90

LinkedListExample.o : LinkedListExample.f90 \
FTValueClass.o \
FTLinkedListClass.o \
FTLinkedListClass.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ LinkedListExample.f90

main.o : ../Examples/main.f90 \
LinkedListExample.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ ../Examples/main.f90

TestSuiteManagerClass.o : ../Source/FTTesting/TestSuiteManagerClass.f90 \
Assert.o
	$(FC) -c $(FFLAGS) $(INCLUDES) -o $@ ../Source/FTTesting/TestSuiteManagerClass.f90

