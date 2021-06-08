# Modify these for a given installation
#
F90 = /usr/local/bin/gfortran
FTOLibPath = ../Source

FFLAGS = -cpp -O
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
FTStringSetClass.o \
FTValueClass.o \
FTValueDictionaryClass.o \
Hash.o \
TestSuiteManagerClass.o \

libFTObjectLibrary.a : $(OBJS)
	ar rc libFTObjectLibrary.a $(OBJS)
	ranlib libFTObjectLibrary.a

#######################################
# Object dependencies and compilation #
#######################################
Assert.o : $(FTOLibPath)/FTTesting/Assert.f90 \
Comparisons.o \
FTOLConstants.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ $(FTOLibPath)/FTTesting/Assert.f90

Comparisons.o : $(FTOLibPath)/FTTesting/Comparisons.f90 \
FTOLConstants.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ $(FTOLibPath)/FTTesting/Comparisons.f90

FTDataClass.o : $(FTOLibPath)/FTObjects/FTDataClass.f90 \
FTObjectClass.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ $(FTOLibPath)/FTObjects/FTDataClass.f90

FTDictionaryClass.o : $(FTOLibPath)/FTObjects/FTDictionaryClass.f90 \
FTObjectArrayClass.o \
FTLinkedListClass.o \
FTObjectClass.o \
FTLinkedListClass.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ $(FTOLibPath)/FTObjects/FTDictionaryClass.f90

FTExceptionClass.o : $(FTOLibPath)/FTObjects/FTExceptionClass.f90 \
FTDictionaryClass.o \
FTValueDictionaryClass.o \
FTStackClass.o \
FTLinkedListClass.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ $(FTOLibPath)/FTObjects/FTExceptionClass.f90

FTLinkedListClass.o : $(FTOLibPath)/FTObjects/FTLinkedListClass.f90 \
FTObjectArrayClass.o \
FTObjectClass.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ $(FTOLibPath)/FTObjects/FTLinkedListClass.f90

FTMultiIndexTable.o : $(FTOLibPath)/FTObjects/FTMultiIndexTable.f90 \
FTObjectClass.o \
FTLinkedListClass.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ $(FTOLibPath)/FTObjects/FTMultiIndexTable.f90

FTObjectArrayClass.o : $(FTOLibPath)/FTObjects/FTObjectArrayClass.f90 \
FTObjectClass.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ $(FTOLibPath)/FTObjects/FTObjectArrayClass.f90

FTObjectClass.o : $(FTOLibPath)/FTObjects/FTObjectClass.f90
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ $(FTOLibPath)/FTObjects/FTObjectClass.f90

FTObjectLibrary.o : $(FTOLibPath)/FTObjectLibrary.f90 \
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
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ $(FTOLibPath)/FTObjectLibrary.f90

FTOLConstants.o : $(FTOLibPath)/Foundation/FTOLConstants.f90
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ $(FTOLibPath)/Foundation/FTOLConstants.f90

FTSparseMatrixClass.o : $(FTOLibPath)/FTObjects/FTSparseMatrixClass.f90 \
FTLinkedListClass.o \
FTObjectClass.o \
FTLinkedListClass.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ $(FTOLibPath)/FTObjects/FTSparseMatrixClass.f90

FTStackClass.o : $(FTOLibPath)/FTObjects/FTStackClass.f90 \
FTLinkedListClass.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ $(FTOLibPath)/FTObjects/FTStackClass.f90

FTStringSetClass.o : $(FTOLibPath)/FTObjects/FTStringSetClass.f90 \
FTObjectClass.o \
FTDictionaryClass.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ $(FTOLibPath)/FTObjects/FTStringSetClass.f90

FTValueClass.o : $(FTOLibPath)/FTObjects/FTValueClass.f90 \
FTOLConstants.o \
FTObjectClass.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ $(FTOLibPath)/FTObjects/FTValueClass.f90

FTValueDictionaryClass.o : $(FTOLibPath)/FTObjects/FTValueDictionaryClass.f90 \
FTValueClass.o \
FTDictionaryClass.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ $(FTOLibPath)/FTObjects/FTValueDictionaryClass.f90

Hash.o : $(FTOLibPath)/FTObjects/Hash.f90
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ $(FTOLibPath)/FTObjects/Hash.f90

TestSuiteManagerClass.o : $(FTOLibPath)/FTTesting/TestSuiteManagerClass.f90 \
Assert.o
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ $(FTOLibPath)/FTTesting/TestSuiteManagerClass.f90

###########
# cleanup #
###########
clean:
	rm -f *.mod
	rm -f *.o
	rm -f libFTObjectLibrary*

.PHONY: clean
