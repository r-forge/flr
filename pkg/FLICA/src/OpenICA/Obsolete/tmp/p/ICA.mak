# This is a makefile to compile ICA under linux with the Intel Fortran 8.1 compiler
# USAGE: make -f ICA.mak

# DATE OF CREATION: 09 November 2004
# DATE OF MODIFICATION: 09 November 2004
# AUTHOR: kienzle_marco@bigfoot.com

# Note that the <TAB> are important: don't use spaces instead.

OBJS = READBLK.o READPRO.o
COMPILER = ifort
HEADER = /home/kienzlem/Programs/Fortran/ICA/ModifiedSource/p/

#myprog: myprog.o file_1.o file_2.o file_3.o
#	g77 -o myprog myprog.o file_1.o file_2.o file_3.o
# ICA depends on object files

#ICA.exe : READBLK.o READPRO.o IN.o
#	$(COMPILER) -o ICA.exe READBLK.o READPRO.o

# 
#READBLK.o : READBLK.F90
#	$(COMPILER) -c READBLK.F90 -I $(HEADER)

#READPRO.o : READPRO.F90
#	$(COMPILER) -c READPRO.F90 -I $(HEADER)

#trim.o : trim.f90
#	$(COMPILER) -c trim.f90 -I $(HEADER)
#SRR.o : SRR.F90
#	$(COMPILER) -c SRR.F90 -I $(HEADER)

pctile.o : pctile.f90
	$(COMPILER) -c pctile.f90 -I $(HEADER)

IN.o : IN.F90
	$(COMPILER) -c ../a/IN.F90 -I $(HEADER)

#NP_IO.o : NP_IO.F90
#	$(COMPILER) -c NP_IO.F90 -I $(HEADER)

WBLK.o : WBLK.F90
	$(COMPILER) -c WBLK.F90 -I $(HEADER)

#myprog.o: myprog.f
#	g77 -c myprog.f
# myprog.o depends on its source file
#
#file_1.o: file_1.f
#	g77 -c file_1.f
# file_1.o depends on its source file
#
#file_2.o: file_2.f file_1.o
#	g77 -c file_2.f file_1.o
# file_2.o depends on its source file and an object file
#
#file_3.o: file_3.f file_2.o
#	g77 -c file_3.f file_2.o
# file_3.o depends on its source file and an object file

# end of makefile.
