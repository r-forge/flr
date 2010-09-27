# This is a makefile to compile OpenICA
# USAGE: make; make clean
# CREATED  15 July 2007
# MODIFIED 27 Sept 2010
# AUTHOR marco.kienzle@gmail.com

# BACKGROUND this version of ICA uses MINUIT from CERN libraries

# PURPOSE: compile ICA under linux
#ifort is the INTEL fortran compiler

# Library to use
LIBS = -L/usr/local/lib/cernlib -lmathlib -lpacklib -lkernlib

# Those symbols are from the GNU fortran runtime system
#OPTS = -L/usr/lib -lg3c
 
ica: ICA.o pctile.o IN.o OUT.o WTAB.o stats.o PARMSET.o IO_DOS.o SEPVPA.o CONVPA.o CVPA1.o WBLK.o object.o covar.o SRR.o SHRINK.o BY.o KP4.o
	ifort -o OpenICA.exe src/ICA2_90.F90 ICA.o pctile.o IN.o OUT.o WTAB.o stats.o PARMSET.o IO_DOS.o SEPVPA.o CONVPA.o CVPA1.o WBLK.o object.o covar.o SRR.o SHRINK.o BY.o KP4.o $(LIBS) $(OPTS)

ICA.o : src/ICA.F90
	ifort -c src/ICA.F90

pctile.o : src/pctile.f90
	ifort -c src/pctile.f90

IN.o : src/IN.F90
	ifort -c src/IN.F90

OUT.o : src/OUT.F90
	ifort -c src/OUT.F90

WTAB.o : src/WTAB.F90
	ifort -c src/WTAB.F90

stats.o : src/stats.f90
	ifort -c src/stats.f90

PARMSET.o : src/PARMSET.F90 
	ifort -c src/PARMSET.F90

IO_DOS.o : src/IO_DOS.F90
	ifort -c src/IO_DOS.F90

SEPVPA.o : src/SEPVPA.F90
	ifort -c src/SEPVPA.F90

CONVPA.o : src/CONVPA.F90
	ifort -c src/CONVPA.F90

CVPA1.o : src/CVPA1.F90
	ifort -c src/CVPA1.F90

WBLK.o : src/WBLK.F90
	ifort -c src/WBLK.F90

object.o : src/object.f90
	ifort -c src/object.f90

covar.o : src/covar.f
	ifort -c src/covar.f

SRR.o : src/SRR.F90
	ifort -c src/SRR.F90

SHRINK.o : src/SHRINK.F90
	ifort -c src/SHRINK.F90

BY.o : src/BY.F90
	ifort -c src/BY.F90

KP4.o : src/KP4.F90
	ifort -c src/KP4.F90

clean:
	rm -f *.o
	cp OpenICA.exe /home/mkienzle/bin/.

# end of makefile.
