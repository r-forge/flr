# This is ica.mak makefile
# USAGE: make -f ica.mak
# CREATED DATE 23 July 2010
# MODIFIED 23 July 2010
# AUTHOR marco.kienzle@gmail.com

# PURPOSE: compile ICA under linux
#ifort is the INTEL fortran compiler

# Library to use
LIBS = -L$NAG_LIB -lnag_nag -lpthread
#/usr/libexec/CERNLIB/2005/lib/  -lmathlib -lpacklib -lkernlib -L/usr/local/lib/port3 -lport -L/usr/local/lib/MinPack -lminpack


# Those symbols are from the GNU fortran runtime system
# OPTS = -L/usr/lib -lg2c
 
ica: obj/ICA.o obj/pctile.o obj/IN.o obj/OUT.o obj/WTAB.o obj/stats.o obj/PARMSET.o obj/IO_DOS.o obj/SEPVPA.o obj/CONVPA.o obj/CVPA1.o obj/WBLK.o obj/object.o obj/SRR.o obj/SHRINK.o obj/BY.o obj/KP4.o
	ifort -o ICA.exe src/ICA2_90.F90 obj/ICA.o obj/pctile.o obj/IN.o obj/OUT.o obj/WTAB.o obj/stats.o obj/PARMSET.o obj/IO_DOS.o obj/SEPVPA.o obj/CONVPA.o obj/CVPA1.o obj/WBLK.o obj/object.o obj/SRR.o obj/SHRINK.o obj/BY.o obj/KP4.o $(LIBS) #$(OPTS)

obj/ICA.o : src/ICA.F90
	ifort -c src/ICA.F90 -o obj/ICA.o

obj/pctile.o : src/pctile.f90
	ifort -c src/pctile.f90 -o obj/pctile.o

obj/IN.o : src/IN.F90
	ifort -c src/IN.F90 -o obj/IN.o

obj/OUT.o : src/OUT.F90
	ifort -c src/OUT.F90 -o obj/OUT.o

obj/WTAB.o : src/WTAB.F90
	ifort -c src/WTAB.F90 -o obj/WTAB.o

obj/stats.o : src/stats.f90
	ifort -c src/stats.f90 -o obj/stats.o

obj/PARMSET.o : src/PARMSET.F90 
	ifort -c src/PARMSET.F90 -o obj/PARMSET.o

obj/IO_DOS.o : src/IO_DOS.F90
	ifort -c src/IO_DOS.F90 -o obj/IO_DOS.o

obj/SEPVPA.o : src/SEPVPA.F90
	ifort -c src/SEPVPA.F90 -o obj/SEPVPA.o

obj/CONVPA.o : src/CONVPA.F90
	ifort -c src/CONVPA.F90 -o obj/CONVPA.o

obj/CVPA1.o : src/CVPA1.F90
	ifort -c src/CVPA1.F90 -o obj/CVPA1.o

obj/WBLK.o : src/WBLK.F90
	ifort -c src/WBLK.F90 -o obj/WBLK.o

obj/object.o : src/object.f90
	ifort -c src/object.f90 -o obj/object.o

obj/SRR.o : src/SRR.F90
	ifort -c src/SRR.F90 -o obj/SRR.o

obj/SHRINK.o : src/SHRINK.F90
	ifort -c src/SHRINK.F90 -o obj/SHRINK.o

obj/BY.o : src/BY.F90
	ifort -c src/BY.F90 -o obj/BY.o

obj/KP4.o : src/KP4.F90
	ifort -c src/KP4.F90 -o obj/KP4.o


# end of makefile.
