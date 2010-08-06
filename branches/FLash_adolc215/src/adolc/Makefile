HEADERS=-I../../inst/include -I../adolc
# Added adolc directory as include
# This is so uni5_for.c is found (it is included by lots of files e.g. zos_forward.c)

# Same settings as in the ADOLC 2-1.5 makefile
ADOLC_CXXFLAGS= -DHAVE_MALLOC=1 -DHAVE_REALLOC=1 -DADOLC_VERSION=1 -DADOLC_SUBVERSION=9 -DADOLC_PATCHLEVEL=0
ADOLC_CFLAGS= -DHAVE_MALLOC=1 -DHAVE_REALLOC=1 -DADOLC_VERSION=1 -DADOLC_SUBVERSION=9 -DADOLC_PATCHLEVEL=0

# List of files taken from ADOLC 2-1.5 Makefile (no Sparse option). Maybe not need all of the files?
# Like tapedoc, remove
ADCOBJS= adalloc.o malloc.o adouble.o \
                       convolut.o fortutils.o\
                       interfaces.o interfacesf.o \
                       taping.o tape_handling.o \
                       zos_forward.o fos_forward.o fov_forward.o \
                       hos_forward.o hov_forward.o hov_wk_forward.o \
                       fos_reverse.o fov_reverse.o \
                       hos_reverse.o hos_ov_reverse.o hov_reverse.o \
                       forward_partx.o \
                       externfcts.o checkpointing.o \
		       fov_offset_forward.o revolve.o \
                       #fixpoint.o 
DRVOBJS= drivers/drivers.o drivers/driversf.o drivers/odedrivers.o drivers/odedriversf.o drivers/taylor.o

library: lib

%.o: %.c
	@echo CC  $<
	@${CC} ${HEADERS} ${ADOLC_CFLAGS} -fPIC -c $<

%.o: %.cpp
	@echo CXX $<
	@${CXX} ${HEADERS} ${ADOLC_CXXFLAGS} -fPIC -c $<

LIB = ../adolc.a

lib: $(LIB)

# target ... : prerequiite
# <tab>   rules
# #       ...

# Go down the dependency tree
# LIB is the adolc.a is an archive file that holds all the ADOLC object files (have not been linked yet)
# To make LIB, requires all the ADCOBJS object files
# To make the *.o files requires the *.c or *.cpp file and the appropriate rule (CC HEADERS FLAGS)
# With that prerequisite fulfilled we go to the first rule for making LIB:
#	make the Makefile in the drivers folder.
#	what are the -C and -f flags: -f specifies the name of the Makefile, -C change to directory
#	what is the library argument? target? make executes the Makefile to update the target, here library
#	so maybe it only recompiles the driver object files if they have changed from what is in library, which is LIB, the archive?
# That rule makes all the DRVOBJS object files
# Second rule then stitches the object files together to make LIB archive file
# $AR is ar - archive. So the second rule just archives the ADCOBJS and DRVOBJS object files into adolc.a
# what is the -rucs argument? AR is ar (on Linux anyway - see /usr/local/lib/R/etc)
# -r insert into member, u only insert if newer than existin, s write object file index into archive, c create the archive

$(LIB): $(ADCOBJS)
	@make -C drivers -f Makefile library
	@${AR} -rucs $(LIB) ${ADCOBJS} ${DRVOBJS}

clean:
		@-rm -f $(LIB)
		@-rm -f $(ADCOBJS)
		@make -C drivers -f Makefile clean
