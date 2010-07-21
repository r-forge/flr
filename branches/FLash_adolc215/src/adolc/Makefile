HEADERS=-I../../inst/include -I./
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

$(LIB): $(ADCOBJS)
	@make -C drivers -f Makefile library
	@${AR} -rucs $(LIB) ${ADCOBJS} ${DRVOBJS}

clean:
		@-rm -f $(LIB)
		@-rm -f $(ADCOBJS)
		@make -C drivers -f Makefile clean
