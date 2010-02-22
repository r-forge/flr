HEADERS=-I../../inst/include

ADCOBJS=adalloc.o malloc.o \
	adouble.o avector.o convolut.o fortutils.o forward_partx.o \
	fos_forward.o fos_reverse.o fov_forward.o fov_reverse.o \
	hos_forward.o hos_ov_reverse.o hos_reverse.o \
	hov_forward.o hov_reverse.o hov_wk_forward.o \
	interfaces.o interfacesf.o \
	taputil.o taputilc.o tayutil.o tayutilc.o zos_forward.o
DRVOBJS=drivers/drivers.o drivers/driversf.o drivers/odedrivers.o \
	drivers/odedriversf.o drivers/taylor.o
SPAOBJS=sparse/int_forward_s.o sparse/int_forward_t.o \
	sparse/int_reverse_s.o sparse/int_reverse_t.o \
	sparse/jacutils.o sparse/sparse.o sparse/sparsedrivers.o \
	sparse/graph_hess.o sparse/graph_jac.o sparse/hessutils.o
TAPOBJS=tapedoc/tapedoc.o

include ../scripts/adolc.flags

library: lib

%.o: %.c
	@echo CC  $<
	@${CC} ${HEADERS} ${ADOLC_CFLAGS} -c $<

%.o: %.cpp
	@echo CXX $<
	@${CXX} ${HEADERS} ${ADOLC_CXXFLAGS} -c $<

LIB = ../adolc.a

lib: $(LIB)

$(LIB): $(ADCOBJS)
	@make -C drivers -f Makefile.win library
	@make -C sparse -f Makefile.win library
	@make -C tapedoc -f Makefile.win library
	@${AR} -rucs $(LIB) ${ADCOBJS} ${DRVOBJS} ${SPAOBJS} ${TAPOBJS}

clean:
		@-rm -f $(LIB)
		@-rm -f $(ADCOBJS)
		@make -C drivers -f Makefile.win clean
		@make -C sparse -f Makefile.win clean
		@make -C tapedoc -f Makefile.win clean
