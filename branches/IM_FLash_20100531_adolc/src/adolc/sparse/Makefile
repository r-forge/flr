HEADERS=-I../../../inst/include

SPAOBJS=sparse_fo_rev.o sparsedrivers.o

%.o: %.c
	@echo CC  $<
	@${CC} ${HEADERS} ${ADOLC_CFLAGS} -c $<
	
%.o: %.cpp
	@echo CC  $<
	@${CXX} ${HEADERS} ${ADOLC_CXXFLAGS} -c $<

library: $(SPAOBJS)

clean:
	@-rm -f *.o
