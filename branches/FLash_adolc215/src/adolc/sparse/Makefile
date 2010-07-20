HEADERS=-I../../../inst/include

SPAOBJS=../int_forward_s.o ../int_forward_t.o	../int_reverse_s.o ../int_reverse_t.o \
	../sparse.o sparsedrivers.o	sparse_fo_rec.o ../graph_hess.o ../graph_jac.o ../hessutils.o

%.o: %.c
	@echo CC  $<
	@${CC} ${HEADERS} ${ADOLC_CFLAGS} -c $<
	
%.o: %.cpp
	@echo CC  $<
	@${CXX} ${HEADERS} ${ADOLC_CXXFLAGS} -c $<

library: $(SPAOBJS)

clean:
	@-rm -f *.o
