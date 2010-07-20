HEADERS=-I../../inst/include -I../adolc
# added adolc directory, seems a bit stupid...

ADOLC_CXXFLAGS= -DHAVE_MALLOC=1 -DHAVE_REALLOC=1 -DADOLC_VERSION=1 -DADOLC_SUBVERSION=9 -DADOLC_PATCHLEVEL=0
ADOLC_CFLAGS= -DHAVE_MALLOC=1 -DHAVE_REALLOC=1 -DADOLC_VERSION=1 -DADOLC_SUBVERSION=9 -DADOLC_PATCHLEVEL=0

# Ignore...
#ADOLC_CFLAGS   =-c -O2 -D__STDC__ -DHAVE_MALLOC=1 -DHAVE_REALLOC=1\
# -DADOLC_DLL=1 -DADOLC_VERSION=1 -DADOLC_SUBVERSION=9 -DADOLC_PATCHLEVEL=0
#ADOLC_CXXFLAGS =-c -O2 -D__STDC__ -DHAVE_MALLOC=1 -DHAVE_REALLOC=1\
# -DADOLC_DLL=1 -DADOLC_VERSION=1 -DADOLC_SUBVERSION=9 -DADOLC_PATCHLEVEL=0


# Taken from Makefile.win - this doesn't look right - e.g. there is no taputil.obj
#ADCOBJS=adalloc.obj malloc.obj \
#	adouble.obj avector.obj convolut.obj fortutils.obj forward_partx.obj \
#	fos_forward.obj fos_reverse.obj fov_forward.obj fov_reverse.obj \
#	hos_forward.obj hos_ov_reverse.obj hos_reverse.obj \
#	hov_forward.obj hov_reverse.obj hov_wk_forward.obj \
#	interfaces.obj interfacesf.obj \
#	taputil.obj taputilc.obj tayutil.obj tayutilc.obj zos_forward.obj
#DRVOBJS=drivers\drivers.obj drivers\driversf.obj drivers\odedrivers.obj \
#	drivers\odedriversf.obj drivers\taylor.obj
#SPAOBJS=sparse\int_forward_s.obj sparse\int_forward_t.obj \
#	sparse\int_reverse_s.obj sparse\int_reverse_t.obj \
#	sparse\jacutils.obj sparse\sparse.obj sparse\sparsedrivers.obj \
#	sparse\graph_hess.obj sparse\graph_jac.obj sparse\hessutils.obj
#TAPOBJS=tapedoc\tapedoc.obj

# Taken from Makefile.am no sparse, - looks more appropriate
#libadolc_la_SOURCES  = adalloc.c malloc.c adouble.cpp \
#                       convolut.c fortutils.c\
#                       interfaces.cpp interfacesf.c \
#                       taping.c tape_handling.cpp \
#                       zos_forward.c fos_forward.c fov_forward.c \
#                       hos_forward.c hov_forward.c hov_wk_forward.c \
#                       fos_reverse.c fov_reverse.c \
#                       hos_reverse.c hos_ov_reverse.c hov_reverse.c \
#                       forward_partx.c \
#                       externfcts.cpp checkpointing.cpp \
#                       fixpoint.cpp fov_offset_forward.c revolve.c


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
                       fixpoint.o fov_offset_forward.o revolve.o
DRVOBJS=drivers/drivers.o drivers/driversf.o drivers/odedrivers.o drivers/odedriversf.o drivers/taylor.o

#SPAOBJS=sparse/int_forward_s.o sparse/int_forward_t.o \
#	sparse/int_reverse_s.o sparse/int_reverse_t.o \
#	sparse/jacutils.o sparse/sparse.o sparse/sparsedrivers.o \
#	sparse/graph_hess.o sparse/graph_jac.o sparse/hessutils.o
TAPOBJS=tapedoc/tapedoc.o


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
#	@make -C sparse -f Makefile library
	@make -C tapedoc -f Makefile library
#	@${AR} -rucs $(LIB) ${ADCOBJS} ${DRVOBJS} ${SPAOBJS} ${TAPOBJS}
	@${AR} -rucs $(LIB) ${ADCOBJS} ${DRVOBJS} ${TAPOBJS}
#	@${AR} -rucs $(LIB) ${ADCOBJS}

clean:
		@-rm -f $(LIB)
		@-rm -f $(ADCOBJS)
		@make -C drivers -f Makefile clean
#		@make -C sparse -f Makefile clean
		@make -C tapedoc -f Makefile clean
