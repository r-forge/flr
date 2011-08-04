FLRDynState.so: DynState

DynState: matalloc.o RDynState.o
	g++ -shared  -o FLRDynState.so matalloc.o RDynState.o   -L/usr/lib/R/lib -lR -lgomp

RDynState.o: RDynState.cpp
	g++ -I/usr/share/R/include -I/usr/share/R/include      -fpic  -g -O2 -c -fopenmp RDynState.cpp -o RDynState.o

matalloc.o: matalloc.cpp
	g++ -I/usr/share/R/include -I/usr/share/R/include      -fpic  -g -O2 -c -fopenmp matalloc.cpp -o matalloc.o


