all:
	R --slave -e 'source("./createRData.R")'

install:
	cp -f *.RData ../../../../pkg/FLBioDym/data/

clean:
	rm -f *.RData
