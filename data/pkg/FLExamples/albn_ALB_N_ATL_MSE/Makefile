all:
	R --slave -e 'source("./createRData.R")'

install:
	cp -f *.RData ../../../pkg/FLExamples/data/

clean:
	rm -f *.RData
