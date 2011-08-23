SUBDIRS = ALB_N_ATL_MSE BFT_E_ATL_MSE YFT_ATL_MSE

all:
	@for i in $(SUBDIRS); do \
  	echo "make all in $$i..."; \
	  (cd $$i; $(MAKE) all); done

install:
	install @for i in $(SUBDIRS); do \
		echo "Installing in $$i..."; \
		(cd $$i; $(MAKE) install); done

clean:
	@for i in $(SUBDIRS); do \
    echo "Clearing in $$i..."; \
		(cd $$i; $(MAKE) clean); done
