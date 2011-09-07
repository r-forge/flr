# $Id: $
RM=rm
PKG=FLCore FLAssess FLXSA FLash FLBRP FLEDA SQLiteFL FLH ggplotFL FLBioDym FLAdvice FLRDynState FLExamples

all: clean
	-for d in $(PKG); do (R CMD build $$d); done

clean :
	-for d in `find . -maxdepth 1 -type d -regex '.*.Rcheck' -printf '%f '`; do ($(RM) -rf $$d); done
	-for d in $(PKG); do ($(RM) -f $$d/src/*.so); done
	-for d in $(PKG); do ($(RM) -f $$d/src/*.o); done
	-for d in $(PKG); do ($(RM) -f $$d*.tar.gz); done

install : clean
	R CMD INSTALL $(PKG)

check: clean
	-for d in $(PKG); do (R CMD check $$d); done

build:
	- for d in $(PKG); do (R CMD build $$d); (R CMD INSTALL $$d); done
	- for d in $(PKG); do (mv $$d_*.tar.gz ../Rdevel/src/contrib/); done
	Rscript -e "source('pkg_file.R'); foo(c('$(PKG)'))"
	mv PACKAGES ../Rdevel/src/contrib/

upload:
	scp -rp ../Rdevel flr-project@flr-project.org:www/
