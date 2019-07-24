
doc: 
	R -s -e "pkgload::load_all('pkg');roxygen2::roxygenize('pkg')"

pkg: doc
	rm -f *.tar.gz
	R CMD build pkg

check: doc
	rm -rf *.tar.gz
	R CMD build pkg
	R CMD check *.tar.gz

cran: doc
	rm -rf *.tar.gz
	R CMD build pkg
	R CMD check --as-cran *.tar.gz

install: doc
	rm -rf *.tar.gz
	R CMD build pkg
	R CMD INSTALL *.tar.gz

test: doc
	R -s -e "tinytest::build_install_test('pkg')"

manual: doc
	R CMD Rd2pdf --force -o manual.pdf ./pkg

clean:
	rm -rf *.Rcheck
	rm -f *.tar.gz

