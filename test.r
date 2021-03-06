#!/usr/bin/Rscript

suppressPackageStartupMessages({
  if (!require("docopt")) stop("docopt not installed")
})

"Usage: test.r [nocovr] 

nocovr Skip measuring test coverage.
" -> doc

opt <- docopt(doc)

t0 <- Sys.time()

cat("# Building package")
if ( system("R CMD build pkg",ignore.stdout=TRUE) != 0){
  stop("Failed to build package", call.=FALSE)
}
pkgfile <- dir("./",pattern="\\.tar\\.gz$")
if (length(pkgfile) != 1){
  stop(sprintf("Found %d package files: %s",length(pkgfile),paste(pkgfile,collapse=", ")))
}
pkgname <- sub("(.+)_.+","\\1",pkgfile)


install_dir <- tempfile()
dir.create(install_dir)



cat(sprintf("# Installing %s to %s\n", pkgfile, install_dir))

dump <- capture.output(
  install.packages(pkgfile,lib=install_dir, verbose=FALSE
    , INSTALL_opts="--install-tests") )


# execute tests
cat("# Starting tests\n")
require(pkgname, lib.loc=install_dir, quietly=TRUE, character.only=TRUE)
for (tf in dir("pkg/tests",pattern="\\.r|R$",full.names=TRUE)){
  cat(sprintf("### Running %s\n",basename(tf)))
  RUnit::runTestFile(tf)
}


cat(sprintf("# Done (%s)\n",format(Sys.time()-t0)))

if (!opt$nocovr){
  covr::package_coverage("pkg")
}

