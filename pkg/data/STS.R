
STS <- local({
  L <- list()
  L$data <- utils::read.csv("stsdata.coseva", stringsAsFactors=FALSE)
  L$meta <- utils::read.csv("stsmeta.coseva", stringsAsFactors=FALSE)
  L
})

