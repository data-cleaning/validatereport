#' @importFrom utils capture.output
{}

#' Read ESS validation report structure (beta)
#'
#' @param file \code{[character]} file An URL or file location
#' @param check \code{[check]} logical Toggle checking against json schema.
#' @param version \code{[character]]} version of ESS reporting scheme.
#' 
#' @details 
#' 
#' If \code{check=TRUE}, the input file is tested against the json schema
#' using package \pkg{jsonvalidate}.
#' 
#' @family ess_report
#'
#' @return A \code{data.frame}.
#'  
#' @export
read_ess_validation_report <- function(file, check=TRUE, version=c("1.0.1","1.0.0")){
  
  # Input checks
  version <- match.arg(version)
  txt <- paste0(readLines(file, encoding = "UTF-8"),collapse="\n")
   
  if ( !validUTF8(txt) ){
    message("The submitted text is not valid UTF-8. Continuing, but results could be b0rked.")
  }
  
  if ( check && !(tst <- is_ess_report(txt, version=version)) ){
    msg1 <- sprintf(
      "%s does not comply with ESS validation report structure version %s"
      , file, version)
    msg2 <- paste(capture.output(print(attr(tst,"errors"))), collapse="\n")
    stop(sprintf("%s:\n%s\n", msg1, msg2))
  }
  
  # Parse report
  a <- jsonlite::fromJSON(txt,flatten = TRUE)
  a$data.source <- gsub("c\\((.*)\\)","[\\1]",a$data.source)
  a$data.target <- gsub("c\\((.*)\\)","[\\1]",a$data.target)
  a$rule.severity <- factor(a$rule.severity, levels=c("information","warning","error"))
  a$value <- factor(a$value, levels=c("0","1","NA"),labels=c("TRUE","FALSE","NA"))
  if (is.list(a$data.target)){
    a$data.target <- sapply(a$data.target, function(x){
      paste0("[",paste(x,collapse=","),"]")
    })
  }  
  if (is.list(a$data.source)){
    a$data.source <- sapply(a$data.source, function(x){
      paste0("[",paste(x,collapse=","),"]")
    })
  }
  
  a
}
