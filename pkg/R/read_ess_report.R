#' @importFrom utils capture.output
{}

#' Read ESS validation report structure (beta)
#'
#' @param file \code{[character]} A file location
#' @param check \code{[logical]}  Toggle checking against json schema.
#' @param version \code{[character]]} Version of ESS reporting scheme to check against.
#' @param json \code{[character]} Optinal. An ESS json string, for example
#'   created by \code{\link{ess_validation_report}}.
#'
#' @details 
#' 
#' If \code{check=TRUE}, the input file is tested against the json schema
#' using package \pkg{jsonvalidate}.
#'
#' At the moment, only JSON version 1.0.1 can be parsed.
#'
#' @examples
#'
#' library(validate)
#' data(SBS2000)
#' rules <- validator(turnover >= 0, mean(profit,na.rm=TRUE)>= 0)
#' out <- confront(SBS2000, rules, key="id")
#' json_report <- ess_validation_report(out, rules)
#' df <- read_ess_validation_report(json=json_report)
#'
#' @family ess_report
#'
#' @return A \code{data.frame}.
#'  
#' @export
read_ess_validation_report <- function(file, check=TRUE
      , version=c("1.0.1","1.0.0"), json=NULL){
  
  # Input checks
  version <- match.arg(version)
  txt <- if (!is.null(json)){
    json
  } else {
    paste0(readLines(file, encoding = "UTF-8"),collapse="\n")
  }
   
  if ( !validUTF8(txt) ){
    message("The submitted text is not valid UTF-8. Continuing, but results could be b0rked.")
  }
  
  if ( check && !(tst <- is_ess_validation_report(txt, version=version)) ){
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
