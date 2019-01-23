#' @import validate
{}

#' Create JSON validation report
#' 
#' Creates a machine-readable validation report according to the ESS 
#' \href{../doc/validation_report_structure.pdf}{validation report structure}.
#' 
#' 
#' @param validation Object of class \code{\link[validate]{validation}}
#' @param rules Object of class \code{\link[validate]{validator}}
#' @param population \code{[character]} Unique descriptor for the population 
#'     from where the data originated.
#' @param measurement \code{[character]} Unique descriptor for the event at 
#'     which the data was collected.
#'
#' @return \code{[character]} A JSON string.
#'
#' 
#' @section Details:
#' Every time a data set or records is validated against a rule, a \code{TRUE}, 
#' \code{FALSE} or \code{NA} results. An ESS validation report stores all 
#' information necessary to identify each individual validation results. 
#' This is done by storing the following metadata with the values:
#' 
#' \itemize{
#' \item{The validation \emph{event} is identified by a time stamp, and the agent 
#' (software, platform, server, person) performing the validation. Optionally
#' information about the event that triggered the validation and the actor (user)
#' responsible for the validation can be added.}
#' \item{The source \emph{data} used to compute the result. Computing a validation
#' result may involve many individual data points. Each point can in 
#' principle be defined by the Population, the moment of measurement, the populaiton
#' unit and the variable measured. The source data may or may not coincide 
#' with the data targeted for validation (e.g. comparing x with mean(x) involves
#' a column of data but the validation target is a single value). Target data
#' is identified in the same way as source data}
#' \item{The validating \emph{expression} defining the border between
#' validity and non-validity.}
#' }
#'
#'
#' @family ess_report
#'
#' @references 
#' 
#' M. van der Loo, O. ten Bosch (2017). Design of a generic machine-readable
#' validation report structure, version 1.0. \href{../doc/validation_report_structure.pdf}{PDF}.
#'
#' @examples
#' 
#' library(validate)
#' data(retailers)
#' # add primary key to the retailers dataset
#' retailers$ID <- sprintf("REC%02d",seq_len(nrow(retailers)))
#' rules <- validator(
#'     total.rev >= 0 
#'   , staff >= 0
#'   , total.costs >= 0
#'   , profit + total.costs == total.rev
#'   , mean(profit) >= 10
#' )
#' # check the rules (we leave no room for machine rounding in this example)
#' result <- confront(retailers, rules, key="ID", lin.ineq.eps=0, lin.eq.eps=0)
#' json <- ess_validation_report(result, rules, population="supermarkets"
#'        , measurement="SBS2000")
#'  
#' @export
ess_validation_report <- function(validation, rules
                                , population = "", measurement="" ){
  if ( is.null(validation$._key)){
    stop("No primary key label found in validation object. 
          Use validate::confront(...,key=)) to set a key.")
  }
  
  if ( anyDuplicated(names(rules)) ){
    dupnames <- names(rules)[duplicated(names(rules))]
    stop(sprintf("Duplicate names in ruleset: %s"
                 , paste(dupnames, collapse=", ")))
  }
  
  # combine rules with validation output
  dat <- merge( validate::as.data.frame(rules)
              , validate::as.data.frame(validation) )
  dat <- unwrap(dat)
  
  # replace empty keys with "" (meaning: all data items)
  # TODO: formal getter for key.
  keys <- dat[[validation$._key]]
  dat[[validation$._key]] <- ifelse(is.na(keys), "", keys)
  
  # key for validation result. Important only for aggregates
  dat$validation_id <- sprintf("%05d",seq_len(nrow(dat)))
  
  # create data identifying key lists (requires unique rule names)
  rulevars <- variables(rules, as="list")[dat$name]
  
  dat$target <- sapply(seq_along(rulevars), function(i){
    x <- paste0("["
              ,       enquote(population)    # U
              , ", ", enquote(measurement)   # t
              , ", ", enquote(keys[i])     # u
              , ", ", enquote(rulevars[[i]])        # X
              , "]")   
    paste("[ ",paste(x,collapse=", ")," ]")
  })
  
  # TODO: allow for differences between source and target
  dat$source <- dat$target
  
  event <- event(validation)
  
  json <- sprintf(validation_template()
      , id = dat$validation_id
      # event
      , event['time']
      , event['actor']
      , event['agent']
      , event['trigger']
      # rule
      , dat$language
      , dat$expression
      , dat$severity
      , dat$description
      # data
      , dat$source
      , dat$target
      , dat$description
      # value
      , sprintf("%s", as.integer(dat$value))
  )
  paste0("[", paste(json, collapse=","),"]")
}

enquote <- function(x){
  str <- strsplit(x,",")
  sapply(str,function(x){
    paste0('"',trimws(x),'"',collapse=", ")
  })
}

#' Write to validation report structure
#'
#' @param validation An object of class \code{\link[validate]{validation}}
#' @param rules  \code{\link[validate]{validator}}, used in the creating the validation
#' @param file A connection, or a character string naming the file to write to. Passed through
#' to \code{\link[base]{write}}.
#' @param ... options passed to \link{ess_validation_report}.
#' 
#' @return The json string, invisibly
#' 
#' @family ess_report
#' @export
export_ess_validation_report <- function(validation, rules, file, ...){
  stopifnot(inherits(validation,"validation"))
  stopifnot(inherits(rules, "validator"))
  report <- ess_validation_report(validation, rules, ...) 
  # note: standard forces UTF-8.
  write(enc2utf8(report), file=file)
  invisible(report)
}

# Template to fill validation report v1.0
validation_template <- function(x){
'{
  "id": "%s",
  "type": "validation",
  "event": {
    "time":    "%s",
    "actor":   "%s",
    "agent":   "%s",
    "trigger": "%s"
  },
  "rule": {
    "language":    "%s",
    "expression":  "%s",
    "severity":    "%s",
    "description": "%s"
  },
  "data": {
    "source":      %s,
    "target":      %s,
    "description": "%s"
  },
  "value": "%s"
}'
}

# control characters are not allowed in JSON so we replace
# newline with literal '\n' and remove carriage return 
cleanish <- function(str){
  # replace newline with '\n'
  str <- gsub("\n","\\\\n",str)
  # replace tab with '\n'
  str <- gsub("\t","\\\\t",str)
  # replace carriage return with '\r'
  gsub("\r","\\\\r", str)
}

unwrap <- function(d){
  for (i in seq_along(d)){
    if(is.character(d[,i])) 
      d[,i] <- cleanish(d[,i]) 
  }
  d
}


#' Receive ESS json schema definition string.
#'
#'
#' @param version \code{[character]} Version of ESS sreporting scheme.
#' 
#' @references 
#' M. van der Loo and O. ten Bosch. Design of a generic machine-readable
#' validation report structure. \href{../doc/validation_report_structure}{PDF}.
#' 
#' @export
ess_json_schema <- function(version=c("1.0.1","1.0.0")){
  version <- match.arg(version)
  eval(parse(text=sprintf("ess_json_schema_%s",version)))
}

#' Check a json string against the ESS validation report json schema
#'
#' @param json \code{[character]} a \code{JSON} string.
#' @param version \code{[character]} version of the ESS validation report definition.
#' 
#' @return  A \code{logical} scalar. If \code{FALSE}, the error messages
#' can be retrieved with \code{attr(value, "errors")}.
#'
#' @details 
#' 
#' See the \href{../doc/validation_report_structure.pdf}{vignette}.
#'
#' @family ess_report
#'
#' @export
is_ess_report <- function(json, version=c("1.0.1","1.0.0")){
  version <- match.arg(version)
  schema <- ess_json_schema(version)
  tryCatch( jsonvalidate::json_validate(json, schema, verbose=TRUE)
        , error=function(e){
          stop("Validation against ESS json schema stopped.:\n %s",e$message)
  })
}

