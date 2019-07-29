#' @import validate
{}

#' Create an ESS JSON validation report
#' 
#' Creates a machine-readable validation report according to the ESS
#' \href{../doc/validation_report_structure.pdf}{validation report structure}.
#' This function is geared for use with data in long format, as it is
#' transferred between National Statistical Institutes and Eurostat.  Use
#' \code{\link{microdata_validation_report}} to create an ESS validation report
#' for data that is represented record-wise (i.e. with multiple variables per
#' record).
#' 
#' 
#' @param validation Object of class \code{\link[validate]{validation}}
#' @param rules Object of class \code{\link[validate]{validator}}
#'
#' @return \code{[character]} A JSON string.
#'
#' 
#' @section Details:
#' 
#' The idea of an ESS validation report is that every validation should be
#' reported with sufficient metadata to fully understand it. The following
#' figure demonstrates the information model behind the ESS report structure.
#' Solid lines indicate aggregation (validation is composed of data, rule,
#' event, value). Dashed arrows indicate the flow of information over time.
#' 
#' \if{html}{\figure{reportuml.png}{options: width=80\% alt="cellwise splitting"}}
#' \if{latex}{\figure{reportuml.png}{options: width=14cm}}
#' 
#' A validation result is generated in a validation event, where a validating
#' expression (rule) is evaluated in the context of one or more data points.
#' The data used for this evaluation is called the \emph{source} data. The data
#' under scrutiny is called the \emph{target} data. These usually coincide, but
#' there are cases where the data under scrutiny is a subset of the data needed
#' to evaluate the rule.  For example when a data point is compared with the
#' mean of a reference set of data points.
#'
#' An ESS validation report is a sequence of \code{validation} instances, which
#' we encode here in JSON format according to the \code{\link{ess_json_schema}}
#' that defines the technical implementation of the above information model.
#'
#' Rule metadata is extracted from the \code{validator} object. Information on
#' the event, data and resulting value is extracted from the \code{validation}
#' object. An ESS validation report becomes application-independent when all
#' (source and target) data points are identified semantically rather than with
#' an abstract key.  The following  elements establish such an identification
#' for a single data point:
#'
#' \itemize{
#' \item{The \emph{population} of objects to which the data pertains.}
#' \item{The identity of the \emph{measurement} in which the data values were observed 
#' (e.g. a survey). By default the name of the data set under validation is 
#' extracted from the validation object.}
#' \item{The identity of the \emph{population unit} to which the value pertains.}
#' \item{The \emph{variable} (attribute) that was measured.}
#' }
#' There are possibly many data points involved in a single validation.
#'
#' @section Extensions with respect to the ESS JSON report scheme:
#' 
#' The ESS JSON scheme allows for extra information to be stored. Here, we add
#' a field called \code{source} under the \code{rule} substructure.  This
#' contrasts with the \code{expression} field in the following way: the
#' \code{expression} is the literal R expression that was evaluated using the
#' data set under scrutiny. The \code{source} field is a more readable version
#' of the rule. In particular, it is not vectorized and not adapted to account
#' for machine accuracy. Assignments (\code{:=}) and variable groups
#' (\code{var_group}) are expanded.
#' 
#'
#'
#' @family ess_report
#'
#' @references 
#' 
#' M. van der Loo, O. ten Bosch (2017). \emph{Design of a generic
#' machine-readable validation report structure, version 1.0}.
#' \href{../doc/validation_report_structure.pdf}{PDF}.
#' 
#' MPJ van der Loo, E. de Jonge (2018). \emph{Statistical Data Cleaning with
#' Applications in R}.  John Wiley & Sons (NY).
#'
#' @examples
#' 
#' 
#'
#'library(validate)
#'library(validatereport)
#'data(STS)
#'
#'# two rules from the STS Data Structure Definition
#'rules <- validator(
#'  STS02 = OBS_VALUE >= 0
#' ,STS04 =  if (INDICATOR %in% c("IMPZ","PRBB","PREN","PREX","PREZ","PRIN","PRON")) OBS_VALUE > 0
#')
#'description(rules) <- c(
#'  "Observed values must be nonnegative"
#' ,"Price indices must be positive"
#')
#'
#'
#'cf <- confront(STS$data, rules
#'  , key = names(STS$data)[1:7])
#'
#'json <- ess_validation_report(cf, rules)
#'# show first 1405 characters of json report
#'cat(substr(json,1, 1405),"\n")
#'
#'
#'  
#' @export
ess_validation_report <- function(validation, rules){

  if ( ncol(keyset(validation)) == 0 ){
    stop("No identifying keys found in validation object. 
          Use validate::confront(...,key=)) to set key(s).")
  }
  
  if ( anyDuplicated(names(rules)) ){
    dupnames <- names(rules)[duplicated(names(rules))]
    stop(sprintf("Duplicate names in ruleset: %s"
                 , paste(dupnames, collapse=", ")))
  }
  
  # Combine rules with validation output.
  # Assignments and variable groups are expanded, but not
  # machine rounding for readability
  rls <- validate::as.data.frame(rules
                    , expand_assignments=TRUE
                    , expand_groups=TRUE
                    , vectorize=FALSE
                    , lin_eq_eps=0
                    , lin_ineq_eps=0)
    
  dat <- merge( validate::as.data.frame(validation), rls, all.x = TRUE)
  dat <- unwrap(dat)
  dat$validation_id <- sprintf("%s-%08d",format(Sys.time(),"%Y%m%dT%H:%M:%S"),seq_len(nrow(dat)))
  keynames <- names(keyset(validation))
  keys <- dat[keynames]
  for ( i in seq_along(keys)){
    keys[[i]] <- paste('"',as.character(keys[[i]]),'"',sep="")
  }
    keys[keys== '"NA"'] <- ""
  dat[keynames] <- keys  
  # put subkeys in a json array
  src <- paste("[ ",do.call(paste, args = c(keys, sep=", "))," ]")
  # replace empty arrays [ , , ... , ] with [ ]
  src <- sub("^\\[[ ,]+\\]$","[]",src) 
  # put in an array again because there can be multiple keys in the array in principle.
  src <- paste("[", src, "]")
  #   
  dat$source <- src
  dat$target <- src

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
      , gsub('"','\\\\"',dat$expression)
      , dat$severity
      , dat$description
      , gsub('"','\\\\"',dat$rule)  # quasisource
      # data
      , dat$source
      , dat$target
      , dat$description
      # value
      , sprintf("%s", as.integer(dat$value))
  )
 # standard required UTF-8.
 enc2utf8(paste0("[", paste(json, collapse=","),"]"))
}

#' Create ESS validation report from microdata validation
#'
#' Creates a machine-readable validation report according to the ESS
#' \href{../doc/validation_report_structure.pdf}{validation report structure}.
#' This function is geared for use with microdata in wide format (multiple
#' variables per record). Use \code{\link{ess_validation_report}} to create an
#' ESS validation report for data that is represented in long format.
#' 
#'
#' @inheritParams ess_validation_report
#' @param U \code{[character]} scalar. The name of the column in the keyset that specifies the
#'    population to wich the data under scrutiny pertains. If \code{U} does not
#'    occur in the key set, then this part of the key is considered equal to \code{U} for all
#'    validaiton results.
#' @param t \code{[character]} scalar. The name of the column in the keyset that specifies the
#'    measurement (e.g. a survey code) to wich the data under scrutiny pertains. If \code{t} does not
#'    occur in the key set, then this part of the key is considered equal to \code{t} for all
#'    validaiton results.
#'
#' @section Details:
#'
#' Each validation result is identified with a set of possibly multiple keys,
#' each of which consist of four parts \code{U}, \code{t}, \code{u}, \code{X}
#' (pronounce: \eqn{U\tau uX}).
#' \itemize{
#'  \item{\code{U}: Labels the population described by the data (e.g.
#'    \code{"households"}).}
#'  \item{\code{t}: Labels the measurement event, such as a survey number (e.g.
#'    \code{"HBS2009"}, for household budget survey.}
#'  \item{\code{u}: Labels the popolation element of interest. (e.g.
#'    \code{"hh12345"})}
#'  \item{\code{X}: Labels the variable of interest, (e.g. \code{"income"})}
#' }
#' In \code{JSON} one such a key is stored as \code{["households",
#' "HBS2009","hh12345","income"]}. There may be multiple of such quads
#' associated with a single result as a single validation result may concern
#' multiple units, measurements, variables, and/or populations. 
#'
#'
#' The value of \code{u} is derived from the keyset that comes with the
#' validation results: it is the not specified with \code{U} and \code{u}. If
#' there are multiple candidate columns for \code{u}, they will be added
#' comma-separated to the key set. In that case a single key may have more than
#' four subkeys, and these will be stored as \code{[U, t, u1, u2,...,un, X]}.
#' Cases where \code{u} is empty are treated as relating to all units (e.g. for
#' a rule like \code{mean(x) < 3}).  The value or values of \code{X} are
#' derived from the rules.
#'
#'
#'
#' @examples
#' library(validate)
#'
#' data(SBS2000)
#' rules <- validator(
#'     total.rev >= 0 
#'   , staff >= 0
#'   , total.costs >= 0
#'   , profit + total.costs == total.rev
#'   , mean(profit) >= 10
#' )
#' # check the rules (we leave no room for machine rounding in this example)
#' # To create an ESS report, it is essential to have an identifying key variable.
#' result <- confront(SBS2000, rules, key="id", lin.ineq.eps=0, lin.eq.eps=0)
#' json <- ess_validation_report(result, rules)
#' 
#' cat(json)
#' @family  ess_report
#'
#'
#' @export
microdata_validation_report <- function(validation, rules, U=NULL, t=NULL){
  key_names <- colnames(validate::keyset(validation))

  if ( length(key_names) == 0 ){
    stop("No identifying keys found in validation object. 
          Use validate::confront(...,key=)) to set key(s).")
  }

  if ( anyDuplicated(names(rules)) ){
    dupnames <- names(rules)[duplicated(names(rules))]
    stop(sprintf("Duplicate names in ruleset: %s"
                 , paste(dupnames, collapse=", ")))
  }
  
  rls <- validate::as.data.frame(rules
                    , expand_assignments=TRUE
                    , expand_groups=TRUE
                    , vectorize=FALSE
                    , lin_eq_eps=0
                    , lin_ineq_eps=0)
    
  dat <- merge( validate::as.data.frame(validation), rls, all.x = TRUE)
  dat <- unwrap(dat)
  
  U <- if ( is.null(U) ) ""
       else if ( U %in% key_names ){ paste0('"',keys[,U],'"'); key_names <- setdiff(key_names,U)}
       else rep(paste0('"',U,'"'), nrow(dat))

  t <- if ( is.null(t) ) paste0('"',as.character(validation$._call[[2]]),'"')
       else if ( t %in% key_names ){ paste0('"',keys[,t],'"'); key_names <- setdiff(key_names, t)}
       else rep(paste0('"',t,'"'), nrow(dat))

  if ( length(key_names) == 0 ){
    stop("Key set has no information on the population unit",call.=FALSE)
  }
  
  keys <- dat[key_names]
  for (i in ncol(keys)){
    keys[[i]] <- gsub('"NA"',"", paste('"',as.character(keys[[i]]) ,'"', sep=""))
  }
  u <- do.call(paste, c(keys, sep=", "))
  
  vars <- sapply(variables(rules, as='list'),function(x) paste0('"',x,'"'))
  X <- vars[dat$name]
 
  Utu <- sprintf("[ %s, %s, %s", U, t, u)
  UtuX <- sapply(seq_along(Utu), function(i){
     paste( sprintf("%s, %s ]",Utu[i], X[[i]]),collapse=", ")
  }) 
  UtuX <- paste0("[ ",UtuX," ]")
  
  event <- event(validation)
  
  dat$validation_id <- sprintf("%s-%08d",format(Sys.time(),"%Y%m%dT%H:%M:%S"),seq_len(nrow(dat)))
  json <- sprintf(validation_template()
      , id = dat$validation_id
      # event
      , event['time']
      , event['actor']
      , event['agent']
      , event['trigger']
      # rule
      , dat$language
      , gsub('"','\\\\"',dat$expression)
      , dat$severity
      , dat$description
      , gsub('"','\\\\"',dat$rule)  # quasisource
      # data
      , UtuX
      , UtuX # todo: derive better target values
      , dat$description
      # value
      , sprintf("%s", as.integer(dat$value))
  )
  # Standard requires UTF-8
  enc2utf8( paste0("[", paste(json, collapse=","),"]") )
  
}





#' Write validation report structure to file.
#'
#' @param validation An object of class \code{\link[validate]{validation}}
#' @param rules  \code{\link[validate]{validator}}, used in the creating the
#'   validation
#' @param file A connection, or a character string naming the file to write to.
#'   Passed through to \code{\link[base]{write}}.
#' @param report_generator A function. Either \code{\link{ess_validation_report}} 
#'   or \code{\link{microdata_validation_report}}.
#' @param ... options passed to \link{ess_validation_report}.
#' 
#' @return The json string, invisibly
#' 
#' @family ess_report
#' @export
export_ess_validation_report <- function(validation
    , rules
    , file, report_generator = ess_validation_report, ...){
  stopifnot(inherits(validation,"validation"))
  stopifnot(inherits(rules, "validator"))
  report <- report_generator(validation, rules, ...) 
  # note: standard forces UTF-8.
  write(report, file=file)
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
    "description": "%s",
    "source":      "%s"
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
#' @param version \code{[character]} version of the ESS validation report
#'   definition.
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
is_ess_validation_report <- function(json, version=c("1.0.1","1.0.0")){
  version <- match.arg(version)
  schema <- ess_json_schema(version)
  tryCatch( jsonvalidate::json_validate(json, schema, verbose=TRUE)
        , error=function(e){
          stopf("Validation against ESS json schema stopped.:\n %s", e$message)
  })
}

stopf <- function(fmt, ...) stop(sprintf(fmt,...), call.=FALSE)

