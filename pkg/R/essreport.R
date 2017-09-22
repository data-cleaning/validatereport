#' @import validate

# control characters are not allowed in JSON so we replace
# newline with literal '\n' and remove carriage return 
cleanish <- function(str){
  str <- gsub("\n","\\\\n",str)
  gsub("\r","",str)
}

unwrap <- function(d){
  for (i in seq_along(d)){
    if(is.character(d[,i])) 
      d[,i] <- cleanish(d[,i]) 
  }
  d
}

get_event <- function( time = as.character(Sys.time())
          , actor=NULL, agent="", trigger=""){

  if (is.null(actor)){
    v <- R.Version()
    actor <- sprintf("%s (%s) running on %s"
            , v$version.string
            , v$nickname
            , v$platform)
  }
  
  list(time=time, actor=actor, agent=agent, trigger=trigger)
}

get_rule <- function(dat,language="R package validate 0.2.0"){
  list(
      language    = language
    , expression  = dat$expression
    , severity    = "error"
    , description = dat$description
    , status      = ""
  )
}

get_data <- function(dat, key){
  src <- if (is.na(key)) "NA" else sprintf("%s",dat[,key])
  list(
      source = src
    , target = if(is.null(dat$target)) src else dat$target
    , description =  ""
  )
}

#' @param version \code{[character]} Version of reporting scheme.
#' @rdname ess_json
#' @export
ess_json_schema <- function(version="1.0.0"){
  if (version == "1.0.0"){
    ess_json_schema_1.0.0
  } else {
    warning("Unknown scheme version")
    invisible(NULL)
  }
}


#' Convert validation results to ESS JSON standard
#' 
#' 
#' 
#' 
#' @param validation An object of class \code{\link[validate]{validation}}
#' @param rules An object of class \code{\link[validate]{validator}}
#' @param id \code{[character]} An identifying key for the report
#' @param ... extra columns, added to the output with \code{cbind}
#' 
#' 
#' @export
#' @rdname ess_json
ess_data_frame <- function(validation, rules, id = NULL , ...){
  out <- merge(
    validate::as.data.frame(rules)
    , validate::as.data.frame(validation))
  # key for validation result. Important only for aggregates
  out$id <- sprintf("%05d",seq_len(nrow(out)))
  out <- cbind(out,...)
  # name of column containing the data key
  attr(out,"key") <- validation$._key
  out
}


#' @param dat \code{[data.frame]} Output of a call to \code{ess_data_frame}
#' @export
#' 
#' @examples
#' 
#' data(retailers,package="validate")
#' retailers$primkey <- sprintf("REC%02d",seq_len(nrow(retailers)))
#' v <- validator(
#'   turnover >= 0
#'   , total.costs + profit == turnover
#'   , mean(turnover,na.rm=TRUE) >= 0 
#' )
#' cf <- validate::confront(retailers, v, key="primkey")
#' dat <- ess_data_frame(cf, v, id="my_validation")
#' json_string <- ess_json(dat)
#' 
#' # if the jsonvalidate package is installed, the 
#' # json string can be checked against the schema.
#' if(require(jsonvalidate)){
#'   json_validate(json_string, schema=ess_json_schema())
#' }
#' 
ess_json <- function(dat){
  dat <- unwrap(dat)
  event <- get_event()
  rule <- get_rule(dat)
  data <- get_data(dat, key=attr(dat,"key"))
  
  json <- sprintf(validation_template()
      , id = dat$id
      # event
      , event$time
      , event$actor
      , event$agent
      , event$trigger
      # rule
      , rule$language
      , rule$expression
      , rule$severity
      , rule$description
      , rule$status
      # data
      , data$source
      , data$target
      , data$description
      # value
      , sprintf("%s", as.integer(dat$value))
  )
  paste0("[", paste(json, collapse=","),"]")
}


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
    "status":      "%s"
  },
  "data": {
    "source":      ["%s"],
    "target":      ["%s"],
    "description": "%s"
  },
  "value": "%s"
}'
}



