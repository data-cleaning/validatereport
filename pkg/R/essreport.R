




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
  list(
      source = dat[,key]
    , target = if(is.null(dat$target)) dat[,key] else dat$target
    , description =  ""
  )
}

#' Convert validation results to ESS JSON standard
#' 
#' @param validation An object of class \code{\link[validate]{validation}}
#' @param rules An object of class \code{\link[validate]{validator}}
#' 
#' @export
#' @rdname ess_json
ess_data_frame <- function(confrontation, rules, id = NULL , ...){
  id <- if (is.null(id)) strftime(Sys.time(),"VALIDATION-%Y%m%dT%H:%M:%S") else id
  out <- merge(
    validate::as.data.frame(rules)
    , validate::as.data.frame(confrontation))
  out <- cbind(out,...)
  attr(out,"id") <- id
  out
}


#' @param dat Output of a call to \code{ess_data_frame}
#' @export
ess_json <- function(dat){
  event <- get_event()
  rule <- get_rule(dat)
  data <- get_data(dat, key=dat$key)
  
  json <- sprintf(validation_template()
      , id
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
      , sprintf("%s", as.integer(d$value))
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



