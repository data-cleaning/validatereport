
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


validation_to_json <- function(confrontation, validator, id="7"){
  d <- merge(validate::as.data.frame(confrontation), validate::as.data.frame(validator))
  event <- get_event()
  rule <- get_rule(d)
  data <- get_data(d, key=confrontation$._key)
  
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



