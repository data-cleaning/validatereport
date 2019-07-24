
#  require(validatereport, quietly=TRUE)
#  require(validate, quietly=TRUE)
#
#  # example rules and data
#  data(retailers, package="validate")
#  retailers$primkey <- sprintf("REC%02d",seq_len(nrow(retailers)))
#  v <- validator(
#    turnover >= 0
#    , total.costs + profit == turnover
#    , mean(turnover,na.rm=TRUE) >= 0 
#  )
#  cf <- confront(retailers,v, key="primkey")
#
#  # check validation report against json schema.
#  json <- ess_validation_report(cf, v, population = "foo",measurement = "bar")
#  if (require(jsonvalidate)){
#     checkTrue(is_ess_report(json))
#  }
#  
#  # read validation report.
#  fl <- tempfile()
#  export_ess_validation_report(cf, v, file=fl)
#  dat <- read_ess_validation_report(file=fl)
#
#  checkTrue(is.data.frame(dat), msg = "ESS-TYPE")
#  checkEquals(nrow(dat),nrow(retailers) * (length(v)-1) + 1, msg="ESS-SIZE")

