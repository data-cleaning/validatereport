

test_that("translation to json follows ESS JSON scheme",{
  data(retailers,package="validate")
  retailers$primkey <- sprintf("REC%02d",seq_len(nrow(retailers)))
  v <- validator(
    turnover >= 0
    , total.costs + profit == turnover
    , mean(turnover,na.rm=TRUE) >= 0 
  )
  cf <- confront(retailers,v, key="primkey")
  
  json <- ess_validation_report(cf, v, population = "foo",measurement = "bar")
  if (require(jsonvalidate)){
    expect_true(is_ess_report(json))
  }
  
  fl <- tempfile()
  export_ess_validation_report(cf, v, file=fl)
  dat <- read_ess_validation_report(file=fl)
  expect_true(is.data.frame(dat))
  expect_equal(nrow(dat),nrow(retailers) * (length(v)-1) + 1)
})
