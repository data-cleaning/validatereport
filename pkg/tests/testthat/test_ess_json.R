

test_that("translation to json follows ESS JSON scheme",{
  data(retailers,package="validate")
  retailers$primkey <- sprintf("REC%02d",seq_len(nrow(retailers)))
  v <- validator(
    turnover >= 0
    , total.costs + profit == turnover
    , mean(turnover,na.rm=TRUE) >= 0 
  )
  cf <- confront(retailers,v, key="primkey")
  dat <- ess_data_frame(cf,v)
  json <- ess_json(dat)
  if (require(jsonvalidate)){
    expect_true(json_validate(json, ess_json_schema()))
  }
  expect_warning(ess_json_schema(version = "foo"))
})
