
# minimal example
library(validatereport)
library(validate)

d <- data.frame(id = "00", x = 1)
v <- validator( x >= 0 )
cf <- confront(d,v, key="id")

expect_true(is_ess_validation_report( ess_validation_report(cf, v) ))
expect_true(is_ess_validation_report( microdata_validation_report(cf, v) ))



# Example with quotes



