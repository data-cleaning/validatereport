
library(validatereport)
library(validate)

# minimal example
d <- data.frame(id = "00", x = 1)
v <- validator( x >= 0 )
cf <- confront(d,v, key="id")

expect_true(is_ess_validation_report( ess_validation_report(cf, v) ))
expect_true(is_ess_validation_report( microdata_validation_report(cf, v) ))



# Example with double quotes in ruleset: these need explicit escaping when
# stored in JSON format.
d <- data.frame(id = "00", x = "a")
v <- validator(x %in% c("a","b"))
cf <- confront(d,v,key="id")

expect_true(is_ess_validation_report( ess_validation_report(cf, v) ))
expect_true(is_ess_validation_report( microdata_validation_report(cf, v) ))

# check output
expect_false(is_ess_validation_report("hihi_haha"))


# writing, reading.

tmpfile <- tempfile()
expect_silent( export_ess_validation_report(cf, v, file=tmpfile) )
str <- paste(readLines(tmpfile),"\n")
expect_true(is_ess_validation_report(str))

# parsing ESS report
expect_silent(df <- read_ess_validation_report(tmpfile))
expect_equal(nrow(df), 1)


