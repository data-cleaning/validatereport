# Download ESS validation report JSON schema version 1.0
#
ess_json_schema_1.0.0 <- paste(
  readLines("https://raw.githubusercontent.com/data-cleaning/ValidatReport/master/json/validation_report.json")
  , collapse="\n")

save(ess_json_schema_1.0.0, file = "pkg/R/sysdata.rda")


