# download and package the supported ess report formats.

readfile <- function(file){
  txt <- readLines(file)
  paste(txt, collapse="\n")
}

ess_json_schema_1.0.0 <- readfile("https://raw.githubusercontent.com/data-cleaning/ValidatReport/master/json/validation_report_1.0.0.json")

ess_json_schema_1.0.1 <- readfile("https://raw.githubusercontent.com/data-cleaning/ValidatReport/master/json/validation_report_1.0.1.json")


save(ess_json_schema_1.0.0, ess_json_schema_1.0.1, file="pkg/R/sysdata.rda")

