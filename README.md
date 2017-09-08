# validatereport

- Create attractive validation reports based on results from the [validate](https://cran.r-project.org/web/packages/validate/) package.
- export validation results to [ESS JSON reporting standard](https://ec.europa.eu/eurostat/cros/system/files/20170815essnetvalidationwp2valreport_1.0.0.pdf) (pdf).
- read validation results from ESS JSON reporting standard.

### Example

After cloning this repo, you can do the following.

```r
install.packages('validate')     # required for current work
install.packages('jsonvalidate') # to check output

devtools::load_all('pkg')
library(jsonvalidate)

data(retailers,package="validate")
v <- validator(
  turnover >= 0
  , total.costs + profit == turnover
  , mean(turnover,na.rm=TRUE) >= 0 
)
cf <- confront(retailers,v)
dat <- ess_data_frame(cf,v)
json <- ess_json(dat)
cat(json)


schema <- paste(
  readLines("https://raw.githubusercontent.com/data-cleaning/ValidatReport/master/json/validation_report.json")
  , collapse="\n")

json_validate(json, schema)


```


