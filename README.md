
[![Build Status](https://travis-ci.org/data-cleaning/validatereport.svg?branch=master)](https://travis-ci.org/data-cleaning/validatereport) [![Coverage Status](https://coveralls.io/repos/github/data-cleaning/validatereport/badge.svg?branch=master)](https://coveralls.io/github/data-cleaning/validatereport?branch=master) ![CRAN version](http://www.r-pkg.org/badges/version/validatereport)

# validatereport

- Export validation results from the [validate](https://cran.r-project.org/web/packages/validate/) package to [ESS JSON reporting standard](https://ec.europa.eu/eurostat/cros/system/files/20170815essnetvalidationwp2valreport_1.0.0.pdf) (pdf)
- Read validation results from ESS JSON reporting standard.
- Create attractive validation reports based on results from the [validate](https://cran.r-project.org/web/packages/validate/) package (EXPERIMENTAL)

# Install the beta version (0.5.0.x)

0. On Windows, make sure that you have the correct version of [rtools](https://cran.r-project.org/bin/windows/Rtools/) installed.
1. Install the [drat](https://cran.r-project.org/package=drat) package.
2. From the R command-line do

```r
drat::addRepo("markvanderloo")
install.packages("validatereport", type="source")
```

# Install the development version

```bash
git clone https://data-cleaning/validatereport
cd validatereport
make install
```


# Exporting to ESS validation report format

```r
library(validatereport)
data(SBS2000)

rules <- validator(turnover >= 0
    , mean(turnover, na.rm=TRUE) >= 1
    , turnover + other.rev == total.rev)

result <- confront(SBS2000, rules, key="id")

json <- ess_validation_report(result, rules)

cat(json)

# or, export to file
export_ess_validation_report(result, rules, file="report.json")

```





