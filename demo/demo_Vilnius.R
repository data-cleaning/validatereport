## 1. Validating data with the 'validate' R package ----------------------------

# load the validate package
library(validate)

# load Task2 data and show it and show it
dat <- read.csv('Task2_Data.csv')
View(dat)

# read the rules
rules <- validator(.file="Task2_Rules.R")

# confront data with the ruleset
validation <- confront(dat, rules, key="ID")

# summarize the results
summary(validation)

# Get a graphical overview of the results.
barplot(validation[1:7], main='Task2 validation results')

# Create validation report structure
# load the R package (PoC version)
library(validatereport)
# Create a data.frame. This allows for adding extra metadata (if desired)
rep <- ess_data_frame(validation, rules
                      , agent   = "Olav"                # optional executing agent
                      , trigger = "Task2_Metrics validation" # optional event trigger
)
# Generate json
json <- ess_validation_report(rep)
#cat(json)
# Write to file
write(json, file="Task2_Report.vrs")


#From validation report structure to human-readable markdown ----
vrs <- read_vrs("Task2_Report.vrs")
md <- as_markdown(vrs)
# export markdown to file
write(md, file = "Task2_Report.md")
# transform markdown to html:
rmarkdown::render("Task2_Report.md"
                  , rmarkdown::html_document()
                  , output_file="Task2_Report.html")
