## 1. From data and rules to validation report structure ----

# load the validate package
library(validate)
# load the supplementary reporting package
library(validatereport)

# load example data, inspect
data(retailers)
retailers$pkey <- sprintf('DUTCH RETAILERS, SBS2012, RETAILER%03d',1:nrow(retailers))
head(retailers)

# create some rules for the dataset
v <- validator(
  staff >= 0
  , turnover >= 0
  , other.rev >= 0
  , total.rev >= 0
  , staff.costs >= 0
  , total.costs >= 0
  , turnover + other.rev == total.rev
  , total.costs > staff.costs
  , if (staff > 0) staff.costs > 0
  , mean(profit, na.rm=TRUE) > 0
)
v

# confront data with the ruleset
validation <- confront(retailers, v, key="pkey")
barplot(validation[1:9], main='retailers')

# prepare to create validation report structure.
# Step 1: create a data.frame. This allows for adding metadata (if desired)
rep <- ess_data_frame(validation, v
      , agent   = "Mark"                # optional executing agent
      , trigger = "Validation overlord" # optional event trigger
      )

head(rep)
json <- ess_validation_report(rep)
cat(json)
# check output agains schema.
jsonvalidate::json_validate(json,schema=ess_json_schema())

# write to file
write(json, file="retailers.vrs")

## 2. From validation report structure to human-readable markdown ----

# read a validation report structure from textfile (or connection)
vrs <- read_vrs("retailers.vrs")

md <- as_markdown(vrs)
cat(md)

## 3. From markdown to HTML using a standard tool ----

# export markdown to file
write(md, file = "retailers.md")
rmarkdown::render("retailers.md"
    , rmarkdown::html_document()
    , output_file="./retailers.html")



