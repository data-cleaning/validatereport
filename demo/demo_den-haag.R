## 1. Validating data with the 'validate' R package ----------------------------

# load the validate package
library(validate)

# load example data, inspect
dat <- read.csv('demo/shops.csv')
View(dat)

# read the rules
rules <- validator(.file="demo/dh-rules.yml")

# confront data with the ruleset
validation <- confront(dat, rules, key="pkey")

# summarize the results
summary(validation)

# gather all results in a tabular structure (R data.frame), print 1st 6 rows.
head(as.data.frame(validation))

# Get a graphical overview of the results.
barplot(validation[1:9], main='Validation results')

## 2. Create the ESS validationreport (json) -----------------------------------
# load the supplementary reporting package (PoC version)
library(validatereport)

# prepare to create validation report structure.
# Step 1: create a data.frame. This allows for adding extra metadata (if desired)
rep <- ess_data_frame(validation, rules
      , agent   = "Mark"                # optional executing agent
      , trigger = "Validation overlord" # optional event trigger
      )

head(rep)
json <- ess_validation_report(rep)
cat(json)
# check output agains schema.
jsonvalidate::json_validate(json, schema=ess_json_schema())

# write to file
write(json, file="shops.vrs")

## 3. From validation report structure to human-readable markdown ----

# read a validation report structure from textfile (or connection)
vrs <- read_vrs("shops.vrs")

md <- as_markdown(vrs)
cat(md)

## 4. From markdown to HTML using a standard tool ----

# export markdown to file
write(md, file = "shops.md")
rmarkdown::render("shops.md"
    , rmarkdown::html_document()
    , output_file="shops.html")

