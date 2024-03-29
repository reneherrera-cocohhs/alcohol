# List of packages needed #### 
packages_needed_list <- c(
  "here", # https://github.com/r-lib/here
  "tidyverse", # https://github.com/tidyverse/tidyverse
  "pins", # https://github.com/rstudio/pins
  "lubridate", # https://github.com/tidyverse/lubridate
  "janitor", # https://github.com/sfirke/janitor
  "curl", # https://github.com/jeroen/curl
  "purrr", # https://github.com/tidyverse/purrr
  "scales", # https://github.com/r-lib/scales
  "tidycensus", # https://github.com/walkerke/tidycensus
  "zipcodeR", # https://github.com/gavinrozzi/zipcodeR/
  "tigris", # https://github.com/walkerke/tigris
  "sf", # https://github.com/r-spatial/sf/
  "tidygeocoder", # https://jessecambon.github.io/tidygeocoder/index.html
  "gtsummary",
  "gt",
  "knitr",
  "RColorBrewer",
  "Rnssp",
  "plotly",
  "lintr",
  "styler",
  "renv",
  "rlang",
  "tmap"
)

# function #### source: https://gist.github.com/stevenworthington/3178163
# check to see if packages are installed. Install them if they are not
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])] # check to see if packages are installed
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE) # Install them if they are not
}

# call function #### 
ipak(packages_needed_list)

# install the Rnssp package
devtools::install_github("cdcgov/Rnssp")

# initiate R environment https://rstudio.github.io/renv/articles/renv.html
renv::init()

# R environment status
renv::status()

# add packages to lockfile
renv::snapshot()

# create new directories and files 
dir.create("scripts")
file.create("scripts/00-setup.R")
file.create("scripts/01-read.R")
file.create("scripts/02-tidy.R")
file.create("scripts/03-transform.R")
file.create("README.md")
dir.create("reports")
dir.create("figures")
dir.create("data-raw")
dir.create("data-tidy")
dir.create("data-viz")

# lint and style #### 
library(here)
library(lintr)
library(styler)

lint_dir(path = "../alcohol/")
style_dir(path = "../alcohol/")

