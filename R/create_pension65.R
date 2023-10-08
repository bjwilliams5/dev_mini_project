##############################################
# Author: Brandon Williams
# Date: 10/4/2023
# Description: 
#   Combine data from other cleaning scripts
#
##############################################


clean_2010 <- read.csv(here("data/clean/2010_clean.csv"))
clean_2011 <- read.csv(here("data/clean/2011_clean.csv"))
clean_2012 <- read.csv(here("data/clean/2012_clean.csv"))
clean_2013 <- read.csv(here("data/clean/2013_clean.csv"))
clean_2014 <- read.csv(here("data/clean/2014_clean.csv"))
clean_2015 <- read.csv(here("data/clean/2015_clean.csv"))
clean_2016 <- read.csv(here("data/clean/2016_clean.csv"))
clean_2017 <- read.csv(here("data/clean/2017_clean.csv"))

pension65 <- bind_rows(clean_2010,
                       clean_2011,
                       clean_2012,
                       clean_2013,
                       clean_2014,
                       clean_2015,
                       clean_2016,
                       clean_2017
                       )

names(pension65) <- gsub(x = names(pension65), pattern = "\\.", replacement = "_")  




write_dta(pension65, here("data/clean/pension65.dta"))
