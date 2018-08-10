library(AER)
library(tidyverse)
library(readxl)

## Narrower version than "population" imported from TSV below.
## Preserved for posterity, especially for gsub() example
# pop.xls <- read_excel("nst-est2017-01.xlsx", 
#                       skip = 4,  n_max = 58,
#                       col_names = c("stateRegion", "census", "estimatesBase",
#                         "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017"
#                      ))
# pop.xls <- pop.xls[-57,] # drop empty row before PR
# pop.xls$stateRegion <- gsub("[.]", "", pop.xls$stateRegion) # remove periods before state names



## Abbreviations to be merged into "population" set imported below.
## Data gathered from Wikipedia article "List of US State Abbrevations",
## URL = https://en.wikipedia.org/wiki/List_of_U.S._state_abbreviations
## Accessed 10/08/2018 (MDY),
## imported into Google Slides using IMPORTHTML(),
## exported unmodified to TSV.
##
## Extracting
abbreviations <- read_tsv("stateAbbreviations.tsv", skip = 3,
                          col_names = c(
                              "state", "type", "ISO", "ST", "numeric", "USPS",
                              "USCG", "GPO", "AP", "other"
                          )) %>% 
    select(state, ST) %>% 
    slice(1:52)
abbreviations$state[abbreviations$state == "United States of America"] <- "United States"


## Population data import from Census Bureau series NST-est2017
population <- read_csv("nst-est2017-alldata.csv", 
                    col_names = TRUE, na = c("", "X", 0)) %>% 
    `colnames<-`(tolower(colnames(.))) %>% 
    mutate(sumlev = factor(sumlev, labels = c("National", "Regional", "State/Teritory"))) %>% 
    mutate(region = factor(region, labels = c("Northeast", "Midwest", "South", "West"))) %>%
    mutate(division = factor(division, labels = c(
        "New England",
        "Mid-Atlantic",
        "East North Central",
        "West North Central",
        "South Atlantic",
        "East South Central",
        "West South Central", 
        "Mountain",
        "Pacific"
    ))) %>% 
    mutate(state = name) %>% 
    merge(abbreviations, by = "state", sort = FALSE)


## drop birth, death, immigration, etc. columns from above
smallpop <- population %>% 
    select(state, ST, region, division, census2010pop,
           estimatesbase2010, starts_with("popestimate")
           ) %>% 
    `colnames<-`(gsub("popestimate", "", colnames(.))) # shorten yearly est. column names