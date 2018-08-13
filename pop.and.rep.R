library(tidyverse)
library(readxl)

## ABBREVIATIONS =============================================================================
##
## Abbreviations to be merged into other tables  below.
## Data gathered from Wikipedia article "List of US State Abbrevations",
## URL = https://en.wikipedia.org/wiki/List_of_U.S._state_abbreviations
## Accessed 10/08/2018 (MDY),
## imported into Google Slides using IMPORTHTML(),
## exported unmodified to TSV.
##
abbreviations <- read_tsv("stateAbbreviations.tsv", skip = 3,
                          col_names = c(
                              "state", "type", "ISO", "ST", "numeric", "USPS",
                              "USCG", "GPO", "AP", "other"
                          )) %>% 
    select(state, ST) %>% 
    slice(1:52)
abbreviations$state[abbreviations$state == "United States of America"] <- "United States"





## POPULATION ================================================================================
##
## Population data import from Census Bureau series NST-est2017
##
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
##
population.small <- population %>% 
    select(state, ST, region, division, census2010pop,
           estimatesbase2010, starts_with("popestimate")
           ) %>% 
    `colnames<-`(gsub("popestimate", "pop.", colnames(.))) %>% # shorten yearly est. column names
    mutate(pct.pop.2017 = `pop.2017`/.[[1,14]]) 





## ELECTORS ==================================================================================
##
## total number of electors per state
## Data gathered from Wikipedia Article "Electoral College (United States)",
## URL = https://en.wikipedia.org/wiki/Electoral_College_(United_States)
## Accessed 10/08/2018 (MDY),
## imported into Google Slides using IMPORTHTML(),
## exported unmodified to CSV.
##
electoral.college <- read_csv("electoralCollege.csv", skip = 1) %>% 
    slice(3:54) %>% 
    select(X2, X36) %>% 
    `colnames<-`(c("state", "electors")) 
electoral.college$state[[1]] <- "United States"
electoral.college$state[[9]] <- "District of Columbia"
electoral.college <- merge(electoral.college, abbreviations, by = "state", sort = FALSE)
electoral.college$electors <- as.integer(electoral.college$electors)
electoral.college$electors[[1]] <- 538L
electoral.college <- electoral.college %>% 
    mutate(pct.electors = `electors`/.[[1,2]]) %>% 
    select(state, electors, pct.electors)





## CONGRESS ==================================================================================
##
## Redone Congress tibble w/ cleaner source dataset
## sourced from "unitedstates" GitHub collective account,
## URL: https://github.com/unitedstates/congress-legislators
## accessed 11-08-2018 (mdy)
##
congress <- read_csv("legislators-current.csv") %>% 
    select(state, type, party, gender, birthday, last_name, first_name) %>% 
    `colnames<-`(c(
        "ST", "chamber", "party", "gender", "birthdate", "last.name", "first.name"
    )) %>% 
    mutate(ST = factor(ST)) %>% 
    mutate(chamber = factor(chamber, labels = c("House", "Senate"))) %>% 
    mutate(party = factor(party)) %>% 
    mutate(gender = factor(gender)) %>% 
    filter(ST != "AS" &          # Removing non-voting members. Including DC
               ST != "GU" &      # even though DC receives Electoral votes
               ST != "DC" &      # so that obs. matches official tally.
               ST != "MP" &
               ST != "PR" &
               ST != "VI"
     ) %>% 
    merge(abbreviations, by = "ST", sort = FALSE)



## The following discovers vacant seats in the House & inserts rows for those seats
## into the "congress" dataset. Apportionment table gathered from Census Bureau
## URL: https://www.census.gov/population/apportionment/data/2010_apportionment_results.html
## Accessed 11-08-18 (mdy)
##
## NOTE: some of these seats may have been filled or had elections since
## publication of the Congress table. GitHub reports Congress table was updated 31/07/2018.
##
## Gather counts of seated Representatives by state. Exclude DC b/c delegate is not apportioned.
##
seated.reps <- congress %>% 
    filter(chamber == "House") %>% 
    group_by(ST) %>% 
    summarise(seats.filled = n())

## Read in apportionments from 2010 Census, match to seated.reps, & tally mismatches
##
unfilled <- read_excel("ApportionmentPopulation2010.xls", skip = 11, n_max = 50,
                            col_names = c("state", "pop", "_", "seats.apportioned",
                                          "-", "change")) %>% 
    select(state, seats.apportioned) %>% 
    merge(abbreviations, by = "state", sort = FALSE) %>% 
    merge(seated.reps, by = "ST", sort = FALSE) %>% 
    filter(seats.apportioned != seats.filled)
unfilled

## Add rows for vacant seats
##
congress <- congress %>% 
    add_row(ST = "MI", chamber = "House", party = NA, gender = NA,
            birthdate = NA, last.name = NA, first.name = NA) %>% 
    add_row(ST = "NY", chamber = "House", party = NA, gender = NA,
            birthdate = NA, last.name = NA, first.name = NA) %>% 
    add_row(ST = "OH", chamber = "House", party = NA, gender = NA,
            birthdate = NA, last.name = NA, first.name = NA) %>% 
    add_row(ST = "OK", chamber = "House", party = NA, gender = NA,
            birthdate = NA, last.name = NA, first.name = NA) %>% 
    add_row(ST = "PA", chamber = "House", party = NA, gender = NA,
            birthdate = NA, last.name = NA, first.name = NA) %>% 
    add_row(ST = "PA", chamber = "House", party = NA, gender = NA,
            birthdate = NA, last.name = NA, first.name = NA) 



## TALLY =====================================================================================
##
## Tallies each State's Congressional delegation by party & chamber.
## First line create table to reference in subsequent left_join() commands.
##
congress.tallies <- congress               
congress.tallies <- congress.tallies %>% 
    group_by(ST) %>%                       # These two lines collapse the membership
    summarise() %>%                        # table individual states.
    left_join(congress.tallies %>% 
        filter(chamber == "House", party == "Republican") %>% 
        group_by(ST) %>% 
        summarise(house.republicans = n())
        ) %>% 
    left_join(congress.tallies %>% 
        filter(chamber == "House", party == "Democrat") %>% 
        group_by(ST) %>% 
        summarise(house.democrats = n())
        ) %>% 
    left_join(congress.tallies %>% 
        filter(chamber == "House", is.na(party)) %>% 
        group_by(ST) %>% 
        summarise(house.vacancies = n())
        ) %>% 
    left_join(congress.tallies %>% 
        filter(chamber == "Senate", party == "Republican") %>% 
        group_by(ST) %>% 
        summarise(senate.republicans = n())
        ) %>% 
    left_join(congress.tallies %>% 
        filter(chamber == "Senate", party == "Democrat") %>% 
        group_by(ST) %>% 
        summarise(senate.democrats = n())
        ) %>% 
    left_join(congress.tallies %>% 
        filter(chamber == "Senate", party == "Independent") %>% 
        group_by(ST) %>% 
        summarise(senate.independents = n())
        ) %>% 
    left_join(congress.tallies %>% 
        filter(chamber == "House", gender == "F") %>% 
        group_by(ST) %>% 
        summarise(house.women = n())
        ) %>% 
    left_join(congress.tallies %>% 
        filter(chamber == "Senate", gender == "F") %>% 
        group_by(ST) %>% 
        summarise(senate.women = n())
        )
congress.tallies <- replace(congress.tallies, is.na(congress.tallies), 0)

## Add totals & fractions. Folding Independents into Dems since Sanders & King causes Dem.
##
congress.tallies <- congress.tallies %>% 
    mutate(total.house = house.republicans +
                        house.democrats +
                        house.vacancies
          ) %>% 
    mutate(total.delegation = total.house + 2) %>% 
    mutate(total.women =  house.women + senate.women) %>% 
    mutate(total.republican.pct =
               (house.republicans+senate.republicans)/total.delegation) %>% 
    mutate(total.dem.ind.pct =
               (house.democrats+senate.democrats+senate.independents)/total.delegation) %>% 
    mutate(total.women.pct = total.women/total.delegation)



## MERGE AND CLEAN =========================================================================
##
## Merge tables into grand table for analysis.
## Clear environment of temporary & unnecessary tables.
##
pop.and.rep <- population.small %>% 
    merge(congress.tallies, by = "ST", sort = FALSE, all = TRUE) %>% 
    merge(electoral.college, by = "state", sort = FALSE, all = TRUE)
## Reorder for relevance
pop.and.rep <- pop.and.rep[c(1:4,14,15,30,31,25,27,28,26,29,5:13,16:24)]

remove(abbreviations, congress.tallies, electoral.college, population,
       population.small, seated.reps, unfilled)



## GROUPS ===================================================================================
## 
## Create grouped tables for descriptive statistics, graphics, exploratory analysis, etc.
##
by.division <- pop.and.rep %>% 
    filter(ST != "DC", !is.na(division)) %>% 
    group_by(division) %>% 
    summarise(div.pop = sum(pop.2017),
              div.pct.pop = sum(pct.pop.2017) * 100,
              div.pct.electors = sum(electors) / 538 * 100,
              div.prop.senate = (sum(senate.republicans) +
                                sum(senate.democrats) +
                                sum(senate.independents)),
              div.pct.gop = (sum(house.republicans) + sum(senate.republicans)) / 
                             sum(total.delegation) * 100,
              div.pct.gop.senate = sum(senate.republicans),
              div.pct.dem.ind = (sum(house.democrats) +
                                 sum(senate.democrats) +
                                 sum(senate.independents)) / 
                                 sum(total.delegation) * 100,
              div.pct.women = sum(total.women) / sum(total.delegation) * 100
              )




## EXPORT ====================================================================================

save(pop.and.rep, congress, by.division, file = "pop.and.rep.Rdata")

saveRDS(pop.and.rep, "pop.and.rep.rds")
write_csv(pop.and.rep, "pop.and.rep.csv")

saveRDS(congress, "congress.full.rds")
write_csv(congress, "congress.full.csv")

saveRDS(by.division, "by.division.rds")
write_csv(by.division, "by.division.csv")
