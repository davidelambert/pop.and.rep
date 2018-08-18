library(tidyverse)
library(fiftystater)

load("pop.and.rep.Rda")
data("fifty_states")
glimpse(fifty_states)

pop.and.rep <- pop.and.rep %>% 
    filter(ST != "DC") %>% 
    group_by(bea.region) %>% 
    mutate(region.pct.pop = sum(pct.pop.2017) * 100,
           region.pct.senate = sum(senate.republicans) +
                               sum(senate.democrats) +
                               sum(senate.independents),
           region.senate.ratio = region.pct.senate / region.pct.pop,
           state = str_to_lower(state)
           ) %>% 
    ungroup()

fifty_states <- fifty_states %>% 
    filter(id != "district of columbia") %>% 
    left_join(pop.and.rep, by = c("id" = "state"))



## Summarise groups per region
reg.new.england <- fifty_states %>% 
    filter(bea.region == "New England") %>% 
    group_by(group) %>% 
    summarise()

reg.mid.east <- fifty_states %>% 
    filter(bea.region == "Mid East") %>% 
    group_by(group) %>% 
    summarise()

reg.great.lakes <- fifty_states %>% 
    filter(bea.region == "Great Lakes") %>% 
    group_by(group) %>% 
    summarise()

reg.plains <- fifty_states %>% 
    filter(bea.region == "Plains") %>% 
    group_by(group) %>% 
    summarise()

reg.south.east <- fifty_states %>% 
    filter(bea.region == "South East") %>% 
    group_by(group) %>% 
    summarise()

reg.south.west <- fifty_states %>% 
    filter(bea.region == "South West") %>% 
    group_by(group) %>% 
    summarise()

reg.rocky.mts <- fifty_states %>% 
    filter(bea.region == "Rocky Mtns") %>% 
    group_by(group) %>% 
    summarise()

reg.far.west <- fifty_states %>% 
    filter(bea.region == "Far West") %>% 
    group_by(group) %>% 
    summarise()


## Recode map groups
regions.map <- fifty_states
levels(regions.map$group) <- list("New England.1" = "Connecticut.1" ,
              "New England.2" = "Maine.1" ,
              "New England.3" = "Maine.2" ,
              "New England.4" = "Massachusetts.1" ,
              "New England.5" = "Massachusetts.2" ,
              "New England.6" = "Massachusetts.3" ,
              "New England.7" = "New Hampshire.1" ,
              "New England.8" = "Rhode Island.1" ,
              "New England.9" = "Rhode Island.2" ,
              "New England.10" = "Rhode Island.2" ,
              "New England.11" = "Vermont.1" ,
              "Mid East.1" = "Delaware.1" ,
              "Mid East.2" = "Maryland.1" ,
              "Mid East.3" = "Maryland.2" ,
              "Mid East.4" = "Maryland.3" ,
              "Mid East.5" = "Maryland.4" ,
              "Mid East.6" = "New Jersey.1" ,
              "Mid East.7" = "New York.1" ,
              "Mid East.8" = "New York.2" ,
              "Mid East.9" = "New York.3" ,
              "Mid East.10" = "New York.4" ,
              "Mid East.11" = "Pennsylvania.1" ,
              "Mid East.12" = "District of Columbia.1" ,
              "Great Lakes.1" = "Illinois.1" ,
              "Great Lakes.2" = "Indiana.1" ,
              "Great Lakes.3" = "Michigan.1" ,
              "Great Lakes.4" = "Michigan.2" ,
              "Great Lakes.5" = "Michigan.3" ,
              "Great Lakes.6" = "Michigan.4" ,
              "Great Lakes.7" = "Michigan.5" ,
              "Great Lakes.8" = "Ohio.1" ,
              "Great Lakes.9" = "Wisconsin.1" ,
              "Great Lakes.10" = "Wisconsin.2" ,
              "Plains.1" = "Iowa.1" ,
              "Plains.2" = "Kansas.1" ,
              "Plains.3" = "Minnesota.1" ,
              "Plains.4" = "Missouri.1" ,
              "Plains.5" = "Nebraska.1" ,
              "Plains.6" = "North Dakota.1" ,
              "Plains.7" = "South Dakota.1" ,
              "South East.1" = "Alabama.1" ,
              "South East.2" = "Arkansas.1" ,
              "South East.3" = "Florida.1" ,
              "South East.4" = "Florida.2" ,
              "South East.5" = "Florida.3" ,
              "South East.6" = "Florida.4" ,
              "South East.7" = "Florida.5" ,
              "South East.8" = "Florida.6" ,
              "South East.9" = "Florida.7" ,
              "South East.10" = "Georgia.1" ,
              "South East.11" = "Georgia.2" ,
              "South East.12" = "Kentucky.1" ,
              "South East.13" = "Kentucky.2" ,
              "South East.14" = "Louisiana.1" ,
              "South East.15" = "Louisiana.2" ,
              "South East.16" = "Louisiana.3" ,
              "South East.17" = "Louisiana.4" ,
              "South East.18" = "Mississippi.1" ,
              "South East.19" = "North Carolina.1" ,
              "South East.20" = "North Carolina.2" ,
              "South East.21" = "North Carolina.3" ,
              "South East.22" = "North Carolina.4" ,
              "South East.23" = "North Carolina.5" ,
              "South East.24" = "North Carolina.6" ,
              "South East.25" = "South Carolina.1" ,
              "South East.26" = "South Carolina.2" ,
              "South East.27" = "Tennessee.1" ,
              "South East.28" = "Virginia.1" ,
              "South East.29" = "Virginia.2" ,
              "South East.30" = "Virginia.3" ,
              "South East.31" = "West Virginia.1" ,
              "South West.1" = "Arizona.1" ,
              "South West.2" = "New Mexico.1" ,
              "South West.3" = "Oklahoma.1" ,
              "South West.4" = "Texas.1" ,
              "South West.5" = "Texas.2" ,
              "South West.6" = "Texas.3" ,
              "South West.7" = "Texas.4" ,
              "South West.8" = "Texas.5" ,
              "South West.9" = "Texas.6" ,
              "Rocky Mtns.1" = "Colorado.1" ,
              "Rocky Mtns.2" = "Idaho.1" ,
              "Rocky Mtns.3" = "Montana.1" ,
              "Rocky Mtns.4" = "Utah.1" ,
              "Rocky Mtns.5" = "Wyoming.1" ,
              "Far West.1" = "Alaska.1" ,
              "Far West.2" = "Alaska.2" ,
              "Far West.3" = "Alaska.3" ,
              "Far West.4" = "Alaska.4" ,
              "Far West.5" = "Alaska.5" ,
              "Far West.6" = "Alaska.6" ,
              "Far West.7" = "Alaska.7" ,
              "Far West.8" = "Alaska.8" ,
              "Far West.9" = "Alaska.9" ,
              "Far West.10" = "Alaska.10" ,
              "Far West.11" = "Alaska.11" ,
              "Far West.12" = "Alaska.12" ,
              "Far West.13" = "Alaska.13" ,
              "Far West.14" = "Alaska.14" ,
              "Far West.15" = "Alaska.15" ,
              "Far West.16" = "Alaska.16" ,
              "Far West.17" = "Alaska.17" ,
              "Far West.18" = "Alaska.18" ,
              "Far West.19" = "Alaska.19" ,
              "Far West.20" = "Alaska.20" ,
              "Far West.21" = "Alaska.21" ,
              "Far West.22" = "Alaska.22" ,
              "Far West.23" = "Alaska.23" ,
              "Far West.24" = "Alaska.24" ,
              "Far West.25" = "Alaska.25" ,
              "Far West.26" = "Alaska.26" ,
              "Far West.27" = "Alaska.27" ,
              "Far West.28" = "Alaska.28" ,
              "Far West.29" = "Alaska.29" ,
              "Far West.30" = "Alaska.30" ,
              "Far West.31" = "Alaska.31" ,
              "Far West.32" = "Alaska.32" ,
              "Far West.33" = "California.1" ,
              "Far West.34" = "California.2" ,
              "Far West.35" = "California.3" ,
              "Far West.36" = "California.4" ,
              "Far West.37" = "California.5" ,
              "Far West.38" = "Hawaii.1" ,
              "Far West.39" = "Hawaii.2" ,
              "Far West.40" = "Hawaii.3" ,
              "Far West.41" = "Hawaii.4" ,
              "Far West.42" = "Hawaii.5" ,
              "Far West.43" = "Hawaii.6" ,
              "Far West.44" = "Hawaii.7" ,
              "Far West.45" = "Nevada.1" ,
              "Far West.46" = "Oregon.1" ,
              "Far West.47" = "Washington.1" ,
              "Far West.48" = "Washington.2" ,
              "Far West.49" = "Washington.3"
              )

regions.map <- within(regions.map, {
    id[ id == "maine" | id == "new hampshire" | id == "vermont" |
        id == "massachusetts" | id == "rhode island" | id == "connecticut"
    ] <- "new england"
    id[ id == "new york" | id == "new jersey" | id == "pennsylvania" |
        id == "delware" | id == "maryland" | id == "district of columbia"
    ] <- "mid east"
    id[ id == "ohio" | id == "indiana" | id == "illinois" |
        id == "michigan" | id == "wisconsin"
    ] <- "great lakes"
    id[ id == "minnesota" | id == "iowa" | id == "missouri" | id == "north dakota" | 
        id == "south dakota" | id == "nebreaska" | id == "kansas"
    ] <- "plains"
    id[ id == "virgina" | id == "west virgina" | id == "kentucky" |
        id == "north carolina" | id == "tennessee" | id == "south carolina" |
        id == "georgia" | id == "alabama" | id == "mississippi" |
        id == "arkansas" | id == "louisiana" | id == "florida"
    ] <- "south east"
    id[ id == "oklahoma" | id == "texas" | id == "new mexico" | id == "arizona"
    ] <- "south west"
    id[ id == "montana" | id == "wyoming" | id == "colorado" |
        id == "idaho" | id == "utah"
    ] <- "rocky mtns"
    id[ id == "washington" | id == "oregon" | id == "nevada" |
        id == "california" | id == "alaska" | id == "hawaii"
    ] <- "far west"
})
