library(AER)
library(descr)
library(tidyverse)

pop.and.rep <- readRDS("pop.and.rep.rds")

by.division <- pop.and.rep %>% 
    filter(ST != "DC") %>% 
    group_by(division) %>% 
    summarise(div.pop = sum(pop.2017),
              div.pct.pop = sum(pct.pop.2017) * 100,
              div.pct.electors = sum(electors) / 538 * 100,
              div.prop.senate = (sum(senate.republicans) +
                                sum(senate.democrats) +
                                sum(senate.independents)),
              div.pct.gop = (sum(house.republicans) + sum(senate.republicans)) / 
                             sum(total.delegation) * 100,
              div.pct.dem.ind = (sum(house.democrats) +
                                 sum(senate.democrats) +
                                 sum(senate.independents)) / 
                                 sum(total.delegation) * 100,
              div.pct.women = sum(total.women) / sum(total.delegation) * 100
              )

qplot(division, div.prop.senate, data = by.division, geom = "col")
