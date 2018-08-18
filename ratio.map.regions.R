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
           id = str_to_lower(state)
           ) %>% 
    ungroup()

fifty_states <- fifty_states %>% 
    filter(id != "district of columbia") %>% 
    left_join(pop.and.rep, by = "id")

ggplot(fifty_states) +
    geom_polygon(aes(group = group, x = long, y = lat,
                     fill = region.senate.ratio), color = "black") +
    scale_fill_gradientn("", colors = c("plum2", "orchid2", 
                                        "mediumorchid2", "darkorchid2")) +
    coord_map(projection = "eisenlohr") + # best alternatives: vandergrinten, aitoff
    labs(x = "", y = "", title = "Senate Representation to Population Ratio") +
    theme_void() +
    theme(legend.position = c(0.92, 0.27),
          plot.title = element_text(hjust = 0.5)
          )
