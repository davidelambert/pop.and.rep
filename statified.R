## HEADERS ===================================================================================

library(tidyverse)
library(fiftystater)

pop.and.rep <- readRDS("pop.and.rep.Rds")
data("fifty_states")
glimpse(fifty_states)





## SUMMARY VARIABLES =========================================================================

pop.and.rep <- pop.and.rep %>% 
    filter(ST != "DC" & ST != "US") %>% 
    mutate(state.senate.ratio = .02/pct.pop.2017) %>% 
    group_by(bea.region) %>% 
    mutate(region.pct.pop = sum(pct.pop.2017) * 100,
           region.pct.senate = sum(senate.democrats) +
                               sum(senate.republicans) +
                               sum(senate.independents),
           region.senate.ratio = region.pct.senate / region.pct.pop,
           region.senate.gop = sum(senate.republicans) / region.pct.senate * 100,
           id = str_to_lower(state)  # for merging w/ fifty_states
           ) %>% 
    ungroup() 





## STATE BAR CHART ===========================================================================

ggplot(pop.and.rep, aes(x = state.senate.ratio, y = reorder(ST, state.senate.ratio))) +
    geom_segment(aes(yend = ST), xend = 0, color = "grey60") +
    geom_point(color = "red4") +
    scale_x_continuous(breaks = seq(0,12,1)) +
    labs(x = "", y = "") +
    theme_bw() +
    theme(panel.grid.minor.x = element_line(linetype = "dashed"),
          panel.grid.major.y = element_blank(),
          axis.text.y = element_text(size = 6)
          )




## STATE MAP ================================================================================= 

fifty_states <- fifty_states %>% 
    filter(id != "district of columbia") %>% 
    left_join(pop.and.rep, by = "id")

ggplot(fifty_states) +
    geom_polygon(aes(group = group, x = long, y = lat,
                     fill = state.senate.ratio), color = "black", size = .1) +
    scale_fill_gradient2("", low = "black", mid = "white", high = "red4", midpoint = 1.0) +
    coord_map(projection = "eisenlohr", # best alternatives: vandergrinten, aitoff
              xlim = c(-124, -68)) + 
    labs(x = "", y = "") +
    theme(legend.direction = "horizontal",
          legend.position = c(0.6, 0.1),
          legend.background = element_blank(),
          legend.key.size = unit(12, "points"),
          legend.text = element_text(size = 7),
          axis.text = element_blank(),
          axis.ticks = element_blank()
          )
