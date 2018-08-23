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
    mutate(ratio.group = factor(if_else(state.senate.ratio <= 0.8, "under",
                                 if_else(state.senate.ratio > 0.8 & state.senate.ratio <= 1.5,
                                         "ok", "over")))
           ) %>% 
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


fifty_states <- fifty_states %>% 
    filter(id != "district of columbia") %>% 
    left_join(pop.and.rep, by = "id")




## STATE BAR CHART ===========================================================================

ggplot(pop.and.rep, aes(y = state.senate.ratio, x = reorder(ST, state.senate.ratio))) +
    geom_col(aes(fill = state.senate.ratio), width = 0.8) +
    geom_hline(yintercept = 1, linetype = "dashed") +
    coord_flip() +
    scale_y_continuous(breaks = seq(0,12,2)) +
    scale_fill_viridis_c("", breaks = c(1,10), labels = c("1:1", "10:1"),
                         option = "plasma", direction = -1) +
    labs(x = "", y = "") +
    theme_bw() +
    theme(panel.grid.minor.x = element_line(linetype = "dashed"),
          panel.grid.major.y = element_blank(),
          axis.text.y = element_text(size = 7),
          legend.direction = "horizontal",
          legend.position = c(0.655, 0.1),
          legend.key.width = unit(36, "points"),
          legend.key.height = unit(10, "points"),
          legend.background = element_blank(),
          legend.text = element_text(size = 10)
          )




## STATE MAP ================================================================================= 

ggplot(fifty_states) +
    geom_polygon(aes(group = group, x = long, y = lat, fill = state.senate.ratio)) +
    scale_fill_viridis_c("", option = "plasma", direction = -1) +
    coord_map(projection = "eisenlohr", # best alternatives: vandergrinten, aitoff
              xlim = c(-124, -68)) + 
    labs(x = "", y = "") +
    guides(fill = FALSE) +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank()
          )
