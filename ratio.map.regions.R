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
                     fill = region.senate.ratio), color = "gray40") +
    scale_fill_gradient2("", low = "blue", mid = "white", high = "red",
                         midpoint = 1.0) +
    coord_map(projection = "eisenlohr", # best alternatives: vandergrinten, aitoff
              xlim = c(-124, -68)) + 
    # Annotations ............................................................................
    annotate("label", x = -119, y = 40, fontface = "bold", size = 3, lineheight = 0.85,
             label = "Far West") +
    annotate("label", x = -110, y = 43.5, fontface = "bold", size = 3, lineheight = 0.85,
             label = "Rocky Mtns") +
    annotate("label", x = -104, y = 34, fontface = "bold", size = 3, lineheight = 0.85,
             label = "South West") +
    annotate("label", x = -85, y = 34.5, fontface = "bold", size = 3, lineheight = 0.85,
             label = "South East") +
    annotate("label", x = -97, y = 43, fontface = "bold", size = 3, lineheight = 0.85,
             label = "Plains") +
    annotate("label", x = -86, y = 41, fontface = "bold", size = 3, lineheight = 0.85,
             label = "Great Lakes") +
    annotate("label", x = -77, y = 42, fontface = "bold", size = 3, lineheight = 0.85,
             label = "Mid East") +
    annotate("label", x = -72.5, y = 46, fontface = "bold", size = 3, lineheight = 0.85,
             label = "New England") +
    labs(x = "", y = "") +
    theme(legend.direction = "horizontal",
          legend.position = c(0.6, 0.1),
          legend.background = element_blank(),
          legend.key.size = unit(12, "points"),
          legend.text = element_text(size = 7),
          axis.text = element_blank(),
          axis.ticks = element_blank()
          )

