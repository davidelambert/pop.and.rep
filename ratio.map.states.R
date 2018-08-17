library(tidyverse)
library(fiftystater)

pop.and.rep <- read_csv("pop.and.rep.csv")
data("fifty_states")
glimpse(fifty_states)

pop.and.rep <- pop.and.rep %>% 
    filter(ST != "DC") %>% 
    mutate(senate.ratio = .02/pct.pop.2017,
           id = str_to_lower(state))

fifty_states <- fifty_states %>% 
    filter(id != "district of columbia") %>% 
    left_join(pop.and.rep, by = "id")

ggplot(fifty_states) +
    geom_polygon(aes(group = group, x = long, y = lat, fill = senate.ratio)) +
    scale_fill_gradientn("", colors = c("orchid1", "mediumorchid2",
                                        "darkorchid3", "purple4")) +
    coord_quickmap() +
    labs(x = "", y = "", title = "Senate Representation to Population Ratio") +
    theme_void() +
    theme(legend.position = c(0.92, 0.27),
          plot.title = element_text(hjust = 0.5)
          )
