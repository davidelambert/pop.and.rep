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


label.farwest <- by.bea %>% 
    filter(bea.region == "Far West") %>% 
    paste("Far West ",by.bea$bea.pct.gop.senate,"% GOP")

ggplot(fifty_states) +
    geom_polygon(aes(group = group, x = long, y = lat,
                     fill = region.senate.ratio), color = "gray40") +
    scale_fill_gradient2("", low = "blue", mid = "white", high = "red",
                         midpoint = 1.0) +
    coord_map(projection = "eisenlohr", # best alternatives: vandergrinten, aitoff
              xlim = c(-124, -68)) + 
    annotate("label", x = -119, y = 40, fontface = "bold", size = 3, lineheight = 0.85,
             label = paste("Far West:\nSenators\n",
                           round(by.bea$bea.pct.gop.senate[by.bea$bea.region ==
                            "Far West"], 1), "% GOP", sep = "")) +
    annotate("label", x = -110, y = 44, fontface = "bold", size = 3, lineheight = 0.85,
             label = paste("Rocky Mtns:\nSenators\n",
                           round(by.bea$bea.pct.gop.senate[by.bea$bea.region ==
                            "Rocky Mtns"], 1),"% GOP", sep = "")) +
    annotate("label", x = -105, y = 34, fontface = "bold", size = 3, lineheight = 0.85,
             label = paste("South West:\nSenators ",
                           round(by.bea$bea.pct.gop.senate[by.bea$bea.region ==
                            "South West"], 1),"% GOP", sep = "")) +
    annotate("label", x = -87, y = 34, fontface = "bold", size = 3, lineheight = 0.85,
             label = paste("South East:\nSenators ",
                           round(by.bea$bea.pct.gop.senate[by.bea$bea.region ==
                            "South East"], 1),"% GOP", sep = "")) +
    annotate("label", x = -98, y = 43, fontface = "bold", size = 3, lineheight = 0.85,
             label = paste("Plains:\nSenators\n",
                           round(by.bea$bea.pct.gop.senate[by.bea$bea.region ==
                            "Plains"], 1),"% GOP", sep = "")) +
    annotate("label", x = -86, y = 42, fontface = "bold", size = 3, lineheight = 0.85,
             label = paste("Great Lakes:\nSenators\n",
                           round(by.bea$bea.pct.gop.senate[by.bea$bea.region ==
                            "Great Lakes"], 1),"% GOP", sep = "")) +
    annotate("label", x = -77.2, y = 42.5, fontface = "bold", size = 3, lineheight = 0.85,
             label = paste("Mid East:\nSenators\n",
                           round(by.bea$bea.pct.gop.senate[by.bea$bea.region ==
                            "Mid East"], 1),"% GOP", sep = "")) +
    annotate("label", x = -72.5, y = 46.7, fontface = "bold", size = 3, lineheight = 0.85,
             label = paste("New England:\nSenators\n",
                           round(by.bea$bea.pct.gop.senate[by.bea$bea.region ==
                            "New England"], 1),"% GOP", sep = "")) +
    labs(x = "", y = "") +
    theme(legend.direction = "horizontal",
          legend.position = c(0.6, 0.1),
          legend.background = element_blank(),
          legend.key.size = unit(12, "points"),
          legend.text = element_text(size = 7),
          axis.text = element_blank(),
          axis.ticks = element_blank()
          )

