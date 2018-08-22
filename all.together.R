## HEADERS ===================================================================================

library(tidyverse)
library(fiftystater)

pop.and.rep <- readRDS("pop.and.rep.Rds")
data("fifty_states")
glimpse(fifty_states)





## SUMMARY VARIABLES =========================================================================

pop.and.rep <- pop.and.rep %>% 
    filter(ST != "DC" & ST != "US") %>% 
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





## BAR CHART =================================================================================

# Force region labels onto multiple lines for legibility
levels(pop.and.rep$bea.region) <- gsub(" ", "\n", levels(pop.and.rep$bea.region))
levels(pop.and.rep$bea.region) <- gsub("-", "\n", levels(pop.and.rep$bea.region))


ggplot(pop.and.rep, aes(x = bea.region)) +
    ##********************************************************************************
    ##
    ## Population percentages
    ##
    geom_col(aes(y = region.pct.pop), fill = "darkorchid3",
             width = 0.4, position = position_nudge(x = -0.2)) +
    geom_text(aes(y = region.pct.pop, label = round(region.pct.pop, 1)),
              fontface = "bold", color = "white", size = 3.5, angle = 90,
              position = position_nudge(x = -0.2, y = -1.5)) +
    ##********************************************************************************
    ##
    ## Senate representation
    ##
    geom_col(aes(y = region.pct.senate), fill = "purple4", 
             width = 0.4, position = position_nudge(x = .2)) +
    geom_text(aes(y = region.pct.senate, label = region.pct.senate),
              fontface = "bold", color = "white", size = 3.5, angle = 90,
              position = position_nudge(x = 0.2, y = -1.5)) +
    ##********************************************************************************
    ##
    ## Percent GOP labels
    ##
    annotate("text", x = 1, y = -0.75, color = "red2", hjust = 0.5, fontface = "bold",
             size = 3.5, label = round(pop.and.rep$region.senate.gop[1], 1)) +
    annotate("text", x = 2, y = -0.75, color = "red2", hjust = 0.5, fontface = "bold",
             size = 3.5, label = round(pop.and.rep$region.senate.gop[2], 1)) +
    annotate("text", x = 3, y = -0.75, color = "red2", hjust = 0.5, fontface = "bold",
             size = 3.5, label = round(pop.and.rep$region.senate.gop[3], 1)) +
    annotate("text", x = 4, y = -0.75, color = "red2", hjust = 0.5, fontface = "bold",
             size = 3.5, label = round(pop.and.rep$region.senate.gop[4], 1)) +
    annotate("text", x = 5, y = -0.75, color = "red2", hjust = 0.5, fontface = "bold",
             size = 3.5, label = round(pop.and.rep$region.senate.gop[5], 1)) +
    annotate("text", x = 6, y = -0.75, color = "red2", hjust = 0.5, fontface = "bold",
             size = 3.5, label = round(pop.and.rep$region.senate.gop[6], 1)) +
    annotate("text", x = 7, y = -0.75, color = "red2", hjust = 0.5, fontface = "bold",
             size = 3.5, label = round(pop.and.rep$region.senate.gop[7], 1)) +
    annotate("text", x = 8, y = -0.75, color = "red2", hjust = 0.5, fontface = "bold",
             size = 3.5, label = round(pop.and.rep$region.senate.gop[8], 1)) +
    ##********************************************************************************
    ##
    ## Legend
    ##
    geom_tile(aes(x = 1.25, y = 23.4, width = 1.25, height = 2), fill = "darkorchid3") +
    annotate("text", x = 1.25, y = 23.5, label = "% Population",
             size = 3.5, fontface = "bold", color = "white") +
    geom_tile(aes(x = 2.5, y = 23.4, width = 1.25, height = 2), fill = "purple4") +
    annotate("text", x = 2.5, y = 23.5, label = "% Senators",
             size = 3.5, fontface = "bold", color = "white") +
    annotate("text", x = 1.875, y = 21.5, label = "% GOP Senators",
             size = 3.5, fontface = "bold", color = "red2") +
    ##********************************************************************************
    ##
    ## Theme adjustments
    ##
    xlab("") + ylab("") + 
    theme(panel.grid.major.x = element_blank(),
          axis.text.x = element_text(face = "bold", lineheight = 1.1, size = 10)
          )





## REGIONAL MAP ==============================================================================

fifty_states <- fifty_states %>% 
    filter(id != "district of columbia") %>% 
    left_join(pop.and.rep, by = "id")


ggplot(fifty_states) +
    geom_polygon(aes(group = group, x = long, y = lat,
                     fill = region.senate.ratio), color = "black", size = .1) +
    scale_fill_gradient2("", low = "black", mid = "white", high = "purple4", midpoint = 1.0) +
    coord_map(projection = "eisenlohr", # best alternatives: vandergrinten, aitoff
              xlim = c(-124, -68)) + 
    # Annotations ............................................................................
    annotate("label", x = -119, y = 40, fontface = "bold", size = 3.5, label = "Far West") +
    annotate("label", x = -110, y = 43.5, fontface = "bold", size = 3.5, label = "Rocky Mtns") +
    annotate("label", x = -104, y = 34, fontface = "bold", size = 3.5, label = "South West") +
    annotate("label", x = -85, y = 34.5, fontface = "bold", size = 3.5, label = "South East") +
    annotate("label", x = -97, y = 43, fontface = "bold", size = 3.5, label = "Plains") +
    annotate("label", x = -86, y = 41, fontface = "bold", size = 3.5, label = "Great Lakes") +
    annotate("label", x = -77, y = 42, fontface = "bold", size = 3.5, label = "Mid East") +
    annotate("label", x = -72.5, y = 46, fontface = "bold", size = 3.5, label = "New England") +
    labs(x = "", y = "") +
    theme(legend.direction = "horizontal",
          legend.position = c(0.6, 0.1),
          legend.background = element_blank(),
          legend.key.size = unit(12, "points"),
          legend.text = element_text(size = 7),
          axis.text = element_blank(),
          axis.ticks = element_blank()
          )
