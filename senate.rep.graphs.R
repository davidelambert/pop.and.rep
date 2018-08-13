library(tidyverse)

load("pop.and.rep.Rda")

## BY DIVISION ===============================================================================
##
## Force division labels onto multiple lines for legibility
levels(by.division$division) <- gsub(" ", "\n", levels(by.division$division))
levels(by.division$division) <- gsub("-", "\n", levels(by.division$division))


ggplot(by.division, aes(x = division)) +
    ##********************************************************************************
    ##
    ## Population percentages
    ##
    geom_col(aes(y = div.pct.pop), fill = "dodgerblue4",
             width = 0.28, position = position_nudge(x = -0.28)) +
    geom_text(aes(y = div.pct.pop, label = round(div.pct.pop, 1)),
              fontface = "bold", color = "white", size = 3, angle = 90,
              position = position_nudge(x = -0.32, y = -1)) +
    ##********************************************************************************
    ##
    ## Electoral representation
    ##
    geom_col(aes(y = div.pct.electors), fill = "royalblue3", 
             width = 0.28, position = position_nudge(x = 0)) +
    geom_text(aes(y = div.pct.electors, label = round(div.pct.electors, 1)),
              fontface = "bold", color = "white", size = 3, angle = 90,
              position = position_nudge(x = -0.03, y = -1)) +
    ##********************************************************************************
    ##
    ## Senate representation
    ##
    geom_col(aes(y = div.prop.senate), fill = "steelblue3", 
             width = 0.28, position = position_nudge(x = .28)) +
    geom_text(aes(y = div.prop.senate, label = div.prop.senate),
              fontface = "bold", color = "white", size = 3, angle = 90,
              position = position_nudge(x = 0.24, y = -1)) +
    ##********************************************************************************
    ##
    ## Percent GOP labels
    ##
    annotate("text", x = 1, y = -0.75, color = "red2", hjust = 0.5, fontface = "bold",
             size = 3.5, label = round(by.division$div.pct.gop.senate[1], 1)) +
    annotate("text", x = 2, y = -0.75, color = "red2", hjust = 0.5, fontface = "bold",
             size = 3.5, label = round(by.division$div.pct.gop.senate[2], 1)) +
    annotate("text", x = 3, y = -0.75, color = "red2", hjust = 0.5, fontface = "bold",
             size = 3.5, label = round(by.division$div.pct.gop.senate[3.5], 1)) +
    annotate("text", x = 4, y = -0.75, color = "red2", hjust = 0.5, fontface = "bold",
             size = 3.5, label = round(by.division$div.pct.gop.senate[4], 1)) +
    annotate("text", x = 5, y = -0.75, color = "red2", hjust = 0.5, fontface = "bold",
             size = 3.5, label = round(by.division$div.pct.gop.senate[5], 1)) +
    annotate("text", x = 6, y = -0.75, color = "red2", hjust = 0.5, fontface = "bold",
             size = 3.5, label = round(by.division$div.pct.gop.senate[6], 1)) +
    annotate("text", x = 7, y = -0.75, color = "red2", hjust = 0.5, fontface = "bold",
             size = 3.5, label = round(by.division$div.pct.gop.senate[7], 1)) +
    annotate("text", x = 8, y = -0.75, color = "red2", hjust = 0.5, fontface = "bold",
             size = 3.5, label = round(by.division$div.pct.gop.senate[8], 1)) +
    annotate("text", x = 9, y = -0.75, color = "red2", hjust = 0.5, fontface = "bold",
             size = 3.5, label = round(by.division$div.pct.gop.senate[9], 1)) +
    ##********************************************************************************
    ##
    ## Legend
    ##
    geom_tile(aes(x = 1.25, y = 18.4, width = 1.25, height = 1.2), fill = "dodgerblue4") +
    annotate("text", x = 1.25, y = 18.5, label = "% Population",
             size = 3, fontface = "bold", color = "white") +
    geom_tile(aes(x = 2.5, y = 18.4, width = 1.25, height = 1.2), fill = "royalblue3") +
    annotate("text", x = 2.5, y = 18.5, label = "% Electors",
             size = 3, fontface = "bold", color = "white") +
    geom_tile(aes(x = 3.75, y = 18.4, width = 1.25, height = 1.2), fill = "steelblue3") +
    annotate("text", x = 3.75, y = 18.5, label = "% Senators",
             size = 3, fontface = "bold", color = "white") +
    annotate("text", x = 2.5, y = 17, label = "% GOP Senators",
             size = 3.5, fontface = "bold", color = "red2") +
    ##********************************************************************************
    ##
    ## Theme adjustments
    ##
    xlab("") + ylab("") + 
    theme(panel.grid.major.x = element_blank(),
          axis.text.x = element_text(face = "bold", lineheight = 1.1)
          )




## BY BEA REGION =============================================================================
##
## Force region labels onto multiple lines for legibility
levels(by.bea$bea.region) <- gsub(" ", "\n", levels(by.bea$bea.region))
levels(by.bea$bea.region) <- gsub("-", "\n", levels(by.bea$bea.region))


ggplot(by.bea, aes(x = bea.region)) +
    ##********************************************************************************
    ##
    ## Population percentages
    ##
    geom_col(aes(y = bea.pct.pop), fill = "seagreen4",
             width = 0.28, position = position_nudge(x = -0.28)) +
    geom_text(aes(y = bea.pct.pop, label = round(bea.pct.pop, 1)),
              fontface = "bold", color = "white", size = 3, angle = 90,
              position = position_nudge(x = -0.32, y = -1)) +
    ##********************************************************************************
    ##
    ## Electoral representation
    ##
    geom_col(aes(y = bea.pct.electors), fill = "springgreen3", 
             width = 0.28, position = position_nudge(x = 0)) +
    geom_text(aes(y = bea.pct.electors, label = round(bea.pct.electors, 1)),
              fontface = "bold", color = "white", size = 3, angle = 90,
              position = position_nudge(x = -0.03, y = -1)) +
    ##********************************************************************************
    ##
    ## Senate representation
    ##
    geom_col(aes(y = bea.prop.senate), fill = "aquamarine3", 
             width = 0.28, position = position_nudge(x = .28)) +
    geom_text(aes(y = bea.prop.senate, label = bea.prop.senate),
              fontface = "bold", color = "white", size = 3, angle = 90,
              position = position_nudge(x = 0.24, y = -1)) +
    ##********************************************************************************
    ##
    ## Percent GOP labels
    ##
    annotate("text", x = 1, y = -0.75, color = "red2", hjust = 0.5, fontface = "bold",
             size = 3.5, label = round(by.bea$bea.pct.gop.senate[1], 1)) +
    annotate("text", x = 2, y = -0.75, color = "red2", hjust = 0.5, fontface = "bold",
             size = 3.5, label = round(by.bea$bea.pct.gop.senate[2], 1)) +
    annotate("text", x = 3, y = -0.75, color = "red2", hjust = 0.5, fontface = "bold",
             size = 3.5, label = round(by.bea$bea.pct.gop.senate[3.5], 1)) +
    annotate("text", x = 4, y = -0.75, color = "red2", hjust = 0.5, fontface = "bold",
             size = 3.5, label = round(by.bea$bea.pct.gop.senate[4], 1)) +
    annotate("text", x = 5, y = -0.75, color = "red2", hjust = 0.5, fontface = "bold",
             size = 3.5, label = round(by.bea$bea.pct.gop.senate[5], 1)) +
    annotate("text", x = 6, y = -0.75, color = "red2", hjust = 0.5, fontface = "bold",
             size = 3.5, label = round(by.bea$bea.pct.gop.senate[6], 1)) +
    annotate("text", x = 7, y = -0.75, color = "red2", hjust = 0.5, fontface = "bold",
             size = 3.5, label = round(by.bea$bea.pct.gop.senate[7], 1)) +
    annotate("text", x = 8, y = -0.75, color = "red2", hjust = 0.5, fontface = "bold",
             size = 3.5, label = round(by.bea$bea.pct.gop.senate[8], 1)) +
    ##********************************************************************************
    ##
    ## Legend
    ##
    geom_tile(aes(x = 1.25, y = 23.4, width = 1.25, height = 1.2), fill = "seagreen4") +
    annotate("text", x = 1.25, y = 23.5, label = "% Population",
             size = 3, fontface = "bold", color = "white") +
    geom_tile(aes(x = 2.5, y = 23.4, width = 1.25, height = 1.2), fill = "springgreen3") +
    annotate("text", x = 2.5, y = 23.5, label = "% Electors",
             size = 3, fontface = "bold", color = "white") +
    geom_tile(aes(x = 3.75, y = 23.4, width = 1.25, height = 1.2), fill = "aquamarine3") +
    annotate("text", x = 3.75, y = 23.5, label = "% Senators",
             size = 3, fontface = "bold", color = "white") +
    annotate("text", x = 2.5, y = 22, label = "% GOP Senators",
             size = 3.5, fontface = "bold", color = "red2") +
    ##********************************************************************************
    ##
    ## Theme adjustments
    ##
    xlab("") + ylab("") + 
    theme(panel.grid.major.x = element_blank(),
          axis.text.x = element_text(face = "bold", lineheight = 1.1)
          )






## BY OMB REGION =============================================================================
##
## Force region labels onto multiple lines for legibility
levels(by.omb$omb.region) <- gsub(" ", "\n", levels(by.omb$omb.region))
levels(by.omb$omb.region) <- gsub("-", "\n", levels(by.omb$omb.region))


ggplot(by.omb, aes(x = omb.region)) +
    ##********************************************************************************
    ##
    ## Population percentages
    ##
    geom_col(aes(y = omb.pct.pop), fill = "purple4",
             width = 0.28, position = position_nudge(x = -0.28)) +
    geom_text(aes(y = omb.pct.pop, label = round(omb.pct.pop, 1)),
              fontface = "bold", color = "white", size = 3, angle = 90,
              position = position_nudge(x = -0.32, y = -1)) +
    ##********************************************************************************
    ##
    ## Electoral representation
    ##
    geom_col(aes(y = omb.pct.electors), fill = "darkorchid3", 
             width = 0.28, position = position_nudge(x = 0)) +
    geom_text(aes(y = omb.pct.electors, label = round(omb.pct.electors, 1)),
              fontface = "bold", color = "white", size = 3, angle = 90,
              position = position_nudge(x = -0.03, y = -1)) +
    ##********************************************************************************
    ##
    ## Senate representation
    ##
    geom_col(aes(y = omb.prop.senate), fill = "orchid3", 
             width = 0.28, position = position_nudge(x = .28)) +
    geom_text(aes(y = omb.prop.senate, label = omb.prop.senate),
              fontface = "bold", color = "white", size = 3, angle = 90,
              position = position_nudge(x = 0.24, y = -1)) +
    ##********************************************************************************
    ##
    ## Percent GOP labels
    ##
    annotate("text", x = 1, y = -0.75, color = "red2", hjust = 0.5, fontface = "bold",
             size = 3.5, label = round(by.omb$omb.pct.gop.senate[1], 1)) +
    annotate("text", x = 2, y = -0.75, color = "red2", hjust = 0.5, fontface = "bold",
             size = 3.5, label = round(by.omb$omb.pct.gop.senate[2], 1)) +
    annotate("text", x = 3, y = -0.75, color = "red2", hjust = 0.5, fontface = "bold",
             size = 3.5, label = round(by.omb$omb.pct.gop.senate[3.5], 1)) +
    annotate("text", x = 4, y = -0.75, color = "red2", hjust = 0.5, fontface = "bold",
             size = 3.5, label = round(by.omb$omb.pct.gop.senate[4], 1)) +
    annotate("text", x = 5, y = -0.75, color = "red2", hjust = 0.5, fontface = "bold",
             size = 3.5, label = round(by.omb$omb.pct.gop.senate[5], 1)) +
    annotate("text", x = 6, y = -0.75, color = "red2", hjust = 0.5, fontface = "bold",
             size = 3.5, label = round(by.omb$omb.pct.gop.senate[6], 1)) +
    annotate("text", x = 7, y = -0.75, color = "red2", hjust = 0.5, fontface = "bold",
             size = 3.5, label = round(by.omb$omb.pct.gop.senate[7], 1)) +
    annotate("text", x = 8, y = -0.75, color = "red2", hjust = 0.5, fontface = "bold",
             size = 3.5, label = round(by.omb$omb.pct.gop.senate[8], 1)) +
    annotate("text", x = 9, y = -0.75, color = "red2", hjust = 0.5, fontface = "bold",
             size = 3.5, label = round(by.omb$omb.pct.gop.senate[9], 1)) +
    annotate("text", x = 10, y = -0.75, color = "red2", hjust = 0.5, fontface = "bold",
             size = 3.5, label = round(by.omb$omb.pct.gop.senate[10], 1)) +
    ##********************************************************************************
    ##
    ## Legend
    ##
    geom_tile(aes(x = 5.75, y = 18.9, width = 1.25, height = 1.2), fill = "purple4") +
    annotate("text", x = 5.75, y = 19, label = "% Population",
             size = 3, fontface = "bold", color = "white") +
    geom_tile(aes(x = 7, y = 18.9, width = 1.25, height = 1.2), fill = "darkorchid3") +
    annotate("text", x = 7, y = 19, label = "% Electors",
             size = 3, fontface = "bold", color = "white") +
    geom_tile(aes(x = 8.25, y = 18.9, width = 1.25, height = 1.2), fill = "orchid3") +
    annotate("text", x = 8.25, y = 19, label = "% Senators",
             size = 3, fontface = "bold", color = "white") +
    annotate("text", x = 7, y = 17.5, label = "% GOP Senators",
             size = 3.5, fontface = "bold", color = "red2") +
    ##********************************************************************************
    ##
    ## Theme adjustments
    ##
    xlab("") + ylab("") + 
    theme(panel.grid.major.x = element_blank(),
          axis.text.x = element_text(face = "bold", lineheight = 1.1)
          )
