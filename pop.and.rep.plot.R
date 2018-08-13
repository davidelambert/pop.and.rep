library(tidyverse)

load("pop.and.rep.Rdata")

## Force division labels onto multiple lines for legibility
levels(by.division$division) <- gsub(" ", "\n", levels(by.division$division))
levels(by.division$division) <- gsub("-", "\n", levels(by.division$division))

ggplot(by.division, aes(x = division)) +
    #################################################################
    ##
    ## Population percentages
    ##
    geom_col(aes(y = div.pct.pop), fill = "dodgerblue4",
             width = 0.28, position = position_nudge(x = -0.28)) +
    geom_text(aes(y = div.pct.pop, label = round(div.pct.pop, 1)),
              fontface = "bold", color = "white", size = 3, angle = 90,
              hjust = 1.5, position = position_nudge(x = -0.32)) +
    #################################################################
    ##
    ## Electoral representation
    ##
    geom_col(aes(y = div.pct.electors), fill = "royalblue3", 
             width = 0.28, position = position_nudge(x = 0)) +
    geom_text(aes(y = div.pct.electors, label = round(div.pct.electors, 1)),
              fontface = "bold", color = "white", size = 3, angle = 90,
              hjust = 1.5, position = position_nudge(x = -0.03)) +
    #################################################################
    ##
    ## Senate representation
    ##
    geom_col(aes(y = div.prop.senate), fill = "steelblue3", 
             width = 0.28, position = position_nudge(x = .28)) +
    geom_text(aes(y = div.prop.senate, label = div.prop.senate),
              fontface = "bold", color = "white", size = 3, angle = 90,
              hjust = 2, position = position_nudge(x = 0.24)) +
    #################################################################
    ##
    ## Percent GOP labels
    ##
    annotate("text", x = 1, y = -0.75, color = "red2", hjust = 0.5, fontface = "bold",
             size = 3, label = round(by.division$div.pct.gop[1], 1)) +
    annotate("text", x = 2, y = -0.75, color = "red2", hjust = 0.5, fontface = "bold",
             size = 3, label = round(by.division$div.pct.gop[2], 1)) +
    annotate("text", x = 3, y = -0.75, color = "red2", hjust = 0.5, fontface = "bold",
             size = 3, label = round(by.division$div.pct.gop[3], 1)) +
    annotate("text", x = 4, y = -0.75, color = "red2", hjust = 0.5, fontface = "bold",
             size = 3, label = round(by.division$div.pct.gop[4], 1)) +
    annotate("text", x = 5, y = -0.75, color = "red2", hjust = 0.5, fontface = "bold",
             size = 3, label = round(by.division$div.pct.gop[5], 1)) +
    annotate("text", x = 6, y = -0.75, color = "red2", hjust = 0.5, fontface = "bold",
             size = 3, label = round(by.division$div.pct.gop[6], 1)) +
    annotate("text", x = 7, y = -0.75, color = "red2", hjust = 0.5, fontface = "bold",
             size = 3, label = round(by.division$div.pct.gop[7], 1)) +
    annotate("text", x = 8, y = -0.75, color = "red2", hjust = 0.5, fontface = "bold",
             size = 3, label = round(by.division$div.pct.gop[8], 1)) +
    annotate("text", x = 9, y = -0.75, color = "red2", hjust = 0.5, fontface = "bold",
             size = 3, label = round(by.division$div.pct.gop[9], 1)) +
    #################################################################
    ##
    ## Legend
    ##
    annotate("label", x = 1.25, y = 18.5, fill = "dodgerblue4", label = "% Population", 
             size = 3, fontface = "bold", color = "white",
             label.padding = unit(0.4, "lines"), label.r = unit(0, "lines")) +
    annotate("label", x = 2.5, y = 18.5, fill = "royalblue3", label = "% Electors",
             size = 3, fontface = "bold", color = "white",
             label.padding = unit(0.4, "lines"), label.r = unit(0, "lines")) +
    annotate("label", x = 3.7, y = 18.5, fill = "steelblue3", label = "% Senators",
             size = 3, fontface = "bold", color = "white",
             label.padding = unit(0.4, "lines"), label.r = unit(0, "lines")) +
    annotate("text", x = 2.5, y = 17, label = "% GOP Senators",
             size = 3.5, fontface = "bold", color = "red2") +
    #################################################################
    ##
    ## Theme adjustments
    ##
    xlab("") + ylab("") + 
    theme(panel.grid.major.x = element_blank())
    
    
