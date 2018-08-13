library(tidyverse)
library(png)
library(grid)

load("pop.and.rep.Rda")

## Force division labels onto multiple lines for legibility
levels(by.division$division) <- gsub(" ", "\n", levels(by.division$division))
levels(by.division$division) <- gsub("-", "\n", levels(by.division$division))

## PNG import for placement
image <- readPNG("CensusDivisions.png")
map <- rasterGrob(image, interpolate = TRUE)


ggplot(by.division, aes(x = division)) +
    #################################################################
    ##
    ## Population percentages
    ##
    geom_col(aes(y = div.pct.pop), fill = "dodgerblue4",
             width = 0.28, position = position_nudge(x = -0.28)) +
    geom_text(aes(y = div.pct.pop, label = round(div.pct.pop, 1)),
              fontface = "bold", color = "white", size = 3, angle = 90,
              position = position_nudge(x = -0.32, y = -1)) +
    #################################################################
    ##
    ## Electoral representation
    ##
    geom_col(aes(y = div.pct.electors), fill = "royalblue3", 
             width = 0.28, position = position_nudge(x = 0)) +
    geom_text(aes(y = div.pct.electors, label = round(div.pct.electors, 1)),
              fontface = "bold", color = "white", size = 3, angle = 90,
              position = position_nudge(x = -0.03, y = -1)) +
    #################################################################
    ##
    ## Senate representation
    ##
    geom_col(aes(y = div.prop.senate), fill = "steelblue3", 
             width = 0.28, position = position_nudge(x = .28)) +
    geom_text(aes(y = div.prop.senate, label = div.prop.senate),
              fontface = "bold", color = "white", size = 3, angle = 90,
              position = position_nudge(x = 0.24, y = -1)) +
    #################################################################
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
    #################################################################
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
    #################################################################
    ##
    ## Map insert
    ##
    annotation_custom(map, xmin = 6, xmax = 8, ymin = 14.3, ymax = 19.8) +
    #################################################################
    ##
    ## Theme adjustments
    ##
    xlab("") + ylab("") + 
    theme(panel.grid.major.x = element_blank(),
          axis.text.x = element_text(face = "bold", lineheight = 1.1)
          )
