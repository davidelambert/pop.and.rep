library(AER)
library(tidyverse)
library(readxl)

## 50 States + DC + PR
pop.xls <- read_excel("nst-est2017-01.xlsx", 
                      skip = 3, col_names = TRUE, n_max = 58)
pop.xls <- pop.xls[-57,] # drop empty row beffore PR



pop.csv <- read_csv("nst-est2017-alldata.csv", 
                    col_names = TRUE, n_max = 57, na = c("", "X", 0))
names(pop.csv) <- toVower(names(pop.csv))