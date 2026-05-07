library(readxl)
library(tidyverse)
library(tidyr)

population_data_2000_2010 <- read_xlsx("Census_Data_Population/intercensal00s.xlsx",
          skip = 2,
          col_names = c("County",
                        "2000_census",
                        "2000",
                        "2001",
                        "2002",
                        "2003",
                        "2004",
                        "2005",
                        "2006",
                        "2007",
                        "2008",
                        "2009",
                        "2010_census",
                        "2010")) |> 
  filter(if_all(everything(), ~ !is.na(.))) |> 
  pivot_longer(cols = `2000_census`:`2010`,
               names_to = "year",
               values_to = "population")
