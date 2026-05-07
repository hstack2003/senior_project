library(readxl)
library(tidyverse)
library(writexl)

# import annual BLS Local Area Unemployment data
# source: https://www.bls.gov/lau/tables.htm#cntyaa

lf_data_2000 <- read_xlsx("BLS_labor_unemployment_data/laucnty00.xlsx", skip = 1)
lf_data_2001 <- read_xlsx("BLS_labor_unemployment_data/laucnty01.xlsx", skip = 1)
lf_data_2002 <- read_xlsx("BLS_labor_unemployment_data/laucnty02.xlsx", skip = 1)
lf_data_2003 <- read_xlsx("BLS_labor_unemployment_data/laucnty03.xlsx", skip = 1)
lf_data_2004 <- read_xlsx("BLS_labor_unemployment_data/laucnty04.xlsx", skip = 1)
lf_data_2005 <- read_xlsx("BLS_labor_unemployment_data/laucnty05.xlsx", skip = 1)
lf_data_2006 <- read_xlsx("BLS_labor_unemployment_data/laucnty06.xlsx", skip = 1)
lf_data_2007 <- read_xlsx("BLS_labor_unemployment_data/laucnty07.xlsx", skip = 1)
lf_data_2008 <- read_xlsx("BLS_labor_unemployment_data/laucnty08.xlsx", skip = 1)
lf_data_2009 <- read_xlsx("BLS_labor_unemployment_data/laucnty09.xlsx", skip = 1)
lf_data_2010 <- read_xlsx("BLS_labor_unemployment_data/laucnty10.xlsx", skip = 1)

# compile to one data frame and keep only Indiana data

lf_data_2000_2010 <- bind_rows(lf_data_2000,
                               lf_data_2001,
                               lf_data_2002,
                               lf_data_2003,
                               lf_data_2004,
                               lf_data_2005,
                               lf_data_2006,
                               lf_data_2007,
                               lf_data_2008,
                               lf_data_2009,
                               lf_data_2010) |> 
  filter(`State FIPS Code` == 18) |> 
  rename(fips_county = `County FIPS Code`,
         year = Year) |> 
  mutate(year = as.numeric(year))

write_xlsx(lf_data_2000_2010, "lf_data_2000_2010.xlsx")
