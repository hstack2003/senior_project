library(tidyverse)
library(writexl)

# pattern to read in and name same columns from each year of data
spec <- fwf_positions(
  start = c(4, 35, 134, 194),
  end   = c(6, 38, 139, 238),
  col_names = c(
    "fips_county",
    "percent_pov",
    "mhi",
    "county_name"
  )
)

# creating a dataset for each year, adding the year to have long format when put together
# US Census Bureau Small Area Income and Poverty Estimates
# source: https://www.census.gov/programs-surveys/saipe/data/datasets.html

saipe_2000 <- read_fwf(
  file = "https://www2.census.gov/programs-surveys/saipe/datasets/2000/2000-state-and-county/est00-in.dat",
  col_positions = spec,
  trim_ws = TRUE,
  col_types = cols(
    fips_county = col_character(),
    percent_pov = col_double(),
    mhi = col_double(),
    county_name = col_character())) |> 
  mutate(year = 2000)

saipe_2001 <- read_fwf(
  file = "https://www2.census.gov/programs-surveys/saipe/datasets/2001/2001-state-and-county/est01-in.dat",
  col_positions = spec,
  trim_ws = TRUE,
  col_types = cols(
    fips_county = col_character(),
    percent_pov = col_double(),
    mhi = col_double(),
    county_name = col_character())) |> 
  mutate(year = 2001)

saipe_2002 <- read_fwf(
  file = "https://www2.census.gov/programs-surveys/saipe/datasets/2002/2002-state-and-county/est02-in.dat",
  col_positions = spec,
  trim_ws = TRUE,
  col_types = cols(
    fips_county = col_character(),
    percent_pov = col_double(),
    mhi = col_double(),
    county_name = col_character())) |> 
  mutate(year = 2002)

saipe_2003 <- read_fwf(
  file = "https://www2.census.gov/programs-surveys/saipe/datasets/2003/2003-state-and-county/est03-in.dat",
  col_positions = spec,
  trim_ws = TRUE,
  col_types = cols(
    fips_county = col_character(),
    percent_pov = col_double(),
    mhi = col_double(),
    county_name = col_character())) |> 
  mutate(year = 2003)

saipe_2004 <- read_fwf(
  file = "https://www2.census.gov/programs-surveys/saipe/datasets/2004/2004-state-and-county/est04-in.txt",
  col_positions = spec,
  trim_ws = TRUE,
  col_types = cols(
    fips_county = col_character(),
    percent_pov = col_double(),
    mhi = col_double(),
    county_name = col_character())) |> 
  mutate(year = 2004)

saipe_2005 <- read_fwf(
  file = "https://www2.census.gov/programs-surveys/saipe/datasets/2005/2005-state-and-county/est05-in.txt",
  col_positions = spec,
  trim_ws = TRUE,
  col_types = cols(
    fips_county = col_character(),
    percent_pov = col_double(),
    mhi = col_double(),
    county_name = col_character())) |> 
  mutate(year = 2005)

saipe_2006 <- read_fwf(
  file = "https://www2.census.gov/programs-surveys/saipe/datasets/2006/2006-state-and-county/est06-in.txt",
  col_positions = spec,
  trim_ws = TRUE,
  col_types = cols(
    fips_county = col_character(),
    percent_pov = col_double(),
    mhi = col_double(),
    county_name = col_character())) |> 
  mutate(year = 2006)

saipe_2007 <- read_fwf(
  file = "https://www2.census.gov/programs-surveys/saipe/datasets/2007/2007-state-and-county/est07-in.txt",
  col_positions = spec,
  trim_ws = TRUE,
  col_types = cols(
    fips_county = col_character(),
    percent_pov = col_double(),
    mhi = col_double(),
    county_name = col_character())) |> 
  mutate(year = 2007)

saipe_2008 <- read_fwf(
  file = "https://www2.census.gov/programs-surveys/saipe/datasets/2008/2008-state-and-county/est08-in.txt",
  col_positions = spec,
  trim_ws = TRUE,
  col_types = cols(
    fips_county = col_character(),
    percent_pov = col_double(),
    mhi = col_double(),
    county_name = col_character())) |> 
  mutate(year = 2008)

saipe_2009 <- read_fwf(
  file = "https://www2.census.gov/programs-surveys/saipe/datasets/2009/2009-state-and-county/est09-in.txt",
  col_positions = spec,
  trim_ws = TRUE,
  col_types = cols(
    fips_county = col_character(),
    percent_pov = col_double(),
    mhi = col_double(),
    county_name = col_character())) |> 
  mutate(year = 2009)

saipe_2010 <- read_fwf(
  file = "https://www2.census.gov/programs-surveys/saipe/datasets/2010/2010-state-and-county/est10-in.txt",
  col_positions = spec,
  trim_ws = TRUE,
  col_types = cols(
    fips_county = col_character(),
    percent_pov = col_double(),
    mhi = col_double(),
    county_name = col_character())) |> 
  mutate(year = 2010)

# putting the 2000-2010 datasets together
saipe_2000_2010 <- bind_rows(saipe_2000,
                             saipe_2001,
                             saipe_2002,
                             saipe_2003,
                             saipe_2004,
                             saipe_2005,
                             saipe_2006,
                             saipe_2007,
                             saipe_2008,
                             saipe_2009,
                             saipe_2009,
                             saipe_2010)

# save as an excel file
write_xlsx(saipe_2000_2010, "saipe_2000_2010.xlsx")
