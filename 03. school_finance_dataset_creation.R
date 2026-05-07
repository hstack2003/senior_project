library(readxl)
library(tidyverse)
library(writexl)

# import Annual Survey of School System Finances (US Census Bureau) data 
# source: https://www.census.gov/programs-surveys/school-finances/data/tables.html

sch_finance_2000 <- read_xls("Census_Data_School_Finances/elsec00t.xls") |>
  filter(str_starts(FIPS, "18")) |> 
  mutate(CONUM = str_sub(FIPS, 
                         start = 1, 
                         end = 5), 
         .before = NAME) |> 
  select(CONUM:PPSSCHAD) |> 
  mutate(across(ENROLL:PPSSCHAD, as.numeric),
         year = 2000,
         .after = NAME)

sch_finance_2001 <- read_xls("Census_Data_School_Finances/elsec01t.xls") |>
  filter(str_starts(FIPS, "18")) |> 
  mutate(CONUM = str_sub(FIPS, 
                         start = 1, 
                         end = 5), 
         .before = NAME) |> 
  select(CONUM:PPSSCHAD) |> 
  mutate(across(ENROLL:PPSSCHAD, as.numeric),
         year = 2001,
         .after = NAME)

sch_finance_2002 <- read_xls("Census_Data_School_Finances/elsec02t.xls") |>
  filter(str_starts(FIPS, "18")) |> 
  mutate(CONUM = str_sub(FIPS, 
                         start = 1, 
                         end = 5), 
         .before = NAME) |> 
  select(CONUM:PPSSCHAD) |> 
  mutate(across(ENROLL:PPSSCHAD, as.numeric),
         year = 2002,
         .after = NAME)

sch_finance_2003 <- read_xls("Census_Data_School_Finances/elsec03t.xls") |>
  filter(str_starts(CONUM, "18")) |> 
  select(CONUM,NAME:PPSSCHAD) |> 
  mutate(across(ENROLL:PPSSCHAD, as.numeric),
         year = 2003,
         .after = NAME)

sch_finance_2004 <- read_xls("Census_Data_School_Finances/elsec04t.xls") |>
  filter(str_starts(CONUM, "18")) |> 
  select(CONUM,NAME:PPSSCHAD) |> 
  mutate(across(ENROLL:PPSSCHAD, as.numeric),
         year = 2004,
         .after = NAME)

sch_finance_2005 <- read_xls("Census_Data_School_Finances/elsec05t.xls") |>
  filter(str_starts(CONUM, "18")) |> 
  select(CONUM,NAME:PPSSCHAD) |> 
  mutate(across(ENROLL:PPSSCHAD, as.numeric),
         year = 2005,
         .after = NAME)

sch_finance_2006 <- read_xls("Census_Data_School_Finances/elsec06t.xls") |>
  filter(str_starts(CONUM, "18")) |> 
  select(CONUM,NAME:PPSSCHAD) |> 
  mutate(across(ENROLL:PPSSCHAD, as.numeric),
         year = 2006,
         .after = NAME)

sch_finance_2007 <- read_xls("Census_Data_School_Finances/elsec07t.xls") |>
  filter(str_starts(CONUM, "18")) |> 
  select(CONUM,NAME:PPSSCHAD) |> 
  mutate(across(ENROLL:PPSSCHAD, as.numeric),
         year = 2007,
         .after = NAME)

sch_finance_2008 <- read_xls("Census_Data_School_Finances/elsec08t.xls") |>
  filter(str_starts(CONUM, "18")) |> 
  select(CONUM,NAME:PPSSCHAD) |> 
  mutate(across(ENROLL:PPSSCHAD, as.numeric),
         year = 2008,
         .after = NAME)

sch_finance_2009 <- read_xls("Census_Data_School_Finances/elsec09t.xls") |>
  filter(str_starts(CONUM, "18")) |> 
  select(CONUM,NAME:PPSSCHAD) |> 
  mutate(across(ENROLL:PPSSCHAD, as.numeric),
         year = 2009,
         .after = NAME)

sch_finance_2010 <- read_xls("Census_Data_School_Finances/elsec10t.xls") |>
  filter(str_starts(CONUM, "18")) |> 
  select(CONUM,NAME:PPSSCHAD) |> 
  mutate(across(ENROLL:PPSSCHAD, as.numeric),
         year = 2010,
         .after = NAME)


# combine into one dataframe
sch_finance_2000_2010 <- bind_rows(sch_finance_2000,
                                   sch_finance_2001,
                                   sch_finance_2002,
                                   sch_finance_2003,
                                   sch_finance_2004,
                                   sch_finance_2005,
                                   sch_finance_2006,
                                   sch_finance_2007,
                                   sch_finance_2008,
                                   sch_finance_2009,
                                   sch_finance_2010) |> 
  rename(fips_county = CONUM) |> 
  mutate(fips_county = str_remove(fips_county,
                                  pattern = "18"))

# save as excel file
write_xlsx(sch_finance_2000_2010, "sch_finance_2000_2010.xlsx")
