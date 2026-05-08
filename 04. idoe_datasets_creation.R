library(readxl)
library(tidyverse)
library(dplyr)

# School Directories

direct_counties <- read_xlsx("IDOE_Data/2005 School Directory.xlsx",
          sheet = "Counties")

school_direct_2005 <- read_xlsx("IDOE_Data/2005 School Directory.xlsx",
                                sheet = "Directory") |> 
  left_join(direct_counties,
            by = join_by(COUNTY == COUNTY_CODE)) |> 
  select(SCHL, CORP, NAME, COUNTY_NAME) |> 
  distinct(SCHL, 
           .keep_all = TRUE)

corp_direct_2005 <- read_xlsx("IDOE_Data/2005 School Directory.xlsx",
                             sheet = 1) |> 
  left_join(direct_counties,
            by = join_by(COUNTY == COUNTY_CODE))

school_direct_2025 <- read_xlsx("IDOE_Data/2025-2026-school-directory-2025-10-27.xlsx",
                                sheet = "SCHL") 

npschool_direct_2025 <- read_xlsx("IDOE_Data/2025-2026-school-directory-2025-10-27.xlsx",
                                sheet = "NPSCHL") 

all_school_directory <- bind_rows(school_direct_2025,
          npschool_direct_2025) |> 
  full_join(school_direct_2005,
            by = join_by(IDOE_SCHOOL_ID == SCHL))

# check which schools are not in istep data but not 2005-2007 or 2025 directories
istep_math_data |> 
  anti_join(school_direct_2005,
            by = join_by(IDOE_SCHOOL_ID == SCHL)) |> 
  anti_join(all_schools_2025,
            by = "IDOE_SCHOOL_ID") |> 
  anti_join(corp_direct_2005,
            by = join_by(IDOE_CORPORATION_ID == CORP)) |> 
  group_by(SCHOOL_YEAR_ID) |> 
  summarise(count = n())

# Test / Outcome Data

grad_data <- read_xlsx("IDOE_Data/DRF-504 - Hannah Stackpole Grad_ATT_ISTEP 02062026_v1.xlsx",
                       sheet = "GRAD RATE")

att_data <- read_xlsx("IDOE_Data/DRF-504 - Hannah Stackpole Grad_ATT_ISTEP 02062026_v1.xlsx",
                                   sheet = "ATT RATE")

att_data <- read_xlsx("IDOE_Data/DRF-504 - Hannah Stackpole Grad_ATT_ISTEP 02062026_v1.xlsx",
                      sheet = "ATT RATE")

istep_math_data <- read_xlsx("IDOE_Data/DRF-504 - Hannah Stackpole Grad_ATT_ISTEP 02062026_v1.xlsx",
                      sheet = "ISTEP+ MATH") |> 
  mutate(subject = "Math")

istep_ela_data <- read_xlsx("IDOE_Data/DRF-504 - Hannah Stackpole Grad_ATT_ISTEP 02062026_v1.xlsx",
                             sheet = "ISTEP+ ELA") |> 
  mutate(subject = "ELA")

#make istep all dataset with county info

istep_all <- istep_ela_data |> 
  bind_rows(istep_math_data) |> 
  left_join(school_direct_2005,
            by = join_by(IDOE_SCHOOL_ID == SCHL)) |> 
  filter_out(Proficient == "***") |> 
  mutate(Proficient = as.numeric(Proficient))


# Controls

#wait for now until load in other ethnicity data
#enroll_data <- read_xlsx("IDOE_Data/DRF-504 - Hannah Stackpole Grad_ATT_ISTEP 02062026_v1.xlsx",
#sheet = "ENROLLMENT")

ethnicity_FRL_2006 <- read_xlsx("IDOE_Data/school-enrollment-ethnicity-and-free-reduced-price-meal-status-2006-26-final.xlsx",
          sheet = "2006") |>
  mutate(year = 2006,
         .after = `Schl Name`)

ethnicity_FRL_2007 <- read_xlsx("IDOE_Data/school-enrollment-ethnicity-and-free-reduced-price-meal-status-2006-26-final.xlsx",
                                sheet = "2007") |>
  mutate(year = 2007,
         .after = `Schl Name`)

ethnicity_FRL_2008 <- read_xlsx("IDOE_Data/school-enrollment-ethnicity-and-free-reduced-price-meal-status-2006-26-final.xlsx",
                                sheet = "2008") |>
  mutate(year = 2008,
         .after = `Schl Name`)

ethnicity_FRL_2009 <- read_xlsx("IDOE_Data/school-enrollment-ethnicity-and-free-reduced-price-meal-status-2006-26-final.xlsx",
                                sheet = "2009") |>
  mutate(year = 2009,
         .after = `Schl Name`)          

ethnicity_FRL_2010 <- read_xlsx("IDOE_Data/school-enrollment-ethnicity-and-free-reduced-price-meal-status-2006-26-final.xlsx",
                                sheet = "2010") |>
  mutate(year = 2010,
         .after = `Schl Name`)

ethnicity_frl_2006_2010 <- bind_rows(ethnicity_FRL_2006,
                                     ethnicity_FRL_2007,
                                     ethnicity_FRL_2008,
                                     ethnicity_FRL_2009,
                                     ethnicity_FRL_2010)
