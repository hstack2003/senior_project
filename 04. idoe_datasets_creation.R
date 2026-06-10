library(readxl)
library(tidyverse)
library(dplyr)
source("03. school_finance_dataset_creation.R")

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

#define treatment and control counties
treated_counties <- c("Daviess", "Dubois", "Knox", "Martin", "Pike", "Perry")
control_counties <- c("Sullivan", "Vigo", "Clay", "Greene", "Monroe", 
                      "Lawrence", "Jackson", "Washington", "Orange", 
                      "Crawford", "Harrison", "Owen")

#make istep all dataset with county info

istep_all <- istep_ela_data |> 
  bind_rows(istep_math_data) |> 
  left_join(school_direct_2005,
            by = join_by(IDOE_SCHOOL_ID == SCHL)) |> 
  filter(Proficient != "***") |> 
  mutate(`Proficient %`= as.numeric(`Proficient %`),
         Proficient = as.numeric(Proficient),
         Tested = as.numeric(Tested),
         treated = case_when(COUNTY_NAME %in% treated_counties ~ 1,
                             COUNTY_NAME %in% control_counties ~0))
write_csv(istep_all, "istep_all.csv")

istep_did_data <- istep_all |> 
  filter(SCHOOL_YEAR_ID %in% c("2005", "2006", "2007")) |> 
  mutate(treated = case_when(COUNTY_NAME %in% treated_counties ~ 1,
                             COUNTY_NAME %in% control_counties ~0),
         post_treat = case_when(SCHOOL_YEAR_ID == 2005 ~ 0,
                                SCHOOL_YEAR_ID %in% c("2006", "2007") ~ 1)) |> 
  filter(!is.na(treated))

county_fips_key <- istep_did_data |> 
  distinct(IDOE_SCHOOL_ID, COUNTY_NAME) |> 
  right_join(county_fips_key, join_by(COUNTY_NAME == county_name))

# Ethnicity / FRL

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

# ELL / Special Ed

ell_spec_ed_2006 <- read_xlsx("IDOE_Data/school-enrollment-ell-special-education-2006-25-updated (1).xlsx",
                                sheet = "2006") |> 
  mutate(year = 2006,
         .after = `School Name`)

ell_spec_ed_2007 <- read_xlsx("IDOE_Data/school-enrollment-ell-special-education-2006-25-updated (1).xlsx",
                              sheet = "2007") |> 
  mutate(year = 2007,
         .after = `School Name`)

ell_spec_ed_2008 <- read_xlsx("IDOE_Data/school-enrollment-ell-special-education-2006-25-updated (1).xlsx",
                              sheet = "2008") |> 
  mutate(year = 2008,
         .after = `School Name`)

ell_spec_ed_2009 <- read_xlsx("IDOE_Data/school-enrollment-ell-special-education-2006-25-updated (1).xlsx",
                              sheet = "2009") |> 
  mutate(year = 2009,
         .after = `School Name`)

ell_spec_ed_2010 <- read_xlsx("IDOE_Data/school-enrollment-ell-special-education-2006-25-updated (1).xlsx",
                              sheet = "2010") |> 
  mutate(year = 2010,
         .after = `School Name`)

ell_special_ed_2006_2010 <- bind_rows(ell_spec_ed_2006,
                                      ell_spec_ed_2007,
                                      ell_spec_ed_2008,
                                      ell_spec_ed_2009,
                                      ell_spec_ed_2010)

# Join Ethnicity/FRL data with ELL / Special Ed

eth_frl_ell_special_ed_2006_2010 <- ethnicity_frl_2006_2010 |> 
  left_join(ell_special_ed_2006_2010,
            by = join_by(`Schl ID`, year, `Corp ID`, `Corp Name`)) |> 
  mutate(white_percent = White/`TOTAL ENROLLMENT`,
         free_meals_percent = `Free Meals`/`TOTAL ENROLLMENT`,
         reduced_meals_percent = `Reduced Price Meals`/`TOTAL ENROLLMENT`,
         free_reduced_meal_percent = (`Free Meals`+`Reduced Price Meals`)/`TOTAL ENROLLMENT`)

write_csv(eth_frl_ell_special_ed_2006_2010, "idoe_covariate_data.csv")
# Variable Creation for Heterogeneous Effects Analysis

# Proficiency Rates from 2005 (pre-treatment)
quartile_levels = c("very low", "low", "high", "very high")

prof_levels_2005 <- istep_did_data |> 
  filter(SCHOOL_YEAR_ID == 2005) |> 
  group_by(IDOE_SCHOOL_ID, subject) |> 
  summarise(prof_rate_2005 = sum(Proficient)/sum(Tested),
            .groups = "drop") |> 
  mutate(prof_level = factor(ntile(prof_rate_2005, 4),
                             labels = quartile_levels)) 

# FRL Status and % White from 2006
cov_levels_2006 <- eth_frl_ell_special_ed_2006_2010 |>
  filter(year == 2006) |> 
  mutate(white_level = factor(ntile(white_percent, 4),
                              labels = quartile_levels),
         free_reduced_level = factor(ntile(free_reduced_meal_percent, 4),
                                     labels = quartile_levels),
         free_meal_level = factor(ntile(free_meals_percent, 4),
                                  labels = quartile_levels),
         reduced_meal_level = factor(ntile(reduced_meals_percent, 4),
                                     labels = quartile_levels)) |> 
  select(`Schl ID`, white_level, free_reduced_level, free_meal_level, reduced_meal_level)

# Join new variables 

istep_did_data <- istep_did_data |> 
  left_join(prof_levels_2005, 
            by = join_by(IDOE_SCHOOL_ID, subject)) |> 
  left_join(cov_levels_2006, 
            by = join_by(IDOE_SCHOOL_ID == `Schl ID`))
