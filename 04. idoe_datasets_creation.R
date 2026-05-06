library(readxl)
library(tidyverse)

# School Directories

direct_counties <- read_xlsx("IDOE_Data/2005 School Directory.xlsx",
          sheet = "Counties")

school_direct_2005 <- read_xlsx("IDOE_Data/2005 School Directory.xlsx",
                                sheet = "Directory") |> 
  select(!(PHONE:PRINC_LNAME)) |> 
  left_join(direct_counties,
            by = join_by(COUNTY == COUNTY_CODE))

corp_direct_2005 <- read_xlsx("IDOE_Data/2005 School Directory.xlsx",
                             sheet = 1) |> 
  left_join(direct_counties,
            by = join_by(COUNTY == COUNTY_CODE))

school_direct_2025 <- read_xlsx("IDOE_Data/2025-2026-school-directory-2025-10-27.xlsx",
                                sheet = "SCHL") 

npschool_direct_2025 <- read_xlsx("IDOE_Data/2025-2026-school-directory-2025-10-27.xlsx",
                                sheet = "NPSCHL") 

all_schools_2025 <- bind_rows(school_direct_2025,
          npschool_direct_2025)

all_schools_2025 |> 
  full_join(school_direct_2005,
            by = join_by(IDOE_SCHOOL_ID == SCHL))

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
                      sheet = "ISTEP+ MATH")

istep_ela_data <- read_xlsx("IDOE_Data/DRF-504 - Hannah Stackpole Grad_ATT_ISTEP 02062026_v1.xlsx",
                             sheet = "ISTEP+ ELA")

# Controls

#wait for now until load in other ethnicity data
#enroll_data <- read_xlsx("IDOE_Data/DRF-504 - Hannah Stackpole Grad_ATT_ISTEP 02062026_v1.xlsx",
#sheet = "ENROLLMENT")