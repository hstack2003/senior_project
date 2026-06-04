library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

# import data
grad_data <- read_excel("DRF-504 - Hannah Stackpole Grad_ATT_ISTEP 02062026_v1.xlsx", sheet = 1)
att_data <- read_excel("DRF-504 - Hannah Stackpole Grad_ATT_ISTEP 02062026_v1.xlsx", sheet = 2)
enroll_data <- read_excel("DRF-504 - Hannah Stackpole Grad_ATT_ISTEP 02062026_v1.xlsx", sheet = 3)
istep_math <- read_excel("DRF-504 - Hannah Stackpole Grad_ATT_ISTEP 02062026_v1.xlsx", sheet = 4)
istep_ela <- read_excel("DRF-504 - Hannah Stackpole Grad_ATT_ISTEP 02062026_v1.xlsx", sheet = 5)

corp_directory <- read_excel("2025-2026-school-directory-2025-10-27.xlsx", sheet = "CORP")
pub_directory <- read_excel("2025-2026-school-directory-2025-10-27.xlsx", sheet = "SCHL")
npub_directory <- read_excel("2025-2026-school-directory-2025-10-27.xlsx", sheet = "NPSCHL")

# rename variables
grad_data <- rename(grad_data, "grad_rate" = "Graduate Rate")
att_data <- rename (att_data, "att_rate" = "AT_RATE")
istep_math <- istep_math |> 
  rename("tested_math" = "Tested", "proficient_math" = "Proficient", "percent_proficient_math" = "Proficient %")
istep_ela <- istep_ela |> 
  rename("tested_ela" = "Tested", "proficient_ela" = "Proficient", "percent_proficient_ela" = "Proficient %")

# bind directories
pub_to_bind <- pub_directory |> subset(select = -c(NCES_ID))
npub_to_bind <- npub_directory |> subset(select = -c(CHOICE_FLAG))

npub_to_bind$IDOE_CORPORATION_ID = c("")
npub_to_bind$CORPORATION_NAME = c("Independent")
npub_to_bind$LOCALE= c("")

school_directory_all <- rbind(pub_to_bind, npub_to_bind)

# merge ISTEP+ data
istep_all <- full_join(
  istep_ela,
  istep_math |> select(tested_math, proficient_math, percent_proficient_math, SCHOOL_YEAR_ID, IDOE_SCHOOL_ID, GRADE_CODE),
  by = c("SCHOOL_YEAR_ID", "IDOE_SCHOOL_ID", "GRADE_CODE")
)

# designate treatment vs control counties
treatment_counties_2006 <- c("Knox", "Daviess", "Pike", "Dubois", "Martin", "Perry")
control_counties_2006 <- c("Sullivan", "Vigo", "Clay", "Owen", "Greene", "Monroe", "Lawrence", "Jackson", "Orange", "Crawford", "Harrison", "Washington")

treatment_counties_2007 <- c("Knox", "Daviess", "Pike", "Dubois", "Martin")
control_counties_2007 <- c("Posey", "Gibson", "Vanderburgh", "Warrick", "Spencer") #maybe include Perry here

treatment_counties_both <- c("Knox", "Daviess", "Pike", "Dubois", "Martin")
control_counties_both <- c("Sullivan", "Vigo", "Clay", "Owen", "Greene", "Monroe", "Lawrence", "Jackson", "Orange", "Crawford", "Harrison", "Washington")

