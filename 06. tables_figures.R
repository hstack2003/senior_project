library(usmap)
library(usmapdata)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(knitr)
library(modelsummary)
library(estimatr)
library(gt)
source("05. population_dataset_creation.R")

# Treatment vs Control Map
map_data <- saipe_2000_2010 |> 
  mutate(fips_county = str_pad(fips_county,
                               width = 3,
                               side = c("left"),
                               pad = "0"),
         year = as.numeric(year)) |> 
  left_join(lf_data_2000_2010, 
            by = join_by(fips_county, year)) |> 
  mutate(County = str_trim(str_extract(County.x,
                                            pattern = ".+\\s")),
         treated = case_when(County.x %in% treated_counties ~ "Treatment",
                             County.x %in% control_counties ~ "Control",
                             .default = "Not in study"),
         treated = fct_relevel(treated, 
                               c("Treatment", "Control", "Not in study"))) |> 
  mutate(fips = fips_county,
         fips = paste0("18", fips_county))

map_2006 <- plot_usmap(data = map_data,
           regions = "counties",
           values = "treated",
           include = "IN") +
  scale_fill_manual(values = c("Treatment" = "pink1",
                               "Control" = "yellow4",
                               "Not in study" = "gray")) +
  theme(legend.position = "left",
        plot.caption = element_text(hjust = 0.5)) +
  labs(fill = "County Designation")

# Proficiency Rate Figures
# Define time zone changes
to_central <- ymd("2006-04-01") # April 2006
back_to_eastern <- ymd("2007-11-01")  # November 2007
# Make Plots
# By age group (EMH)
proficiency_EMH_year_plot <-
istep_all |> 
  mutate(treated = case_when(COUNTY_NAME %in% treated_counties ~ "Treatment",
                             COUNTY_NAME %in% control_counties ~ "Control"), 
         age_range = fct(case_when(GRADE_CODE %in% c("03", "04", "05") ~ "Elementary",
                                   GRADE_CODE %in% c("06", "07", "08") ~ "Middle",
                                   GRADE_CODE %in% c("09", "10") ~ "High School")),
         age_range = fct_relevel(age_range,
                                 c("Elementary", "Middle", "High School"))) |> 
  filter(treated == "Treatment" | treated == "Control") |> 
  group_by(treated, subject, SCHOOL_YEAR_ID, age_range) |> # edit age range vs grade here
  summarise(Tested = sum(Tested),
            Proficient = sum(Proficient),
            .groups = "drop") |> 
  mutate(percent_proficient = (Proficient / Tested)*100,
         year_date = as.Date(paste0(SCHOOL_YEAR_ID, "-09-01"))) |>
  ggplot(mapping = aes(x = year_date,
                       y = percent_proficient, 
                       color = treated)) +
  geom_point() + 
  geom_line() +
  facet_grid(cols = vars(subject),
             rows = vars(age_range)) + # chose age range or grade again here
  geom_vline(xintercept = to_central,
             linetype = "dashed",
             color = "black") +
  geom_vline(xintercept = back_to_eastern,
             linetype = "dashed",
             color = "black") + 
  scale_color_manual(values = c("Treatment" = "pink1",
                               "Control" = "yellow4")) +
  labs(subtitle = "Percent Proficient",
       y = NULL,
       x = "Year",
       color = "Group")
  

ggsave("proficiency_EMH_year.png",
       plot = proficiency_EMH_year_plot,
       width = 10,
       height = 8,
       dpi = 300)

# By grade (3-10)
proficiency_grade_year_plot <- 
istep_all |> 
  mutate(treated = case_when(COUNTY_NAME %in% treated_counties ~ "Treatment",
                             COUNTY_NAME %in% control_counties ~ "Control")) |> 
  filter(treated == "Treatment" | treated == "Control") |> 
  group_by(treated, subject, SCHOOL_YEAR_ID, GRADE_CODE) |> # edit age range vs grade here
  summarise(Tested = sum(Tested),
            Proficient = sum(Proficient),
            .groups = "drop") |> 
  mutate(percent_proficient = (Proficient / Tested)*100,
         year_date = as.Date(paste0(SCHOOL_YEAR_ID, "-09-01"))) |>
  ggplot(mapping = aes(x = year_date,
                       y = percent_proficient, 
                       color = treated)) +
  geom_point() + 
  geom_line() +
  facet_grid(cols = vars(subject),
             rows = vars(GRADE_CODE)) + # chose age range or grade again here
  geom_vline(xintercept = to_central,
             linetype = "dashed",
             color = "black") +
  geom_vline(xintercept = back_to_eastern,
             linetype = "dashed",
             color = "black") + 
  labs(title = "Subject Proficiency Rates by Grade", # edit grade vs EMH
       subtitle = "Percent Proficient",
       y = NULL,
       x = "Year",
       caption = "placeholder",
       color = "Group")

ggsave("proficiency_grade_year.png",
       plot = proficiency_grade_year_plot,
       width = 10,
       height = 8,
       dpi = 300)

# Summary Stats
school_bal_data <- eth_frl_ell_special_ed_2006_2010 |> 
  left_join(county_fips_key, join_by(`Schl ID` == IDOE_SCHOOL_ID)) |> 
  mutate(treat = case_when(COUNTY_NAME %in% treated_counties ~ 1,
                           COUNTY_NAME %in% control_counties ~ 0),
         across(c(white_percent, free_reduced_meal_percent, free_meals_percent, `ELL %`,`Special Education %`),
                ~ .x * 100)) |> 
  filter(treat == 0 | treat ==1) 

school_tab <- datasummary_balance(data = school_bal_data, 
                                  formula = white_percent + free_reduced_meal_percent + free_meals_percent + `ELL %` + `Special Education %`~ treat,
                                  output = "data.frame")

district_bal_data <- sch_finance_2000_2010 |> 
  mutate(treat = case_when(county_name.x %in% treated_counties ~ 1,
                           county_name.x %in% control_counties ~ 0)) |> 
  filter(treat == 0 | treat ==1,
         year < 2006) 

district_tab <- datasummary_balance(data = district_bal_data, 
                                    formula = ENROLL + TOTALREV + TOTALEXP + TCURINST + PPCSTOT + PPITOTAL ~ treat,
                                    output = "data.frame",
                                    fmt = fmt_decimal(digits = 0, pdigits = 3))

county_bal_data <- saipe_2000_2010 |> 
  left_join(lf_data_2000_2010, 
            by = join_by(County, year)) |> 
  left_join(population_data_2000_2010,
            join_by(County, year)) |> 
  mutate(treat = case_when(County %in% treated_counties ~ 1,
                           County %in% control_counties ~ 0),
         weights = population,
         percent_pov = percent_pov*100) |> 
  filter(treat == 0 | treat ==1,
         year == 2005)

county_tab <- datasummary_balance(data = county_bal_data, 
                                  formula = mhi + percent_pov + `Unemployment Rate (%)` ~ treat,
                                  output = "data.frame")

school_tab <- school_tab |> 
  mutate(panel = "Panel A: School Characteristics")

district_tab <- district_tab |> 
  mutate(panel = "Panel B: District Characteristics")

county_tab <- county_tab |> 
  mutate(panel = "Panel C: County Characteristics")

all_tabs <- bind_rows(school_tab, district_tab, county_tab) |> 
  mutate(
      ` ` = recode(
      ` `,
      white_percent = "White (%)",
      free_reduced_meal_percent = "Free/Reduced Lunch (%)",
      free_meals_percent = "Free Lunch (%)",
      `ELL %` = "English Language Learners (%)",
      `Special Education %` = "Special Education (%)",
      ENROLL = "Enrollment",
      TOTALREV = "Total Revenue",
      TOTALEXP = "Total Expenditures",
      TCURINST = "Total Instructional Spending",
      PPCSTOT = "Per-Pupil Current Spending",
      PPITOTAL = "Per-Pupil Instructional Spending",
      mhi = "Median Household Income",
      percent_pov = "Poverty Rate (%)",
      `Unemployment Rate (%)` = "Unemployment Rate (%)"
    )
  ) |> 
  rename("Statistic" = ` `)

summary_stats <- all_tabs |>
  gt(groupname_col = "panel") |> 
  tab_spanner(label = "Control",
              columns = c("0 / Mean", "0 / Std. Dev.")) |> 
  tab_spanner(label = "Treatment",
              columns = c("1 / Mean", "1 / Std. Dev.")) |> 
  cols_label(`0 / Mean` = "Mean",
             `0 / Std. Dev.` = "Std. Dev.",
             `1 / Mean` = "Mean", 
             `1 / Std. Dev.` = "Std. Dev.",
             `Diff. in Means` = "Diff.")

summary_stats

# School Finance over Time
sch_finance_fig <- sch_finance_2000_2010 |> 
  mutate(treated = case_when(county_name.x %in% treated_counties ~ "Treatment",
                             county_name.x %in% control_counties ~ "Control"),
         year_date = as.Date(paste0(year, "-09-01"))) |> 
  filter(treated == "Treatment" | treated == "Control") |> 
  group_by(treated, year_date) |> 
  summarise(mean_ppi = mean(PPITOTAL),
            mean_ppcs = mean(PPCSTOT),
            mean_enroll = mean(ENROLL),
            mean_total_rev = mean(TOTALREV),
            mean_total_exp = mean(TOTALEXP),
            .groups = "drop") |> 
  pivot_longer(cols = -c(treated, year_date),
               names_to = "measure",
               values_to = "value") |> 
  ggplot(mapping = aes(x = year_date,
                       y = value,
                       color = factor(treated))) +
  geom_line() +
  geom_point() +
  facet_wrap(~ measure, scales = "free_y",
             ncol = 1) +
  scale_color_manual(
    values = c("Treatment" = "pink1",
               "Control" = "yellow4")) +
  geom_vline(xintercept = to_central,
             linetype = "dashed",
             color = "black") +
  geom_vline(xintercept = back_to_eastern,
             linetype = "dashed",
             color = "black") +
  labs(color = "Group",
       x = "Year",
       y = NULL,
       title = "School Enrollment and Revenue/Expenditures over Time",
       subtitle = "All measures in dollars ($) except for enrollment")

  