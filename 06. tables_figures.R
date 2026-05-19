library(usmap)
library(usmapdata)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(knitr)
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
  mutate(county_name = str_trim(str_extract(county_name,
                                            pattern = ".+\\s")),
         treated = case_when(county_name %in% treated_counites ~ "Treatment",
                             county_name %in% control_counties ~ "Control")) |> 
  filter(treated == "Treatment" | treated == "Control") |> 
  mutate(fips = fips_county,
         fips = paste0("18", fips_county))

map_2006 <- plot_usmap(data = map_data,
           regions = "counties",
           values = "treated",
           include = "IN") +
  labs(title = "2006 Time Zone Change",
       caption = "Treated counties Daviess, Dubois, Knox, Martin, Perry, and Pike move to Central time zone on April 2, 2006 \nwhile surrounding control counties (pink) remain in the Eastern time zone") +
  theme(legend.position = "left",
        plot.caption = element_text(hjust = 0.5))

ggsave("2006_map.png",
       plot = map_2006,
       width = 10,
       height = 8,
       dpi = 300)

# Proficiency Rate Figures
# Define time zone changes
to_central <- ymd("2006-04-01") # April 2006
back_to_eastern <- ymd("2007-11-01")  # November 2007
# Make Plots
proficieny_EMH_year_plot <- istep_all |> 
  mutate(treated = case_when(COUNTY_NAME %in% treated_counites ~ "Treatment",
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
  mutate(percent_proficient = Proficient / Tested,
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
             color = "black") 

ggsave("proficieny_EMH_year.png",
       plot = proficieny_EMH_year_plot,
       width = 10,
       height = 8,
       dpi = 300)


  