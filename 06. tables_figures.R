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


  