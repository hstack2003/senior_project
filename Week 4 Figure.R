library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

# load data 
ilearn_school_2025 <- read_excel("ILEARN-2025-Grade3-8-Final-School_edited_v1.xlsx", sheet = "ELA")
View(ilearn_school_2025)

# categorize counties as treatment and control (treatment = central, control = eastern)
treatment_counties <- c("Lake", "Porter", "LaPorte", "Newton", "Jasper", "Starke", "Gibson", "Posey", "Vanderburgh", "Warrick", "Spencer", "Perry")
control_counties   <- c("Benton", "Warren", "White", "Tippecanoe", "Carroll", "Cass", "Pulaski", "Fulton", "Kosciusko", "Marshall", "Elkhart", "Knox", "Daviess", "Pike", "Dubois", "Crawford", "Orange", "Martin")

ilearn_school_2025$group <- ifelse(
  ilearn_school_2025$County %in% treatment_counties, "treatment",
  ifelse(ilearn_school_2025$County %in% control_counties, "control", "none")
)

# data cleaning
  # change to numeric
  ilearn_school_2025$`ELAProficient %_3` <- as.numeric(ilearn_school_2025$`ELAProficient %_3`)
  class(ilearn_school_2025$`ELAProficient %_3`)
  ilearn_school_2025$`ELAProficient %_8` <- as.numeric(ilearn_school_2025$`ELAProficient %_3`)
  class(ilearn_school_2025$`ELAProficient %_8`)

#histograms:

#separate
ilearn_school_2025 %>%
  filter(group %in% c("treatment", "control")) %>%
  ggplot(aes(x = `ELAProficient %_3`)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  facet_wrap(~ group, scales = "free_y") +
  labs(
    title = "Outcome distribution by group",
    x = "Outcome",
    y = "Count"
  ) +
  theme_minimal()

#overlaid
ilearn_school_2025 %>%
  filter(group %in% c("treatment", "control")) %>%
  ggplot(aes(x = `ELAProficient %_3`, fill = group)) +
  geom_histogram(
    bins = 30,
    alpha = 0.5,
    position = "identity"
  ) +
  labs(
    title = "Outcome distribution: treatment vs control",
    x = "Outcome",
    y = "Count"
  ) +
  theme_minimal()

ilearn_school_2025 %>%
  filter(group %in% c("treatment", "control")) %>%
  ggplot(aes(x = `ELAProficient %_8`, fill = group)) +
  geom_histogram(
    bins = 30,
    alpha = 0.5,
    position = "identity"
  ) +
  labs(
    title = "Outcome distribution: treatment vs control",
    x = "Outcome",
    y = "Count"
  ) +
  theme_minimal()

#mirrored
ilearn_school_2025 %>%
  filter(group %in% c("treatment", "control")) %>%
  filter(is.finite(`ELAProficient %_3`)) %>%
  ggplot(aes(x = `ELAProficient %_3`)) +
  geom_histogram(
    aes(y = after_stat(count) * ifelse(group == "control", -1, 1),
        fill = group),
    bins = 30,
    color = "white"
  ) +
  scale_y_continuous(labels = abs) +
  labs(x = "Outcome", y = "Count", title = "Mirrored histogram: treatment vs control") +
  theme_minimal()
