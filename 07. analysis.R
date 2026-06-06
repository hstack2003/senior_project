library(knitr)
library(tidyverse)
library(tidyr)
library(fixest)
library(gt)
source("06. tables_figures.R")

#Main Results: Age group analysis by subject
subject_age_did_results <- istep_did_data |> 
  mutate(age_range = (case_when(GRADE_CODE %in% c("03", "04", "05") ~ "Elementary",
                                GRADE_CODE %in% c("06", "07", "08") ~ "Middle",
                                GRADE_CODE %in% c("09", "10") ~ "High School"))) |>
  bind_rows(istep_did_data |> mutate(age_range = "All Grades")) |> 
  mutate(age_range = factor(age_range,
                            levels = c("All Grades", "Elementary", "Middle", "High School"))) |> 
  nest(data = -c(subject, age_range)) |>
  mutate(mod = map(data, 
                   ~ feols(`Proficient %` ~ treated * post_treat | 
                             IDOE_SCHOOL_ID + SCHOOL_YEAR_ID,
                           cluster = ~ IDOE_SCHOOL_ID,
                           data = .x)),
         coefs = map(mod, broom::tidy)) |> 
  select(subject, age_range, coefs) |> 
  unnest(cols = coefs) |> 
  group_by(subject) |> 
  mutate(p_adj_holm = p.adjust(p.value, method = "holm")) |> 
  ungroup() |> 
  mutate(age_range = factor(age_range,
                            levels = c("All Grades", "Elementary", "Middle", "High School"))) |>
  arrange(age_range)

main_results_tbl <- subject_age_did_results |> 
  mutate(estimate_se = sprintf("%.3f\n(%.2f)", estimate, std.error)) |>
  select(age_range, subject, estimate_se, p.value, p_adj_holm) |>
  pivot_wider(names_from = subject,
              values_from = c(estimate_se, p.value, p_adj_holm)) |>
  gt() |> 
  cols_label(estimate_se_ELA = "Estimate (SE)",
             p.value_ELA = "p-value",
             p_adj_holm_ELA = "Holm p",
             estimate_se_Math = "Estimate (SE)",
             p.value_Math = "p-value",
             p_adj_holm_Math = "Holm p") |>
  tab_spanner(label = "ELA",
              columns = c(estimate_se_ELA, p.value_ELA, p_adj_holm_ELA)) |>
  tab_spanner(label = "Math",
              columns = c(estimate_se_Math, p.value_Math, p_adj_holm_Math)) |>
  fmt_number(columns = contains("p.value"),decimals = 3) |>
  fmt_number(columns = contains("p_adj_holm"),decimals = 3)

# Subject / Proficiency Level Results
subject_prof_did_results <- istep_did_data |> 
  mutate(age_range = (case_when(GRADE_CODE %in% c("03", "04", "05") ~ "Elementary",
                                GRADE_CODE %in% c("06", "07", "08") ~ "Middle",
                                GRADE_CODE %in% c("09", "10") ~ "High School"))) |>
  bind_rows(istep_did_data |> mutate(age_range = "All Grades")) |> 
  mutate(age_range = factor(age_range,
                            levels = c("All Grades", "Elementary", "Middle", "High School"))) |>
  filter(
         !is.na(prof_level)) |> 
  nest(data = -c(subject, prof_level, age_range)) |>
  mutate(mod = map(data, 
                   ~ feols(`Proficient %` ~ treated * post_treat | 
                             IDOE_SCHOOL_ID + SCHOOL_YEAR_ID,
                           cluster = ~ IDOE_SCHOOL_ID,
                           data = .x)),
         coefs = map(mod, broom::tidy)) |> 
  select(subject, prof_level, age_range, coefs) |> 
  unnest(cols = coefs) |> 
  group_by(subject) |> 
  mutate(p_adj_holm = p.adjust(p.value, method = "holm")) |> 
  ungroup() |> 
  arrange(age_range, prof_level)

prof_level_results_tbl <- subject_prof_did_results |> 
  mutate(estimate_se = sprintf("%.3f\n(%.2f)", estimate, std.error)) |>
  select(age_range, subject, prof_level, estimate_se, p.value, p_adj_holm) |>
  pivot_wider(names_from = subject,
              values_from = c(estimate_se, p.value, p_adj_holm)) |>
  gt(
     groupname_col = "age_range") |>
  cols_label(estimate_se_ELA = "Estimate (SE)",
             p.value_ELA = "p-value",
             p_adj_holm_ELA = "Holm p",
             estimate_se_Math = "Estimate (SE)",
             p.value_Math = "p-value",
             p_adj_holm_Math = "Holm p") |>
  tab_spanner(label = "ELA",
              columns = c(estimate_se_ELA, p.value_ELA, p_adj_holm_ELA)) |>
  tab_spanner(label = "Math",
              columns = c(estimate_se_Math, p.value_Math, p_adj_holm_Math)) |>
  fmt_number(columns = contains("p.value"),decimals = 3) |>
  fmt_number(columns = contains("p_adj_holm"),decimals = 3)

# Subject / Age / White Level results
# subject_white_did_results <- istep_did_data |> 
#   mutate(age_range = (case_when(GRADE_CODE %in% c("03", "04", "05") ~ "Elementary",
#                                 GRADE_CODE %in% c("06", "07", "08") ~ "Middle",
#                                 GRADE_CODE %in% c("09", "10") ~ "High School"))) |>
#   bind_rows(istep_did_data |> mutate(age_range = "All Grades")) |> 
#   mutate(age_range = factor(age_range,
#                             levels = c("All Grades", "Elementary", "Middle", "High School"))) |>
#   filter(
#     !is.na(white_level)) |> 
#   nest(data = -c(subject, white_level, age_range)) |>
#   mutate(mod = map(data, 
#                    ~ feols(`Proficient %` ~ treated * post_treat | 
#                              IDOE_SCHOOL_ID + SCHOOL_YEAR_ID,
#                            cluster = ~ IDOE_SCHOOL_ID,
#                            data = .x)),
#          coefs = map(mod, broom::tidy)) |> 
#   select(subject, white_level, age_range, coefs) |> 
#   unnest(cols = coefs) |> 
#   group_by(subject) |> 
#   mutate(p_adj_holm = p.adjust(p.value, method = "holm")) |> 
#   ungroup() |> 
#   arrange(age_range, white_level)
# 
# white_level_results_tbl <- subject_white_did_results |> 
#   mutate(estimate_se = sprintf("%.3f\n(%.2f)", estimate, std.error)) |>
#   select(age_range, subject, white_level, estimate_se, p.value, p_adj_holm) |>
#   pivot_wider(names_from = subject,
#               values_from = c(estimate_se, p.value, p_adj_holm)) |>
#   gt(
#     groupname_col = "age_range") |>
#   cols_label(estimate_se_ELA = "Estimate (SE)",
#              p.value_ELA = "p-value",
#              p_adj_holm_ELA = "Holm p",
#              estimate_se_Math = "Estimate (SE)",
#              p.value_Math = "p-value",
#              p_adj_holm_Math = "Holm p") |>
#   tab_spanner(label = "ELA",
#               columns = c(estimate_se_ELA, p.value_ELA, p_adj_holm_ELA)) |>
#   tab_spanner(label = "Math",
#               columns = c(estimate_se_Math, p.value_Math, p_adj_holm_Math)) |>
#   fmt_number(columns = contains("p.value"),decimals = 3) |>
#   fmt_number(columns = contains("p_adj_holm"),decimals = 3)

# FRL / Age / Subject 
# subject_free_meal_did_results <- istep_did_data |> 
#   mutate(age_range = (case_when(GRADE_CODE %in% c("03", "04", "05") ~ "Elementary",
#                                 GRADE_CODE %in% c("06", "07", "08") ~ "Middle",
#                                 GRADE_CODE %in% c("09", "10") ~ "High School"))) |>
#   bind_rows(istep_did_data |> mutate(age_range = "All Grades")) |> 
#   mutate(age_range = factor(age_range,
#                             levels = c("All Grades", "Elementary", "Middle", "High School"))) |>
#   filter(
#     !is.na(free_meal_level)) |> 
#   nest(data = -c(subject, free_meal_level, age_range)) |>
#   mutate(mod = map(data, 
#                    ~ feols(`Proficient %` ~ treated * post_treat | 
#                              IDOE_SCHOOL_ID + SCHOOL_YEAR_ID,
#                            cluster = ~ IDOE_SCHOOL_ID,
#                            data = .x)),
#          coefs = map(mod, broom::tidy)) |> 
#   select(subject, free_meal_level, age_range, coefs) |> 
#   unnest(cols = coefs) |> 
#   filter(term == "treated:post_treat") |> 
#   group_by(subject) |> 
#   mutate(p_adj_holm = p.adjust(p.value, method = "holm")) |> 
#   ungroup() |> 
#   arrange(age_range, free_meal_level)
# 
# free_meal_level_results_tbl <- subject_free_meal_did_results |> 
#   mutate(estimate_se = sprintf("%.3f\n(%.2f)", estimate, std.error)) |>
#   select(age_range, subject, free_meal_level, estimate_se, p.value, p_adj_holm) |>
#   pivot_wider(names_from = subject,
#               values_from = c(estimate_se, p.value, p_adj_holm)) |>
#   gt(
#     groupname_col = "age_range") |>
#   cols_label(estimate_se_ELA = "Estimate (SE)",
#              p.value_ELA = "p-value",
#              p_adj_holm_ELA = "Holm p",
#              estimate_se_Math = "Estimate (SE)",
#              p.value_Math = "p-value",
#              p_adj_holm_Math = "Holm p") |>
#   tab_spanner(label = "ELA",
#               columns = c(estimate_se_ELA, p.value_ELA, p_adj_holm_ELA)) |>
#   tab_spanner(label = "Math",
#               columns = c(estimate_se_Math, p.value_Math, p_adj_holm_Math)) |>
#   fmt_number(columns = contains("p.value"),decimals = 3) |>
#   fmt_number(columns = contains("p_adj_holm"),decimals = 3)
