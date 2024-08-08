# PROJECT:  crossroads
# PURPOSE:  PLHIV projection
# AUTHOR:   A.Chafetz | USAID
# REF ID:   fcbe3e59 
# LICENSE:  MIT
# DATE:     2024-08-06
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
  #general
  library(tidyverse)
  library(arrow, warn.conflicts = FALSE)
  library(glue)
  #oha
  library(gagglr) ##install.packages('gagglr', repos = c('https://usaid-oha-si.r-universe.dev', 'https://cloud.r-project.org'))
  library(mindthegap)  
  #viz extensions
  library(scales, warn.conflicts = FALSE)
  library(systemfonts)
  library(tidytext)
  library(patchwork)
  library(ggtext)

# GLOBAL VARIABLES --------------------------------------------------------
  
  ref_id <- "fcbe3e59"  #a reference to be places in viz captions 
  
  path <- "Data/2023_unaids_est_global.parquet" 
  path_proj <- "Data/plhiv_proj.csv" 
  
  caption_src <- glue("Source: UNAIDS AIDSinfo Global Data 2024 Release + UNAID 2024 Global AIDS Update (p215) | Ref id: {ref_id}")
  
  
# IMPORT ------------------------------------------------------------------
  
  df_unaids <- read_parquet(path)
  
  #Source - UNAIDS The Urgency of Now p 215
  df_proj <- read_csv(path_proj)


# MUNGE -------------------------------------------------------------------

  df_unaids %>% 
    distinct(indicator)
  
  df_unaids %>% 
    filter(indicator == "Number PLHIV") %>% 
    count(indicator, age, sex)
  
  df_plhiv <- df_unaids %>% 
    filter(indicator == "Number PLHIV",
           age == "All",
           sex == "All",
           (country == "Global" | pepfar == TRUE))
  
  
  df_plhiv <- df_plhiv %>% 
    mutate(type = ifelse(country == "Global", "Global", "PEPFAR")) %>% 
    group_by(type, year, indicator) %>% 
    summarise(across(c(estimate, lower_bound, upper_bound),\(x) sum(x, na.rm = TRUE)),
              .groups = "drop")

  
  df_proj_start <- df_plhiv %>% 
    filter(type == "Global",
           # year == 2020) %>% 
           year == max(year)) %>% 
    select(-ends_with("bound")) 
  
  df_proj <- bind_rows(df_proj_start %>% 
              mutate(indicator = "PLHIV with constant 2022 coverage"),
            df_proj_start %>% 
              mutate(indicator = "PLHIV with 2025 targets met"),
            df_proj
            ) %>% 
    arrange(indicator)
  
  df_viz <- df_plhiv %>% 
    # filter(type == "Global") %>% 
    mutate(indicator = ifelse(type == "PEPFAR", "Number PLHIV (PEPFAR)", indicator),
           across(ends_with("bound"), \(x) ifelse(indicator == "Number PLHIV (PEPFAR)", NA, x))) %>% 
    bind_rows(df_proj)
  
  df_viz <- df_viz %>% 
    mutate(lab_pt = case_when(year == min(year) | year == max(year) | year == 2004 ~ estimate,
                          year == min(df_proj$year) & str_detect(indicator,"Number PLHIV") ~ estimate),
           fill_color = case_when(indicator == "Number PLHIV" ~ slate,
                                  indicator == "PLHIV with constant 2022 coverage" ~ orchid_bloom,
                                  indicator == "PLHIV with 2025 targets met" ~ electric_indigo))

  
# VIZ ---------------------------------------------------------------------

  
  df_viz %>% 
    ggplot(aes(year, estimate)) +
    geom_vline(xintercept = 2004, color = matterhorn, linetype = "dashed") +
    geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound), na.rm = TRUE,
                fill = si_palettes$slate_t[4], alpha = .4) +
    geom_line(aes(color = indicator, group = indicator), na.rm = TRUE,
              linewidth = 1.1) +
    geom_point(aes(fill = indicator, y = lab_pt), na.rm = TRUE, show.legend = FALSE,
               color = "white", shape = 21, size = 3, stroke = 1.3) +
    geom_text(aes(label = label_number(scale_cut = cut_si(""))(lab_pt)), 
              family = "Source Sans Pro", color = matterhorn,
              vjust = -1,
              na.rm = TRUE) +
    ylim(0, NA) +
    scale_x_continuous(breaks = seq(1990, 2050, by = 10)) +
    scale_fill_manual(aesthetics = c("fill", "color"),
                      values = c("Number PLHIV" = slate,
                                 "Number PLHIV (PEPFAR)" = si_palettes$slate_t[3],
                                 # "PLHIV with constant 2022 coverage" = orchid_bloom,
                                 # "PLHIV with 2025 targets met" = electric_indigo)) +
                                 "PLHIV with constant 2022 coverage" = "#CF0B29",
                                 "PLHIV with 2025 targets met" = "#15A6AE")) +
    labs(x = NULL, y = NULL, color = NULL,
         title = "[TITLE TO BE SUPPLIED]",
         subtitle = "PLHIV estimates projected out based on whether UNAID 2025 targets are met",
         caption = glue("Note: PEPFAR values calculated by summing point estimates
                        {caption_src} ")) + 
    coord_cartesian(clip = "off") +
    si_style() +
    theme(axis.text.y = element_blank())

  si_save("03_global_plhiv_proj",
          path = "Images", device = "png")  
  