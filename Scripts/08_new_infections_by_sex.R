# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  New infections by sex
# REF ID:   135ec134 
# LICENSE:  MIT
# DATE:     2024-08-22
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
# Libraries
library(tidyverse)
library(glitr)
library(glamr)
library(gophr)
library(systemfonts)
library(scales)
library(tidytext)
library(patchwork)
library(ggtext)
library(glue)
library(googlesheets4)
library(mindthegap)

# GLOBAL VARIABLES --------------------------------------------------------
  
  # SI specific paths/functions  
    load_secrets()
  
  ref_id <- "135ec134"

  
# IMPORT ------------------------------------------------------------------------------------
  
  g_id <- "1dJE-npFhFTSBn6ZhblhyDUaE_C7wX2QbTab-a_mAFUg"
  global_id <- "1KFgJlcY2aU9gLGmM0wtRry2YmyFpwk-OXGQ1bzB3LI8"
  
  df_inf <- range_speedread(g_id)
  df_inf_global <- read_sheet(global_id)
  
# MUNGE ------------------------------------------------------------------------------------
  
  
  #pull in PEPFAR countries
  df_inf_clean_pepfar <- df_inf %>% 
    janitor::clean_names() %>% 
    filter(time >= 2010) %>% 
    select(e_count, iso3, age, sex, other, e_ind, time, value) %>% 
    rename(country = e_count,
           ci = other,
           year = time) %>% 
    mutate(indicator = "New HIV Infections") %>% 
    mutate(ci = ifelse(is.na(ci), "val", ci)) %>% 
    filter(ci == "val") %>% 
    group_by(age, sex, year, indicator) %>% 
    summarize(total = sum(value, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(grp = "PEPFAR")
  
  #pull in glboal
  df_inf_clean_global <- df_inf_global %>% 
    janitor::clean_names() %>% 
    filter(time >= 2010) %>% 
    select(e_count, iso3, age, sex, other, e_ind, time, value) %>% 
    rename(country = e_count,
           ci = other,
           year = time) %>% 
    mutate(indicator = "New HIV Infections") %>% 
    mutate(ci = ifelse(is.na(ci), "val", ci)) %>% 
    filter(ci == "val") %>% 
    group_by(age, sex, year, indicator) %>% 
    summarize(total = sum(value, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(grp = "Global")
  
  
  df_inf_clean <- rbind(df_inf_clean_global, df_inf_clean_pepfar)
  #1) Btwn 2010 - 2023: Global (39% drop) vs ESA (59%) 
  
  
  #reshape to calculate percentage change 
  epi_calc <- df_inf_clean %>%
    filter(year %in% c(2010:2023)) %>% 
    pivot_wider(names_from = year,
                values_from = total,
                names_prefix = "year_") %>%
    mutate(
      perc_change = (year_2023 - year_2010)/(year_2010)) 
  
  epi_calc_2 <- df_inf_clean %>% 
    filter(year %in% c(2010:2023)) %>% 
    pivot_wider(names_from = year,
                values_from = total,
                names_prefix = "year_") %>%
    mutate(
      perc_change = (year_2023 - year_2010)/(year_2010)) 
  
  #get reference line value for 2010
  #epi_ref <- epi_glob %>% 
  # filter(year == 2010) %>% 
  #pull(number_new_hiv_infections)
  
  #prep data 
  epi_plot <- epi_calc %>% 
    mutate(base_value = year_2010) %>% 
    pivot_longer(cols = starts_with("year_"),
                 names_to = "year",
                 values_to = "number_new_hiv_infections") %>% 
    mutate(year = as.numeric(sub("year_","", year))) %>% 
    #filter(year == 2023) %>% 
    mutate(perc_change = (number_new_hiv_infections - base_value)/(base_value))
  
  epi_plot_2 <- epi_calc_2 %>% 
    mutate(base_value = year_2010) %>% 
    pivot_longer(cols = starts_with("year_"),
                 names_to = "year",
                 values_to = "number_new_hiv_infections") %>% 
    mutate(year = as.numeric(sub("year_","", year))) %>% 
    #filter(year == 2023) %>% 
    mutate(perc_change = (number_new_hiv_infections - base_value)/(base_value))
  
  
  #plot data
  epi_plot %>% 
    ggplot(aes(x = year, y = perc_change, group = sex)) + 
    geom_line(aes(color = sex), size = 1) + 
    geom_point(data = filter(epi_plot, year == 2023),
               aes(color = sex), size = 3) + 
    geom_hline(yintercept = 0, color = grey80k, size = 1) + 
    geom_text(data = . %>% filter(year == max(year)),
              aes(y = perc_change, color = sex, 
                  label = percent(perc_change, 1)),
              hjust = -0.3, family = "Source Sans Pro", size = 4) +
    facet_wrap(~grp) +
    si_style_ygrid()+ 
    scale_color_manual(values = c("M" = hw_hunter, "F" = hw_lavender_haze, "M+F" = trolley_grey)) +
    scale_x_continuous(breaks= seq(2010, 2025, 5),limits = c(2010, 2025)) + 
    scale_y_continuous(labels = scales::percent, limits = c(-.6,0)) +
    theme(legend.position = "none",
          plot.title = element_markdown(),
          axis.text.y = element_blank()) +
    labs(x = NULL, y = NULL ,
         title = glue("PERCENT DECLINE IN NEW HIV INFECTIONS AMONGST <span style= 'color:#419164;'>ADULT MEN</span> AND <span style= 'color:#876EC4;'>ADULT WOMEN</span>"),
         subtitle = "PEPFAR total calculated by summing point estimates",
         caption = glue("{source_note} 
                         Calculation: Relative change from baseline (2010) = Year Estimate / Baseline Estimate (2010)
                      Note: Missing data for Trinidad & Tobago, Papua New Guinea, Cameroon, and Brazil"))
  
  si_save("Images/08_new_inf_by_sex_revised.png")
  ######################################
  
  df_inf_clean_pepfar %>% 
    ggplot(aes(x = year, y= total, group = sex)) +
    geom_line(aes(color = sex), linewidth = 1) +
    facet_wrap(~grp)
