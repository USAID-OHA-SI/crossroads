# PROJECT:  crossroads
# PURPOSE:  infection trends
# AUTHOR:   A.Chafetz | USAID
# REF ID:   874ba17d 
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
  library(ggrepel)
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  ref_id <- "874ba17d"  #a reference to be places in viz captions 
  
  path <- "Data/2023_unaids_est_global.parquet" 
  path_tiles <- "Data/worldtilegrid.csv" 
  
  caption_src <- glue("Source: UNAIDS AIDSinfo Global Data 2024 Release | Ref id: {ref_id}")
  
# IMPORT ------------------------------------------------------------------
  
  df_unaids <- read_parquet(path)
  

# MUNGE -------------------------------------------------------------------

  df_infections <- df_unaids %>% 
    filter(indicator == "Number New HIV Infections",
           # pepfar == TRUE,
           # country != region,
           !is.na(estimate),
           year >= 2010,
           age %in% c("All", "0-14"),
           sex == "All")
  
  
  df_infections_tr <- df_infections %>% 
    select(year, region, country, age, pepfar, estimate) %>% 
    mutate(baseline = case_when(year == 2010 ~ estimate),
           age = ifelse(age == "0-14", "Peds", age)) %>% 
    group_by(country, age) %>% 
    fill(baseline) %>% 
    ungroup() %>% 
    mutate(trend_from_baseline = estimate / baseline)
    # mutate(trend_from_baseline = ((estimate - baseline) / baseline) + 1)

  
  df_infections_tr <- df_infections_tr %>% 
    mutate(reg_value = case_when(country == region & age == "All" & year == max(year) ~ trend_from_baseline)) %>% 
    group_by(region) %>% 
    fill(reg_value, .direction = "up") %>% 
    ungroup()
  
  v_hi_plhiv <- df_unaids %>%
    filter(indicator == "Number PLHIV",
           pepfar == TRUE,
           country != region,
           !is.na(estimate),
           age == "All",
           sex == "All") %>% 
    group_by(country) %>% 
    filter(year == max(year)) %>% 
    ungroup() %>% 
    count(country, wt = estimate, sort = TRUE) %>% 
    slice_max(n = 7, order_by = n) %>% 
    pull(country)
  
  df_viz <- df_infections_tr %>% 
    filter(pepfar == TRUE,
           country != region,
           !is.na(trend_from_baseline)) %>% 
    mutate(country = case_match(country,
                                "Democratic Republic of the Congo" ~ "DRC",
                                "Dominican Republic" ~ "DR",
                                "Trinidad and Tobago" ~ "T&T",
                                "Papua New Guinea" ~ "PNG",
                                .default = country),
           pt_lab = case_when(year == max(year) & trend_from_baseline > 1 ~ country,
                              year == max(year) & country %in% v_hi_plhiv ~ country),
           fill_color = case_when(year == max(year) & trend_from_baseline > 1 ~ "#CF0B29")) %>% 
    group_by(country, age) %>% 
    fill(fill_color, .direction = "up") %>% 
    ungroup() %>% 
    mutate(fill_color = ifelse(is.na(fill_color), slate, fill_color))

    
# VIZ ---------------------------------------------------------------------
  
  df_viz %>% 
    filter(age == "All") %>% 
    ggplot(aes(year, trend_from_baseline, group = country, color = fill_color)) +
    geom_hline(yintercept = 1) +
    geom_line(alpha = .2, linewidth = 1.1) +
    geom_text_repel(aes(label = pt_lab), na.rm = TRUE,
                    seed = 42,
                    family = "Source Sans Pro", color = matterhorn) +
    facet_wrap(~fct_reorder(region, reg_value, .fun = max, .na_rm = TRUE, .desc = TRUE)) +
    scale_y_continuous(limits = c(0, 2), oob = squish) +
    scale_color_identity() + 
    coord_cartesian(clip = "off") +
    labs(x = NULL, y = NULL,
         title = "[TITLE TO BE SUPPLIED]",
         subtitle = "Relative change in new infections (all ages) from country's 2010 baseline",
         caption = glue("Calculation: Relative change from baseline (2010) = Year Estimate / Baseline Estimate (2010)
                         Note: Region sorted by relative change (all countries), but only PEPFAR countries displayed
                         PEPFAR countries with {max(df_viz$year)} values creater than 2010 labeled along with larger PLHIV countries
                        {caption_src}")) +
    si_style()
  
  si_save("05_infection_trends_all",
          path = "Images", device = "png")
  
  
  df_viz %>% 
    filter(age == "Peds") %>% 
    ggplot(aes(year, trend_from_baseline, group = country, color = fill_color)) +
    geom_hline(yintercept = 1) +
    geom_line(alpha = .2, linewidth = 1.1) +
    geom_text_repel(aes(label = pt_lab), na.rm = TRUE,
                    seed = 42,
                    family = "Source Sans Pro", color = matterhorn) +
    facet_wrap(~fct_reorder(region, reg_value, .fun = max, .na_rm = TRUE, .desc = TRUE)) +
    scale_y_continuous(limits = c(0, 2), oob = squish) +
    scale_color_identity() + 
    coord_cartesian(clip = "off") +
    labs(x = NULL, y = NULL,
         title = "[TITLE TO BE SUPPLIED]",
         subtitle = "Relative change in new pediatric (<15) infections from from country's 2010 baseline",
         caption = glue("Calculation: Relative change from baseline (2010) = Year Estimate / Baseline Estimate (2010)
                         Note: Region sorted by relative change (all countries), but only PEPFAR countries displayed
                         PEPFAR countries with {max(df_viz$year)} values creater than 2010 labeled along with larger PLHIV countries
                        {caption_src}")) +
    si_style()
  
  si_save("05_infection_trends_peds",
          path = "Images", device = "png")
