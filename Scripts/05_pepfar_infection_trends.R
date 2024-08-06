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
           pepfar == TRUE,
           country != region,
           !is.na(estimate),
           year >= 2010,
           age == "All",
           sex == "All")
  
  # df_infections <- 
  df_infections_tr <- df_infections %>% 
    select(year, country, pepfar, estimate) %>% 
    mutate(baseline = case_when(year == 2010 ~ estimate)) %>% 
    group_by(country) %>% 
    fill(baseline) %>% 
    ungroup() %>% 
    mutate(trend_from_baseline = ((estimate - baseline) / baseline) + 1)

# VIZ ---------------------------------------------------------------------
  
  df_infections_tr %>% 
    filter(!is.na(trend_from_baseline)) %>% 
    ggplot(aes(year, trend_from_baseline, group = country)) +
    geom_hline(yintercept = 1) +
    geom_line(alpha = .2, linewidth = 1.1) +
    geom_text_repel(data = . %>% filter(year == max(year)), #trend_from_baseline > 1),
               aes(label = country), family = "Source Sans Pro", color = matterhorn) +
    # facet_wrap(~fct_reorder2(country, year, trend_from_baseline)) +
    scale_y_continuous(limits = c(0, 2), oob = squish) +
    labs(x = NULL, y = NULL,
         caption = caption_src) +
    coord_cartesian(clip = "off") +
    si_style()
