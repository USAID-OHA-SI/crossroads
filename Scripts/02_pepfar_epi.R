# PROJECT:  crossroads
# PURPOSE:  global epi trends
# AUTHOR:   A.Chafetz | USAID
# REF ID:   0a43dd9d 
# LICENSE:  MIT
# DATE:     2024-08-05
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
  
  ref_id <- "0a43dd9d"  #a reference to be places in viz captions 
  
  path <- "Data/2023_unaids_est_global.parquet" 
  
  caption_src <- glue("Source: UNAIDS AIDSinfo Global Data 2024 Release | Ref id: {ref_id}")
  
# IMPORT ------------------------------------------------------------------
  
  df_unaids <- read_parquet(path)
  

# MUNGE -------------------------------------------------------------------

  df_epi <- df_unaids %>% 
    filter(indicator %in% c("Total deaths to HIV Population", "Number New HIV Infections"),
           age == "All",
           sex == "All",
           (country == "Global" | pepfar == TRUE)) %>% 
    distinct()
  
  df_epi <- df_epi %>% 
    mutate(type = ifelse(country == "Global", "Global", "PEPFAR")) %>% 
    group_by(type, year, indicator) %>% 
    summarise(estimate = sum(estimate, na.rm = TRUE),
              .groups = "drop")
  
  df_viz <- df_epi %>% 
    mutate(est_viz = ifelse(indicator == "Total deaths to HIV Population", -estimate, estimate),
           end_pt = case_when(year == max(year) | year == min(year) ~ est_viz),
           lab_pt = case_when(year == max(year) ~ est_viz),
           grp = glue("{type}_{indicator}"))
  

# VIZ ---------------------------------------------------------------------

  df_viz %>% 
    ggplot(aes(year, est_viz, group = grp, fill = indicator, color = indicator)) +
    geom_blank(aes(y = -est_viz)) +
    geom_area(alpha = .2, position = "identity") +
    geom_point(aes(y = end_pt), na.rm = TRUE, show.legend = FALSE) +
    geom_text(aes(label = label_number(.1, scale_cut = cut_si(""))(abs(lab_pt))), na.rm = TRUE,
              family = "Source Sans Pro", hjust = -.3) +
    geom_hline(yintercept = 0, color = "white") +
    geom_vline(xintercept = 2004, color = matterhorn, linetype = "dashed") +
    facet_grid(~type) +
    scale_y_continuous(breaks = seq(-3e6, 3e6, by = 1e6)) + 
    scale_x_continuous(breaks = c(2004, seq(1990, 2020, by = 10))) +
    scale_fill_manual(values = c("Total deaths to HIV Population" = "#CF0B29" ,
                                 "Number New HIV Infections" = "#15A6AE"),) +
    scale_color_manual(values = c("Total deaths to HIV Population" = "#CF0B29" ,
                                  "Number New HIV Infections" = "#15A6AE"),
                       guides) +
    guides(color = FALSE) + 
    labs(x = NULL, y = NULL, fill = NULL,
         title = "[TITLE TO BE SUPPLIED]",
         caption = glue("Note: PEPFAR values calculated by summing point estimates
                        {caption_src}")) +
    coord_cartesian(clip = "off") +
    si_style_ygrid() +
    theme(axis.text.y = element_blank())
  
  si_save("02_pepfar_epi",
          path = "Images", device = "png")
  