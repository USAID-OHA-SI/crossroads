# PROJECT:  crossroads
# PURPOSE:  
# AUTHOR:   A.Chafetz | USAID
# REF ID:   683acb85 
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
  
  ref_id <- "683acb85"  #a reference to be places in viz captions 
  
  path <- "Data/2023_unaids_est_global.parquet" 
  path_tiles <- "Data/worldtilegrid.csv" 
  
  caption_src <- glue("Source: UNAIDS AIDSinfo Global Data 2024 Release + Country Coordinates from Schwabish/Saifee/Llambrechts | Ref id: {ref_id}")
  
  
  
  
# IMPORT ------------------------------------------------------------------
  
  df_unaids <- read_parquet(path)
  
  #Source - https://www.maartenlambrechts.com/2017/10/22/tutorial-a-worldtilegrid-with-ggplot2.html
  worldtilegrid <- read_csv(path_tiles)

# MUNGE -------------------------------------------------------------------

  df_unaids %>% 
    distinct(indicator)
  
  df_unaids %>% 
    filter(indicator == "Number New HIV Infections") %>% 
    count(indicator, age, sex)
  
  df_infections <- df_unaids %>% 
    filter(indicator == "Number New HIV Infections",
           country != region,
           !is.na(estimate),
           age == "All",
           sex == "All")
  
  df_infections <- df_infections %>% 
    select(year, iso, country, pepfar, estimate) %>% 
    group_by(year) %>%
    mutate(share = estimate / sum(estimate, na.rm = TRUE)) %>% 
    ungroup()
  
  df_infections <- df_infections %>% 
    group_by(country) %>% 
    mutate(delta_yoy = lag(estimate, order_by = year) - estimate,
           is_decreaing = delta_yoy < 1) %>% 
    ungroup()
    
  df_viz <- df_infections %>% 
    filter(year == max(year)) %>%
    # tidylog::left_join(worldtilegrid, by = join_by(iso == alpha.3))
    tidylog::right_join(worldtilegrid %>% 
                          select(alpha.3, x, y),
                          by = join_by(iso == alpha.3)) %>% 
    mutate(pepfar = ifelse(is.na(pepfar) | pepfar == FALSE, "Rest of the World", "PEPFAR Countries"))

# VIZ ---------------------------------------------------------------------
  
  # map inspired by  https://www.maartenlambrechts.com/2017/10/22/tutorial-a-worldtilegrid-with-ggplot2.html

  v1 <- df_viz %>% 
    ggplot(aes(xmin = x, ymin = y, xmax = x + 1, ymax = y + 1)) +
    geom_rect(data=. %>% select(-pepfar), color = "#ffffff", 
              fill = si_palettes$slate_t[3]) +
    geom_rect(aes(fill = share), color = "#ffffff") +
    geom_text(aes(x = x, y = y, label = iso), 
              color = "#ffffff", alpha = 0.5, size = 2.5,
              nudge_x = 0.5, nudge_y = -0.5 ) + 
    facet_wrap(~pepfar) +
    scale_y_reverse() + 
    scale_fill_viridis_c(end = .8, labels = label_percent(),
                         option = "C", na.value = si_palettes$slate_t[3]) +
    coord_equal() +
    labs(caption = caption_src, fill = "Global share of new infection in 2023") +
    si_style_map() +
    theme(panel.spacing = unit(.3, "lines"))

  v2 <- df_viz %>% 
    filter(pepfar == "PEPFAR Countries") %>% 
    mutate(country = ifelse(country == "Democratic Republic of the Congo", "DRC", country)) %>% 
    mutate(country_lump = country %>% 
             fct_lump(n = 15, w = estimate, other_level = "Rest of PEPFAR") %>% 
             fct_reorder(estimate, .fun = sum) %>% 
             fct_relevel("Rest of PEPFAR", after = 0)) %>% 
    ggplot(aes(estimate, country_lump)) +
    geom_col(fill = si_palettes$slate_t[2]) +
    geom_text(data =. %>% filter(country_lump != "Rest of PEPFAR"),
              aes(label = label_number(1, scale_cut = cut_si(""))(estimate)),
              hjust = -.2, family = "Source Sans Pro", color = matterhorn) +
    scale_x_continuous(expand = c(.005,005)) +
    coord_cartesian(clip = "off") +
    labs(x = NULL, y = NULL,
         caption = caption_src) +
    si_style_xgrid() +
    theme(axis.text.x = element_blank())

  
  v1 + v2 +plot_layout(widths = c(2,1))

