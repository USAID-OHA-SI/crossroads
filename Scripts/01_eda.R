# PROJECT:  crossroads
# PURPOSE:  EDA
# AUTHOR:   A.Chafetz | USAID
# REF ID:   887ba8cf 
# LICENSE:  MIT
# DATE:     2024-08-01
# UPDATED:  2024-08-05

# DEPENDENCIES ------------------------------------------------------------
  
  #general
  library(tidyverse)
  library(glue)
  library(arrow, warn.conflicts = FALSE)
  #oha
  library(gagglr) ##install.packages('gagglr', repos = c('https://usaid-oha-si.r-universe.dev', 'https://cloud.r-project.org'))
  #viz extensions
  library(scales, warn.conflicts = FALSE)
  library(systemfonts)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  ref_id <- "887ba8cf"  #a reference to be places in viz captions 
  
  
# IMPORT ------------------------------------------------------------------
  
  df_unaids <- read_parquet("Data/2023_unaids_est.parquet")

# MUNGE -------------------------------------------------------------------

  glimpse(df_unaids)
  
  df_unaids %>% 
    distinct(indicator)
  
  df_unaids <- df_unaids %>% 
    rename( achv_95_plhiv = `Achieved 95s with PLHIV base`,
            achv_95_relative = `Achieved 95s with relative base`)

  
  df_unaids %>% 
    filter(indicator == "Number PLHIV",
           age == "All",
           sex == "All",
           country != region,
           year == max(year)) %>%
    count(year, pepfar, wt = estimate) %>% 
    mutate(share = n / sum(n))
  
  
  df_unaids %>% 
    filter(indicator == "Number New HIV Infections",
           age == "All",
           sex == "All",
           country != region,
           year > 1999,
           ) %>%
    count(year, pepfar, wt = estimate) %>% 
    group_by(year) %>% 
    mutate(share = n / sum(n)) %>% 
    ungroup() %>% 
    filter(pepfar) %>% 
    prinf()
  
  df_unaids %>% 
    filter(year == max(year),
           pepfar == TRUE
           ) %>% 
    distinct(year, country, achv_95_plhiv, achv_95_relative, epi_control, epi_ratio_2023) %>% 
    arrange(desc(achv_95_relative), desc(epi_control)) %>% 
    prinf()
  
  

# EPI CONTROL -------------------------------------------------------------


  #Pull indicators 
  df_epi_pepfar <- df_unaids %>%
    dplyr::filter(age == "All", sex == "All",
                  indicator %in% c("Total deaths to HIV Population", "Number New HIV Infections")) %>% #grab indicators 
    dplyr::select(year, country,indicator, estimate) %>% 
    dplyr::arrange(country, indicator, year) #order rows by these variables 
  
  
  #Perform necessary munging
  df_epi_pepfar <- df_epi_pepfar %>% 
    tidyr::pivot_wider(names_from = "indicator", 
                       values_from = "estimate", #pivots data wide into infections column
                       names_glue = "{indicator %>% stringr::str_extract_all('deaths|Infections') %>% tolower}") 
  
  #Add in ALL PEPFAR data
  df_epi_pepfar <-
    df_epi_pepfar %>%
    dplyr::bind_rows(df_epi_pepfar %>%
                       dplyr::mutate(country = "All PEPFAR") %>%
                       dplyr::group_by(country, year) %>%
                       dplyr::summarise(across(where(is.numeric), \(x) sum(x,na.rm = TRUE)), .groups = "drop")) #sums PEPFAR country estimates 
  
  #Create epi control flag
  df_epi_pepfar <-
    df_epi_pepfar %>%
    group_by(country) %>% 
    dplyr::mutate(declining_deaths = deaths - dplyr::lag(deaths, order_by = year) <= 0) %>% #TRUE/FALSE declining 
    dplyr::mutate(declining_infections = infections - dplyr::lag(infections, order_by = year) <= 0) %>% #TRUE/FALSE declining 
    dplyr::mutate(infections_below_deaths = infections < deaths,
                  ratio = infections / deaths,
                  direction_streak = sequence(rle(declining_deaths)$lengths),
                  epi_control = declining_deaths == TRUE & infections_below_deaths == TRUE) %>% #epi control definition 
    ungroup()
  
  
  #Add colors to indicators and flip axis
  df_epi_pepfar <- df_epi_pepfar %>% 
    tidyr::pivot_longer(c(infections, deaths), names_to = "indicator") %>% #put back indicators in column
    dplyr::arrange(country, indicator, year) %>%
    dplyr::mutate(val_mod = ifelse(indicator == "deaths", -value, value), #create dual-axis
                  fill_color = ifelse(indicator == "deaths", glitr::orchid_bloom, glitr::electric_indigo)) #add colors to indicate flip axis
 
  
  #COUNTRY
  df_viz_cntry <- df_epi_pepfar %>%
    filter(!is.na(value)) %>% 
    group_by(country) %>% 
    dplyr::mutate(val_lab = dplyr::case_when(year == max(year, na.rm = TRUE) ~ 
                                               scales::number(value, 1, scale = 1e-3, suffix = "k")), #standardize number format
                  max_plot_pt = max(value, na.rm = TRUE),
                  min_plot_pt = min(val_mod, na.rm = TRUE),
                  lab_pt = dplyr::case_when(year == max(year) ~ val_mod),
                  indicator = ifelse(indicator == "deaths", "Total Deaths to HIV Population", "New HIV Infections"), #creating labels
                  new_hiv_label = dplyr::case_when(value == max_plot_pt ~ indicator),  #assigning label location to min/max
                  tot_death_label = dplyr::case_when(val_mod == min_plot_pt ~ indicator)) %>%
    dplyr::mutate(cntry_order = max(value, na.rm = T)) %>%
    ungroup() %>% 
    dplyr::mutate(country = forcats::fct_reorder(country, cntry_order, .desc = T))
  
  df_viz_cntry %>%
    ggplot(aes(year, val_mod, group = indicator, fill = fill_color, color = fill_color)) +
    geom_blank(aes(y = max_plot_pt)) + #sets max y-axis above
    geom_blank(aes(y = -max_plot_pt)) + #sets max y-axis below 
    geom_area(alpha = .25) +
    geom_hline(yintercept = 0, color = glitr::grey80k) +
    geom_line() +
    facet_wrap(~country, scales =  "free_y") +
    scale_fill_identity() +
    scale_color_identity() +
    si_style() +
    theme(legend.position = "none")
  


# WHERE ARE DEATHS INCREASING ---------------------------------------------


  df_epi_pepfar %>% 
    filter(indicator == "deaths",
           year == max(year),
           declining_deaths == FALSE) %>% 
    # View()
    pull(country)
    
# WHERE ARE INFECTIONS INCREASING -----------------------------------------
  
  df_epi_pepfar %>% 
    filter(indicator == "infections",
           year == max(year),
           declining_infections == FALSE) %>% 
    # View() %>% 
    pull(country)
  
  

# WHICH COUNTRIES JOINED EPI CONTROL --------------------------------------

  df_status <- df_epi_pepfar %>% 
    filter(year >= max(year) - 1) %>%
    distinct(country, year, epi_control) %>% 
    mutate(year = ifelse(year == max(year), "current", "prior")) %>% 
    pivot_wider(names_from = year,
                values_from = epi_control) %>% 
    mutate(status = case_when(prior == FALSE & current == FALSE ~ "outside",
                              prior == TRUE & current == TRUE ~ "maintained",
                              prior == FALSE & current == TRUE ~ "joined",
                              prior == TRUE & current == FALSE ~ "lost",
                              )) %>%
    mutate(status = factor(status, c("maintained", "joined", "lost", "outside"))) %>% 
    arrange(status) %>% 
    select(-prior, -current)
  
  df_status %>% 
    count(status)

  df_plhiv <- df_unaids %>% 
    filter(indicator == "Number PLHIV",
           age == "All",
           sex == "All",
           year == max(year)) %>%
    count(country, wt = estimate, name = "plhiv_2023") 
    

    
  df_epi_pts <- df_epi_pepfar %>% 
    filter(year >= max(year)) %>% 
    distinct(year, country, epi_control, declining_deaths) %>% 
    pivot_longer(-c(year, country),
                 names_to = "indicator",
                 values_to = "achieved") %>% 
    mutate(indicator = case_match(indicator,
                                  "epi_control" ~ "Epi",
                                  "declining_deaths" ~ "Deaths"),
           type = "Epi",
           age = "All") 
  
  
  df_95s <- df_unaids %>% 
    filter(indicator %in% c("Percent Known Status of PLHIV",
                            "Percent on ART of PLHIV",
                            "Percent VLS of PLHIV",
                            "Percent on ART with Known Status",
                            "Percent VLS on ART")) %>% 
    mutate(type = ifelse(str_detect(indicator, "PLHIV"), "PLHIV", "Relative"))
    
  df_95s <- df_95s %>% 
    bind_rows(df_95s %>% 
                filter(indicator == "Percent Known Status of PLHIV") %>% 
                mutate(type = "Relative"))
  
  df_95s <- df_95s %>% 
    mutate(indicator = str_extract(indicator, "Known Status|on ART|VLS"))

  df_target <- tibble::tribble(
          ~type,     ~indicator, ~target,
        "PLHIV", "Known Status",     95L,
        "PLHIV",          "VLS",     90L,
        "PLHIV",       "on ART",     86L,
     "Relative", "Known Status",     95L,
     "Relative",          "VLS",     95L,
     "Relative",       "on ART",     95L
     )


  df_95s <- df_95s %>%
    left_join(df_target,
              by = join_by(indicator, type)) %>%
    mutate(achieved = estimate >= target) %>% 
    bind_rows(df_epi_pts) %>% 
    left_join(df_status,
              by = join_by(country)) %>% 
    mutate(fill_shape =  case_when(indicator == "Deaths" & achieved == TRUE ~ 25,
                                   indicator == "Deaths" & achieved == FALSE ~ 24,
                                   indicator == "Epi" ~ 22,
                                   TRUE ~ 21),
           fill_color = ifelse(achieved, electric_indigo, "white"),
           indicator = factor(indicator, c("Known Status", "on ART", "VLS", "Epi", "Deaths"))) %>% 
    left_join(df_plhiv, join_by(country))
  
  df_95s_all <-  df_95s %>% 
    filter(year == max(year),
           age == "All") %>% 
    group_by(country) %>%
    mutate(count95s = sum(achieved, na.rm = TRUE)) %>% 
    ungroup()
    
  
  df_95s_all %>% 
    mutate(country = case_match(country,
                                "Democratic Republic of the Congo" ~ "DRC",
                                "Dominican Republic" ~ "DR",
                                "Trinidad and Tobago" ~ "T&T",
                                "Papua New Guinea" ~ "PNG",
                                .default = country
                                )) %>%
    filter(country != "All PEPFAR") %>% 
    # ggplot(aes(indicator, fct_reorder2(country, count95s, plhiv_2023, .desc = FALSE),
    ggplot(aes(indicator, fct_reorder2(country, plhiv_2023,count95s, .desc = FALSE),
               fill = fill_color, shape = fill_shape)) +
    geom_point(color = electric_indigo, stroke = 1.1) +
    facet_grid(status ~ fct_rev(type), scales = "free", space = "free") +
    scale_x_discrete(position = "top") +
    scale_fill_identity() +
    scale_shape_identity() +
    labs(x = NULL, y = NULL) + 
    si_style_nolines() +
    theme(axis.text.x = element_markdown(),
          strip.placement = "outside",
          panel.spacing = unit(.5, "lines"))
  

  
# 95s ---------------------------------------------------------------------

  df_95s %>% 
    filter(year == max(year),
           age == "All",
           sex == "All") %>% 
    distinct(country, achv_95_relative) %>% 
    count(wt= achv_95_relative)

  df_95s %>% 
    filter(year == max(year),
           age == "All",
           sex == "All") %>% 
    distinct(country, achv_95_plhiv) %>% 
    count(wt= achv_95_plhiv)
  

  df_95s %>%
    filter(type == "Relative",
           age == "All",
           sex == "All",
           achieved == TRUE) %>% 
    group_by(year, country) %>% 
    filter(n() == 3) %>% 
    ungroup() %>% 
    distinct(year, country) %>% 
    arrange(country) %>% 
    count(year)
  
  df_95s %>% 
    filter(year == max(year),
           age == "All",
           sex == "All",
           type == "Relative") %>% 
    group_by(country) %>%
    # mutate(count95s = sum(achieved, na.rm = TRUE)) %>% 
    mutate(count95s = sum(achv_95_relative, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(order = ifelse(indicator == "Known Status", estimate, 0),
           order = ifelse(is.na(order), 0, order)) %>% 
    ggplot(aes(estimate, fct_reorder(country, order, .fun = max, .na_rm = TRUE, .desc = FALSE), color = achieved)) +
    # ggplot(aes(estimate, fct_reorder2(country, plhiv_2023, count95s, .desc = FALSE), color = achieved)) +
    annotate(geom = "rect",
             xmin = 95, xmax = 100,
             ymin = 0, ymax = Inf,
             fill = si_palettes$electric_indigo_t[5]) +
    geom_linerange(aes(xmin = lower_bound, xmax = upper_bound), color = "gray70", linewidth = 1.1, na.rm = TRUE) +
    geom_point(na.rm = TRUE) +
    facet_wrap(~ indicator) +
    scale_x_continuous(limits = c(0, 100)) +
    scale_color_manual(values = c(orchid_bloom,electric_indigo)) +
    labs(x = NULL, y = NULL) +
    si_style_xgrid() +
    theme(legend.position = "none")
  
  