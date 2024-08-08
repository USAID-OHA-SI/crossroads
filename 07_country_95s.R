# PROJECT:  crossroads
# PURPOSE:  country 95s 
# AUTHOR:   A.Chafetz | USAID
# REF ID:   f175df83 
# LICENSE:  MIT
# DATE:     2024-08-08
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
    
  #general
  library(tidyverse)
  library(arrow, warn.conflicts = FALSE)
  library(glue)
  #oha
  library(gagglr) ##install.packages('gagglr', repos = c('https://usaid-oha-si.r-universe.dev', 'https://cloud.r-project.org'))
  #viz extensions
  library(scales, warn.conflicts = FALSE)
  library(systemfonts)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  ref_id <- "f175df83"  #a reference to be places in viz captions 
  
  path <- "Data/2023_unaids_est_global.parquet" 

  caption_src <- glue("Source: UNAIDS AIDSinfo Global Data 2024 Release | Ref id: {ref_id}")
  
  
  
# IMPORT ------------------------------------------------------------------
  
  df_unaids <- read_parquet(path)
  

# MUNGE -------------------------------------------------------------------

  #Pull indicators 
  df_epi_pepfar <- df_unaids %>%
    filter(indicator %in% c("Total deaths to HIV Population", "Number New HIV Infections"),
           age == "All", 
           sex == "All",
           pepfar == TRUE
           ) %>% 
    select(year, country, indicator, estimate) %>% 
    arrange(country, indicator, year) #order rows by these variables 
  
  
  #Perform necessary munging
  df_epi_pepfar <- df_epi_pepfar %>% 
    pivot_wider(names_from = "indicator", 
                       values_from = "estimate", #pivots data wide into infections column
                       names_glue = "{indicator %>% stringr::str_extract_all('deaths|Infections') %>% tolower}") 
  
  #Create epi control flag
  df_epi_pepfar <- df_epi_pepfar %>%
    group_by(country) %>% 
    mutate(declining_deaths = deaths - lag(deaths, order_by = year) <= 0) %>% #TRUE/FALSE declining 
    mutate(declining_infections = infections - lag(infections, order_by = year) <= 0) %>% #TRUE/FALSE declining 
    mutate(infections_below_deaths = infections < deaths,
           ratio = infections / deaths,
           direction_streak = sequence(rle(declining_deaths)$lengths),
           epi_control = declining_deaths == TRUE & infections_below_deaths == TRUE) %>% #epi control definition 
    ungroup()
  
  
  #Add colors to indicators and flip axis
  df_epi_pepfar <- df_epi_pepfar %>% 
    pivot_longer(c(infections, deaths), names_to = "indicator") %>% #put back indicators in column
    arrange(country, indicator, year) %>%
    mutate(val_mod = ifelse(indicator == "deaths", -value, value), #create dual-axis
           # fill_color = ifelse(indicator == "deaths", glitr::orchid_bloom, glitr::electric_indigo)) #add colors to indicate flip axis
           fill_color = ifelse(indicator == "deaths", "#CF0B29", "#15A6AE")) #add colors to indicate flip axis
  
  
  #status
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
  
  
  df_plhiv <- df_unaids %>% 
    filter(indicator == "Number PLHIV",
           age == "All",
           sex == "All",
           pepfar == TRUE,
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
                            "Percent VLS on ART"),
           pepfar == TRUE) %>% 
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
    "PLHIV",       "on ART",     90L,
    "PLHIV",          "VLS",     86L,
    "Relative", "Known Status",     95L,
    "Relative",       "on ART",     95L,
    "Relative",          "VLS",     95L
  )
  
  
  df_95s <- df_95s %>%
    left_join(df_target,
              by = join_by(indicator, type)) %>%
    mutate(achieved = estimate >= target) %>%
    bind_rows(df_epi_pts) %>% 
    left_join(df_status,
              by = join_by(country)) %>% 
    mutate(fill_shape =  case_when(is.na(achieved) ~ NA_integer_,
                                   indicator == "Deaths" & achieved == TRUE ~ 25,
                                   indicator == "Deaths" & achieved == FALSE ~ 24,
                                   indicator == "Epi" ~ 22,
                                   TRUE ~ 21),
           fill_color = ifelse(achieved, "#15A6AE", "white"),
           indicator = factor(indicator, c("Known Status", "on ART", "VLS", "Epi", "Deaths"))) %>% 
    left_join(df_plhiv, join_by(country))
  
  df_viz <- df_95s %>% 
    filter(year == max(year),
           age == "All") %>% 
    mutate(achv_95s = case_when(type == "Relative" ~ achieved)) %>% 
    group_by(country) %>%
    mutate(count95s = sum(achv_95s, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(country = case_match(country,
                                "Democratic Republic of the Congo" ~ "DRC",
                                "Dominican Republic" ~ "DR",
                                "Trinidad and Tobago" ~ "T&T",
                                "Papua New Guinea" ~ "PNG",
                                .default = country),
           status = fct_na_value_to_level(status, level = ""))


# VIZ ---------------------------------------------------------------------

  df_viz %>% 
    ggplot(aes(indicator, fct_reorder(country, count95s, .desc = FALSE),
               fill = fill_color, shape = fill_shape)) +
    geom_point(color = "#15A6AE", stroke = 1.1, na.rm = TRUE) +
    facet_grid(status ~ fct_rev(type), scales = "free", space = "free", switch = "y") +
    scale_x_discrete(position = "top") +
    scale_fill_identity() +
    scale_shape_identity() +
    labs(x = NULL, y = NULL,
         title = "[TITLE TO BE SUPPLIED]",
         subtitle = "Comparing UNAIDS Target Achievement for the 95s (Relative and PLHIV Base)",
         caption = glue("Calculation: Epi Control = total new HIV infections falls below the total deaths from all causes among PLHIV, with total deaths declining
                        Note: Countries groups as to whether they were in epidemic control last year, were new, fell out, or have not yet attained.
                        {caption_src}")) + 
    si_style_nolines() +
    theme(axis.text.x = element_markdown(),
          strip.placement = "outside",
          strip.text = element_text(face = "bold", hjust = .5),
          panel.spacing = unit(.5, "lines"))
  
  si_preview()
  
  si_save("07_country_95s_status",
          path = "Images", device = "png")
  
  df_viz %>% 
    filter(type != "Epi") %>% 
    ggplot(aes(estimate, fct_reorder(country, count95s), color = achieved)) +
    geom_vline(data = df_target, aes(xintercept = target), linetype = "dotted") +
    geom_rect(data = df_target %>% mutate(xmax = 100, ymin = 0, ymax = Inf),
              aes(xmin = target, xmax = xmax,
                  ymin = ymin, ymax = ymax),
              fill = "#15A6AE", alpha = .2,
              inherit.aes = FALSE) +
    geom_linerange(aes(xmin = lower_bound, xmax = upper_bound), na.rm = TRUE) +
    geom_point(na.rm = TRUE) +
    facet_grid(~fct_rev(type) + indicator) +
    scale_x_continuous(limits = c(0, NA), position = "top") +
    scale_color_manual(values = c(slate, "#15A6AE")) +
    labs(x = NULL, y = NULL,
         title = "[TITLE TO BE SUPPLIED]",
         subtitle = "Comparing UNAIDS Target Achievement for the 95s (Relative and PLHIV Base)",
         caption = caption_src) +
    si_style_xgrid() +
    theme(strip.placement = "outside",
          strip.text = element_text(face = "bold", hjust = .5),
          legend.position = "none")
  
  si_save("07_country_95s_estimates",
          path = "Images", device = "png")

    
  