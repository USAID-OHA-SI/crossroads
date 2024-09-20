# PROJECT: Epi Control Progress
# PURPOSE: Munge and Analysis of UNAIDS Data
# AUTHOR:  Lemlem Baraki | SI
# REF ID:  db07af08
# LICENSE: MIT
# DATE:   2024-08-26
# NOTES:   

# LOCALS & SETUP ============================================================================

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
  library(gt)
  library(gtExtras)
  library(mindthegap)
    
  # SI specific paths/functions  
    load_secrets()
  
  # REF ID for plots
    ref_id <- "db07af08"
    
  # Functions  
  

# LOAD DATA ============================================================================  

    #Pull the relative base: 95-95-95 goals
    df_rel <- pull_unaids(data_type = "HIV Test & Treat", pepfar_only = FALSE) %>% 
      dplyr::filter(indic_type == "Percent",
                    #country == "Global"
                    ) 
    
    #Pull the PLHIV base: 95-90-86 goals (testing & treatment cascade)
    df_tt <- pull_unaids(data_type = "HIV Test & Treat", pepfar_only = TRUE) %>% 
      dplyr::filter(indic_type == "Percent")

# MUNGE ============================================================================
  
    ##Progress toward 90-90-90 
    df_rel_lim <- df_rel %>% 
      filter(year == 2023,
             indicator %in% c("Percent Known Status of PLHIV",
                              "Percent on ART with Known Status",
                              "Percent VLS on ART"), #using rel base indicators
             age == "All",
             sex == "All") %>% 
      select(year, country, indicator, estimate, lower_bound, upper_bound, achv_95_relative, pepfar)
    
    #Treatment over time 
    df_art <- df_tt %>% 
      filter(
        indicator %in% c("Percent on ART of PLHIV"), #Using the PLHIV base indicators
        age == "All", 
        sex == "All",
        pepfar == TRUE) 
    
    df_art_lim <- df_art %>% 
      arrange(desc(year)) %>% 
      select(year, region, country, indicator, estimate, lower_bound, upper_bound) %>% 
      mutate(share = estimate/100, 
             share_low = lower_bound/100, share_up = upper_bound/100,
             art_flag = share <= 0.90
      ) %>% 
      arrange(desc(share))
    
    #VLS over time
    df_vls <- 
      df_tt %>% 
      filter(
        indicator %in% c("Percent VLS of PLHIV"), #Using the PLHIV base indicators
        age == "All", sex == "All",
        pepfar == TRUE) 
    
    df_vls_lim <- df_vls %>% 
      arrange(desc(year)) %>% 
      select(year, region, country, indicator, estimate, lower_bound, upper_bound) %>% 
      mutate(share = estimate/100, 
             share_low = lower_bound/100, share_up = upper_bound/100,
             vls_flag = share <= 0.86) %>% 
      arrange(desc(share)) 
  
# VIZ ============================================================================

    #Table of progress towards 95s and 90s
    goal <- 90
    
    df_viz <- df_rel_lim %>% 
      filter(pepfar == TRUE) %>% 
      mutate(
        goal_rate = goal, #use 90 as the goal metric for each indicator
        achieved = estimate >= goal_rate)
        
      #Which OUs have reached 90-90-90 or above?
      ou_list <- df_viz %>% 
        group_by(country) %>% 
        summarise(all_achieved = all(achieved), .groups = 'drop') %>% 
        filter(all_achieved) %>% 
        pull(country) #10 OUs 
    
      df_viz %>% 
        filter(country %in% ou_list) %>% 
        select(country, indicator, estimate, achv_95_relative) %>% 
        pivot_wider(names_from = indicator,
                    values_from = "estimate",
                    names_glue = "{indicator %>% stringr::str_extract_all('PLHIV|with|VLS') %>% tolower}"
        ) %>% 
        arrange(desc(achv_95_relative)#, desc(with), desc(vls)
                )%>% 
        gt() %>%
        cols_hide(c(achv_95_relative)) %>% 
        fmt_percent(columns = c(plhiv, with, vls),
                    decimals = 0, scale_values = FALSE) %>%
        cols_label(plhiv = "Known Status",
                   with = "On ART",
                   vls = "VLS") %>% 
        gtExtras::gt_theme_nytimes() %>% 
        tab_source_note(source_note = gt::md(glue(source_note))) %>% 
        tab_options(source_notes.font.size = px(8),
                    data_row.padding = px(1),
                    table.font.size = px(12)) %>% 
        tab_style(
          style = list(
            cell_fill(color = glitr::orchid_bloom),
            cell_text(color = "white")
          ),
          locations = cells_body(
            columns = c(plhiv,with, vls),
            rows = 1:7))%>% 
        #tab_spanner(label = "Goal: 95, 90, 86") %>% 
        tab_header(#title = "FAST TRACK TARGETS",
          title = md("Ten countries have reached the 90s,
                               with seven reaching the <span style= 'color:#E14BA1;'>95s</span>")) %>% 
        gtsave("Images/Progress_90s_table.png")
    
    #Bar chart of global performance (rel base indicators) with CI's 
    
    df_rel_lim %>% 
      filter(country == "Global") %>% 
      mutate(share = estimate/100, share_low = lower_bound/100, share_up = upper_bound/100) %>% 
      slice_max(share, n = 3) %>% 
      mutate(benchmark = .90) %>% 
      ggplot(aes(x = indicator, y = share))+ 
      geom_col(aes(y = benchmark), fill = grey20k, alpha = 0.5) + 
      geom_col(aes(fill = indicator), fill = glitr::orchid_bloom, alpha = 0.9) + 
      geom_hline(yintercept = .9, color = grey40k, linetype = "dotted") + 
      geom_errorbar(aes(ymin = share_low, ymax = share_up),
                    linetype = "dashed", width = 0.1, na.rm = TRUE) + 
      #geom_text(aes(label = percent(share,1)),
      #vjust = -1.5,
      #         hjust = -.5,
      #        family = "Source Sans Pro"
      #) + 
      facet_wrap(~indicator, ncol = 3,
                 scale = "free_x" #fix bar spacing 
      ) +  
      #scale_fill_si(palette = "orchid_bloom_d", discrete = T) + 
      si_style_ygrid(facet_space = 0.5) + 
      scale_y_continuous(labels = percent, limits = c(0,1)) + 
      theme(axis.text.x = element_blank(),
            #axis.text.y = element_blank(), #drop the y-axis text 
            legend.position = "none") + 
      labs(x = NULL, y = NULL,
           title = str_to_upper("Globally, countries fell short of two of the 90-90-90 targets in 2023"),
           caption = glue("{source_note}"))
    
    #PEPFAR countries performance (% on ART) with CI's
    
      #How many OUs behind on ART (goal = 90%)
    df_art_lim %>% 
      filter(art_flag == TRUE, 
             year == 2023) %>% pull(country) #42 of 55
    
    df_art_lim %>% 
      filter(year == 2023, !is.na(share)) %>% #exclude countries with missing data
      mutate(country = case_when(country == "Democratic Republic of the Congo" ~ "DRC",
                                 country == "Papua New Guinea" ~ "PNG", 
                                 TRUE ~ country ),
             country = as.factor(country),
             region_group = ifelse(str_detect(region, "Africa"), "African PEPFAR Countries","Non-African PEPFAR Countries"),
             country = fct_reorder(country, share),
             country_color = ifelse(art_flag == "FALSE", glitr::slate, glitr::electric_indigo)
      ) %>%
      ggplot(aes(y = country)) + 
      geom_segment(aes(x = share_low, xend = share_up, yend = country, color = glitr::whisper), size = 2) + #lollipop line
      geom_point(aes(x = share, color = country_color),size = 3) + #lollipop points
      facet_wrap(~region_group, scale = "free_y") + 
      #geom_vline(xintercept = 0.86, linetype = "dashed", color = slate)+
      scale_x_continuous(labels = percent, limits = c(0,1)) + 
      scale_color_identity()+ 
      si_style_xgrid()+
      theme(plot.subtitle = element_markdown()) +
      labs(x = NULL, y = NULL,
           subtitle = glue("<span style= 'color:{slate};'>Achieved 90 goal</span> | <span style= 'color:{electric_indigo};'>Did not Achieve"),
           caption = glue("{source_note}
                           Note: Missing data for Benin, Cameroon, Colombia, Trinidad & Tobago, and Ukraine"))
    
    
    #PEPFAR countries performance (% VLS) with CI's
    
      #How many OUs are behind on %VLS (goal = 86%)
      df_vls_lim %>% 
        filter(vls_flag == TRUE, 
               year == 2023) %>% pull(country) #35 of 55 
      
      
      df_vls_lim %>% 
        filter(year == 2023, !is.na(share)) %>% #exclude countries with missing data
        mutate(country = case_when(country == "Democratic Republic of the Congo" ~ "DRC",
                                   country == "Papua New Guinea" ~ "PNG", 
                                   TRUE ~ country ),
               country = as.factor(country),
               region_group = ifelse(str_detect(region, "Africa"), "African PEPFAR Countries","Non-African PEPFAR Countries"),
               country = fct_reorder(country, share),
               country_color = ifelse(vls_flag == "FALSE", glitr::slate, glitr::electric_indigo)
        ) %>%
        ggplot(aes(y = country)) + 
        #geom_errorbar(aes(xmin = share_low, xmax = share_up, color = glitr::whisper), width = 0.5) + 
        geom_segment(aes(x = share_low, xend = share_up, yend = country, color = glitr::whisper), size = 2) + #lollipop line
        geom_point(aes(x = share, color = country_color),size = 3) + #lollipop points
        facet_wrap(~region_group, scale = "free_y") + 
        #geom_vline(xintercept = 0.86, linetype = "dashed", color = slate)+
        scale_x_continuous(labels = percent, limits = c(0,1)) + 
        scale_color_identity()+ 
        si_style_xgrid()+
        theme(plot.subtitle = element_markdown()) +
        labs(x = NULL, y = NULL,
             #title = glue("35 COUNTRIES FELL SHORT OF THE 3RD 95, 
             #            86% OF PLHIV IS <span style= 'color:#876EC4;'>VIRALLY SUPRESSED</span> IN 2023"),
             subtitle = glue("<span style= 'color:{slate};'>Achieved 86 goal</span> | <span style= 'color:{electric_indigo};'>Did not Achieve"),
             caption = glue("{source_note}
                          Note: Missing data for Angola, Cameroon, Colombia, Mali, Peru, Philippines,Sierra Leone, South Sudan, Trinidad & Tobago, and Ukraine"))
      
    

# SPINDOWN ============================================================================

