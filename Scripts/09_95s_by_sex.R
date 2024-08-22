# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  95s by sex
# REF ID:   aac59804 
# LICENSE:  MIT
# DATE:     2024-08-22
# UPDATED:

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
library(mindthegap)

load_secrets()

# REF ID for plots
ref_id <- "aac59804"

# Functions  


# LOAD DATA ============================================================================  

#Recent Infections


#PEPFAR only
df_pepfar <- 
  pull_unaids(data_type = "HIV Test & Treat", pepfar_only = TRUE)


df_viz <- df_pepfar %>% 
  filter(year == max(year),
         indicator %in% c("Percent Known Status of PLHIV",
                          "Percent on ART of PLHIV",
                          "Percent VLS of PLHIV"),
         age == "15+",
         sex != "All",
         # stat == "est"
  ) %>% 
  select(year, country, region, sex, age, indicator, estimate) 

df_viz <- df_viz %>% 
  mutate(met_goal = case_when(indicator == "Percent Known Status of PLHIV" & estimate >= 95 ~ TRUE,
                              indicator == "Percent on ART of PLHIV" & estimate >= 90 ~ TRUE,
                              indicator == "Percent VLS of PLHIV" & estimate >= 86 ~ TRUE,
                              TRUE ~ FALSE)) %>% 
 # mutate(flag = ifelse(estimate >= 95, 1, 0.5)) %>% 
  dplyr::mutate(stroke_color = ifelse(sex == "Female", hw_lavender_haze, hw_hunter),
                viz_group = glue::glue("{indicator}{country}")) %>% 
  mutate(country = case_when(country == "Democratic Republic of the Congo" ~ "DRC",
                             country == "Dominican Republic" ~ "DR", 
                             TRUE ~ country)) %>% 
  mutate(fill_color = ifelse(met_goal == TRUE, stroke_color, "white"))

# Create a separate dataframe for goal thresholds
goal_thresholds <- tibble(
  indicator = c("Percent Known Status of PLHIV", "Percent on ART of PLHIV", "Percent VLS of PLHIV"),
  goal = c(95, 90, 86)
)
  

# VIZ -------------------------------------------------------------------------------------

df_viz %>% 
  filter(str_detect(region, "Africa"),
         !is.na(estimate)) %>% 
  ggplot(aes(x = estimate,
             y = reorder(country, ifelse(sex == "Male" & indicator == "Percent Known Status of PLHIV", estimate, NA), na.rm = TRUE)))+  # Reorder by Male estimates
  geom_vline(data = goal_thresholds, aes(xintercept = goal), linetype = "dashed") + 
  geom_text(data = goal_thresholds, aes(x = goal, y = Inf, label = paste("Goal:", goal, "%")), 
            vjust = -0.5, hjust = 1.2, color = "black", size = 3) + 
  geom_line(aes(group = viz_group), linewidth = 2, alpha = 1, color = "white", na.rm = TRUE) +
  geom_line(aes(group = viz_group),linewidth = 2, alpha = .6, color = grey30k, na.rm = TRUE) +
  geom_errorbar(aes(xmin = estimate, xmax = estimate), linewidth = 1, color = "white", na.rm = TRUE) +
  geom_point(size = 3, color = "white", na.rm = TRUE) +
  geom_point(aes(color = stroke_color, fill = fill_color), shape = 21, size = 2, stroke = 1.5) +
  facet_wrap(~indicator, nrow = 1, ncol = 3) +
  scale_x_continuous(labels=function(x) paste0(x,"%"),breaks = seq(0, 100, by = 25)) +
  scale_color_identity() +
  scale_fill_identity() +
  scale_alpha_identity() +
  si_style_xgrid() +
  theme(plot.title = element_markdown()) +
  labs(x = NULL, y= NULL,
       title = glue("GREATER GAPS TO ACHIEVING THE TESTING AND TREATMENT GOALS AMONG <span style= 'color:#419164;'>ADULT MEN (15+)</span> COMPARED TO <br><span style= 'color:#876EC4;'>ADULT WOMEN</span>"),
       subtitle = "PEPFAR Countries in Sub-Saharan Africa",
       caption = glue("{source_note} | Ref ID: {ref_id}"))
  
si_save("Graphics/09_95s_SSA_REVISED_scale.svg")
si_save("Images/09_95s_SSA_REVISED_scale.png")


df_viz %>% 
  filter(!str_detect(region, "Africa"),
         !is.na(estimate)) %>% 
  ggplot(aes(x = estimate,
             y = reorder(country, ifelse(sex == "Male" & indicator == "Percent Known Status of PLHIV", estimate, NA), na.rm = TRUE)))+  # Reorder by Male estimates
  geom_vline(data = goal_thresholds, aes(xintercept = goal), linetype = "dashed") + 
  geom_text(data = goal_thresholds, aes(x = goal, y = Inf, label = paste("Goal:", goal, "%")), 
            vjust = -0.5, hjust = 1.2, color = "black", size = 3) + 
  geom_line(aes(group = viz_group), linewidth = 2, alpha = 1, color = "white", na.rm = TRUE) +
  geom_line(aes(group = viz_group),linewidth = 2, alpha = .6, color = grey30k, na.rm = TRUE) +
  geom_errorbar(aes(xmin = estimate, xmax = estimate), linewidth = 1, color = "white", na.rm = TRUE) +
  geom_point(size = 3, color = "white", na.rm = TRUE) +
  geom_point(aes(color = stroke_color, fill = fill_color), shape = 21, size = 2, stroke = 1.5) +
  facet_wrap(~indicator, nrow = 1, ncol = 3) +
  scale_x_continuous(labels=function(x) paste0(x,"%"), limits = c(20, 100)) +
  scale_color_identity() +
  scale_fill_identity() +
  scale_alpha_identity() +
  si_style_xgrid() +
  theme(plot.title = element_markdown()) +
  labs(x = NULL, y= NULL,
       title =glue("<span style= 'color:#419164;'>ADULT MEN (15+)</span> LAG BEHIND ON THE 1ST 95 IN AFRICAN PEPFAR COUNTRIES COMPARED TO <span style= 'color:#876EC4;'>ADULT WOMEN</span>, <br>BUT ARE CLOSING THE GAP IN ASIA AND LATIN AMERICA"),
       subtitle = "Non-African PEPFAR Countries",
       caption = glue("{source_note} | Ref ID: {ref_id}"))


si_save("Graphics/09_95s_non_AFR_REVISED.svg")
si_save("Images/09_95s_non_AFR_REVISED.png")

# FUNCTIONS ------------------------------------------------------------------------

viz_95s <- function(df, pepfar_region) {
  
  viz <- df %>% 
    filter(str_detect(region, pepfar_region),
           !is.na(estimate)) %>% 
    ggplot(aes(x = estimate,
               y = reorder(country, ifelse(sex == "Male" & indicator == "Percent Known Status of PLHIV", estimate, NA), na.rm = TRUE),  # Reorder by Female estimates
               color = fill_color,
               alpha = flag,
               group = viz_group)) +
    # geom_vline(xintercept = 0, color = "#D3D3D3") +
    geom_vline(xintercept = 95, linetype = "dashed") + 
    # geom_hline(yintercept = ref_psnu,
    #            linewidth = .8, linetype = "dashed", color = usaid_darkgrey) +
    geom_line(linewidth = 2, alpha = 1, color = "white", na.rm = TRUE) +
    geom_line(linewidth = 2, alpha = .6, color = grey30k, na.rm = TRUE) +
    geom_errorbar(aes(xmin = estimate, xmax = estimate), linewidth = 1, color = "white", na.rm = TRUE) +
    geom_point(size = 4, color = "white", alpha = 1, na.rm = TRUE) +
    geom_point(size = 4, na.rm = TRUE) +
    facet_wrap(~indicator, nrow = 1, ncol = 3) +
    scale_x_continuous(labels=function(x) paste0(x,"%"), limits = c(45, 100)) +
    # facet_grid(flag ~ indicator) +
    scale_color_identity() +
    scale_alpha_identity() +
    si_style_xgrid() +
    labs(x = NULL, y= NULL,
         title = "Across the board, adult men (15+) have bigger gaps to achieving the 3 95's than adult women" %>% toupper(),
         subtitle = glue("PEPFAR countries in {pepfar_region}"))
  
  return(viz)
  
  
}

