# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  VMMC coverage from DMPPT
# REF ID:   f7e9abc8 
# LICENSE:  MIT
# DATE:     2024-08-19
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
library(readxl)

# GLOBAL VARIABLES --------------------------------------------------------
  
  # SI specific paths/functions  
    load_secrets()

  # Grab metadata
    data_folder <- "Data" 
  
  ref_id <- "f7e9abc8"

# IMPORT AND PROCESS INDIVIDUAL FILES -------------------------------------------
  
  #If re-running, please use the tidy data from Dataout pulled in next section (line 89)
  
  files <- data_folder %>% 
    list.files(full.names = TRUE) 
  
  df <- files[1] %>% 
    read_excel(sheet = "Coverage")
  
  df <- files[1] %>% 
    read_excel(sheet = "Coverage", n_max = 1, col_names = FALSE) %>% unlist()


# ITERATE ----------------------------------------------------------------------


# Assuming `files` is a vector of file paths
process_file <- function(file) {
  # Read in the first row for column names
  df <- file %>% 
    read_excel(sheet = "Coverage")
  
  df_tidy <- df %>% 
    select(1:3, starts_with("End of 2023")) %>% 
    mutate(year = 2023)
  
  # Step 1: Extract the first row to use as column names
  new_colnames <- df_tidy %>%
    slice(1) %>%
    unlist() # Convert to a character vector
  
  # Step 2: Assign new column names and remove the first row
  df_tidy <- df_tidy %>%
    slice(-1) %>%  # Remove the first row
    set_names(new_colnames) # Set the first row as column names
  
  # Step 3: Tidy the dataframe
  df_final <- df_tidy %>% 
    rename(year = `2023`) %>% 
    pivot_longer(cols = c(`EIMC`:`15-49`), names_to = "age", values_to = "value") %>% 
    mutate(value = as.numeric(value)) %>% 
    filter(age == "15-49")
  
  return(df_final)
}

# Use purrr's map function to iterate over all files
all_data <- files %>% 
  map_dfr(process_file)  # map_dfr combines the results into one dataframe

write_csv(all_data, "Dataout/all_vmmc_tidy.csv")

# MUNGE ------------------------------------------------------------------------------

all_data <- read_csv("Dataout/all_vmmc_tidy.csv")
  
df_viz <- all_data %>% 
  filter(SNU %in% c("Botswana", "Eswatini", "Kenya", "Lesotho", "Malawi", "Mozambique", "Namibia", "Rwanda", "South Africa",
                    "United Republic of Tanzania",
                    "Uganda", "Zambia", "Zimbabwe")) %>% 
  mutate(goal = .9) %>% 
  mutate(flag = ifelse(value >= goal, hw_viking, hw_midnight_blue)) %>% 
  mutate(SNU = case_when(SNU == "United Republic of Tanzania" ~ "Tanzania",
                         TRUE ~ SNU))

df_viz %>% 
  ggplot(aes(x = fct_reorder(SNU, value), y = value, fill = flag)) +
  geom_col() +
  coord_flip() +
  geom_hline(yintercept = .9, linetype = "dashed") +
  geom_text(aes(y = value, label = percent(value, 1), family = "Source Sans Pro",
                hjust = -0.4)) +
  scale_fill_identity() +
  si_style_xgrid() +
  scale_y_continuous(labels = percent, limits = c(0,1)) +
  labs(x = NULL, y = NULL,
       title = str_wrap("VMMC Program Coverage (15-49) vary across the 13 priority countries, with only Kenya exceeding the targeted 90%", width = 90) %>% toupper(),
       subtitle = "Modelled estimates of national VMMC coverage by end of 2023",
       caption = glue("Source: VMMC 3MC Decision Maker's Program Planning Toolkit, Version 2, 2024 version, data extracted on August 19, 2024 4 (http://www.vmmcipt.org/2024_3MC/).
                      Ref id: {ref_id}"))
 
si_save("Images/10_vmmc.png")
