
library(tidyverse)

# UNAIDS full dataset with PEPFAR column ----------

unaids <- read.csv("UNAIDS_2024_Clean_Estimates - df_unaids_2024_release.csv")

# 183 total countries in the UNAIDS dataset
unaids %>%
  summarise(total_countries = n_distinct(country))


# identify PEPFAR only countries
pepfar_countries <- unaids %>%
  group_by(country) %>%
  arrange(country) %>%
  filter(row_number()==1) %>%
  filter(pepfar == TRUE) %>%
  pull(country)

# AGWY data - Karishma had to pull this separately ----------------
agyw_raw <- read.csv("incidence_15_24_unaids_2023 - incidence_15_24_unaids_2023.csv")

head(agyw)
dput(names(agyw_raw))

agyw <- agyw_raw %>%
  select("Time", "Region", "E_Count", "ISO3", 
         "Type", "e_cat", "Age", 
         "Sex", "E_Ind", "Value") %>%
  rename(country = "E_Count",
         indicator = "E_Ind",
         year = "Time")

# rename countries to match later
agyw$country <- replace(agyw$country, agyw$country == "Cote dIvoire", "Cote d'Ivoire")
agyw$country <- replace(agyw$country, agyw$country == "United Republic of Tanzania", "Tanzania")
agyw$country <- replace(agyw$country, agyw$country == "Myanmar", "Burma")
agyw$country <- replace(agyw$country, agyw$country == "Viet Nam", "Vietnam")
agyw$country <- replace(agyw$country, agyw$country == "Lao People Democratic Republic", "Laos")


# add pepfar to AGYW data
agyw <- agyw %>%
  mutate(pepfar = case_when(country %in% pepfar_countries ~ "PEPFAR",
                            TRUE ~ "Non-PEPFAR"))

agyw_pepfar <- agyw %>%
  filter(pepfar == "PEPFAR") %>%
  pull(country)

agyw_pepfar <- unique(agyw_pepfar)

# identify what countries are PEPFAR but do not have AGYW data
not_in_agyw <- setdiff(pepfar_countries, agyw_pepfar)


# 150 countries in the AGWY dataset
agyw %>%
  summarise(total_countries = n_distinct(country))


# Count the number of unique countries for each 'program' group
agyw %>%
  distinct(country, pepfar) %>%
  group_by(pepfar) %>%
  summarise(total_countries = n_distinct(country))

# df1 = Incidence (per 100k) only ---------------------
df1 <- agyw %>%
  filter(Sex %in% c("M", "F")) %>%
  filter(indicator %in% c("O- Incidence (15-24) (Per 1000) Female", "O- Incidence (15-24) (Per 1000) Male")) %>%
  select("year", "Region", "country", "indicator", "Age", "Sex", "Value", "pepfar")


# df2 - Pivot wider and add Female:Male Ratio ----------------

df2 <- df1 %>%
  select(!indicator) %>%
  pivot_wider(names_from = Sex, values_from = Value)

df2 <- df2 %>%
  rename(Male = "M", Female = "F") %>%
  mutate(fm_ratio = Female / Male) %>%
  mutate(fm_ratio_over3 = fm_ratio > 3) %>%
  mutate(fm_ratio_over1 = fm_ratio > 1)

# save this dataset
write.csv(df2, "AGYW_Incidence_Rates_and_Ratios_1990_2023.csv", row.names = F)


# df3 - just 2023 rates
df3 <- df2 %>%
  filter(year == 2023)

df3 %>%
  distinct(country, pepfar) %>%
  group_by(pepfar) %>%
  summarise(total_countries = n_distinct(country))

# df3_pepfar - filter to just PEPFAR countries ----------------

df3_pep <- df3 %>%
  filter(pepfar == "PEPFAR")

df3_pep$country <- reorder(df3_pep$country, df3_pep$fm_ratio)

# VISUAL 1 - Bar chart of 2023 Incidence Ratio --- ended up making this in Tableau --------------
df3_pep %>%
  ggplot(aes(x = country, y = fm_ratio)) + geom_col() + coord_flip() + 
  geom_hline(yintercept = 1, linetype = "dashed", color = "lightblue", size = 1) + 
  geom_hline(yintercept = 3, linetype = "dashed", color = "darkred", size = 1) +
  #facet_wrap(~Region) +
  theme_minimal()+
  xlab("") +  # Relabeling the x-axis
  ylab("Female to Male Incidence Ratio") +  # Relabeling the y-axis
  ggtitle("Young Women to Young Men Incidence Ratio in PEPFAR countries, 2023")


# df2_pepfar - PEPFAR countries ratios over time

df2_pep <- df2 %>%
  filter(pepfar == "PEPFAR")

# rename just for the viz
df2_pep$country <- replace(df2_pep$country, df2_pep$country == "Democratic Republic of the Congo", "DRC")

# sort by F:M ratio (for visuals 2 and 3)
df2_pep_2023 <- df2_pep %>%
  filter(year == 2023) %>%
  group_by(country) %>%
  arrange(country) %>%
  filter(row_number()==1) %>%
  arrange(desc(fm_ratio))

desc_fm_ratio <- df2_pep_2023$country
df2_pep$country <- factor(df2_pep$country, levels = desc_fm_ratio)

# VISUAL 2: Ratio over time, split by female/male -------------
df1_pep <- df1 %>%
  filter(pepfar == "PEPFAR")
df1_pep$country <- replace(df1_pep$country, df1_pep$country == "Democratic Republic of the Congo", "DRC")

df1_pep$country <- factor(df1_pep$country, levels = desc_fm_ratio)


df1_pep %>%
  filter(Region %in% c("Eastern and southern Africa", "Western and central Africa")) %>%
  ggplot(aes(x = year, y = Value, color = Sex)) + 
  geom_line(size = 1.5) +
  facet_wrap(~country, scales = "free_y") +  
  theme_minimal() +
  scale_color_manual(values = c("M" = "#287C6F", "F" = "#8980CB")) +
  labs(x = "", y = "Incidence") +
  theme(text = element_text(family = "Source Sans Pro"))  # Set default font



# VISUAL 3: visual of the ratio over time in all PEPFAR USAID countries ------------
df2_pep %>%
  filter(Region %in% c("Eastern and southern Africa", "Western and central Africa")) %>%
ggplot(aes(x = year, y = fm_ratio)) + 
  geom_line(size = 1.1) +
  facet_wrap(~country, scales = "free_y") +
  labs(x = "", y = "Female:Male Incidence Ratio", title = "Ratio of new infections in young women to young men, African PEPFAR countries") +
  theme_minimal() +
  theme(text = element_text(family = "Source Sans Pro"))
