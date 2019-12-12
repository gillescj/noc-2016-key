# packages -----
library(tidyverse)
library(lubridate)
library(stringr)
library(rvest)

# DATA SETUP ------

# url link no longer works, used wayback machine to scrape html and saved to "nocs_2016.csv"

# url = "http://noc.esdc.gc.ca/English/noc/QuickSearch.aspx?ver=16&val65=*"
# download.file(url, destfile = "scrapedpage1.html", quiet=TRUE)
# nocs_2016_page <- read_html("scrapedpage1.html")

# nocs_2016 <- nocs_2016_page %>%
#   html_nodes(".NoBulletWithLessPadding a") %>% 
#   html_text() %>% 
#   tibble() %>% 
#   separate(".", into=c("code", "desc"), sep=4)

# rm(url,nocs_2016_page)

nocs_2016 <- read_csv("nocs_2016.csv") %>% 
  select(-X1)

# Stats Canada Employment Income by NOC, Census 2016
# from https://www12.statcan.gc.ca/census-recensement/2016/dp-pd/dt-td/Rp-eng.cfm?TABID=2&Lang=E&APATH=3&DETAIL=0&DIM=0&FL=A&FREE=0&GC=0&GID=1325190&GK=0&GRP=1&PID=112125&PRID=10&PTYPE=109445&S=0&SHOWALL=0&SUB=0&Temporal=2017&THEME=124&VID=0&VNAMEE=&VNAMEF=&D1=0&D2=0&D3=0&D4=0&D5=0&D6=0

# National Income
noc_income_nat <- read_csv("StatsCan NOC Employment Income.csv", skip = 8) %>% 
  select(-X5) %>% 
  rename("Occupation" = colnames(.[1]), "Total Employed (Canada 2015)" = colnames(.[2]), 
         "Median Income (Canada 2015)" = colnames(.[3]), "Average Income (Canada 2015)" = colnames(.[4])) %>% 
  na.omit() %>% 
  mutate(Code = word(Occupation, 1), "Occupation Name" = word(Occupation, start = 2, end = -1))

# Provincial Income
noc_income_on <- read_csv("StatsCan NOC Ontario Employment Income.csv", skip = 8) %>% 
  select(-X5) %>% 
  rename("Occupation" = colnames(.[1]), "Total Employed (Ontario 2015)" = colnames(.[2]), 
         "Median Income (Ontario 2015)" = colnames(.[3]), "Average Income (Ontario 2015)" = colnames(.[4])) %>% 
  na.omit() %>% 
  mutate(Code = word(Occupation, 1), "Occupation Name" = word(Occupation, start = 2, end = -1))

# National and Provincial Income
noc_income <- merge(noc_income_nat, noc_income_on, by = c("Occupation", "Occupation Name", "Code"))

# Skill Type
skill_type_income <- noc_income %>% 
  filter(nchar(Code) == 1) %>% 
  select(-`Occupation`) %>% 
  rename(`Skill Type` = `Occupation Name`)

skill_type <- skill_type_income %>% 
  select(-`Total Employed (Canada 2015)`, -`Median Income (Canada 2015)`, -`Average Income (Canada 2015)`,
         -`Total Employed (Ontario 2015)`, -`Median Income (Ontario 2015)`, -`Average Income (Ontario 2015)`)
  
# Major Group
major_group_income <- noc_income %>% 
  filter(nchar(Code) == 2 | str_detect(Code, "-")) %>% 
  select(-`Occupation`) %>% 
  rename(`Major Group` = `Occupation Name`)

major_group <- major_group_income %>% 
  select(-`Total Employed (Canada 2015)`, -`Median Income (Canada 2015)`, -`Average Income (Canada 2015)`,
         -`Total Employed (Ontario 2015)`, -`Median Income (Ontario 2015)`, -`Average Income (Ontario 2015)`)

# Minor Group
minor_group_income <- noc_income %>% 
  filter(nchar(Code) == 3) %>% 
  select(-`Occupation`) %>% 
  rename(`Minor Group` = `Occupation Name`)

minor_group <- minor_group_income %>% 
  select(-`Total Employed (Canada 2015)`, -`Median Income (Canada 2015)`, -`Average Income (Canada 2015)`,
         -`Total Employed (Ontario 2015)`, -`Median Income (Ontario 2015)`, -`Average Income (Ontario 2015)`)


# filter for only the unit groups (500 total)
unit_group_income <- noc_income %>% 
  filter(!grepl("\\D", Code)) %>% 
  filter(nchar(Code) == 4) %>% 
  select(-`Occupation`)

unit_group <- unit_group_income %>% 
  select(-`Total Employed (Canada 2015)`, -`Median Income (Canada 2015)`, -`Average Income (Canada 2015)`,
         -`Total Employed (Ontario 2015)`, -`Median Income (Ontario 2015)`, -`Average Income (Ontario 2015)`)


# dictionary for noc_code_to_skill_level
skill_level_dictionary <- data.frame(
  number = c(0:9), letter = c("A", "A", "B", "B", "C", "C", "D", "D", "Other", "Other"))

# takes a 4 digit integer NOC Code
# returns a string/character matching the skill level of that NOC Code 
noc_code_to_skill_level <- function(noc_code){
  letter <- ifelse(as.numeric(noc_code) < 1000, "A",
         as.character(skill_level_dictionary[as.numeric(str_sub(noc_code, 2, 2)) + 1, 2]))
  return(letter)
}
  
# Occupational Outlook
# from http://occupations.esdc.gc.ca/sppc-cops/content.jsp?cid=occupationdatasearch&lang=en

noc_outlook <- read_csv("Occupational Outlook by NOC.csv") %>% 
  select(1,2,4) %>% 
  filter(Future_Labour_Market_Conditions != "N/A") %>% 
  rename("Future Labour Market Conditions (National 2013-2026)" = Future_Labour_Market_Conditions)

noc_outlook <- noc_outlook %>% 
  mutate(Code = str_sub(Code, 2, -1))


# MASTER NOC 2016 FILE -------

# Add the Skill Types
master_noc_2016 <- nocs_2016 %>% 
  mutate(Code = str_sub(code, 1, 1)) %>% 
  merge(skill_type, by = "Code") %>% 
  mutate(`Skill Type` = paste0(Code, " ", `Skill Type`)) %>% 
  select(-Code) %>% 
  rename("noc_code" = "code") %>% 
  rename("Occupation Name (Unit Group)" = "desc")


# Add the Major group names
master_noc_2016 <- master_noc_2016 %>% 
  mutate("Code" =  ifelse(as.numeric(str_sub(noc_code, 1, 2)) %in% c(1,2,3,4,5), "01-05", 
                          ifelse(as.numeric(str_sub(noc_code, 1, 2)) %in% c(7,8,9), "07-09",
                            str_sub(noc_code, 1, 2)))) %>%
  merge(major_group, by = "Code") %>% 
  mutate(`Major Group` = paste0(Code, " ", `Major Group`)) %>% 
  select(-Code)

# Add the Minor Group Names
master_noc_2016 <- master_noc_2016 %>% 
  mutate("Code" = str_sub(noc_code, 1, 3)) %>% 
  merge(minor_group, by = "Code") %>% 
  mutate(`Minor Group` = paste0(Code, " ", `Minor Group`)) %>% 
  select(-Code)

# Add the Skill Level
master_noc_2016 <- master_noc_2016 %>% 
  mutate("Skill Level" = noc_code_to_skill_level(as.numeric(noc_code)))

# Add Stats Canada National + Ontario Income
master_noc_2016 <- master_noc_2016 %>% 
  merge(unit_group_income, by.x = "noc_code", by.y = "Code") %>% 
  select(-`Occupation Name`)

# Add 
master_noc_2016 <- master_noc_2016 %>% 
  # Add Difference between Ontario and Canada Average Income
  mutate(`Average Income Difference` = `Average Income (Ontario 2015)` - `Average Income (Canada 2015)`) %>% 
  # Add % of how large that difference is for the Occupation's Canadian income
  mutate(`Average Income Canadian Ratio` = `Average Income Difference`/(`Average Income (Canada 2015)`))


# Add Occupational Outlook
master_noc_2016 <- master_noc_2016 %>% 
  merge(noc_outlook, by.x = "noc_code", by.y = "Code", all.x = TRUE) %>% 
  select(-Occupation_Name)

# MINI ANALYSIS -------

# Plot Difference
master_noc_2016 %>% 
  filter(abs(`Average Income Difference`) > 10000) %>% 
  ggplot(aes(x = noc_code, y = `Average Income Difference`, fill = (`Average Income Difference` > 0))) +
  geom_col() +
  facet_wrap(~`Skill Type`, scales = "free") +
  geom_text(aes(label = `Skill Level`)) +
  theme(axis.text.x = element_text(angle = 90), legend.position = "none") +
  coord_flip()

# Plot %
master_noc_2016 %>% 
  filter(abs(`Average Income Canadian Ratio`) > 0.1) %>% 
  ggplot(aes(x = noc_code, y = `Average Income Canadian Ratio`, fill = (`Average Income Canadian Ratio` > 0))) +
  geom_col() +
  facet_wrap(~`Skill Type`, scales = "free") +
  geom_text(aes(label = `Skill Level`)) +
  theme(axis.text.x = element_text(angle = 90), legend.position = "none") +
  coord_flip()

# OUTPUT -------

master_noc_2016 %>%  write_csv("Master - NOC-Key.csv")









