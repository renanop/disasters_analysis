################## NOTES ####################
# gdp_cap is on thousand reais of year 2010 #
#                                           #
# self made gdp per capita data using the   #
# GDP deflator, the state GDPs and pop.     # 
# starts at 2012.                           #




library(tidyverse)
library(readxl)

#setwd('C:/Users/renan/Desktop/ic_desastres')

setwd('C:/Users/renan/Desktop/ic_desastres')
#general data
checkpoint <- read_csv('df_checkpoint1.csv')
gdp_cap_wip <- read.csv2('gdp_per_capita.csv')
gdp_def <- read.csv2('gdp_deflator_index.csv')
state_gdp_2012_2015 <- read.csv2('state_gdps_2012_2015.csv')
ipca <- read.csv2('ipca_anual.csv')
deaths_sanitation <- read.csv2('mortes_por_saneamento_inadequado.csv', skip = 4) #also bad hygiene and bad water
state_pop <- read.csv('G:/Meu Drive/ic_desastres/mensal/tidy_state_pop.csv')

#data from 4 aspects of adequate living conditions. Those are all %.

basic_water <- read.csv2('basic_water_access.csv')
garbage_collection <- read.csv2('garbage_collection.csv')
sew_or_sep <- read.csv2('sewerage_or_septic_access.csv')
max_2_per_dorm <- read.csv2('max_2_per_dorm.csv')

colnames(checkpoint)[3] <- 'code'
#renaming wrong name GDP to gdp_anual_growth
colnames(checkpoint)[20] <- 'gdp_anual_growth'

#wrangling adequate living conditions data
basic_water <- basic_water %>%
  gather(year, basic_water_access, starts_with('X')) %>%
  mutate(year = as.numeric(str_replace(year,'X','')))

garbage_collection <- garbage_collection %>%
  gather(year, garbage_collection_access, starts_with('X')) %>%
  mutate(year = as.numeric(str_replace(year,'X','')))

sew_or_sep <- sew_or_sep %>%
  gather(year, sewerage_or_tank_access, starts_with('X')) %>%
  mutate(year = as.numeric(str_replace(year,'X','')))

max_2_per_dorm <- max_2_per_dorm %>%
  gather(year, max_2_per_dorm_percent, starts_with('X')) %>%
  mutate(year = as.numeric(str_replace(year,'X','')))


#wrangling deaths by poor sanitation data
deaths_sanitation <- deaths_sanitation %>%
  select(1:10) %>%
  gather(year, poor_sanitation_deaths_per_100thousand, starts_with('X')) %>%
  mutate(year = as.numeric(str_replace(year,'X','')))

#wrangling gdp_cap
gdp_cap_wip <- gdp_cap_wip %>% 
  gather(year,gdp_cap, starts_with('X')) %>%
  mutate(year = as.numeric(str_replace(year,'X','')))%>%
  select(-state)

#cleaning undesirable collumn at gdp_def:
gdp_def <- gdp_def %>% 
  select(year, GDP_deflator_index)

#wrangling state gdps
state_gdp_2012_2015 <- state_gdp_2012_2015 %>%
  gather(year, gdp, starts_with('X')) %>%
  mutate(year = as.numeric(str_replace(year,'X','')))

#cleaning weird excel stuff in ipca, and ajusting it for an index

ipca <- ipca[-nrow(ipca),]
ipca <- ipca %>%
  select(1:2) %>%
  filter(year >= 2010 & year <= 2015)
colnames(ipca)[2] <- 'ipca_index'
ipca[['ipca_index']] <- ipca[['ipca_index']]+1
ipca[['ipca_index']][[1]] <- 1
replacement <- cumprod(ipca[['ipca_index']])
ipca[['ipca_index']] <- replacement

#MForgot to attach the checkpoint last time. Be aware, next code uses attached data. Do not mess up.
attach(checkpoint)

#merging new data frames with the original one
checkpoint <- merge(checkpoint, gdp_cap_wip, all.x = TRUE)
checkpoint <- merge(checkpoint, gdp_def, all.x = TRUE)
checkpoint <- merge(checkpoint, state_gdp_2012_2015, all.x = TRUE)
checkpoint <- merge(checkpoint, ipca, all.x = TRUE)
checkpoint <- merge(checkpoint, deaths_sanitation, all.x = TRUE)
checkpoint <- merge(checkpoint, basic_water, all.x = TRUE)
checkpoint <- merge(checkpoint, garbage_collection, all.x = TRUE)
checkpoint <- merge(checkpoint, sew_or_sep, all.x = TRUE)
checkpoint <- merge(checkpoint, max_2_per_dorm, all.x = TRUE)

#subsetting for the lines that miss gdp per capita data and adding new data using the GDP, GDP deflator and population
subset_for_new_gdp_cap <- checkpoint %>% filter(year >= 2012)
gdp_cap_transformation <- mutate(subset_for_new_gdp_cap, gdp_cap = gdp*100/(population*GDP_deflator_index))
checkpoint[153:210,] <- gdp_cap_transformation

checkpoint <- select(checkpoint, -sanitation_percentage)

checkpoint <- select(checkpoint, year, end_year, code, disaster_type:insured_losses_000USD, 
                     magnitude_scale, magnitude_value, population, unemployment:gdp_cap, 
                     poor_sanitation_deaths_per_100thousand:max_2_per_dorm_percent, everything())

#finally

#write_csv(checkpoint, path = 'final_for_now.csv')

#Making a gdp dataset:
colnames(state_pop)[1] <- 'code'
state_pop <- select(state_pop, -state)

gdp_df <- merge(state_gdp_2012_2015, gdp_def, all.x = TRUE)
gdp_df <- merge(gdp_df, state_pop, all.x = TRUE, all.y = TRUE)
gdp_df <- mutate(gdp_df, gdp_cap = gdp*100/(population*GDP_deflator_index)) #list of states gdp_cap in reais of 2010.

gdp_df <- select(gdp_df, year, code, gdp_cap, everything())
gdp_cap_wip <- select(gdp_cap_wip, year, code, gdp_cap, everything())

gdp_df[1:324, 1:3] <- gdp_cap_wip

gdp_df <- select(gdp_df, year, code, gdp_cap, population, gdp, GDP_deflator_index)
