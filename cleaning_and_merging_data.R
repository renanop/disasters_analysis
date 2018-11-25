######################### INTRODUCTION ############################
# 1 - The emdat_brazil_natural_disasters database was provided by #
# The International Disaster Database (EM-DAT), by the Centre for #
# Research on the Epidemiology of Disasters (CRED). The data was  #
# extracted on 19/08/2018                                         #
#                                                                 #
# 2 - state_pop.csv is a dataset offered by the Instituto de      #
# Pesquisa Econômica Aplicada (IPEA) and its database: IPEAData   # 
# That said, the available data only covered the period of year   #
# 2000 until the year 2017                                        #
# So, the data on population for the year of 2018 was extracted   #
# directly from the website of the Instituto Brasileiro de        #
# Geografia e Estatística (IBGE), and updated manually during the #
# data preparation for analysis (08/09/2018):                     #
#                                                                 #
# 2018: Rio de Janeiro's population: 17.159.960 people (estimate) #
#       Sao Paulo's population: 45.538.936 people (estimate)      #
#                                                                 #
# 3 - GDP_growth_2000_2010 is a dataset also offered by IPEA,     #
# based on data from IBGE, and presents a series of GDP growths   #
# on state level, year by year, at constant prices. For more info #
# and also for download links, check: https://bit.ly/2JYbzmu.     #
#                                                                 #
# 4- We considered only women with ages beetween 30 to 54 years,  #
# including the extremes. The data was retrieved from the SIDRA   #
# system of IBGE. The source was the PNAD(2015). The data covers  #
# the years of 2001 until 2015, excluding the year os 2010        #
#                                                                 #
# 5 - The sewerage system access data was also retrieved from the #
# SIDRA base, and its source is also the PNAD(2015)               #
###################################################################

################# IMPORTANT INFO ABOUT THE DATA ####################
# 1 - On the EM-DAT data, the total financial damage and the       #
# total insured losses are both in thousands of dollars            #
#                                                                  #
# 2 - PNAD_unemployment data was based on the "taxa de desocupação"#
# of people with age above 14 years old. The metric  "taxa de des- #
# ocupação" considers that, in order to be unoccupied, one must    #
# not be involved in any gainful occupation and should have looked #
# for a job in the last period of 30 days . For more information:  #
# https://bit.ly/2L5lCrD (link for the IBGE methodology)           #
#                                                                  #
# 3 - State populations are on a scale of thousands of people      #
#                                                                  #
####################################################################

library(tidyverse)
library(lubridate)

setwd('G:/Meu Drive/ic_desastres')

input_emdat_disasters <- read.csv('emdat_brazil_natural_disasters.csv',
                header = TRUE, sep = ';', na.strings = c('--','',' '))
input_state_pop <- read_csv2('state_pop.csv')
input_house_sanitation <- read_csv('houses_with_sanitation.csv', skip = 1)
GDP_series <- read_csv('GDP_growth_2000_2010.csv', skip = 1)
PNAD_unemployment <- read_csv2('PNAD_unemployment.csv', skip = 2)
sewerage_system <- as.tibble(read.csv2('sewerage_system_access_states.csv', skip = 3, check.names = FALSE))[1:27,]
women <- as.tibble(read.csv2('mulheres_30_54.csv', skip = 3, check.names = FALSE))[1:27,]

# Tibbles!
disasters <- as_tibble(input_emdat_disasters[1:201,])
malaquiazada <- as_tibble(input_emdat_disasters[1:201,])
state_populations <- as_tibble(input_state_pop)
house_sanitation <- as_tibble(input_house_sanitation[,1:24])
gdp_growth <- as_tibble(GDP_series)
gdp_growth <- gdp_growth[,-ncol(gdp_growth)]
gather(gdp_growth, years,GDP,c(-1:-3))
unemployment <- as_tibble(PNAD_unemployment)

#Getting state names and ISOs in order to make an ISO column
#Notice that the state vector is ordered in alphabetical order
#And the ISO vector follow the same order of the state vector
states <- c('Acre', 'Alagoas', 'Amapa', 'Amazonas', 'Bahia',
            'Ceara', 'Distrito Federal', 'Espirito Santo', 'Goias', 'Maranhao',
            'Mato Grosso', 'Mato Grosso do Sul', 'Minas Gerais', 'Parana', 'Paraiba',
            'Para', 'Pernambuco', 'Piaui', 'Rio de Janeiro', 'Rio Grande do Norte',
            'Rio Grande do Sul', 'Rondonia', 'Roraima', 'Santa Catarina', 'Sergipe',
            'Sao Paulo', 'Tocantins')

ISOS <- c('AC', 'AL', 'AP', 'AM', 'BA',
          'CE', 'DF', 'ES', 'GO', 'MA',
          'MT', 'MS', 'MG', 'PR', 'PB',
          'PA', 'PE', 'PI', 'RJ', 'RN',
          'RS', 'RO', 'RR', 'SC', 'SE',
          'SP', 'TO')

states_br <- c('Acre', 'Alagoas', 'Amapá', 'Amazonas', 'Bahia',
               'Ceará', 'Distrito Federal', 'Espírito Santo', 'Goiás', 'Maranhão',
               'Mato Grosso', 'Mato Grosso do Sul', 'Minas Gerais', 'Paraná', 'Paraíba',
               'Pará', 'Pernambuco', 'Piauí', 'Rio de Janeiro', 'Rio Grande do Norte',
               'Rio Grande do Sul', 'Rondônia', 'Roraima', 'Santa Catarina', 'Sergipe',
               'São Paulo', 'Tocantins')

states_df <- tibble(ISOS, states_br, states)

#Defining a function that remove the brazilian accents from the state names  
change_name <- function(state) {
  if (state == 'Mato Grosso') {
    return('Mato Grosso')
  } else {
    index <- grep(state, states_df[['states_br']])
    return(states_df[['states']][index])
  }
}

# State populations scale is 1, we are going make it "thousands of people"
state_populations[4:31] <- state_populations[4:31]/1000

# Also, we need to reshape this dataset in order to have all the years in a column.
# And remove this weird code from the data.
state_populations <- gather(state_populations, year, population, 4:31)
state_populations <- state_populations[c('ISO','year','state','population')]
state_populations <- state_populations[which(state_populations$year >= 2000),]
#state_populations <- apply(state_populations['state'], 1, change_name)

# Cleaning the house sanitation data:
house_sanitation <- gather(house_sanitation, key = 'year',
                           value = 'sanitation_percentage', c(-1:-3))
house_sanitation <- rename(house_sanitation, ISO = Sigla, state = Estado, code = Código)
house_sanitation <- select(house_sanitation, - code)
house_sanitation <- house_sanitation[which(house_sanitation$year >= 2000),]

# Removing last column (was empty) and Gathering years in GDP dataset
gdp_growth <- gdp_growth[,-ncol(gdp_growth)]     
gdp_growth <- gather(gdp_growth, years,GDP,c(-1:-3))
gdp_growth <- rename(gdp_growth, ISO = Sigla, state = Estado, code = Código, year = years)
gdp_growth <- select(gdp_growth, - code)

# Renaming columns of unemployment data and gathering them.
names(unemployment)[1] <- 'location'
vec_of_col_names <- character()
for (i in 2012:2018) {
  vec_of_col_names <- c(vec_of_col_names, (paste(i, 1:4, sep = '_')))
}

vec_of_col_names <- head(vec_of_col_names, -2)
names(unemployment)[2:length(names(unemployment))] <- vec_of_col_names

for (i in 2012:2017) {
  unemployment[[toString(i)]] <- (unemployment[[paste(i,1, sep = '_')]] +
    unemployment[[paste(i,2, sep = '_')]]+
    unemployment[[paste(i,3, sep = '_')]]+ 
    unemployment[[paste(i,4, sep = '_')]])/4
}

sum <- (unemployment[['2018_1']] + unemployment[['2018_2']])/2
unemployment[['2018']] <- sum
unemployment <- unemployment[,-2:-27]
unemployment <- gather(unemployment, year, unemployment, -1)


unemployment['location'] <- apply(unemployment['location'], 1, change_name)

# Cleaning sewerage system access data
sewerage_system <- gather(sewerage_system, key = 'year', value = 'sewerage_system_coverage', -1)
sewerage_system['location'] <- apply(sewerage_system['location'], 1, change_name)

#cleaning the women data
women <- gather(women, key = 'year', value = 'percentage_of_woman_30_to_54',-1)
women['location'] <- apply(women['location'], 1, change_name)

# let's pick a standard for our column names:
# Standard is xxx_yyy

disasters <- disasters %>% rename(start_date = ï..Start.date,
                                  end_date = End.date,
                                  magnitude_value = Magnitude.value,
                                  magnitude_scale = Magnitude.scale,
                                  disaster_type = Disaster.type,
                                  disaster_subtype = Disaster.subtype,
                                  associated_disaster = Associated.disaster,
                                  associated_disaster2 = Associated.disaster2,
                                  total_deaths = Total.deaths,
                                  total_damage_000USD = Total.damage.000US..,
                                  total_affected = Total.affected,
                                  insured_losses_000USD = Insured.losses..000.US..,
                                  disaster_name = Disaster.name,
                                  disaster_key = Disaster.No,
                                  location = Location)

# Dealing with some spaces
disasters[['location']] <- trimws(disasters[['location']])


#Getting an overview
disaster_overview <- disasters %>% 
  group_by(disaster_type) %>% 
  summarise(
    count = n(),
    mean_deaths = mean(total_deaths, na.rm = TRUE),
    mean_affected = mean(total_affected, na.rm = TRUE),
    mean_financial_damage = mean(total_damage_000USD,
                                 na.rm = TRUE)
    )
# Removing useless columns (Country and ISO(country code)).
disaster_analysis <- disasters %>%
  select(
    start_date,
    end_date,
    location, 
    magnitude_value, 
    magnitude_scale, 
    disaster_type, 
    total_deaths, 
    total_affected, 
    total_damage_000USD, 
    insured_losses_000USD, 
    disaster_name,
    disaster_key)


### Counting unique entries on locations (It's clean now!) ###
### Multiple entries can correspond to only one event. ###
### Each entry correspond to one location. ###

unique_locations <- disaster_analysis %>% 
  select(location) %>% unique()
sep_analysis <- separate_rows(disaster_analysis, location, sep = ',')

# Loading state-population dataframe:

# Getting unique disaster type entries (It's clean!!).
unique_disaster_type <- disaster_analysis %>%
  select(disaster_type) %>% unique()

# Disaster count plot.
disaster_count_plot <- ggplot(
  data = disaster_overview,
  mapping = aes(x = disaster_type,
    y = count,
    fill = disaster_type
    )) + geom_col() + theme(
    axis.text.x = element_text(angle = 60, hjust = 1)) +
      labs(title = 'Disaster type count',
           x = 'Type of disaster', y = 'Number of ocurrences')


# Yearly level data
start_date_disasters_year <- disaster_analysis %>% 
  separate(col = start_date, into = c('day','month','year'), sep ='/')
start_date_disasters_year <-start_date_disasters_year %>%
  separate(col = end_date, into = c('end_day','end_month','end_year'), sep = '/')
disaster_year <- subset(start_date_disasters_year, year >= 2000 )

year_overview <- start_date_disasters_year %>% 
  group_by(year, disaster_type) %>% 
  summarise(
    count = n(),
    sum_deaths = sum(total_deaths, na.rm = TRUE),
    sum_affected = sum(total_affected, na.rm = TRUE),
    sum_financial_damage = sum(total_damage_000USD,
                                 na.rm = TRUE)
  )

recent_overview <- subset(year_overview, year >= 2000)

### National and yearly level visualizations ###

# Total deaths from natural disasters 1981 - 2018
total_deaths <- ggplot(data = recent_overview, 
                       mapping = aes(x = year, 
                                     y = sum_deaths, 
                                     fill = disaster_type)) +
  geom_col() + theme(axis.text.x = element_text(angle = 70,
                                                hjust = 1)) + 
  labs(title = 'Deaths due to natural disasters', 
       subtitle = '(yearly)', x = 'Year', y = 'Total Deaths')

# Total financial impact from natural disasters.
total_finance <- ggplot(data = recent_overview, 
                        mapping = aes(x = year, 
                                      y = sum_financial_damage/1000, 
                                      fill = disaster_type)) +
  geom_col() + theme(axis.text.x = element_text(angle = 70, hjust = 1)) + 
  labs(title = 'Total financial damage', 
       subtitle = '(in millions of U$)', 
       x = 'Year', y = 'Total damage')

# Total people affected by disasters
total_affected <- ggplot(data = recent_overview, 
                        mapping = aes(x = year, 
                                      y = sum_affected/1000, 
                                      fill = disaster_type)) +
  geom_col() + theme(axis.text.x = element_text(angle = 70, hjust = 1)) + 
  labs(title = 'Total number of people affected by disasters', 
       subtitle = '(in thousands of people)', 
       x = 'Year', y = 'Total number of people')

# Separating data with multiple locations into rows

disaster_rows <- separate_rows(disaster_year, location, sep = ',')
disaster_rows['ISO'] <- NA

summary <- summary(disaster_rows)

# Appending ISOs to the main data set (disaster_rows)

# first we need to clean some typos

disaster_rows[which(disaster_rows$location == 'Rio Grande Do Sul'), ][['location']] <- 'Rio Grande do Sul'
disaster_rows[which(disaster_rows$location == 'Minas Girais'), ][['location']] <- 'Minas Gerais'
disaster_rows[which(disaster_rows$location == 'Rio De Janeiro'), ][['location']] <- 'Rio de Janeiro'
disaster_rows[which(disaster_rows$location == 'Brasilia'), ][['location']] <- 'Distrito Federal'
disaster_rows[which(disaster_rows$location == ' Mato Grosso do Sul'), ][['location']] <- 'Mato Grosso do Sul'
disaster_rows[which(disaster_rows$location == 'Santa Cartarina'), ][['location']] <- 'Santa Catarina'
disaster_rows[which(disaster_rows$location == 'Rio Grande Do Norte'), ][['location']] <- 'Rio Grande do Norte'


disaster_rows <- select(disaster_rows, year, end_year, ISO, 
                        location,disaster_type, total_affected, 
                        total_deaths, total_damage_000USD, insured_losses_000USD, everything())

for (i in 1:length(states)) {
  if (length(disaster_rows[which(disaster_rows$location == states[i]),][['year']]) != 0) {
    disaster_rows[which(disaster_rows$location == states[i]),][['ISO']] <- ISOS[i]
  }
}


#Merging tibbles

sanitation_population <- merge(house_sanitation, state_populations,
                               all.x = TRUE, all.y = TRUE)
full_tibble <- merge(disaster_rows, sanitation_population, all.x = TRUE)
full_tibble <- merge(full_tibble, gdp_growth, all.x = TRUE)
full_tibble <- as.tibble(select(full_tibble, - state))
full_tibble <- as.tibble(merge(full_tibble, unemployment, all.x = TRUE))
full_tibble <- as.tibble(merge(full_tibble, sewerage_system, all.x = TRUE))
full_tibble <- as.tibble(merge(full_tibble, women, all.x = TRUE))

# Setting population of Rio and Sao Paulo for 2018

full_tibble[209,19] <- 17159.960
full_tibble[210,19] <- 45538.936

# Getting data proportional to state populations 
iter <- 1
unique_disaster_keys <- c()

for (key in unique(full_tibble[['disaster_key']])) {
  unique_disaster_keys[[iter]] <- key
  iter <- iter+1
}
for (key in unique_disaster_keys) {
  sum_pop <- sum(filter(full_tibble, disaster_key == key)[['population']])
  for (row in 1:nrow(full_tibble[which(full_tibble$disaster_key == key),])) {
    full_tibble[which(full_tibble$disaster_key == key),][row,6:9] <- 
      (full_tibble[which(full_tibble$disaster_key == key),][row,6:9]*
         full_tibble[which(full_tibble$disaster_key == key),][[row,'population']])/sum_pop
  }
}

#Generating  a new data frame that sums the damage variables
total_data_locations <- summarise(group_by(full_tibble, year, location,disaster_type), 
                                  sum_damage = sum(total_damage_000USD, na.rm = TRUE), 
                                  sum_death = sum(total_deaths, na.rm = TRUE), 
                                  sum_affected = sum(total_affected))

#merging explanatory variables to the new dataset
#total_data_locations <- merge(total_data_locations, gdp_growth, all.x = TRUE) !(Check the gdp dataset)!
total_data_locations <- as.tibble(merge(total_data_locations, unemployment, all.x = TRUE))
total_data_locations <- as.tibble(merge(total_data_locations, sewerage_system, all.x = TRUE))
total_data_locations <- as.tibble(merge(total_data_locations, women, all.x = TRUE))

#writing new csv
