library(dplyr)
library(openxlsx)
library(lubridate)
library(reshape2)
library(openair)

hen_gas <- read.xlsx('data/Hengrove Park - Historical Utility Usage.xlsx', sheet = 'Gas', startRow = 2) %>% 
  mutate(date = convertToDateTime(From, origin = "1900-01-01")) %>% 
  select(-From, -To) %>% 
  timeAverage('year', statistic = 'sum')

hen_e <- read.xlsx('data/Hengrove Park - Historical Utility Usage.xlsx', sheet = 'Electric', startRow = 2)

hen_elec <- hen_e[,c(1,5)] %>% 
  mutate(date = convertToDateTime(From, origin = "1900-01-01")) %>% 
  select(-From)  %>% 
  timeAverage('year', statistic = 'sum')

hor_in <- read.xlsx('data/Horfield - 2018-19 &2017-18 Monthly Electricity & Gas consumtion.xlsx', startRow = 2)

hor_elec_17 <- hor_in[1:12,1:2] %>% 
  mutate(date = convertToDateTime(`Year/.Month`, origin = "1900-01-01")) %>% 
  select(-`Year/.Month`)
hor_elec_18 <- hor_in[1:12,3:4]%>% 
  mutate(date = convertToDateTime(`Year/.Month`, origin = "1900-01-01")) %>% 
  select(-`Year/.Month`)
hor_gas_17 <- hor_in[15:26,1:2] %>% 
  mutate(date = convertToDateTime(`Year/.Month`, origin = "1900-01-01")) %>% 
  select(-`Year/.Month`)
hor_gas_18 <- hor_in[15:26,3:4] %>% 
  mutate(date = convertToDateTime(`Year/.Month`, origin = "1900-01-01")) %>% 
  select(-`Year/.Month`)

hor_elec <- rbind(hor_elec_17, hor_elec_18) %>% 
  mutate(consumption = as.numeric(`Consumption.(Kwh)`)) %>% 
  timeAverage('year', statistic = 'sum')
hor_gas <- rbind(hor_gas_17, hor_gas_18)%>% 
  mutate(consumption = as.numeric(`Consumption.(Kwh)`)) %>% 
  timeAverage('year', statistic = 'sum')

bs_elec_in <- read.csv('data/bristol_south_elec.csv') %>% 
  mutate(date = my(Month.Current.Year.Previous.Year)) %>% 
  select(-Month.Current.Year.Previous.Year, -change) %>% 
  melt('date')

bs_elec_17 <- bs_elec_in %>% 
  filter(variable == 'last_yr') %>% 
  mutate(date = date-365)

bs_elec_18 <- bs_elec_in %>% 
  filter(variable == 'current_year')

bs_elec <- rbind(bs_elec_17, bs_elec_18) %>% 
  mutate(value = gsub(',','', value),
         value = as.numeric(gsub(' ','', value))) %>% 
  group_by(date) %>% 
  timeAverage('year', statistic = 'sum')

bs_gas_in <- read.csv('data/bristol_south_gas.csv') %>% 
  mutate(date = my(Month.Current.Year.Previous.Year)) %>% 
  select(-Month.Current.Year.Previous.Year, -change) %>% 
  melt('date')

bs_gas_17 <- bs_gas_in %>% 
  filter(variable == 'last_year') %>% 
  mutate(date = date-365)

bs_gas_18 <- bs_gas_in %>% 
  filter(variable == 'current_year')

bs_gas <- rbind(bs_gas_17, bs_gas_18) %>% 
  mutate(value = gsub(',','', value),
         value = as.numeric(gsub(' ','', value))) %>% 
  group_by(date)%>% 
  timeAverage('year', statistic = 'sum')

east_tot <- read.csv('data/easton_2019_2022.csv')
names(east_tot) <- c('util', '2019', '2020', '2021', '2022')
east_tot <- melt(east_tot, 'util')
names(east_tot) <- c('util', 'year', 'consumption')
east_tot <- mutate(east_tot, consumption = as.numeric(consumption))
# load the tabulizer package
library(tabulizer)

# set the path to your PDF file
pdf_path <- "data/BAM utilities invoice 16-17.pdf"

# extract tables from the PDF file
tables <- extract_tables(pdf_path)

# print the extracted tables
henbury <- data.frame(tables[2])

henbury_consumption <- henbury[16,]$X2

