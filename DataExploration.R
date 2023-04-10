library("readxl")
library("writexl")
library(tidyverse)
library(dplyr)
library(data.table)
library(tidycensus)
library(weights)



getwd()

# Filter data for SASH 1.0 project only from Low Income Incentive dataset
SASH_data <- read.csv("Data/LowIncome_Applications_Dataset_2023-02-16.csv") %>% 
  filter(Low.Income.Program.Type == "SASH 1.0") %>% 
  discard(~all(is.na(.) | . ==""))
  
str(SASH_data)

# Filter data for California 2008-2017 from TTS dataset
TTS_data_CA_2008_2017 <- read.csv("Data/TTS_LBNL_public_file_07-Sep-2022_all.csv") %>% 
  mutate(installation_date = as.character(installation_date),
         zip_code = as.factor(zip_code)) %>% 
  mutate(installation_year = substr(installation_date, 8, -1)) %>% 
  filter(state == "CA", customer_segment == "RES") %>% 
  filter(installation_year %in% c("2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017"))
  

str(TTS_data)

summary(TTS_data$installation_date)
summary(TTS_data$zip_code)

# Save new datasets
write.csv(TTS_data_CA_2008_2017, "Data/TTS_data_CA_2008_2017.csv")  
write.csv(SASH_data, "Data/SASH_data.csv")  


# Read saved datasets
TTS_data_CA_2008_2017 <- read.csv("Data/TTS_data_CA_2008_2017.csv") 
SASH_data <- read.csv("Data/SASH_data.csv") 

# Get zip code - year summaries for TTS and SASH installations
TTS_data_zipyr <- TTS_data_CA_2008_2017 %>% 
  mutate(zip_code = substr(zip_code, 1, 5)) %>% 
  mutate(zip_code = as.factor(zip_code), 
         year = as.factor(installation_year)) %>% 
  group_by(zip_code, year) %>% 
  summarise(total_sys_installed = n(),
            total_avg_size_DC = mean(system_size_DC))

summary(TTS_data_zipyr) # Average of 54 TTS installations per zip code

SASH_data_zipyr <- SASH_data %>% 
  mutate(zip_code = substr(Host.Customer.Physical.Address.Zip.Code, 1, 5),
         year = substr(First.Completed.Date, (nchar(First.Completed.Date)+1)-4,nchar(First.Completed.Date))) %>% 
  mutate(zip_code = as.factor(zip_code), 
         year = as.factor(year)) %>% 
  group_by(zip_code, year) %>% 
  summarise(SASH_sys_installed = n(),
            SASH_avg_size_DC = mean(Nameplate.Rating..KW.),
            SASH_avg_incentive = mean(Incentive.Amount),
            SASH_avg_syscost = mean(Total.System.Cost))


summary(SASH_data_zipyr) # Average of 4 SASH installations per zip code

# Join both 
installations_data_zipyr <- SASH_data_zipyr %>% 
  full_join(TTS_data_zipyr, by=c("zip_code","year")) %>% 
  mutate(SASH_installations_share = SASH_sys_installed/total_sys_installed)

write.csv(installations_data_zipyr, "Data/installations_data_zipyr.csv")  


summary(installations_data_zipyr$SASH_installations_share) # SASH installations represent 20% of total installations on average

installations_data_zipyr %>% filter(SASH_installations_share >1) %>% nrow() # 12 zip codes where SASH installations > TTS installations



installations_data_zipyr <- read.csv("Data/installations_data_zipyr.csv")


## -----------------------------------------------------------------------------
## Get demographic data from the American Community Survey (via tidycensus)
## -----------------------------------------------------------------------------

# Download data from the US Census Bureau using the tidycensus package

# # census_api_key("0fb5f5569947094b64426a40babb1c0840952f0b", install = TRUE)
# # Data security: run this in console, not in R markdown file
# 
# # initialize new data frame to store ACS data from API requests
 CA_acs_zipcode_11_18 <- data_frame() # Zip code data only available from 2011 onwards in 5-year ACS

 v15 <- load_variables(2015, "acs5", cache = TRUE)

# #View(v15)
 
# Set up a for loop for each year of data to access
 for (i in 2011:2018) {

  # Query ACS data from the census API for each year 2010-2017
  acs <- get_acs(geography = "zip code tabulation area",
                 state = "CA",
                 variables = c(pop = "B01001_001",
                 white_pop = "B01001H_001",
                 hisp_pop = "B01001I_001",
                 asian_pop = "B02001_005",
                 black_pop = "B02001_003",
                 male_pop = "B01001_002",
                 medianinc = "B19013_001",
                 med_age = "B01002_001",
                 spanish_nat = "B06007_003",
                 pov = "B17025_002"),
            year = i)

  # Transform data for later analysis and prep for join
    acs <- acs %>%
      select(-moe, -NAME) %>%
      pivot_wider(names_from = variable, values_from = estimate) %>%
      mutate(year = i,
             whiteshare = 100 * (white_pop/pop),
             hispshare = 100 * (hisp_pop/pop),
             asianshare = 100 * (asian_pop/pop),
             blackshare = 100 * (black_pop/pop),
             maleshare = 100 * (male_pop/pop),
             poverty_rate = 100 * (pov/pop),
             zip_code = as.character(GEOID)) %>%
      select(-GEOID)
    print(i)

  # Append each year of data to a combined dataset
  CA_acs_zipcode_11_18 <- CA_acs_zipcode_11_18 %>%
    bind_rows(acs)
 }
 
 # clean input data
 CA_acs_zipcode_11_18 <- CA_acs_zipcode_11_18 %>%
   mutate(zip_code = as.factor(str_sub(zip_code, -5, - 1)))

 # Save ACS data frame
 write.csv(CA_acs_zipcode_11_18, "Data/CA_acs_zipcode_11_17.csv")
 
 # Join installations and demographic data
 complete_data_zipyr <- installations_data_zipyr %>% 
   mutate(zip_code = as.factor(zip_code)) %>% 
   left_join(CA_acs_zipcode_11_18, by=c("zip_code","year")) %>%
   filter(year >= 2011) %>% 
   filter(zip_code != -1) %>% 
   #mutate(SASH_sys_installed = replace_na(SASH_sys_installed, 0)) %>% 
   mutate(SASH_installations_share = SASH_sys_installed/total_sys_installed,
          SASH_install_rate_1000 = SASH_sys_installed/pop*1000,
          total_install_rate_1000 = total_sys_installed/pop*1000) %>%
   arrange(year, zip_code) 
 
 # Inspection
 summary(complete_data_zipyr)
 table(complete_data_zipyr$year, complete_data_zipyr$SASH_sys_installed)
 table(complete_data_zipyr$year, complete_data_zipyr$total_sys_installed)
 
 write.csv(complete_data_zipyr, "Data/complete_data_zipyr.csv")  
 
 # Descriptive statistics
 
 summary(complete_data_zipyr$SASH_install_rate_1000)
 
 summary(complete_data_zipyr$total_install_rate_1000)
 
 
 
 # Exploratory correlations
 wtd.cor(complete_data_zipyr$SASH_install_rate_1000, complete_data_zipyr$medianinc)
 
 wtd.cor(complete_data_zipyr$SASH_install_rate_1000, complete_data_zipyr$poverty_rate)
 
 wtd.cor(complete_data_zipyr$total_install_rate_1000, complete_data_zipyr$medianinc)
 
 wtd.cor(complete_data_zipyr$total_install_rate_1000, complete_data_zipyr$poverty_rate)
 
 wtd.cor(complete_data_zipyr$total_install_rate_1000, complete_data_zipyr$hispshare)
 
 
 
 