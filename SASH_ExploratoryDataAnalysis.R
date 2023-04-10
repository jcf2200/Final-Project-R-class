
library("readxl")
library("writexl")
library(tidyverse)
library(dplyr)
library(data.table)
library(tidycensus)
library(weights)
library(vtable)
library(ggplot2)
library(ggbreak) 
library(patchwork)

setwd("G:/My Drive/Spring 2023/Data Analysis in R/Final_Project/R Final Project")
getwd()

# -----------------------------------------------------------------------------
#    Initial data loading
# -----------------------------------------------------------------------------


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
  filter(installation_year %in% c("2008", "2009", "2010", "2011", "2012", "2013",
                                  "2014", "2015", "2016", "2017"))
  
# Inspection
str(TTS_data)

summary(TTS_data$installation_date)
summary(TTS_data$zip_code)

# Save new datasets
write.csv(TTS_data_CA_2008_2017, "Data/TTS_data_CA_2008_2017.csv")  
write.csv(SASH_data, "Data/SASH_data.csv")  


## -----------------------------------------------------------------------------
## Get demographic data from the American Community Survey (via tidycensus)
## -----------------------------------------------------------------------------

# Download data from the US Census Bureau using the tidycensus package

# census_api_key("0fb5f5569947094b64426a40babb1c0840952f0b", install = TRUE)
# Data security: run this in console, not in R markdown file

# initialize new data frame to store ACS data from API requests
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
                 med_age = "B01002_001",
                 nonfluent_english_pop = "B06007_005",
                 ed_LThighschool = "B23006_002",
                 ed_college = "B23006_023",
                 ed_total = "B23006_001",
                 medianinc = "B19013_001",
                 inc_under10 = "B19001_002",
                 inc_10to15 = "B19001_003",
                 inc_15to20 = "B19001_004",
                 inc_20to25 = "B19001_005",
                 inc_25to30 = "B19001_006",
                 inc_30to35 = "B19001_007",
                 inc_35to40 = "B19001_008",
                 inc_40to45 = "B19001_009",
                 inc_45to50 = "B19001_010",
                 inc_50to60 = "B19001_011",
                 inc_60to75 = "B19001_012",
                 inc_75to100 = "B19001_013",
                 inc_100to125 = "B19001_014",
                 inc_125to150 = "B19001_015",
                 inc_150to200 = "B19001_016",
                 inc_over200 = "B19001_017",
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
             nonfluentshare = 100 * (nonfluent_english_pop/pop),
             LThighschoolshare = 100 * (ed_LThighschool/ed_total),
             collegeshare = 100 * (ed_college/ed_total),
             poverty_rate = 100 * (pov/pop),
             zip_code = as.character(GEOID)) %>%
      select(-GEOID, -ed_total)
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


# Get median income at county level - used as reference for SASH eligibility
CA_acs_countyAMI_11_18 <- data_frame() # Zip code data only available from 2011 onwards in 5-year ACS

v15 <- load_variables(2015, "acs5", cache = TRUE)

# #View(v15)

# Set up a for loop for each year of data to access
for (i in 2011:2018) {

  # Query ACS data from the census API for each year 2010-2017
  acs <- get_acs(geography = "county",
                 state = "CA",
                 variables = c(medianinc = "B19013_001"),
                 year = i)

  # Transform data for later analysis and prep for join
  acs <- acs %>%
    select(-moe) %>%
    pivot_wider(names_from = variable, values_from = estimate) %>%
    mutate(year = i,
           county = as.character(NAME),
           county_medianinc = medianinc) %>%
    select(-GEOID, -NAME, -medianinc)
  print(i)

  # Append each year of data to a combined dataset
  CA_acs_countyAMI_11_18 <- CA_acs_countyAMI_11_18 %>%
    bind_rows(acs)
}

# clean input data
CA_acs_countyAMI_11_18 <- CA_acs_countyAMI_11_18 %>%
  mutate(county = as.factor(str_sub(county, 1, - 20)))

# Save ACS data frame
write.csv(CA_acs_countyAMI_11_18, "Data/CA_acs_countyAMI_11_18.csv")

## -----------------------------------------------------------------------------
## Restructuring of installation datasets into zip code - year panel
## -----------------------------------------------------------------------------

# Read saved datasets
TTS_data_CA_2008_2017 <- read.csv("Data/TTS_data_CA_2008_2017.csv") 
SASH_data <- read.csv("Data/SASH_data.csv") 

# Get zip code - year summaries for TTS installations
TTS_data_zipyr <- TTS_data_CA_2008_2017 %>% 
  mutate(zip_code = substr(zip_code, 1, 5)) %>% 
  mutate(zip_code = as.factor(zip_code), 
         year = as.factor(installation_year)) %>% 
  group_by(zip_code, year) %>% 
  summarise(total_sys_installed = n(),
            total_avg_size_DC = mean(system_size_DC))

summary(TTS_data_zipyr) # Average of 54 TTS installations per zip code

# Get zip code - year summaries for SASH installations
SASH_data_zipyr <- SASH_data %>% 
  mutate(zip_code = substr(Host.Customer.Physical.Address.Zip.Code, 1, 5),
         year = substr(First.Completed.Date, 
                       (nchar(First.Completed.Date)+1)-4,nchar(First.Completed.Date))) %>% 
  mutate(zip_code = as.factor(zip_code), 
         year = as.factor(year)) %>% 
  group_by(zip_code, year) %>% 
  summarise(SASH_sys_installed = n(),
            SASH_avg_size_DC = mean(Nameplate.Rating..KW.),
            SASH_avg_incentive = mean(Incentive.Amount),
            SASH_avg_syscost = mean(Total.System.Cost))

summary(SASH_data_zipyr) # Average of 4 SASH installations per zip code

# Join TTS and SASH installation data at zip code - year level
installations_data_zipyr <- SASH_data_zipyr %>% 
  full_join(TTS_data_zipyr, by=c("zip_code","year")) %>% 
  mutate(SASH_installations_share = SASH_sys_installed/total_sys_installed)

# Save joined dataset
write.csv(installations_data_zipyr, "Data/installations_data_zipyr.csv")  

# Inspection

summary(installations_data_zipyr$SASH_installations_share) # SASH installations represent 20% of total installations on average
installations_data_zipyr %>% filter(SASH_installations_share >1) %>% nrow() # 12 zip codes where SASH installations > TTS installations

## -----------------------------------------------------------------------------
## Merging datasets
## -----------------------------------------------------------------------------

# Read SASH and TTS installations dataframe
 installations_data_zipyr <- read.csv("Data/installations_data_zipyr.csv") %>% 
  select(-X)

 # Read ACS socioeconomic dataframe
 CA_acs_zipcode_11_18 <- read.csv("Data/CA_acs_zipcode_11_17.csv") %>% 
  mutate(zip_code = as.factor(zip_code)) %>% 
  select(-X)
 
 # Read list of counties for each zip code
 CA_county_zipcode <- read_excel("Data/CA_Zipcode_County_List.xlsx") %>% 
   rename(zip_code = `IP Code`, county = County) %>% 
   mutate(zip_code = as.factor(str_sub(zip_code, -5, - 1))) %>% 
   select(-City, -Type)
 
 # Read Median Income per county from ACS (substitute for CA Housing Department data, not digitalized)
 CA_acs_countyAMI_11_18 <-  read.csv("Data/CA_acs_countyAMI_11_18.csv") %>% 
   select(-X)
 
 
 # Join installations and demographic data
 complete_data_zipyr <- installations_data_zipyr %>% 
   mutate(zip_code = as.factor(zip_code)) %>%
   complete(zip_code,year, fill = list(SASH_sys_installed = 0, total_sys_installed = 0), 
            explicit = FALSE) %>% 
   left_join(CA_county_zipcode, by = "zip_code") %>% 
   left_join(CA_acs_countyAMI_11_18, by=c("county","year")) %>%
   left_join(CA_acs_zipcode_11_18, by=c("zip_code","year")) %>%
   filter(year >= 2011 & year < 2017) %>% 
   filter(zip_code != -1) %>% 
   filter(!is.na(pop)) %>% 
   #mutate(SASH_sys_installed = replace_na(SASH_sys_installed, 0)) %>% 
   mutate(SASH_sys_installed = replace_na(SASH_sys_installed, 0),
          SASH_avg_size_DC = replace_na(SASH_avg_size_DC, 0),
          SASH_avg_incentive = replace_na(SASH_avg_incentive, 0),
          SASH_avg_syscost = replace_na(SASH_avg_syscost, 0),
          total_sys_installed = replace_na(total_sys_installed, 0),
          total_sys_installed = ifelse(
            total_sys_installed < SASH_sys_installed, SASH_sys_installed, total_sys_installed),
          total_avg_size_DC = replace_na(SASH_avg_size_DC, 0),
          SASH_installations_share = SASH_sys_installed/total_sys_installed,
          SASH_install_rate_10000 = SASH_sys_installed/pop*10000,
          total_install_rate_10000 = total_sys_installed/pop*10000,
          inc_threshold = 0.8*county_medianinc) %>%
   mutate(inc_under15 = inc_under10 + inc_10to15,
          inc_under20 = inc_under15 + inc_15to20,
          inc_under25 = inc_under20 + inc_20to25,
          inc_under30 = inc_under25 + inc_25to30,
          inc_under35 = inc_under30 + inc_30to35,
          inc_under40 = inc_under35 + inc_35to40,
          inc_under45 = inc_under40 + inc_40to45,
          inc_under50 = inc_under45 + inc_45to50,
          inc_under60 = inc_under50 + inc_50to60,
          inc_under75 = inc_under60 + inc_60to75,
          inc_under100 = inc_under75 + inc_75to100,
          inc_under125 = inc_under100 + inc_100to125,
          inc_under150 = inc_under125 + inc_125to150,
          inc_under200 = inc_under150 + inc_150to200,
          total_hh = inc_under200 + inc_over200,
          eligible_hh = NA) %>% 
   select(-inc_10to15,-inc_15to20,-inc_20to25,-inc_25to30,-inc_30to35,-inc_35to40,
          -inc_40to45,-inc_45to50,-inc_50to60,-inc_60to75,-inc_75to100,-inc_100to125,
          -inc_125to150,-inc_150to200,
          -male_pop, -black_pop, -asian_pop, -white_pop, -hisp_pop, -nonfluent_english_pop) %>% 
   relocate(year,county,zip_code) %>% 
   arrange(year, county, zip_code) 
 
 # Interpolate number of people under 80% of zip code median income based on ACS income buckets
 income_interpolation =  data.frame(15,20,25,30,35,40,45,50,60,75,100,125,150,200)
 
 for (i in (1:nrow(complete_data_zipyr))) {
   hh_eligible = approx(x = income_interpolation, y = complete_data_zipyr[i,32:45], 
                        xout=complete_data_zipyr[i,31]/1000, method = "linear")
   complete_data_zipyr[i,47] <- hh_eligible$y
 }
 
 # Calculate eligibility share in each zip code (% hh under 80% of median income)
 complete_data_zipyr <- complete_data_zipyr %>% 
   mutate(eligibilityshare = eligible_hh/ total_hh)

 # Inspection
 complete_data_zipyr %>% filter(is.na(eligible_hh))
 complete_data_zipyr %>% filter(is.na(eligibilityshare))

 # Remove 14 county observations with no or very small population
 complete_data_zipyr <- complete_data_zipyr %>% 
   filter(!is.na(eligibilityshare))
 
 summary(complete_data_zipyr)
 table(complete_data_zipyr$year, complete_data_zipyr$SASH_sys_installed)
 table(complete_data_zipyr$year, complete_data_zipyr$total_sys_installed)
 
 write.csv(complete_data_zipyr, "Data/complete_data_zipyr.csv") 
 
 
 ## -----------------------------------------------------------------------------
 ## Summary statistics
 ## ----------------------------------------------------------------------------- 
 
 complete_data_zipyr <- read.csv("Data/complete_data_zipyr.csv") %>% 
   select(-X)

 # Descriptive statistics

 summary(complete_data_zipyr$SASH_install_rate_1000)

 summary(complete_data_zipyr$total_install_rate_1000)

 # Labels for summary statistics table
 labs <- c('SASH installed systems per 10000 people',
           'Total installed systems per 10000 people',
           '% of households eligible for SASH program',
           'Share of SASH in total installations',
           'Median household income',
           'Hispanic share',
           'Share of non-fluent English',
           'Share with less than highschool degree',
           'Share with college degree')

 # (Include in RMD) Summary statistics table with subset of variables
 st(complete_data_zipyr,
    vars = c('SASH_install_rate_10000','total_install_rate_10000','eligibilityshare',
             'SASH_installations_share','medianinc','hispshare','nonfluentshare', 
             'LThighschoolshare', 'collegeshare'),
    digits=2,
    labels=labs,
    title='Summary statistics at zip code - year level.')
 
 #  Histogram for SASH installations by zip code - year
 ggplot(data = complete_data_zipyr,
        aes(x = SASH_sys_installed)) +
   geom_histogram(fill="darkgoldenrod1", binwidth=1, boundary=0) +
   scale_y_break(c(300, 11000), ticklabels = NULL) +
   labs(x = "Number of SASH systems installed (zip code - year)", y = "Count") +
   scale_y_continuous(n.breaks = 4) +
   xlim(0,50) +
   theme_minimal()
 
 # Histogram for SASH installation rate by zip code - year
 ggplot(data = complete_data_zipyr,
        aes(x = SASH_install_rate_10000)) +
   geom_histogram(fill="darkgoldenrod1", bindwith =1, boundary=0) +
   # scale_y_break(c(300, 500), ticklabels = NULL) +
   labs(x = "SASH systems installed per 10000 people (zip code - year)", y = "Count") +
   xlim(0,50) +
   theme_minimal()
 
 
 # Histogram for total installations by zip code - year
 ggplot(data = complete_data_zipyr,
        aes(x = total_sys_installed)) +
   geom_histogram(fill="darkorange3", binwidth=1, boundary=0) +
   # scale_y_break(c(300, 500), ticklabels = NULL) +
   labs(x = "Number of systems installed (zip code - year)", y = "Count") +
   scale_y_continuous(n.breaks = 4) +
   xlim(0,50) +
   theme_minimal()
 
 #  Histogram for total installation rate by zip code - year
 ggplot(data = complete_data_zipyr,
        aes(x = total_install_rate_10000)) +
   geom_histogram(fill="darkorange3", bindwith=1, boundary=0) +
   # scale_y_break(c(300, 500), ticklabels = NULL) +
   labs(x = "Total systems installed per 10000 people (zip code - year)", y = "Count") +
   xlim(0,50) +
   theme_minimal()
 
 # Histogram of eligibility share
 ggplot(data = complete_data_zipyr,
        aes(x = eligibilityshare)) +
   geom_histogram(fill="darkolivegreen",binwidth=0.01, boundary=0) +
   # scale_y_break(c(300, 500), ticklabels = NULL) +
   labs(x = "Household SASH elegibility share (zip code - year)", y = "Count") +
   scale_y_continuous(n.breaks = 4) +
   theme_minimal()
 
 
 # Calculate number of systems installed per year
 data_zipyr_annualstat <- complete_data_zipyr %>% 
   group_by(year) %>% 
   summarise(annual_SASH_installations = sum(SASH_sys_installed),
             annual_total_installations = sum(total_sys_installed))
 
 
 #  Bar plot for total SASH installations by year
 ggplot(data = data_zipyr_annualstat,
        aes(x = year, y = annual_SASH_installations)) +
   geom_bar(stat = "identity", fill="darkgoldenrod1") +
   labs(x = "Year", y = "Total SASH systems installed") +
   theme_minimal()
 
 
 #  Bar plot for total installations by year
 ggplot(data = data_zipyr_annualstat,
        aes(x = year, y = annual_total_installations)) +
   geom_bar(stat = "identity", fill="darkorange3") +
   labs(x = "Year", y = "Total systems installed") +
   theme_minimal()
 
 
 
 # Exploratory correlations
 wtd.cor(complete_data_zipyr$SASH_install_rate_1000, complete_data_zipyr$eligibilityshare) 
 
 wtd.cor(complete_data_zipyr$total_install_rate_1000, complete_data_zipyr$eligibilityshare)
 
 wtd.cor(complete_data_zipyr$total_install_rate_1000, complete_data_zipyr$poverty_rate)
 
 wtd.cor(complete_data_zipyr$total_install_rate_1000, complete_data_zipyr$hispshare)
 
 
 ## -----------------------------------------------------------------------------
 ## Alternative: aggregation at county level - county data extraction from ACS
 ## ----------------------------------------------------------------------------- 
 
 
 # Get median income and socioeconomic statistics at county level - used as reference for SASH eligibility
 CA_acs_countystats_11_18 <- data_frame() # Zip code data only available from 2011 onwards in 5-year ACS

 v15 <- load_variables(2015, "acs5", cache = TRUE)

 # #View(v15)

 # Set up a for loop for each year of data to access
 for (i in 2011:2018) {

   # Query ACS data from the census API for each year 2010-2017
   acs <- get_acs(geography = "county",
                  state = "CA",
                 variables = c(medianinc = "B19013_001",
                 pop = "B01001_001",
                 white_pop = "B01001H_001",
                 hisp_pop = "B01001I_001",
                 asian_pop = "B02001_005",
                 black_pop = "B02001_003",
                 male_pop = "B01001_002",
                 med_age = "B01002_001",
                 nonfluent_english_pop = "B06007_005",
                 ed_LThighschool = "B23006_002",
                 ed_college = "B23006_023",
                 ed_total = "B23006_001",
                 medianinc = "B19013_001",
                 inc_under10 = "B19001_002",
                 inc_10to15 = "B19001_003",
                 inc_15to20 = "B19001_004",
                 inc_20to25 = "B19001_005",
                 inc_25to30 = "B19001_006",
                 inc_30to35 = "B19001_007",
                 inc_35to40 = "B19001_008",
                 inc_40to45 = "B19001_009",
                 inc_45to50 = "B19001_010",
                 inc_50to60 = "B19001_011",
                 inc_60to75 = "B19001_012",
                 inc_75to100 = "B19001_013",
                 inc_100to125 = "B19001_014",
                 inc_125to150 = "B19001_015",
                 inc_150to200 = "B19001_016",
                 inc_over200 = "B19001_017",
                 pov = "B17025_002"),
            year = i)

  # Transform data for later analysis and prep for join
    acs <- acs %>%
      select(-moe) %>%
      pivot_wider(names_from = variable, values_from = estimate) %>%
      mutate(year = i,
             county = as.character(NAME),
             county_medianinc = medianinc,
             whiteshare = 100 * (white_pop/pop),
             hispshare = 100 * (hisp_pop/pop),
             asianshare = 100 * (asian_pop/pop),
             blackshare = 100 * (black_pop/pop),
             maleshare = 100 * (male_pop/pop),
             nonfluentshare = 100 * (nonfluent_english_pop/pop),
             LThighschoolshare = 100 * (ed_LThighschool/ed_total),
             collegeshare = 100 * (ed_college/ed_total),
             poverty_rate = 100 * (pov/pop)) %>%
      select(-GEOID, -NAME, -medianinc, -ed_total)
    print(i)

   # Append each year of data to a combined dataset
   CA_acs_countystats_11_18 <- CA_acs_countystats_11_18 %>%
     bind_rows(acs)
 }

 # clean input data
 CA_acs_countystats_11_18 <- CA_acs_countystats_11_18 %>%
   mutate(county = as.factor(str_sub(county, 1, - 20)))

 # Save ACS data frame
 write.csv(CA_acs_countystats_11_18, "Data/CA_acs_countystats_11_18.csv")
 
 ## -----------------------------------------------------------------------------
 ## Alternative: aggregation at county level - Merging datasets
 ## ----------------------------------------------------------------------------- 
 
 
 # Read SASH and TTS installations dataframe
 installations_data_zipyr <- read.csv("Data/installations_data_zipyr.csv") %>% 
   select(-X)
 
 # Read ACS socioeconomic dataframe
 CA_acs_zipcode_11_18 <- read.csv("Data/CA_acs_zipcode_11_17.csv") %>% 
   mutate(zip_code = as.factor(zip_code)) %>% 
   select(-X)
 
 # Read list of counties for each zip code
 CA_county_zipcode <- read_excel("Data/CA_Zipcode_County_List.xlsx") %>% 
   rename(zip_code = `IP Code`, county = County) %>% 
   mutate(zip_code = as.factor(str_sub(zip_code, -5, - 1))) %>% 
   select(-City, -Type)
 
 # Read Median Income per county from ACS (substitute for CA Housing Department data, not digitalized)
 CA_acs_countystats_11_18 <-  read.csv("Data/CA_acs_countystats_11_18.csv") %>% 
   select(-X)
 
 
 # Join installations and demographic data
 complete_data_countyr <- installations_data_zipyr %>% 
   mutate(zip_code = as.factor(zip_code)) %>% 
   left_join(CA_county_zipcode, by = "zip_code") %>% 
   group_by(county,year) %>% 
   summarise(SASH_sys_installed = sum(SASH_sys_installed, na.rm = TRUE),
             total_sys_installed = sum(total_sys_installed,  na.rm = TRUE)) %>% 
   ungroup() %>% 
   complete(county,year, fill = list(SASH_sys_installed = 0, total_sys_installed = 0), 
            explicit = FALSE) %>% 
   left_join(CA_acs_countystats_11_18, by=c("county","year")) %>%
   filter(year >= 2011 & year < 2017) %>% 
   filter(!is.na(pop)) %>% 
   #mutate(SASH_sys_installed = replace_na(SASH_sys_installed, 0)) %>% 
   mutate(total_sys_installed = ifelse(
     total_sys_installed < SASH_sys_installed, SASH_sys_installed, total_sys_installed),
          SASH_installations_share = SASH_sys_installed/total_sys_installed,
          SASH_install_rate_10000 = SASH_sys_installed/pop*10000,
          total_install_rate_10000 = total_sys_installed/pop*10000,
          inc_threshold = 0.8*county_medianinc) %>%
   mutate(inc_under15 = inc_under10 + inc_10to15,
          inc_under20 = inc_under15 + inc_15to20,
          inc_under25 = inc_under20 + inc_20to25,
          inc_under30 = inc_under25 + inc_25to30,
          inc_under35 = inc_under30 + inc_30to35,
          inc_under40 = inc_under35 + inc_35to40,
          inc_under45 = inc_under40 + inc_40to45,
          inc_under50 = inc_under45 + inc_45to50,
          inc_under60 = inc_under50 + inc_50to60,
          inc_under75 = inc_under60 + inc_60to75,
          inc_under100 = inc_under75 + inc_75to100,
          inc_under125 = inc_under100 + inc_100to125,
          inc_under150 = inc_under125 + inc_125to150,
          inc_under200 = inc_under150 + inc_150to200,
          total_hh = inc_under200 + inc_over200,
          eligible_hh = NA) %>% 
   select(-inc_10to15,-inc_15to20,-inc_20to25,-inc_25to30,-inc_30to35,-inc_35to40,
          -inc_40to45,-inc_45to50,-inc_50to60,-inc_60to75,-inc_75to100,-inc_100to125,
          -inc_125to150,-inc_150to200,
          -male_pop, -black_pop, -asian_pop, -white_pop, -hisp_pop, -nonfluent_english_pop) %>% 
   relocate(year,county) %>% 
   arrange(year, county) 
 
 # Interpolate number of people under 80% of zip code median income based on ACS income buckets
 income_interpolation =  data.frame(15,20,25,30,35,40,45,50,60,75,100,125,150,200)
 
 for (i in (1:nrow(complete_data_countyr))) {
   hh_eligible = approx(x = income_interpolation, y = complete_data_countyr[i,26:39], 
                        xout=complete_data_countyr[i,25]/1000, method = "linear")
   complete_data_countyr[i,41] <- hh_eligible$y
 }
 
 # Calculate eligibility share in each zip code (% hh under 80% of median income)
 complete_data_countyr <- complete_data_countyr %>% 
   mutate(eligibilityshare = eligible_hh/ total_hh)
 
 # Inspection
 complete_data_countyr %>% filter(is.na(eligible_hh))
 complete_data_countyr %>% filter(is.na(eligibilityshare))

 summary(complete_data_countyr)
 write.csv(complete_data_countyr, "Data/complete_data_countyr.csv") 
 
 
 ## -----------------------------------------------------------------------------
 ## Alternative: aggregation at county level - Summary statistics
 ## ----------------------------------------------------------------------------- 
 
 complete_data_countyr <- read.csv("Data/complete_data_countyr.csv") %>% 
   select(-X)
 
 table(complete_data_countyr$county, complete_data_countyr$year)
 
 # Descriptive statistics
 
 summary(complete_data_countyr$SASH_install_rate_10000)
 
 summary(complete_data_countyr$total_install_rate_10000)
 
 # Labels for summary statistics table
 labs <- c('SASH installed systems per 10000 people',
           'Total installed systems per 10000 people',
           '% of households eligible for SASH program',
           'Share of SASH in total installations',
           'Median household income',
           'Hispanic share',
           'Share of non-fluent English',           
           'Share with less than highschool degree',
           'Share with college degree')
 
 # Summary statistics table with subset of variables
 st(complete_data_countyr,
    vars = c('SASH_install_rate_10000','total_install_rate_10000','eligibilityshare',
             'SASH_installations_share','county_medianinc','hispshare','nonfluentshare', 
             'LThighschoolshare', 'collegeshare'),
    digits=2,
    labels=labs,
    title='Summary statistics at county - year level.')
 
 #  Histogram for SASH installations by zip code - year
 ggplot(data = complete_data_countyr,
        aes(x = SASH_sys_installed)) +
   geom_histogram(fill="darkgoldenrod1", binwidth=5, boundary=0) +
   # scale_y_break(c(300, 8400), ticklabels = NULL) +
   labs(x = "Number of SASH systems installed (county - year)", y = "Count") +
   scale_y_continuous(n.breaks = 4) +
   theme_minimal() 
 
 # Histogram for SASH installation rate by zip code - year
 ggplot(data = complete_data_countyr,
        aes(x = SASH_install_rate_10000)) +
   geom_histogram(fill="darkgoldenrod1", bindwith =1, boundary=0) +
   # scale_y_break(c(300, 500), ticklabels = NULL) +
   labs(x = "SASH systems installed per 10000 people (county - year)", y = "Count") +
   xlim(0,30) + 
   theme_minimal() 
 
 #  Histogram for total installations by zip code - year
 ggplot(data = complete_data_countyr,
        aes(x = total_sys_installed)) +
   geom_histogram(fill="darkorange3", binwidth=1000, boundary=0) +
   # scale_y_break(c(300, 500), ticklabels = NULL) +
   labs(x = "Number of total systems installed (county - year)", y = "Count") +
   scale_y_continuous(n.breaks = 4) +
   theme_minimal() 
 
 #  Histogram for total installation rate by zip code - year
 ggplot(data = complete_data_countyr,
        aes(x = total_install_rate_10000)) +
   geom_histogram(fill="darkorange3", bindwith=1, boundary=0) +
   # scale_y_break(c(300, 500), ticklabels = NULL) +
   labs(x = "Total systems installed per 10000 people (county - year)", y = "Count") +
   xlim(0,200) + 
   theme_minimal() 
 
 # Histogram of eligibility share
 ggplot(data = complete_data_countyr,
        aes(x = eligibilityshare)) +
   geom_histogram(fill="darkolivegreen",binwidth=0.001, boundary=0) +
   # scale_y_break(c(300, 500), ticklabels = NULL) +
   labs(x = "Household SASH elegibility share", y = "Count") +
   scale_y_continuous(n.breaks = 4) +
   theme_minimal()
 
 
 