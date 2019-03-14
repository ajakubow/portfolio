################################################################################
# File:           importBLS2.R
#
# Description:    Scrape and Wrangle Bureau of Labor Force Statistics Data
# First version:  20180630
# This verison:   20180719
# Last executed:  20180725    
# Last change by: Alex
# requires:       N/A
# provides:       blsData01.csv; blsData01H.csv
################################################################################


### Priors ---------------------------------------------------------------------
rm(list = ls())

# Package Management #
library(rvest)
library(dplyr)
library(httr)
#library(readtext)
                 
# Browser Credentials for Scraping #
agent <- paste0("Mozilla/5.0 (Macintosh; Intel Mac OS X 10.11; rv:52.0) ",
                "Gecko/20100101 Firefox/52.0")

### Functions ------------------------------------------------------------------
# Harmonizer for County FIPS Codes
#
# This function harmonizes county FIPS codes to facilitate the construction of
# longitudinal datasets from disparate data sources that may employ different
# coding decisions for FIPS changes over time.
#
# The function assumes the variables of interest are measured in counts
# (as opposed to rates, logs, etc.)  It also assumes (and subsequently
# drops) rate variables that are  defined by an "R$" or "RH$" suffix.
#
# This version harmonizes all counties (except Alaska) from 1990 to present
# df Data frame to be harmonized.  Time unit is assumed to be in years.
# the data frame must contain the following numeric columns:
# fips_st (FIPS state code); fips_cnty (FIPS county code); year (year).

fipsClean <- function(df) {
  df <- df %>%
    select(-ends_with("R", ignore.case = FALSE),
           -ends_with("RH", ignore.case = FALSE),
           -ends_with("M", ignore.case = FALSE)) %>%
    mutate(fips_cnty = ifelse(fips_st == 46 & fips_cnty ==  113, 102, fips_cnty)) %>%
    mutate(fips_cnty = ifelse(fips_st == 12 & fips_cnty ==  25, 86, fips_cnty)) %>%
    mutate(fips_cnty = ifelse(fips_st == 51 & fips_cnty ==  560, 5, fips_cnty)) %>%
    mutate(fips_cnty = ifelse(fips_st == 51 & fips_cnty ==  515, 19, fips_cnty)) %>%
    mutate(fips_cnty = ifelse(fips_st == 51 & fips_cnty ==  780, 83, fips_cnty)) %>%
    mutate(fips_cnty = ifelse(fips_st == 8 & fips_cnty %in% c(1, 13, 14, 59, 123), 901,
                              fips_cnty)) %>%
    mutate(fips_cnty = ifelse(fips_st == 30 & fips_cnty %in% c(31, 67, 113), 901,
                              fips_cnty)) %>%
    mutate(fips_cnty = ifelse(fips_st == 15 & fips_cnty %in% c(5, 9), 901,
                              fips_cnty)) %>%
    group_by(fips_st, fips_cnty, year) %>%
    summarize_at(.vars = vars(starts_with("pop_"), starts_with("bls_"),
                              starts_with("bea_"), starts_with("shr_"),
                              starts_with("spe_"), starts_with("shi_"),
                              starts_with("ucr_"), starts_with("geo_"),
                              starts_with("dth_")),
                 .funs = sum)
  
  return(df)
}

### Download and Wrangle Files--------------------------------------------------
for (i in 0:17) {
  if (i < 10) {
    docName <- paste0("bls0", i)
    link <- paste0("https://www.bls.gov/lau/laucnty0", i, ".txt")
  }
  if (i >= 10) {
    docName <- paste0("bls", i)
    link <- paste0("https://www.bls.gov/lau/laucnty", i, ".txt")
  }
  # Download Raw Text File #
  GET(link, write_disk(paste0("../input/", docName, ".txt"), 
                       overwrite = TRUE), user_agent(agent))
   
  # Import as Fixed-Width #
  dataTemp <- read.fwf(file = paste0("../input/", docName, ".txt"),
                    widths = c(15, 5, 8, 53, 4, 14, 13, 11, 9),
                    stringsAsFactors = FALSE, skip = 6)
    
  # Wrangle #
  #remove commas from numeric fields
  vlist <- c(2, 3, 5:9)
    for (v in vlist) {
      dataTemp[v] <- dataTemp[v] %>% lapply(function(x) gsub(",", "", x))
    }
    
  #remove missing fips, reclassify numeric vars, rename
  dataTemp <- dataTemp %>%
    filter(!is.na(V3)) %>%
    mutate_at(vars(V2:V3, V5:V9), as.numeric) %>%
    rename(blscode = V1,
            fips_st = V2,
            fips_cnty = V3,
            name = V4,
            year = V5,
            bls_lf = V6,
            bls_emp = V7,
            bls_ue = V8,
            bls_ueR = V9) 

  # Combine All Years #
  if (i == 0) {
    blsData <- dataTemp
  } else {
    blsData <- bind_rows(dataTemp, blsData)
  }
  print(paste("Appending Year:", i))
}


### Download and Wrangle Files (1990+ Data) ------------------------------------
for (i in 90:99) {
  docName <- paste0("bls", i)
  link <- paste0("https://www.bls.gov/lau/laucnty", i, ".txt")
  
  # Download Raw Text File #
  GET(link, write_disk(paste0("../input/", docName, ".txt"), 
                       overwrite = TRUE), user_agent(agent))
  
  # Import as Fixed-Width #
  dataTemp <- read.fwf(file = paste0("../input/", docName, ".txt"),
                       widths = c(15, 5, 8, 53, 4, 14, 13, 11, 9),
                       stringsAsFactors = FALSE, skip = 6)
  
  # Wrangle #
  #remove commas from numeric fields#
  vlist <- c(2, 3, 5:9)
  for (v in vlist) {
    dataTemp[v] <- dataTemp[v] %>% lapply(function(x) gsub(",", "", x))
  }
  
  #remove missing fips, reclassify numeric vars, rename#
  dataTemp <- dataTemp %>%
    filter(!is.na(V3)) %>%
    mutate_at(vars(V2:V3, V5:V9), as.numeric) %>%
    rename(blscode = V1,
           fips_st = V2,
           fips_cnty = V3,
           name = V4,
           year = V5,
           bls_lf = V6,
           bls_emp = V7,
           bls_ue = V8,
           bls_ueR = V9) 
  
  # Combine All Years #
  if (i == 90) {
    blsData90 <- dataTemp
  } else {
    blsData90 <- bind_rows(dataTemp, blsData90)
  }
  print(paste("Appending Year:", i))
}

### Combine --------------------------------------------------------------------
blsData <- bind_rows(blsData90, blsData)


### Final Inspection -----------------------------------------------------------
head(blsData)
blsData <- blsData %>%
  select(fips_st, fips_cnty, year, name, everything()) %>%
  select(-blscode) %>%
  mutate(test = as.character(fips_st),
         test2 = as.character(fips_cnty)) %>%
  mutate(test = ifelse(fips_st < 10, paste0("0", test), test),
         test2 = ifelse(fips_cnty < 10, paste0("00", test2),
                                   ifelse(fips_cnty >= 10 & fips_cnty <100, 
                                                 paste0("0", test2),
                                          test2)),
         fips_stc = as.numeric(paste0(test, test2))) %>%
  select(-test, -test2) %>%
arrange(fips_stc, year)
head(blsData)
stat.desc(blsData, desc = FALSE)  


### Harmonize Dataset ----------------------------------------------------------
blsDataH <- fipsClean(blsData)
blsDataH <- blsDataH %>%
  mutate(bls_ueR = (bls_ue/bls_lf)*100)
stat.desc(blsDataH, desc = FALSE)  
  

### Save -----------------------------------------------------------------------
fwrite(blsData, file = paste0("../output/", outdata, ".csv"))
fwrite(blsDataH, file = paste0("../output/", outdata, "H.csv"))  
