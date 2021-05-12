# May 11th, 2021
# UPWP Appendix D Demographic Information
## The purpose of this script is to create the demographic information 
## for all the municipalities in the MPO and the subregions.
## For 2021, use the 2019 ACS 5YR Estimates.

#Columns Required   : Census Fields/Tables
# Total Population  : B02001_001E
# % Minority        : B02001_00E - B02001_002E
# % Low Income HH   : (Agg Income = B19025_001E) (HH = B11001) (HH Income by Ranges = B19001)
  #Poverty Line = 24,900 for a family of 4
  #200% = 49,800 (Looking for % of HH with income below 200% of poverty line)
# % LEP             : C16001 (less than very well)/(Total Population - (B01001_003 +B01001_27) 
  #(pop over 5 with LEP)
# Median Income     : B19013

library(dplyr)
library(tidycensus)
library(tidyverse)
library(stringr)
library(hash)
#set API key
census_api_key("e4bec76221ba04c7df76c7c580659bf1f54ed2c1", install = TRUE)
# First time, reload your environment so you can use the key without restarting R.
readRenviron("~/.Renviron")
# You can check it with:
Sys.getenv("CENSUS_API_KEY")
#set working directory
setwd("M:/UPWP_Appendix_D")

#set up the hashes to represent the relationship between subregions and municipalities
ICC <- c('Arlington', 'Belmont', 'Boston', 'Brookline', 'Cambridge', 'Chelsea', 'Everett', 'Lynn', 'Malden', 
         'Medford','Melrose', 'Nahant', 'Newton', 'Quincy', 'Revere', 'Saugus', 'Somerville', 'Waltham', 
         'Watertown', 'Winthrop')
MAGIC <- c('Acton', 'Bedford', 'Bolton', 'Boxborough', 'Carlisle', 'Concord', 'Hudson', 'Lexington', 'Lincoln',
           'Littleton', 'Maynard', 'Stow', 'Sudbury')
MWRC <- c('Ashland', 'Framingham', 'Holliston', 'Marlborough', 'Natick', 'Southborough', 'Wayland', 
          'Wellesley', 'Weston')
NSPC <- c('Burlington', 'Lynnfield', 'North Reading', 'Reading', 'Stoneham', 'Wakefield', 'Wilmington', 
          'Winchester', 'Woburn')
NSTF <- c('Beverly', 'Danvers', 'Essex', 'Gloucester', 'Hamilton', 'Ipswich', 'Manchester', 'Marblehead',
          'Middleton', 'Peabody', 'Rockport', 'Salem', 'Swampscott', 'Topsfield', 'Wenham')
SSC <- c('Braintree', 'Cohasset', 'Hingham', 'Holbrook', 'Hull', 'Marshfield', 'Norwell', 'Rockland', 
         'Scituate', 'Weymouth')
SWAP <- c('Bellingham', 'Franklin', 'Hopkinton', 'Medway', 'Milford', 'Millis', 'Norfolk', 'Sherborn',
          'Wrentham')
TRIC <- c('Canton', 'Dedham', 'Dover', 'Foxborough', 'Medfield', 'Milton', 'Needham', 'Norwood', 'Randolph',
          'Sharon', 'Walpole', 'Westwood')
hash()
subV <- c(ICC, MAGIC, MWRC, NSPC, NSTF, SSC, SWAP, TRIC)
subS <- c('ICC', "MAGIC", 'MWRC', 'NSPC', 'NSTF', 'SSC', 'SWAP', "TRIC")
subregions <- hash(subS, subV)




