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
#library(hash)
library(censusapi)
library(readxl)
library(readr)
library(dplyr)
library(writexl)

setwd("M:/UPWP_Appendix_D")

#load ACS 2019 5YR SF Appendices
#The Appendices list the tables, table description, table universe
#downloaded from https://www2.census.gov/programs-surveys/acs/summary_file/2019/documentation/tech_docs/
ACS_2019_SF_5YR_Appendices <- read_excel("ACS_2019_SF_5YR_Appendices.xlsx")

#load ACS 2019 5YR Table Shells
#The Table Shells list and describe all the columns in each table.
#downloaded from https://www.census.gov/programs-surveys/acs/technical-documentation/table-shells.html
#(2019 ACS Detailed Table Shells)
ACS2019_Table_Shells <- read_excel("ACS2019_Table_Shells.xlsx")

#censusapi trial
#attempt to retrieve census data with the censusapi
key = "e4bec76221ba04c7df76c7c580659bf1f54ed2c1"

# retrieve census data for the specific columns listed in vars
# vars = total population, white alone population, total population over age 5, LEP population, Median Income, 
#       total # Households, 
# B02001_001E = total population
# B02001_002E = white alone population
# B19013_001E = median income
# C16001_005E, C16001_008E, C16001_011E, C16001_014E, C16001_017E, C16001_020E, C16001_023E, C16001_026E,
#        C16001_029E, C16001_032E, C16001_035E, C16001_038E = (sum) speaks english less than "very well"
# B01001_003E, B01001_027E = male, female population age 5 and under
# B11001_001E = total number of households (unneccesary?)
# C17002 = Ratio Of Income To Poverty Level In The Past 12 Months
#       C17002_002E,C17002_003E,C17002_004E,C17002_005E,C17002_006E,C17002_007E = (sum) # individuals below 200% of poverty level

DTabs <- getCensus(name = "acs/acs5", vintage = 2019,key = key ,
                   vars = c("NAME","GEO_ID","B02001_001E","B02001_002E","B11001_001E","B19013_001E", 
                            "B01001_003E", "B01001_027E", 
                            "C17002_002E", "C17002_003E", "C17002_004E", "C17002_005E", "C17002_006E", "C17002_007E",
                            "C16001_005E", "C16001_008E", "C16001_011E", "C16001_014E", "C16001_017E", "C16001_020E",
                            "C16001_023E", "C16001_026E", "C16001_029E", "C16001_032E", "C16001_035E", "C16001_038E"),
                   region = "county subdivision:*", regionin = "state:25")


Totals <- data.frame(DTabs$NAME)
Totals$Total_Population <- DTabs$B02001_001E
Totals$FivePlus_Population <- (DTabs$B02001_001E - (DTabs$B01001_003E + DTabs$B01001_027E))
Totals$Minority_Pop <- (DTabs$B02001_001E - DTabs$B02001_002E)
Totals$LEP_Pop <-  (DTabs$C16001_005E + DTabs$C16001_008E + DTabs$C16001_011E + DTabs$C16001_014E + 
                      DTabs$C16001_017E + DTabs$C16001_020E + DTabs$C16001_023E +DTabs$C16001_026E + 
                      DTabs$C16001_029E + DTabs$C16001_032E + DTabs$C16001_035E + DTabs$C16001_038E)
Totals$Low_Income_Pop <- (DTabs$C17002_002E + DTabs$C17002_003E + DTabs$C17002_004E + DTabs$C17002_005E + 
                            DTabs$C17002_006E + DTabs$C17002_007E)
Totals$Median_Income <- DTabs$B19013_001E

#split the NAME column to just be the town names
nametemp <- str_split_fixed(Totals$DTabs.NAME, " town",2)
nametemp <- str_split_fixed(nametemp[,1], " city",2)
nametemp <- str_split_fixed(nametemp[,1], " Town",2)
nametemp <- nametemp[,1]
Totals$NAME <- nametemp

#subset to just the towns/cities within the Boston Region MPO
BRMPO <- c("Beverly","Boston","Braintree","Cambridge","Chelsea","Everett","Framingham","Franklin","Gloucester","Lynn",
           "Malden","Marlborough","Medford","Melrose","Newton","Peabody","Quincy","Revere","Salem","Somerville",
           "Waltham","Watertown","Weymouth","Woburn","Acton","Arlington","Ashland","Bedford","Bellingham","Belmont",
           "Bolton","Boxborough","Brookline","Burlington","Canton","Carlisle","Cohasset","Concord","Danvers","Dedham",
           "Dover","Essex","Foxborough","Hamilton","Hingham","Holbrook","Holliston","Hopkinton","Hudson","Hull","Ipswich",
           "Lexington","Lincoln","Littleton","Lynnfield","Manchester-by-the-Sea","Marblehead","Marshfield","Maynard","Medfield",
           "Medway","Middleton","Milford","Millis","Milton","Nahant","Natick","Needham","Norfolk","North Reading","Norwell",
           "Norwood","Randolph","Reading","Rockland","Rockport","Saugus","Scituate","Sharon","Sherborn","Southborough",
           "Stoneham","Stow","Sudbury","Swampscott","Topsfield","Wakefield","Walpole","Wayland","Wellesley","Wenham",
           "Weston","Westwood","Wilmington","Winchester","Winthrop","Wrentham")
MPO_Totals <- Totals %>% filter(Totals$NAME %in% BRMPO)
MPO_Totals <- MPO_Totals[2:8] #get rid of long name

#create grand total row (NOTE THIS DOES NOT INCLUDE MEDIAN INCOME)
MPO_Totals <- MPO_Totals %>% add_row(Total_Population = sum(MPO_Totals$Total_Population),
                                     FivePlus_Population = sum(MPO_Totals$FivePlus_Population),
                                     Minority_Pop = sum(MPO_Totals$Minority_Pop),
                                     LEP_Pop = sum(MPO_Totals$LEP_Pop),
                                     Low_Income_Pop = sum(MPO_Totals$Low_Income_Pop),
                                     NAME = 'Grand Total')

#Do the Calculations for SubRegions Section:
#MAPC subregions make hash
NSTF <- c("Beverly","Danvers","Essex","Gloucester","Hamilton","Ipswich","Manchester-by-the-Sea","Marblehead",
          "Middleton","Nahant","Peabody","Rockport","Salem","Swampscott","Topsfield","Wenham")

NSPC <- c("Woburn","Burlington","Lynnfield","North Reading","Reading","Stoneham","Wakefield","Wilmington",
          "Winchester")

MAGIC <- c("Acton","Lexington","Bedford","Bolton","Boxborough","Carlisle","Concord","Hudson","Littleton","Lincoln",
           "Maynard","Stow","Sudbury")

MWRC <- c("Framingham","Ashland","Holliston","Marlborough","Natick","Southborough","Wayland","Wellesley","Weston")

SWAP <- c("Medway","Bellingham","Dover","Franklin","Hopkinton","Milford","Millis","Norfolk","Sherborn","Wrentham")

TRIC <- c("Norwood","Canton","Dedham","Dover","Foxborough","Medfield","Milton","Needham","Randolph","Sharon",
          "Walpole","Westwood")

SSC <- c("Rockland","Braintree","Cohasset","Hingham","Holbrook","Hull","Marshfield","Norwell","Scituate","Weymouth")

ICC <- c("Somerville","Arlington","Everett","Newton","Belmont","Boston","Brookline","Cambridge","Chelsea","Lynn",
         "Malden","Medford","Melrose","Milton", "Needham","Quincy","Revere","Saugus","Waltham","Watertown","Winthrop")

subREG <- list(NSTF, NSPC, MAGIC, MWRC, SWAP, TRIC, SSC, ICC)
subReg_Names <- c("NSTF", "NSPC", "MAGIC", "MWRC", "SWAP", "TRIC", "SSC", "ICC")
#subHash <- hash(subReg_Names, subREG)

#make tables for subregions
#get them in separate tables because overlap
ICC_Tab <- data.frame(MPO_Totals[which(MPO_Totals$NAME %in% ICC),])
ICC_Tab$SubRegion <- "ICC"

SSC_Tab <- data.frame(MPO_Totals[which(MPO_Totals$NAME %in% SSC),])
SSC_Tab$SubRegion <- "SSC"

TRIC_Tab <- data.frame(MPO_Totals[which(MPO_Totals$NAME %in% TRIC),])
TRIC_Tab$SubRegion <- "TRIC"

SWAP_Tab <- data.frame(MPO_Totals[which(MPO_Totals$NAME %in% SWAP),])
SWAP_Tab$SubRegion <- "SWAP"

MWRC_Tab <- data.frame(MPO_Totals[which(MPO_Totals$NAME %in% MWRC),])
MWRC_Tab$SubRegion <- "MWRC"

MAGIC_Tab <- data.frame(MPO_Totals[which(MPO_Totals$NAME %in% MAGIC),])
MAGIC_Tab$SubRegion <- "MAGIC"

NSPC_Tab <- data.frame(MPO_Totals[which(MPO_Totals$NAME %in% NSPC),])
NSPC_Tab$SubRegion <- "NSPC"

NSTF_Tab <- data.frame(MPO_Totals[which(MPO_Totals$NAME %in% NSTF),])
NSTF_Tab$SubRegion <- "NSTF"

#aggregate based on subregions
ICC_Tab <- ICC_Tab %>% add_row(NAME = "ICC",
                               Total_Population= sum(ICC_Tab$Total_Population),
                               FivePlus_Population = sum(ICC_Tab$FivePlus_Population), 
                               Minority_Pop = sum(ICC_Tab$Minority_Pop), 
                               LEP_Pop = sum(ICC_Tab$LEP_Pop),
                               Low_Income_Pop = sum(ICC_Tab$Low_Income_Pop), 
                               Median_Income = median(ICC_Tab$Median_Income), 
                               SubRegion = "ICC")
SSC_Tab <- SSC_Tab %>% add_row(NAME = "SSC",
                               Total_Population= sum(SSC_Tab$Total_Population),
                               FivePlus_Population = sum(SSC_Tab$FivePlus_Population), 
                               Minority_Pop = sum(SSC_Tab$Minority_Pop), 
                               LEP_Pop = sum(SSC_Tab$LEP_Pop),
                               Low_Income_Pop = sum(SSC_Tab$Low_Income_Pop), 
                               Median_Income = median(SSC_Tab$Median_Income), 
                               SubRegion = "SSC")
TRIC_Tab <- TRIC_Tab %>% add_row(NAME = "TRIC",
                                 Total_Population= sum(TRIC_Tab$Total_Population),
                                 FivePlus_Population = sum(TRIC_Tab$FivePlus_Population), 
                                 Minority_Pop = sum(TRIC_Tab$Minority_Pop), 
                                 LEP_Pop = sum(TRIC_Tab$LEP_Pop),
                                 Low_Income_Pop = sum(TRIC_Tab$Low_Income_Pop), 
                                 Median_Income = median(TRIC_Tab$Median_Income), 
                                 SubRegion = "TRIC")
SWAP_Tab <- SWAP_Tab %>% add_row(NAME = "SWAP",
                                 Total_Population= sum(SWAP_Tab$Total_Population),
                                 FivePlus_Population = sum(SWAP_Tab$FivePlus_Population), 
                                 Minority_Pop = sum(SWAP_Tab$Minority_Pop), 
                                 LEP_Pop = sum(SWAP_Tab$LEP_Pop),
                                 Low_Income_Pop = sum(SWAP_Tab$Low_Income_Pop), 
                                 Median_Income = median(SWAP_Tab$Median_Income), 
                                 SubRegion = "SWAP")
MWRC_Tab <- MWRC_Tab %>% add_row(NAME = "MWRC",
                                 Total_Population= sum(MWRC_Tab$Total_Population),
                                 FivePlus_Population = sum(MWRC_Tab$FivePlus_Population), 
                                 Minority_Pop = sum(MWRC_Tab$Minority_Pop), 
                                 LEP_Pop = sum(MWRC_Tab$LEP_Pop),
                                 Low_Income_Pop = sum(MWRC_Tab$Low_Income_Pop), 
                                 Median_Income = median(MWRC_Tab$Median_Income), 
                                 SubRegion = "MWRC")
MAGIC_Tab <- MAGIC_Tab %>% add_row(NAME = "MAGIC",
                                   Total_Population= sum(MAGIC_Tab$Total_Population),
                                   FivePlus_Population = sum(MAGIC_Tab$FivePlus_Population), 
                                   Minority_Pop = sum(MAGIC_Tab$Minority_Pop), 
                                   LEP_Pop = sum(MAGIC_Tab$LEP_Pop),
                                   Low_Income_Pop = sum(MAGIC_Tab$Low_Income_Pop), 
                                   Median_Income = median(MAGIC_Tab$Median_Income), 
                                   SubRegion = "MAGIC")
NSPC_Tab <- NSPC_Tab %>% add_row(NAME = "NSPC",
                                 Total_Population= sum(NSPC_Tab$Total_Population),
                                 FivePlus_Population = sum(NSPC_Tab$FivePlus_Population), 
                                 Minority_Pop = sum(NSPC_Tab$Minority_Pop), 
                                 LEP_Pop = sum(NSPC_Tab$LEP_Pop),
                                 Low_Income_Pop = sum(NSPC_Tab$Low_Income_Pop), 
                                 Median_Income = median(NSPC_Tab$Median_Income), 
                                 SubRegion = "NSPC")
NSTF_Tab <- NSTF_Tab %>% add_row(NAME = "NSTF",
                                 Total_Population= sum(NSTF_Tab$Total_Population),
                                 FivePlus_Population = sum(NSTF_Tab$FivePlus_Population), 
                                 Minority_Pop = sum(NSTF_Tab$Minority_Pop), 
                                 LEP_Pop = sum(NSTF_Tab$LEP_Pop),
                                 Low_Income_Pop = sum(NSTF_Tab$Low_Income_Pop), 
                                 Median_Income = median(NSTF_Tab$Median_Income), 
                                 SubRegion = "NSTF")

#merge the subregions
SubRegions <- rbind((ICC_Tab %>% filter(NAME == "ICC")), (SSC_Tab %>% filter(NAME == "SSC")),
                    (TRIC_Tab %>% filter(NAME == "TRIC")), (SWAP_Tab %>% filter(NAME == "SWAP")),
                    (MWRC_Tab %>% filter(NAME == "MWRC")),(MAGIC_Tab %>% filter(NAME == "MAGIC")),
                    (NSPC_Tab %>% filter(NAME == "NSPC")),(NSTF_Tab %>% filter(NAME == "NSTF")))

#merge the grand total and town data with subregions
FullTotals <- rbind(MPO_Totals, SubRegions[1:7])

# Columns:
# Total Population = B02001_001E
# % Minority = (B02001_001E - B02001_002E)/B02001_001E
# % LEP = (B16001_005E + B16001_008E + B16001_011E + B16001_014E + B16001_017E + B16001_020E + B16001_023E +
#       B16001_026E + B16001_029E + B16001_032E + B16001_035E + B16001_038E) / (B00001_001E-(B01001_003E+B01001_027E))
# Median Income = B19013_001E
# % Low Income = (C17002_002E + C17002_003E + C17002_004E + C17002_005E + C17002_006E + C17002_007E)/ B02001_001E
# % of HH that are Low Income currently an incorrect calculation but partial (right idea)

FullTotals$Minority_Perc <- FullTotals$Minority_Pop/FullTotals$Total_Population
FullTotals$LEP_Perc <- FullTotals$LEP_Pop/FullTotals$FivePlus_Population
FullTotals$Low_Income_Perc <- FullTotals$Low_Income_Pop/FullTotals$Total_Population

#get subset of just useful columns at this point
FullPerc <- FullTotals %>% select(NAME, Total_Population, Minority_Perc, LEP_Perc, Low_Income_Perc, Median_Income)

#export output table
write_xlsx(FullPerc, "UPWP_Totals_2021.xlsx")

#MEDIAN INCOME FOR SUBREGIONS AND GRAND TOTAL
B19001 <- getCensus(name = "acs/acs5", vintage = 2019,
                    key = key,vars = c("NAME","GEO_ID","B19001_001E","B19001_002E",
                                       "B19001_003E","B19001_004E","B19001_005E","B19001_006E","B19001_007E",
                                       "B19001_008E","B19001_009E","B19001_010E","B19001_011E","B19001_012E",
                                       "B19001_013E","B19001_014E","B19001_015E","B19001_016E","B19001_017E"),
                    region = "county subdivision:*", regionin = "state:25")

nametemp <- str_split_fixed(B19001$NAME, " town",2)
nametemp <- str_split_fixed(nametemp[,1], " city",2)
nametemp <- str_split_fixed(nametemp[,1], " Town",2)
nametemp <- nametemp[,1]
B19001$NAME <- nametemp

SubRegions2 <- rbind(ICC_Tab,SSC_Tab,TRIC_Tab,SWAP_Tab,MWRC_Tab,
                    MAGIC_Tab,NSPC_Tab,NSTF_Tab)

#SubRegions <- SubRegions %>% rename("NAME" = "replace")
#SubRegions <- SubRegions %>% rename("NAME" = "UPWP_2019df.NAME")

#JOIN TO TOWNS BUT MARKED WITH SUBREGIONS
MAPC_SubRegions <- merge(SubRegions2, B19001, by = c("NAME"))


MAPC_19001 <- MAPC_SubRegions[c(1,7,8,13:29)]

#get sums of households in each range of income for each subregion
MEDINC_HH <- MAPC_19001 %>% 
  group_by(SubRegion) %>% 
  summarise(across(c(B19001_001E,B19001_002E,
                     B19001_003E,B19001_004E,B19001_005E,B19001_006E,B19001_007E,
                     B19001_008E,B19001_009E,B19001_010E,B19001_011E,B19001_012E,
                     B19001_013E,B19001_014E,B19001_015E,B19001_016E,B19001_017E), sum))
#get total sum of households in each range of income for the whole region (can't use previous because doubles)
GT_HH <- B19001 %>% 
  filter(B19001$NAME %in% BRMPO) %>%
  summarise(across(c(B19001_001E,B19001_002E,
                     B19001_003E,B19001_004E,B19001_005E,B19001_006E,B19001_007E,
                     B19001_008E,B19001_009E,B19001_010E,B19001_011E,B19001_012E,
                     B19001_013E,B19001_014E,B19001_015E,B19001_016E,B19001_017E), sum))
GT_HH$SubRegion <- "Grand Total"

#merge the subregions and GT
MEDINC_HH <- rbind(MEDINC_HH, GT_HH)

#export for use in linear interpolation spreadsheet from paul.
write_xlsx(MEDINC_HH, "MEDINC_HH.xlsx")

#At this point, the only thing that needs updating is the Median Income for the Grand Total and all Subregions
#Take the MEDINC_HH and put as a sheet in median_income_estimation_2019.xlsx and copy it into the 
#columns H through W in the subregions_median_calc sheet. The rest will calculate for you. 
#Then copy the interpolated Median Incomes into UPWP_Totals_2021.xlsx. 
#Resave as UPWP_Totals_2021_wMI.xlsx, and update all subregions to be like: ICC Subtotals 
#Then all you have to do is make sure the format matches the previous years.


#load original table that we are updating
#from Sandy yearly - make new first sheet that is just one header row with NAME for town name field
UPWP_OLD_TABLE <- read_excel("2020-04-28 Appendix D tables TBL SJ FOR LAYOUT.xlsx")
#read in excel file representing FullPerc but with median incomes updated outside R (in excel, see above)
FINAL <- read_excel("UPWP_Totals_2021_wMI.xlsx")

#this is sketch, make sure Manchester-by-the-Sea is really 34 before running.
FINAL$NAME[34] <- "Manchester"
#join to the old table so that I can copy the the format (this keeps order)
UPWP_JOIN <- left_join(UPWP_OLD_TABLE,FINAL, by= "NAME")
#update columns
UPWP_JOIN$`Total Population` <- UPWP_JOIN$Total_Population
UPWP_JOIN$`Percent Minority` <- UPWP_JOIN$Minority_Perc
UPWP_JOIN$`Percentage of  Residents in Poverty` <- UPWP_JOIN$Low_Income_Perc
UPWP_JOIN$`Percentage of Residents Age 5+ with Low English Proficiency` <- UPWP_JOIN$LEP_Perc
UPWP_JOIN$`Household Median Income` <- UPWP_JOIN$Median_Income

#drop unnecessary columns
dropCols <- c('Total_Population', 'Minority_Perc', 'Low_Income_Perc', 'LEP_Perc', 'Median_Income')
UPWP <- UPWP_JOIN %>% select(-one_of(dropCols))

write_xlsx(UPWP, "UPWP_Updated21.xlsx")
