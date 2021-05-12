#Update from May 12th, 2021 - Using ACS2019
# March 23rd, 2020 - Margaret Atkinson for CTPS
# The purpose of this R Script is to collect data for the UPWP Appendix D table at CTPS 
# The data is ACS 2018 5YR estimates at the county subdivision (municipality/town/city) level.
#UPDATE FOR ACS 2019 on May 12th, 2021.

#Load data for UPWP
install.packages("censusapi")
library(censusapi)
library(readxl)
library(readr)
library(plyr)
library(dplyr)
library(stringr)
install.packages("writexl")
library(writexl)

setwd("M:/UPWP_Appendix_D")

apis <- listCensusApis()
View(apis)

#load ACS 2019 5YR SF Appendices
#The Appendices list the tables, table description, table universe
#downloaded from https://www2.census.gov/programs-surveys/acs/summary_file/2019/documentation/tech_docs/
ACS_2019_SF_5YR_Appendices <- read_excel("ACS_2019_SF_5YR_Appendices.xls")

#load ACS 2019 5YR Table Shells
#The Table Shells list and describe all the columns in each table.
#downloaded from https://www.census.gov/programs-surveys/acs/technical-documentation/table-shells.html
#(2019 ACS Detailed Table Shells)
ACS2019_Table_Shells <- read_excel("ACS2019_Table_Shells.xlsx")

#Load Metadata for S1901 (subject table)
#ACSST5Y2018_S1901_metadata <- read_csv("ACSST5Y2018.S1901_metadata_2020-03-23T154139.csv")

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


# Columns:
# Total Population = B02001_001E
# % Minority = (B02001_001E - B02001_002E)/B02001_001E
# % LEP = (B16001_005E + B16001_008E + B16001_011E + B16001_014E + B16001_017E + B16001_020E + B16001_023E +
#       B16001_026E + B16001_029E + B16001_032E + B16001_035E + B16001_038E) / (B00001_001E-(B01001_003E+B01001_027E))
# Median Income = B19013_001E
# % Low Income = (C17002_002E + C17002_003E + C17002_004E + C17002_005E + C17002_006E + C17002_007E)/ B02001_001E
# % of HH that are Low Income currently an incorrect calculation but partial (right idea)


#add and calculate the columns to UPWP_2018df
UPWP_2019df <- data.frame(DTabs$GEO_ID)
UPWP_2019df <- rename(UPWP_2019df, replace =c("GEO_ID"="DTabs.GEO_ID"))
UPWP_2019df$NAME_FULL <- DTabs$NAME
UPWP_2019df$Total_Population <- DTabs$B02001_001E
UPWP_2019df$Minority_Perc <- ((DTabs$B02001_001E - DTabs$B02001_002E)/DTabs$B02001_001E)*100
UPWP_2019df$LEP_Perc <-  ((DTabs$C16001_005E + DTabs$C16001_008E + DTabs$C16001_011E + DTabs$C16001_014E + 
                             DTabs$C16001_017E + DTabs$C16001_020E + DTabs$C16001_023E +DTabs$C16001_026E + 
                             DTabs$C16001_029E + DTabs$C16001_032E + DTabs$C16001_035E + DTabs$C16001_038E) / 
                            (DTabs$B02001_001E - (DTabs$B01001_003E + DTabs$B01001_027E)))*100
UPWP_2019df$Low_Income_Perc <- ((DTabs$C17002_002E + DTabs$C17002_003E + DTabs$C17002_004E + DTabs$C17002_005E + 
                                   DTabs$C17002_006E + DTabs$C17002_007E)/ DTabs$B02001_001E)*100
UPWP_2019df$Median_Income <- DTabs$B19013_001E

#split the NAME column to just be the town names
nametemp <- str_split_fixed(UPWP_2019df$NAME, " town",2)
nametemp <- str_split_fixed(nametemp[,1], " city",2)
nametemp <- str_split_fixed(nametemp[,1], " Town",2)
nametemp <- nametemp[,1]
UPWP_2019df$NAME <- nametemp

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
UPWP_SUB <- UPWP_2019df %>% filter(UPWP_2019df$NAME %in% BRMPO)

#load original table that we are updating
#from Sandy yearly - make new first sheet that is just one header row with NAME for town name field
UPWP_OLD_TABLE <- read_excel("2020-04-28 Appendix D tables TBL SJ FOR LAYOUT.xlsx")

#join the original and new table
UPWP_SUB$NAME[60] <- "Manchester"
UPWP_JOIN <- merge(UPWP_OLD_TABLE,UPWP_SUB, by= "NAME", all=T)

#replace data in columns
UPWP_JOIN$`Total Population` <- UPWP_JOIN$Total_Population
UPWP_JOIN$`Percent Minority` <- UPWP_JOIN$Minority_Perc/100
UPWP_JOIN$`Percentage of Residents Age 5+ with Low English Proficiency` <- UPWP_JOIN$LEP_Perc/100

#rename this column before replacing data because now pop based instead of households (2020 issue not 2021)
#UPWP_JOIN <- rename(UPWP_JOIN, "Percent Low Income" = "Percentage of  Low-Income Households")
UPWP_JOIN$`Percent Low Income` <- UPWP_JOIN$Low_Income_Perc/100

UPWP_JOIN$`Median Income` <- UPWP_JOIN$Median_Income

#final columns (just for user reference, they are the columns we replaced)
cols <- c("Total Population", "Percent Minority", "Percentage of Residents Age 5+ with Low English Proficiency", 
          "Percent Low Income", "Median Income")
#subset to get rid of the extra data
UPWP_SUB_Towns <- UPWP_JOIN[1:12]
UPWP_SUB_Towns$`Median Income` <- UPWP_JOIN$`Median Income`
#UPWP_SUB_Towns <- rename(UPWP_SUB_Towns, "Median Income" = "Median_Income")

#MAPC SUBDIVISIONS CALCULATIONS
#create a MAPC table to collapse to the subdivision (aka sum these columns,take median of median income)
MAPC <- data.frame(UPWP_2019df$NAME)
MAPC$Total_Population <- DTabs$B02001_001E
MAPC$Minority_Pop <- (DTabs$B02001_001E - DTabs$B02001_002E)
MAPC$LEP_Pop <-  (DTabs$C16001_005E + DTabs$C16001_008E + DTabs$C16001_011E + DTabs$C16001_014E + 
                    DTabs$C16001_017E + DTabs$C16001_020E + DTabs$C16001_023E +DTabs$C16001_026E + 
                    DTabs$C16001_029E + DTabs$C16001_032E + DTabs$C16001_035E + DTabs$C16001_038E)
MAPC$Low_Income_Pop <- (DTabs$C17002_002E + DTabs$C17002_003E + DTabs$C17002_004E + DTabs$C17002_005E + 
                          DTabs$C17002_006E + DTabs$C17002_007E)
MAPC$Median_Income <- DTabs$B19013_001E


#MAPC subregions
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
         "Malden","Medford","Melrose","Milton","Quincy","Revere","Saugus","Waltham","Watertown","Winthrop")

subREG <- list(NSTF, NSPC, MAGIC, MWRC, SWAP, TRIC, SSC, ICC)
#I WISH THERE WERE BETTER DICTIONARIES IN R - THIS IS A DUMB WAY TO DO THIS
subReg_Names <- c("NSTF", "NSPC", "MAGIC", "MWRC", "SWAP", "TRIC", "SSC", "ICC")

#add column for subregion into MAPC table
MAPC$SubRegion <- NA

#THIS IS BRILLIANT BUT DON"T RUN BECAUSE OVERLAP
#initialize counter
index <- 0
#add the subregions to the table to allow for aggregation based on the subregion
for (reg in subREG){
  index <- index+1
  MAPC[which(MAPC$UPWP_2018df.NAME %in% reg),7]<- subReg_Names[index]
}
#END THE DON"T RUN SECTION


#get them in separate tables because overlap
ICC_Tab <- data.frame(MAPC[which(MAPC$UPWP_2019df.NAME %in% ICC),])
ICC_Tab$SubRegion <- "ICC"
ICC_Tab$UPWP_2019df.NAME <- as.character(ICC_Tab$UPWP_2019df.NAME)

SSC_Tab <- data.frame(MAPC[which(MAPC$UPWP_2019df.NAME %in% SSC),])
SSC_Tab$SubRegion <- "SSC"
SSC_Tab$UPWP_2019df.NAME <- as.character(SSC_Tab$UPWP_2019df.NAME)

TRIC_Tab <- data.frame(MAPC[which(MAPC$UPWP_2019df.NAME %in% TRIC),])
TRIC_Tab$SubRegion <- "TRIC"
TRIC_Tab$UPWP_2019df.NAME <- as.character(TRIC_Tab$UPWP_2019df.NAME)

SWAP_Tab <- data.frame(MAPC[which(MAPC$UPWP_2019df.NAME %in% SWAP),])
SWAP_Tab$SubRegion <- "SWAP"
SWAP_Tab$UPWP_2019df.NAME <- as.character(SWAP_Tab$UPWP_2019df.NAME)

MWRC_Tab <- data.frame(MAPC[which(MAPC$UPWP_2019df.NAME %in% MWRC),])
MWRC_Tab$SubRegion <- "MWRC"
MWRC_Tab$UPWP_2019df.NAME <- as.character(MWRC_Tab$UPWP_2019df.NAME)

MAGIC_Tab <- data.frame(MAPC[which(MAPC$UPWP_2019df.NAME %in% MAGIC),])
MAGIC_Tab$SubRegion <- "MAGIC"
MAGIC_Tab$UPWP_2019df.NAME <- as.character(MAGIC_Tab$UPWP_2019df.NAME)

NSPC_Tab <- data.frame(MAPC[which(MAPC$UPWP_2019df.NAME %in% NSPC),])
NSPC_Tab$SubRegion <- "NSPC"
NSPC_Tab$UPWP_2019df.NAME <- as.character(NSPC_Tab$UPWP_2019df.NAME)

NSTF_Tab <- data.frame(MAPC[which(MAPC$UPWP_2019df.NAME %in% NSTF),])
NSTF_Tab$SubRegion <- "NSTF"
NSTF_Tab$UPWP_2019df.NAME <- as.character(NSTF_Tab$UPWP_2019df.NAME)

#aggregate based on subregions
ICC_Tab["ICC" ,] <- c("ICC",sum(ICC_Tab$Total_Population), sum(ICC_Tab$Minority_Pop), sum(ICC_Tab$LEP_Pop),
                      sum(ICC_Tab$Low_Income_Pop), median(ICC_Tab$Median_Income), "ICC")
SSC_Tab["SSC" ,] <- c("SSC",sum(SSC_Tab$Total_Population), sum(SSC_Tab$Minority_Pop), sum(SSC_Tab$LEP_Pop),
                      sum(SSC_Tab$Low_Income_Pop), median(SSC_Tab$Median_Income), "SSC")
TRIC_Tab["TRIC" ,] <- c("TRIC",sum(TRIC_Tab$Total_Population), sum(TRIC_Tab$Minority_Pop), sum(TRIC_Tab$LEP_Pop),
                        sum(TRIC_Tab$Low_Income_Pop), median(TRIC_Tab$Median_Income), "TRIC")
SWAP_Tab["SWAP" ,] <- c("SWAP",sum(SWAP_Tab$Total_Population), sum(SWAP_Tab$Minority_Pop), sum(SWAP_Tab$LEP_Pop),
                        sum(SWAP_Tab$Low_Income_Pop), median(SWAP_Tab$Median_Income), "SWAP")
MWRC_Tab["MWRC" ,] <- c("MWRC",sum(MWRC_Tab$Total_Population), sum(MWRC_Tab$Minority_Pop), sum(MWRC_Tab$LEP_Pop),
                        sum(MWRC_Tab$Low_Income_Pop), median(MWRC_Tab$Median_Income), "MWRC")
MAGIC_Tab["MAGIC" ,] <- c("MAGIC",sum(MAGIC_Tab$Total_Population), sum(MAGIC_Tab$Minority_Pop), sum(MAGIC_Tab$LEP_Pop),
                          sum(MAGIC_Tab$Low_Income_Pop), median(MAGIC_Tab$Median_Income), "MAGIC")
NSPC_Tab["NSPC" ,] <- c("NSPC",sum(NSPC_Tab$Total_Population), sum(NSPC_Tab$Minority_Pop), sum(NSPC_Tab$LEP_Pop),
                        sum(NSPC_Tab$Low_Income_Pop), median(NSPC_Tab$Median_Income), "NSPC")
NSTF_Tab["NSTF" ,] <- c("NSTF",sum(NSTF_Tab$Total_Population), sum(NSTF_Tab$Minority_Pop), sum(NSTF_Tab$LEP_Pop),
                        sum(NSTF_Tab$Low_Income_Pop), median(NSTF_Tab$Median_Income), "NSTF")

#merge the subregions
SubRegions <- rbind(ICC_Tab["ICC" ,],SSC_Tab["SSC" ,],TRIC_Tab["TRIC" ,],SWAP_Tab["SWAP" ,],MWRC_Tab["MWRC" ,],
                    MAGIC_Tab["MAGIC" ,],NSPC_Tab["NSPC" ,],NSTF_Tab["NSTF" ,])

#calculate percentage fields
SubRegions$`Percent Minority` <- as.numeric(SubRegions$Minority_Pop)/as.numeric(SubRegions$Total_Population)
SubRegions$`Percentage of Residents Age 5+ with Low English Proficiency` <- as.numeric(SubRegions$LEP_Pop)/as.numeric(SubRegions$Total_Population)
SubRegions$`Percent Low Income` <- as.numeric(SubRegions$Low_Income_Pop)/as.numeric(SubRegions$Total_Population)

SubRegions$`Total Population` <- as.numeric(SubRegions$Total_Population)
SubRegions$`Median Income` <- as.numeric(SubRegions$Median_Income)
SubRegions$NAME <- SubRegions$SubRegion

#get just the columns I need
SubR_FINAL <- SubRegions[8:13]

#put it all together
#ICC
UPWP_SUB_Towns[38,9:13] <- SubR_FINAL["ICC",c(4,1,3,2,5)]
#SCC
UPWP_SUB_Towns[86,9:13] <- SubR_FINAL["SSC",c(4,1,3,2,5)]
#TRIC
UPWP_SUB_Towns[93,9:13] <- SubR_FINAL["TRIC",c(4,1,3,2,5)]
#SWAP
UPWP_SUB_Towns[91,9:13] <- SubR_FINAL["SWAP",c(4,1,3,2,5)]
#MWRC
UPWP_SUB_Towns[60,9:13] <- SubR_FINAL["MWRC",c(4,1,3,2,5)]
#MAGIC
UPWP_SUB_Towns[45,9:13] <- SubR_FINAL["MAGIC",c(4,1,3,2,5)]
#NSPC
UPWP_SUB_Towns[70,9:13] <- SubR_FINAL["NSPC",c(4,1,3,2,5)]
#NSTF
UPWP_SUB_Towns[71,9:13] <- SubR_FINAL["NSTF",c(4,1,3,2,5)]

FINAL <- data.frame(UPWP_SUB_Towns)

#EXPORT TO CSV BABYYYYY
#write.csv(FINAL, "UPWP2020.csv")


UPWP_SUB_Towns$`Median Income`<- as.numeric(UPWP_SUB_Towns$`Median Income`)
write_xlsx(UPWP_SUB_Towns, "UPWP2021.xlsx")
write_xlsx(UPWP_JOIN, "UPWP_EXTRA2021.xlsx")



#MEDIAN INCOME FOR SUBREGION
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

SubRegions <- rbind(ICC_Tab,SSC_Tab,TRIC_Tab,SWAP_Tab,MWRC_Tab,
                    MAGIC_Tab,NSPC_Tab,NSTF_Tab)

#SubRegions <- SubRegions %>% rename("NAME" = "replace")
SubRegions <- SubRegions %>% rename("NAME" = "UPWP_2019df.NAME")

#JOIN TO TOWNS BUT MARKED WITH SUBREGIONS
MAPC_SubRegions <- merge(SubRegions, B19001, by = c("NAME"))


MAPC_19001 <- MAPC_SubRegions[c(1,7,12:28)]
write_xlsx(MAPC_19001, "MAPC_B19001.xlsx")
