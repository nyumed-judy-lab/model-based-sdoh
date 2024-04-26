####################################################################################
### Data Preprocessing
####################################################################################

rm(list=ls()); gc()
library(dplyr)

#----- ACS 2020: ACS 5-Year Estimates Subject Tables
# https://www.census.gov/programs-surveys/acs/data.html
# View Popular ACS Tables -> Demographic and Housing Estimates
# TABLE ID: ACSDP5Y2020.DP05
# TABLE Title: ACS DEMOGRAPHIC AND HOUSING ESTIMATES
# Geos filter: Census Tract -> All Census Tracts within United States
acs <- read.csv("Data/ACS/CensusTract/ACSDP5Y2020.DP05-Data.csv")
var.names<- as.character(acs[1,])
acs<- acs[-1,]

#--- extract census tract
acs$tract<- as.numeric(gsub("1400000US","",acs$GEO_ID))

#--- extract STATE, County, Borough
temp<- strsplit(as.character(acs$NAME), split=', ')
acs$COUNTY<- sapply(temp, function(x) gsub(" County","",x[2]))
acs$STATE <- sapply(temp, function(x) x[3])

#--- Age
#r.index=10:66
idx=(grepl("Estimate!!SEX AND AGE!!Total population!!",var.names)) &
  (grepl("years",var.names)) &
  (!grepl("Median",var.names)) & (!grepl("Sex ratio",var.names))
Age<- data.frame(tract=acs$tract, acs[idx])
var<- var.names[idx]
var.new=strsplit(as.character(var), split='!!')

var.new<- sapply(var.new, function(x) paste0(x[5],".Age",x[4]))
var.new <- gsub(" to ", "_", var.new)
var.new <- gsub("NA.", "", var.new)
var.new <- gsub(" years and over", "over", var.new)
var.new <- gsub(" years", "", var.new)
var.new <- gsub("Under 18", "18under", var.new)
var.new <- gsub("Under 5", "5under", var.new)
colnames(Age)[-1]<- var.new

Age<-Age %>% select(tract,unique(var.new)) %>% mutate(across(contains("Age"),as.numeric)) 

#--- Sex & Race/Ethnicity

# Male,Female, HISP, White (NOT HISP), AFAM (NOT HISP), AIAN (NOT HISP), ASIAN (NOT HISP), NHPI (NOT HISP), OTHERRACE (NOT HISP), TWOMORE (NOT HISP)
var=c("STATE","COUNTY","tract","TOTPOP","Male","Female","HISP","WHITE","AFAM","AIAN","ASIAN","NHPI","OTHERRACE", "TWOMORE")

idx=(grepl("Estimate!!SEX AND AGE!!Total population",var.names)) &
  (!grepl("years",var.names)) & (!grepl("Sex ratio",var.names))
idx2=which(var.names%in%c("Estimate!!HISPANIC OR LATINO AND RACE!!Total population!!Hispanic or Latino (of any race)",
                          paste0("Estimate!!HISPANIC OR LATINO AND RACE!!Total population!!Not Hispanic or Latino!!", 
                                 c("White alone", "Black or African American alone",
                               "American Indian and Alaska Native alone", "Asian alone",
                               "Native Hawaiian and Other Pacific Islander alone","Some other race alone",
                               "Two or more races"))))

df.acs<- data.frame(STATE=acs$STATE, COUNTY=acs$COUNTY, tract=acs$tract, acs[,which(idx==1)], acs[,idx2])

df.acs=df.acs %>% mutate(across(contains("DP"),as.numeric)) 
names(df.acs) <- var

# 'AIAN and OTHERRACE'to OTHERRACE because AIAN does not exist in AoU data. 
df.acs=df.acs %>% mutate(OTHERRACE=AIAN+OTHERRACE, MINTRY=TOTPOP-WHITE) %>% select(-AIAN)

df.acs<- df.acs %>% inner_join(Age, by=join_by("tract"))
scale <- function(x, denom) x/denom
df.acs<- df.acs %>% mutate(across(Male:Female.Age65over, ~ scale(.,TOTPOP)))
df.acs[is.na(df.acs)]<- 0

##############################################################################################################
# We utilized a well-structured data source, which consisted of aggregated data derived from the ACS 2020 5-year dataset
# For further information, please refer to the 'CDC/ATSDR SVI Documentation 2020'.

#----- CDC/ATSDR SVI Data
# https://www.atsdr.cdc.gov/placeandhealth/svi/data_documentation_download.html
# Year: 2020, Geography: United States/Puerto Rico, Geography Type: Census Tracts
CDC<-read.csv("Data/CDC/SVI_2020_US.csv")
CDCp<-read.csv("Data/CDC/PuertoRico.csv")
CDC <- rbind(CDC, CDCp); rm(CDCp); gc()
var<-names(CDC)[grep("EP_",names(CDC))]
CDC <- CDC %>% select(ST, STATE, ST_ABBR, STCNTY, COUNTY, FIPS, LOCATION, AREA_SQMI, E_TOTPOP, all_of(var))
CDC[CDC==-999]<-NA

#--- select variables we need
df.sdoh <- df.acs %>% select(-STATE, -COUNTY) %>% 
  inner_join(CDC %>% select(-EP_MINRTY,-EP_AFAM,-EP_HISP,-EP_ASIAN,-EP_AIAN,-EP_NHPI,-EP_TWOMORE,-EP_OTHERRACE), 
             by=join_by("tract"=="FIPS")) 
df.sdoh[,grep("EP_",names(df.sdoh))]<-df.sdoh[,grep("EP_",names(df.sdoh))]/100
names(df.sdoh)<-gsub("EP_","",names(df.sdoh))

# combine age variables
df.sdoh <- df.sdoh %>% mutate(Age5_14=Age5_9+Age10_14,
                              Age55_64 = Age55_59+Age60_64) %>%
  select(ST, STATE, ST_ABBR, STCNTY, COUNTY, tract, LOCATION, AREA_SQMI, TOTPOP,
         Male, Female, HISP, WHITE, AFAM, ASIAN, NHPI, OTHERRACE, TWOMORE,
         Age5under, Age5_14, Age15_19, Age20_24, Age25_34, Age35_44, Age45_54, Age55_64,
         Age65_74, Age75_84, Age85over, NOHSDP, UNINSUR)

# remove 63 Census tracts that total population is less then 10.
df.sdoh=df.sdoh %>% filter(TOTPOP>=10)

#--- Save Preprocessed Aggregated Data
save(df.sdoh, file="Preprocessing/AggregatedSDoH_data.RData")

