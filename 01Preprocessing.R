###########################################################################

rm(list=ls()); gc()
library(dplyr)

#----- ACS 2020: ACS 5-Year Estimates Subject Tables
# https://www.census.gov/acs/www/data/data-tables-and-tools/data-profiles/
# Geos filter: All Census Tracts within United States
acs <- read.csv("../Data/ACS/CensusTract/ACSDP5Y2020.DP05-Data.csv")
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
#var.new <- paste0("Age",var.new)
colnames(Age)[-1]<- var.new

Age<-Age %>% select(tract,unique(var.new)) %>% mutate(across(contains("Age"),as.numeric)) 

#--- Race

# Male,Female, HISP, White (NOT HISP), AFAM (NOT HISP), AIAN (NOT HISP), ASIAN (NOT HISP), NHPI (NOT HISP), OTHERRACE (NOT HISP), TWOMORE (NOT HISP)
var=c("STATE","COUNTY","tract","TOTPOP","Male","Female","HISP","WHITE","AFAM","AIAN","ASIAN","NHPI","OTHERRACE", "TWOMORE")
df.acs=acs %>% select(STATE, COUNTY, tract,DP05_0001E, DP05_0002E, DP05_0003E, 
                      DP05_0071E, DP05_0077E, DP05_0078E, DP05_0079E, DP05_0080E, DP05_0081E, DP05_0082E, DP05_0083E) %>% 
  mutate(across(contains("DP"),as.numeric)) 
names(df.acs) <- var
# 'AIAN and OTHERRACE'to OTHERRACE because AIAN does not exist in AoU data. 
df.acs=df.acs %>% mutate(OTHERRACE=AIAN+OTHERRACE, MINTRY=TOTPOP-WHITE)
df.acs$AIAN <- NULL
#scale <- function(x, denom) round(x/denom, digits=3)

df.acs<- df.acs %>% inner_join(Age, by=join_by("tract"))
scale <- function(x, denom) x/denom
df.acs<- df.acs %>% mutate(across(Male:Female.Age65over, ~ scale(.,TOTPOP)))
df.acs[is.na(df.acs)]<- 0

##############################################################################################################
#----- CDC census tract 
# https://www.atsdr.cdc.gov/placeandhealth/svi/data_documentation_download.html
CDC<-read.csv("../Data/CDC/SVI_2020_US.csv")
CDCp<-read.csv("../Data/CDC/PuertoRico.csv")
CDC <- rbind(CDC, CDCp); rm(CDCp); gc()
var<-names(CDC)[grep("EP_",names(CDC))]
CDC <- CDC %>% select(ST, STATE, ST_ABBR, STCNTY, COUNTY, FIPS,LOCATION, AREA_SQMI, E_TOTPOP, var)
CDC[CDC==-999]<-NA

#--- select variables we need
df.sdoh <- df.acs %>% select(-STATE, -COUNTY) %>% 
  inner_join(CDC %>% select(-EP_MINRTY,-EP_AFAM,-EP_HISP,-EP_ASIAN,-EP_AIAN,-EP_NHPI,-EP_TWOMORE,-EP_OTHERRACE), 
             by=join_by("tract"=="FIPS")) 
df.sdoh[,grep("EP_",names(df.sdoh))]<-df.sdoh[,grep("EP_",names(df.sdoh))]/100
# df.sdoh <- df.sdoh %>% select(ST, STATE, ST_ABBR, STCNTY, COUNTY, tract, LOCATION, AREA_SQMI, TOTPOP, 
#                               Male, Female,
#                               HISP, WHITE, AFAM, AIAN, ASIAN, NHPI, OTHERRACE, TWOMORE, MINTRY,
#                               EP_POV150, EP_UNEMP, EP_HBURD, EP_NOHSDP, EP_UNINSUR, EP_AGE65, EP_AGE17,
#                               EP_DISABL, EP_SNGPNT,EP_LIMENG, EP_MUNIT, EP_MOBILE, EP_CROWD, EP_NOVEH, EP_GROUPQ, EP_NOINT)
names(df.sdoh)<-gsub("EP_","",names(df.sdoh))


save(df.sdoh,file="01AggregatedSDoH/RData/02AggregatedSDoH_data.RData")
