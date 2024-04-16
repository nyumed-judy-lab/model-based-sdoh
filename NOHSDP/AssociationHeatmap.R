####################################################################################
### Heatmap of state-specific associations 
### between no high school diploma and demographic covariates
####################################################################################

rm(list=ls()); gc()
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(lmerTest)
library(ComplexHeatmap)
library(data.table)

#####################################################################################################
#----- For Annotation
#----- ACS 2020: ACS 5-Year Estimates Subject Tables
# https://www.census.gov/programs-surveys/acs/data.html
# ACS S1051 Table
# TABLE ID: ACSST5Y2020.S1501
# TABLE Title: ACS Educational Attainment
# Geos filter: State -> All states within United States, Puerto Rica, and the Island Areas
dat<- fread("Data/ACS/State/ACSST5Y2020.S1501-2024-04-16T194408.csv")

# Check the data table and extract necessary data
var=colnames(dat)[grepl("Percent!!Estimate",colnames(dat))]
var=c(colnames(dat)[1],var)
dat <- dat %>% select(all_of(var))

rowvar<- dat[,1]
rowvar
rowvar[7] # Population 25 years and over
rowvar[15] # High school graduate or higher

dat <- dat[15,-1]
dat=dat %>% mutate_at(vars(contains("Percent")), ~(as.numeric(gsub("[\\%,]", "", .) )))
dat=data.frame(STATE=gsub("!!Percent!!Estimate", "", colnames(dat)), HSDP_rate=t(dat[1,]))

load(file="NOHSDP/Association.RData")

# for state abbreviation and state region annotation
temp <- data.frame(ST_ABBR=state.abb, Region=state.region, Division=state.division)
state.tb <- state.tb %>% left_join(temp, by=join_by("ST_ABBR"))
rm(temp);gc()

#--- set different color vectors for each interval
mycol <- rev(brewer.pal(11,"RdBu"))



#####################################################################################################
# Mixed-effects Coefficients for all states
mixef.mat<- coef(fit.x)$STATE 
names(mixef.mat) <- gsub("bar","", names(mixef.mat))


rownames(mixef.mat)<- state.tb$ST_ABBR[match(rownames(mixef.mat), state.tb$STATE)]
mixef.mat$ST_ABBR=row.names(mixef.mat)
mixef.mat <- mixef.mat %>% 
  left_join(state.tb %>% select(ST, STATE, ST_ABBR, Region, Division), by=join_by(ST_ABBR)) %>%
  arrange(ST)


# mat is the associations and annotation is for the annotation
mat <- mixef.mat %>% select(-ST, -STATE, -ST_ABBR, -Region, -Division)
annotation <- mixef.mat %>% left_join(dat, by="STATE") %>% select(Region, HSDP_rate)
rownames(annotation) <- rownames(mat) <- mixef.mat$ST_ABBR
annotation<- annotation %>% mutate('NOHSDP Rate' = 100-HSDP_rate) %>% select(-HSDP_rate)

# Color of annotations
setcol <- brewer.pal(9, "Set1")
annocol <- list(Region=c(`Northeast`=setcol[3], `South`=setcol[4],`North Central`=setcol[6], `West`=setcol[8]),
                'NOHSDP Rate' = c("white", "darkolivegreen"))

# rename variables
mat<- mat %>% rename(NHB="AFAM", Hispanic="HISP", 
                     Twomore="TWOMORE", 
                     Asian="ASIAN", Other="OTHERRACE",
                     `Age 25-34` = "Age25_34", `Age 35-44` ="Age35_44", 
                     `Age 45-54` ="Age45_54", `Age 65-74` = "Age65_74", 
                     `Age 75+` = "Age75over")

# draw heatmap
thres=3
bk= seq(-thres, thres, length=11)
row_dend.std = hclust(dist(scale(as.matrix(mat)))) # hierarchical clustering

p<- ComplexHeatmap::pheatmap(as.matrix(mat),
                             show_rownames=T, show_colnames=T, 
                             col=mycol,cluster_cols=T, cluster_rows=row_dend.std, breaks=bk,
                             cellwidth=30, cellheight=11, cutree_rows = NA, 
                             fontsize=12,
                             show_row_dend = FALSE, show_column_dend = FALSE,
                             annotation_row=annotation,
                             annotation_colors = annocol, row_names_side = "left",
                             annotation_legend = TRUE,
                             heatmap_legend_param = list(title = "Coefficient   ", at = seq(-thres, thres, length=7)))

draw(p, heatmap_legend_side = "right", annotation_legend_side = "right",
     legend_grouping = "original")

# png(paste0("fig/NOHSDPHeatmap.png"), units="in", height=9.3, width=8, res=300)
# draw(p, heatmap_legend_side = "right", annotation_legend_side = "right",
#      legend_grouping = "original")
# dev.off()
