
#Example script of analysis using FLTag##

library(spatial)
library(rio)
library(data.table)
library(sp)
library(doBy)
library(rgdal)
library(RColorBrewer)
library(ggplot2)
library(vmstools)
library(mgcv)
library(maps)
library(mapdata)
library(reshape2)
library(rgeos)
library(lattice)
library(pander)
library(kfigr)
library(knitr)
library(ggmap)
library(effdisR)
library(latticeExtra)
library(RODBC)
library(leaflet)
library(zoo)
library(FLTag)

# attach to database

odbcCloseAll()
aottp <-  odbcConnect("aottp-local", case ="postgresql", believeNRows=FALSE)

# get AOTTP data
releases <- sqlQuery(aottp, "SELECT * from releases WHERE chktagcanceled ='false';")
recoveries <- sqlQuery(aottp, "SELECT * from recoveries WHERE rcstagecode LIKE 'RCF';")

# check data

head(releases);dim(releases)
head(recoveries);dim(recoveries)

# put on recoveries matching on specimenid using matchTagsA

rel_rec <- matchTagsA(rels=releases,recs=recoveries,mtch='specimenid')

# Change factors to characters and generate R-format timestamps

rel_rec <- formatTagdata(rel_rec=rel_rec)

# Convert weights to lengths

rel_rec$kg <- NA

rel_rec$kg[rel_rec$speciescode == 'BET'] <- lenW_BET(lf=rel_rec$len[rel_rec$speciescode == 'BET'])
rel_rec$kg[rel_rec$speciescode == 'SKJ'] <- lenW_SKJ(lf=rel_rec$len[rel_rec$speciescode == 'SKJ'])
rel_rec$kg[rel_rec$speciescode == 'YFT'] <- lenW_YFT(lf=rel_rec$len[rel_rec$speciescode == 'YFT'])
rel_rec$kg[rel_rec$speciescode == 'LTA'] <- lenW_LTA(lf=rel_rec$len[rel_rec$speciescode == 'LTA'])

rel_rec$rec_kg <- NA

rel_rec$rec_kg[rel_rec$speciescode == 'BET'] <- lenW_BET(lf=rel_rec$rec_len[rel_rec$speciescode == 'BET'])
rel_rec$rec_kg[rel_rec$speciescode == 'SKJ'] <- lenW_SKJ(lf=rel_rec$rec_len[rel_rec$speciescode == 'SKJ'])
rel_rec$rec_kg[rel_rec$speciescode == 'YFT'] <- lenW_YFT(lf=rel_rec$rec_len[rel_rec$speciescode == 'YFT'])
rel_rec$rec_kg[rel_rec$speciescode == 'LTA'] <- lenW_LTA(lf=rel_rec$rec_len[rel_rec$speciescode == 'LTA'])


# Add on useful time vectors, e.g. julian day, month, year

rel_rec <- timeVectors(rel_rec=rel_rec)

# Add on useful spatial information

rel_rec <- spatialVectors(rel_rec=rel_rec)


# Simplify rel_rec

rel_rec <- rel_rec[,c("speciescode","rcstagecode","electronictagcode1","ctcode1","ctcode2","date","time","latitude","longitude","gearcode","depth","len","taggerid",
                      "persontypecode","chktagcanceled","specimenid","taggrpid","rcstageid","surveycode","zone","areazone","rec_longitude","rec_latitude","rec_len",
                      "rec_gearcode","rec_date","rec_time","timestamp","rec_timestamp","kg","rec_kg","month","rec_month","jday","rec_jday","days_at_liberty","eez",
                      "lme","ocean","rec_eez","rec_lme","rec_ocean")]

# Quality assessment

rel_rec <- tagDataValidation(rel_rec=rel_rec)


# Calculated distance between release and recovery

rel_rec$kms <- distance(rec_longitude=rel_rec$rec_longitude, rec_latitude=rel_rec$rec_latitude, rel_longitude=rel_rec$longitude, rel_latitude = rel_rec$latitude)
rel_rec$nautical_m <- rel_rec$kms * 0.5399 # nautical miles
rel_rec$month_fraction <- rel_rec$days_at_liberty/30.43 # month fraction
rel_rec$migration_per_month <- rel_rec$nautical_m/rel_rec$month_fraction #migration distance per month

x<-rel_rec[!is.na(rel_rec$kms) & rel_rec$kms > 7000,]


# Plotting 














