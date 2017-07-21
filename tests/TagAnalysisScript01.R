
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

# Quality assessment




