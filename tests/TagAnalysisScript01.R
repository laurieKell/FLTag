
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

# #read in file of Nabou's
# 
# dat <- read.table('/home/dbeare/Dropbox/AOTTP/Data/lista_of_codes.csv',sep=',',header=T)
# #recoveries[recoveries$ctcode1 %in% dat$ctcode,]
# abidjan<- read.table('/home/dbeare/Dropbox/AOTTP/Data/R03_20160819_for_send.csv',sep=',',header=T)
# names(abidjan)[1]<-'ctcode1'
# #x0 <- abidjan[abidjan$ctcode1 %in% dat$ctcode,]
# xx <- releases[releases$ctcode1 %in% dat$ctcode,]
# dat$no_match1 <- match(dat$ctcode,releases$ctcode1)
# dat$no_match2 <- match(dat$ctcode,releases$ctcode2)


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

rel_rec <- rel_rec[,c("speciescode","rcstagecode","electronictagcode1","ctcode1","ctcode2","ctcolor1","ctcolor2","date","yrmon","time","latitude","longitude","gearcode","vesselid","schooltype","depth","len","taggerid",
                      "persontypecode","chktagcanceled","specimenid","taggrpid","rcstageid","surveycode","zone","areazone","recovered","rec_longitude","rec_latitude","rec_len",
                      "rec_gearcode","rec_date","rec_time","rec_yrmon","timestamp","rec_timestamp","kg","rec_kg","month","rec_month","jday","rec_jday","days_at_liberty","eez",
                      "lme","ocean","rec_eez","rec_lme","rec_ocean")]

# Quality assessment

rel_rec <- tagDataValidation(rel_rec=rel_rec)


# Calculated distance between release and recovery

rel_rec$kms <- distance(rec_longitude=rel_rec$rec_longitude, rec_latitude=rel_rec$rec_latitude, rel_longitude=rel_rec$longitude, rel_latitude = rel_rec$latitude)
rel_rec$nautical_m <- rel_rec$kms * 0.5399 # nautical miles
rel_rec$month_fraction <- rel_rec$days_at_liberty/30.43 # month fraction
rel_rec$migration_per_month <- rel_rec$nautical_m/rel_rec$month_fraction #migration distance per month

x<-rel_rec[!is.na(rel_rec$kms) & rel_rec$kms > 7000,]
x


# Plotting 
# frequencies
fplot(input=rel_rec,what.to.plot='kms',what.species='YFT',max.obs=5000)
fplot(input=rel_rec,what.to.plot='days_at_liberty',what.species='YFT',max.obs=350)
fplot(input=rel_rec,what.to.plot='days_at_liberty',what.species=c('BET','SKJ','LTA','YFT'),max.obs=350)
fplot(input=rel_rec,what.to.plot='nautical_m',what.species=c('BET','SKJ','LTA','YFT'),max.obs=2000)


#maps
#points
mapPoints(input = rel_rec,what.longitude = "longitude",what.latitude="latitude", what.species = c("SKJ","LTA","YFT","BET"))
mapPoints(input = rel_rec,what.longitude = "rec_longitude",what.latitude="rec_latitude", what.species = c("SKJ","LTA","YFT","BET"))

#hexbins
mapHexbin(what.species='YFT',nbins=200)
mapHexbin(input = rel_rec,what.longitude = "rec_longitude",what.latitude="rec_latitude", what.species = c("SKJ","LTA","YFT","BET"),nbins=200)
mapHexbin(input = rel_rec,what.longitude = "rec_longitude",what.latitude="rec_latitude", what.species = c("BET"),nbins=300)


#tracks

mapTrack(what.species='YFT')
mapTrack(what.species='BET')
mapTrack(what.species='SKJ')
mapTrack(what.species=c('BET','LTA'))
mapTrack(what.species=c('SKJ','YFT'))


#scatterpids

mapScatterpie()
mapScatterpie(input=rel_rec,what.species=c('BET','YFT'),sf=3)
mapScatterpie(input=rel_rec,what.species=c('BET','SKJ','YFT'),sf=3)

mapScatterpie(input=rel_rec[year(rel_rec$date)==2016,],what.species=c('BET','SKJ','YFT'),sf=4)
mapScatterpie(input=rel_rec[year(rel_rec$date)==2017,],what.species=c('BET','SKJ','YFT'),sf=4)

#tables

pander(relRecSummaryTab()$Releases)
pander(relRecSummaryTab()$Recoveries)

#releases and recoveries in time

relRecTimeSeries()
relRecTimeSeries(what.species=c('BET',"YFT"))
relRecTimeSeries(what.species='SKJ')










p1 <- ggplot(data=ts1[ts1$speciescode %in% c('BET','LTA','SKJ','YFT'),], aes(x=date,y=releases))
p1 + geom_point(aes(x=date,y=releases),color='green')


+
  geom_point(aes(x=date,y=sqrt(recoveries)),color='red') +
  xlab("Date")+ylab("sqrt(Frequency)")+
  facet_wrap(~species,ncol=2)









