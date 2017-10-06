#Example script of analysis using FLTag##

library(spatial)
library(scatterpie)
library(rio)
library(data.table)
library(sp)
library(doBy)
library(rgdal)
library(RColorBrewer)
library(ggplot2)
#library(vmstools)
library(lubridate)
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

# get AOTTP data needed 
releases <- sqlQuery(aottp, "SELECT * from releases WHERE chktagcanceled ='false';");dim(releases)
recoveries <- sqlQuery(aottp, "SELECT * from recoveries WHERE rcstagecode LIKE 'RCF' AND date > '2016-07-01';");dim(recoveries)
persons <- sqlQuery(aottp, "SELECT * from persons WHERE persontagger = TRUE;")
persons$personname <- as.character(persons$personname)
persons$personcountryid[persons$personname == 'GAIZKA BIDEGAIN'] <- 21
persons$personcountryid[persons$personname == 'IÑIGO ONANDIA CALVO'] <- 21
persons$personcountryid[persons$personname == 'YAO Kouakou Appolinaire'] <- 50
persons$personcountryid[persons$personname == 'MARINA CHIFFLET'] <- 21
persons$personcountryid[persons$personid == 976] <- 50
persons$personcountryid[persons$personname == 'Edward NELSON-COFIE'] <- 50
countries <- sqlQuery(aottp, "SELECT * from countries;")
tagseries <- sqlQuery(aottp, "SELECT * from tagseries;")
electronictags <- sqlQuery(aottp,"SELECT * from electronictags;")
fadmoratorium <- readOGR("/home/dbeare/fadmoratorium",layer="fadmoratorium",verbose=FALSE)

# put on recoveries matching on specimenid using matchTagsA

rel_rec <- matchTagsA(rels=releases,recs=recoveries,mtch='specimenid');dim(rel_rec)

# harmonise character strings

rel_rec <- cleanTagData(input = rel_rec);dim(rel_rec)

# Change factors to characters and generate R-format timestamps

rel_rec <- formatTagdata(input=rel_rec);dim(rel_rec)

# Convert weights to lengths

rel_rec$kg <- NA

rel_rec$kg[rel_rec$speciescode == 'BET'] <- lenW_BET(lf=rel_rec$len[rel_rec$speciescode == 'BET'])
rel_rec$kg[rel_rec$speciescode == 'SKJ'] <- lenW_SKJ(lf=rel_rec$len[rel_rec$speciescode == 'SKJ'])
rel_rec$kg[rel_rec$speciescode == 'YFT'] <- lenW_YFT(lf=rel_rec$len[rel_rec$speciescode == 'YFT'])
rel_rec$kg[rel_rec$speciescode == 'LTA'] <- lenW_LTA(lf=rel_rec$len[rel_rec$speciescode == 'LTA'])/1000

rel_rec$rec_kg <- NA

rel_rec$rec_kg[rel_rec$speciescode == 'BET'] <- lenW_BET(lf=rel_rec$rec_len[rel_rec$speciescode == 'BET'])
rel_rec$rec_kg[rel_rec$speciescode == 'SKJ'] <- lenW_SKJ(lf=rel_rec$rec_len[rel_rec$speciescode == 'SKJ'])
rel_rec$rec_kg[rel_rec$speciescode == 'YFT'] <- lenW_YFT(lf=rel_rec$rec_len[rel_rec$speciescode == 'YFT'])
rel_rec$rec_kg[rel_rec$speciescode == 'LTA'] <- lenW_LTA(lf=rel_rec$rec_len[rel_rec$speciescode == 'LTA'])/1000
dim(rel_rec)


# Add on useful time vectors, e.g. julian day, month, year

rel_rec <- timeVectors(input=rel_rec);dim(rel_rec)

# Add on useful spatial information

rel_rec <- spatialVectors(input=rel_rec);dim(rel_rec)

# Quality assessment

rel_rec <- tagDataValidation(input=rel_rec)
dim(rel_rec);table(rel_rec$score)

# Calculated distance between release and recovery

rel_rec$kms <- distance(rec_longitude=rel_rec$rec_longitude, rec_latitude=rel_rec$rec_latitude, rel_longitude=rel_rec$longitude, rel_latitude = rel_rec$latitude)

rel_rec$nautical_m <- rel_rec$kms * 0.5399 # nautical miles
rel_rec$month_fraction <- rel_rec$days_at_liberty/30.43 # month fraction
rel_rec$migration_per_month <- rel_rec$nautical_m/rel_rec$month_fraction #migration distance per month

x<-rel_rec[!is.na(rel_rec$kms) & rel_rec$kms > 4000,]
x



# Plotting 
# Frequencies

fplot(input=rel_rec[rel_rec$score>2,],what.to.plot='kms',what.species='YFT',max.obs=5000)
fplot(input=rel_rec[rel_rec$score>2,],what.to.plot='days_at_liberty',what.species='YFT',max.obs=500)
fplot(input=rel_rec,what.to.plot='days_at_liberty',what.species=c('BET','SKJ','LTA','YFT'),max.obs=400)
fplot(input=rel_rec,what.to.plot='days_at_liberty',what.species=c('BET','YFT'),max.obs=420)



fplot(input=rel_rec,what.to.plot='nautical_m',what.species=c('BET','SKJ','LTA','YFT'),max.obs=2000)
fplot(input=rel_rec,what.to.plot='kg',what.species=c('BET','SKJ','LTA','YFT'),max.obs=15)
fplot(input=rel_rec,what.to.plot='rec_kg',what.species=c('BET','SKJ','LTA','YFT'),max.obs=15)
fplot(input=rel_rec,what.to.plot='len',what.species=c('BET','SKJ','LTA','YFT'),max.obs=150)
fplot(input=rel_rec,what.to.plot='rec_len',what.species=c('BET','SKJ','LTA','YFT'),max.obs=150)


#maps
#points
mapPoints(input = rel_rec,what.longitude = "longitude",what.latitude="latitude", what.species = c("YFT"),what.size=2)

mapPoints(input = rel_rec,what.longitude = "longitude",what.latitude="latitude", what.species = c("SKJ","LTA","YFT","BET"))
mapPoints(input = rel_rec,what.longitude = "rec_longitude",what.latitude="rec_latitude", what.species = c("SKJ","LTA","YFT","BET"))
mapPoints(input = rel_rec[!is.na(rel_rec$electronictagcode1),],what.longitude = "longitude",what.latitude="latitude", what.species = c("SKJ","YFT","BET"))
mapPoints(input = rel_rec[!is.na(rel_rec$electronictagcode1),],what.longitude = "rec_longitude",what.latitude="rec_latitude", what.species = c("SKJ","YFT","BET"))
mapPoints(input = rel_rec[rel_rec$model == 'Lotek-2810',],what.longitude = "rec_longitude",what.latitude="rec_latitude", what.species = c("YFT","BET"))

mapPoints(input = rel_rec[rel_rec$model == 'Lotek-2810',],what.longitude = "longitude",what.latitude="latitude", what.species = c("BET","YFT"))

mapPoints(input = rel_rec[rel_rec$model == 'MiniPAT-348C',],what.longitude = "longitude",what.latitude="latitude", what.species = c("YFT","BET"))
mapPoints(input = rel_rec[rel_rec$model == 'MiniPAT-348C',],what.longitude = "longitude",what.latitude="latitude", what.species = c("YFT","BET"))

data(iotc)
dimnames(iotc)[[2]][1]<-'speciescode'
mapPoints(input = iotc,what.longitude = "rel_lon",what.latitude="rel_lat", what.species = c("YFT","BET"),location=c(60,0))


mapPoints(input = rel_rec[rel_rec$eez == 'Spanish EEZ (Canary Islands)',],what.longitude = "longitude",what.latitude="latitude", what.species = c("YFT","BET","SKJ"))

cislas <- rel_rec[rel_rec$eez == 'Spanish EEZ (Canary Islands)',]
map('world',xlim=c(-30,30),ylim=c(-10,45))
points(cislas$longitude,cislas$latitude,col=2)


mapPointsSpeciesByMonth(input = rel_rec, what.longitude='longitude',what.latitude='latitude',what.species = c("SKJ","LTA","YFT","BET"))
mapPointsSpeciesByMonth(input = rel_rec, what.longitude='rec_longitude',what.latitude='rec_latitude',what.species = c("SKJ","LTA","YFT","BET"), what.facet='rec_yrmon')

#hexbins
mapHexbin(input = rel_rec, what.longitude='longitude',what.latitude='latitude',what.species = c("SKJ","LTA","YFT","BET") ,nbins=120)
mapHexbin(input = rel_rec,what.longitude = "rec_longitude",what.latitude="rec_latitude", what.species = c("SKJ","LTA","YFT","BET"),nbins=200)
mapHexbin(input = rel_rec,what.longitude = "rec_longitude",what.latitude="rec_latitude", what.species = c("BET"),nbins=300)

# Nautical miles
tapply(rel_rec$nautical_m,rel_rec$speciescode,summary,na.rm=T)
#TaL
tapply(rel_rec$days_at_liberty,rel_rec$speciescode,summary,na.rm=T)


#tracks
mapTrack(input = rel_rec[rel_rec$score==6 & rel_rec$nautical_m>1000,], what.species='BET',what.size=2)
mapTrack(input = rel_rec[rel_rec$score==6,],what.species='SKJ',what.gear='BB')
mapTrack(input = rel_rec[rel_rec$score==6,],what.species='SKJ',what.gear='PS')
mapTrack(input = rel_rec[rel_rec$score ==6,],what.species=c('BET','LTA'))
mapTrack(input = rel_rec[rel_rec$score ==6,],what.species=c('BET','LTA','SKJ','YFT'),what.gear='BB')
mapTrack(input = rel_rec[rel_rec$score ==6,],what.species=c('BET','LTA','SKJ','YFT'),what.gear='PS')
mapTrack(input = rel_rec[rel_rec$score ==6,],what.species=c('BET','LTA','SKJ','YFT'),what.gear='LL')


#scatterpids
mapScatterpie()
mapScatterpie(input=rel_rec,what.species=c('BET','YFT'),sf=3)
mapScatterpie(input=rel_rec,what.species=c('BET','SKJ','YFT'),sf=5)
mapScatterpie(input=rel_rec,what.longitude='rec_longitude',
              what.latitude='rec_latitude',what.yrmon='rec_yrmon',what.species=c('BET','SKJ','YFT'),sf=3)


mapScatterpie(input=rel_rec[year(rel_rec$date)==2016,],what.species=c('BET','SKJ','YFT'),sf=4)
mapScatterpie(input=rel_rec[year(rel_rec$date)==2017,],what.species=c('BET','SKJ','YFT'),sf=4)

#tables

pander(relRecSummaryTab()$Releases)
pander(relRecSummaryTab()$Recoveries)

#double-tagging, tag-shedding

TagSheddingTab()$Double_Tag_Nos
TagSheddingTab()$Tag_Shed_Nos
TagSheddingTab()$Tag_Shed_Perc
TagSheddingTab(input=rel_rec[rel_rec$quad == 'NE',])

#chemically-tagged totals

ChemTaggingTab()

table(releases$ctcolor1,releases$speciescode)

#releases and recoveries in time

relRecTimeSeries()
relRecTimeSeries(what.species=c('BET',"YFT"))
relRecTimeSeries(what.species='SKJ')

#tag-seeding

pander(TagSeedingTab(input=rel_rec)$tagSeedRel)
pander(TagSeedingTab(input=rel_rec)$tagSeedRec)
pander(TagSeedingTab(input=rel_rec)$tagSeedPerc)

#n tags by country

pander(nTagsRelByCountry())

#e tags

nElectronicTagsTab()

x<- rel_rec[rel_rec$model=='Lotek-2810' & rel_rec$recovered ==T,]

# FAD moratorium
jf2017 <- rel_rec[rel_rec$year ==2017 & rel_rec$month %in% c('enero','febrero'),]
table(jf2017$fmor17)
table(jf2017$fmor17,jf2017$speciescode)
table(jf2017$rec_fmor17,jf2017$speciescode)


#growth tracks

growthTrack(input=rel_rec[rel_rec$score > 3 & rel_rec$rec_gearcode == 'PS' & rel_rec$days_at_liberty > 89,],what.species ='YFT')
growthTrack(input=rel_rec[rel_rec$score > 3 & rel_rec$rec_gearcode == 'PS',],what.species =c('BET','LTA','SKJ','YFT'))
growthTrack(input=rel_rec[rel_rec$score > 3 & rel_rec$rec_gearcode == 'PS'& rel_rec$days_at_liberty > 29,],what.species =c('BET','LTA','SKJ','YFT'))

growthTrack(input=rel_rec[rel_rec$score > 3 & rel_rec$rec_gearcode == 'BB' & rel_rec$days_at_liberty > 270,],what.species =c('BET','YFT'),what.size=1)

By quadrant

q1 <- rel_rec[rel_rec$tagseeding == 0,]
q1 <- table(q1$speciescode,q1$quad)
Total <- apply(q1,2,sum,na.rm=T)
q1 <- rbind(q1,Total)
pander(q1)


#Recoveries by EEZ

table(rel_rec$rec_eez,rel_rec$speciescode)
plot(rel_rec$rec_longitude,rel_rec$rec_latitude,pch='.')
plot(eez,add=TRUE)
points(recoveries$longitude,recoveries$latitude,col='red',pch='.')

#Probability of being re-caught

rel_rec$bin <- ifelse(rel_rec$recovered == TRUE,1,0)
data1 <- rel_rec[!is.na(rel_rec$len),]
data1 <- data1[data1$speciescode %in% c('BET','LTA','SKJ','YFT'),]
data1$speciescode <- as.factor(data1$speciescode)
data1$rec_gearcode <- as.factor(data1$rec_gearcode)
 z1 <- gam(bin ~ s(len,by=speciescode)+s(longitude,latitude,days_at_liberty,by=speciescode), data=data1,family='quasibinomial' ) # P(Recapture) depends strongly on release location.
 #bet.z1 <- mgcv(bin ~ s(longitude,latitude)+s(len,by=speciescode), data=rel_rec[!is.na(rel_rec$len) & rel_rec$speciescode == 'BET',],family='quasibinomial' )
# 
 #bet <- rel_rec[!is.na(rel_rec$len) & rel_rec$speciescode == 'BET',]
 #bet$P_capture <- round(predict(bet.z1,bet,type='response'),2)

gd <- expand.grid(speciescode=c('BET','LTA','SKJ','YFT'),len=52,latitude=15,longitude=-18)

data1$P_capture <- round(predict(z1,data1,type='response'),2)

Atl <- c(-30,0)
wAfMap <- get_map(location=Atl,source='google',maptype='satellite',crop=TRUE,zoom=3)

ggmap(wAfmap)


Atl <- c(-30,0)
wAfMap <- get_map(location=Atl,source='google',maptype='satellite',crop=TRUE,zoom=3)
ggmap(wAfMap) +
geom_point(aes(x=jitter(longitude),y=jitter(latitude),color=P_capture,size=P_capture),data=data1[data1$speciescode =='YFT',]) +
  scale_color_gradientn(colours=heat.colors(100)) +
  facet_wrap(~speciescode,ncol=2)



















