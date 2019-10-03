# #Example script of analysis using FLTag##

# options(OutDec= ".")
Sys.setlocale("LC_MESSAGES", 'en_GB.UTF-8')
Sys.setenv(LANG = "en_US.UTF-8")

# Google API Key: AIzaSyARODN7WldCnR2Uuiq9FZ39fCfPLxQKp7M


library(spatial)
library(sp)
library(glue)
library(scatterpie)
library(rio)
library(data.table)
library(sp)
library(doBy)
library(rgdal)
library(RColorBrewer)
library(ggplot2)
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
#library(effdisR)
library(latticeExtra)
#library(vmstools)
library(scales)
library(RODBC)
library(leaflet)
library(zoo)
library(FLTag)
#library(growthmodels)

# require(RPostgreSQL)
# con <- dbConnect(RPostgreSQL::PostgreSQL(),
#                  dbname="aottp",
#                  host= "172.16.1.48",
#                  port= 5432,
#                  user= "aottpr",
#                  password="tunasr")
# 
# strSQL <- "SELECT * FROM releases;"
# rs <- dbSendQuery(con, strSQL)
# df <- dbFetch(rs)
# dbClearResult(rs)
# 
# unique(df$sexcode)

# # attach to database

odbcCloseAll()
#
aottp <-  odbcConnect("aottp-local", case ="tolower",DBMSencoding='utf8')
#aottp <-  odbcConnect("bigeye-aottp", case ="postgresql", believeNRows=FALSE)

# # get AOTTP data 

releases <- sqlQuery(aottp, "SELECT * from releases WHERE chktagcanceled = '0';",as.is=T);
# releases1 <- sqlQuery(aottp, "SELECT * from releases;",as.is=T);
sqlQuery(aottp,"SELECT min(longitude) from releases;")

xx <- sqlQuery(aottp,'SELECT MAX(date) from releases;')
xx
dim(releases)
releases <- subset(releases, select = -notes )
releases$len <- as.numeric(releases$len)

###  Tag series etc ###

tagseries <- sqlQuery(aottp, 'SELECT * from tagseries;')
red<- tagseries[tagseries$tagcolorid == 2,]
sum(red$tagnums)
yellow <- tagseries[tagseries$tagcolorid == 3,]

### Remove falsified tags

'%notin%' <- Negate(`%in%`)
tgs_canceled <- sqlQuery(aottp,"SELECT * FROM releases_canceled;")
releases <- releases[releases$ctcode1 %notin% tgs_canceled$ctcode1,]
dim(releases)

### Set Helena releases ###

elena <- releases[releases$team %in% c('E30','E31','E32') & releases$rcstagecode == 'R-1',]
table(elena$rcstagecode,elena$speciescode)
str(elena)
elena$redate <- as.POSIXct(strptime(elena$date, format = "%Y-%m-%d"))
elena$reyrmon <- as.yearmon(elena$redate)


elena <- elena[elena$rcstagecode == 'R-1',]
sth.tots <- table(elena$speciescode,elena$reyrmon)
sth.tots <-rbind(sth.tots,apply(sth.tots,2,sum))
sth.tots <-cbind(sth.tots,apply(sth.tots,1,sum))
write.table(sth.tots,"T:\\AOTTP/Tenders/Phase2-Tagging/SE-Atlantic/Evaluation/Contract/St.Helena.Totals.April-30-2019.csv",sep=',')


#releases$longitude <- as.numeric(gsub(',','.',releases$longitude))
#releases$latitude  <- as.numeric(gsub(',','.',releases$latitude))


#releases$project <-'aottp'
#rels_recs_all <- sqlQuery(aottp,"SELECT * from releases_recoveries_all");dim(rels_recs_all)
recoveries <- sqlQuery(aottp, "SELECT * from recoveries WHERE rcstagecode LIKE 'RCF' AND date > '2016-07-01';",as.is=T); dim(recoveries);
recoveries <- subset(recoveries,select = -notes)
#throw out fake tags
recoveries <- recoveries[recoveries$ctcode1 %notin% tgs_canceled$ctcode1,]
dim(recoveries)


#recoveries$project <-'aottp'
#recoveries$longitude <- as.numeric(gsub(',','.',recoveries$longitude))
#

# ## get the historical ICCAT data
#  hreleases <- sqlQuery(aottp, "SELECT * from public.re_iccat_all;");dim(hreleases)
#  
#  colnames(hreleases)[colnames(hreleases)=='ctcode1'] <- 'numtag1'
#  colnames(hreleases)[colnames(hreleases)=='ctcode2'] <- 'numtag2'
#  colnames(hreleases)[colnames(hreleases)=='date_'] <- 'redate'
#  colnames(hreleases)[colnames(hreleases)=='time_'] <- 'retime'
#  colnames(hreleases)[colnames(hreleases)=='len'] <- 'relen'
#  colnames(hreleases)[colnames(hreleases)=='ctcolor1'] <- 'tagcolor1'
#  colnames(hreleases)[colnames(hreleases)=='ctcolor2'] <- 'tagcolor2'
#  
#   hreleases$project <- 'iccat'
# # 
#  hrecoveries <- sqlQuery(aottp, "SELECT * from public.rc_iccat_all;"); dim(hrecoveries);
#  colnames(hrecoveries)[colnames(hrecoveries)=='ctcode1'] <- 'numtag1'
#  colnames(hrecoveries)[colnames(hrecoveries)=='ctcode2'] <- 'numtag2'
#  colnames(hrecoveries)[colnames(hrecoveries)=='date_'] <- 'rcdate'
#  colnames(hrecoveries)[colnames(hrecoveries)=='time_'] <- 'rctime'
# colnames(hrecoveries)[colnames(hrecoveries)=='len'] <- 'rclen'
# colnames(hrecoveries)[colnames(hrecoveries)=='ctcolor1'] <- 'tagcolor1'
# colnames(hrecoveries)[colnames(hrecoveries)=='ctcolor2'] <- 'tagcolor2'
# 
# hrecoveries$project <- 'iccat'
# 
# releases <- merge(releases,hreleases,all=T);dim(releases)
# recoveries <- merge(recoveries,hrecoveries,all=T);dim(recoveries)

# hrel_rec <- matchTagsA(rels=hreleases,recs=hrecoveries,mtch='ctcode1');dim(hrel_rec)
#str(rel_rec)



# Electronic tags

elec <- sqlQuery(aottp, "SELECT * from releases_recoveries_electronic;"); dim(etags);
nams <- dimnames(elec)[[2]]

i <- sapply(elec, is.factor)
elec[i] <- lapply(elec[i], as.character)
elec <-timeVectors(input=elec,orig.date="2016-01-01");

elec <- spatialVectors(input=elec);
elec$reeez[is.na(elec$reeez)] <- 'High Seas'

elec[!is.na(elec$ndays) & elec$ndays > 399 & elec$supplier == 'LOTEK ARCGEO9',]


str(recoveries$latitude)
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
fadmoratorium <- readOGR("d://dbeare/dbeare/fadmoratorium",layer="fadmoratorium",verbose=FALSE)

'%notin%' <- Negate(`%in%`)

vessels <- sqlQuery(aottp,"SELECT * from vessels;",as.is=T)
vessels$vesselname[vessels$vesselid == 862] <- 'ACORIANA'
vessels$vesselname[vessels$vesselid == 1019] <- 'ALDEBARAN_1'
vessels$vesselname[vessels$vesselid == 1047] <- 'TUBURAO_TIGRE'


vessels <- vessels[vessels$vesselname %notin% c("AT000USA00050", 'XIXILI'),]
vessels$vesselname <- gsub("\r\n\t\r\n","",vessels$vesselname)

#vessels <- vessels[!is.na(vessels$vesselrelease) & vessels$vesselrelease ==1,]

#znb2 <- readOGR("d://Dropbox/AOTTP/DataExploration/GISData/ZoneBV2.kml",verbose=T)
#zna2 <- readOGR("d:/Dropbox/AOTTP/DataExploration/GISData/ZoneAV2.kml",verbose=T)
#znc2 <- readOGR("d://Dropbox/AOTTP/DataExploration/GISData/ZoneCV2.kml",verbose=T)

# # IOTC data #
# data(iotc)
# dimnames(iotc)[[2]][1]<-'speciescode'
# dimnames(iotc)[[2]][4] <- 'date'
# iotc$date <-     as.POSIXct(as.character(iotc$date))
# iotc$rec_date <- as.character(iotc$rec_date)
# iotc$rec_date <- ifelse(iotc$rec_date=="",NA,iotc$rec_date)
# iotc$rec_date <- as.POSIXct(iotc$rec_date)
# iotc$len     <- iotc$rel_length_fl
# iotc$rec_len <- iotc$rec_length_fl
# dimnames(iotc)[[2]][5] <- 'latitude'
# dimnames(iotc)[[2]][6] <- 'longitude'
# dimnames(iotc)[[2]][15] <- 'rec_latitude'
# dimnames(iotc)[[2]][16] <- 'rec_longitude'

# put on recoveries matching on specimenid using matchTagsA

#rel_rec <- matchTagsA(rels=releases,recs=recoveries,mtch='ctcode1');dim(rel_rec)
#str(rel_rec)


##################################################################################################
##################################################################################################
### Use Jesus' View ###
##############################################################

odbcCloseAll()
#
aottp <-  odbcConnect("aottp-local", case ="tolower",DBMSencoding='utf8')
#aottp <-  odbcConnect("bigeye-aottp", case ="postgresql", believeNRows=FALSE)

rel_rec <- sqlQuery(aottp,'SELECT * from releases_recoveries_withhistoricaldataiccat')

rel_rec <- subset(rel_rec, select = -rcnotes )
i <- sapply(rel_rec, is.factor)
rel_rec[i] <- lapply(rel_rec[i], as.character)

summary(rel_rec$rcdate,na.rm=T)

table(rel_rec$tagseries)
#ATP = 114834

colnames(rel_rec)[colnames(rel_rec) == 'strtags1'] <- 'numtag1'
colnames(rel_rec)[colnames(rel_rec) == 'strtags2'] <- 'numtag2'
dim(rel_rec)  # 681939



# Check the lats and longs

hist(rel_rec$relonx)
hist(rel_rec$rclonx);summary(rel_rec$rclonx)
hist(rel_rec$relaty)
hist(rel_rec$rclaty);summary(rel_rec$rclaty)

ko2<- which(rel_rec$rclonx >= 180)
ko3<- which(rel_rec$rclaty >= 180)
rel_rec<-rel_rec[-c(ko2,ko3),]

plot(rel_rec$relonx,rel_rec$relaty,pch=".")
points(rel_rec$rclonx,rel_rec$rclaty,pch='.',col='red')
map('world',add=T,col='green',fill=T)
#rel_rec <- cleanTagData(input = nrel_rec);dim(rel_rec) # Don't need this here as it is for tag seeding.

 # Change factors to characters and generate R-format timestamps

 rel_rec_all <- formatTagdata(input=rel_rec);dim(rel_rec_all)
 
 # Make sure there is no Brazilian faked tags in the data
 
 # ftags <- read.csv("\\\\tunadata/AOTTP/AOTTP/DataExploration/Brazil report/False tags check/thav3fakes.csv",sep=',',header=T)
 # ftsags <-  read.csv("\\\\tunadata/AOTTP/AOTTP/DataExploration/Brazil report/False tags check/thav3Seeding.csv",sep=',',header=T)
 # ftags <- rbind(ftags,ftsags)
 
 # ctcode1 <- c(42073,84113,84115,84118,84121,84128,84142,84143,84162,84163,84201,84210,84212,84231,84240,84253,84260,84263,84380,84420,84613,84631,84656,85147)
 # 
 # 
 # ctcode1 %in% ftags
 # 
 
# Convert weights to lengths
#
 rel_rec_all$rekg <- NA
#
 rel_rec_all$rekg[!is.na(rel_rec_all$speciescode) & rel_rec_all$speciescode == 'BET'] <- lenW_BET(lf=rel_rec_all$relen[!is.na(rel_rec_all$speciescode) & rel_rec_all$speciescode == 'BET'])
 rel_rec_all$rekg[!is.na(rel_rec_all$speciescode) & rel_rec_all$speciescode == 'SKJ'] <- lenW_SKJ(lf=rel_rec_all$relen[!is.na(rel_rec_all$speciescode) & rel_rec_all$speciescode == 'SKJ'])
 rel_rec_all$rekg[!is.na(rel_rec_all$speciescode) & rel_rec_all$speciescode == 'YFT'] <- lenW_YFT(lf=rel_rec_all$relen[!is.na(rel_rec_all$speciescode) & rel_rec_all$speciescode == 'YFT'])
 rel_rec_all$rekg[!is.na(rel_rec_all$speciescode) & rel_rec_all$speciescode == 'LTA'] <- lenW_LTA(lf=rel_rec_all$relen[!is.na(rel_rec_all$speciescode) & rel_rec_all$speciescode == 'LTA'])/1000

 rel_rec_all$rckg <- NA
#
 rel_rec_all$rckg[!is.na(rel_rec_all$speciescode) & rel_rec_all$speciescode == 'BET'] <- lenW_BET(lf=rel_rec_all$rclen[!is.na(rel_rec_all$speciescode) & rel_rec_all$speciescode == 'BET'])
rel_rec_all$rckg[!is.na(rel_rec_all$speciescode) & rel_rec_all$speciescode == 'SKJ'] <- lenW_SKJ(lf=rel_rec_all$rclen[!is.na(rel_rec_all$speciescode) & rel_rec_all$speciescode == 'SKJ'])
 rel_rec_all$rckg[!is.na(rel_rec_all$speciescode) & rel_rec_all$speciescode == 'YFT'] <- lenW_YFT(lf=rel_rec_all$rclen[!is.na(rel_rec_all$speciescode) & rel_rec_all$speciescode == 'YFT'])
 rel_rec_all$rckg[!is.na(rel_rec_all$speciescode) & rel_rec_all$speciescode == 'LTA'] <- lenW_LTA(lf=rel_rec_all$rclen[!is.na(rel_rec_all$speciescode) & rel_rec_all$speciescode == 'LTA'])/1000
 dim(rel_rec_all)
 
#
# # Add on useful time vectors, e.g. julian day, month, year
#
 
 rel_rec_all <- timeVectors(input=rel_rec_all,orig.date="2016-01-01");dim(rel_rec_all)
 
 #iotc  <- timeVectors(input = iotc)
#
# # Add on useful spatial information
#
 rel_rec_all <- spatialVectors(input=rel_rec_all);dim(rel_rec_all)

 #table(rel_rec$refmor)
 #table(rel_rec$rcfmor)
 
 table(rel_rec_all$reyrmon,rel_rec_all$requadrant)
 tt <- table(rel_rec_all$rcyrmon,rel_rec_all$rcquadrant)
 
 #table(rel_rec$tagseeding)
 
 #iotc    <- spatialVectors(input=iotc);dim(iotc)
 
# # Quality assessment
#
 rel_rec_all <- tagDataValidation(input=rel_rec_all)
 dim(rel_rec_all);table(rel_rec_all$score)
#
# # Calculated distance between release and recovery

 rel_rec_all$kms <- distance(rclonx=rel_rec_all$rclonx, rclaty=rel_rec_all$rclaty, relonx=rel_rec_all$relonx, relaty = rel_rec_all$relaty)
 #
 rel_rec_all$nautical_m <- rel_rec_all$kms * 0.5399 # nautical miles
 rel_rec_all$month_fraction <- rel_rec_all$days_at_liberty/30.43 # month fraction
 rel_rec_all$migration_per_month <- rel_rec_all$nautical_m/rel_rec_all$month_fraction #migration distance per month
 rel_rec_all$tagseeding <- 0
 rel_rec_all$electronictagcode1 <- rep(NA,length(rel_rec_all[,1]))

write.table(rel_rec_all,file="C:\\FLTag/AllICCAT-Tagging-Data.csv",sep=",",row.names = FALSE)
 
rci <- (1:dim(aottp)[[1]])[rel_rec_all$rcstagecode_rc %in% c("R-2","R-3","R-4","R-5","RCF")]       # Where are the recoveries
aottp$recovered <- FALSE
aottp$recovered[rci] <- TRUE
table(aottp$recovered)

rci2 <- which(!is.na(iccat$rcdate))       # Where are the recoveries

iccat$recovered <- FALSE
iccat$recovered[rci2] <- TRUE
table(iccat$recovered)
 
table(rel_rec_all$recovered,rel_rec_all$proyect)

dimx <- rel_rec$rcdate[rel_rec$recovered==T]

### Add in vesselname ### 

rel_rec$revesselname <- vessels$vesselname[match(rel_rec$revesselid,vessels$vesselid)]




### Bermuda releases ###

bermuda <- rel_rec_all[!is.na(rel_rec$reeez) & rel_rec$reeez == 'Bermudian EEZ',]
table(bermuda$revesselname)

 
### St Helena releases ###
 
 elena <- rel_rec[rel_rec$reteam %in% c('E30','E31','E32'),]
 table(elena$rcstagecode_re,elena$speciescode)
 str(elena)
 #elena$redate <- as.POSIXct(strptime(elena$rdate, format = "%Y-%m-%d"))
 #elena$reyrmon <- as.yearmon(elena$redate)
 
 
 elena <- elena[elena$rcstagecode_re == 'R-1',]
 sth.tots <- table(elena$speciescode,elena$reyrmon)
 sth.tots <-rbind(sth.tots,apply(sth.tots,2,sum))
 sth.tots <-cbind(sth.tots,apply(sth.tots,1,sum))
 write.table(sth.tots,"T:\\AOTTP/Tenders/Phase2-Tagging/SE-Atlantic/Evaluation/Contract/St.Helena.Totals.July-30-2019.csv",sep=',')

 
 # Ivorian releases
 
 civ <- rel_rec[rel_rec$reteam %in% c('E29'),]
 
 # Have a look at Jesus' gis schema
 
 relrecline <- sqlQuery(aottp, "SELECT * from gis.relrecline;"); 
 
 # Etag coding.
 
 #Write out joined data as a csv file.
 
 write.table(rel_rec,file='c:/Users/dbeare/Documents/new.aottp.tagging.csv',sep='|')
 
 #tag-seeding
 
 write.table(rel_rec[rel_rec$tagseeding==1,],file='c:/Users/dbeare/Documents/aottp.tagseeding.csv',sep='|')

 tseed <- read.table(file='c:/Users/dbeare/Documents/aottp.tagseeding.csv',sep='|')
 
 dim(tseed)
 
 ###################################################
 ### Relationship between Tagging and Recovery #####
 ###################################################
 
 ## Just the East side ##
 
 rel_rec_e <- rel_rec[rel_rec$relonx >= -30,]
 
 # Yellow tags from Jesus' 'view'
 
 yel <- rel_rec[rel_rec$tagcolor1 == 3,]
 
 # By month
 
 xxx <- table(yel$reyrmon,yel$recovered)
 
 dat <- data.frame(abt = 1:dim(xxx)[[1]]+5,yrmon=dimnames(xxx)[[1]],
   rc = as.vector(xxx[,"TRUE"]),re = as.vector(xxx[,"FALSE"]+xxx[,"TRUE"]))
 
 #dat<-dat[1:36,]
 
 
 # Regress recoveries on the number of releases the same and previous month
 dd <- dim(dat)[1]
 dat$re1 <- c(NA,dat$re[-dd])
 dat$re2 <- c(NA,NA,dat$re[1:(dd-2)])
 dat$re3 <- c(NA,NA,NA,dat$re[1:(dd-3)])
 
 dat <- dat[3:35,]
 
 tm1 <- glm(rc~abt,data=dat,family=quasipoisson)
 tm2 <- glm(rc~abt+re,data=dat,family=quasipoisson)
 tm3 <- glm(rc~abt+re+re1,data=dat,family=quasipoisson)
 tm4 <- glm(rc~abt+re+re1+re2,data=dat,family=quasipoisson)
 
 anova(tm1,tm2,tm3,tm4,test="Chi")
 
 ndata <-data.frame(abt=1:120,re=0)
 ndata$pred.recoveries <- round(predict(tm2,ndata,type='response'),1)
 
 ndata$yr <- rep(2015:2024,rep(12,10))
 ndata$mon <- rep(1:12,10)
 
 
 
par(mfrow=c(1,1))
  plot(ndata$abt,ndata$pred.recoveries,xlab='year',ylab='no recoveries',
      xlim=c(8,120),ylim=c(0,3000),xaxt='n')
 #points(ndata$abt,ndata$pred.recoveries.lag1,col='red')
 abline(v=seq(1,120,by=12),lty=2,col='green')
 mtext(as.character(2015:2024),side=1,at=seq(7,125,by=12),cex=.6)
 points(dat$abt,dat$rc)
 
 write.table(ndata[,c(1,2,3,5,6)][73:120,],file="T:/AOTTP/Planning/Recovery_predictions.csv",sep=",",row.names=F)
 
 ##########################
 ## Just the NW Quadrant ##
 
 rel_rec_nw <- rel_rec[rel_rec$relonx <= -30 & rel_rec$relaty >=10,]
 
 # Yellow tags from Jesus' 'view'
 
 yel <- rel_rec[rel_rec$tagcolor1 == 3,]
 
 # By month
 
 xxx <- table(yel$reyrmon,yel$recovered)
 xxx<-xxx[1:36,]
 
 dat <- data.frame(abt = 1:dim(xxx)[[1]]+5,yrmon=dimnames(xxx)[[1]],re = as.vector(xxx[,"FALSE"]+xxx[,"TRUE"]), rc = as.vector(xxx[,"TRUE"]))
 
 # Regress recoveries on the number of releases the same and previous month
 
 dat$rc1 <- c(dat$rc[-1],NA)
 dat$rc2 <- c(dat$rc[-c(1:2)],NA,NA)
 
 tm1 <- glm(rc~re+abt,data=dat,family=quasipoisson)
 tm2 <- glm(rc1~re+abt,data=dat,family=quasipoisson)
 anova(tm1,tm2)
 
 ndata <-data.frame(abt=1:120,re=0)
 ndata$pred.recoveries <- round(predict(tm1,ndata,type='response'),1)
 ndata$pred.recoveries.lag1 <- round(predict(tm2,ndata,type='response'),1)
 ndata$yr <- rep(2015:2024,rep(12,10))
 ndata$mon <- rep(1:12,10)
 
 plot(ndata$abt,ndata$pred.recoveries,xlab='year',ylab='no recoveries',
   xlim=c(48,120),ylim=c(0,25),xaxt='n')
 #points(ndata$abt,ndata$pred.recoveries.lag1,col='red')
 abline(v=seq(1,120,by=12),lty=2,col='green')
 mtext(as.character(2015:2024),side=1,at=seq(6,125,by=12),)
 
 
 
 

 ####################################################
 ### Create file for charter type for each survey ###
 ####################################################
 
 rel_rec <- orderBy(~retimestamp+surveycode,data=rel_rec) # order data from first released fish
 #rel_rec <- rel_rec[rel_rec$tagseeding ==0,]
 rel_rec[rel_rec$regearcode == 'PS',]
 rel_rec$regearcode[rel_rec$revesselid == 863] <- 'BB'
 
 start.time<- aggregate(list(start.time=rel_rec$retimestamp),
                        by=list(vesselid=rel_rec$revesselid,
                                gearcode=rel_rec$regearcode,surveycode=rel_rec$surveycode),min,na.rm=T)
 end.time  <- aggregate(list(end.time=rel_rec$retimestamp),
                        by=list(vesselid=rel_rec$revesselid,
                                gearcode=rel_rec$regearcode,surveycode=rel_rec$surveycode),max,na.rm=T)
 
 t_a <- start.time
 t_a$end.time <- end.time$end.time
 t_a <- orderBy(~start.time,data=t_a)
 
 vessels$vesselname <- as.character(vessels$vesselname)
 t_a$revesselname <- vessels$vesselname[match(t_a$vesselid,vessels$vesselid)]
 t_a$vesselnotes <- vessels$vesselnotes[match(t_a$vesselid,vessels$vesselid)]
 
 t_a$gearcode <- ifelse(t_a$gearcode == 'PS','BB',t_a$gearcode) 
 
 t_a[is.na(t_a$start.time),] # check for NAs
 t_a[is.na(t_a$end.time),]
 

 t_a$ndays <- difftime(t_a$end.time,t_a$start.time,unit='days')
 t_a$ndays <- ifelse(t_a$ndays=='-Inf',1,t_a$ndays)
 t_a$ndays <- ifelse(t_a$ndays <= 1,1,t_a$ndays)
 t_a$zone <- substr(t_a$surveycode,5,5)
 sum(t_a$ndays)
 
 fo <- (1:length(t_a[,1]))[!is.na(t_a$revesselname) & t_a$revesselname =='ARGOS']
 #t_a <- t_a[-fo,]
 
 t_a[is.na(t_a$vesselname),]
 
 t_a<- orderBy(~vesselid+start.time+end.time,data=t_a)
 
 t_a$agreement <- NA
 

 uv <- sort(unique(t_a$revesselname))
 
 t_a$agreement[t_a$revesselname == "ACORIANA"]  <- 'Charter' # Acoriana
 
 t_a$agreement[t_a$revesselname == "AITA FRAXKU" & t_a$end.time < as.POSIXct('2017-03-18')] <- 'Charter'
 t_a$agreement[t_a$revesselname == "AITA FRAXKU" & t_a$end.time > as.POSIXct('2017-03-16')] <- 'Buy_fish'
 t_a$agreement[t_a$revesselname == "ALBACORE"] <- 'Charter'
 t_a$agreement[t_a$revesselname == "ALDEBARAN_1"] <- 'Charter'
 t_a$agreement[t_a$revesselname == "ARAGAO"] <- 'Buy_fish(trap)' # 
 t_a$agreement[t_a$revesselname == "BOY"] <- 'Charter' #
 t_a$agreement[t_a$revesselname == "CANYON RUNNER"] <- 'Charter' #
 t_a$agreement[t_a$revesselname == "EL CLASSICO"] <- 'Charter' #
t_a$agreement[t_a$revesselname == "EL GRANDE PRIMERO" & t_a$end.time < as.POSIXct('2016-11-07')] <- 'Charter' # Primero
t_a$agreement[t_a$revesselname == "EL GRANDE PRIMERO" & t_a$end.time >= as.POSIXct('2016-11-06')] <- 'Buy_fish' # Primero              
               
 t_a$agreement[t_a$revesselname == "EL MACIZO"] <- 'Charter' #
 t_a$agreement[t_a$revesselname == "ESTRELLA DALVA"] <- 'Charter and Buy_fish' #
 
 
 t_a$agreement[t_a$revesselname == "EXILE"] <- 'Charter' #
 t_a$agreement[t_a$revesselname == "FV AMALIA"] <- 'Charter' #
 t_a$agreement[t_a$revesselname == "FV CATFISH"] <- 'Charter' # 
 t_a$agreement[t_a$revesselname == "FV EXTRACTOR"] <- 'Charter' #
 t_a$agreement[t_a$revesselname == "FV HELENA DOROTHY"] <- 'Charter' #
 t_a$agreement[t_a$revesselname == "FV JOHN MELLIS"] <- 'Charter' #
 t_a$agreement[t_a$revesselname == "FV Ocean Wave"] <- 'Charter' #
 t_a$agreement[t_a$revesselname == "FV SEAHORSE"] <- 'Charter' #
 t_a$agreement[t_a$revesselname == "KATSUSHIO MARU 8"] <- 'Buy_fish' #
 t_a$agreement[t_a$revesselname == "KERRY-D"] <- 'Charter' # 
 t_a$agreement[t_a$revesselname == "LEVANA"] <- 'Charter' # 
 t_a$agreement[t_a$revesselname == "N.D.N.S."] <- 'Buy_fish' # 
 t_a$agreement[t_a$revesselname == "NUEVO BATABANO I"] <- 'Buy_fish' # 
 t_a$agreement[t_a$revesselname == "OULED SI MOHAND\r\n\t\r\n"] <- 'Charter and Buy_fish' # 
 t_a$agreement[t_a$revesselname == "PONTA CALHAU"] <- 'Charter' # 
 t_a$agreement[t_a$revesselname == "SINUELO"] <- 'Charter and Buy_fish' #
 t_a$agreement[t_a$revesselname == "SLACK'D UP"] <- 'Charter' # Transmar I
 t_a$agreement[t_a$revesselname == "TARRYNAMY"] <- 'Charter' #
 t_a$agreement[t_a$revesselname == "THAVISSON III"] <- 'Charter and Buy_fish' # T
 t_a$agreement[t_a$revesselname == "TRAMSMAR I"] <- 'Charter and Buy_fish' # T
 t_a$agreement[t_a$revesselname == "TUBURAO_TIGRE"] <- 'Charter and Buy_fish' # T
 t_a$agreement[t_a$revesselname == "TXILAMON NI SON"] <- 'Charter' # T
 
 sum(t_a$ndays)
 
 write.table(t_a,file='c:/Users/dbeare/tagging.agreements.17_May_2019.csv',sep=",",row.names=F)
 write.table(t_a,file='T:/AOTTP/Database/data/tagging.agreements.17_May_2019.csv',sep=",",row.names=F)
 
 
 ###################################################################################
 ###################################################################################
 ###################################################################################

 
 rel_rec_1 <- read.table(file='c:/Users/dbeare/Documents/aottp.tagging.csv',sep="|",header=T)
 
  
 # harmonise character strings
 
 rel_rec_1 <- cleanTagData(input = rel_rec_1);dim(rel_rec_1)
 
 # Change factors to characters and generate R-format timestamps
 
 input <- rel_rec_1
 input$longitude <- as.numeric(as.character(input$longitude))
 input$rec_longitude <- as.numeric(as.character(input$rec_longitude))
 input$latitude <- as.numeric(as.character(input$latitude))
 input$rec_latitude <- as.numeric(as.character(input$rec_latitude))
 
 
 #Change zeros to NAs
 input$len <- ifelse(input$len==0,NA, input$len)
 input$rec_len <- ifelse(input$rec_len==0,NA, input$rec_len)
 
 #Make sure lengths are numeric
 input$len <- as.numeric(as.character(input$len))
 input$rec_len <- as.numeric(as.character(input$rec_len))
 
 i <- sapply(input, is.factor)
 input[i] <- lapply(input[i], as.character)
 
 #Timestamps
 
 orig.date="1940-01-01"
 input$timestamp <- strptime(paste(input$date,input$time), format = "%Y-%m-%d %H:%M:%S") 
 input$rec_timestamp <- strptime(paste(input$rec_date,input$rec_time), format = "%Y-%m-%d %H:%M:%S") 
 
 #Dates
 input$date <-     strptime(input$date, format = "%Y-%m-%d") 
 input$rec_date <- strptime(input$rec_date, format = "%Y-%m-%d") 
 
 
 #Month
 input$month<- months(as.POSIXlt(input$timestamp, format="%Y-%b-%d"))
 input$rec_month<- months(as.POSIXlt(input$rec_timestamp, format="%Y-%b-%d"))
 input$year <- year(as.POSIXlt(input$timestamp, format="%Y-%b-%d"))
 input$rec_year <- year(as.POSIXlt(input$rec_timestamp, format="%Y-%b-%d"))
 input$yrmon<-as.yearmon(input$date)
 input$rec_yrmon <- as.yearmon(input$rec_date)
 
 #Julian day
 input$jday     <- julian(as.POSIXlt(input$date, format="%Y-%b-%d"),origin=orig.date)
 input$rec_jday <- julian(as.POSIXlt(input$rec_date, format="%Y-%b-%d"),origin=orig.date)
 #Time at liberty
 input$days_at_liberty <- as.numeric(difftime(input$rec_timestamp,input$timestamp,units='days'))
 rel_rec<-input
 
 
 x<-rel_rec[!is.na(rel_rec$kms) & rel_rec$kms > 4000,]
 
 
 ########################
###Growth modeling ####
 ########################
 
 #rel_rec <- read.csv('/home/dbeare/aottp.tagging.csv',header=T)
 #rel_rec <- read.table('/home/dbeare/aottp.tagging.csv',header=T,sep="|")
 
 test <- read.table("\\Tunadata\\aottp\\AOTTP\\Meetings\\BigeyeDataPrepApril2018\\AOTTP_BET_RELEASES_RECOVERIES.csv",sep='|',header=T)
 
 list.files("\\Tunadata\aottp\AOTTP\AOTTP-EU FINANCIAL REPORTS")
 
 
 dim(rel_rec)
 gth <- rel_rec
 what.species <- 'BET'
 gth$dL <- gth$rec_len-gth$len # difference between release and recovery length
 gth <- gth[!is.na(gth$dL) & gth$dL > 0,]; # discard zeros and NAs
 gth <- gth[!is.na(gth$days_at_liberty) & gth$days_at_liberty > 29,] # only use data with 1month or more at liberty
 gth <- gth[gth$dL/gth$days_at_liberty < 1,] 
 
 
 gth <- gth[gth$speciescode == what.species,];dim(gth)
 summary(gth$dL)
 summary(gth$days_at_liberty)
 summary(gth$len);summary(gth$rec_len)
 
 #Attach packages
 #install.packages('FSA','FSAdata','nlstools')
 library(FSA)
 library(FSAdata)
 library(nlstools)

# Get starting values for Linf and K
 
gth<- gth[!is.na(gth$len),]
gth<- gth[!is.na(gth$rec_len),]
 
# Linf0
 
Linf0<-max(gth$rec_len,na.rm=T)
Linf0

# may be more realistic to fix a Linf0 based on max size observed in catch

Linf0<-Linf0
 
x <- log(gth$rec_len)-log(gth$len)
y <-gth$days_at_liberty/365
 
mat<- data.frame(x=x,y=y)
mat<-mat[mat$x!=0,]
mat<-mat[mat$y!=0,]
mat$x.y <- mat$x/mat$y
K0 <- median(mat$x.y,na.omit=T)

# K0 

K0
 
 Fabens.sv <- list(Linf=Linf0,K=K0)
 fvb <- vbFuns("Fabens")
 FVB1 <- nls(gth$rec_len ~ fvb(gth$len,gth$days_at_liberty/365,Linf,K),start=Fabens.sv,data=gth)
 
 summary(FVB1,correlation=TRUE)
 
 output <- summary(FVB1)
 capture.output(output,file = "summaryFabens.txt")
 overview(FVB1)
 
 
 # or by using length increment
 
 fvb2 <- vbFuns("Fabens2")
 FVB2 <- nls(gth$dL ~ fvb2(gth$len,gth$days_at_liberty/365,Linf,K),start=Fabens.sv,data=gth)
 output <- summary(FVB2,correlation=TRUE)
 capture.output(output,file='SummaryFabens.txt')
 
 overview(FVB2)
 
 Id<-gth$speciescode; LF1<-gth$len; LF2<-gth$rec_len; DL<-gth$len-gth$rec_len; Dt<-gth$days_at_liberty/365.25
 
 # PLOT residvsDelta L expected
 
 toto<-summary(FVB2,correlation=FALSE)
 Linf.fit<-toto$param[1,1]; 
 K.fit<-toto$param[2,1]
 
 
 #Plot the model fit (Lisa Ailloud) :
 # This is the von Bertalanffy equation
 
 vonB <- function(x) {Linf*(1-exp(-K*x))} 
 # It predicts length at age as a function of age (x). (Remember that we do not actually know t0 
 # with tagging data so it is left at t0=0 and instead of age we have relative age)
 # So let’s calculate the relative age?
 # I flip the equation around and make it as a function of length (L) instead:
 relage <- function(L, Linf, K) {-log(1-L/Linf)/K}

 # Here are my estimated parameters:
 Linf <- Linf.fit
 K <- K.fit
 # Here are the data
 Len_rel <- gth$len
 Len_rec <- gth$rec_len
 # we can calculate relative age at release using the relage function above:
 age_rel <- relage(Len_rel, Linf.fit, K.fit)
 #age_rel <- age_rel[!is.na(age_rel)]
 
 # we can then calculate relative age at recapture by
 # adding time at liberty to the relative age
 age_rec <- age_rel + (gth$days_at_liberty)/365.25
 
 #setting the boundaries of the plot:
 range(age_rel,na.rm=T)
 range(age_rec,na.rm=T)
 xlim <- c(0,8)
 range(Len_rel)
 range(Len_rec)
 ylim <- c(0,180)
 # creating an empty plot:
 par(las=1, mar=c(6,6,4,2)+0.1,mgp=c(3.5,1,0), mfrow=c(1,1))
 plot(2,2,xlim = xlim, ylim = ylim, xlab="Relative age (yr)", 
      ylab="Length (cm)", col=0, cex.lab=1.5, cex.axis=1.5,
      main="");title('BET growth from AOTTP estimated using Fabens model')
 # adding segments to represent the growth trajectory of each fish
 arrows(x0=age_rel, y0=Len_rel, x1=age_rec, y1=Len_rec, code=2,length=.1,lwd=.5)
 # adding the von Bertalanffy curve that we fitted to the data to the plot:
 # notice the origin crosses (0,0)
 # that's because we cannot get absolute age with Fabens method
 # just relative age
 curve(vonB, 0, 8, add=TRUE, col="red", lwd=2.5)
 x
 
###########################################
# # Plotting
# # Frequencies
###########################################
 
 fplot(input=rel_rec,what.to.plot='kms',what.species='YFT',max.obs=5000)
 fplot(input=rel_rec,what.to.plot='kms',what.species='BET',max.obs=5000)
 
 
 fplot(input=rel_rec[rel_rec$project=='iccat',],what.to.plot='days_at_liberty',what.species='YFT',max.obs=900)
 fplot(input=rel_rec,what.to.plot='days_at_liberty',what.species=c('BET','SKJ','LTA','YFT'),max.obs=900)
 fplot(input=rel_rec,what.to.plot='days_at_liberty',what.species=c('BET','YFT'),max.obs=700)
#
 fplot(input=rel_rec,what.to.plot='nautical_m',what.species=c('BET','SKJ','LTA','YFT'),max.obs=2000)
 fplot(input=rel_rec,what.to.plot='kg',what.species=c('BET','SKJ','LTA','YFT'),max.obs=15)
 fplot(input=rel_rec,what.to.plot='rec_kg',what.species=c('BET','SKJ','LTA','YFT'),max.obs=15)
 fplot(input=rel_rec[rel_rec$project == 'aottp',],what.to.plot='len',what.species=c('BET','SKJ','LTA','YFT'),max.obs=150)
 fplot(input=rel_rec[rel_rec$project == 'iccat',],what.to.plot='relen',what.species=c('BET','SKJ','LTA','YFT'),max.obs=150)
 
 fplot(input=rel_rec,what.to.plot='relen',what.species=c('BET','SKJ','LTA','YFT'),max.obs=150)
 fplot(input=rel_rec[rel_rec$zone == 'F',],what.to.plot='days_at_liberty',what.species=c("BET","SKJ",'YFT'),max.obs=550)
 fplot(input=rel_rec[rel_rec$zone == 'B',],what.to.plot='days_at_liberty',what.species=c("BET","SKJ",'YFT'),max.obs=550)
 
#
# maps
# points
 #ftags <- read.csv(file="\\\\tunadata/AOTTP/AOTTP/DataExploration/Brazil report/False tags check/Hberto_list_false_tags_Feb_2019.csv",sep=',')
 #dimnames(ftags)[[2]]<-c('tc','ctcode1','day','mon','year')
 #ftags1 <- ftags[ftags$tc==1,]
 
 
input <- rel_rec_all
mapPoints(input = input[input$tagseries == 'ATP',],what.longitude = "relonx",what.latitude="relaty",
  what.species =c("BET","YFT","SKJ"),what.size=1)
mapPoints(input = input[input$tagseries != 'ATP',],what.longitude = "relonx",what.latitude="relaty",
  what.species =c("BET","YFT","SKJ","SAI"),what.size=1)
mapPoints(input = input[input$tagseries != 'ATP',],what.longitude = "relonx",what.latitude="relaty",
  what.species =c("BFT","ALB","BSH"),what.size=1)



mapPoints(input = input[input$vesselid != 1017,],what.longitude = "relonx",what.latitude="relaty", what.species = c("BET","LTA","SKJ","YFT"),what.size=2)

 
 
 #### TAG SEEDING #####
 
 tseed <- sqlQuery(aottp,"SELECT * from releases_recoveries_tagseeding_export")
 tseed <- subset(tseed, select = -rcnotes)
 ts1<- TagSeedingTab(input=tseed)$tagSeedRel
 ts2<- TagSeedingTab(input=tseed)$tagSeedRec
 ts3<- TagSeedingTab(input=tseed)$tagSeedPerc
 
 mapPoints(input=tseed,what.longitude = 'relonx',
   what.latitude = "relaty",what.species=c("BET","SKJ","YFT"),
   lon.limits=c(-40,30),lat.limits=c(-25,25))
 
 
 
 
 plot(input)
 
 
 mapPoints(input = rel_rec[!is.na(rel_rec$electronictagcode1),],what.longitude = "longitude",what.latitude="latitude", what.species = c("SKJ","YFT","BET"))
 mapPoints(input = rel_rec[!is.na(rel_rec$electronictagcode1),],what.longitude = "rec_longitude",what.latitude="rec_latitude", what.species = c("SKJ","YFT","BET"))
 mapPoints(input = rel_rec[rel_rec$model == 'Lotek-2810',],what.longitude = "rec_longitude",what.latitude="rec_latitude", what.species = c("YFT","BET"))

 mapPoints(input = rel_rec[rel_rec$model == 'Lotek-2810',],what.longitude = "longitude",what.latitude="latitude", what.species = c("BET","YFT"))
#
 mapPoints(input = rel_rec[rel_rec$model == 'MiniPAT-348C',],what.longitude = "longitude",what.latitude="latitude", what.species = c("YFT","BET"))
 mapPoints(input = rel_rec[rel_rec$model == 'MiniPAT-348C',],what.longitude = "longitude",what.latitude="latitude", what.species = c("YFT","BET"))

 # Release locations of e tags
mapPoints(input=etags[etags$supplier=='WC',],what.longitude = 'relonx',what.latitude = "relaty",what.species=c("BET","SKJ","YFT"))
mapPoints(input=etags[etags$supplier=='WC',],what.longitude = 'relonx',what.latitude = "relaty",what.species=c("BET"))

mapPoints(input=etags[etags$supplier=='DS',],what.longitude = 'relonx',what.latitude = "relaty",what.species=c("BET","SKJ","YFT"))
mapPoints(input=etags[etags$supplier=='LOTEK LAT2810',],what.longitude = 'relonx',what.latitude = "relaty",what.species=c("BET","SKJ","YFT"))
# Recovery locations of e tags
mapPoints(input=etags[etags$supplier=='WC',],what.longitude = 'rclonx',what.latitude = "rclaty",what.species=c("BET","SKJ","YFT"))
mapPoints(input=etags[etags$supplier=='DS',],what.longitude = 'rclonx',what.latitude = "rclaty",what.species=c("BET","SKJ","YFT"))
mapPoints(input=etags[etags$supplier=='LOTEK 8610',],what.longitude = 'rclonx',what.latitude = "rclaty",what.species=c("BET","SKJ","YFT"))

mapPoints(input=elec.r1[elec.r1$supplier == 'WC',],what.longitude = 'relonx',what.latitude = "relaty",
          what.species=c("BET","YFT"),what.size=2)

mapPoints(input=elec.r1[elec.r1$supplier == 'WC',],what.longitude = 'rclonx',what.latitude = "rclaty",
  what.species=c("BET","YFT"),what.size=1)



 
 
 mapPoints(input = iotc,what.longitude = "longitude",what.latitude="latitude", what.species = c("YFT","BET"),location=c(60,0))
#
#
 mapPoints(input = rel_rec[rel_rec$eez == 'Spanish EEZ (Canary Islands)',],what.longitude = "longitude",what.latitude="latitude", what.species = c("YFT","BET","SKJ"))
#
# cislas <- rel_rec[rel_rec$eez == 'Spanish EEZ (Canary Islands)',]
# map('world',xlim=c(-30,30),ylim=c(-10,45))
# points(cislas$longitude,cislas$latitude,col=2)
#
#
 mapPointsSpeciesByMonth(input = rel_rec[rel_rec$tagseeding==0,]
                         , what.longitude='longitude',what.latitude='latitude',what.species = c("BET","LTA","SKJ","YFT"),
                         what.facet='yrmon',what.size=2,ncol=6)
 

 
 
  mapPointsSpeciesByMonth(input = rel_rec[rel_rec$tagseeding==0,], 
                          what.longitude='rec_longitude',what.latitude='rec_latitude',what.species = c("BET"),what.size=2, what.facet='rec_yrmon',ncol=6)

 
 
 
 #
# #hexbins
 mapHexbin(input = rel_rec, what.longitude='relonx',what.latitude='relaty',what.species = c("SKJ","LTA","YFT","BET") ,nbins=150)
 mapHexbin(input = rel_rec, what.longitude='rclonx',what.latitude='rclaty',what.species = c("SKJ","LTA","YFT","BET") ,nbins=150)
 
# # Nautical miles
 input <-rel_rec
pander(tapply(input$nautical_m,input$speciescode,summary,na.rm=T))
# #TaL
tapply(input$days_at_liberty,input$speciescode,summary,na.rm=T)
tapply(input$days_at_liberty,input$speciescode,summary,na.rm=T)
tapply(rel_rec$days_at_liberty,rel_rec$requad,summary,na.rm=T)

xyplot(days_at_liberty ~ redate|requad,groups=speciescode,data=rel_rec[rel_rec$project == 'iccat' & 
      rel_rec$days_at_liberty >0 & rel_rec$days_at_liberty <= 3650,])


xyplot(days_at_liberty ~ redate|requad,data=rel_rec[rel_rec$project == 'aottp' &
      rel_rec$days_at_liberty >0 & rel_rec$days_at_liberty <= 3650,])















#
# #tracks
input <-rel_rec
 mapTrack(input = input,what.distance=1250, what.species=c('BET',"YFT"),what.size=.01,
   lon.limits=c(-50,20),lat.limits=c(-40,40))
 
 iccat <-rel_rec[rel_rec$project == 'iccat',]
 
 mapTrack(input = input[input$project=='iccat',],what.distance=1, 
    what.species=c('YFT'),
   what.size=.01,
   lon.limits=c(-90,20),lat.limits=c(-50,40))
 
 sth <- iccat[iccat$reeez == 'St. Helena EEZ',]
 
 mapTrack(input = sth,what.distance=1, what.species=c('BET','SKJ','YFT'),what.gear = c('PS','HL','BB','GILL','UNKN','LL','SPOR'),
   what.size=.01,
   lon.limits=c(-6,-5.0),lat.limits=c(-16.25,-15.5))
 
 
  mapTrack(input = input, what.species='YFT',what.size=1)
 mapTrack(input = input[input$nautical_m >= 500,], what.species='SKJ',what.size=1)
 
 mapTrack(input = input, what.species='LTA',what.size=.1)
 
 
 mapTrack(input = rel_rec[rel_rec$nautical_m >= 50,],what.species='SKJ',what.gear='BB',what.size=.5)
 mapTrack(input = rel_rec[rel_rec$nautical_m >= 50,],what.species='SKJ',what.gear='PS',what.size=.25)
 mapTrack(input = rel_rec[rel_rec$tagseeding==0 & is.na(rel_rec$electronictagcode1)& rel_rec$nautical_m >= 500,],what.species=c('BET','LTA','SKJ','YFT'),what.size=.1)
 mapTrack(input = rel_rec[rel_rec$nautical_m >= 1250,],what.species=c('SKJ','YFT'),what.size=.25)
 
 
 mapTrack(input = rel_rec[rel_rec$nautical_m >= 100,],what.species=c('BET','LTA','SKJ','YFT'),what.gear='BB',what.size=.25)
 mapTrack(input = rel_rec[rel_rec$nautical_m >= 100,],what.species=c('BET','LTA','SKJ','YFT'),what.gear='PS',what.size=.25)
 mapTrack(input = rel_rec[rel_rec$nautical_m >= 100,],what.species=c('BET','LTA','SKJ','YFT'),what.gear='LL',what.size=.25)
 mapTrack(input = rel_rec[rel_rec$nautical_m >= 100,],what.species=c('BET','LTA','SKJ','YFT'),
   what.gear=c('BB','PS','LL'),what.size=.25)
 
 
 mapTrack(input = elec[elec$renumtagelectronic == "86929",],what.species=c('BET'),what.distance=5,what.size=1)

 ltk <-elec[!is.na(elec$days_at_liberty) & elec$days_at_liberty > 365 & elec$supplier == 'LOTEK LAT2810',]

  mapTrack(input = ltk[ltk$renumtagelectronic == 86827,],what.species=c('YFT'),what.distance=3,what.size=1,
    lon.limits=c(-50,10),lat.limits=c(-5,10))
 
  mapTrack(input = ltk[ltk$renumtagelectronic == 86859,],what.species=c('YFT'),what.distance=3,what.size=1,
    lon.limits=c(-50,50),lat.limits=c(-50,20))
  
 
 
 
 
 
#
#
# #scatterpids
 mapScatterpie()
 mapScatterpie(input=rel_rec,what.species=c('BET','YFT'),sf=3)
 mapScatterpie(input=rel_rec,what.species=c('BET','SKJ','YFT'),sf=5)
 mapScatterpie(input=rel_rec,what.longitude='rec_longitude',
               what.latitude='rec_latitude',what.yrmon='rec_yrmon',what.species=c('BET','SKJ','YFT'),sf=3)
#
#
# mapScatterpie(input=rel_rec[year(rel_rec$date)==2016,],what.species=c('BET','SKJ','YFT'),sf=4)
# mapScatterpie(input=rel_rec[year(rel_rec$date)==2017,],what.species=c('BET','SKJ','YFT'),sf=4)
#
# #tables
 relRecSummaryTab(input=rel_rec[rel_rec$tagseeding==0,])
 pander(relRecSummaryTab()$Releases)
 pander(relRecSummaryTab()$Recoveries)
 
 relRecSummaryTab(input=rel_rec[rel_rec$tagseeding==0 & rel_rec$vesselid != 1017,])
 
 
 
 
#
# #double-tagging, tag-shedding
#
 TagSheddingTab(input=rel_rec[rel_rec$tagseeding==0,])$Double_Tag_Nos
 TagSheddingTab(input=rel_rec[rel_rec$tagseeding==0,])$Tag_Shed_Nos
 TagSheddingTab(input=rel_rec[rel_rec$tagseeding==0,])$Tag_Shed_Perc
 TagSheddingTab(input=rel_rec[rel_rec$quad == 'NE',])
#
# #chemically-tagged totals
#
 ChemTaggingTab(input=rel_rec[rel_rec$tagseeding==0,])
apply(ChemTaggingTab(),1,sum)
 
 #
 table(releases$ctcolor1,releases$speciescode)
#
# #releases and recoveries in time
#

relRecTimeSeries(rel_rec,what.species=c('BET','YFT'))
 
 rel_rec$redate[rel_rec$redate == '2019-06-04'] <- '2019-05-04'
 
 relRecTimeSeries(rel_rec,what.species=c('YFT'))
 relRecTimeSeries(rel_rec[rel_rec$redate > '2019-01-01',],what.species=c("YFT"))
 
 
 relRecTimeSeries(input=data(iotc))
 relRecTimeSeries(input=bermuda)
 relRecTimeSeries(input=rel_rec[rel_rec$reteam == 'E33',])
 
 relRecTimeSeries(input=iotc,what.species='BET')
#
# #tag-seeding
#
 pander(TagSeedingTab(input=rel_rec)$tagSeedRel)
 pander(TagSeedingTab(input=rel_rec)$tagSeedRec)
 pander(TagSeedingTab(input=rel_rec)$tagSeedPerc)
#
# #n tags by country
#
 pander(nTagsRelByCountry())
#
# #e tags
 
 etags1 <- sqlQuery(aottp, "SELECT supplier,count(supplier) from releases_electronic group by supplier ;") 
 
 
#
pander(nElectronicTagsTab()$eTagRel)
#
# x<- rel_rec[rel_rec$model=='Lotek-2810' & rel_rec$recovered ==T,]
# x<- rel_rec[!is.na(rel_rec$model) & rel_rec$model == 'DS-SeaTag-3D-PSAT' & rel_rec$rcstagecode == 'R-1',]
# ds <- data.frame(speciescode=x[,2],ctcode1=x$ctcode1,date=x$date,time=x$time,latitude=x$latitude,longitude=x$longitude,len=x$len,notes=x$notes,supplierserialnumber=x$supplierserialnumber)
#
# write.table(ds,'/home/dbeare/ds-tag-deployments.csv',sep=',',row.names=F)
#
#
#
# # FAD moratorium
jf2017 <- rel_rec[rel_rec$year ==2017 & rel_rec$month %in% c('January','February'),]

table(jf2017$fmor17)
table(jf2017$fmor17,jf2017$speciescode)
table(jf2017$fmor17,jf2017$speciescode,jf2017$recovered)


jf2018 <- rel_rec[rel_rec$year ==2018 & rel_rec$month %in% c('January','February'),]

table(jf2018$fmor18)
table(jf2018$fmor18,jf2018$speciescode)
table(jf2018$rec_fmor18,jf2018$speciescode)


#
#
# #growth tracks

#
rel_rec <- timeVectors()
rel_rec$dlen <- rel_rec$rclen-rel_rec$relen
 growthTrack(input=rel_rec,what.species ="SKJ")
 growthTrack(input=rel_rec[rel_rec$score > 3 & rel_rec$rcgearcode == 'PS',],what.species =c('BET','LTA','SKJ','YFT'))
 growthTrack(input=rel_rec[rel_rec$rcgearcode=="BB" & rel_rec$dlen>0 & rel_rec$days_at_liberty > 60,],
   what.size=.1,what.species =c('BET'))

 
 
 
# By quadrant
#
 
table(rel_rec$requad)
 
input <- rel_rec[rel_rec$tagseeding == 0,]

qdr <- as.character(input$requad[input$speciescode %in% c('BET','SKJ','YFT','WAH','LTA')])
       
spc <- as.character(input$speciescode[input$speciescode %in% c('BET','SKJ','YFT','WAH','LTA')])

tbq <- table(spc,qdr)

tbq

apply(tbq,2,sum)

tbyrmon<- table(rel_rec$yrmon,rel_rec$quad)
write.table(tbyrmon,file='/home/dbeare/aottp.by.mon.yr.quad.csv',sep=',')

table(qdr)


# By Zone
#

table(rel_rec$zone)

input <- rel_rec[rel_rec$tagseeding == 0,]

zdr <- as.character(input$zone[input$speciescode %in% c('BET','SKJ','YFT','WAH','LTA')])

spc <- as.character(input$speciescode[input$speciescode %in% c('BET','SKJ','YFT','WAH','LTA')])

tbq <- table(spc,zdr)

tbq

table(qdr)

ztbyrmon<- table(rel_rec$yrmon,rel_rec$zone)
write.table(ztbyrmon,file='/home/dbeare/aottp.by.mon.yr.zone.csv',sep=',')

qtbyrmon<- table(rel_rec$yrmon,rel_rec$quad)
write.table(qtbyrmon,file='/home/dbeare/aottp.by.mon.yr.quad.csv',sep=',')

#
## Recoveries by EEZ
#
 table(rel_rec$rec_eez,rel_rec$speciescode)
 plot(rel_rec$rec_longitude,rel_rec$rec_latitude,pch='.')
 plot(eez,add=TRUE)
 points(recoveries$longitude,recoveries$latitude,col='red',pch='.')
#
# #Probability of being re-caught
#
 rel_rec$bin <- ifelse(rel_rec$recovered == TRUE,1,0)
 data1 <- rel_rec[!is.na(rel_rec$len),]
 data1 <- data1[data1$speciescode %in% c('BET','LTA','SKJ','YFT'),]
 data1$speciescode <- as.factor(data1$speciescode)
 data1$rec_gearcode <- as.factor(data1$rec_gearcode)
  #z1 <- gam(bin ~ s(len,by=speciescode)+s(longitude,latitude,days_at_liberty,by=speciescode), data=data1,family='quasibinomial' ) # P(Recapture) depends strongly on release location.
  dat.bet <- data1[!is.na(data1$len) & data1$speciescode == 'BET',]
 bet.z1 <- gam(bin ~ s(longitude,latitude)+s(len), data=dat.bet,family='quasibinomial' )
# #

dat.bet$P_capture <- round(predict(bet.z1,dat.bet,type='response'),2)
#
 gd <- expand.grid(speciescode=c('BET','LTA','SKJ','YFT'),len=52,latitude=15,longitude=-18)
#
# dat.bet$P_capture <- round(predict(z1,data1[!is.na(data1$len) & data1$speciescode == 'BET',] ,type='response'),2)
#
# Atl <- c(-30,0)
# wAfMap <- get_map(location=Atl,source='google',maptype='satellite',crop=TRUE,zoom=3)
#
# ggmap(wAfmap)
#
#

 world <- map_data('world');
 
 ggplot(data=dat.bet,aes(x=longitude,y=latitude))+
   coord_fixed(1.3,xlim=c(-80,30),ylim=c(-50,50)) +
 geom_point(aes(x=jitter(longitude),y=jitter(latitude),color=P_capture,size=P_capture),data=dat.bet) +
   scale_color_gradientn(colours=heat.colors(100)) +
   geom_polygon(data=world,aes(x=long,y=lat,group=group),col='darkgreen',fill='darkgreen')
   

 
 
 
 
 #
#
#
#
#

 input <- fortify(input)
 brazil <- eez[eez@data$EEZ=='Brazilian Exclusive Economic Zone',]
 ghana <- eez[eez@data$EEZ=='Ghanaian Exclusive Economic Zone',]
 ivory <- eez[eez@data$EEZ=='Ivory Coast Exclusive Economic Zone',]
 togo <- eez[eez@data$EEZ=='Togolese Exclusive Economic Zone',]
 benin <- eez[eez@data$EEZ=='Beninese Exclusive Economic Zone',]
 gabon <- eez[eez@data$EEZ=='Gabonese Exclusive Economic Zone',]
 congo <- eez[eez@data$EEZ=='Congolese Exclusive Economic Zone',]
 eqg  <- eez[eez@data$EEZ=='Equatorial Guinean Exclusive Economic Zone',]
 stp  <- eez[eez@data$EEZ=='Sao Tome and Principe Exclusive Economic Zone',]
 nigj  <- eez[eez@data$EEZ=='Nigeria - Sao Tome and Principe Joint',]
 nig  <- eez[eez@data$EEZ=='Nigerian Exclusive Economic Zone',]
 st.helena <- eez[eez@data$EEZ=='St. Helena Exclusive Economic Zone',]
 ascension <- eez[eez@data$EEZ=='Ascension Exclusive Economic Zone',]
 namibia  <-   eez[eez@data$EEZ=='Namibian Exclusive Economic Zone',]
 angola  <-   eez[eez@data$EEZ=='Angolan Exclusive Economic Zone',]
 
 location<-c(-5,0)
 wAfMap <- get_map(location=location,source='google',maptype='satellite',crop=TRUE,zoom=4)
 ggmap(wAfMap) +
   geom_polygon(data=fadmoratorium,aes(x=long,y=lat,group=group),color='orange',size=1) +
     geom_polygon(data=ghana,aes(x=long,y=lat),color='lightblue',fill=NA,size=1) +
     geom_polygon(data=ivory,aes(x=long,y=lat,group=group),color='lightblue',fill=NA,size=1) +
      theme(plot.margin=unit(c(.1,.1,.1,.1),"cm"),
         axis.text.x =element_text(colour="grey20",size=12,face="plain"),
         axis.text.y=element_text(colour="grey20",size=12,face="plain"),
         axis.title.y=element_text(size=15))+
   xlab("")+ylab("")
 
 location<-c(7,0)
 lat <- c(2,-4)
 lon <- c(9,9)
 label <- c("1","2")
 df<-data.frame(lon,lat,label)
 
 wAfMap <- get_map(location=location,source='google',maptype='satellite',crop=TRUE,zoom=6)
 ggmap(wAfMap) +
   geom_polygon(data=gabon,aes(x=long,y=lat,group=group),color='lightblue',fill=NA,size=.5) +
   geom_polygon(data=eqg,aes(x=long,y=lat,group=group),color='lightblue',fill=NA,size=.5) +
   geom_polygon(data=stp,aes(x=long,y=lat,group=group),color='lightblue',fill=NA,size=.5) +
   geom_text(data=df,aes(x=lon,y=lat,label=label,size=5),color='white',vjust=0) +
   theme(plot.margin=unit(c(.1,.1,.1,.1),"cm"),
         axis.text.x =element_text(colour="grey20",size=12,face="plain"),
         axis.text.y=element_text(colour="grey20",size=12,face="plain"),
         axis.title.y=element_text(size=15))+
   xlab("")+ylab("")
 
 # Brazil 5-8N
 
 location<-c(-30,0)
# lat <- c(2,-4)
#lon <- c(9,9)
#label <- c("1","2")
#df<-data.frame(lon,lat,label)
 
 wAfMap <- get_map(location=location,source='google',maptype='satellite',crop=TRUE,zoom=3)
 
 ggmap(wAfMap) +
   geom_polygon(data=brazil,aes(x=long,y=lat,group=group),color='lightblue',fill=NA,size=.5) +
   geom_hline(yintercept=5, color='white',size=.2)+
   geom_hline(yintercept=8, color='white',size=.2)+ 
   geom_point(data=input[input$speciescode %in% c('BET','YFT'),],aes(x=longitude,y=latitude,color="speciescode"),alpha=1,size=.1)+
   
   #geom_text(data=df,aes(x=lon,y=lat,label=label,size=5),color='white',vjust=0) +
   theme(plot.margin=unit(c(.1,.1,.1,.1),"cm"),
         axis.text.x =element_text(colour="grey20",size=12,face="plain"),
         axis.text.y=element_text(colour="grey20",size=12,face="plain"),
         axis.title.y=element_text(size=15))+
   xlab("")+ylab("")
 
 par(mar=c(4,4,2,2))
 plot(c(-70,5),c(-40,20),type='n',ylab='',xlab='')
 #abline(h=c(5,8),col='green')
 segments(x0=-30,y0=5,x1=-30,y1=8,col='green',lwd=2)
 segments(x0=-60,y=5,x1=-30,y1=5,col='green',lwd=2)
 segments(x0=-60,y=8,x1=-30,y1=8,col='green',lwd=2)
 plot(brazil,add=T,lty=2)
 
 points(releases$lon[releases$gearcode=='BB'],releases$lat[releases$gearcode=='BB'],pch=16,col='blue')
 points(releases$lon[releases$gearcode=='LL'],releases$lat[releases$gearcode=='LL'],pch=16,col='red')
 abline(h=seq(-10,10,by=1),lty=3,col='orange')
 map('world',add=T,fill=T)
 
 location<-c(0,-10)
 wAfMap <- get_map(location=location,source='google',maptype='satellite',crop=TRUE,zoom=4)
 ggmap(wAfMap) +
   geom_polygon(data=ascension,aes(x=long,y=lat,group=group),color='lightblue',fill='lightblue',size=.5) +
   geom_polygon(data=st.helena,aes(x=long,y=lat,group=group),color='lightblue',fill='lightblue',size=.5) +
   geom_polygon(data=namibia,  aes(x=long,y=lat),color='lightblue',fill='lightblue',size=.5) +
   #geom_hline(yintercept=10, color='green',size=.2)   +
   #geom_vline(xintercept=-30, color='green',size=.2)   +
   theme(plot.margin=unit(c(.1,.1,.1,.1),"cm"),
         axis.text.x =element_text(colour="grey20",size=12,face="plain"),
         axis.text.y=element_text(colour="grey20",size=12,face="plain"),
         axis.title.y=element_text(size=15))+
   xlab("")+ylab("")
 
 #Tagging at St Helena

 
 mapPoints(input=rel_rec,what.species=c('BET'),lon.limits=c(-10,10), lat.limits=c(0,-20))
 
 
 
 
 
 
 
 
 
 
 
  location<-c(0,20)
 lat <- c(10,-3,26)
 lon <- c(-20,5,-18)
 label <- c("A","B","C")
 df<-data.frame(lon,lat,label)
 wAfMap <- get_map(location=location,source='google',maptype='satellite',crop=TRUE,zoom=3)
 ggmap(wAfMap) +
   geom_polygon(data=eez,aes(x=long,y=lat,group=group),color='lightblue',size=1) +
   geom_polygon(data=zna2,aes(x=long,y=lat,group=group),fill=NA,color='orange',size=.1) +
   geom_polygon(data=znb2,aes(x=long,y=lat,group=group),fill=NA,color='orange',size=.1) +
   geom_polygon(data=znc2,aes(x=long,y=lat,group=group),fill=NA,color='orange',size=.1) +
   #geom_point(data=df,aes(x=lon,y=lat,shape=label,label=label),size=1) +
   geom_text(data=df,aes(x=lon,y=lat,label=label,size=5),color='white',vjust=0) +
   #geom_polygon(data=ghana,aes(x=long,y=lat),color='lightblue',fill=NA,size=1) +
   #geom_polygon(data=ivory,aes(x=long,y=lat,group=group),color='lightblue',fill=NA,size=1) +
   theme(plot.margin=unit(c(.1,.1,.1,.1),"cm"),
         axis.text.x =element_text(colour="grey20",size=12,face="plain"),
         axis.text.y=element_text(colour="grey20",size=12,face="plain"),
         axis.title.y=element_text(size=15))+
   xlab("")+ylab("")
 
 
 
 
 

###################################################
#Modeling the time taken for fish to be recovered##
###################################################
 
# Classically, the analysis of the time to death
# But can be used anywhere you want to know what factors affect the time for an event to occur:

# Right censoring (where the date of death is unknown but is after some known date). Survival analysis
# accounts for this.
# Left censoring (incomplete survival time, e.g. following up after an exposure to infection but we don't know the exact time of exposure)

# Read in the data and chuck out negative times at liberty
 
#write.table(rel_rec,file='c:/Users/dbeare/Documents/aottp.tagging.csv',sep='|')
 
 
rel_rec <- read.table('c:/Users/dbeare/Documents/aottp.tagging.csv',sep='|',header=T)


rel_rec$speciescode <- as.character(rel_rec$speciescode)
x <- as.numeric(as.character(rel_rec$days_at_liberty))
ind <- (1:length(rel_rec[,1]))[!is.na(x) & x<1]
sdata <- rel_rec[-ind,] # chuck out really short times at liberty
dim(sdata)
sum(is.na(sdata$date))
sdata<-sdata[sdata$speciescode %in% c('BET','SKJ','YFT','LTA'),] # four main species
sdata<-sdata[sdata$rcstagecode_re =='R-1',]

start <- as.numeric(sdata$rejday)-min(as.numeric(sdata$rejday),na.rm=T)
stop  <- as.numeric(sdata$rcjday)-min(as.numeric(sdata$rcjday),na.rm=T)


#stop[is.na(stop)] <- max(stop,na.rm=T)
stop[is.na(stop)] <- 1461 # 4 years
death <- ifelse(sdata$recovered==FALSE,0,1)
eez   <- sdata$reeez
species <- sdata$speciescode
length <- sdata$relen
month <- sdata$remonth
lat <-sdata$relaty
lon <- sdata$relon

smod <- data.frame(start=start,stop=stop,death=death,eez=eez,longitude=lon,latitude=lat,species=species,length=length,month=month) # data set for modeling
 
smod <- smod[smod$stop>smod$start,]



library(survival)

S <- Surv(time = smod$start, time2 = smod$stop, event = smod$death)

model <- coxph(S ~ longitude + latitude + as.factor(species) + length, data = smod) # how does time til death (recapture) 
#depend on latitude, species and length ?

exp(coef(model))

# exp(coef) is the hazard ratio. HR = 1: No effect, HR < 1: reduction in the hazard, HR>1: Increase in the hazard.

par(mfrow=c(2,1))

plot(survfit(model, newdata = expand.grid(longitude=c(0),latitude=c(-5,0,5),species='YFT',length=c(105))), 
     xscale = 365.25,
     conf.int = F,
     xlab = "Years after tagging",
     ylab = "Proportion survived",
     col = c("red", "green","blue"))
legend(1.5, 0.9, 
       legend = c("5 South", 
                  "0","5 North"), 
       lty = 1, 
       col = c("red", "green","blue"))

plot(survfit(model, newdata = expand.grid(longitude=c(0),latitude=c(0),species='YFT',length=c(50,75,100))), 
     xscale = 365.25,
     conf.int = F,
     xlab = "Years after tagging",
     ylab = "Proportion survived",
     col = c("red", "green","blue"))
legend(1.5, 0.9, 
       legend = c("50cm", 
                  "75cm","100cm"), 
       lty = 1, 
       col = c("red", "green","blue"))

par(mfrow=c(1,1))
plot(survfit(model, newdata = expand.grid(longitude=0,latitude=0,species=c("BET","YFT","SKJ","LTA"),length=c(60))), 
     xscale = 365.25,
     conf.int = T,
     xlab = "Years after tagging",
     ylab = "Proportion survived",
     col = c("red", "green","blue","yellow"))
legend(2.5, 0.9, 
       legend = c("50cm","75cm","100cm"), 
       lty = 1, 
       col = c("red", "green","blue"))




cox.zph(model) # conservative hypothesis test


a <- cox.zph(model)
par(mfrow = c(3, 1))
plot(a[1], main = "latitude")
plot(a[3], main = "SKJ")
plot(a[5], main = "length")

##########################################################################################
# GROWTH MODELING #

devtools::install_github("quantifish/TagGrowth")

library(TagGrowth)

######################################################################################################
# RANDOM EFFECTS MODELLING OF GROWTH USING TAG RECAPTURE DATA
######################################################################################################

# Make sure R is clean
rm(list = ls())

# Load package
require(TagGrowth)

# Versions
# 0. none
# 1. k
# 2. z
# 3. y - not pdH
# 4. k, z
# 5. k, y - did not work, RETURN TO THIS
# 6. z, y
# 7. k, z, y - did not work, RETURN TO THIS
#scenarios <- c("v0/","v1/","v2/","v4/")
#scenarios <- c("v0/","v1/","v2/","v3/","v4/","v5/","v6/","v7/")
scenarios <- c("v4/")

setwd("/home/dbeare/TagGrowth/TagGrowth")


# Compile the model

compile("/home/dbeare/TagGrowth/TagGrowth/inst/executables/ATR.cpp")

# Load data
data(toothfish)

# Change to daily/weekly estimates
toothfish <- time_step(toothfish, units = "weeks")
data <- toothfish


# Specify the random-effects we want to try to estimate
for (Iscenario in scenarios)
{
  # Load the model
  dyn.load(dynlib("/home/dbeare/TagGrowth/TagGrowth/inst/executables/ATR"))
  
  folder <- Iscenario
  if (Iscenario == "v0/") # none
    Options <- c("YearTF" = 0, "AreaTF" = 0, "IndivTF" = 0, "IndivTimeTF" = 0)
  if (Iscenario == "v1/") # k
    Options <- c("YearTF" = 0, "AreaTF" = 0, "IndivTF" = 1, "IndivTimeTF" = 0)
  if (Iscenario == "v2/") # z
    Options <- c("YearTF" = 0, "AreaTF" = 0, "IndivTF" = 0, "IndivTimeTF" = 1)
  if (Iscenario == "v3/") # y
    Options <- c("YearTF" = 1, "AreaTF" = 0, "IndivTF" = 0, "IndivTimeTF" = 0)
  if (Iscenario == "v4/") # k, z
    Options <- c("YearTF" = 0, "AreaTF" = 0, "IndivTF" = 1, "IndivTimeTF" = 1)
  if (Iscenario == "v5/") # k, y
    Options <- c("YearTF" = 1, "AreaTF" = 0, "IndivTF" = 1, "IndivTimeTF" = 0)
  if (Iscenario == "v6/") # z, y
    Options <- c("YearTF" = 1, "AreaTF" = 0, "IndivTF" = 0, "IndivTimeTF" = 1)
  if (Iscenario == "v7/") # k, z, y
    Options <- c("YearTF" = 1, "AreaTF" = 0, "IndivTF" = 1, "IndivTimeTF" = 1)
  
  # Dimensions
  Nindiv <- nrow(data)
  Nyears <- length(min(data$Year0, data$Year1, data$Year2):max(data$Year0, data$Year1, data$Year2))
  Nareas <- length(unique(data$Area1))
  
  # Create lists of data and parameters
  Data <- list(Options = Options,
               iAge1 = data[1:Nindiv,'Age1'], iLiberty = data[1:Nindiv,'Liberty'],
               Length1 = data[1:Nindiv,'Length1'], Length2 = data[1:Nindiv,'Length2'],
               Sex = data[1:Nindiv,'Sex'],
               Time0 = data[1:Nindiv,'Time0'], Time1 = data[1:Nindiv,'Time1'], Time2 = data[1:Nindiv,'Time2'],
               Year0 = data[1:Nindiv,'Year0'], Year1 = data[1:Nindiv,'Year1'], Year2 = data[1:Nindiv,'Year2'],
               Area1 = data[1:Nindiv,'Area1'])
  Params <- list(ln_gamma = c(log(0.3), log(0.3)), logit_psi = qlogis(0.000001), L0 = c(0.0, 0.0),
                 ln_bmean = c(log(0.002), log(0.002)), ln_bdev = rep(0, Nindiv), ln_sd_bdev = c(log(0.001), log(0.001)),
                 ln_sd_obs = log(0.102),
                 z1 = rep(0, Nindiv), z2 = rep(0, Nindiv), ln_sd_z = log(0.001),
                 ln_ydev = rep(0, Nyears), ln_sd_ydev = log(0.001),
                 ln_xdev = rep(0, Nareas), ln_sd_xdev = log(0.001))
  
  # Use TMB's Map option to turn parameters on/off
  Random <- NULL
  Map <- list()
  Map[["logit_psi"]] <- factor(NA)
  if (Options[1] == 0)
  {
    Map[["ln_ydev"]] = factor(rep(NA, Nyears))
    Map[["ln_sd_ydev"]] = factor(NA)
  } else {
    Random = c(Random, "ln_ydev")
  } 
  if (Options[2]==0)
  {
    Map[["ln_sd_xdev"]] = factor(NA)
    Map[["ln_xdev"]] = factor(rep(NA,length(Params$ln_xdev))) 
  } else {
    Random = c(Random, "ln_xdev")
  }
  if (Options[3]==0)
  {
    Map[["ln_bdev"]] = factor(rep(NA,length(Params$ln_bdev)))
    Map[["ln_sd_bdev"]] = factor(rep(NA,2)) 
  } else {
    Random = c(Random, "ln_bdev")
  }
  if (Options[4]==0)
  {
    Map[["z2"]] = factor(rep(NA,length(Params$ln_bdev)))
    Map[["z1"]] = factor(rep(NA,length(Params$z1)))
    Map[["ln_sd_z"]] = factor(NA)  
  } else {
    Random = c(Random, "z1", "z2")
  }
  
  # Create the AD object
  obj <- MakeADFun(data = Data, parameters = Params, map = Map, random = Random, inner.control=list(maxit=50))
  
  # List of parameters that are "on"
  names(obj$par)
  
  # Set up estimation
  newtonOption(obj,smartsearch = TRUE)
  obj$fn(obj$par)
  obj$gr(obj$par)
  obj$hessian <- TRUE
  obj$control <- list(trace=100)
  ConvergeTol <- 1 # 1:Normal; 2:Strong
  #obj$env$inner.control$step.tol <- c(1e-12,1e-15)[ConvergeTol] # Default : 1e-8  # Change in parameters limit inner optimization
  #obj$env$inner.control$tol10 <- c(1e-8,1e-12)[ConvergeTol]  # Default : 1e-3     # Change in pen.like limit inner optimization
  #obj$env$inner.control$grad.tol <- c(1e-12,1e-15)[ConvergeTol] # # Default : 1e-8  # Maximum gradient limit inner optimization
  summary(obj)
  
  Upr <- rep(Inf, length(obj$par))
  Lwr <- rep(-Inf, length(obj$par))
  Upr[match("logit_psi",names(obj$par))] = qlogis(0.999)
  Lwr[match("logit_psi",names(obj$par))] = qlogis(0.001)
  Lwr[match("ln_sd_z",names(obj$par))] = log(0.001)
  Lwr[match("ln_sd_xdev",names(obj$par))] = log(0.001)
  Lwr[match("ln_sd_bdev",names(obj$par))] = log(0.0001)
  
  # Optimize!
  opt <- nlminb(start = obj$par, objective = obj$fn, gr = obj$gr, upper = Upr, lower = Lwr, control = list(eval.max = 1e4, iter.max = 1e4, rel.tol = c(1e-10, 1e-8)[ConvergeTol], trace = 1))
  opt[["final_gradient"]] <- obj$gr(opt$par)
  Diag <- obj$report()
  Report <- sdreport(obj)
  
  # Do we need this bit?
  #Hess <- optimHess(par = opt$par, fn = obj$fn)
  #opt <- nlminb(start = opt$par, objective = obj$fn, upper = Upr, lower = Lwr, control = list(eval.max = 1e4, iter.max = 1e4, rel.tol = c(1e-10, 1e-8)[ConvergeTol], trace = 1))
  
  # Dynamically unload the model
  dyn.unload(dynlib("/home/dbeare/TagGrowth/TagGrowth/inst/executables/ATR"))
  
  # Save outputs
  capture.output(Report, file = paste(folder, "Report.txt", sep = ""))
  save(obj, file = paste(folder, "obj.RData", sep = ""))
  save(opt, file = paste(folder, "opt.RData", sep = ""))
  save(Report, file = paste(folder, "Report.RData", sep = ""))
  write.csv(data.frame(names(Report$value), Report$value), file = paste(folder, "Pars.csv", sep = ""), row.names = TRUE)
  
  # Is the fit positive definite Hessian?
  print(Report$pdHess)
}





######################################################################################################
# Inspect results
######################################################################################################

Delta <- rep(0,length(opt$par))
Delta[2] = 1e-5
(obj$fn(opt$par - Delta/2) - obj$fn(opt$par + Delta/2))  / abs(max(Delta))

# Check none of the parameters are up against the bounds
cbind(Lwr, opt$par, Upr)

REs_b <- Report$par.random[names(Report$par.random) %in% "ln_bdev"]
REs_z1 <- Report$par.random[names(Report$par.random) %in% "z1"]
REs_z2 <- Report$par.random[names(Report$par.random) %in% "z2"]
REs_y <- Report$par.random[names(Report$par.random) %in% "ln_ydev"]
head(Report$value, 15)
t(t(tapply(X = Report$value, INDEX = names(Report$value), FUN = length)))

# END


###########################################################################

# Capture-recapture matrix Matt Lauretta #

library(RODBC)
library(lubridate)
library(FSA);
library(dplyr);
library(Rcapture)

data.source.name <- 'aottp'
aottp <- odbcConnect(dsn=data.source.name, case="postgresql" ,believeNRows=FALSE)
aottp.tables <- sqlTables(aottp) # extract list of the tables in the database
rels_recs <- sqlQuery(aottp,"SELECT * FROM releases_recoveries ")
head(rels_recs)

rels_recs$re_month=month(rels_recs$re_date)
rels_recs$rc_month=month(rels_recs$rc_date)

rels_recs <- rels_recs[rels_recs$speciescode == 'YFT',]

BET_releases=aggregate(specimenid~re_month+re_year,data=rels_recs,length)
colnames(BET_releases)[3] <- 'n'

BET_recoveries=function(reYear,reMonth,rcYear,rcMonth)
{
  cohort=subset(rels_recs,re_year==reYear&re_month==reMonth)
  recovered=subset(cohort,rc_year==rcYear&rc_month==rcMonth)
  no_recoveries=length(recovered[,1])
  no_recoveries
}

# Capture-recapture matrix
BET_returns=t(sapply(1:length(BET_releases[,1]),function(i)
  c(mapply(BET_recoveries,BET_releases[i,2],BET_releases[i,1],2016,6:12),
    mapply(BET_recoveries,BET_releases[i,2],BET_releases[i,1],2017,1:12))))

mat1 <- cbind(BET_releases,BET_returns)
mrOpen(mat1)

#########################

trip1 <- read.table('/home/dbeare/Desktop/PositionGPS_itineraire_marquage_2018-02-26.csv',sep=',',header=T)


plot(trip1$Longitude,trip1$Latitude)


location <- c(-4,5)
input <- fortify(trip1)
wAfMap <- get_map(location=location,source='google',maptype='terrain',crop=TRUE,zoom=10)
ggmap(wAfMap) +
  #coord_fixed(1.3,xlim=c(-40,30),ylim=c(-40,45)) +
  geom_point(data=input,aes(x=Longitude,y=Latitude),alpha=1,size=2)+
geom_polygon(data=eez,aes(x=long,y=lat,group=group),color='lightblue',fill=NA,size=0.1)


bio <- sqlQuery(aottp,"SELECT * from biologic")
bio$speciescode <- releases$speciescode[match(bio$specimenid,releases$specimenid)]
bio$len <- recoveries$len[match(bio$specimenid,recoveries$specimenid)]
bio$speciescode <- as.character(bio$speciescode)
bio$biosexcode[!is.na(bio$biosexcode) & bio$biosexcode == 'm'] <- 'M'
bio$biosexcode <- as.character(bio$biosexcode)

table(bio$speciescode)

### CODE Tables ########

beaufort<- sqlQuery(aottp, "SELECT * from beaufwindspeeds")
write.table(beaufort,file='/home/dbeare/beaufort.csv',sep=',')

doug<- sqlQuery(aottp, "SELECT * from douglaswseastates")
write.table(doug,file='/home/dbeare/douglas.csv',sep=',')

gears<- sqlQuery(aottp, "SELECT * from gears ")
write.table(gears,file='/home/dbeare/gears.csv',sep=',')

fishinjury<- sqlQuery(aottp, "SELECT * from fishinjuries ")
write.table(fishinjury,file='/home/dbeare/fishinjury.csv',sep=',')

skyconditions<- sqlQuery(aottp, "SELECT * from skyconditions ")
write.table(skyconditions,file='/home/dbeare/skyconditions.csv',sep=',')




### Read in spatial objects from postgis to R

library(RPostgreSQL)
library(postGIStools)

con <- dbConnect(PostgreSQL(), dbname = "aottp", user = "aottpw",
                 host = "172.16.1.48",
                 password = "tunasw")
yfta<- get_postgis_query(con, "SELECT * FROM yftfa",
                               geom_name = "geom")

plot(yfta)


#with RODBC
aottp <-  odbcConnect("aottp-local", case ="tolower",DBMSencoding='utf8')


query <- "SELECT * FROM  gis.yftfa"
yftfa <- sqlQuery(aottp, query)
coordinates(chisoaks) <- ~x + y
plot(chis)
points(chisoaks, pch = 21, bg = 2, cex = 0.4)
box()
axis(1)
axis(2)
grid()


query <- "PG:dbname='aottp' host='172.16.1.48' port='5432' user='tunasw' password='aottpw'"

ogrListLayers(dsn)
