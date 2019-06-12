cruises <- function(input=rel_rec)
{
#input <- rel_rec

input <- orderBy(~retimestamp+surveycode,data=input) # order data from first released fish
input <- input[input$tagseeding ==0,]

start.time<- aggregate(list(start.time=input$retimestamp),
                       by=list(vesselid=input$revesselid,
                               gearcode=input$regearcode,surveycode=input$surveycode),min,na.rm=T)
end.time  <- aggregate(list(end.time=input$retimestamp),
                       by=list(vesselid=input$revesselid,
                               gearcode=input$regearcode,surveycode=input$surveycode),max,na.rm=T)

t_a <- start.time
t_a$end.time <- end.time$end.time
t_a <- orderBy(~start.time,data=t_a)

vessels$vesselname <- as.character(vessels$vesselname)
t_a$revesselname <- vessels$vesselname[match(t_a$vesselid,vessels$vesselid)]
t_a$vesselnotes <- vessels$vesselnotes[match(t_a$vesselid,vessels$vesselid)]

t_a$gearcode <- ifelse(t_a$gearcode == 'PS','BB',t_a$gearcode) 

#t_a[is.na(t_a$start.time),] # check for NAs
#t_a[is.na(t_a$end.time),]


t_a$ndays <- difftime(t_a$end.time,t_a$start.time,unit='days')
t_a$ndays <- ifelse(t_a$ndays=='-Inf',1,t_a$ndays)
t_a$ndays <- ifelse(t_a$ndays <= 1,1,t_a$ndays)
t_a$ndays <- ceiling(t_a$ndays)

t_a$zone <- substr(t_a$surveycode,5,5)



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

t_a
}

