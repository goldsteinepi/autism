#################
# Autism networks
# Citation: Goldstein ND, Tager-Flusberg H, Lee BK. Mapping Collaboration Networks in the World of Autism Research. Autism Res. 2015 Feb;8(1):1-8.
# 12/15/12 -- Neal Goldstein
#################


#load data
load("map.RData")

#load sqldf library
library("sqldf")

#get all abstracts
tmpAbstracts <- sqldf("SELECT DISTINCT Key_Paperid FROM abstractAuthorAffil WHERE Year='2008'")

latLonListExt <- NULL  #external collaborators
latLonListPair <- NULL #pairs of collaborators
latLonListInt <- NULL  #internal collaborators
latLonListAgg <- NULL  #overall collaborators

#count collaborations per abstract
for (i in 1:length(tmpAbstracts[,1]))
{
  cat("ABSTRACT:",i,"\n\n")
  
  #get current abstract
  currentAbstract <- sqldf(sprintf("SELECT Key_Paperid, AuthorName, Affiliation, Latitude, Longitude FROM abstractAuthorAffil WHERE Year='2008' AND Key_Paperid=%s",tmpAbstracts[i,]))
  
  #STEP1: External Collab
  
  #get unique lat/lon to count, ie external collaborators
  mapLatLonExt <- sqldf("SELECT Latitude, Longitude FROM currentAbstract GROUP BY Latitude, Longitude")
  
  #if more than one location then add as an external collaborator and overall collaborator
  if (length(mapLatLonExt[,1]) > 1)
  {
    for (j in 1:length(mapLatLonExt[,1]))
    {
      latLonListExt = rbind(latLonListExt,toString(c(mapLatLonExt[j,1],mapLatLonExt[j,2])))
      latLonListAgg = rbind(latLonListAgg,toString(c(mapLatLonExt[j,1],mapLatLonExt[j,2])))
    }
  } else
    mapLatLonExt <- mapLatLonExt[-1,]
  
  #STEP2: Internal Collab
  
  #get duplicate lat/lon, ie internal collaborators
  mapLatLonInt <- sqldf("SELECT DISTINCT T2.Latitude, T2.Longitude FROM (SELECT Latitude, Longitude FROM currentAbstract GROUP BY Latitude, Longitude HAVING COUNT(*)>=2) T1 JOIN currentAbstract T2 ON T1.Latitude=T2.Latitude AND T1.Longitude=T2.Longitude")
  
  #count each internal collaborator
  if (length(mapLatLonInt[,1]) > 0)
  {
    for (j in 1:length(mapLatLonInt[,1]))
    {
      latLonListInt = rbind(latLonListInt,toString(c(mapLatLonInt[j,1],mapLatLonInt[j,2])))
      
      #add to aggregate count provided haven't already counted this abstract as an external for location
      #ie don't want to count this abstract again towards collaboration if accounted for in external
      #the only way this should happen is if there were no external collabs, ie all from same location
      if (length(mapLatLonExt[,1])==0)
        latLonListAgg = rbind(latLonListAgg,toString(c(mapLatLonInt[j,1],mapLatLonInt[j,2])))
    }
  }

  #STEP3: Pairwise Collab
  
  #fully connect all unique locations to track top pairs of collaborators
  while (length(mapLatLonExt[,1]) > 1)
  {
    #first location
    latFirst <- mapLatLonExt[1,1]
    lonFirst <- mapLatLonExt[1,2]
    
    #connect first location to all other locations
    j <- 2
    while (j <= length(mapLatLonExt[,1]))
    {
      latJ <- mapLatLonExt[j,1]
      lonJ <- mapLatLonExt[j,2]

      latLonListPair <- rbind(latLonListPair,toString(c(latFirst,lonFirst,latJ,lonJ)))
      
      j <- j+1
    }
    
    #remove first location
    mapLatLonExt <- mapLatLonExt[-1,]
  }
}

#count and order by increasing frequency the external collabs
countCrossCollab <- as.data.frame(table(latLonListExt))
countCrossCollab <- countCrossCollab[with(countCrossCollab, order(Freq)), ]

#count and order by increasing frequency the pairwise collabs
countPairCollab <- as.data.frame(table(latLonListPair))
countPairCollab <- countPairCollab[with(countPairCollab, order(Freq)), ]

#count and order by increasing frequency the internal collabs
countSelfCollab <- as.data.frame(table(latLonListInt))
countSelfCollab <- countSelfCollab[with(countSelfCollab, order(Freq)), ]

#count and order by increasing frequency the overall collabs
countOverallCollab <- as.data.frame(table(latLonListAgg))
countOverallCollab <- countOverallCollab[with(countOverallCollab, order(Freq)), ]

#count and order by increasing frequency overall abstracts per institution
countAbstracts <- as.data.frame(table(sqldf("SELECT Institution FROM aggregateInstitution WHERE Year='2008'")))
countAbstracts <- countAbstracts[with(countAbstracts, order(Freq)), ]

#clean up
rm(i,j,latFirst,lonFirst,latJ,lonJ,latLonListExt,latLonListPair,latLonListInt,latLonListAgg,tmpAbstracts,currentAbstract,mapLatLonExt,mapLatLonInt)
detach("package:sqldf")

#load maps, geosphere library
library("maps")
library("geosphere")
png("map2008_raw.png",width=200,height=100,units="mm",res=600)
par(oma=c(0, 0, 0, 0))
par(mar=c(0, 0, 0, 0), xaxs='i', yaxs='i')
par(plt=c(0, 1, 0, 1))

#world map, antarctica trimmed
map("world", col="#333333", fill=TRUE, bg="#191919", lwd=0.05, ylim=c(-60,90))
#north america map
#map("world", col="#333333", fill=TRUE, bg="#191919", lwd=0.05, xlim=c(-171.738281, -56.601563), ylim=c(12.039321, 71.856229))
#europe map
#map("world", col="#333333", fill=TRUE, bg="#191919", lwd=0.05, xlim=c(-25,70), ylim=c(35,71))
#western us map
#map("state", col="#333333", fill=TRUE, bg="#191919", lwd=0.05, xlim=c(-125,-93))
#eastern us map
#map("state", col="#333333", fill=TRUE, bg="#191919", lwd=0.05, xlim=c(-95,-67))

#plot external collaborators
while (length(countPairCollab[,1]) > 0)
{
  #only plot successful geocodes
  if (length(grep("NA",countPairCollab[1,1]))==0)
  {
    #get first location and weight
    latFirst <- as.numeric(unlist(strsplit(paste(countPairCollab[1,1]), ", "))[[1]])
    lonFirst <- as.numeric(unlist(strsplit(paste(countPairCollab[1,1]), ", "))[[2]])
    latJ <- as.numeric(unlist(strsplit(paste(countPairCollab[1,1]), ", "))[[3]])
    lonJ <- as.numeric(unlist(strsplit(paste(countPairCollab[1,1]), ", "))[[4]])
    weightLatLon <- as.numeric(countPairCollab[1,2])
    
    connectLine <- gcIntermediate(c(lonFirst, latFirst), c(lonJ, latJ), n=50, addStartEnd=TRUE, breakAtDateLine=TRUE)
    
    #assign color and size based on weight
    if (weightLatLon==1) {
      
      #determine if line crosses intl date line
      if (length(connectLine[[1]])>1) {
        lines(connectLine[[1]],col="#666666",lwd=0.1)
        lines(connectLine[[2]],col="#666666",lwd=0.1)
      } else lines(connectLine,col="#666666",lwd=0.1)
      
    } else if (weightLatLon==2) {
      
      #determine if line crosses intl date line
      if (length(connectLine[[1]])>1) {
        lines(connectLine[[1]],col="#FFFFFF",lwd=0.1)
        lines(connectLine[[2]],col="#FFFFFF",lwd=0.1)
      } else lines(connectLine,col="#FFFFFF",lwd=0.1)
      
    } else if (weightLatLon>2) {
      
      #determine if line crosses intl date line
      if (length(connectLine[[1]])>1) {
        lines(connectLine[[1]],col="#1292DB",lwd=0.1)
        lines(connectLine[[2]],col="#1292DB",lwd=0.1)
      } else lines(connectLine,col="#1292DB",lwd=0.1)
      
    }
  }
  
  #remove first location
  countPairCollab <- countPairCollab[-1,]
}

#plot overall abstracts
while (length(countAbstracts[,1]) > 0)
{
  #only plot successful geocodes
  if (length(grep("NA",countAbstracts[1,1]))==0)
  {
    #get first location and weight
    latFirst <- as.numeric(unlist(strsplit(paste(countAbstracts[1,1]), ", "))[[1]])
    lonFirst <- as.numeric(unlist(strsplit(paste(countAbstracts[1,1]), ", "))[[2]])
    weightLatLon <- as.numeric(countAbstracts[1,2])
    
    #assign color and size based on weight
    if (weightLatLon==1) {
      points(lonFirst,latFirst,pch=21,col="#000000",bg="#00FF00",lwd=0.1,cex=0.1)
    } else if ((weightLatLon>1) & (weightLatLon<10)) {
      points(lonFirst,latFirst,pch=21,col="#000000",bg="#FFFF00",lwd=0.1,cex=0.15)
    } else if (weightLatLon>=10) {
      points(lonFirst,latFirst,pch=21,col="#000000",bg="#FF0000",lwd=0.1,cex=0.25)
    }
  }
  
  #remove first location
  countAbstracts <- countAbstracts[-1,]
}

# #plot grant money
# library("XLConnect")
# grantMoney <- readWorksheet(loadWorkbook("NIH_Grant_Dollars.xlsx"), 1)
# detach("package:XLConnect")
# 
# while (length(grantMoney[,1]) > 0)
# {
#   #get first location and weight
#   latFirst <- grantMoney[1,4]
#   lonFirst <- grantMoney[1,5]
#   weightLatLon <- grantMoney[1,1]
#   
#   #assign color and size based on weight
#   if (weightLatLon<1000000) {
#     points(lonFirst,latFirst,pch=36,col="#006600",bg="#000000",lwd=0.1,cex=0.1)
#   } else if ((weightLatLon>=1000000) & (weightLatLon<10000000)) {
#     points(lonFirst,latFirst,pch=36,col="#339933",bg="#000000",lwd=0.1,cex=0.15)
#   } else if (weightLatLon>=10000000) {
#     points(lonFirst,latFirst,pch=36,col="#00CC33",bg="#000000",lwd=0.1,cex=0.25)
#   }
#   
#   #remove first location
#   grantMoney <- grantMoney[-1,]
# }

#clean up
dev.off()
rm(latFirst,lonFirst,latJ,lonJ,weightLatLon,connectLine,countCrossCollab,countSelfCollab,countOverallCollab,countPairCollab,countAbstracts,worldMapEnv,stateMapEnv,grantMoney)
detach("package:maps")
detach("package:geosphere")