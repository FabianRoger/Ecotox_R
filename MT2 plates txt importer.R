
library(plyr)

# set working directory with all txt files to import

#setwd("/Users/fabianroger/Documents/01_PhD/01_Research/03_Ecotox/MT2/MT2 Mid")
setwd("/Users/fabianroger/Documents/01_PhD/01_Research/03_Ecotox/MT2/MT2 End")

# list of all files in wd

FileL<-list.files()

for(i in 1:length(FileL)) {
  
  B<-read.table(FileL[i],sep="\t",header=F)
  
  DatTim<-as.character(B[9,6])
  
  PlateN<-sub("MT2_(.+)2013.+","\\1",FileL[i])
  
  B590<-as.matrix(B[41:48,2:13])
  B590<-as.numeric(gsub(",",".",B590))
  
  B595<-as.matrix(B[63:70,2:13])
  B595<-as.numeric(gsub(",",".",B595))
  
  B700<-as.matrix(B[85:92,2:13])
  B700<-as.numeric(gsub(",",".",B700))
  
  Wells<-paste(rep(LETTERS[1:8],12),rep(formatC(c(1:12),width=2,flag="0"),
                                        each=8),sep="")
  
  Plate.t<-data.frame(PlateN=PlateN,DAT=DatTim,Well=Wells,
                      OD590=B590,OD595=B595,OD700=B700)
  Plate.t<-Plate.t[with(Plate.t,order(Wells)),]
  
  
  if (i == 1) Plate<-Plate.t else Plate<-rbind(Plate,Plate.t)
  
}

MT2<-Plate

#############################
# for the Mid sampling #
#############################

#adding sapling occasion
MT2$sampling<-"Mid"

#extract Plate Name
MT2$Plate<-sub("(.+)_set.+","\\1",MT2$PlateN)

#extract hour information form filename
MT2$hour<-sub(".+(_\\d+h)","\\1",MT2$PlateN)
MT2$hour<-sub("_(\\d+)h","\\1",MT2$hour)

#extract set info
MT2$set<-sub(".+(set\\d).+","\\1",MT2$PlateN)

#exclude filename column
MT2<-MT2[,-1]

#trim the date string to POSIXct compatible format
MT2$DAT<-substr(MT2$DAT,1,19)

MT2Mid<-MT2

#############################
# for the End sampling #
#############################

#adding sapling occasion
MT2$sampling<-"End"

#extract Plate Name
MT2$Plate<-sub("(.+)_\\d+h","\\1",MT2$PlateN)
MT2$Plate<-sub("(.+)_set\\d(.+)","\\1\\2",MT2$Plate)

#extract hour information form filename
MT2$hour<-sub(".+(_\\d+h)","\\1",MT2$PlateN)
MT2$hour<-sub("_(\\d+)h","\\1",MT2$hour)

#extract set info
MT2$set<-sub(".+(set\\d).+","\\1",MT2$PlateN)
MT2$set[MT2$set %in% c("set1","set2")==F]<-sub(
  "(\\w+)(\\d+_\\w+)","\\1", MT2$set[MT2$set %in% c("set1","set2")==F])
MT2$set[MT2$set=="Cipro10"]<-"Cipro"
MT2$set[MT2$set=="Kontroll"]<-"Control"

#exclude filename column
MT2<-MT2[,-1]

#trim the date string to POSIXct compatible format
MT2$DAT<-substr(MT2$DAT,1,19)

MT2End<-MT2

################
#add well information
################
setwd("/Users/fabianroger/Documents/01_PhD/01_Research/03_Ecotox/R")

WI.Mid<-read.csv("WI_MT2Mid.csv",header=T)
WI.End<-read.csv("WI_MT2End.csv",header=T)

#correct error in cipro conc
WI.End[WI.End$set=="Cipro"&WI.End$Comp=="Cipro",]$Conc<-NA
WI.End[WI.End$set=="Cipro"&WI.End$Comp=="Cipro",]$Unit<-NA

WI.Mid[,-3]<-apply(WI.Mid[,-3],2,as.character)
WI.End[,-3]<-apply(WI.End[,-3],2,as.character)

MT2Mid$Well<-as.character(MT2Mid$Well)
MT2End$Well<-as.character(MT2End$Well)

MT2Mid<-join(MT2Mid,WI.Mid)
MT2End<-join(MT2End,WI.End)


##### adding bottle Numbers to Datasets

# MT2 Mid

# Bottle 3 (Div 1 Tox 2) Div1 & Div1 1-10
# Bottle 42 (Div 3 Tox 2) Div3
# Bottle 31 (Div 5 Tox 2) Div5

BOT<-data.frame(Plate=levels(as.factor(MT2Mid$Plate)),BOT=c("03","03","42","31"))
BOT[,1:2]<-apply(BOT[,1:2],2,as.character)

MT2Mid<-join(MT2Mid,BOT)

# MT2 End

#Kontroll1: Div 1 = bottle 20. Div 2 = bottle 2. Div 3 = bottle 4. 
        #Div 4 = bottle 6. Div 5 = bottle 9. 
#Kontroll2: Div 1 = bottle 27. Div 2 = bottle 43. Div 3 = bottle 26. 
        #Div 4 = bottle 8. Div 5 = bottle 11. 
#Kontroll3: Div 1 = bottle 35. Div 2 = bottle 44. Div 3 = bottle 41. 
        #Div 4 = bottle 29. Div 5 = bottle 30. 
#Cipro100: Div 1 = bottle 36. Div 2 = bottle 32. Div 3 = bottle 37. 
        #Div 4 = bottle 45. Div 5 = bottle 13. 

# for the DIV1 and DIV5 the Bottles can be extracted from the Plate Name

BOT<-data.frame(Plate=rep(levels(as.factor(MT2End$Plate))[c(1,20:22)],each=6),
                treatment=rep(levels(as.factor(MT2End$treatment)),4),
                BOT=c(NA,"36","32","37","45","13",NA,"20","02","04",
                      "06","09",NA,"27","43","26","08","11",
                      NA,"35","44","41","29","30"))

BOT[,1:3]<-apply(BOT[,1:3],2,as.character)

BOT2<-data.frame(Plate=levels(as.factor(MT2End[c(agrep("Div", 
      as.character(MT2End$Plate),max.distance=1)),]$Plate)),treatment=NA)

BOT2$BOT<-sub("Div\\d_(\\d+)","\\1",BOT2$Plate)

BOT2[,1:3]<-apply(BOT2[,1:3],2,as.character)

BOT<-rbind(BOT,BOT2)


#adding Bot for control anc cipro100 treatments
MT2End<-join(MT2End,BOT)




write.table(MT2Mid,"MT2Mid.txt",sep="\t")
write.table(MT2End,"MT2End.txt",sep="\t")


