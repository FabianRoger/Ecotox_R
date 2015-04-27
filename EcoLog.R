
# read in data 
EcoM <- read.table("EcoMid.txt",header=T,sep="\t")
EcoE <- read.table("EcoEnd.txt",header=T,sep="\t")
BOT_ID <- read.table("BOT_ID.txt",header=T,sep="\t",stringsAsFactors=F)

# add Time column (mid/end) to each respective dataframe and join them
EcoM$Timepoint<-"mid"
EcoE$Timepoint<-"end"

# join dataframes
Eco<-rbind(EcoM,EcoE)

# transform $DAT to POSIXct
Eco$DAT <-  dmy_hm(Eco$DAT)



####### first very rough statistics to eyeball the data ######

# take median OD595 for each Well on each date (replicates are read silmoutaniously so I split by DAT)
medEco<-ddply(Eco, .(BOT,Timepoint,Wells,DAT), summarize, mOD595=median(OD595))

# extract maximum OD595 for each Plate and each Well
maxEco <- ddply(medEco, .(BOT,Timepoint,Wells), summarize, maxOD=max(mOD595))

# count number of carbon surces with OD > 0.2 (excluding the blank well)
NEco <- ddply(maxEco[maxEco$Well != "A1",], .(BOT,Timepoint), summarize, N = length(which(maxOD>=0.2)))


# join metadata
NEco<-join(NEco,BOT_ID)

# change factor level for timepoint
NEco$Timepoint<-factor(NEco$Timepoint,levels=c("mid","end"))


# some plots

# exclude sterile controls
NEcoNS<-NEco[-c(grep("S", NEco$DIV)), ]

ggplot(NEcoNS, aes(x=Timepoint,y=N,fill=Timepoint))+
  geom_bar(stat="identity")+
  facet_wrap(~DIV*ToxC,nrow=5,ncol=7)

ggplot(NEcoNS, aes(x=ToxC, y=N))+
  geom_point()+
  scale_x_log10()+
  facet_wrap(~DIV*Timepoint,nrow=5)

ggplot(NEcoNS[NEcoNS$Timepoint == "end",], aes(x=ToxC+1E-13, y=N, colour=DIV))+
  geom_line(,linetype="dotted")+
  stat_smooth(method="lm",se=F)+
  scale_x_log10()+
  theme_bw()


ggplot(NEcoNS[NEcoNS$Timepoint == "mid",], aes(x=DIV, y=N))+
  geom_point()
  

# calculate relative loss of N carbon sources per bottle

EcoMt<-NEcoNS[NEcoNS$Timepoint=="mid",]
EcoEt<-NEcoNS[NEcoNS$Timepoint=="end",]

EcoMt$N.E<-EcoEt$N

EcoMt$delta<-EcoMt$N.E-EcoMt$N
EcoMt$prct<-EcoMt$N.E/(EcoMt$N/100)

ggplot(EcoMt, aes(x=DIV,y=prct))+
  geom_point()+
  facet_wrap(~Tox)

ggplot(EcoMt, aes(x=DIV,y=delta))+
  geom_point()+
  facet_wrap(~Tox)

######### check average well colour developpment for dose response curve ########

# extract 72 hours timepoint

# function to calculate incubation timepoint of plates
delta.T<-function(x)  {x$dT <- as.numeric(round((x$DAT-(min(x$DAT)-hms(paste(min(x$hour),"00:00",sep=":"))))*24))
                       return(x)}

# function to subset for timelslot t1 < x < t2
Func <- function(x) {return(x[x$dT %in% c(71,72), ])}

# add deltaT column to Eco
Eco<-do.call(rbind.data.frame, (by(Eco, Eco[,c(6,17)], delta.T)))

#extract reading at ~ 72 hours
Eco72<-do.call(rbind.data.frame, (by(Eco, Eco[,c(6,17)], Func)))

# calculate AWCD for whole plate
AWCD72 <- ddply(Eco72, .(BOT, DIV,Tox,ToxC,Timepoint), summarize, AWCD = mean (OD590))

# plot AWCD vs DIV*TOX

AWCD72<-AWCD72[-c(grep("S",AWCD72$DIV)),]

ggplot(AWCD72[AWCD72$Timepoint == "end",], aes(x=ToxC+1e-12, y=AWCD))+
  geom_point()+
  scale_x_log10()+
  facet_wrap(~DIV)
  
# calculate Colour for each compound on each plate at 72h
  
CC <- ddply(Eco72, .(BOT, DIV,Tox,ToxC,Timepoint, C.Source), summarize, AWCD = median (OD590))

CC<-CC[-c(grep("S",CC$DIV)),]
  
ggplot(CC[CC$Timepoint == "end",], aes(x=ToxC+1e-11, y=AWCD,colour=DIV))+
  geom_point()+
  stat_smooth(se=F)+
  scale_x_log10()+
  facet_wrap(~C.Source)
  







