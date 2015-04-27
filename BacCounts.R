
##### flow cytometer counts of Bacteria, counted every second day ######

####### column legend: ######

##  sampling  : sampling date (dd/mm/yy)
##  CellsL    : number of "alive" Cells per ml (Propidium Iodide negative)
##  CellsD  	: number of "dead" Cells per ml (Propidium Iodide negative)
##  CellsT		: sum of CellsL and CellsD
##  pDead		  : percentage of "dead" Cells. OBS: the percentage can be very high if the total count is very low! That's an artefact!  
##  DIV		    : nominal Diversity level. 1=undiluted, 5 = highest dilution 
##  Tox		    : nominal Toxicity level. 1=highest tox level, 6=lowest. 0 = no Cipro added. OBS: Cipro was only added after 2013-10-10!
##  BOT		    : Botle number
##  ToxC		  : intendet Ciprofloxacin concentration in mol/L. OBS not measuresd concetrations!

###############################

# read in data
Bac <- read.table("BacCounts.txt" , header = T , sep = "\t")

# transform $sampling into POSIXct date
Bac$sampling <- dmy(Bac$sampling)

# add with / without tox level
Bac$ToxYN <- NA
Bac[Bac$sampling < ymd(20131010),]$ToxYN <-"NO"
Bac[Bac$sampling >= ymd(20131010),]$ToxYN <-"YES"

# plot Cell counts by diversity and Tox

ggplot(Bac[Bac$DIV != "S",], aes(y=CellsT, x=sampling,colour=ToxYN))+
  facet_wrap(~DIV*Tox,ncol=7)+
  stat_summary(fun.y = mean, geom = "line",linetype="solid")+
  #stat_summary(aes(y=CellsL),fun.y = mean, geom = "line",linetype="dashed")+
  #stat_summary(aes(y=CellsD),fun.y = mean, geom = "line",linetype="dotted")+
  theme_bw()+
  scale_colour_manual(values=c("darkgreen","darkred"))


# calculate max diversity for both with and without toxin as 
# mean of two highest values

maxBac<-ddply(Bac[,c("CellsT","DIV","Tox","BOT","ToxC","ToxYN")],
              .(DIV,Tox,BOT,ToxYN,ToxC
                ), function(x) mean(x[with(x, order(-CellsT)),][1:2,]$Cells))

ggplot(maxBac[maxBac$DIV != "S",], aes(y=V1, x=DIV,colour=ToxYN,group=ToxYN))+
  facet_wrap(~ToxYN*Tox,nrow=2)+
  stat_summary(fun.y = mean, geom = "point")+
  stat_smooth(method="lm")+
  labs(y="maximum Biomass", x="Diversity level")+
  theme_bw(base_size=15)+
  scale_colour_manual(values=c("darkgreen","darkred"))

ggplot(maxBac[maxBac$DIV != "S" & maxBac$ToxYN == "YES",], aes(y=V1, x=Tox,group=DIV))+
  facet_wrap(~DIV,nrow=1)+
  stat_summary(fun.y = mean, geom = "point")+
  stat_smooth(method="lm")+
  labs(y="maximum Biomass", x="Toxicity level")+
  theme_bw(base_size=15)+
  scale_colour_manual(values=c("darkgreen","darkred"))





