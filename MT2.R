MT2<-read.table("MT2Mid.txt",header=T,sep="\t")

# transform $DAT to POSIXct
MT2$DAT <-  dmy_hm(MT2$DAT)

# substract median blanks and control from all plates

subC <- function(x) { y <- x[x$Comp != "Blank",]
                      y$OD590 <- y$OD590 - median(x[x$Comp == "Blank",]$OD590)
                      z <- y[y$Comp != "Cont",]
                      z$OD590 <- z$OD590 -  median(y[y$Comp == "Cont",]$OD590)
                      return(z) }

MT2b <- do.call(rbind.data.frame, (by(MT2, MT2[,c("Plate","DAT","set")], subC)))

# function to clalculate reading hours
delta.T<-function(x)  {x$dT <- as.numeric(round((x$DAT-(min(x$DAT)))/3600))
                       return(x)}

MT2b <- do.call(rbind.data.frame, (by(MT2b, MT2b[,c("Plate")], delta.T)))

# function to subset for timepoint closets to 60h

Cl60 <- function(x) { H <- which(abs(x$dT-60) == min(abs(x$dT-60))) 
                      return(x[H,]) }


MT2b60 <- do.call(rbind.data.frame, (by(MT2b, MT2b[,c("Plate")], Cl60)))

ggplot(MT2b60[MT2b60$DIV %in% c(1,5),], aes(x=Conc+1e-11, y=OD590))+
  geom_point()+
  facet_wrap(~Plate*Comp,ncol=5,nrow=14)+
  scale_x_log10()+
  stat_smooth()





