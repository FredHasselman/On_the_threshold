# Fred Hasselman on behalf of "Comments on reducing ‘statistical significance’ to 0.005"

# SETUP local R-------------------------------------------------------------------

# Use this code (from the devtools package) to source C-3PR directly from GitHub:
require(devtools)
source_url('https://raw.githubusercontent.com/FredHasselman/toolboxR/master/C-3PR.R')

# This will load and (if necessary) install libraries frequently used for data management and plotting
in.IT(c('ggplot2','RColorBrewer','rio','ggforce','tidyverse'))

RPPdata <- get.OSFfile(code='https://osf.io/fgjvw/',dfCln=TRUE)$df

## If you dowloaded the csv file to your harddrive use this code:
#  RPPdata<-read.csv('rpp_data.csv',stringsAsFactors=F )
#  RPPdata<-df.Clean(RPPdata)
#  RPPdata<-RPPdata$df

# Select the completed replication studies
RPPdata <- dplyr::filter(RPPdata, !is.na(T.pval.USE.O),!is.na(T.pval.USE.R))
# We have 99 studies for which p-values and effect sizes could be calculated
nrow(RPPdata)
# We have 97 studies for which p-values of the original effect were significantly below .05
idOK <- complete.cases(RPPdata$T.r.O,RPPdata$T.r.R)
sum(idOK)

# Choose a palette from RColorBrewer
mypalette        <- brewer.pal(8,"Set2")
# Get the Replication observed power
RPPdata$Power.Rn <- as.numeric(RPPdata$Power.R)

# Get ggplot2 themes predefined in C-3PR
mytheme <- gg.theme("clean")


# CREATE A P-VALUE PLOT -------------------------------------------------------------------------------------------
# Might be of use
RPPdata$T.pval.USE.R.f <- "p >= .05"
RPPdata$T.pval.USE.R.f[RPPdata$T.pval.USE.R<.05]  <- "p < .05"
RPPdata$T.pval.USE.R.f[RPPdata$T.pval.USE.R<.005] <- "p < .005"
RPPdata$T.pval.USE.R.f <- factor(RPPdata$T.pval.USE.R.f)

RPPdata$T.pval.USE.O.f <- "p >= .05"
RPPdata$T.pval.USE.O.f[RPPdata$T.pval.USE.O<.05]  <- "p < .05"
RPPdata$T.pval.USE.O.f[RPPdata$T.pval.USE.O<.005] <- "p < .005"
RPPdata$T.pval.USE.O.f <- factor(RPPdata$T.pval.USE.O.f)

library(ggplot2)
library(ggforce)
library(rio)

idO.05 <- (RPPdata$T.pval.USE.O<.05)&!is.na(RPPdata$T.pval.USE.O)
idR.05 <- RPPdata$T.pval.USE.R<.05&!is.na(RPPdata$T.pval.USE.R)
idO.005 <- RPPdata$T.pval.USE.O<.005&!is.na(RPPdata$T.pval.USE.O)
idR.005 <- RPPdata$T.pval.USE.R<.005&!is.na(RPPdata$T.pval.USE.R)
idO.0005 <- RPPdata$T.pval.USE.O<.0005&!is.na(RPPdata$T.pval.USE.O)


lm.05 <- lm(T.pval.USE.R ~ T.pval.USE.O,data=RPPdata, subset=idO.05)
summary(lm.05)

lm.005 <- lm(T.pval.USE.R ~ T.pval.USE.O, data=RPPdata, subset=idO.005)
summary(lm.005)

RPPdata$pred05 <- NA
RPPdata$pred05[idO.05] <- predict(lm.05)

RPPdata$pred005 <- NA
RPPdata$pred005[idO.005] <- predict(lm.005)

cor(RPPdata$T.pval.USE.O[idO.05&idR.05],RPPdata$T.pval.USE.R[idR.05&idO.05])
cor(RPPdata$T.pval.USE.O[idO.005&idR.005],RPPdata$T.pval.USE.R[idR.005&idO.005])

cor(RPPdata$T.pval.USE.O[idO.05],RPPdata$T.pval.USE.R[idO.05])
cor(RPPdata$T.pval.USE.O[idO.005],RPPdata$T.pval.USE.R[idO.005])
cor(log1p(RPPdata$T.pval.USE.O[idO.0005]),log1p(RPPdata$T.pval.USE.R[idO.0005]))

library(plyr)

dfRPP <- RPPdata[idO.05,]
dfRPP$isSmaller005 <- NA
#dfRPP$isSmaller005[dfRPP$T.pval.USE.O<.05] <- "p < .05 "
dfRPP$isSmaller005[dfRPP$T.pval.USE.O<.005] <- "p < .005"
dfRPP$isSmaller005 <- factor(dfRPP$isSmaller005)
dfRPP$isSmaller05 <- NA
dfRPP$isSmaller05[dfRPP$T.pval.USE.O<.05] <- "p < .05 "
dfRPP$isSmaller05.num[dfRPP$T.pval.USE.O<.05] <- dfRPP$T.pval.USE.O[dfRPP$T.pval.USE.O<.05]

#dfRPP$isSmaller05[dfRPP$T.pval.USE.O<.05] <- 1
zoomplot <- ggplot(dfRPP,
                   aes(y=T.pval.USE.R,
                       x=T.pval.USE.O,
                       group = isSmaller05)) +
  geom_point(aes(size=Power.Rn,color=Journal.O),alpha=.8) +
  ggtitle("") + xlab("Original Study p-value") + ylab("Replication p-value") +
  #geom_hline(aes(yintercept=0.05),linetype=2) +
  #geom_line(aes(y=pred05,group=factor(T.pval.USE.O<.05))) +
  scale_color_brewer(name="Journal",palette="Set2") +
  scale_size_continuous(name="Replication:\nPower",breaks=seq(0,1,length=11))  + ylim(c(0,1)) +
  scale_linetype_manual(name="Level of\n significance", values = c(1,3)) +
  facet_zoom(xlim = c(0,.005),
             #zoom.data = T.pval.USE.O < .001,
             #x =  isSmaller005 == "p < .005",
             #shrink = TRUE
             ) +
  geom_line(aes(y=pred005,group=isSmaller005),size=1) +
  geom_line(aes(y=pred05,group=isSmaller05), size=1) +
  #geom_line(data = dfRPP[dfRPP$T.pval.USE.O<.005,],aes(y=pred005,group=isSmaller005,linetype=isSmaller005),size=1) +
  #geom_line(data = dfRPP[dfRPP$T.pval.USE.O<.05,],aes(y=pred05,group=isSmaller05,linetype=isSmaller05),size=1) +
  #stat_smooth(data = dfRPP[dfRPP$isSmaller005==1,],aes(group=isSmaller005),method = "lm", colour="black", fill= mypalette[3], alpha=.4, fullrange = FALSE) +
  #geom_smooth(aes(group=isSmaller005),method = "lm", colour="black", fill= mypalette[3], alpha=.4, fullrange = FALSE) +
    mytheme

zoomplot + stat_smooth(method = "lm", colour="black", fill= mypalette[3], alpha=.4, fullrange = TRUE)

## Uncomment to save panel A as a seperate file
 ggsave("pvalue_plot_RPP.pdf",plot=zoomplot)

 RPPdata$T.pval.USE.O.log <- log1p( RPPdata$T.pval.USE.O)
 RPPdata$T.pval.USE.R.log <- log1p( RPPdata$T.pval.USE.R)

 zoomplot2 <- ggplot(RPPdata[idO.05,],
                     aes(y=T.pval.USE.R.log,x=T.pval.USE.O.log))+
   geom_point(aes(size=Power.Rn, color=Journal.O),alpha=.8) +
   stat_smooth(method = "lm", colour="black", fill= mypalette[3], alpha=.4, fullrange = TRUE)+
   ggtitle("") + xlab("Original Study p-value") + ylab("Replication p-value") +
   scale_color_brewer(name="Journal",palette="Set2") +
   scale_size_continuous(name="Replication:\nPower",breaks=seq(0,1,length=11))  + ylim(c(0,1)) +
   facet_zoom(xlim = c(log1p(0),log1p(0.005)),
              horizontal = FALSE,
              zoom.size = 1) +
   mytheme

 ## Uncomment to save panel A as a seperate file
 ggsave("pvalue_plot_RPP_log.eps",plot=zoomplot2)


