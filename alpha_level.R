# Fred Hasselman on behalf of "Comments on reducing ‘statistical significance’ to 0.005"
# feel free to edit

# SETUP local R-------------------------------------------------------------------

# Use this code (from the devtools package) to source C-3PR directly from GitHub:
require(devtools)
source_url('https://raw.githubusercontent.com/FredHasselman/toolboxR/master/C-3PR.R')

library(plyr)
library(tidyverse)
library(ggplot2)
library(ggforce)
library(gridExtra)
library(RColorBrewer)
library(scales)
library(rio)


# Get the RPP data ------------------------------------------------------------------------------------------------
RPPdata <- get.OSFfile(code='https://osf.io/fgjvw/',dfCln=TRUE)$df

## If you dowloaded the csv file to your harddrive from trhe OSF repo use this code:
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

# A factor might be of use for plotting

RPPdata$T.pval.USE.R.f <- "p >= .05"
RPPdata$T.pval.USE.R.f[RPPdata$T.pval.USE.R<.05]  <- "p < .05"
RPPdata$T.pval.USE.R.f[RPPdata$T.pval.USE.R<.005] <- "p < .005"
RPPdata$T.pval.USE.R.f <- factor(RPPdata$T.pval.USE.R.f)

RPPdata$T.pval.USE.O.f <- "p >= .05"
RPPdata$T.pval.USE.O.f[RPPdata$T.pval.USE.O<.05]  <- "p < .05"
RPPdata$T.pval.USE.O.f[RPPdata$T.pval.USE.O<.005] <- "p < .005"
RPPdata$T.pval.USE.O.f <- factor(RPPdata$T.pval.USE.O.f)


# Look correlations / regression ----------------------------------------------------------------------------------
idO.05   <- (RPPdata$T.pval.USE.O <.05)  &!is.na(RPPdata$T.pval.USE.O)
idR.05   <- (RPPdata$T.pval.USE.R <.05)  &!is.na(RPPdata$T.pval.USE.R)
idO.005  <- (RPPdata$T.pval.USE.O <.005) &!is.na(RPPdata$T.pval.USE.O)
idR.005  <- (RPPdata$T.pval.USE.R <.005) &!is.na(RPPdata$T.pval.USE.R)
idO.0005  <- (RPPdata$T.pval.USE.O <.0005) &!is.na(RPPdata$T.pval.USE.O)
idR.0005  <- (RPPdata$T.pval.USE.R <.0005) &!is.na(RPPdata$T.pval.USE.R)

# Simple linear models
lm.05 <- lm(T.pval.USE.R ~ T.pval.USE.O,data=RPPdata, subset=idO.05)
summary(lm.05)

lm.005 <- lm(T.pval.USE.R ~ T.pval.USE.O, data=RPPdata, subset=idO.005)
summary(lm.005)

# Better on a log scale?
RPPdata$T.pval.USE.O.log <- log10(RPPdata$T.pval.USE.O)
RPPdata$T.pval.USE.R.log <- log10(RPPdata$T.pval.USE.R)

# RPPdata$T.pval.USE.O.log[RPPdata$T.pval.USE.O.log==-Inf] <- 0
# RPPdata$T.pval.USE.R.log[RPPdata$T.pval.USE.R.log==-Inf] <- 0


# Simple linear log models - no intercept
lm.05 <- lm(T.pval.USE.R.log ~ 0+T.pval.USE.O.log,data=RPPdata, subset=idO.05)
summary(lm.05)

lm.005 <- lm(T.pval.USE.R.log ~ 0+T.pval.USE.O.log, data=RPPdata, subset=idO.005)
summary(lm.005)

lm.0005 <- lm(T.pval.USE.R.log ~ 0+T.pval.USE.O.log, data=RPPdata, subset=idO.0005)
summary(lm.0005)

# Result: Same coefficients!!!
# p<.05   = 0.8334
# p<.005  = 0.8350
# p<.0005 = 0.8502



#----------------------------------------------------------------------------
# This is a bit weird, but we need it to make the RPP zoomplot
 df.rpp <- cbind.data.frame(ori.p.value.log10 = c(RPPdata$T.pval.USE.O.log[idO.05],RPPdata$T.pval.USE.O.log[idO.005]),
                            rep.p.value.log10 = c(RPPdata$T.pval.USE.R.log[idO.05],RPPdata$T.pval.USE.R.log[idO.005]),
                            ori.p.value = c(RPPdata$T.pval.USE.O[idO.05],RPPdata$T.pval.USE.O[idO.005]),
                            rep.p.value = c(RPPdata$T.pval.USE.R[idO.05],RPPdata$T.pval.USE.R[idO.005]),
                            ori.effect.size = c(RPPdata$T.r.O[idO.05],RPPdata$T.r.O[idO.005]),
                            rep.effect.size = c(RPPdata$T.r.O[idO.05],RPPdata$T.r.O[idO.005]),
                            journal       = c(RPPdata$Journal.O[idO.05],RPPdata$Journal.O[idO.005]),
                            rep.power     = c(RPPdata$Power.Rn[idO.05],RPPdata$Power.Rn[idO.005]),
                            alpha.level   = factor(c(rep("p < .05", NROW(RPPdata$T.pval.USE.O[idO.05])),
                                              rep("p < .005",NROW(RPPdata$T.pval.USE.O[idO.005])))))

# Simple facet plot
ggplot(df.rpp,aes(x=ori.p.value,y=rep.p.value,group=alpha.level)) +
  geom_point()+
  stat_smooth(method = "lm") +
  facet_grid(.~alpha.level, scales = "free_x") +
  mytheme

# Simple facet plot - log
ggplot(df.rpp,aes(x=ori.p.value.log10,y=rep.p.value.log10,group=alpha.level)) +
  geom_point()+
  stat_smooth(method = "lm") +
  facet_grid(.~alpha.level) +
  mytheme

# Zoomplot  like RPP
zoomplot <- ggplot(df.rpp,aes(x=ori.p.value,y=rep.p.value,group=alpha.level)) +
  geom_point(aes(size= rep.power, colour= journal), alpha=.6) +
  stat_smooth(aes(fill = alpha.level), method = "lm", colour = "steelblue", alpha=.2) +
  ggtitle("") + xlab("Original Study p-value") + ylab("Replication p-value") +
  scale_colour_manual(name="Journal",values = mypalette) +
  scale_fill_manual(name="Original:\nAlpha level", values = mypalette) +
  scale_size_continuous(name="Replication:\nPower",breaks=seq(0.40,1,length=7)) +
  facet_zoom(zoom.data = alpha.level == "p < .005",
             x =  alpha.level == "p < .005",
             xlim = c(0,0.005)) +
  mytheme

# Over the top
zoomplot2 <- ggplot(df.rpp,aes(x=ori.p.value,y=rep.p.value,group=alpha.level)) +
  geom_point(aes(size=ori.effect.size, colour = rep.effect.size),alpha=.6) +
  stat_smooth(aes(fill = alpha.level), method = "lm", colour = "steelblue", alpha=.2) +
  ggtitle("") + xlab("Original Study p-value") + ylab("Replication Study p-value") +
  scale_fill_manual(name="Original:\nAlpha level",values =  c("steelblue","violetred4")) +
  scale_colour_gradient2(name="Original:\nEffect Size",breaks=seq(0,1,length=5),
                         midpoint=0.5, low = "steelblue",mid = "whitesmoke", high = "violetred4") +
  scale_size_continuous(name="Replication:\nEffect Size",breaks=seq(1,0,length=5)) +
  facet_zoom(zoom.data = alpha.level == "p < .005",
             x =  alpha.level == "p < .005",
             xlim = c(0,0.005)) +
  mytheme


#----------------------------------------------------------------------------
# Now add another level: .0005
df.rpp <- cbind.data.frame(ori.p.value.log10 = c(RPPdata$T.pval.USE.O.log[idO.05],RPPdata$T.pval.USE.O.log[idO.005],RPPdata$T.pval.USE.O.log[idO.0005]),
                           rep.p.value.log10 = c(RPPdata$T.pval.USE.R.log[idO.05],RPPdata$T.pval.USE.R.log[idO.005],RPPdata$T.pval.USE.R.log[idO.0005]),
                           ori.p.value = c(RPPdata$T.pval.USE.O[idO.05],RPPdata$T.pval.USE.O[idO.005],RPPdata$T.pval.USE.O[idO.0005]),
                           rep.p.value = c(RPPdata$T.pval.USE.R[idO.05],RPPdata$T.pval.USE.R[idO.005],RPPdata$T.pval.USE.R[idO.0005]),
                           ori.effect.size = c(RPPdata$T.r.O[idO.05],RPPdata$T.r.O[idO.005],RPPdata$T.r.O[idO.0005]),
                           rep.effect.size = c(RPPdata$T.r.O[idO.05],RPPdata$T.r.O[idO.005],RPPdata$T.r.O[idO.0005]),
                           journal       = c(RPPdata$Journal.O[idO.05],RPPdata$Journal.O[idO.005],RPPdata$Journal.O[idO.0005]),
                           rep.power     = c(RPPdata$Power.Rn[idO.05],RPPdata$Power.Rn[idO.005],RPPdata$Power.Rn[idO.0005]),
                           alpha.level   = factor(c(rep("p < .05", NROW(RPPdata$T.pval.USE.O[idO.05])),
                                                    rep("p < .005",NROW(RPPdata$T.pval.USE.O[idO.005])),
                                                    rep("p < .0005",NROW(RPPdata$T.pval.USE.O[idO.0005]))))
                           )

# Simple facet plot
ggplot(df.rpp2,aes(x=ori.p.value,y=rep.p.value,group=alpha.level)) +
  geom_point()+
  stat_smooth(method = "lm") +
  facet_grid(.~alpha.level, scales = "free_x") +
  mytheme

# Simple facet plot - log
ggplot(df.rpp2,aes(x=ori.p.value.log10,y=rep.p.value.log10,group=alpha.level)) +
  geom_point()+
  stat_smooth(method = "lm") +
  facet_grid(.~alpha.level) +
  mytheme


# Zoomplot  like RPP
zoomplot3 <- ggplot(df.rpp2,aes(x=ori.p.value,y=rep.p.value,group=alpha.level)) +
  geom_point(aes(size= rep.power, colour= journal), alpha=.6) +
  stat_smooth(aes(fill = alpha.level), method = "lm", colour = "steelblue", alpha=.2) +
  ggtitle("") + xlab("Original Study p-value") + ylab("Replication p-value") +
  scale_colour_manual(name="Journal",values = mypalette) +
  scale_fill_manual(name="Original:\nAlpha level", values = mypalette) +
  scale_size_continuous(name="Replication:\nPower",breaks=seq(0.40,1,length=7)) +
  facet_zoom(zoom.data = alpha.level == "p < .0005",
             x =  alpha.level == "p < .0005",
             xlim = c(0,0.0005)) +
  mytheme


# Over the top
zoomplot4 <- ggplot(df.rpp2,aes(x=ori.p.value,y=rep.p.value,group=alpha.level)) +
  geom_point(aes(size=ori.effect.size, colour = rep.effect.size),alpha=.6) +
  stat_smooth(aes(fill = alpha.level), method = "lm", colour = "steelblue", alpha=.2) +
  ggtitle("") + xlab("Original Study p-value") + ylab("Replication Study p-value") +
  scale_fill_manual(name="Original:\nAlpha level",values =  c("mistyrose4","steelblue","violetred4")) +
  scale_colour_gradient2(name="Original:\nEffect Size",breaks=seq(0,1,length=5),
                         midpoint=0.5, low = "steelblue",mid = "whitesmoke", high = "violetred4") +
  scale_size_continuous(name="Replication:\nEffect Size",breaks=seq(1,0,length=5)) +
  facet_zoom(zoom.data = alpha.level == "p < .0005",
             x =  alpha.level == "p < .0005",
             xlim = c(0,0.0005)) +
  mytheme


#----------------------------------------------------------------------------
# Why not go "continuous"?

table(RPPdata$T.pval.USE.O[idO.05])

alphas <- c(1e-14,5e-14,
            1e-13,5e-13,
            1e-12,5e-12,
            1e-11,5e-11,
            1e-10,5e-10,
            1e-9,5e-9,
            1e-8,5e-8,
            1e-7,5e-7,
            1e-6,5e-6,
            1e-5,5e-5,
            1e-4,2e-4,3e-4,4e-4,5e-4,6e-4,7e-4,8e-4,9e-4,
            1e-3,2e-3,3e-3,4e-3,5e-3,6e-3,7e-3,8e-3,9e-3,
            1e-2,2e-2,3e-2,4e-2,5e-2,6e-2,7e-2,8e-2,9e-2)

alphadata <- ldply(alphas,function(alpha) cbind.data.frame(
  alpha.level=alpha,
  r.ori.rep = cor(RPPdata$T.pval.USE.O[RPPdata$T.pval.USE.O<alpha],RPPdata$T.pval.USE.R[RPPdata$T.pval.USE.O<alpha]),
  r.ori.rep.log10 = cor(RPPdata$T.pval.USE.O.log[RPPdata$T.pval.USE.O<alpha],RPPdata$T.pval.USE.R.log[RPPdata$T.pval.USE.O<alpha]),
  N = sum(RPPdata$T.pval.USE.O<alpha))
  )

ggplot(alphadata,aes(x=alpha.level,y=r.ori.rep)) +
  geom_point() + geom_line() + mytheme

alphaslabels <- c("1e-14","",
                  "","",
                  "1e-12","5e-12",
                  "","",
                  "","",
                  "","5e-9",
                  "","5e-8",
                  "","5e-7",
                  "","5e-6",
                  "","5e-5",
                  "1e-4","","","","5e-4","","","","",
                  "1e-3","","","","5e-3","","","","",
                  "1e-2","","","","5e-2","","","","")

p1 <- ggplot(alphadata,aes(x=alpha.level,y=r.ori.rep)) +
  geom_hline(yintercept = 0) +
  geom_line(linetype=2, colour="grey70") +
  geom_point(aes(fill=N,size=N), colour="whitesmoke", pch=21) +
  scale_x_log10("Significance threshold",breaks=alphas, labels=alphaslabels) +
  scale_y_continuous("Correlation: p-values", limits = c(-.3,1)) +
  scale_fill_continuous("Data points", breaks = c(10,45,90), guide="legend") +
  scale_size_continuous("Data points", breaks = c(10,45,90)) +
  theme_bw()  + theme(axis.text.x = element_text(angle =  45, vjust=.7,hjust=.8),
                   legend.position = c(.1,.50))


p2 <- ggplot(alphadata,aes(x=alpha.level,y=r.ori.rep.log10)) +
  geom_hline(yintercept = 0) +
  geom_line(linetype=2, colour="grey70") +
  geom_point(aes(fill=N,size=N), colour="whitesmoke", pch=21) +
  scale_x_log10("Significance threshold",breaks=alphas, labels=alphaslabels) +
  scale_y_continuous("Correlation: p-values [log10]", limits = c(-.3,1)) +
  scale_fill_continuous("Data points", breaks = c(10,45,90), guide="legend") +
  scale_size_continuous("Data points", breaks = c(10,45,90)) +
  theme_bw()  +  theme(axis.text.x = element_text(angle =  45, vjust=.7,hjust=.8),
                   legend.position = c(.1,.50))

grid.arrange(p1,p2)

