#########################
# Lake Mattamuskeet  WQ #
#########################

setwd("/Users/April/Desktop/Chapter 2")

library(wql)
library(ggplot2)

# Read in WQ data
WQ_long <- read.csv("/Users/April/Desktop/Chapter 2/Data/Matta_WQ.csv", header = TRUE)
WQ_long_combsite <- read.csv("/Users/April/Desktop/Chapter 2/Data/Matta_WQ_combsite.csv", header = TRUE)
WQ_long_comb_ff <- read.csv("/Users/April/Desktop/Chapter 2/Data/Matta_comb_fresh.csv", header = TRUE)

# Make sure original dataset looks correct
WQ_long
WQ_long_combsite
WQ_long_comb_ff


# Transform original dataset into wqData "long" form
WQ <- wqData(WQ_long, c(1, 2:3), 4:18, site.order = TRUE,
             type = "wide", time.format = "%m/%d/%Y")
WQ.combsite <- wqData(WQ_long_combsite, c(1, 2:3), 4:18, site.order = TRUE,
             type = "wide", time.format = "%m/%d/%Y")
WQ.comb.fresh <- wqData(WQ_long_comb_ff, c(1, 2:3), 4:18, site.order = TRUE,
                      type = "wide", time.format = "%m/%d/%Y")

###############
## All sites ##
###############

# Boxplots across all months sampled by site
# Physical characteristics
plot(WQ, vars = c("temp", "cond", "do", "ph", "sal", "secchi" ), num.col = 3,2)
# Nutrients
plot(WQ, vars = c('hardness', 'nitrate', 'nitrite', 'ammonia', 'orthop', 'tp', 'turb', 'chlora1', 'chlora2'), num.col = 3,2)

# Boxplots across all months sampled by bay
# Physical characteristics
plot(WQ.combsite, vars = c("temp", "cond", "do", "ph", "sal", "secchi" ), num.col = 3,2)
# Nutrients
plot(WQ.combsite, vars = c('hardness', 'nitrate', 'nitrite', 'ammonia', 'orthop', 'tp', 'turb', 'chlora1', 'chlora2'), num.col = 3,2)


# Boxplots across all months sampled by bay
# Physical characteristics
plot(WQ.comb.fresh, vars = c("temp", "cond", "do", "ph", "sal", "secchi" ), num.col = 3,2)
# Nutrients
plot(WQ.comb.fresh, vars = c('hardness', 'nitrate', 'nitrite', 'ammonia', 'orthop', 'tp', 'turb', 'chlora1', 'chlora2'), num.col = 3,2)




# Make and plot time series

# Physical characteristics
# By parameter across sites
temp <- tsMake(WQ, focus = "temp", layer = 1, type = "ts.mon",  qprob = 0.5)
cond <- tsMake(WQ, focus = "cond", layer = 1, type = "ts.mon",  qprob = 0.5)
do <- tsMake(WQ, focus = "do", layer = 1, type = "ts.mon",  qprob = 0.5)
ph <- tsMake(WQ, focus = "ph", layer = 1, type = "ts.mon",  qprob = 0.5)
sal <- tsMake(WQ, focus = "sal", layer = 1, type = "ts.mon",  qprob = 0.5)
secchi <- tsMake(WQ, focus = "secchi", layer = 1, type = "ts.mon", qprob = 0.5)
plotTs(temp, ylab = "temperature", xlab = "month")
plotTs(cond, ylab = "conductivity", xlab = "month")
plotTs(do, ylab = "dissolved oxygen", xlab = "month")
plotTs(ph, ylab = "pH", xlab = "month")
plotTs(sal, ylab = "salinity", xlab = "month")
plotTs(secchi, ylab = "secchi", xlab = "month")

# By parameter across bays
temp.comb <- tsMake(WQ.combsite, focus = "temp", layer = 1, type = "ts.mon",  qprob = 0.5)
cond.comb <- tsMake(WQ.combsite, focus = "cond", layer = 1, type = "ts.mon",  qprob = 0.5)
do.comb <- tsMake(WQ.combsite, focus = "do", layer = 1, type = "ts.mon",  qprob = 0.5)
ph.comb <- tsMake(WQ.combsite, focus = "ph", layer = 1, type = "ts.mon",  qprob = 0.5)
sal.comb <- tsMake(WQ.combsite, focus = "sal", layer = 1, type = "ts.mon",  qprob = 0.5)
secchi.comb <- tsMake(WQ.combsite, focus = "secchi", layer = 1, type = "ts.mon", qprob = 0.5)
plotTs(temp.comb, ylab = "temperature", xlab = "month")
plotTs(cond.comb, ylab = "conductivity", xlab = "month")
plotTs(do.comb, ylab = "dissolved oxygen", xlab = "month")
plotTs(ph.comb, ylab = "pH", xlab = "month")
plotTs(sal.comb, ylab = "salinity", xlab = "month")
plotTs(secchi.comb, ylab = "secchi", xlab = "month")

# By site
s1 <- tsMake(WQ, focus = "s1", layer = 1, type = "ts.mon",  qprob = 0.5)
s2 <- tsMake(WQ, focus = "s2", layer = 1, type = "ts.mon",  qprob = 0.5)
s3 <- tsMake(WQ, focus = "s3", layer = 1, type = "ts.mon",  qprob = 0.5)
s4 <- tsMake(WQ, focus = "s4", layer = 1, type = "ts.mon",  qprob = 0.5)
s5 <- tsMake(WQ, focus = "s5", layer = 1, type = "ts.mon",  qprob = 0.5)
s6 <- tsMake(WQ, focus = "s6", layer = 1, type = "ts.mon", qprob = 0.5)
s7 <- tsMake(WQ, focus = "s7", layer = 1, type = "ts.mon",  qprob = 0.5)
s8 <- tsMake(WQ, focus = "s8", layer = 1, type = "ts.mon",  qprob = 0.5)
s9 <- tsMake(WQ, focus = "s9", layer = 1, type = "ts.mon",  qprob = 0.5)
s10 <- tsMake(WQ, focus = "s10", layer = 1, type = "ts.mon",  qprob = 0.5)
s11 <- tsMake(WQ, focus = "s11", layer = 1, type = "ts.mon",  qprob = 0.5)
s12 <- tsMake(WQ, focus = "s12", layer = 1, type = "ts.mon", qprob = 0.5)
plotTs(s1, scales = "free_y", ylab = "s1", xlab = "month")
plotTs(s2, scales = "free_y", ylab = "s2", xlab = "month")
plotTs(s3, scales = "free_y", ylab = "s3", xlab = "month")
plotTs(s4, scales = "free_y", ylab = "s4", xlab = "month")
plotTs(s5, scales = "free_y", ylab = "s5", xlab = "month")
plotTs(s6, scales = "free_y", ylab = "s6", xlab = "month")
plotTs(s7, scales = "free_y", ylab = "s7", xlab = "month")
plotTs(s8, scales = "free_y", ylab = "s8", xlab = "month")
plotTs(s9, scales = "free_y", ylab = "s9", xlab = "month")
plotTs(s10, scales = "free_y", ylab = "s10", xlab = "month")
plotTs(s11, scales = "free_y", ylab = "s11", xlab = "month")
plotTs(s12, scales = "free_y", ylab = "s12", xlab = "month")


# Nutrients
# By parameter across sites
hardness <- tsMake(WQ, focus = "hardness", layer = 1, type = "ts.mon", qprob = 0.5)
nitrate <- tsMake(WQ, focus = "nitrate", layer = 1, type = "ts.mon", qprob = 0.5)
nitrite <- tsMake(WQ, focus = "nitrite", layer = 1, type = "ts.mon", qprob = 0.5)
ammonia <- tsMake(WQ, focus = "ammonia", layer = 1, type = "ts.mon", qprob = 0.5)
orthop <- tsMake(WQ, focus = "orthop", layer = 1, type = "ts.mon", qprob = 0.5)
phos <- tsMake(WQ, focus = "tp", layer = 1, type = "ts.mon", qprob = 0.5)
turb <- tsMake(WQ, focus = "turb", layer = 1, type = "ts.mon", qprob = 0.5)
chlora1 <- tsMake(WQ, focus = "chlora1", layer = 1, "ts.mon", qprob = 0.5)
chlora2 <- tsMake(WQ, focus = "chlora2", layer = 1, "ts.mon", qprob = 0.5)
plotTs(hardness, ylab = "hardness", xlab = "month")
plotTs(nitrate, ylab = "nitrate", xlab = "month")
plotTs(nitrite, ylab = "nitrite", xlab = "month")
plotTs(ammonia, ylab = "ammonia", xlab = "month")
plotTs(orthop, ylab = "ortho-p", xlab = "month")
plotTs(phos, ylab = "tp", xlab = "month")
plotTs(turb, ylab = "turbidity", xlab = "month")
plotTs(chlora1, ylab = "chlor-a 1", xlab = "month")
plotTs(chlora2, ylab = "chlor-a 2", xlab = "month")

# By parameter across bays
hardness.comb <- tsMake(WQ.combsite, focus = "hardness", layer = 1, type = "ts.mon", qprob = 0.5)
nitrate.comb <- tsMake(WQ.combsite, focus = "nitrate", layer = 1, type = "ts.mon", qprob = 0.5)
nitrite.comb <- tsMake(WQ.combsite, focus = "nitrite", layer = 1, type = "ts.mon", qprob = 0.5)
ammonia.comb <- tsMake(WQ.combsite, focus = "ammonia", layer = 1, type = "ts.mon", qprob = 0.5)
orthop.comb <- tsMake(WQ.combsite, focus = "orthop", layer = 1, type = "ts.mon", qprob = 0.5)
phos.comb <- tsMake(WQ.combsite, focus = "tp", layer = 1, type = "ts.mon", qprob = 0.5)
turb.comb <- tsMake(WQ.combsite, focus = "turb", layer = 1, type = "ts.mon", qprob = 0.5)
chlora1.comb <- tsMake(WQ.combsite, focus = "chlora1", layer = 1, "ts.mon", qprob = 0.5)
chlora2.comb <- tsMake(WQ.combsite, focus = "chlora2", layer = 1, "ts.mon", qprob = 0.5)
plotTs(hardness.comb, ylab = "hardness", xlab = "month")
plotTs(nitrate.comb, ylab = "nitrate", xlab = "month")
plotTs(nitrite.comb, ylab = "nitrite", xlab = "month")
plotTs(ammonia.comb, ylab = "ammonia", xlab = "month")
plotTs(orthop.comb, ylab = "ortho-p", xlab = "month")
plotTs(phos.comb, ylab = "tp", xlab = "month")
plotTs(turb.comb, ylab = "turbidity", xlab = "month")
plotTs(chlora1.comb, ylab = "chlor-a 1", xlab = "month")
plotTs(chlora2.comb, ylab = "chlor-a 2", xlab = "month")

########################
## USGS Water Quality ##
########################

# Load required packages
library(wql)
library(waterData)
library(ggplot2)
library(plotrix)
library(tibble)
library(ggpubr)

# Daily values- time series
West.TE <- importDVs("0208458893", code = "00010", stat = "00003", sdate = "2018-07-29",
                     edate = "2019-08-29") # Temperature (mean)

West.S <- importDVs("0208458893", code = "00480", stat = "00003", sdate = "2018-07-29",
                    edate = "2019-08-29") # Salinity (mean)

West.PH <- importDVs("0208458893", code = "00400", stat = "00008", sdate = "2018-07-29",
                     edate = "2019-08-29") # pH (median)

West.SC <- importDVs("0208458893", code = "00095", stat = "00003", sdate = "2018-07-29",
                     edate = "2019-08-29") # Specific Conductance (mean)

West.DO <- importDVs("0208458893", code = "00300", stat = "00003", sdate = "2018-07-29",
                     edate = "2019-08-29") # DO (mean)

West.TU <- importDVs("0208458893", code = "63680", stat = "00003", sdate = "2018-07-29",
                     edate = "2019-08-29") # Temperature (mean)

# Plot
# Temp
plot(West.TE$dates, West.TE$val, type="l",
     ylab="Daily mean Water Temperature (C)",
     xlab="", yaxs='i', xaxs='i', ylim=c(-5, 40),
     xlim=c(as.Date("2018-07-29"), as.Date("2019-08-29")), col="blue")
title("West of Hwy 94: Water Temperature",cex.main=0.95)

# Salinity
plot(West.S$dates, West.S$val, type="l",
     ylab="Daily mean Salinity, unfiltered (ppt)",
     xlab="", yaxs='i', xaxs='i', ylim=c(0, 2),
     xlim=c(as.Date("2018-07-29"), as.Date("2019-08-29")), col="blue")
title("West of Hwy 94: Salinity",cex.main=0.95)

# PH
plot(West.PH$dates, West.PH$val, type="l",
     ylab="Daily median pH",
     xlab="", yaxs='i', xaxs='i', ylim=c(6.5, 10.5),
     xlim=c(as.Date("2018-07-29"), as.Date("2019-08-29")), col="blue")
title("West of Hwy 94: pH",cex.main=0.95)

# SC
plot(West.SC$dates, West.SC$val, type="l",
     ylab="Daily mean Specific Conductance at 25C (uS/cm)",
     xlab="", yaxs='i', xaxs='i', ylim=c(0, 3100),
     xlim=c(as.Date("2018-07-29"), as.Date("2019-08-29")), col="blue")
title("West of Hwy 94: Specific Conductance",cex.main=0.95)

# Do
plot(West.DO$dates, West.DO$val, type="l",
     ylab="Daily mean Dissolved Oxygen (mg/L)",
     xlab="", yaxs='i', xaxs='i', ylim=c(4, 16),
     xlim=c(as.Date("2018-07-29"), as.Date("2019-08-29")), col="blue")
title("West of Hwy 94: Dissolved Oxygen",cex.main=0.95)

# Function to get summary stats
summary.list = function(x)list(
  N.with.NA.removed= length(x[!is.na(x)]),
  Count.of.NA= length(x[is.na(x)]),
  Mean=mean(x, na.rm=TRUE),
  Max.Min=range(x, na.rm=TRUE),
  Coeff.Variation.Prcnt=sd(x, na.rm=TRUE)/mean(x, na.rm=TRUE)*100,
  Std.Error=sd(x, na.rm=TRUE)/sqrt(length(x[!is.na(x)])))

# Summary stats
# Physical params only
summary.list(West.TE$val)
summary.list(West.S$val)
summary.list(West.PH$val)
summary.list(West.SC$val)
summary.list(West.DO$val)
summary.list(West.TU$val)

sd(West.TE$val, na.rm=TRUE)
sd(West.S$val, na.rm=TRUE)
sd(West.PH$val, na.rm=TRUE)
sd(West.SC$val, na.rm=TRUE)
sd(West.DO$val, na.rm=TRUE)
sd(West.TU$val, na.rm=TRUE)

median(West.TE$val, na.rm=TRUE)
median(West.S$val, na.rm=TRUE)
median(West.PH$val, na.rm=TRUE)
median(West.SC$val, na.rm=TRUE)
median(West.DO$val, na.rm=TRUE)
median(West.TU$val, na.rm=TRUE)

mean(West.TE$val[West.TE$val>=quantile(West.TE$val, 0.9, na.rm=TRUE)], na.rm=TRUE)
mean(West.S$val[West.S$val>=quantile(West.S$val, 0.9, na.rm=TRUE)], na.rm=TRUE)
mean(West.PH$val[West.PH$val>=quantile(West.PH$val, 0.9, na.rm=TRUE)], na.rm=TRUE)
mean(West.SC$val[West.SC$val>=quantile(West.SC$val, 0.9, na.rm=TRUE)], na.rm=TRUE)
mean(West.DO$val[West.DO$val>=quantile(West.DO$val, 0.9, na.rm=TRUE)], na.rm=TRUE)
mean(West.TU$val[West.TU$val>=quantile(West.TU$val, 0.9, na.rm=TRUE)], na.rm=TRUE)

mean(West.TE$val[West.TE$val<=quantile(West.TE$val, 0., na.rm=TRUE)], na.rm=TRUE)
mean(West.S$val[West.S$val<=quantile(West.S$val, 0.1, na.rm=TRUE)], na.rm=TRUE)
mean(West.PH$val[West.PH$val<=quantile(West.PH$val, 0.1, na.rm=TRUE)], na.rm=TRUE)
mean(West.SC$val[West.SC$val<=quantile(West.SC$val, 0.1, na.rm=TRUE)], na.rm=TRUE)
mean(West.DO$val[West.DO$val<=quantile(West.DO$val, 0.1, na.rm=TRUE)], na.rm=TRUE)
mean(West.TU$val[West.TU$val<=quantile(West.TU$val, 0.1, na.rm=TRUE)], na.rm=TRUE)


# Parse WQ by site
s1 <- WQ_long_comb_ff [ which(WQ_long_comb_ff$site=='1'), ]
s2 <- WQ_long_comb_ff [ which(WQ_long_comb_ff$site=='2'), ]

#Bay1
summary.list(s1$temp)
summary.list(s1$sal)
summary.list(s1$do)
summary.list(s1$ph)
summary.list(s1$cond)
summary.list(s1$secchi)

#Bay2
summary.list(s2$temp)
summary.list(s2$sal)
summary.list(s2$do)
summary.list(s2$ph)
summary.list(s2$cond)
summary.list(s2$secchi)

# Both bays
summary.list(WQ_long_comb_ff$temp)
summary.list(WQ_long_comb_ff$sal)
summary.list(WQ_long_comb_ff$do)
summary.list(WQ_long_comb_ff$ph)
summary.list(WQ_long_comb_ff$cond)
summary.list(WQ_long_comb_ff$sal)
summary.list(WQ_long_comb_ff$nitrate)
summary.list(WQ_long_comb_ff$nitrite)
summary.list(WQ_long_comb_ff$cond)
summary.list(WQ_long_comb_ff$hardness)
summary.list(WQ_long_comb_ff$tp)
summary.list(WQ_long_comb_ff$orthop)
summary.list(WQ_long_comb_ff$turb)



median(WQ_long_comb_ff$orthop, na.rm=TRUE)
mean(WQ_long_comb_ff$orthop[WQ_long_comb_ff$orthop<=quantile(WQ_long_comb_ff$orthop, 0.1, na.rm=TRUE)], na.rm=TRUE)
mean(WQ_long_comb_ff$orthop[WQ_long_comb_ff$orthop>=quantile(WQ_long_comb_ff$orthop, 0.9, na.rm=TRUE)], na.rm=TRUE)


WQ_long$site <- as.factor(WQ_long$site)
WQ_long_comb_ff$site <- as.factor(WQ_long_comb_ff$site)

# ANOVA to test for between site differenes
summary(aov(temp ~ site, data = WQ_long))
summary(aov(sal ~ site, data = WQ_long))
summary(aov(do ~ site, data = WQ_long))
summary(aov(ph ~ site, data = WQ_long))
summary(aov(cond ~ site, data = WQ_long))
summary(aov(secchi ~ site, data = WQ_long))

summary(aov(hardness ~ site, data = WQ_long))
summary(aov(nitrate ~ site, data = WQ_long))
summary(aov(nitrite ~ site, data = WQ_long))
summary(aov(ammonia ~ site, data = WQ_long))
summary(aov(orthop ~ site, data = WQ_long))
summary(aov(tp ~ site, data = WQ_long))
summary(aov(turb ~ site, data = WQ_long))

summary(aov(temp ~ site, data = WQ_long_comb_ff))
summary(aov(sal ~ site, data = WQ_long_comb_ff))
summary(aov(do ~ site, data = WQ_long_comb_ff))
summary(aov(ph ~ site, data = WQ_long_comb_ff))
summary(aov(cond ~ site, data = WQ_long_comb_ff))
summary(aov(secchi ~ site, data = WQ_long_comb_ff))

summary(aov(hardness ~ site, data = WQ_long_comb_ff))
summary(aov(nitrate ~ site, data = WQ_long_comb_ff))
summary(aov(nitrite ~ site, data = WQ_long_comb_ff))
summary(aov(ammonia ~ site, data = WQ_long_comb_ff))
summary(aov(orthop ~ site, data = WQ_long_comb_ff))
summary(aov(tp ~ site, data = WQ_long_comb_ff))
summary(aov(turb ~ site, data = WQ_long_comb_ff))


#library("ggpubr")
#ggboxplot(WQ.comb.fresh, x = "site", y = "turb", 
         # color = "site", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
         # order = c("1", "2"),
         # ylab = "Turbidity", xlab = "Site")

# Welch's t-test
t.test(WQ_long$temp, West.TE$val, na.om=TRUE)
t.test(WQ_long$do, West.DO$val, na.om=TRUE)
t.test(WQ_long$ph, West.PH$val, na.om=TRUE)
t.test(WQ_long$sal, West.S$val, na.om=TRUE)
t.test(WQ_long$cond, West.SC$val, na.om=TRUE)
t.test(WQ_long$turb, West.TU$val, na.om=TRUE)

# TukeyHSD
res.aov <- aov(tp ~ site, data = WQ_long)
summary(res.aov)
TukeyHSD(res.aov)
