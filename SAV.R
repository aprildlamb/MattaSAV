##########################
# Lake Mattamuskeet  SAV #
##########################

setwd("/Users/April/Desktop/Chapter 2")

library(ggplot2)
library(forcats)
library(PresenceAbsence)
library(foreign)
library(ggplot2)
library(MASS)
library(Hmisc)
library(reshape2)

# Read in data
PA <- read.csv("/Users/April/Desktop/Chapter 2/Data/SAV_PA.csv", header = TRUE)
PA.matrix <- read.csv("/Users/April/Desktop/Chapter 2/Data/SAV_PAmatrix.csv", header = FALSE)
quad <- read.csv("/Users/April/Desktop/Chapter 2/Data/SAV_quadrat.csv", header = TRUE)

# Write a function to alcualte mean and SD
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

# plot with error bars
quadrat <- data_summary(quad, varname="Biomass", 
                    groupnames=c("Trt", "Biomass_type", "PlantID"))

p1<- ggplot(quadrat, aes(x=PlantID, y=Biomass, fill=Biomass_type)) + 
  geom_bar(stat="identity", color="black", position=position_dodge()) +
  geom_errorbar(aes(ymin=Biomass-sd, ymax=Biomass+sd), width=.2, position=position_dodge(.9)) 
p1 + facet_grid(cols= vars(Trt), switch= "x")

# plot with error bars
quadrat2 <- data_summary(quad, varname="Biomass_corrected", 
                        groupnames=c("Trt", "Biomass_type", "PlantID"))

p2<- ggplot(quadrat2, aes(x=PlantID, y=Biomass_corrected, fill=Biomass_type)) + 
  geom_bar(stat="identity", color="black", position=position_dodge()) +
  geom_errorbar(aes(ymin=Biomass_corrected-sd, ymax=Biomass_corrected+sd), width=.2, position=position_dodge(.9)) 
p2 + facet_grid(cols= vars(Trt), switch= "x")


# Parse out into just Val and Val + Lilly
Biomass_V<- quad [ which(quad$Trt=='Val' & quad$Biomass_type=="Total"), ]
Biomass_VL <- quad [ which(quad$Trt=='Val + Lilly' & quad$PlantID=='Val' & quad$Biomass_type=='Total'), ]

# Welch's t-test between Val and Val + Lilly
t.test(Biomass_V$Biomass_corrected, Biomass_VL$Biomass_corrected, na.om=TRUE)


quad2<- quad [ which(quad$PlantID=='Val', quad$Biomass_type=='Total'), ]
res.aov<-(aov(Biomass_corrected ~ Trt, data = quad2))

TukeyHSD(res.aov)


# Aerial dataset
cover_count <- read.csv("/Users/April/Desktop/Chapter 2/Data/Aerial coverage.csv", header = TRUE)
cover_type <- read.csv("/Users/April/Desktop/Chapter 2/Data/Aerial coverage_bloomcount.csv", header = TRUE)


# parse by treatment
trt1 <- cover_count [ which(cover_count$trt=='Val'), ]
trt2 <- cover_count [ which(cover_count$trt=='Val + Lilly'), ]
trt3 <- cover_count [ which(cover_count$trt=='Najas + Lilly'), ]
trt4 <- cover_count [ which(cover_count$trt=='Lilly'), ]
trt5 <- cover_count [ which(cover_count$trt=='All'), ]

# Function to get summary stats
summary.list = function(x)list(
  N.with.NA.removed= length(x[!is.na(x)]),
  Count.of.NA= length(x[is.na(x)]),
  Mean=mean(x, na.rm=TRUE),
  Max.Min=range(x, na.rm=TRUE),
  Coeff.Variation.Prcnt=sd(x, na.rm=TRUE)/mean(x, na.rm=TRUE)*100,
  Std.Error=sd(x, na.rm=TRUE)/sqrt(length(x[!is.na(x)])))

# Percent cover stats
summary.list(trt1$percent.cover)
summary.list(trt2$percent.cover)
summary.list(trt3$percent.cover)
summary.list(trt4$percent.cover)
summary.list(trt5$percent.cover)

# Val bloom stats
summary.list(trt1$number.of.val.blooms)
summary.list(trt2$number.of.val.blooms)
summary.list(trt3$number.of.val.blooms)
summary.list(trt4$number.of.val.blooms)
summary.list(trt5$number.of.val.blooms)

# Lilly leaf stats
summary.list(trt1$number.of.lilly.pads)
summary.list(trt2$number.of.lilly.pads)
summary.list(trt3$number.of.lilly.pads)
summary.list(trt4$number.of.lilly.pads)
summary.list(trt5$number.of.lilly.pads)


# Plot for fun
p3 <- ggplot(cover_count) + geom_col(aes(trt, percent.cover))
p3
p4 <- ggplot(cover_type) + geom_col(aes(Trt, number)
p4

summary(aov(biomass ~ trt1, data = quadrat))