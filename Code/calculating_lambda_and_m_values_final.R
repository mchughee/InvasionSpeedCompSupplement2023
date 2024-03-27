# Calculating lambda values and merging these lambda values with dispersal spreadsheet, then using this master spreadsheet to calculate paramters with and without competition

# set working directory
setwd("C:/Users/Erin/Dropbox/Williams' Lab/Arabidopsis/UBC_data/Competition Experiment 2022")

# call packages
library(tidyverse)
library(ggplot2)
library(car)
library(dplyr)

# read in new dataset
mval<-read.csv("simulationmaterialsfall2023/compexp2022.kernelwholepotmomis0.summary.csv")

# Read in old data set and manipulate to get number of germinating seedlings per runways, which will be our measures of fecundity for the simulation
disperse<-read.csv("Competition_Dispersal_2022.csv")
disperse$observation <- 1:nrow(disperse) 
disperse %>% 
  summarize(germ_total=(rowSums(across(X0:X40))), RIL=RIL, observation=observation, Rep=Rep, treatment=treatment)->sum_germ


# Bind with output from fitting m values
cbind(sum_germ$germ_total, mval)->master
colnames(master)[1] <- "germnum"


# time to reduce germ number for competitive runways

master$lambda_compyes<-case_when(
  master$RIL=="53"~ master$germnum*0.095238095,
  master$RIL=="58" ~ master$germnum*0.043816254,
  master$RIL=="144" ~ master$germnum*0.057877813,
  master$RIL=="187"~ master$germnum*0.064236497)

master$lambda_compno<-case_when(
  master$RIL=="53"~ master$germnum,
  master$RIL=="58" ~ master$germnum,
  master$RIL=="144" ~ master$germnum,
  master$RIL=="187"~ master$germnum)


# now, make "master" dataframe into a csv file
# write.csv(master, "individual_wholepot_parameters_compiled_sept2023.csv")
master$RIL<-as.factor(master$RIL)
master$treatment<-as.factor(master$treatment)

## Okay, time to take averages. let's subdivide the data to get going:

empty53<-subset(master, treatment=="EMPTY" & RIL=="53")
lolium53<-subset(master, treatment=="LOLIUM" & RIL=="53")

empty58<-subset(master, treatment=="EMPTY" & RIL=="58")
lolium58<-subset(master, treatment=="LOLIUM" & RIL=="58")

empty144<-subset(master, treatment=="EMPTY" & RIL=="144")
lolium144<-subset(master, treatment=="LOLIUM"& RIL=="144")

empty187<-subset(master, treatment=="EMPTY" & RIL=="187")
lolium187<-subset(master, treatment=="LOLIUM"& RIL=="187")


# alright, now let's take the actual averages for m

### 53
### disp comp no
mean(empty53$scaleparm)
t.test(empty53$scaleparm)$conf.int
### disp comp yes
mean(lolium53$scaleparm)
t.test(lolium53$scaleparm)$conf.int

### 58
### disp comp no
mean(empty58$scaleparm)
t.test(empty58$scaleparm)$conf.int
### disp comp yes
mean(lolium58$scaleparm)
t.test(lolium58$scaleparm)$conf.int

### 144
### disp comp no
mean(empty144$scaleparm)
### disp comp yes
mean(lolium144$scaleparm)

### 187
### disp comp no
mean(empty187$scaleparm)
### disp comp yes
mean(lolium187$scaleparm)


### find lambda values for each scenario
### 53 first
#### lambdacomp/mcomp-- comp x reduction
mean(lolium53$lambda_compyes)
t.test(lolium53$scaleparm)$conf.int
#### lambdacomp/memp-- empty x reduction
mean(empty53$lambda_compyes)
### lamemp/mcomp-- comp no reduction
mean(lolium53$lambda_compno)
### lamemp/memp-- empty no reduction
mean(empty53$lambda_compno)


### find lambda values for each scenario
### Genotype 58
#### lambdacomp/mcomp-- comp x reduction
mean(lolium58$lambda_compyes)
#### lambdacomp/memp-- empty x reduction
mean(empty58$lambda_compyes)
### lamemp/mcomp-- comp no reduction
mean(lolium58$lambda_compno)
### lamemp/memp-- empty no reduction
mean(empty58$lambda_compno)


### find lambda values for each scenario
### Genotype 144
#### lambdacomp/mcomp-- comp x reduction
mean(lolium144$lambda_compyes)
#### lambdacomp/memp-- empty x reduction
mean(empty144$lambda_compyes)
### lamemp/mcomp-- comp no reduction
mean(lolium144$lambda_compno)
### lamemp/memp-- empty no reduction
mean(empty144$lambda_compno)


### find lambda values for each scenario
### Genotype 187
#### lambdacomp/mcomp-- comp x reduction
mean(lolium187$lambda_compyes)
#### lambdacomp/memp-- empty x reduction
mean(empty187$lambda_compyes)
### lamemp/mcomp-- comp no reduction
mean(lolium187$lambda_compno)
### lamemp/memp-- empty no reduction
mean(empty187$lambda_compno)


