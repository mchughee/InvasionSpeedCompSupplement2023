# Linear modelling for factors explaining variation in maximum distance dispersed

# First, read in phenology data

getwd()
pheno<-read.csv("Arabidopsis/UBC_data/Competition Experiment 2022/Competition_Experiment_2022_PhenData.csv")
pheno<-pheno[!is.na(pheno$Height),]


# bring in lubridate
# format data so that we can get days to flowering

library(lubridate)

pheno$FirstFlower<-as.Date(pheno$FirstFlower, format = "%d/%m/%Y")
pheno$PlantDate<-as.Date(pheno$PlantDate, format="%d/%m/%Y")
pheno$GermDate<-as.Date(pheno$GermDate, format="%d/%m/%Y")

pheno$toflower<-pheno$FirstFlower-pheno$PlantDate
pheno$toflower

pheno$fromgermtoflower<-pheno$FirstFlower-pheno$GermDate
pheno$toflower

pheno$chilldate<-pheno$PlantDate+pheno$ChillTime

pheno$fromchilltoflower<-pheno$FirstFlower-pheno$chilldate



# Is time to flowering normally distributed? No. Lol.
qqnorm(pheno$toflower)
pheno$toflower<-as.numeric(pheno$toflower)

# Get max distances of seeds dispersed for each RIL
library(tidyverse)
disperse_halfpot_phen<-
  group_by(dispersal1L,RIL) %>%
  mutate(tot_seeds = sum(seeds)) %>%
  group_by(RIL,Rep, dist_halfpot) %>%
  summarize(kernel=sum(seeds/tot_seeds),n=tot_seeds[1]) %>%
  filter(kernel!=0) %>% 
  data.frame()

disperse_halfpot_phen1 <- disperse_halfpot_phen %>% 
  group_by(RIL, Rep) %>% 
  summarise(max_distance = max(dist_halfpot))

# check to make sure the dataframes have the same number of observations
str(pheno)
str(disperse_cm_phen1)

# merge the data
lmdata<-merge(pheno, disperse_halfpot_phen1)
lmdata$toflower<-as.numeric(lmdata$toflower)

lmphennew<-lm(max_distance~fromchilltoflower, data=lmdata)
summary(lmphennew)
anova(lmphennew)

# Run individual linear regressions

# test 1: maxd~1

lm1<-lm(max_distance~1, data=lmdata)
summary(lm1)
anova(lm1)

# test 2: # maxd~comp*RIL

lm2<-lm(max_distance~Habitat+RIL, data=lmdata)
summary(lm2)
anova(lm2)  

# test 3:  maxd~comp*phen
lm3<-lm(max_distance~Habitat+fromchilltoflower, data=lmdata)
summary(lm3)
anova(lm3)

# test 4: maxd~comp*seedmass
lm4<-lm(max_distance~Habitat+SeedMass, data=lmdata)
summary(lm4)
anova(lm4)

# test 5: maxd~comp*siliqueno
lm5<-lm(max_distance~Habitat+SiliqueNo, data=lmdata)
summary(lm5)
anova(lm5)

# test 6: maxd~comp*height
lm6<-lm(max_distance~Habitat*Height, data=lmdata)
summary(lm6)
anova(lm6)


# test 7: maxd~competition
lm7<-lm(max_distance~Habitat, data=lmdata)
summary(lm7)
anova(lm7)


# Run AIC on significant models
AIC(lm1, lm2, lm3, lm4, lm5, lm6, lm7)