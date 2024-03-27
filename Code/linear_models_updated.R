# Code for linear models

# Make sure plyr is unloaded! Otherwise some dplyr functions will not run
detach("package:plyr", unload = TRUE)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(car)

# Read in old data set and manipulate to get number of germinating seedlings per runways, which will be our measures of fecundity for the simulation
dispersal1 <- read.csv("Competition_Dispersal_2022.csv", header=TRUE)
dispersal1<-dispersal1[!is.na(dispersal1$X0),]  
dispersal1$observation <- 1:nrow(dispersal1) 

# Change to tall format

dispersal1L<-gather(dispersal1,key="dist_cm",value="seeds",X0:X40)
dispersal1L$dist_cm<-as.numeric(sub(".","",dispersal1L$dist_cm)) 
dispersal1L<-mutate_at(dispersal1L,"seeds",~replace(.,is.na(.),0))

disperse_cm_phen<-
  group_by(dispersal1L,observation, RIL) %>%
  mutate(tot_seeds = sum(seeds)) %>%
  group_by(RIL,observation, dist_cm) %>%
  summarize(kernel=sum(seeds/tot_seeds),n=tot_seeds[1], treatment=treatment) %>%
  filter(kernel!=0) %>% 
  data.frame()

# Summarize max distance
disperse_phen1 <- disperse_cm_phen %>% 
  group_by(RIL, observation, treatment) %>% 
  summarise(max_distance = max(dist_cm))

write.csv(disperse_phen1, "maxdispdistance_cm.csv")

# Just calculating coefficient of variation!

subset(disperse_phen1, treatment=="LOLIUM")->lolium
subset(disperse_phen1, treatment=="EMPTY")->empty

library(goeveg)

cv(lolium$max_distance)
cv(empty$max_distance)


# bring in phenology data

getwd()
pheno<-read.csv("Competition_Experiment_2022_PhenData.csv")
pheno<-pheno[!is.na(pheno$Height),]

# Properly format all phenological dates
# install.packages("lubridate")
library(lubridate)

pheno$FirstFlower<-as.Date(pheno$FirstFlower, format = "%d/%m/%Y")
pheno$PlantDate<-as.Date(pheno$PlantDate, format="%d/%m/%Y")
pheno$GermDate<-as.Date(pheno$GermDate, format="%d/%m/%Y")
pheno$IntoRunway<-as.Date(pheno$IntoRunway, format="%d/%m/%Y")
pheno$RunwayDown<-as.Date(pheno$RunwayDown, format="%d/%m/%Y")

# Calculate phenological milestones
pheno$toflower<-pheno$FirstFlower-pheno$PlantDate
pheno$toflower

pheno$fromgermtoflower<-pheno$FirstFlower-pheno$GermDate
pheno$toflower

pheno$chilldate<-pheno$PlantDate+pheno$ChillTime
pheno$chilldate

pheno$fromchilltoflower<-pheno$FirstFlower-pheno$chilldate
pheno$fromchilltoflower

# separate into temporal blocks for further analysis

pheno$chilldate<-as.factor(pheno$chilldate)
pheno$block<-pheno$chilldate


library(plyr)
pheno$block<-as.factor(pheno$block)
revalue(pheno$block, c("2021-11-22" ="b1"))->pheno$block
revalue(pheno$block, c("2021-12-11" = "b2"))->pheno$block
revalue(pheno$block, c("2021-12-10" = "b2"))->pheno$block


# check to make sure the dataframes have the same number of observations
str(pheno)
str(disperse_phen1)

# merge the data
lmdata<-cbind(pheno, disperse_phen1)
lmdata$toflower<-as.numeric(lmdata$toflower)
lmdata$fromchilltoflower<-as.numeric(lmdata$fromchilltoflower)

# make sure that R is reading habitat as a factor
lmdata$Habitat<-as.factor(lmdata$Habitat)

### Linear models with habitat and block as fixed effects
# maxd~comp+block
lm0<-lm(max_distance~Habitat+block, data=lmdata)
summary(lm0)
Anova(lm0)

# test 1: maxd~comp+RIL+block
lm1<-lm(max_distance~Habitat+RIL+block, data=lmdata)
summary(lm1)
Anova(lm1)

# test 2: maxd~comp+phen+block
lm2<-lm(max_distance~Habitat+fromchilltoflower+block, data=lmdata)
summary(lm2)
Anova(lm2)

# test 3: maxd~comp+siliqueno+block
lm3<-lm(max_distance~Habitat+SiliqueNo+block, data=lmdata)
summary(lm3)
Anova(lm3)

# test 4: maxd~comp*height+block
lm4<-lm(max_distance~Habitat*Height+block, data=lmdata)
summary(lm4)
Anova(lm4)

# test 5: maxd~1
lm5<-lm(max_distance~1, data=lmdata)
summary(lm5)
anova(lm5)


### Extra linear models for supporting stats

# meandisp~treatment
parms_relevelled<-read.csv("parms_relevelled.csv")
parms_pheno<-cbind(pheno, parms_relevelled)
lmdisp<-lm(meandispcm~treatment, data=parms_pheno)
anova(lmdisp)

### Checking to see if there is any correlation between phenology and chill time
lm9<-lm(fromchilltoflower~ChillTime, data=lmdata)
summary(lm9)
anova(lm9)


# Checking for correlation between chill time and height
lm10<-lm(Height~ChillTime, data=lmdata)
summary(lm10)
anova(lm10)

# Plotting the pattern between chill time and phenology
plot(x=lmdata$ChillTime, y=lmdata$fromchilltoflower)
abline()

### Checking for correlation between phenology and maximum dispersal distance
lmflower<-lm(max_distance~fromchilltoflower, data=lmdata)
summary(lmflower)
anova(lmflower)


## redo lm for effect of habitat on dispersal distance

parms_pheno<-cbind(pheno, parms_relevelled)
lmdisp<-lm(meandispcm~treatment, data=parms_pheno)
Anova(lmdisp)

## Test to see if earlier plants dispersed more seeds--ensure that plyr is unloaded!!

dispersal1 %>% 
  summarize(totseed=rowSums(across(X0:X40)), RIL=RIL, Rep=Rep)->moreseeds

seeddata<-cbind(pheno, moreseeds)

lmseed<-lm(totseed~fromchilltoflower+block, data=seeddata)
summary(lmseed)
Anova(lmseed)

seeddata <- seeddata[ -c(24, 25)]

# Plotting the data just to check out the pattern

ggplot(seeddata, aes(x=fromchilltoflower, y=totseed, fill=block))+
  geom_point()+
  geom_smooth(method=lm)

ggplot(seeddata, aes(x=block, y=totseed))+
  geom_point()

# Running linear models without block and habitat as fixed effects for basic summary stats

### Linear models
# maxd~comp
lmhab<-lm(max_distance~Habitat, data=lmdata)
summary(lmhab)
Anova(lmhab)

# maxd~RIL

lmril<-lm(max_distance~RIL, data=lmdata)
summary(lmril)
Anova(lmril)  

# maxd~phen
lmphen<-lm(max_distance~fromchilltoflower, data=lmdata)
summary(lmphen)
Anova(lmphen)

# maxd~siliqueno
lmsil<-lm(max_distance~SiliqueNo, data=lmdata)
summary(lmsil)
Anova(lmsil)

# maxd~height
lmheight<-lm(max_distance~Height, data=lmdata)
summary(lmheight)
Anova(lmheight)

