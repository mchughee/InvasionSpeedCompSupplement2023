# Generating height vs max dispersal distance lm figure

library(tidyverse)
library(ggplot2)

setwd("C:/Users/erinm/Dropbox/Williams' Lab/Arabidopsis/UBC_Data/Competition Experiment 2022")

## read in data

disp<-read.csv("Competition_Dispersal_2022.csv")

## group data by cm first

dispersal1 <- read.csv("Competition_Dispersal_2022.csv", header=TRUE)
dispersal1<-dispersal1[!is.na(dispersal1$X0),]  
dispersal1$observation <- 1:nrow(dispersal1) 

dispersal1L<-gather(dispersal1,key="dist_cm",value="seeds",X0:X40)
dispersal1L$dist_cm<-as.numeric(sub(".","",dispersal1L$dist_cm)) 
dispersal1L<-mutate_at(dispersal1L,"seeds",~replace(.,is.na(.),0))

disperse_cm_phen<-
  group_by(dispersal1L, observation, RIL) %>%
  mutate(tot_seeds = sum(seeds)) %>%
  group_by(RIL, observation, dist_cm) %>%
  summarize(kernel=sum(seeds/tot_seeds),n=tot_seeds[1]) %>%
  filter(kernel!=0) %>% 
  data.frame()

# first, group to find max distance in cm

disperse_cm <- disperse_cm_phen %>% 
  group_by(RIL, observation, treatment) %>% 
  summarise(max_distance = max(dist_cm))

# first, read in trait data
traits<-read.csv("Competition_Experiment_2022_PhenData.csv")
cbind(disperse_cm, traits)->mastersheet


## write code for lm figure

lmdisp<-lm(max_distance~treatment, data=disperse_cm)
anova(lmdisp)

## find coefficient of variation

comp<-subset(mastersheet, treatment=="LOLIUM")
empty<-subset(mastersheet, treatment=="EMPTY")

cvempty<-(sd(empty$max_distance)/mean(empty$max_distance))
cvempty

cvcomp<-(sd(comp$max_distance)/mean(comp$max_distance))
cvcomp

# Put height into cm

mastersheet$Heightcm<-mastersheet$Height/10

# renaming habitat levels using relevel
plyr::revalue(mastersheet$Habitat, c("EMPTY" = "no competition")) -> mastersheet$Habitat
plyr::revalue(mastersheet$Habitat, c("LOLIUM" = "competition")) -> mastersheet$Habitat



## make linear regression figure
## Next, make the actual figure
library(ggplot2)


ggplot(mastersheet, aes(x=Heightcm, y=max_distance, colour=Habitat))+
  geom_point(aes(shape=Habitat), size=2)+
  geom_smooth(method="lm", se=T)+
  ylab("maximum dispersal distance (cm)")+
  xlab("height (cm)")+
  theme_classic()+
  scale_color_manual(values = c("competition" = "forestgreen", "no competition" = "black"))

# extract slopes

mastersheet %>% 
  group_by(Habitat) %>% 
  do({
    mod = lm(max_distance~Heightcm, data = .)
    data.frame(Intercept = coef(mod)[1],
               Slope = coef(mod)[2])
  })



