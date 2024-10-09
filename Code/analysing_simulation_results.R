# figure generation for 500-patch simulations
# this code was written before Erin knew how to write loops!! She apologizes for the clunkiness of the code!!

# load library
library(dplyr)
library(tidyverse)
library(ggplot2)
library(car)


# first, read in last gens file
setwd("C:/Users/erinm/Dropbox/Williams' Lab/Arabidopsis/UBC_data/Competition Experiment 2022")
empty53<-read.csv("simulationmaterialsfall2023/53memp_lemp/lastgens53memp_lemp_wholepot.csv")

# Add in variables that describe the combo of parameters, the geno, and the observation number. This will help us
# keep track of which geno/parameter combo it is down the line!

empty53$type<-"empty, empty"
empty53$RIL<-"53"
empty53$rep<-1:nrow(empty53)

# then, find max distance each rep spread

empty53l<-gather(empty53,key="dist_patches",value="individuals",V1:V300)
empty53l$dist_patches<-as.numeric(sub(".","",empty53l$dist_patches)) 
empty53l<-mutate_at(empty53l,"individuals",~replace(.,is.na(.),0))
empty53l %>% filter(individuals!=0)->empty53l


group_by(empty53l, X)%>%
  dplyr::summarize(max_dist=max(dist_patches))->empty53max


empty53max$rep<-empty53max$X
empty53max$type<-"m empty, lambda empty"
empty53max$RIL<-"53"
empty53max$speed<-(empty53max$max_dist)/30


# read in other lastgens for each rep
comp53<-read.csv("simulationmaterialsfall2023/53mcomp_lcomp/lastgens53_mcomp_lcomp_wholepot.csv")


comp53<-gather(comp53,key="dist_patches",value="individuals",V1:V300)
comp53$dist_patches<-as.numeric(sub(".","",comp53$dist_patches)) 
comp53<-mutate_at(comp53,"individuals",~replace(.,is.na(.),0))
comp53 %>% filter(individuals!=0)->comp53

group_by(comp53, X)%>%
  summarize(max_dist=max(dist_patches))->comp53max

comp53max$rep<-comp53max$X
comp53max$type<-"m comp, lambda comp"
comp53max$RIL<-"53"
comp53max$speed<-(comp53max$max_dist/30)

# m comp, lambda emp

mcomp53<-read.csv("simulationmaterialsfall2023/53lemp_mcomp_sept2023/lastgens53_mcomp_lemp_wholepot.csv")

mcomp53<-gather(mcomp53,key="dist_patches",value="individuals",V1:V300)
mcomp53$dist_patches<-as.numeric(sub(".","",mcomp53$dist_patches)) 
mcomp53<-mutate_at(mcomp53,"individuals",~replace(.,is.na(.),0))
mcomp53 %>% filter(individuals!=0)->mcomp53

group_by(mcomp53, X)%>%
  summarize(max_dist=max(dist_patches))->mcomp53max

mcomp53max$rep<-mcomp53max$X
mcomp53max$type<-("m comp, lambda empty")
mcomp53max$RIL<-"53"
mcomp53max$speed<-(mcomp53max$max_dist)/30

# m empty, lambda comp
memp53<-read.csv("simulationmaterialsfall2023/53lcomp_memp_sept2023/lastgens53_memp_lcomp_wholepot.csv")

memp53<-gather(memp53,key="dist_patches",value="individuals",V1:V300)
memp53$dist_patches<-as.numeric(sub(".","",memp53$dist_patches)) 
memp53<-mutate_at(memp53,"individuals",~replace(.,is.na(.),0))
memp53 %>% filter(individuals!=0)->memp53

group_by(memp53, X)%>%
  summarize(max_dist=max(dist_patches))->memp53max

memp53max$rep<-memp53max$X
memp53max$type<-"m empty, lamda comp"
memp53max$RIL<-"53"
memp53max$speed<-(memp53max$max_dist)/30

# bind the different dataframes for each parameter combo for the genotype into one master sheet
sheet53<-rbind(empty53max, comp53max, mcomp53max, memp53max)

# make boxplot
sheet53$type<-as.factor(sheet53$type)
sheet53$max_dist<-as.numeric(sheet53$max_dist)
ggplot(sheet53, aes(type, max_dist))+
  geom_boxplot(stat="boxplot")+
  xlab("combination of m and lambda values, 53")+
  ylab("max spread")+
  theme_classic()

# now for 58

empty58<-read.csv("simulationmaterialsfall2023/58memp_lemp/lastgens58_memp_lemp_wholepot.csv")
empty58$type<-"empty, empty"
empty58$RIL<-"58"
empty58$rep<-1:nrow(empty58)

# then, find max distance each rep spread

empty58l<-gather(empty58,key="dist_patches",value="individuals",V1:V300)
empty58l$dist_patches<-as.numeric(sub(".","",empty58l$dist_patches)) 
empty58l<-mutate_at(empty58l,"individuals",~replace(.,is.na(.),0))
empty58l %>% filter(individuals!=0)->empty58l


group_by(empty58l, X)%>%
  dplyr::summarize(max_dist=max(dist_patches))->empty58max


empty58max$rep<-empty58max$X
empty58max$type<-"m empty, lambda empty"
empty58max$RIL<-"58"
empty58max$speed<-(empty58max$max_dist)/30

# comp58
comp58<-read.csv("simulationmaterialsfall2023/58mcomp_lcomp/lastgens58_mcomp_lcomp_wholepot.csv")


comp58<-gather(comp58,key="dist_patches",value="individuals",V1:V300)
comp58$dist_patches<-as.numeric(sub(".","",comp58$dist_patches)) 
comp58<-mutate_at(comp58,"individuals",~replace(.,is.na(.),0))
comp58 %>% filter(individuals!=0)->comp58

group_by(comp58, X)%>%
  summarize(max_dist=max(dist_patches))->comp58max

comp58max$rep<-comp58max$X
comp58max$type<-"m comp, lambda comp"
comp58max$RIL<-"58"
comp58max$speed<-(comp58max$max_dist/30)

# m comp, lambda emp

mcomp58<-read.csv("simulationmaterialsfall2023/58mcomp_lemp_sept2023/lastgens58_mcomp_lemp_wholepot.csv")

mcomp58<-gather(mcomp58,key="dist_patches",value="individuals",V1:V300)
mcomp58$dist_patches<-as.numeric(sub(".","",mcomp58$dist_patches)) 
mcomp58<-mutate_at(mcomp58,"individuals",~replace(.,is.na(.),0))
mcomp58 %>% filter(individuals!=0)->mcomp58

group_by(mcomp58, X)%>%
  summarize(max_dist=max(dist_patches))->mcomp58max

mcomp58max$rep<-mcomp58max$X
mcomp58max$type<-("m comp, lambda empty")
mcomp58max$RIL<-"58"
mcomp58max$speed<-(mcomp58max$max_dist)/30

# m empty, lambda comp
memp58<-read.csv("simulationmaterialsfall2023/58memp_lcomp_sept2023/lastgens58_memp_lcomp_wholepot.csv")

memp58<-gather(memp58,key="dist_patches",value="individuals",V1:V200)
memp58$dist_patches<-as.numeric(sub(".","",memp58$dist_patches)) 
memp58<-mutate_at(memp58,"individuals",~replace(.,is.na(.),0))
memp58 %>% filter(individuals!=0)->memp58

group_by(memp58, X)%>%
  summarize(max_dist=max(dist_patches))->memp58max

memp58max$rep<-memp58max$X
memp58max$type<-"m empty, lamda comp"
memp58max$RIL<-"58"
memp58max$speed<-(memp58max$max_dist)/30

# bind into one master sheet
sheet58<-rbind(empty58max, comp58max, mcomp58max, memp58max)

# make boxplot
sheet58$type<-as.factor(sheet58$type)
sheet58$max_dist<-as.numeric(sheet58$max_dist)
ggplot(sheet58, aes(type, max_dist))+
  geom_boxplot(stat="boxplot")+
  xlab("combination of m and lambda values, 58")+
  ylab("max spread")+
  theme_classic()

# 144

empty144<-read.csv("simulationmaterialsfall2023/144memp_lemp/lastgens144_memp_lemp_wholepot.csv")
empty144$type<-"empty, empty"
empty144$RIL<-"144"
empty144$rep<-1:nrow(empty144)

# then, find max distance each rep spread

empty144l<-gather(empty144,key="dist_patches",value="individuals",V1:V300)
empty144l$dist_patches<-as.numeric(sub(".","",empty144l$dist_patches)) 
empty144l<-mutate_at(empty144l,"individuals",~replace(.,is.na(.),0))
empty144l %>% filter(individuals!=0)->empty144l


group_by(empty144l, X)%>%
  dplyr::summarize(max_dist=max(dist_patches))->empty144max


empty144max$rep<-empty144max$X
empty144max$type<-"m empty, lambda empty"
empty144max$RIL<-"144"
empty144max$speed<-(empty144max$max_dist)/30

# comp144
comp144<-read.csv("simulationmaterialsfall2023/144mcomp_lcomp/lastgens144_mcomp_lcomp_wholepot.csv")


comp144<-gather(comp144,key="dist_patches",value="individuals",V1:V300)
comp144$dist_patches<-as.numeric(sub(".","",comp144$dist_patches)) 
comp144<-mutate_at(comp144,"individuals",~replace(.,is.na(.),0))
comp144 %>% filter(individuals!=0)->comp144

group_by(comp144, X)%>%
  summarize(max_dist=max(dist_patches))->comp144max

comp144max$rep<-comp144max$X
comp144max$type<-"m comp, lambda comp"
comp144max$RIL<-"144"
comp144max$speed<-(comp144max$max_dist/30)

# m comp, lambda emp

mcomp144<-read.csv("simulationmaterialsfall2023/144mcomp_lemp_sept2023/lastgens144_mcomp_lemp_wholepot.csv")

mcomp144<-gather(mcomp144,key="dist_patches",value="individuals",V1:V300)
mcomp144$dist_patches<-as.numeric(sub(".","",mcomp144$dist_patches)) 
mcomp144<-mutate_at(mcomp144,"individuals",~replace(.,is.na(.),0))
mcomp144 %>% filter(individuals!=0)->mcomp144

group_by(mcomp144, X)%>%
  summarize(max_dist=max(dist_patches))->mcomp144max

mcomp144max$rep<-mcomp144max$X
mcomp144max$type<-("m comp, lambda empty")
mcomp144max$RIL<-"144"
mcomp144max$speed<-(mcomp144max$max_dist)/30

# m empty, lambda comp
memp144<-read.csv("simulationmaterialsfall2023/144memp_lcomp_sept2023/lastgens144_memp_lcomp_wholepot.csv")

memp144<-gather(memp144,key="dist_patches",value="individuals",V1:V300)
memp144$dist_patches<-as.numeric(sub(".","",memp144$dist_patches)) 
memp144<-mutate_at(memp144,"individuals",~replace(.,is.na(.),0))
memp144 %>% filter(individuals!=0)->memp144

group_by(memp144, X)%>%
  summarize(max_dist=max(dist_patches))->memp144max

memp144max$rep<-memp144max$X
memp144max$type<-"m empty, lamda comp"
memp144max$RIL<-"144"
memp144max$speed<-(memp144max$max_dist)/30

# bind into one master sheet
sheet144<-rbind(empty144max, comp144max, mcomp144max, memp144max)

# make boxplot
sheet144$type<-as.factor(sheet144$type)
sheet144$max_dist<-as.numeric(sheet144$max_dist)
ggplot(sheet144, aes(type, max_dist))+
  geom_boxplot(stat="boxplot")+
  xlab("combination of m and lambda values, 144")+
  ylab("max spread")+
  theme_classic()



## Now for 187

empty187<-read.csv("simulationmaterialsfall2023/187memp_lemp/lastgens187_memp_lemp_wholepot.csv")
empty187$type<-"empty, empty"
empty187$RIL<-"187"

# then, find max distance each rep spread

empty187<-gather(empty187,key="dist_patches",value="individuals",V1:V300)
empty187$dist_patches<-as.numeric(sub(".","",empty187$dist_patches)) 
empty187<-mutate_at(empty187,"individuals",~replace(.,is.na(.),0))
empty187 %>% filter(individuals!=0)->empty187
group_by(empty187, X)%>%
  summarize(max_dist=max(dist_patches))->empty187max

empty187max$rep<-empty187max$X
empty187max$type<-"m empty, lambda empty"
empty187max$RIL<-"187"
empty187max$speed<-(empty187max$max_dist)/30


# read in other lastgens for each rep
comp187<-read.csv("simulationmaterialsfall2023/187mcomp_lcomp/lastgens187_mcomp_lcomp_wholepot.csv")


comp187<-gather(comp187,key="dist_patches",value="individuals",V1:V300)
comp187$dist_patches<-as.numeric(sub(".","",comp187$dist_patches)) 
comp187<-mutate_at(comp187,"individuals",~replace(.,is.na(.),0))
comp187 %>% filter(individuals!=0)->comp187

group_by(comp187, X)%>%
  summarize(max_dist=max(dist_patches))->comp187max

comp187max$rep<-comp187max$X
comp187max$type<-"m comp, lambda comp"
comp187max$RIL<-"187"
comp187max$speed<-(comp187max$max_dist/30)

# m comp, lambda emp

mcomp187<-read.csv("simulationmaterialsfall2023/187mcomp_lemp_sept2023/lastgens187_mcomp_lemp_wholepot.csv")

mcomp187<-gather(mcomp187,key="dist_patches",value="individuals",V1:V300)
mcomp187$dist_patches<-as.numeric(sub(".","",mcomp187$dist_patches)) 
mcomp187<-mutate_at(mcomp187,"individuals",~replace(.,is.na(.),0))
mcomp187 %>% filter(individuals!=0)->mcomp187

group_by(mcomp187, X)%>%
  summarize(max_dist=max(dist_patches))->mcomp187max

mcomp187max$rep<-mcomp187max$X
mcomp187max$type<-("m comp, lambda empty")
mcomp187max$RIL<-"187"
mcomp187max$speed<-(mcomp187max$max_dist)/30

# m empty, lambda comp
memp187<-read.csv("simulationmaterialsfall2023/187memp_lcomp_sept2023/lastgens187_memp_lcomp_wholepot.csv")

memp187<-gather(memp187,key="dist_patches",value="individuals",V1:V300)
memp187$dist_patches<-as.numeric(sub(".","",memp187$dist_patches)) 
memp187<-mutate_at(memp187,"individuals",~replace(.,is.na(.),0))
memp187 %>% filter(individuals!=0)->memp187

group_by(memp187, X)%>%
  summarize(max_dist=max(dist_patches))->memp187max

memp187max$rep<-memp187max$X
memp187max$type<-"m empty, lamda comp"
memp187max$RIL<-"187"
memp187max$speed<-(memp187max$max_dist)/30

# bind into one master sheet
sheet187<-rbind(empty187max, comp187max, mcomp187max, memp187max)

# make boxplot
sheet187$type<-as.factor(sheet187$type)
sheet187$max_dist<-as.numeric(sheet187$max_dist)
ggplot(sheet187, aes(type, max_dist))+
  geom_boxplot(stat="boxplot")+
  xlab("combination of m and lambda values 187")+
  ylab("max spread")+
  theme_classic()

### combine all sheets
master<-rbind(sheet53, sheet58, sheet144, sheet187)
master$speedcm<-master$speed*8

library(plyr)
library(goeveg)
### Change level names for "type" to be clearer!
plyr::revalue(master$type, c("m empty, lambda empty" = "no competition")) -> master$type

plyr::revalue(master$type, c("m comp, lambda comp" = "both parameters reduced by competition")) -> master$type

plyr::revalue(master$type, c("m empty, lamda comp" = "lambda reduced by competition")) -> master$type
# I forgot the b in lambda when I wrote out m empty, lambda comp, that's why it was acting funny

plyr::revalue(master$type, c("m comp, lambda empty" = "dispersal reduced by competition")) -> master$type

#revalue(master$RIL, c("short, late-flowering"="187"))->master$RIL
#revalue(master$RIL, c("tall, early-flowering"="53"))->master$RIL

# let's reorder factor levels so the boxplot is more intuitive
master$type <- factor(master$type, levels = c("no competition", "dispersal reduced by competition", "lambda reduced by competition", "both parameters reduced by competition"))

# summarize into an average for each ril x treatment, plus coefficient of variation for each ril * treatment

master %>% 
  dplyr::group_by(RIL, type) %>% 
  dplyr::summarize(mean_speed=mean(speedcm), cv_speed=cv(speedcm))->master_avg


## Plotting points for new manuscript figure November 2023-- effect of treatment on invasion speed in sims
library(stringr)

master_avg %>% 
  mutate(RIL=fct_relevel(RIL,"53", "187", "144", "58")) %>% 
  ggplot(aes(x=RIL, y=mean_speed, colour=type))+
  geom_point(size=1.5)+
  xlab("Genotype")+
  ylab("invasion speed \n (cm/generation)")+
  labs(tag="A")+
  scale_colour_manual(values=c("black", "skyblue2", "coral1", "green4"))+
  theme_classic()+
  theme(legend.position = "none")+
  theme(text=element_text(size=10), legend.text=element_text(size=7))+
  scale_x_discrete(labels = c("no competition" = "no competition", "dispersal reduced by competition" = "dispersal reduced \n by competition", "lambda reduced by competition"="lambda reduced \n by competition",  "both parameters reduced by competition"="both parameters reduced \n by competition"))->avg_fig

cv_fig<-master_avg %>% 
  mutate(RIL=fct_relevel(RIL,"53", "187", "144", "58")) %>% 
  ggplot(aes(x=RIL, y=cv_speed, colour=type))+
  geom_point(size=1.5)+
  xlab("Genotype")+
  ylab("coefficient of variation \n of invasion speed")+
  labs(tag="B")+
  scale_colour_manual(values=c("black", "skyblue2", "coral1", "green4"))+
  theme_classic()+
  theme(text=element_text(size=10), legend.text=element_text(size=7), legend.title=element_blank())+
  scale_x_discrete(labels = c("no competition" = "no competition", "dispersal reduced by competition" = "dispersal reduced \n by competition", "lambda reduced by competition"="lambda reduced \n by competition",  "both parameters reduced by competition"="both parameters reduced \n by competition"))+
  guides(colour=guide_legend(ncol=2))

cv_fig

library(cowplot)

# Pull legend
legend_b <- get_legend(cv_fig)
legend<-plot_grid(legend_b)
legend

# pull plots
plots<-plot_grid(avg_fig+ theme(legend.position="none"), cv_fig+ theme(legend.position="none"), label_size = 12, align="v")
plots

# Make compound figure

fig_change<-ggdraw() +
  draw_plot(plots, x = 0, y=0.25, width = 0.9, height=0.7) +
  draw_plot(legend, x = 0.2, y=0.05, width = .5, height=0.1)

pdf("Fig4_changed.pdf", width=4.33, height=2.75)

fig_change

dev.off()
