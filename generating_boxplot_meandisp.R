## Boxplot showing reductions in dispersal due to competition
library(ggplot2)

# read in file
parms_raw<-read.csv("simulationmaterialsfall2023/individual_wholepot_parameters_compiled_sept2023.csv")

# convert m to mean dispersal distance in cm (*8 for number of cm in each pot)
parms_raw$meandispcm<-parms_raw$meandisp*8

# tell R to read RIL as a factor, then rename as a RIL
parms_raw$RIL<-as.factor(parms_raw$RIL)
names(parms_raw)[names(parms_raw) == "RIL"] <- "genotype"

# Revalue treatment to make names nicer
plyr::revalue(parms_raw$treatment, c("EMPTY" = "no competition")) -> parms_raw$treatment
plyr::revalue(parms_raw$treatment, c("LOLIUM" = "competition")) -> parms_raw$treatment

write.csv(parms_raw, "parms_relevelled.csv")

# Make boxplot
ggplot(parms_raw, aes(genotype, meandispcm, fill=treatment))+
  geom_boxplot(stat="boxplot")+
  xlab("CVL")+
  ylab("mean dispersal (cm/generation)")+
  scale_fill_manual(values=c("green4", "grey"))+
  theme_classic()

