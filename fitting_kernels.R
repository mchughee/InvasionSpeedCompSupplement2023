# 5 April 2023
# Fitting whole-pot kernels to my data using Jenn's double-loop code
# First, remove all previous runs
rm(list = ls(all.names = TRUE))

# set working directory and read in data. Here, it is set to Williams' lab
setwd("C:/Users/Erin/Dropbox/Williams' Lab/Arabidopsis/UBC_data/Competition Experiment 2022")
seeddata<-read.csv("Competition_Dispersal_2022.csv")

# Now call in packages. Note: we are using plyr, which is the older version of dplyr! still supported, but note to Erin: if you are using this code later on and it isn't working, might be because plyr is no longer supported

library(plyr)
library(tidyverse)
library(bbmle)
library(lattice)

# define expnential function
fitNLLnegexp<-function(m){
  NLL<- -sum(dexp(dist,m, log=T)) #this log=T is CRUCIAL!!!
  return(NLL)
}

# Now, we are going to add in replicate number and transform the dataframe from wide to long
seeddata$replicate <- 1:nrow(seeddata) 

# Group each cm bin into a pot
seeddata %>% 
  transmute(RIL=seeddata$RIL, Rep=seeddata$Rep, treatment=seeddata$treatment, replicate=seeddata$replicate, P0=(X0), P1=rowSums(across(X1:X8)), P2=rowSums(across(X9:X16)), P3=rowSums(across(X17:X24)), P4=rowSums(across(X25:X32)), P5=rowSums(across(X33:X40)))->seeddata1
seeddata1 %>% mutate(across(P0:P5, round, 0))->seeddata1

# Organize data
seeddataL<-gather(seeddata1,key="dist_cm",value="seeds",P0:P5)
seeddataL$dist_cm<-as.numeric(sub(".","",seeddataL$dist_pot)) 
seeddataL<-mutate_at(seeddataL,"seeds",~replace(.,is.na(.),0))

# plot empirical kernels

par(mfrow=c(4,10), mar=c(2,3,1,1))

results<-split(seeddataL, list(seeddataL$replicate), drop=T)
for(i in seq_along(results))
{ 
  data<-results[[i]]
  data<-data[order(data$dist_pot),]
  
  plot(log(data$seeds)~data$dist_pot,xlim=c(0,5), pch=19, cex=0.9, ylim=c(0,1),
       xlab="", ylab="", cex.main=0.6, cex.lab=0.5, cex.axis=0.6)
  #abline(v=c(7.5, 14.5)) #draws lines in between pots
}

# fit functions. First, make dataframe
answers<-list(runway=rep(NA,40), mom_sil=rep(NA,40),mom_ht=rep(NA,40),
              scaleparm=rep(NA,40), CIlow=rep(NA,40),CIhigh=rep(NA,40), treatment=rep(NA,40), RIL=rep(NA, 40))

par(mfrow=c(4,10), mar=c(2,3,1,1))
y<-seq(0,5,1)
# big loop!
for(i in 1:40){
  data<-results[[i]]
  data<-data[order(data$dist_pot),]
  
  #create vector of distances
  for (j in 1:length(data$dist_pot))
  {
    if(j==1){
      distance.vec<-rep(data$dist_pot[1],data$seeds[1])
    }
    else{
      distance.vec<-c(distance.vec,rep(data$dist_pot[j],data$seeds[j]))
    }}
  tot<-length(distance.vec)  
  
  fit1<-mle2(minuslogl=fitNLLnegexp,start=list(m=0.5), data=list(dist=distance.vec), method="BFGS")    
  
  plot(data$seeds~data$dist_pot,xlim=c(0,5), pch=19, cex=0.9, 
       main=paste(data$replicate[1]),
       xlab="", ylab="", cex.main=1.2, cex.lab=0.5, cex.axis=0.6)
  lines(y,tot*dexp(y,rate=coef(fit1)), lwd=2,lty=2, col="green3")
  
  answers$replicate[i]<-data$replicate[1]
  
  #answers$mom_sil[i]<-data$siliques[1]
  #answers$mom_ht[i]<-data$height[1]
  answers$scaleparm[i]<-coef(fit1)
  answers$CIlow[i]<-confint(fit1)[1]
  answers$CIhigh[i]<-confint(fit1)[2] 
  answers$treatment[i]<-data$treatment[1]
  answers$RIL[i]<-data$RIL
}

# force answers into a dataframe
answers<-data.frame(answers)
answers$meandisp<- (1/answers$scaleparm)

write.csv(answers,"compexp2022.kernelwholepotmomis0.summary.1.csv")

