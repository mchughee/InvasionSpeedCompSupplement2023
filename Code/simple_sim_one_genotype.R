## basic script for simulation, adapted from:
## Urquhart, C.U., and Williams, J.L. (2021). Trait correlations and landscape fragmentation jointly alter expansion speed via evolution
## at the leading edge in simulated range expansions. Theor. Ecol. 14(3), 381-394.
## and from
## Williams, J.L., Snyder, R.E., and Levine, J.M. (2016b). The influence of evolution on population spread through patchy landscapes. Am. Nat. 
#188(1), 15-26.

rm(list=ls(all=TRUE))
## Tell R our *fixed* parameters for the model

# how many simulations are we running the model for?
nsim<-500

# how many generations?
tmax<-30

# how many individuals should make up the starting population?
N_0 <- 3

# how long should the environment be for the simulation?
patch<-300

# how many genotypes or strategies are there? (in Williams 2016b, they use multiple strategies,
# but here, we're just using one genotype)
strat<-1

## genotype parameters (switch out these for the different combos of 
# lambda and fecundity)

#fecundity - change depending on which genotype/combo we're running
lambda<-68

# competitive ability- keep this one constant
alpha<-0.1

# dispersal ability-change depending on which genotype/combo we're running
m <- 2.607800





##########################################################

# Let's define some functions

# First, the function for seed production-- using modified Beverton-Holt where:
 # j= # of plants of genotype j
  # k=  total population
  # lambda= low-density reproductive rate

growth <- function(j,k,lambda){ 
  # tell r to populate the vector, seeds, with the output of the stochastic Beverton-Holt model
  seeds<-vector()
  seeds<-round((lambda*j)/(1+alpha*(k-1)),0)
  return(rpois(length(seeds),seeds))
}

### dispersal function 
# allow seeds to disperse randomly from x to y
# dispersal kernel is exponential
# "disperse randomly" means seeds can move backward and forward

disperse<-function(x,y,m){ 
  # x= number of dispersing seeds a patch
  # y = the row that the patch is in
  # m = disperal parameter
  # Tell r to create a vector with the patch number and use if else statement to change patch number in vector
  pmove<-vector("numeric", patch)
  # if no seeds in x, return empty value
  if (x==0)
    return(pmove)
  # otherwise,  multiply direction by distance to find movement vector of seeds
  else
    move<-distance(x,m)*direction(x)
  for (i in 1:x){ 
    # move seeds to new patch
    for (j in 1:patch){ 
      if (move[i]==pwdist[y,j])
      {pmove[j]<-pmove[j]+1}
    }
  }
  return(pmove) 
}

### distance function will generate random distances, moving x number of seeds by drawing distances from exponential dispersal
### kernel with parameter m

distance<-function(x,m){
  dist<-round(rexp(x,m),0)
  return(dist)
}

## direction function will generate random directions for x number of seeds to disperse in,
#drawing random directions from a binomial distribution where p=0.5
## and any draws of 0 get refactored to -1 so that seeds are dispersing backwards
direction<-function(x){
  dir<-rbinom(x,1,0.5)
  sapply(dir,refactor)
}

## this is where we define refactor
refactor<-function(x){ 
  #  the x in this equation comes from the random binomial draws previously
  if (x==0)
    x<- -1
  else x
}

### get the patch at the leading edge

lemove<-function(x){ 
  # x= row in spread matrix at time t
  moved<-patch-pleft(rev(x))
  if (moved == 0){
    return(moved)
  }
  else{
    moved<-dvect[moved]
    return(moved) 
  }
  
}
# pleft function tells us how many patches there are left in the landscape that haven't been colonized
pleft<-function(x){ 
  # x is x from lemove-- i.e. the patch at the leading edge!
  # y tells us how far the leading edge is from the end of the landscape
  y<-0
  # Here, we tell R to check each patch to see if the population is greater than 0 
  for (k in 1:patch){
    if (x[k] != 0){
      # once we reach a patch with pop=0 (aka the leading edge)
      break
    }
    # if we haven't reached the leading edge at that patch, add a patch to y
    else y<-y+1
  }
  return(y)
}

### speed function tells us how the leading edge moves with each generation
speed<- function(x,y){ 
  # x = leading edge patch
  # y = number of patches moved (this is our output!)
  for (i in 1:tmax){
    if (i == 1){
      y[i]<-x[i]-1
    }
    else if (x[i] == 0){
      y[i]<-0
    }
    else
      y[i]=x[i]-x[i-1]
  }
  return(y)
}
###################

# Initialize data structures


#Population Spread Matrix - each column is a new patch. This will be added to each
# generation as plants grow and disperse seeds
spread1<-matrix(0,tmax,patch)
spread1[1,1]<-N_0

#seeds stores seeds produced in each generation
seeds<-matrix(NA,strat,patch)

# stores the number of individuals in each patch after a dispersal event, with
# rows indicating where seeds were produced, and columns indicated where seeds end up
seedsd1<-matrix(0,patch,patch)

# Pairwise Distances Matrix
pwdist<-matrix(NA,patch,patch)
for (i in 1:patch){
  for (j in 1:patch){
    dvect<-1:patch
    pwdist[i,j]<-dvect[j]-dvect[i]
  }
}

# the most important part-- tmax_pop matrix stores last generation of spread
tmax_pop<-matrix(NA,nsim,patch)
#Vector to store max spread dist for each strategy (again, we only have one here)
ledge<-vector("numeric",strat)


# matrices contain the colonized patches
pcol<-matrix(NA, nrow=strat+1, ncol=tmax-1)
tcol<-matrix(NA,nrow=nsim,ncol=tmax-1)

####################################################################

#Finally! the simulation 
for (x in 1:nsim){
  for (i in 1:(tmax-1)){
    
    # make plants reproduce
    seeds[1,]<-growth(spread1[i,],spread1[i,],lambda) 
    
    for (j in 1:patch){  
      
      # let those seeds disperse
      seedsd1[j,]<-disperse(seeds[1,j],j,m[1])
      
      # populate the spread matrix
      spread1[i+1,]<-apply(seedsd1,2,sum)  
      
      #find how far the leading edge has spread in this generation
      pcol[1,i]<-lemove(spread1[i+1,])
    }
    
    # put this handy info into tmax (our population matrix)
    tmax_pop[x,]<-spread1[tmax,]
    
    for (i in 1:strat){
      # this is strategy stuff that is not important because we only have one "strategy" (one genotype)
      ledge[i]<-lemove(tmax_pop[i,])
    }}
  
  # save matrices
  write.csv(spread1,file=paste0("spread187_memp_lcomp_wholepot",x,".csv"))
  write.csv(pcol,file=paste0("leading187_memp_lcomp_wholepot",x,".csv"))
}
# the most important matrix!! This one saves the populations during the last generation of spread
# at each patch in the landscape
write.csv(tmax_pop,file=paste0("lastgens187_memp_lcomp_wholepot.csv"))
 

