## basic script for simulation
rm(list=ls(all=TRUE))
## parameters (constant)

# number of simulations
nsim<-500

# number of generations 
tmax<-30

 #starting population for each strategy
N_0 <- 3

# length of environment
patch<-300

# number of genotypes / strategies to simulate at once
strat<-1

# population / genotype trait parameters (may change)

#fecundity
lambda<-68

# competitive ability
alpha<-0.1

# dispersal ability
m <- 2.607800





##########################################################

#functions
# here all all the functions used in simulations, main functions are denoted by three pound signs,
# helper functions by two pounds.

### seed production function
# Beverton-Holt model of density dependent growth, modified to consider all individuals in a patch 
# as competitors, but only the relevant subpopulation for growth rate
# computed for each strategy population j across the landscape of n patches (vectorized)

growth <- function(j,k,lambda){ 
  # j is number of individuals belonging to strategy j in patch x in 1:n, 
  # k is total population of all strategies in patch x in 1:n
  # lambda is reproductuve rate at low density of strategy j
  # first create a vector to store seeds 
  seeds<-vector()
  # compute stochastic Beverton-Holt population growth model
  seeds<-round((lambda*j)/(1+alpha*(k-1)),0)
  return(rpois(length(seeds),seeds))
}

### dispersal function 
# move seeds in patch x to patch y according to exponential dispersal kernel
# with random direction (forward & back)

disperse<-function(x,y,m){ 
  # x is the number of seeds in the patch to disperse
  # y is the row index of the patch in pair-wise distance matrix
  # m is the parameter defining the dispersal kernel
  # first define a vector to store dispersal distances
  pmove<-vector("numeric", patch)
  # if there are no seeds in patch x, return the empty vector
  if (x==0)
    return(pmove)
  # otherwise, compute distance and direction with helper functions below
  else
    move<-distance(x,m)*direction(x)
  for (i in 1:x){ 
    #this moves seeds to their new home, x is number of seeds, compute for each seed 
    for (j in 1:patch){ 
      #this finds the corresponding home for each seed 
      # place(move[i],pwdist[y,j],pmove[j])
      if (move[i]==pwdist[y,j])
      {pmove[j]<-pmove[j]+1}
    }
  }
  return(pmove) 
}

## helper function for disperse-- generate random distances, 
# parameters are from disperse function:
# x is the number of seeds in the patch
# m is the defined dispersal kernel parameter
distance<-function(x,m){
  # generate x random variates from exponental kernel with rate=m
  dist<-round(rexp(x,m),0)
  return(dist)
}

## helper function for disperse-- generate random directions,
# x is from disperse, number of seeds in patch
direction<-function(x){
  # draw x random variates from binomial distribution with p=0.5
  dir<-rbinom(x,1,0.5)
  # refactor results so that 0's return -1 to indicate negative direction
  # (disperse backwards/leftwards)
  sapply(dir,refactor)
}

## helper for direction-- refactor 0 to -1
refactor<-function(x){ 
  #  x is values from binomial distribution in direction
  if (x==0)
    x<- -1
  else x
}

### find the leading edge patch to record speed of advance

lemove<-function(x){ 
  # x is the row in spread matrix corresponding to time t
  # compute the 
  moved<-patch-pleft(rev(x))
  if (moved == 0){
    return(moved)
  }
  else{
    moved<-dvect[moved]
    return(moved) 
  }
  
}
#lemove helper how many patches left to colonize
pleft<-function(x){ 
  # x is from lemove, the row in spread matrix corresponding to time t
  # note we reverse the order so the first value in x corresponds to the right-most patch 
  # set index value y -- counts how far from the rightmost patch the leading edge is
  y<-0
  # for each patch, check if there is a non-zero population
  for (k in 1:patch){
    if (x[k] != 0){
      # halt once we reach the leading edge
      break
    }
    # otherwise, add one to the index
    else y<-y+1
  }
  return(y)
}

### calculate how many patches the leading edge moves each generation
# note this function assumes that only one patch is colonized at start
speed<- function(x,y){ 
  # x is vector of the leading edge patch each generation
  # y is the output vector, showing the number of patches the leading edge moved
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
##########################################################

# Initialize data structures


#Population Spread Matrix
#Adds values to each row for each generation
#Coloumns correspond to patches
spread1<-matrix(0,tmax,patch)
spread1[1,1]<-N_0

#Seeds Dispersed
#matrix for storing seeds produced, resets each generation
seeds<-matrix(NA,strat,patch)

#matrix containing number of plants/seeds in each patch after dispersal
#rows are originating patches where seeds were produced,
#coloumns are where they disperse to
seedsd1<-matrix(0,patch,patch)

# Pairwise Distances Matrix for computing dispersal
# Distances between patch i,j for row i 
# and coloumn j
pwdist<-matrix(NA,patch,patch)
for (i in 1:patch){
  for (j in 1:patch){
    dvect<-1:patch
    pwdist[i,j]<-dvect[j]-dvect[i]
  }
}

#matrix for storing data from the last generation of spread 
tmax_pop<-matrix(NA,nsim,patch)
#Vector for the extent of each strategy
ledge<-vector("numeric",strat)


# vectors for storing colonized patches
pcol<-matrix(NA, nrow=strat+1, ncol=tmax-1)
tcol<-matrix(NA,nrow=nsim,ncol=tmax-1)

####################################################################

#Run Simulation 
for (x in 1:nsim){
  for (i in 1:(tmax-1)){
    
    #produce seeds for each strategy in each patch
    seeds[1,]<-growth(spread1[i,],spread1[i,],lambda) 
    
    for (j in 1:patch){  
      
      #disperse seeds for each strategy in each patch
      seedsd1[j,]<-disperse(seeds[1,j],j,m[1])
      
      #fill in next row in population matrix
      spread1[i+1,]<-apply(seedsd1,2,sum)  
      
      #calculate how many patches the leading edge has advanced
      pcol[1,i]<-lemove(spread1[i+1,])
    }
    
    #fill in population matrix for last generation 
    tmax_pop[x,]<-spread1[tmax,]
    
    for (i in 1:strat){
      # calculate the leading edge patch for each strategy
      # at the last generation
      ledge[i]<-lemove(tmax_pop[i,])
    }}
    
    # save extent and speed matrices to CSV
    write.csv(spread1,file=paste0("spread187_memp_lcomp_wholepot",x,".csv"))
    write.csv(pcol,file=paste0("leading187_memp_lcomp_wholepot",x,".csv"))
}
write.csv(tmax_pop,file=paste0("lastgens187_memp_lcomp_wholepot.csv"))
 

