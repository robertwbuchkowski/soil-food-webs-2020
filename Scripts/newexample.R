# Simple example food web simulation 

# R version 4.0.2 (2020-06-22) -- "Taking Off Again"

library(diagram) # version 1.6.4
library(tidyverse) # version 1.3.0
verbose = F
runthesims = T

# Load functions
source("Scripts/functions.R")

# Food webs and combined IDs ----

foodwebs = c(list(
  #1
  c(0,1,1,0,
    0,0,0,1,
    0,0,0,1,
    0,0,0,0),
  
  c(0,1,1,0,
    0,0,1,1,
    0,0,0,1,
    0,0,0,0),
  #3 
  c(0,1,0,0,0,0,0,0,
    0,0,1,1,0,0,0,0,
    0,0,0,0,1,0,0,0,
    0,0,0,0,1,0,0,0,
    0,0,0,0,0,1,1,0,
    0,0,0,0,0,0,0,1,
    0,0,0,0,0,0,0,1,
    0,0,0,0,0,0,0,0),
  
  c(0,1,1,1,0,0,0,0,
    0,0,0,0,1,1,0,0,
    0,0,0,0,1,1,0,0,
    0,0,0,0,0,0,1,0,
    0,0,0,0,0,0,0,1,
    0,0,0,0,0,0,0,1,
    0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0),
  #5 
  c(0,0,1,0,0,0,0,0,0,0,0,0,
    0,0,0,1,1,0,0,0,0,0,0,0,
    0,0,0,0,0,1,0,0,0,0,0,0,
    0,0,0,0,0,1,0,0,0,0,0,0,
    0,0,0,0,0,0,0,1,0,0,0,0,
    0,0,0,0,0,0,0,0,1,1,0,0,
    0,0,0,0,0,0,0,0,1,1,0,0,
    0,0,0,0,0,0,0,0,0,0,1,0,
    0,0,0,0,0,0,0,0,0,0,1,1,
    0,0,0,0,0,0,0,0,0,0,1,1,
    0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0),
  
  c(0,0,1,1,1,0,0,0,0,0,0,0,
    0,0,0,1,1,0,0,0,0,0,0,0,
    0,0,0,0,0,1,0,0,0,0,0,0,
    0,0,0,0,0,1,0,0,0,0,0,0,
    0,0,0,0,0,0,0,1,0,0,0,0,
    0,0,0,0,0,0,0,0,1,1,0,0,
    0,0,0,0,0,0,0,0,1,1,0,0,
    0,0,0,0,0,0,0,0,0,0,1,0,
    0,0,0,0,0,0,0,0,0,0,1,1,
    0,0,0,0,0,0,0,0,0,0,1,1,
    0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0),
  
  #7
  c(0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
  
  c(0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,
    0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
  
  #9
  c(0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,1,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,1,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
  
  c(0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,1,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,1,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  
))

COMBLIST = vector(mode = "list", length = 10)

COMBLIST[[1]] = COMBLIST[[2]] = c("X2", "X3")

COMBLIST[[3]] = list(c("X2", "X3"), c("X5", "X6"))

COMBLIST[[4]] = list(c("X3", "X4"), c("X6", "X7"))

COMBLIST[[5]] = COMBLIST[[6]] = list(c("X3", "X4"), c("X6", "X7"),c("X9", "X10"))

COMBLIST[[7]] = list(c("X3", "X4"), c("X6", "X7"),c("X9", "X10"), c("X15", "X16"))

COMBLIST[[8]] = list(c("X4", "X5"), c("X8", "X9"),c("X10", "X11"), c("X15", "X16"))

COMBLIST[[9]] = COMBLIST[[10]] = list(c("X5", "X6"), c("X13", "X14"),c("X17", "X18"), c("X19", "X20"))

# General function ----

generalrun <- function(Xinput, param, Reps, FOODWEB, sortPARAM = T, combineID = c("X2","X3"), CANimm = c("X2","X3"), Ilist = T, RUNID = 1, OTHERSAME = T){
  
  if(!Ilist) CANimm = c(NA)
  
  Xa = Xb = Xc = vector(mode = "list", length = Reps*length(Xinput))
  
  Xselect = rep(seq(1,length(Xinput),1), each = Reps)
  
  NODES = sqrt(length(FOODWEB))
  
  IMAT = matrix(FOODWEB, ncol = NODES, nrow = NODES, byrow = T,
                dimnames = list(
                  paste0("X", seq(NODES,1,-1)),
                  paste0("X", seq(NODES,1,-1)))
                )
  
  for(ih in 1:(Reps*length(Xinput))){
    
    STABTEST = F
    TRIALS = 1
    
    while(!STABTEST){
      
      if(sortPARAM){
        comm1 = list(imat = IMAT,
        prop = data.frame(ID = paste0("X", seq(NODES,1,-1)),
                          d = c(sort(rlnorm(NODES, meanlog = log(0.5), sdlog = 0.35))),
                          a = c(runif(NODES, min = 0.3, max = 0.8)),
                          p = c(runif(NODES, min = 0.3, max = 0.5)),
                          B = c(sort(rlnorm(NODES, meanlog = log(5), sdlog = 0.35))),
                          CN = c(sort(rlnorm(NODES, meanlog = log(5), sdlog = 0.35))),
                          isDetritus = c(rep(0, NODES-1),1),
                          isPlant = c(rep(0, NODES)),
                          canIMM = c(rep(0, NODES)))
        )
      }else{
        comm1 = list(imat = IMAT,
                     prop = data.frame(ID = paste0("X", seq(NODES,1,-1)),
                                       d = c(rlnorm(NODES, meanlog = log(0.5), sdlog = 0.35)),
                                       a = c(runif(NODES, min = 0.3, max = 0.8)),
                                       p = c(runif(NODES, min = 0.3, max = 0.5)),
                                       B = c(rlnorm(NODES, meanlog = log(5), sdlog = 0.35)),
                                       CN = c(rlnorm(NODES, meanlog = log(5), sdlog = 0.35)),
                                       isDetritus = c(rep(0, NODES-1),1),
                                       isPlant = c(rep(0, NODES)),
                                       canIMM = c(rep(0, NODES)))
        )
      }
      
      # Reset detritus parameters to reasonable range
      comm1$prop[comm1$prop$isDetritus == 1,-1] = 
        c(0,1,1, 
          rlnorm(1, meanlog = log(10000), sdlog = 0.35),
          rlnorm(1, meanlog = log(30), sdlog = 0.35),
          1,0,0)
      
      # Reset assimilation efficiency for organisms that can immobilize N (i.e. microorganisms)
      comm1$prop[comm1$prop$ID %in% CANimm,"a"] = c(1,1)
      
      # Add the canIMM flags to the community
      if(all(!is.na(CANimm))){
        comm1$prop[comm1$prop$ID %in% CANimm,"canIMM"] = rep(1, length(CANimm))
      }

      if(param %in% c("CN", "a", "p", "B", "d")){
        if(OTHERSAME){
          comm1$prop[comm1$prop$ID == combineID[2], -1] = comm1$prop[comm1$prop$ID == combineID[1],-1]
        }
        comm1$prop[comm1$prop$ID == combineID[2], param] = comm1$prop[comm1$prop$ID == combineID[1],param] + Xinput[Xselect[ih]]
        
      }
      
      tempout = loopsubsets(comm1, STEPS = 2, MKPLOT = F, selectedCOMBO = T, selected2COMBO = list(combineID))
      
      if(stability(tempout$USINvec[[1]])$rmax < 0 & stability(tempout$USINvec[[2]])$rmax < 0) STABTEST = T
      
      TRIALS = TRIALS + 1
      
      if(TRIALS > 1000) STABTEST = T
      
    }
    
    Xa[[ih]] = cbind(tempout$different[[2]], WebN = ih)
    Xb[[ih]] = cbind(comm1$prop, WebN = ih,
                     Cu = sum(FOODWEB)/((NODES*NODES -NODES)/2))
    Xc[[ih]] = c(rmax1 = stability(tempout$USINvec[[1]])$rmax,rmax2 = stability(tempout$USINvec[[2]])$rmax, WebN = ih)
    
    if(ih %% 100 == 0) print(ih)
    
  }
  
  Xa = do.call("rbind",Xa)
  Xb = do.call("rbind",Xb)
  Xc = do.call("rbind",Xc)
  
  Xd = Xa %>% left_join(data.frame(Xc)) %>%
    mutate(stable = ifelse(rmax1 < 0 & rmax2 < 0, "Yes", "No"))
  
  Xd$RUNID = RUNID
  Xb$RUNID = RUNID
  
  return(list(Xd = Xd, Xb = Xb))
}

# Run the 70 original combinations ----

TORUN = expand.grid(PLIST = c("CN", "a", "p", "B", "d"),
                    FLIST = seq(1,9,2),
                    SLIST = T,
                    ILIST = F,
                    CLIST = seq(1,4,1))

dl = matrix(NA, nrow = length(foodwebs), ncol = 2)
for(i in 1:length(COMBLIST)){
  dl[i,1] = i
  dl[i,2] = ifelse(class(COMBLIST[[i]])=="list",length(COMBLIST[[i]]), 1)
  TORUN = TORUN[!(TORUN$FLIST == i & TORUN$CLIST > dl[i,2]),]
  
}

TORUN = merge(TORUN, data.frame(PLIST = c("CN", "a", "p", "B", "d"),
                                MAX = c(20,0.5,0.3,20,5)))

(NN = dim(TORUN)[1])

if(runthesims){
  
  tttttt = Sys.time()
  
  OUTPUTXd = vector(mode = "list", length = NN)
  OUTPUTXb = vector(mode = "list", length = NN)
  
  for(ijk in 1 :NN){
    
    if(class(COMBLIST[[TORUN[ijk,"FLIST"]]]) == "list"){
      CBL = COMBLIST[[TORUN[ijk,"FLIST"]]][[TORUN[ijk,"CLIST"]]]
      IBL = COMBLIST[[TORUN[ijk,"FLIST"]]][[1]]
    }else{
      CBL = COMBLIST[[TORUN[ijk,"FLIST"]]]
      IBL = COMBLIST[[TORUN[ijk,"FLIST"]]]
    }
    
    OUT = generalrun(Xinput = seq(0, TORUN[ijk,"MAX"], length = 4), 
                     param = as.character(TORUN[ijk,"PLIST"]), 
                     Reps = 1000, 
                     FOODWEB = foodwebs[[TORUN[ijk,"FLIST"]]], 
                     sortPARAM = TORUN[ijk,"SLIST"], 
                     combineID = CBL, 
                     CANimm = IBL,
                     Ilist = TORUN[ijk,"ILIST"],
                     RUNID = ijk,
                     OTHERSAME = T
    )
    
    OUTPUTXd[[ijk]] = OUT$Xd
    OUTPUTXb[[ijk]] = OUT$Xb
    print(paste("Done TORUN", ijk))
    
  }
  
  OUTPUTXd2 = do.call("rbind", OUTPUTXd)
  OUTPUTXb2 = do.call("rbind", OUTPUTXb)
  
  tttttt1 = Sys.time()
  
  tttttt1 - tttttt
  
  TORUN2 = TORUN
  
  TORUN2[,"RUNID"] = seq(1,dim(TORUN2)[1],1) 
  
  TORUN2[,"MAX"] = NULL
  
  testout = merge(OUTPUTXd2, TORUN2, by = "RUNID")
  
  (testout2 = with(testout, table(FLIST, CLIST, stable)))
  
  rm(testout, testout2)
  
  OUTPUTXd2 %>% write_rds("Data/OUTPUTXd2_large.rds")
  OUTPUTXb2 %>% write_rds("Data/OUTPUTXb2_large.rds")
  
  
}

# Read back in data if runthesims = F
if(!runthesims){
  OUTPUTXd2=read_rds("Data/OUTPUTXd2_large.rds")
  OUTPUTXb2=read_rds("Data/OUTPUTXb2_large.rds")
}

# Modify for plots ----

# Get the parameter change

TORUN2 = TORUN

TORUN2[,"RUNID"] = seq(1,dim(TORUN2)[1],1) 

TORUN2[,"MAX"] = NULL

combos = distinct(TORUN2[,c("FLIST", "CLIST")])
combos = combos[order(combos$FLIST, combos$CLIST),]
combos[,"clist"] = "A"
for(i in 1:dim(combos)[1]){
  
  if(class(COMBLIST[[combos$FLIST[i]]])=="list"){
    combos$clist[i] = paste0(COMBLIST[[combos$FLIST[i]]][[combos$CLIST[i]]], collapse = "")
  }else{
    combos$clist[i] = paste0(COMBLIST[[combos$FLIST[i]]], collapse = "")
  }
  
  
}

OUTPUTXb4 = OUTPUTXb2 %>% select(-isDetritus, -isPlant, -canIMM) %>% 
  gather(-ID, -WebN, - Cu, - RUNID, key = param, value = value) %>%
  spread(key = ID, value = value) %>%
  mutate(X2X3 = X2 - X3, X3X4 = X3 - X4, X5X6 = X5 - X6, X6X7 = X6 - X7, X9X10 = X9 - X10, X13X14 = X13 - X14, X15X16 = X15 - X16, X17X18 = X17-X18, X19X20 = X19-X20) %>%
  select(WebN, Cu, RUNID, param, X2X3: X19X20) %>%
  gather(-WebN, - Cu, - RUNID, -param, key = clist, value = DIFF) %>%
  filter(!is.na(DIFF)) %>%
  left_join(
    combos
  )

# Create allocation plot dataframe
OUTPUTXd3 = OUTPUTXd2 %>%
  filter(ID != "Net") %>%
  filter(stable == "Yes") %>%
  select(ID, WebN,RUNID, Nmin, Cmin, Nmin2, Cmin2) %>%
  mutate(dNmin = abs(Nmin2 - Nmin),
         dCmin = abs(Cmin2 - Cmin)) %>%
  group_by(WebN, RUNID) %>%
  summarize(dNmin = sum(dNmin), dCmin = sum(dCmin),
            Nmin = sum(Nmin), Cmin = sum(Cmin)) %>%
  mutate(dNmin = ifelse(Nmin > 1e-5, abs(dNmin/Nmin), 0), # Controls the cases where Nmin is zero or close to zero and therefore the metric dNmin/Nmin doesn't make sense.
         dCmin = abs(dCmin/Cmin)) %>% select(-Nmin, -Cmin)

plotdata1 = OUTPUTXd3 %>% 
  left_join(TORUN2 %>% rename(param = PLIST)) %>%
  left_join(OUTPUTXb4) %>%
  left_join(
    data.frame(FLIST = seq(1,10,1),
               Size = rep(c(4,8,12,16,20), each = 2))
  ) %>%
  left_join(
    data.frame(SLIST = c(T,F,T,F),
               ILIST = c(F,T,T,F),
               ISLIST = c("Sorted | Only min.",
                          "Unsorted | Imm.",
                          "Sorted | Imm.",
                          "Unsorted | Only min."))
  ) %>% mutate(CLIST = as.factor(CLIST)) %>% 
  mutate(DIFF = round(DIFF,2)) %>%
  mutate(Cu = round(Cu,2)) %>%
  mutate(dNmin = dNmin*100, dCmin = dCmin*100)

# Create overall plot dataframe 
OUTPUTXd4 = OUTPUTXd2 %>% 
  filter(stable == "Yes") %>% select(ID, dNmin, dCmin, WebN,RUNID) %>% filter(ID == "Net") %>%
  select(-ID) # Get just the Net change


plotdata2 = OUTPUTXd4 %>% 
  left_join(TORUN2 %>% rename(param = PLIST)) %>%
  left_join(OUTPUTXb4) %>%
  left_join(
    data.frame(FLIST = seq(1,10,1),
               Size = rep(c(4,8,12,16,20), each = 2))
  ) %>%
  left_join(
    data.frame(SLIST = c(T,F,T,F),
               ILIST = c(F,T,T,F),
               ISLIST = c("Sorted | Only min.",
                          "Unsorted | Imm.",
                          "Sorted | Imm.",
                          "Unsorted | Only min."))
  ) %>% mutate(CLIST = as.factor(CLIST))  %>%
  mutate(DIFF = round(DIFF,2)) %>%
  mutate(Cu = round(Cu,2)) %>%
  mutate(dNmin = dNmin*100, dCmin = dCmin*100)

# Plot the difference between the errors ----

tempforplot = OUTPUTXd2 %>% 
  filter(ID != "Net") %>%
  filter(stable == "Yes") %>%
  select(ID, WebN,RUNID, Nmin, Cmin, Nmin2, Cmin2) %>%
  mutate(dNminDIST = abs(Nmin2 - Nmin),
         dCminDIST = abs(Cmin2 - Cmin),
         dNmin = (Nmin2 - Nmin),
         dCmin = (Cmin2 - Cmin)) %>%
  group_by(WebN, RUNID) %>%
  summarize(dNmin = abs(sum(dNmin)), dCmin = abs(sum(dCmin)),
            dNminDIST = sum(dNminDIST), dCminDIST = sum(dCminDIST))

p1 = tempforplot %>%
  ggplot(aes(x = dNmin+0.001, y = dNminDIST+0.001)) + geom_point(alpha = 0.5) + geom_abline(intercept = 0, slope = 1, linetype = 2, col = "grey", size = 2) + theme_classic() + xlab("Net change in N. mineralization") +
  ylab("Change in N. mineralization distribution") +
  scale_x_log10() + scale_y_log10() +
  geom_abline(intercept = 1, slope = 1, linetype = 3, col = "grey", size = 2)

p2 = tempforplot %>%
  ggplot(aes(x = dCmin+0.001, y = dCminDIST+0.001)) + geom_point(alpha = 0.5) + geom_abline(intercept = 0, slope = 1, linetype = 2, col = "grey", size = 2) + theme_classic() + xlab("Net change in C. mineralization") +
  ylab("Change in C. mineralization distribution") +
  scale_x_log10() + scale_y_log10() +
  geom_abline(intercept = 1, slope = 1, linetype = 3, col = "grey", size = 2)

rm(tempforplot)

png(paste0("Plots/netVSdist_",Sys.Date(),".png"), width = 14, height = 8, units = "in", res = 300)
cowplot::plot_grid(p1,p2,labels = "AUTO", ncol = 2, nrow = 1)
dev.off()

# Plot the data cummulatively ----

# ...Plot one example across different parmaeter ranges ----

maxplotdata1 <- plotdata1 %>%
  left_join(
    data.frame(param = c("CN", "a", "p", "B", "d"),
               PARAMMAX = c(20,0.5,0.3,20,5))
  ) %>%
  mutate(DIFF2 = as.character(round(-100*DIFF/PARAMMAX,-1))) %>% 
  group_by(param, Size, CLIST) %>%
  summarize(max = max(dNmin))

cumplotdata1 = plotdata1 %>%
  mutate(dNmin = abs(round(dNmin))) %>%
  mutate(DIFF = round(DIFF, 2)) %>%
  group_by(DIFF, param, dNmin) %>%
  summarize(N = n()) %>%
  arrange(dNmin) %>%
  mutate(cumsum = cumsum(N)) %>%
  left_join( # ADD IN MAX NUMBER
    plotdata1 %>%
      mutate(dNmin = abs(round(dNmin))) %>%
      mutate(DIFF = round(DIFF, 2)) %>%
      group_by(DIFF, param, dNmin) %>%
      summarize(N = n()) %>%
      arrange(dNmin) %>%
      mutate(cumsum = cumsum(N)) %>%
      group_by(DIFF, param) %>%
      summarize(max = max(cumsum))
  ) %>%
  mutate(cumsum = cumsum/max) %>%
  ungroup() %>%
  left_join(
    data.frame(param = c("CN", "a", "p", "B", "d"),
               PARAMMAX = c(20,0.5,0.3,20,5))
  ) %>%
  mutate(DIFF = -100*DIFF/PARAMMAX)

cumplotdata2 = plotdata2 %>%
  mutate(dNmin = abs(round(dNmin))) %>%
  mutate(DIFF = round(DIFF, 2)) %>%
  group_by(DIFF, param, dNmin) %>%
  summarize(N = n()) %>%
  arrange(dNmin) %>%
  mutate(cumsum = cumsum(N)) %>%
  left_join( # ADD IN MAX NUMBER
    plotdata2 %>%
      mutate(dNmin = abs(round(dNmin))) %>%
      mutate(DIFF = round(DIFF, 2)) %>%
      group_by(DIFF, param, dNmin) %>%
      summarize(N = n()) %>%
      arrange(dNmin) %>%
      mutate(cumsum = cumsum(N)) %>%
      group_by(DIFF, param) %>%
      summarize(max = max(cumsum))
  ) %>%
  mutate(cumsum = cumsum/max) %>%
  ungroup() %>%
  left_join(
    data.frame(param = c("CN", "a", "p", "B", "d"),
               PARAMMAX = c(20,0.5,0.3,20,5))
  ) %>%
  mutate(DIFF = -100*DIFF/PARAMMAX)

cumplotdataCOMBO = cumplotdata1 %>%
  mutate(Change = "Distribution") %>%
  bind_rows(
    cumplotdata2 %>% mutate(Change = "Net")
  )

pdf(paste0("Plots/parameters_magnitude_",Sys.Date(),".pdf"), width = 14, height = 8)
cumplotdataCOMBO %>%
  ggplot(aes(x = dNmin, y = cumsum)) +
  geom_line(aes(group = paste0(DIFF, Change), color = DIFF, linetype = Change), size = 1.2) + 
  theme_classic() +
  theme(legend.key.width = unit(2.5,"cm")) + 
  theme(legend.position = c(1, 0),
        legend.justification = c(1, 0),
        legend.box = "vertical") +
  theme(text = element_text(size = 20),
        axis.text.y = element_text(margin = unit(c(t = 0, r = 3.5, b = 0, l = 0), "mm")),
        axis.text.x = element_text(margin = unit(c(t = 3.5, r = 0, b = 0, l = 0), "mm")),
        axis.ticks.length = unit(-0.25, "cm")) +
  scale_x_continuous(breaks = c(0,20,40)) +
  scale_y_continuous(breaks = c(0,0.5,1)) +
  coord_cartesian(xlim = c(0,50), ylim = c(0,1)) +
  ylab("Proportion of simulations") +
  xlab("Change in N. mineralization (%)") +
  scale_color_continuous(name = "Size of parameter difference \n (% of maximum)") +
  scale_linetype_discrete(name = "Type of Error") +
  facet_wrap(.~param, scales = "free",
             labeller = as_labeller(c(
               a = "Assimilation efficiency",
               B = "Biomass",
               CN = "C:N",
               d = "Death rate",
               p = "Production efficiency")))
dev.off()

png(paste0("Plots/full_data_sim1_",Sys.Date(),".png"), width = 14, height = 8, units = "in", res = 300)
plotdata1 %>%
  left_join(
    data.frame(param = c("CN", "a", "p", "B", "d"),
               PARAMMAX = c(20,0.5,0.3,20,5))
  ) %>%
  mutate(DIFF = round(-100*DIFF/PARAMMAX,digits = -1)) %>%
  ggplot(aes(x = as.factor(Size), y = dNmin, color = as.factor(DIFF)))  +
  ylab("Change in N. mineralization (%)") +
  xlab("Number of species") + 
  geom_boxplot() + theme_classic() + facet_grid(param~CLIST, scale = "free",labeller = as_labeller(c(
    a = "Assimilation efficiency",
    B = "Biomass",
    CN = "C:N",
    d = "Death rate",
    p = "Production efficiency",
    `1` = "Bottom (2)",
    `2` = "Middle 1 (3 - 4)",
    `3` = "Middle 2 (4 - 5)",
    `4` = "Top (4.5 - 6)"))) +
  scale_color_discrete(name = "Parameter difference \n (% of maximum)")
dev.off()


# Design annotation placement:

pdf(paste0("Plots/boxplot_size_",Sys.Date(),".pdf"), width = 14, height = 8)
plotdata1 %>%
  left_join(
    data.frame(param = c("CN", "a", "p", "B", "d"),
               PARAMMAX = c(20,0.5,0.3,20,5))
  ) %>%
  mutate(DIFF = round(-100*DIFF/PARAMMAX,digits = -1)) %>%
  filter(DIFF == 100) %>%
  ggplot(aes(x = as.factor(Size), y = dNmin)) +
  ylab("Change in N. mineralization (%)") +
  xlab("Number of species") + 
  geom_boxplot(aes(color = CLIST), position = position_dodge2(width = 1, preserve = "single")) + 
  theme_classic() + facet_wrap(~param, scale = "free",labeller = as_labeller(c(
    a = "Assimilation efficiency",
    B = "Biomass",
    CN = "C:N",
    d = "Death rate",
    p = "Production efficiency"))) +
  scale_color_manual(name = "Trophic level combined*", 
                     labels = c("2 (leftmost box)", "3 - 4 (second from the left)", "4 - 5 (third from the left)", "4.5 - 6 (fourth from the left)"), 
                    values = c("#E69F00", "#56B4E9", "#009E73", "#F0E442")) +
  # scale_linetype_manual(name = "Trophic level combined*", 
  #                    labels = c("2 (leftmost box)", "3 - 4 (second left)", "4 - 5 (third box)", "4.5 - 6 (fourth box)"), 
  #                    values = c(1,2,3,5)) +
  theme_classic() +
  theme(legend.key.width = unit(2.5,"cm")) + 
  theme(legend.position = c(1, 0),
        legend.justification = c(1, 0),
        legend.box = "vertical") +
  theme(text = element_text(size = 20),
        axis.text.y = element_text(margin = unit(c(t = 0, r = 3.5, b = 0, l = 0), "mm")),
        axis.text.x = element_text(margin = unit(c(t = 3.5, r = 0, b = 0, l = 0), "mm")),
        axis.ticks.length = unit(-0.25, "cm")) +
  
  geom_text(aes(label = labeltext),
            nudge_x = -0.3,
            data = 
              tibble(Size = as.factor(c(8,8,16,8,8)),
                     param = unique(plotdata1$param),
                     dNmin = c(26, 34, 950, 30,58),
                     labeltext = c(
                       "Assimilation efficiency\n matters more in BIGGER\n food webs and for HIGHER\n trophic levels
",
                       "Biomass matters when\n combining organisms with\n different diets",
                       "C:N ratio\n matters more in SMALLER\n food webs and for LOWER\n trophic levels",
                       "Death rate matters when\n combining organisms with\n different diets",
                       "Production efficiency\n matters more in BIGGER\n food webs and for HIGHER\n trophic levels"
                     ))) +
  geom_line(aes(group = GRP), 
            data =
              tibble(Size = as.factor(c(12,20,12,20,12,12,
                                        8,20,8,20,8,12)),
                     param = c("d","d","d","d","d","d", "B", "B", "B", "B", "B", "B"),
                     dNmin = c(30,10,30,25,30,15,
                               30,15,30,20,30,16),
                     GRP = rep(letters[1:6], each = 2)),
            arrow = grid::arrow(length = unit(0.2, "cm")))
dev.off()


tibble(Size = as.factor(c(12,20)),
       param = c("d","d"),
       dNmin = c(30,10),)

# ... Carbon plots ----

maxplotdata1 <- plotdata1 %>%
  left_join(
    data.frame(param = c("CN", "a", "p", "B", "d"),
               PARAMMAX = c(20,0.5,0.3,20,5))
  ) %>%
  mutate(DIFF2 = as.character(round(-100*DIFF/PARAMMAX,-1))) %>% 
  group_by(param, Size, CLIST) %>%
  summarize(max = max(dCmin))

cumplotdata1 = plotdata1 %>%
  mutate(dCmin = abs(round(dCmin))) %>%
  mutate(DIFF = round(DIFF, 2)) %>%
  group_by(DIFF, param, dCmin) %>%
  summarize(N = n()) %>%
  arrange(dCmin) %>%
  mutate(cumsum = cumsum(N)) %>%
  left_join( # ADD IN MAX NUMBER
    plotdata1 %>%
      mutate(dCmin = abs(round(dCmin))) %>%
      mutate(DIFF = round(DIFF, 2)) %>%
      group_by(DIFF, param, dCmin) %>%
      summarize(N = n()) %>%
      arrange(dCmin) %>%
      mutate(cumsum = cumsum(N)) %>%
      group_by(DIFF, param) %>%
      summarize(max = max(cumsum))
  ) %>%
  mutate(cumsum = cumsum/max) %>%
  ungroup() %>%
  left_join(
    data.frame(param = c("CN", "a", "p", "B", "d"),
               PARAMMAX = c(20,0.5,0.3,20,5))
  ) %>%
  mutate(DIFF = -100*DIFF/PARAMMAX)

cumplotdata2 = plotdata2 %>%
  mutate(dCmin = abs(round(dCmin))) %>%
  mutate(DIFF = round(DIFF, 2)) %>%
  group_by(DIFF, param, dCmin) %>%
  summarize(N = n()) %>%
  arrange(dCmin) %>%
  mutate(cumsum = cumsum(N)) %>%
  left_join( # ADD IN MAX NUMBER
    plotdata2 %>%
      mutate(dCmin = abs(round(dCmin))) %>%
      mutate(DIFF = round(DIFF, 2)) %>%
      group_by(DIFF, param, dCmin) %>%
      summarize(N = n()) %>%
      arrange(dCmin) %>%
      mutate(cumsum = cumsum(N)) %>%
      group_by(DIFF, param) %>%
      summarize(max = max(cumsum))
  ) %>%
  mutate(cumsum = cumsum/max) %>%
  ungroup() %>%
  left_join(
    data.frame(param = c("CN", "a", "p", "B", "d"),
               PARAMMAX = c(20,0.5,0.3,20,5))
  ) %>%
  mutate(DIFF = -100*DIFF/PARAMMAX)

cumplotdataCOMBO = cumplotdata1 %>%
  mutate(Change = "Distribution") %>%
  bind_rows(
    cumplotdata2 %>% mutate(Change = "Net")
  )

png(paste0("Plots/parameters_magnitude_carbon_",Sys.Date(),".png"), width = 14, height = 8, units = "in", res = 300)
cumplotdataCOMBO %>%
  ggplot(aes(x = dCmin, y = cumsum)) +
  geom_line(aes(group = paste0(DIFF, Change), color = DIFF, linetype = Change), size = 1.2) + 
  theme_classic() +
  theme(legend.key.width = unit(2.5,"cm")) + 
  theme(legend.position = c(1, 0),
        legend.justification = c(1, 0),
        legend.box = "horizontal") +
  coord_cartesian(xlim = c(0,50), ylim = c(0,1)) +
  ylab("Proportion of simulations") +
  xlab("Change in C. mineralization (%)") +
  scale_color_continuous(name = "Size of parameter difference \n (% of maximum)") +
  scale_linetype_discrete(name = "Type of Error") +
  facet_wrap(.~param, scales = "free",
             labeller = as_labeller(c(
               a = "Assimilation efficiency",
               B = "Biomass",
               CN = "C:N",
               d = "Death rate",
               p = "Production efficiency")))
dev.off()

png(paste0("Plots/full_data_sim1_carbon_",Sys.Date(),".png"), width = 14, height = 8, units = "in", res = 300)
plotdata1 %>%
  left_join(
    data.frame(param = c("CN", "a", "p", "B", "d"),
               PARAMMAX = c(20,0.5,0.3,20,5))
  ) %>%
  mutate(DIFF = round(-100*DIFF/PARAMMAX,digits = -1)) %>%
  ggplot(aes(x = as.factor(Size), y = dCmin, color = as.factor(DIFF)))  +
  ylab("Change in C. mineralization (%)") +
  xlab("Number of species") + 
  geom_boxplot() + theme_classic() + facet_grid(param~CLIST, scale = "free",labeller = as_labeller(c(
    a = "Assimilation efficiency",
    B = "Biomass",
    CN = "C:N",
    d = "Death rate",
    p = "Production efficiency",
    `1` = "Bottom (2)",
    `2` = "Middle 1 (3 - 4)",
    `3` = "Middle 2 (4 - 5)",
    `4` = "Top (4.5 - 6)"))) +
  scale_color_discrete(name = "Parameter difference \n (% of maximum)")
dev.off()

# Run ISLIT comparisons -----

TORUN = expand.grid(PLIST = c("none"),
                    FLIST = 9,
                    SLIST = c(T,F),
                    ILIST = c(T,F),
                    CLIST = c(1,2,3,4))

dim(TORUN)

NN = dim(TORUN)[1]

tttttt = Sys.time()
if(runthesims){
  OUTPUTXd = vector(mode = "list", length = NN)
  OUTPUTXb = vector(mode = "list", length = NN)
  
  for(ijk in 1 :NN){
    
    if(class(COMBLIST[[TORUN[ijk,"FLIST"]]]) == "list"){
      CBL = COMBLIST[[TORUN[ijk,"FLIST"]]][[TORUN[ijk,"CLIST"]]]
      IBL = COMBLIST[[TORUN[ijk,"FLIST"]]][[1]]
    }else{
      CBL = COMBLIST[[TORUN[ijk,"FLIST"]]]
      IBL = COMBLIST[[TORUN[ijk,"FLIST"]]]
    }
    
    OUT = generalrun(Xinput = NA, 
                     param = as.character(TORUN[ijk,"PLIST"]), 
                     Reps = 1000, 
                     FOODWEB = foodwebs[[TORUN[ijk,"FLIST"]]], 
                     sortPARAM = TORUN[ijk,"SLIST"], 
                     combineID = CBL, 
                     CANimm = IBL,
                     Ilist = TORUN[ijk,"ILIST"],
                     RUNID = ijk
    )
    
    OUTPUTXd[[ijk]] = OUT$Xd
    OUTPUTXb[[ijk]] = OUT$Xb
    print(paste("Done TORUN", ijk))
    
  }
  
  OUTPUTXd2 = do.call("rbind", OUTPUTXd)
  OUTPUTXb2 = do.call("rbind", OUTPUTXb)
  
}
tttttt1 = Sys.time()

tttttt1 - tttttt

TORUN2 = TORUN

TORUN2[,"RUNID"] = seq(1,dim(TORUN2)[1],1) 


testout = merge(OUTPUTXd2, TORUN2, by = "RUNID")
with(testout, table(FLIST, CLIST, stable))

# Plots 

combos = distinct(TORUN2[,c("FLIST", "CLIST")])
combos = combos[order(combos$FLIST, combos$CLIST),]
combos[,"clist"] = "A"
for(i in 1:dim(combos)[1]){
  
  if(class(COMBLIST[[combos$FLIST[i]]])=="list"){
    combos$clist[i] = paste0(COMBLIST[[combos$FLIST[i]]][[combos$CLIST[i]]], collapse = "")
  }else{
    combos$clist[i] = paste0(COMBLIST[[combos$FLIST[i]]], collapse = "")
  }
  
  
}

OUTPUTXb4 = OUTPUTXb2 %>% select(-isDetritus, -isPlant) %>% gather(-ID, -WebN, - Cu, - RUNID, key = param, value = value) %>%
  spread(key = ID, value = value) %>%
  mutate(X2X3 = X2 - X3, X3X4 = X3 - X4, X5X6 = X5 - X6, X6X7 = X6 - X7, X9X10 = X9 - X10, X13X14 = X13 - X14, X15X16 = X15 - X16, X17X18 = X17-X18, X19X20 = X19-X20) %>%
  select(WebN, Cu, RUNID, param, X2X3: X19X20) %>%
  gather(-WebN, - Cu, - RUNID, -param, key = clist, value = DIFF) %>%
  filter(!is.na(DIFF)) %>%
  left_join(
    combos
  )

# Create allocation plot dataframe
plotdata1 = OUTPUTXd2 %>%
  filter(ID != "Net") %>%
  filter(stable == "Yes") %>%
  select(ID, WebN,RUNID, Nmin, Cmin, Nmin2, Cmin2) %>%
  mutate(dNmin = abs(Nmin2 - Nmin),
         dCmin = abs(Cmin2 - Cmin)) %>%
  group_by(WebN, RUNID) %>%
  summarize(dNmin = sum(dNmin), dCmin = sum(dCmin),
            Nmin = sum(Nmin), Cmin = sum(Cmin)) %>%
  mutate(dNmin = abs(dNmin/Nmin),
         dCmin = abs(dCmin/Cmin)) %>% select(-Nmin, -Cmin) %>% 
  left_join(TORUN2 %>% rename(param = PLIST)) %>%
  mutate(dNmin = dNmin*100, dCmin = dCmin*100)

# Create overall plot dataframe 
OUTPUTXd4 = OUTPUTXd2 %>% 
  filter(stable == "Yes") %>% select(ID, dNmin, dCmin, WebN,RUNID) %>% filter(ID == "Net") %>%
  select(-ID) # Get just the Net change


plotdata2 = OUTPUTXd2 %>%
  filter(ID == "Net") %>%
  filter(stable == "Yes") %>%
  select(ID, WebN,RUNID, Nmin, Cmin, Nmin2, Cmin2) %>%
  mutate(dNmin = abs(Nmin2 - Nmin),
         dCmin = abs(Cmin2 - Cmin)) %>%
  group_by(WebN, RUNID) %>%
  summarize(dNmin = sum(dNmin), dCmin = sum(dCmin),
            Nmin = sum(Nmin), Cmin = sum(Cmin)) %>%
  mutate(dNmin = abs(dNmin/Nmin),
         dCmin = abs(dCmin/Cmin)) %>% select(-Nmin, -Cmin) %>% 
  left_join(TORUN2 %>% rename(param = PLIST)) %>%
  mutate(dNmin = dNmin*100, dCmin = dCmin*100)

cumplotdata1 = plotdata1 %>%
  mutate(dNmin = abs(round(dNmin))) %>%
  group_by(SLIST, ILIST, dNmin) %>%
  summarize(N = n()) %>%
  arrange(dNmin) %>%
  mutate(cumsum = cumsum(N)) %>%
  left_join( # ADD IN MAX NUMBER
    plotdata1 %>%
      mutate(dNmin = abs(round(dNmin))) %>%
      group_by(SLIST, ILIST, dNmin) %>%
      summarize(N = n()) %>%
      arrange(dNmin) %>%
      mutate(cumsum = cumsum(N)) %>%
      group_by(SLIST, ILIST) %>%
      summarize(max = max(cumsum))
  ) %>%
  mutate(cumsum = cumsum/max) %>%
  ungroup()

cumplotdata2 = plotdata2 %>%
  mutate(dNmin = abs(round(dNmin))) %>%
  group_by(SLIST, ILIST, dNmin) %>%
  summarize(N = n()) %>%
  arrange(dNmin) %>%
  mutate(cumsum = cumsum(N)) %>%
  left_join( # ADD IN MAX NUMBER
    plotdata1 %>%
      mutate(dNmin = abs(round(dNmin))) %>%
      group_by(SLIST, ILIST, dNmin) %>%
      summarize(N = n()) %>%
      arrange(dNmin) %>%
      mutate(cumsum = cumsum(N)) %>%
      group_by(SLIST, ILIST) %>%
      summarize(max = max(cumsum))
  ) %>%
  mutate(cumsum = cumsum/max) %>%
  ungroup()

cumplotdataCOMBO = cumplotdata1 %>%
  mutate(Change = "Distribution") %>%
  bind_rows(
    cumplotdata2 %>% mutate(Change = "Net")
  )

cumplotdataCOMBO_IS = cumplotdataCOMBO

cumplotdataCOMBO_IS %>% write_rds("Data/cumplotdataCOMBO_IS.rds")

pISCOMBO = cumplotdataCOMBO_IS %>%
  filter(Change =="Distribution") %>%
  ggplot(aes(x = dNmin, y = cumsum)) +
  geom_line(aes(group = paste0(SLIST, ILIST, Change), color = SLIST, linetype = ILIST), size = 1.2) + 
  theme_classic() +
  theme(legend.key.width = unit(2.5,"cm")) +
  theme(legend.position = c(1, 0.1),
        legend.justification = c(1, 0.1),
        legend.box = "horizontal") +
  coord_cartesian(xlim = c(0,100), ylim = c(0,1)) +
  ylab("Proportion of simulations") +
  xlab("Change in N. mineralization (%)") +
  scale_color_manual(name = "Sorted parameters",
                     values = c("darkblue", "orange1")) +
  scale_linetype_manual(name = "Immobilization",
                        values = c(3,5)) +
  theme(text = element_text(size = 20),
        axis.text.y = element_text(margin = unit(c(t = 0, r = 3.5, b = 0, l = 0), "mm")),
        axis.text.x = element_text(margin = unit(c(t = 3.5, r = 0, b = 0, l = 0), "mm")),
        axis.ticks.length = unit(-0.25, "cm"))  +
  scale_x_continuous(breaks = c(0,50,100))+
  scale_y_continuous(breaks = c(0,0.5,1))

png(paste0("Plots/IScombos_",Sys.Date(),".png"), width = 6, height = 4, units = "in", res = 300)
pISCOMBO
dev.off()

# Run Connectance-type comparisons -----

TORUN = expand.grid(PLIST = c("none"),
                    FLIST = seq(1,10,1),
                    SLIST = T,
                    ILIST = F,
                    CLIST = c(1,2,3,4))

dl = matrix(NA, nrow = length(foodwebs), ncol = 2)
for(i in 1:length(COMBLIST)){
  dl[i,1] = i
  dl[i,2] = ifelse(class(COMBLIST[[i]])=="list",length(COMBLIST[[i]]), 1)
  TORUN = TORUN[!(TORUN$FLIST == i & TORUN$CLIST > dl[i,2]),]
  
}

dim(TORUN)

NN = dim(TORUN)[1]

tttttt = Sys.time()
if(runthesims){
  OUTPUTXd = vector(mode = "list", length = NN)
  OUTPUTXb = vector(mode = "list", length = NN)
  
  for(ijk in 1 :NN){
    
    if(class(COMBLIST[[TORUN[ijk,"FLIST"]]]) == "list"){
      CBL = COMBLIST[[TORUN[ijk,"FLIST"]]][[TORUN[ijk,"CLIST"]]]
      IBL = COMBLIST[[TORUN[ijk,"FLIST"]]][[1]]
    }else{
      CBL = COMBLIST[[TORUN[ijk,"FLIST"]]]
      IBL = COMBLIST[[TORUN[ijk,"FLIST"]]]
    }
    
    OUT = generalrun(Xinput = NA, 
                     param = as.character(TORUN[ijk,"PLIST"]), 
                     Reps = 1000, 
                     FOODWEB = foodwebs[[TORUN[ijk,"FLIST"]]], 
                     sortPARAM = TORUN[ijk,"SLIST"], 
                     combineID = CBL, 
                     CANimm = IBL,
                     Ilist = TORUN[ijk,"ILIST"],
                     RUNID = ijk
    )
    
    OUTPUTXd[[ijk]] = OUT$Xd
    OUTPUTXb[[ijk]] = OUT$Xb
    print(paste("Done TORUN", ijk))
    
  }
  
  OUTPUTXd2 = do.call("rbind", OUTPUTXd)
  OUTPUTXb2 = do.call("rbind", OUTPUTXb)
  
}
tttttt1 = Sys.time()

tttttt1 - tttttt

TORUN2 = TORUN

TORUN2[,"RUNID"] = seq(1,dim(TORUN2)[1],1) 

TORUN2[,"MAX"] = NULL


# Plots 

combos = distinct(TORUN2[,c("FLIST", "CLIST")])
combos = combos[order(combos$FLIST, combos$CLIST),]
combos[,"clist"] = "A"
for(i in 1:dim(combos)[1]){
  
  if(class(COMBLIST[[combos$FLIST[i]]])=="list"){
    combos$clist[i] = paste0(COMBLIST[[combos$FLIST[i]]][[combos$CLIST[i]]], collapse = "")
  }else{
    combos$clist[i] = paste0(COMBLIST[[combos$FLIST[i]]], collapse = "")
  }
  
  
}

OUTPUTXb4 = OUTPUTXb2 %>% select(RUNID, Cu) %>% distinct()

# Create allocation plot dataframe
plotdata1 = OUTPUTXd2 %>%
  filter(ID != "Net") %>%
  filter(stable == "Yes") %>%
  select(ID, WebN,RUNID, Nmin, Cmin, Nmin2, Cmin2) %>%
  mutate(dNmin = abs(Nmin2 - Nmin),
         dCmin = abs(Cmin2 - Cmin)) %>%
  group_by(WebN, RUNID) %>%
  summarize(dNmin = sum(dNmin), dCmin = sum(dCmin),
            Nmin = sum(Nmin), Cmin = sum(Cmin)) %>%
  mutate(dNmin = abs(dNmin/Nmin),
         dCmin = abs(dCmin/Cmin)) %>% select(-Nmin, -Cmin) %>% 
  left_join(TORUN2 %>% rename(param = PLIST)) %>%
  mutate(dNmin = dNmin*100, dCmin = dCmin*100) %>%
  left_join(
    data.frame(FLIST = seq(1,10,1),
               Size = rep(c(4,8,12,16,20), each = 2))
  ) %>%
  left_join(OUTPUTXb4) %>%
  left_join(combos)

plotdata2 = OUTPUTXd2 %>%
  filter(ID == "Net") %>%
  filter(stable == "Yes") %>%
  select(ID, WebN,RUNID, Nmin, Cmin, Nmin2, Cmin2) %>%
  mutate(dNmin = abs(Nmin2 - Nmin),
         dCmin = abs(Cmin2 - Cmin)) %>%
  group_by(WebN, RUNID) %>%
  summarize(dNmin = sum(dNmin), dCmin = sum(dCmin),
            Nmin = sum(Nmin), Cmin = sum(Cmin)) %>%
  mutate(dNmin = abs(dNmin/Nmin),
         dCmin = abs(dCmin/Cmin)) %>% select(-Nmin, -Cmin) %>% 
  left_join(TORUN2 %>% rename(param = PLIST)) %>%
  mutate(dNmin = dNmin*100, dCmin = dCmin*100) %>%
  left_join(
    data.frame(FLIST = seq(1,10,1),
               Size = rep(c(4,8,12,16,20), each = 2))
  ) %>%
  left_join(OUTPUTXb4) %>%
  left_join(combos)

cumplotdata1 = plotdata1 %>%
  mutate(dNmin = abs(round(dNmin))) %>%
  group_by(Size,Cu, dNmin) %>%
  summarize(N = n()) %>%
  arrange(dNmin) %>%
  mutate(cumsum = cumsum(N)) %>%
  left_join( # ADD IN MAX NUMBER
    plotdata1 %>%
      mutate(dNmin = abs(round(dNmin))) %>%
      group_by(Size,Cu, dNmin) %>%
      summarize(N = n()) %>%
      arrange(dNmin) %>%
      mutate(cumsum = cumsum(N)) %>%
      group_by(Size,Cu) %>%
      summarize(max = max(cumsum))
  ) %>%
  mutate(cumsum = cumsum/max) %>%
  ungroup()

cumplotdata2 = plotdata2 %>%
  mutate(dNmin = abs(round(dNmin))) %>%
  group_by(Size,Cu, dNmin) %>%
  summarize(N = n()) %>%
  arrange(dNmin) %>%
  mutate(cumsum = cumsum(N)) %>%
  left_join( # ADD IN MAX NUMBER
    plotdata1 %>%
      mutate(dNmin = abs(round(dNmin))) %>%
      group_by(Size,Cu, dNmin) %>%
      summarize(N = n()) %>%
      arrange(dNmin) %>%
      mutate(cumsum = cumsum(N)) %>%
      group_by(Size,Cu) %>%
      summarize(max = max(cumsum))
  ) %>%
  mutate(cumsum = cumsum/max) %>%
  ungroup()

cumplotdataCOMBO = cumplotdata1 %>%
  mutate(Change = "Distribution") %>%
  bind_rows(
    cumplotdata2 %>% mutate(Change = "Net")
  )

cumplotdataCOMBO %>% write_rds("Data/cumplotdataCOMBO_Cu.rds")

pconnect = cumplotdataCOMBO %>%
  filter(Change =="Distribution") %>%
  mutate(Size = as.factor(Size)) %>%
  left_join(
    cumplotdataCOMBO %>% select(Size, Cu) %>% distinct() %>% 
      left_join(
        cumplotdataCOMBO %>% select(Size, Cu) %>% distinct() %>% group_by(Size) %>% summarise(max = max(Cu))
      ) %>%
      mutate(LCU = ifelse(max == Cu, "Higher", "Lower")) %>%
      select(Size, Cu, LCU) %>% mutate(Size = as.factor(Size))
  ) %>%
  ggplot(aes(x = dNmin, y = cumsum)) +
  geom_line(aes(group = paste0(Size,LCU, Change), color = Size, linetype = LCU), size = 1.2) + 
  theme_classic() +
  theme(legend.key.width = unit(2.5,"cm")) +
  theme(legend.position = c(1, 0.1),
        legend.justification = c(1, 0.1),
        legend.box = "horizontal") +
  coord_cartesian(xlim = c(0,25), ylim = c(0,1)) +
  ylab("Proportion of simulations") +
  xlab("Change in N. mineralization (%)") +
  scale_color_manual(name = "Food web size",values = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00")) +
  scale_linetype_discrete(name = "Connectance") +
  theme(text = element_text(size = 20),
        axis.text.y = element_text(margin = unit(c(t = 0, r = 3.5, b = 0, l = 0), "mm")),
        axis.text.x = element_text(margin = unit(c(t = 3.5, r = 0, b = 0, l = 0), "mm")),
        axis.ticks.length = unit(-0.25, "cm")) +
  scale_x_continuous(breaks = c(0,10,20))+
  scale_y_continuous(breaks = c(0,0.5,1))

png(paste0("Plots/SizeCu_",Sys.Date(),".png"), width = 8, height = 5, units = "in", res = 300)
pconnect
dev.off()

# Combine connect and ISCOMBO plots
pdf(paste0("Plots/SizeCuISCOMBO_",Sys.Date(),".pdf"), width = 13, height = 5)
ggpubr::ggarrange(pISCOMBO, pconnect, labels = "AUTO")
dev.off()

