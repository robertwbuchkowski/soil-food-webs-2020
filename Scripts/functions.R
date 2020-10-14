# Functions for running our analysis to be loaded via source code

# R version 4.0.2 (2020-06-22) -- "Taking Off Again"

# ------- Function to calculate Trophic level ------- #
# 1. Borrowed from the R package Cheddar: Calculates the trophic level
TLcheddar <- function(W){
  # Make each row sum to one.
  rs <- rowSums(W)
  W <- W / matrix(rs, ncol=ncol(W), nrow=nrow(W))
  W[0==rs,] <- 0      # Fix NA resulting from div by zero
  
  # Identity matrix
  I <- diag(ncol(W))
  
  # Invert the matrix. From Rich 2011-08-17: "If this matrix inversion 
  # fails, there is an important problem with the network topology. 
  # For a food web to be energetically feasible, every node must be 
  # connected to a basal node. When the inversion fails it is because 
  # there is at least one node that has no connection to a basal node."
  result <- tryCatch(solve(I-W), error = function(e) e)
  
  if('error' %in% class(result))
  {
    tl <- rep(NA, ncol(W))
    names(tl) <- colnames(W)
  }
  else
  {
    # Returned vector is named by node
    tl <- rowSums(result)   # Isolated nodes have tl of 1
  }
  
  return (tl)
}

# 2. Function to sort trophic levels hiarchically
TLsort <- function(usin){
  imat = usin$imat # row values of imat sets predator feeding preferences!
  prop = usin$prop
  
  Nnodes = dim(imat)[1] # Number of nodes in the food web
  
  # Calculate the trophic level 
  TL = seq(Nnodes,1,-1)
  names(TL) = colnames(imat)
  
  TL_temp = TLcheddar(imat)
  
  if(!(any(is.na(TL_temp)))){
    TL = TL_temp
  }else{
    print("Error in TL sorting: usually comes from a poorly defined matrix where feeding relationships are not possible at equilibrium.")
  }
  
  rm(TL_temp)
  
  # Shuffle the trophic levels to make sure the calculations start at the top and continue downwards
  
  # First sort by trophic level to get the order close
  imat = imat[order(-TL),order(-TL)]
  
  # Then confirm that there are no cases where a low trophic level predator eats a higher trophic level prey
  positions <- 1:dim(imat)[1]
  rid = min(positions)
  while(rid <= max(positions)){
    predatorlist = imat[,rid] > 0
    if(sum(predatorlist) == 0){
      rid = rid + 1
    }else{
      max_predator = max(which(predatorlist))
      if(max_predator > rid){
        part1 = min(positions):max_predator
        if(max_predator == max(positions)){
          part2 = NULL
        }else{
          part2 = (max_predator+1):max(positions)
        }
        new_order = c(part1[part1 != rid], rid, part2[part2 != rid])
        imat = imat[new_order,new_order]
      }else{
        rid = rid + 1
      }
    }
  }
  
  # Order of the colnames:
  prop = prop[match(colnames(imat), prop$ID),]
  
  stopifnot(all(colnames(imat) == prop$ID))
  
  return(list(imat = imat, prop = prop))
}

# 3. Function to calculate the nitrogen surplus or deficit each species gets from consuming another species
Aijfcn <- function(usin){
  tt = comana(usin, shuffleTL = F)
  FMAT = tt$fmat
  Nnodes = dim(FMAT)[1]
  FMAT2 = FMAT
  FMAT2[FMAT2>0] = 1
  
  Aij = (matrix(1/usin$prop$CN, nrow = Nnodes, ncol = Nnodes, byrow = T) - matrix(usin$prop$p/usin$prop$CN, nrow=Nnodes, ncol = Nnodes))*matrix(usin$prop$a, nrow=Nnodes, ncol = Nnodes)*FMAT2
  
  return(Aij)
}

# 4. A utility function to convert an imat to proportion of total diet
adjust_imat_percent <- function(imat){
  for(i in 1:dim(imat)[1]){
    XX = ifelse(sum(imat[i,]) ==0, 1, sum(imat[i,]))
    imat[i,] = imat[i,]/XX
  }
  return(imat)
}

# 5. A utility function to convert imat proportion to diet scaled to 1
adjust_imat_ratio <- function(imat){
  for(i in 1:dim(imat)[1]){
    if(sum(imat[i,])> 0){
      imat[i,] = imat[i,]/min(imat[i,imat[i,]!= 0])
    }
  }
  return(imat)
}

# 6. A utility function to calculate the cij
Cijfcn <- function(usin){ # Function only requires the community inputs
  
  imat = usin$imat
  prop = usin$prop
  
  Nnodes = dim(imat)[1] # Number of nodes
  
  Bpred = matrix(prop$B, ncol = Nnodes, nrow = Nnodes) # A matrix of predators
  Bprey = matrix(prop$B, ncol = Nnodes, nrow = Nnodes, byrow = T) # A matirx of prey
  fmat = comana(usin, shuffleTL = F)$fmat # Get the consumption matrix (units = gC/ time)
  cij = fmat/(Bpred*Bprey) # Get the consumption rate matrix (units 1/ (gC * time))
  return(cij) # Return the consumption rates (gC^-1 time^-1)
}

# 7. Function to analyze the food web
comana <- function(usin, # The community that you are analyzing: contains a matrix of interactions and a data frame of properties in a list.
                   mkplot = F, # Should the plots be output 
                   whattoplot = c("web","Nmin","Cmin"), # A vector of what to plot. Food web typology, nitrogen mineralization, and/or carbon mineralization
                   outsideplotctrl = F, # Should this function create a new plot (F) or do you want it to add plots to the existing device (T)
                   showCN = F, # Should the food web show the C:N ratio of each trophic species next to it's name in the food web plot
                   BOX.SIZE = 0.1, # Size of boxes in the food wen plot
                   BOX.PROP = 0.3, # Proportion of box length and width in the food wen plot
                   BOX.CEX = 1,  # Size of box text in the food web plot
                   PLOT.CEX = 1,
                   edgepos = c(0.1, 0.9), # Where to put the far left and far right boxes for trophic species in the food web plot (range = 0, 1)
                   shuffleTL = T, # Should the function correct the trophic level order in the input data? The order must be correct for this function to work, but sometimes you know the order has already been fixed.
                   prettynames = NA # Alternative names in order for the food web plot trophic species. Cannot be used if showCN = T.
){
  
  # Calculate the trophic level 
  if(shuffleTL) usin = TLsort(usin)
  
  imat = usin$imat # row values of imat sets predator feeding preferences!
  prop = usin$prop # properties of each trophic species
  
  # Save the trophic levels for later use
  TL = TLcheddar(imat)
  TL2 = TL[order(-TL)]
  
  Nnodes = dim(imat)[1] # Number of nodes in the food web
  
  if(any(prop$isDetritus >0)){
    # Make sure TL of detritus is 1
    stopifnot(all(TL[prop$isDetritus >0] ==1))
    
    # Detritus a and p are 1
    stopifnot(
      subset(prop, isDetritus >0)$a == 1 & subset(prop, isDetritus >0)$p == 1 & subset(prop, isDetritus >0)$d == 0
    )
    
    # Make sure detritus input proportion sums to 1
    prop$isDetritus = prop$isDetritus/sum(prop$isDetritus)
    
  }
  
  # Properties and matrix have the same individuals
  stopifnot(all(c(colnames(imat) == rownames(imat),colnames(imat) == prop$ID)))
  
  # Create a vector for the consumption rates  
  consumption = rep(NA, Nnodes)
  names(consumption) = colnames(imat) # Names match the trophic species names
  
  # Calculate the consumption of the first trophic species. This species has no predators and so the loss to predation is zero.
  consumption[1] = (prop$d[1]*prop$B[1] + 0)/(prop$a[1]*prop$p[1])
  
  # Create a new matrix for feeding rates with the same dimensions as the food web matrix
  fmat = imat
  
  # Calculate the feeding of the first trophic species. This is calculated using the biomass and values in the matrix imat to distribute the total consumption (in the vector) amoung the possible prey items.
  fmat[1,] = (imat[1,]*prop$B/sum(imat[1,]*prop$B))*consumption[1]
  
  # Repeat the above calculations for all other trophic species using a loop. Notice how the consumption calculations now include the loss to predators (sum(fmat[,i]))
  for(i in 2:Nnodes){
    
    consumption[i] = (prop$d[i]*prop$B[i] + sum(fmat[,i]))/(prop$a[i]*prop$p[i])
    if(consumption[i] < 0) browser()
    if(all(imat[i,]==0)){
      fmat[i,] = imat[i,]
    }else{
      fmat[i,] = (imat[i,]*prop$B/sum(imat[i,]*prop$B))*consumption[i]
    }
  }
  
  # Fix the detritus calculations: detritus recieves dead material from all other trophic levels and so consumption is the losses minus the inputs it already gets from inefficient eating and dead biomass. This value can be negative indicating that inputs from outside the ecosystem are necessary to meet internal demand for C.
  if(any(prop$isDetritus >0)){
    detritusPOS = which(prop$isDetritus >0)
    
    for(i in detritusPOS){
      consumption[i] = sum(fmat[,i]) - prop$isDetritus[i]*(sum((1-prop$a)*consumption) + sum(prop$d*prop$B))
    }
  }
  
  # Calculate carbon mineralization using the production efficiency
  Cmin = prop$a*(1-prop$p)*consumption
  
  # Calculate nitrogen mineralization as a matrix so that the nitrogen mineralized by each consumption interaction is recorded. Summing the rows of this matrix gives the nitrogen that is mineralized overall.
  Nmin = (matrix(1/prop$CN, nrow = Nnodes, ncol = Nnodes, byrow = T) - matrix(prop$p/prop$CN, nrow=Nnodes, ncol = Nnodes))*matrix(prop$a, nrow=Nnodes, ncol = Nnodes)*fmat
  
  # Make the plot if necessary
  if(mkplot){
    
    #Function to rescale the food web trophic levels so they stack nicely in the plot
    RESCALE <- function(invec, a = 0, b = 1){
      (b-a)*(invec - min(invec))/(max(invec)-min(invec)) + a
    }
    
    TL3 = round(TL2)
    # Position matrix
    posmat = matrix(NA, ncol = 2, nrow = Nnodes)
    posmat[,2] = RESCALE(TL2, a = 0.1, b=0.9)
    for(tl in sort(unique(TL3))){
      posmat[TL3 == tl,1] = seq(edgepos[1], edgepos[2], length = table(TL3)[as.character(tl)])
    }
    
    fmatlwd = fmat
    fmatlwd[fmatlwd!=0]  = log(fmatlwd[fmatlwd!=0])
    fmatlwd = RESCALE(fmat, a = 0.1, b = 4)
    
    if(!outsideplotctrl){
      if(length(whattoplot) >2){
        par(mfrow=c(2,2), mar = c(2,4,2,2)+0.1, tck = 0.2, cex = PLOT.CEX)
      }else{
        if(length(whattoplot) == 2){
          par(mfrow=c(1,2), mar = c(2,4,2,2)+0.1, tck = 0.2, cex = PLOT.CEX)
        }else{
          par(mfrow=c(1,1), mar = c(2,4,2,2)+0.1, tck = 0.2, cex = PLOT.CEX)
        }
      }
    }
    
    if("web" %in% whattoplot){
      
      if(showCN){
        namevec = paste0(colnames(imat), ": ", prop$CN)
      }else{
        if(is.na(prettynames[1])){
          namevec = colnames(imat)
        }else{
          namevec = prettynames
        }
        
      }
      
      plotmat(imat, pos = posmat,name = namevec, box.size = BOX.SIZE, shadow.size = 0, box.type = "rect", box.prop = BOX.PROP, arr.lwd = fmatlwd, cex.txt = 0, arr.length = 0.1, box.cex = BOX.CEX)
    }
    
    
    if("Nmin" %in% whattoplot){
      barplot(rowSums(Nmin), ylab = "N min.", names.arg = prop$ID)
    }
    
    if("Cmin" %in% whattoplot){
      barplot(Cmin, ylab = "C min.", names.arg = prop$ID)
    }
    
    
  }
  
  return(list(consumption = consumption, # consumption vector
              Cmin = Cmin, # carbon mineralization vector
              Nmin=Nmin, # nitrogen mineralization matrix
              fmat = fmat, # Feeding matrix
              usin =list(imat = imat, prop = prop))) # revised community with sorted trophic levels
}

# ------ A function to run stability analysis ------ #
# 8. The stability analysis assesses stability of the equilibirum using the principle eigenvalue of the Jacobian matrix. It is only valid near the equilibirum.

# I TESTED THIS FUNCTION FOR THE FOOD WEBS THAT I HAVE ALREADY RUN. ERRORS COULD ARISE WITH NEW FOOD WEB MOTIFS. PLEASE CHECK THE ANSWER BY CALCULTING STABILITY FOR SOME WEBS ANALYTICALLY!

stability <- function(usin){ # Function only requires the community inputs
  
  imat = usin$imat
  prop = usin$prop
  
  try(if(sum(prop$isPlant) > 1) warning("Function is only set up for 1 plant pool at the current time"))
  
  # Rescale isDetritus to sum to 1
  if(sum(prop$isDetritus) != 0 ){
    prop$isDetritus = prop$isDetritus/sum(prop$isDetritus)
  }
  
  Nnodes = dim(imat)[1] # Number of nodes
  
  Bpred = matrix(prop$B, ncol = Nnodes, nrow = Nnodes) # A matrix of predators
  Bprey = matrix(prop$B, ncol = Nnodes, nrow = Nnodes, byrow = T) # A matirx of prey
  fmat = comana(usin, shuffleTL = F)$fmat # Get the consumption matrix (units = gC/ time)
  cij = fmat/(Bpred*Bprey) # Get the consumption rate matrix (units 1/ (gC * time))
  
  D1 = rep(NA, dim(cij)[1]) # Create a vector that will be the diagonal of the Jacobian. This is the partial derivative of each trophic species equation w.r.t. the bioamss of that species
  for(ig in 1: dim(cij)[1]){
    D1[ig] = sum(prop$a[ig]*prop$p[ig]*cij[ig,]*Bprey[ig,]- # Deriative of consumption term of the focal species
                   cij[,ig]*Bpred[,ig]) - # Deritative of terms where the focal species is a prey item
      prop$d[ig] # Deriative of the death rate term
  }
  
  # Need to ID the basal trophic species and fix the terms if they are plants or detritus
  
  # If plant, add the plant growth rate so the growth rate is enough to balance losses
  if(any(prop$isPlant>0)){
    D1[prop$isPlant>0] = D1[prop$isPlant>0] +
      (comana(usin)$consumption/prop$B)[prop$isPlant>0]
  }
  
  # If detritus, the detritus equation has terms for the biomass recycling of all trophic species that eat detritus and recycle the unassimilated biomass back to that pool.
  detritusPOS = which(prop$isDetritus >0)
  
  if(any(prop$isDetritus>0)){
    
    for(DPOS in detritusPOS){
      D1[DPOS] = D1[DPOS]  + 
        prop$isDetritus[DPOS]*sum((1-prop$a)*cij[,DPOS]*prop$B)
    }
    
    
  }
  
  # Create a Jacobian of the same dimensions as the consumption matrix
  J = cij
  
  for(ii in 1:dim(J)[1]){ # Focus equation
    for(jj in 1:dim(J)[1]){ # derivative w.r.t.
      
      if(cij[ii,jj] >0 & cij[jj,ii] ==0) J[ii,jj] = prop$a[ii]*prop$p[ii]*cij[ii,jj]*prop$B[ii] # The derivative is based on consumption if the focal species is eaten by the species whose biomass is in the derivative
      
      if(cij[ii,jj] ==0 & cij[jj,ii] > 0) J[ii,jj] = -cij[jj,ii]*prop$B[ii] # The derivative is based on loss to predation is the focal equation is the prey and the derivative is take w.r.t. the predator biomass
      
      if(cij[ii,jj] > 0 & cij[jj,ii] > 0) J[ii,jj] = prop$a[ii]*prop$p[ii]*cij[ii,jj]*prop$B[ii] - cij[jj,ii]*prop$B[ii] # Both terms are present for species that feed on each other.
    }
  }
  
  diag(J) = D1 # Replace the diagonal with the one calculated above
  
  # Fix the detritus calculations for the derivatives associated with detritus
  if(any(prop$isDetritus >0)){
    
    for(DPOS in detritusPOS){
      
      all_other_pools = 1:Nnodes
      all_other_pools = all_other_pools[all_other_pools != DPOS]
      J[DPOS,all_other_pools] = 
        J[DPOS,all_other_pools] +
        
        prop$isDetritus[DPOS]*(
          prop$d[all_other_pools] + # Death rates
            
            rowSums(cij[all_other_pools,]*matrix(prop$B, nrow = length(all_other_pools), ncol = Nnodes, byrow=T)*matrix((1-prop$a[all_other_pools]), nrow = length(all_other_pools), ncol = Nnodes, byrow=F)) + # Add poop from when focal pool is predator
            
            colSums(cij[,all_other_pools]*matrix(prop$B, nrow = Nnodes, ncol = length(all_other_pools), byrow=F)*matrix((1-prop$a), nrow = Nnodes, ncol = length(all_other_pools), byrow=F))# add poop from where it is prey
        )
      
    }
  }
  
  return(list(J = J, # Return the Jacobian
              eigen = eigen(J), # Return the eigenvalues and vectors
              rmax = max(Re(eigen(J)$values)))) # Return the maximum eigenvalue. The system is stable if this is negative
}

# ------ Correction of stoichiometry ------ #
# 9. A function to fix production efficiency
productionadj = function(usin # The input community to fix the production efficiency
){
  FMAT = comana(usin, shuffleTL = F)$fmat
  Nnodes = dim(FMAT)[1]
  CNj = usin$prop$CN 
  CNi = matrix(usin$prop$CN, ncol = Nnodes, nrow = Nnodes, byrow = T) 
  
  temp1 = CNj*rowSums(FMAT/CNi)/rowSums(FMAT)
  temp1[is.na(temp1)] = 1
  
  tochange = usin$prop$p > temp1 & usin$prop$canIMM != 1
  
  usin$prop$p[tochange] = temp1[tochange]
  
  return(usin)
}

# 10. A function to fix diet
correct_diet <- function(usin, # The input community to fix the diets
                         dietlimits = c(NA) # A matrix the same size as imat that gives the diet limits as a proportion of the total diet. All values must be between 0 and 1. Leaving it as NA sets the limits of all diet items to 1.
){
  
  # Setting up and verifying diet limits
  if(any(is.na(dietlimits))){
    dietlimits = usin$imat
    dietlimits[dietlimits > 0] = 1
  }else{
    if(!all(dim(dietlimits) == dim(usin$imat))) stop("dietlimits must have the same dimensions as imat")
    if(any(dietlimits > 1) | any(dietlimits < 0)) stop("dietlimits must be a proportion of the diet between 0 and 1")
  }
  
  #Identify the species that need correction
  AIJ = Aijfcn(usin)
  species = which(
    rowSums(comana(usin, shuffleTL = F)$Nmin) < 0 & # Species must have negative Nmin rate
      apply(AIJ,1, max) > 0 & # Species must have a food item that gives them a positive nitrogen balance
      apply(usin$imat > 0, 1, sum) > 1 & # Species must have more than one food item
      !usin$prop$canIMM == 1 # The species is not flagged as a species that can immobilize N
  )
  
  for(sp in species){
    
    food = usin$imat[sp,] > 0
    ai = AIJ[sp,food]
    biomass = usin$prop$B[food]
    FT = rowSums(comana(usin, shuffleTL = F)$fmat)[sp]
    limits = dietlimits[sp,food]
    
    # Try to see whether there is a solution where the diet can be optimized
    sol1 <- NULL
    try(
      sol1 <- quadprog::solve.QP(Dmat = diag(length(ai)), # We need the squared terms for each diet, use the identity matrix to get them f^T I f
                                 dvec = biomass/sum(biomass), # The diet is as close to the relative abundance as possible
                                 Amat = t(rbind(rep(1, length(ai)),ai*FT,-diag(nrow = length(ai)),diag(nrow = length(ai)))), # proportions sum to 1, Nmin is zero, and none of the limits are exceeded
                                 bvec = c(1, 0, -limits, rep(0,length(ai))), # proportions sum to 1, Nmin is zero, and none of the limits are exceeded, and all values greater than zero
                                 meq = 1 # first position in Amat is an equality constraint)
      ),
      silent = T)
    
    # If there is no solution, then run a linear program to find the closest diet within the limits
    if(is.null(sol1)){
      sol1 <- lpSolve::lp(direction = "max",
                          objective.in = c(ai*FT),
                          const.mat = rbind(rep(1, length(ai)),-diag(nrow = length(ai)),diag(nrow = length(ai))),
                          const.dir = c("=", rep(">=", 2*length(ai))),
                          const.rhs = c(1, -limits, rep(0, length(ai)))
      )
    }
    
    FLIST = sol1$solution
    FLIST1 = FLIST/biomass
    FLIST1 = min(FLIST1[FLIST1 > 0])
    FLIST2 = (FLIST/biomass)/FLIST1
    
    usin$imat[sp,food] = FLIST2
    
    if(sum((comana(usin,shuffleTL = F)$fmat[sp,food]/FT - FLIST)^2) > 1e-10) stop("Check quadratic optimization. There is an issue.")
    
  }
  
  return(usin)
  
}

# 11. A function that combines the production efficiency and diet functions with a convenient wrapper and options
corrstoich <- function(USIN, # Community to correct stoichiometry.
                       forceProd = F,# Should we force organisms to only change their production efficiency?
                       dietlimits = c(NA) # A matrix the same size as imat that gives the diet limits as a proportion of the total diet. All values must be between 0 and 1. Leaving it as NA sets the limits of all diet items to 1.
){
  USIN = TLsort(USIN)
  
  if(forceProd){
    USIN3 = productionadj(usin = USIN)
  }else{
    USIN2 = correct_diet(usin = USIN, dietlimits = dietlimits)
    
    USIN3 = productionadj(usin = USIN2)
  }
  return(USIN3)
}

# ---------------- Combine tropho-species ---------------- #
# 12. This is a function that lets you combine trophic species and return a new community with the two species combined
# This function only considers similarity based on trophic links and not based on parameters!

comtrosp <- function(usin, # The community were you are combining trophic species
                     selected = F, # Do you want to select the trophic species to combine or do you want to combine two based on their similarity?
                     selected2 = c("X2","X3"), # If you selected trophic species to combine, which ones do you want to combine (vector of two names)?
                     deleteCOMBOcannibal = T, # Do you want to delete the cannibalism that may have been created by combining two trophic species (T) or leave it in the model (F)?
                     allFEEDING1 = T # Do you want to return all feeding preferences to 1 (T), or would you like to set the feeding preferences of the newly combined trophic species as the biomass-weighted average of the old ones (F)?
){
  
  imat = usin$imat
  prop = usin$prop
  
  imat_mod_update = imat_mod = imat # Create some replicates of the feeding matrix to store intermediate steps
  
  # If selected trophic species are combined, then just set them in the idmax, which is the matrix identifying their position in the web
  if(selected){
    idmax = matrix(which(rownames(imat) %in% selected2), ncol = 2, nrow = 1)
  }else{
    
    # If you don't select the species to be combined, then the most similar species are calcualted by comparing the number of matching trophic links
    
    # Matrix to save how similar each pair of species are
    simidx = matrix(NA, ncol = dim(imat_mod)[1], nrow = dim(imat_mod)[1]) 
    colnames(simidx) = rownames(simidx) = rownames(imat_mod)
    
    # Loop through and calculate that similarity
    for(ii in 1:dim(imat_mod)[1]){
      for(jj in 1:dim(imat_mod)[2]){
        simidx[ii,jj] = unname(table(c(imat_mod[ii,] == imat_mod[jj,],imat_mod[,ii] == imat_mod[,jj]))["TRUE"])
      }
    }
    
    diag(simidx) = -1 # Set the diagonal equal to -1, because each species is most similar to itself and we want to identify the most different ones
    
    simidx[lower.tri(simidx)] = -1 # Set the lower triangle of the matrix to -1, because these values repeat the upper triangle and create duplicates that are not appropriate
    
    # Find the most similar tropho-species
    idmax = which(simidx == max(simidx), arr.ind=T)
    
    # Randomly pick one combination if there are two trophic species that have the same trophic similarity.
    if(dim(idmax)[1] > 1){
      idmax = idmax[sample(seq(1,dim(idmax)[1]),1),]
    }
    
  }
  
  # The next section of code combines the two trophic species listed in idmax
  
  # Take the biomass average of their feeding
  imat_mod[idmax,] = imat[idmax,]*prop$B[idmax]/sum(prop$B[idmax])
  
  imat_mod[,idmax[1]] = imat[,idmax[1]]*prop$B[idmax[1]]/sum(prop$B[idmax])
  
  imat_mod[,idmax[2]] = imat[,idmax[2]]*prop$B[idmax[2]]/sum(prop$B[idmax])
  
  # Combine them
  imat_mod_update[idmax[1],] = imat_mod[idmax[1],] + imat_mod[idmax[2],]
  imat_mod_update[,idmax[1]] = imat_mod[,idmax[1]] + imat_mod[,idmax[2]]
  
  # Remove old species
  imat_mod_update = imat_mod_update[-idmax[2],]
  imat_mod_update = imat_mod_update[,-idmax[2]]
  
  #Update name: The new name is name1/name2
  rownames(imat_mod_update)[idmax[1]] = colnames(imat_mod_update)[idmax[1]] = paste0(colnames(imat_mod)[idmax], collapse = "/")
  
  # Rebalance the feeding relationships to 1 if asked
  if(allFEEDING1){
    imat_mod_update[imat_mod_update > 0] = 1
  }
  
  # Modify the parameters: Weighted average based on biomass
  
  # necessary function that is used to add a factor to the property list of trophic species names
  addlevel <- function(x, newlevel){
    if(is.factor(x)) return(factor(x, levels=c(levels(x), newlevel)))
    return(x)
  }
  
  # Find the properties of the combined trophic species
  prop_mod = prop[prop$ID %in% colnames(imat_mod)[idmax],]
  
  # Create an empty property vector to fill with the new data
  emptyprop = prop[1,]
  
  # The biomass is the sume of the two combined species biomass
  emptyprop$B = sum(prop_mod$B)
  
  # The other values are the biomass weighted average
  emptyprop[,c("d", "a", "p", "CN")] = colSums(prop_mod[,c("d", "a", "p", "CN")]*prop_mod[,"B"]/sum(prop_mod$B))
  
  emptyprop[,"canIMM"] = max(prop_mod$canIMM)
  if(length(unique(prop_mod$canIMM))>1) warning("Combined trophic species have different parameters for canIMM. Choosing canIMM == 1")
  
  emptyprop[,"isDetritus"] = max(prop_mod$isDetritus)
  if(length(prop_mod$isDetritus > 0)<2) warning("Combined trophic species have different parameters for isDetritus Choosing isDetritus == 1")
  
  emptyprop[,"isPlant"] = max(prop_mod$isPlant)
  if(length(unique(prop_mod$isPlant))>1) warning("Combined trophic species have different parameters for isPlant Choosing isPlant == 1")
  
  # Create the property updated data frame
  prop_update = prop
  
  # Add the level of the ID column
  prop_update[,1] = addlevel(prop_update[,1], newlevel = paste0(colnames(imat_mod)[idmax], collapse = "/"))
  
  # Add in the new row for the new species to replace the first species that was combined
  prop_update[prop_update$ID %in% colnames(imat_mod)[idmax[1]],-1] = emptyprop[,-1]
  prop_update[prop_update$ID %in% colnames(imat_mod)[idmax[1]],1] = paste0(colnames(imat_mod)[idmax], collapse = "/")
  
  # Remove the second species that was combined
  prop_update = prop_update[!(prop_update$ID %in% colnames(imat_mod)[idmax[2]]),]
  
  # Set cannibalism to zero
  if(deleteCOMBOcannibal){
    imat_mod_update = as.matrix(imat_mod_update)
    diag(imat_mod_update)[which(rownames(imat_mod_update) %in% paste0(colnames(imat_mod)[idmax], collapse = "/"))] = 0
  }
  
  # Reset isDetritus so it sums to one:
  if(sum(prop_update$isDetritus) != 0){
    prop_update$isDetritus = prop_update$isDetritus/sum(prop_update$isDetritus)
  }

  # Double check that imat and prop have the same order
  stopifnot(all(colnames(imat_mod_update) == prop_update$ID))
  
  return(list(imat = imat_mod_update, prop = prop_update))
}

# ------ Compare community analysis results models ------ #
# 13. Compare the community analysis
diffcomana <- function(mo1, # The community with more trophic species or the analysis output from comana.
                       mo2, # The community with fewer trophic species or the analysis output from comana.
                       mkplot = F, # Do you want to make a plot?
                       whattoplot = c("Nmin","Cmin"), # Should the plot have nitrogen and carbon mineralization?
                       outsideplotctrl = F # Should this function create a new plot (F) or do you want it to add plots to the existing device (T)
){
  
  # Check to see if user input the base community or the community analysis. If they input the base community, then analyze it for them.
  if(!('consumption' %in% attributes(mo1)$names)){
    mo1 = comana(mo1)
    mo2 = comana(mo2)
  }
  
  # A conveniance function to switch the communities if they are put in the wrong starting spot
  if(length(mo1$consumption) < length(mo2$consumption)){
    mo1a = mo2
    mo2a = mo1
    mo1 = mo1a
    mo2 = mo2a
    rm(mo1a, mo2a)
  }
  
  stopifnot(length(mo1$consumption) > length(mo2$consumption))
  
  # mo3 is the replacement for mo1 but in mo2 format
  mo3 = mo1
  
  # Identify which species in the first community are not in the second community:
  l1 = names(mo3$consumption)
  l2 = names(mo2$consumption)
  
  # Find mismatches
  l1a = l1[!(l1 %in% l2)]
  l2a = l2[!(l2 %in% l1)]
  
  # Check to see whether the mismatched species are common:
  if(any(grepl("/", l2a))){
    l3 = strsplit(l2a, split = "/") # The list of differences between the two communities
    
    # Add back together changes that have already been made
    for(kk in 1:length(l3)){
      if(length(l3[[kk]]) > 2){
        ll3 = length(l3[[kk]])
        
        l3[[kk]] = c(paste0(l3[[kk]][1:(ll3-1)], collapse = "/"), l3[[kk]][ll3])
        
        if(!(l3[[kk]][1] %in% l1)){
          l3temp = strsplit(l3[[kk]][1], split = "/")
          
          for(jj in length(l3):kk){
            l3[jj+1] = l3[jj]
          }
          
          l3[kk] = l3temp
          rm(l3temp)
        }
        
      }
    }
    
    
    
    # RUN THROUGH THE LIST OF DIFFERENCES 
    for(kk in 1:length(l3)){
      
      # Generate new lists
      l1 = names(mo3$consumption)
      
      slt = l1 %in% l3[[kk]]
      sltname = paste0(l3[[kk]], collapse = "/")
      
      # Calculate the differences in total consumption for each trophic species
      mo3$consumption[slt][[1]] = sum(mo1$consumption[slt])
      names(mo3$consumption)[slt][[1]] = sltname
      mo3$consumption = mo3$consumption[!(names(mo3$consumption) %in% l3[[kk]])]
      
      # Calculate the differences in carbon mineralization
      mo3$Cmin[slt][[1]] = sum(mo1$Cmin[slt])
      names(mo3$Cmin)[slt][[1]] = sltname
      mo3$Cmin = mo3$Cmin[!(names(mo3$Cmin) %in% l3[[kk]])]
      
      # Calculate the differences in nitrogen mineralization
      mo3$Nmin[,slt][,1] = rowSums(mo3$Nmin[,slt])
      mo3$Nmin[slt,][1,] = colSums(mo3$Nmin[slt,])
      colnames(mo3$Nmin)[slt][1] = sltname
      rownames(mo3$Nmin)[slt][1] = sltname
      mo3$Nmin = mo3$Nmin[!(rownames(mo3$Nmin) %in% l3[[kk]]),!(colnames(mo3$Nmin) %in% l3[[kk]])]
      
      # Calculate the differences in consumption
      mo3$fmat[,slt][,1] = rowSums(mo3$fmat[,slt])
      mo3$fmat[slt,][1,] = colSums(mo3$fmat[slt,])
      colnames(mo3$fmat)[slt][1] = sltname
      rownames(mo3$fmat)[slt][1] = sltname
      mo3$fmat = mo3$fmat[!(rownames(mo3$fmat) %in% l3[[kk]]),!(colnames(mo3$fmat) %in% l3[[kk]])]
      
    }
  }
  
  # Combined these data together for each trophic species. Also calculate the net carbon and net nitrogen mineralization of the entire web
  OUT = merge(data.frame(ID = c(names(rowSums(mo3$Nmin)), "Net"),
                         Nmin = c(rowSums(mo3$Nmin),sum(rowSums(mo3$Nmin), na.rm = T)),
                         Cmin = c(mo3$Cmin, sum(mo3$Cmin, na.rm = T))), 
              data.frame(ID = c(names(rowSums(mo2$Nmin)), "Net"),
                         Nmin2 = c(rowSums(mo2$Nmin),sum(rowSums(mo2$Nmin), na.rm = T)),
                         Cmin2 = c(mo2$Cmin, sum(mo2$Cmin, na.rm = T))), all.x = T, all.y = T)
  
  # Calculate the proportional change in nitrogen and carbon mineralization for each trophic species and for the net
  OUT[,"dNmin"] = (OUT$Nmin2-OUT$Nmin)/OUT$Nmin
  OUT[,"dCmin"] = (OUT$Cmin2-OUT$Cmin)/OUT$Cmin
  OUT[,"Color"] = rep("grey", dim(OUT)[1])
  OUT[OUT$ID == "Net", "Color"] = "blue" # Set the net color blue to show up in plots
  
  # Plot the results if requested:
  if(mkplot){
    
    if(!outsideplotctrl){
      if(length(whattoplot) >2){
        par(mfrow=c(2,2), mar = c(2,4,2,2)+0.1)
      }else{
        if(length(whattoplot) == 2){
          par(mfrow=c(1,2), mar = c(2,4,2,2)+0.1)
        }else{
          par(mfrow=c(1,1), mar = c(2,4,2,2)+0.1)
        }
      }
    }
    
    if("Nmin" %in% whattoplot){
      OUT2 = OUT[order(-abs(OUT$dNmin)),]
      barplot(100*OUT2$dNmin, col = OUT2$Color, names.arg = OUT2$ID, ylab = "Change N min. (%)")
    }
    
    if("Cmin" %in% whattoplot){
      OUT2 = OUT[order(-abs(OUT$dCmin)),]
      barplot(100*OUT2$dCmin, col = OUT2$Color, names.arg = OUT2$ID, ylab = "Change C min. (%)")
    }
  }
  
  OUT[,"Color"] = NULL
  
  return(OUT) # Return the differences
}

# ------ Loop over model, combining tropho-species ------ #
# 14. This function provides a convenient way to loop over multiple combinations of the trophic species and return lists of each step

loopsubsets <- function(USin, # Starting community
                        STEPS = 2, # Number of times to combine trophic species + 1 (need to include 1 step which is the inital community)
                        MKPLOT = T, # Make a plot of the combinations and their properties
                        selectedCOMBO = T, # Do we want to combine species that are user defined?
                        selected2COMBO = list(c("X2","X3")), # A list of trophic species to combine in the order you want them combined. Must be of length 1 less than the value in STEPS
                        labeldiff = F # Do you want to label the difference with the step number?
){
  
  # Are there enough selected trophic species to be combined for the number of steps taken?
  if(selectedCOMBO) stopifnot(length(selected2COMBO) == STEPS-1)
  
  # Set up graphics
  par(mfrow=c(STEPS,2), mar = c(2,4,2,2)+0.1)
  
  # Lists to save the communities and the differences
  itm = vector(mode = "list", length = STEPS)
  USINvec = vector(mode = "list", length = STEPS)
  different = vector(mode = "list", length = STEPS)
  
  # Correct stoichiometry
  USINvec[[1]] = corrstoich(USin)
  
  for(hh in 1:STEPS){
    
    # For the first plot, plot the web and the N mineralization
    if(hh == 1){
      itm[[hh]] <- comana(USINvec[[hh]], mkplot = MKPLOT, whattoplot = c("web", "Nmin"), outsideplotctrl = T)
    }else{
      # Otherwise, plot only the web
      itm[[hh]] <- comana(USINvec[[hh]], mkplot = MKPLOT, whattoplot = c("web"), outsideplotctrl = T)
    }
    
    # If you aren't at the last step, fix the stoichiometry and combine the selected trophic species
    if(hh != STEPS){
      USINvec[[hh+1]] = corrstoich(comtrosp(USINvec[[hh]], selected = selectedCOMBO, selected2 = selected2COMBO[[hh]]))
    }
    
    # If the one step has already been taken, return the difference and plot the nitrogen mineralization difference. The difference is always compated to the original community!
    if(hh > 1){
      different[[hh]] = diffcomana(itm[[1]], itm[[hh]], mkplot = MKPLOT, whattoplot = c("Nmin"), outsideplotctrl = T )
      
      # Label the difference step in the data frame to help keep track of the results
      if(labeldiff){
        different[[hh]][,"STEP"] = hh
      }
    }
  }
  
  # Return a list of each community analysis, then the community, and finally the difference matrix.
  return(list(itm = itm, USINvec = USINvec, different = different))
}