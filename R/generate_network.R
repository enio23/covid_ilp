generate_network <- function(nHubs = nHubs, minClinics = minClinics, maxClinics = maxClinics, maxDemands = maxDemands){
  
  #
  mm <- matrix(data = , nrow = 1, ncol = 2)
  
  toBind <- matrix(data = , nrow = nHubs, ncol = 2)
  toBind[, 1] <- "P"
  toBind[, 2] <- paste0("H", 1:nHubs, "_")
  mm <- rbind(mm, toBind)
  
  nClinics <- round(x = runif(n = nHubs, min = minClinics, max = maxClinics))
  
  cnt <- 1
  for(ii in 1:nHubs){
    
    toBind <- matrix(data = , nrow = nClinics[ii], ncol = 2)
    toBind[, 1] <- paste0("H", ii, "_")
    for(jj in 1:nClinics[ii]){
      
      toBind[jj, 2] <- paste0("C", cnt, "_")
      cnt <- cnt + 1
      
    }
    
    mm <- rbind(mm, toBind)
    
  }
  
  mm <- as.data.frame(mm)
  mm <- mm[complete.cases(mm), ]
  colnames(mm) <- c("source", "target")
  mm$source <- as.character(mm$source)
  mm$target <- as.character(mm$target)
  
  #
  demands <- sample.int(n = maxDemands, size = cnt-1)
  names(demands) <- paste0("C", 1:(cnt-1), "_")
  
  #
  freq <- c(rep(1, nHubs), rep(6, cnt-1))
  names(freq) <- c(paste0("H", 1:nHubs, "_"), paste0("C", 1:(cnt-1), "_"))
  
  #
  returnList <- list()
  returnList[[length(returnList)+1]] <- mm
  returnList[[length(returnList)+1]] <- demands
  returnList[[length(returnList)+1]] <- freq
  names(returnList) <- c("network", "demands", "frequency")
  
  return(returnList)
  
}