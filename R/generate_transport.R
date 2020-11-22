generate_transport <- function(vehicle_types = c("truck", "4x4", "car", "motorcycle"), 
                               costRangeMin = c(500, 100, 50, 10), 
                               costRangeMax = c(2500, 300, 200, 50),
                               maxCapacity = c(10000, 500, 200, 20),
                               availabilty = c(100, 500, 1000, 1000),
                               network = network, expandFactor = 1){
  
  returnList <- list()
  
  for(ii in 1:length(vehicle_types)){
    
    currList <- list()
    currList[[length(currList)+1]] <- runif(n = nrow(network$network), min = costRangeMin[ii], max = costRangeMax[ii])
    currList[[length(currList)+1]] <- maxCapacity[ii]
    currList[[length(currList)+1]] <- availabilty[ii]*expandFactor
    names(currList) <- c("cost", "capacity", "availability")
    returnList[[length(returnList)+1]] <- currList
    
  }
  
  names(returnList) <- vehicle_types
  
  return(returnList)
  
}