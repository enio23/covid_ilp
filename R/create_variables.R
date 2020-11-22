create_variables <- function(network = network, transport = transport){
  
  cnt <- 1
  
  #
  cc1 <- c()
  cc1Exp <- c()
  for(ii in 1:length(transport)){
    
    for(jj in 1:nrow(network$network)){
      
      cc1 <- c(cc1, paste0("xb", cnt))
      cc1Exp <- c(cc1Exp, 
                  paste0("Binary of ", network$network$source[jj], "=", network$network$target[jj], 
                         " for type ", names(transport)[ii]))
      cnt <- cnt + 1
      
    }
    
  }
  
  #
  cc2 <- c()
  cc2Exp <- c()
  for(ii in 1:length(transport)){
    
    for(jj in 1:nrow(network$network)){
      
      cc2 <- c(cc2, paste0("xb", cnt))
      cc2Exp <- c(cc2Exp, 
                  paste0("Transported in ", network$network$source[jj], "=", network$network$target[jj], 
                         " for type ", names(transport)[ii]))
      cnt <- cnt + 1
      
    }
    
  }
  
  #
  cc3 <- c()
  cc3Exp <- c()
  for(ii in 1:length(transport)){
    
    for(jj in 1:nrow(network$network)){
      
      cc3 <- c(cc3, paste0("xb", cnt))
      cc3Exp <- c(cc3Exp, 
                  paste0("Assigned in ", network$network$source[jj], "=", network$network$target[jj], 
                         " for type ", names(transport)[ii]))
      cnt <- cnt + 1
      
    }
    
  }
  
  #
  variables <- list()
  variables[[length(variables)+1]] <- c(cc1, cc2, cc3)
  variables[[length(variables)+1]] <- c(cc1Exp, cc2Exp, cc3Exp)
  names(variables) <- c("var", "var_exp")
  return(variables)
  
}