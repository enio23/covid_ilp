write_constraints <- function(transport = transport, variables = variables, network = network){
  
  options(scipen=999)
  
  allNodes <- unique(c(network$network$source, network$network$target))
  center <- allNodes[which(grepl(pattern = "P", x = allNodes))]
  hubs <- allNodes[which(grepl(pattern = "H", x = allNodes))]
  clinics <- allNodes[which(grepl(pattern = "C", x = allNodes))]
  
  # Constraints - 1a
  cc1 <- c()
  idx1 <- which(grepl(pattern = "Transported", x = variables$var_exp, fixed = TRUE))
  for(ii in 1:length(hubs)){
    idx2 <- c(which(grepl(pattern = paste0("=H", ii, "_"), x = variables$var_exp, fixed = TRUE)))
    idx <- intersect(x = idx1, y = idx2)
    
    vv1 <- paste0(" + ", variables$var[idx])
    vv1 <- paste(vv1, collapse = "")
    vv1 <- substring(text = vv1[1], first = 4, last = nchar(vv1))
    
    idx3 <- c(which(grepl(pattern = paste0("H", ii, "_="), x = variables$var_exp, fixed = TRUE)))
    idx <- intersect(x = idx1, y = idx3)
    vv2 <- paste0(" - ", variables$var[idx])
    vv2 <- paste(vv2, collapse = "")
    
    cc1 <- c(cc1, paste0(vv1, vv2, " = 0"))
    
  }
  
  # Constraints - 1b
  for(ii in 1:length(network$demands)){
    
    idx2 <- which(grepl(pattern = paste0("=", names(network$demands)[ii]), x = variables$var_exp, fixed = TRUE))
    idx <- intersect(x = idx1, y = idx2)
    
    vv1 <- paste0(" + ", variables$var[idx])
    vv1 <- paste(vv1, collapse = "")
    vv1 <- substring(text = vv1[1], first = 4, last = nchar(vv1))
    
    cc1 <- c(cc1, paste0(vv1, " = ", network$demands[ii]))
    
  }
  
  # Constraints - 2
  cc2 <- c()
  idx1 <- which(grepl(pattern = "Binary", x = variables$var_exp, fixed = TRUE))
  for(ii in 1:nrow(network$network)){
    
    idx2 <- which(grepl(pattern = paste0(network$network$source[ii], "=", network$network$target[ii]), 
                        x = variables$var_exp, fixed = TRUE))
    idx <- intersect(x = idx1, y = idx2)
    
    vv1 <- paste0(" + ", variables$var[idx])
    vv1 <- paste(vv1, collapse = "")
    vv1 <- substring(text = vv1[1], first = 4, last = nchar(vv1))
    
    cc2 <- c(cc2, paste0(vv1, " = 1"))
    
  }
  for(ii in 1:length(idx1)){
    
    cc2 <- c(cc2, paste0(variables$var[idx1[ii]], " >= 0"))
    cc2 <- c(cc2, paste0(variables$var[idx1[ii]], " <= 1"))
    
  }
  
  # Constraints - 3
  cc3 <- c()
  aa <- c()
  for(ii in 1:length(transport)){
    aa <- c(aa, transport[[ii]]$availability)
  }
  idx2 <- which(grepl(pattern = "Assigned", x = variables$var_exp, fixed = TRUE))
  for(ii in 1:length(idx2)){
    cc3 <- c(cc3, paste0(variables$var[idx2[ii]], " >= 0"))
    cc3 <- c(cc3, paste0(variables$var[idx2[ii]], " - ", as.numeric(max(aa)+1), " ", variables$var[idx1[ii]], " <= 0"))
  }
  
  # Constraints - 4
  cc4 <- c()
  ww <- c()
  for(ii in 1:length(transport)){
    ww <- c(ww, transport[[ii]]$capacity)
  }
  idx1 <- which(grepl(pattern = "Assigned", x = variables$var_exp, fixed = TRUE))
  idx2 <- which(grepl(pattern = "Transported", x = variables$var_exp, fixed = TRUE))
  for(ii in 1:length(transport)){
    idx3 <- which(grepl(pattern = names(transport)[ii], x = variables$var_exp, fixed = TRUE))
    for(jj in 1:nrow(network$network)){
      idx4 <- which(grepl(pattern = paste0(network$network$source[jj], "=", network$network$target[jj]), 
                          x = variables$var_exp, fixed = TRUE))
      idx5 <- intersect(x = idx3, y = idx4)
      idx6 <- intersect(x = idx1, y = idx5)
      idx7 <- intersect(x = idx2, y = idx5)
      
      cc4 <- c(cc4, paste0(variables$var[idx6], " - ", 1/ww[ii], " ", variables$var[idx7], " >= 0"))
      
    }
  }
  
  # Constraints - 5
  cc5 <- c()
  aa <- c()
  for(ii in 1:length(transport)){
    aa <- c(aa, transport[[ii]]$availability)
  }
  for(ii in 1:length(transport)){
    idx1 <- which(grepl(pattern = names(transport)[ii], x = variables$var_exp, fixed = TRUE))
    for(jj in 1:nrow(network$network)){
      idx2 <- which(grepl(pattern = paste0(network$network$source[jj], "=", network$network$target[jj]), 
                          x = variables$var_exp, fixed = TRUE))
      idx3 <- which(grepl(pattern = "Assigned", x = variables$var_exp, fixed = TRUE))
      idx <- intersect(x = idx1, y = intersect(x = idx2, idx3))
      
      cc5 <- c(cc5, paste0(variables$var[idx], " <= ", aa[ii]))
    }
  }
  
  #
  cc <- c(cc1, cc2, cc3, cc4, cc5)
  constraints <- paste0("\tc", 1:length(cc), ":", cc)
  return(constraints)
  
}
