write_generals <- function(variables=variables){
  
  cc1 <- paste0("\t", variables$var[which(grepl(pattern = "Transported", x = variables$var_exp, fixed = TRUE))])
  cc2 <- paste0("\t", variables$var[which(grepl(pattern = "Assigned", x = variables$var_exp, fixed = TRUE))])
  
  return(c(cc1, cc2))
  
}