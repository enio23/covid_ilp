write_binaries <- function(variables=variables){
  
  cc <- paste0("\t", variables$var[which(grepl(pattern = "Binary", x = variables$var_exp, fixed = TRUE))])
  
  return(cc)
  
}