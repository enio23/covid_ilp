solve_cplex <- function(solverPath = "~/Documents/cplex"){
  
  system(paste0(solverPath, " -f cplexCommand.txt"))
  
}