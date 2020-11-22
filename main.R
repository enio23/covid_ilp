library(ggplot2)
library(xlsx)
library(dplyr)

#
dir.create("output")

#
set.seed(1234)

source(file = "R/cleanup_files.R")
source(file = "R/create_variables.R")
source(file = "R/generate_network.R")
source(file = "R/generate_transport.R")
source(file = "R/read_solution.R")
source(file = "R/solve_cplex.R")
source(file = "R/write_binaries.R")
source(file = "R/writeSolverFiles.R")
source(file = "R/write_of.R")
source(file = "R/write_generals.R")
source(file = "R/write_constraints.R")

solutionList <- list()
demands <- c()

## Case - 1
network <- generate_network(nHubs = 30, minClinics = 5, maxClinics = 10, minDemands = 100, maxDemands = 1000)
demands <- c(demands, sum(network$demands))
transport <- generate_transport(network = network, availabilty = c(100, 500, 1000, 1000))
variables <- create_variables(network = network, transport = transport)
objective_function <- write_of(transport = transport, variables = variables, network = network)
generals <- write_generals(variables = variables)
binaries <- write_binaries(variables = variables)
constraints <- write_constraints(transport = transport, variables = variables, network = network)
writeSolverFiles(oF = objective_function, binaries = binaries, constraints = constraints, generals = generals)
solve_cplex()
solution1 <- read_solution(variables = variables)
cleanup_files()
solutionList[[length(solutionList)+1]] <- solution1

## Case - 2
network <- generate_network(nHubs = 60, minClinics = 5, maxClinics = 10, minDemands = 100, maxDemands = 1000)
demands <- c(demands, sum(network$demands))
transport <- generate_transport(network = network, availabilty = c(100, 500, 1000, 1000), expandFactor = 2)
variables <- create_variables(network = network, transport = transport)
objective_function <- write_of(transport = transport, variables = variables, network = network)
generals <- write_generals(variables = variables)
binaries <- write_binaries(variables = variables)
constraints <- write_constraints(transport = transport, variables = variables, network = network)
writeSolverFiles(oF = objective_function, binaries = binaries, constraints = constraints, generals = generals)
solve_cplex()
solution2 <- read_solution(variables = variables)
cleanup_files()
solutionList[[length(solutionList)+1]] <- solution2

## Case - 3
network <- generate_network(nHubs = 100, minClinics = 5, maxClinics = 10, minDemands = 1000, maxDemands = 10000)
demands <- c(demands, sum(network$demands))
transport <- generate_transport(network = network, availabilty = c(100, 500, 1000, 1000), expandFactor = 6)
variables <- create_variables(network = network, transport = transport)
objective_function <- write_of(transport = transport, variables = variables, network = network)
generals <- write_generals(variables = variables)
binaries <- write_binaries(variables = variables)
constraints <- write_constraints(transport = transport, variables = variables, network = network)
writeSolverFiles(oF = objective_function, binaries = binaries, constraints = constraints, generals = generals)
solve_cplex()
solution3 <- read_solution(variables = variables)
cleanup_files()
solutionList[[length(solutionList)+1]] <- solution3

## Case - 4
network <- generate_network(nHubs = 120, minClinics = 5, maxClinics = 10, minDemands = 1000, maxDemands = 10000)
demands <- c(demands, sum(network$demands))
transport <- generate_transport(network = network, availabilty = c(100, 500, 1000, 1000), expandFactor = 5)
variables <- create_variables(network = network, transport = transport)
objective_function <- write_of(transport = transport, variables = variables, network = network)
generals <- write_generals(variables = variables)
binaries <- write_binaries(variables = variables)
constraints <- write_constraints(transport = transport, variables = variables, network = network)
writeSolverFiles(oF = objective_function, binaries = binaries, constraints = constraints, generals = generals)
solve_cplex()
solution4 <- read_solution(variables = variables)
cleanup_files()
solutionList[[length(solutionList)+1]] <- solution4

## Case - 5
network <- generate_network(nHubs = 150, minClinics = 5, maxClinics = 10, minDemands = 1000, maxDemands = 10000)
demands <- c(demands, sum(network$demands))
transport <- generate_transport(network = network, availabilty = c(100, 500, 1000, 1000), expandFactor = 5)
variables <- create_variables(network = network, transport = transport)
objective_function <- write_of(transport = transport, variables = variables, network = network)
generals <- write_generals(variables = variables)
binaries <- write_binaries(variables = variables)
constraints <- write_constraints(transport = transport, variables = variables, network = network)
writeSolverFiles(oF = objective_function, binaries = binaries, constraints = constraints, generals = generals)
solve_cplex()
solution5 <- read_solution(variables = variables)
cleanup_files()
solutionList[[length(solutionList)+1]] <- solution5

## Case - 6
network <- generate_network(nHubs = 180, minClinics = 5, maxClinics = 10, minDemands = 1000, maxDemands = 10000)
demands <- c(demands, sum(network$demands))
transport <- generate_transport(network = network, availabilty = c(100, 500, 1000, 1000), expandFactor = 6)
variables <- create_variables(network = network, transport = transport)
objective_function <- write_of(transport = transport, variables = variables, network = network)
generals <- write_generals(variables = variables)
binaries <- write_binaries(variables = variables)
constraints <- write_constraints(transport = transport, variables = variables, network = network)
writeSolverFiles(oF = objective_function, binaries = binaries, constraints = constraints, generals = generals)
solve_cplex()
solution6 <- read_solution(variables = variables)
cleanup_files()
solutionList[[length(solutionList)+1]] <- solution6

##
df <- matrix(data = , nrow = 6, ncol = 3)
colnames(df) <- c("demands", "cost", "time")
rownames(df) <- c("A", "B", "C", "D", "E", "F")
for(ii in 1:nrow(df)){
  
  df[ii, 1] <- demands[ii]
  df[ii, 2] <- solutionList[[ii]]$`objective value`
  df[ii, 3] <- solutionList[[ii]]$time*1000
  
}
df <- as.data.frame(df)

write.xlsx2(x = df, file = "output/demands_cost_time.xls", col.names = TRUE, row.names = TRUE)

##
for(ii in 1:length(solutionList)){
  
  #
  df_hubs <- matrix(data = , nrow = length(transport), ncol = 3)
  colnames(df_hubs) <- c("vehicle_type", "n", "prop")
  rownames(df_hubs) <- names(transport)
  
  df_hubs[, 1] <- names(transport)
  idx1 <- which(grepl(pattern = "=H", x = names(solutionList[[ii]]$solution), fixed = TRUE))
  idx2 <- which(grepl(pattern = "truck", x = names(solutionList[[ii]]$solution), fixed = TRUE))
  idx3 <- which(grepl(pattern = "4x4", x = names(solutionList[[ii]]$solution), fixed = TRUE))
  idx4 <- which(grepl(pattern = "car", x = names(solutionList[[ii]]$solution), fixed = TRUE))
  idx5 <- which(grepl(pattern = "motorcycle", x = names(solutionList[[ii]]$solution), fixed = TRUE))
  df_hubs[, 2] <- c(sum(solutionList[[1]]$solution[intersect(x = idx1, y = idx2)]),
                    sum(solutionList[[1]]$solution[intersect(x = idx1, y = idx3)]),
                    sum(solutionList[[1]]$solution[intersect(x = idx1, y = idx4)]),
                    sum(solutionList[[1]]$solution[intersect(x = idx1, y = idx5)]))
}