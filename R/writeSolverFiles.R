writeSolverFiles <- function(oF=oF, binaries=binaries, constraints = constraints,
                             generals=generals, mipGAP=0, 
                             poolrelGAP=0, poolReplace=2,
                             limitPop=500, poolCap=100,
                             poolIntensity=4, timelimit=3600){
  
  ## write the .lp file
  data = paste0("testFile.lp")
  write("enter Problem", data)
  write("", data, append = TRUE)
  write("Minimize", data, append = TRUE)
  write(oF, data, append = TRUE)
  write("Subject To", data, append = TRUE)
  write(constraints, data, append = TRUE)
  write("Binaries", data, append = TRUE)
  write(binaries, data, append = TRUE)
  write("Generals", data, append = TRUE)
  write(generals, data, append = TRUE)
  # write("Integers", data, append = TRUE)
  # write(binaries, data, append = TRUE)
  # write(generals, data, append = TRUE)
  write("End", data, append = TRUE)
  
  ## write cplexCommand file
  cplexCommand <- paste0("cplexCommand.txt")
  write(paste0("read testFile.lp"), 
        cplexCommand, append = TRUE)
  write(paste0("set mip tolerances mipgap ",mipGAP), 
        cplexCommand, append = TRUE)
  write(paste0("set mip pool relgap ",poolrelGAP), 
        cplexCommand, append = TRUE)
  write(paste0("set mip pool replace ", poolReplace), 
        cplexCommand, append = TRUE)
  write(paste0("set mip limits populate ",limitPop), 
        cplexCommand, append = TRUE)
  write(paste0("set mip pool capacity ",poolCap), 
        cplexCommand, append = TRUE)
  write(paste0("set mip pool intensity ",poolIntensity), 
        cplexCommand, append = TRUE)
  write(paste0("set timelimit ",timelimit), cplexCommand, append = TRUE)
  write("populate", cplexCommand, append = TRUE)
  write(paste0("write results_cplex.txt sol all"), 
        cplexCommand, append = TRUE)
  write("quit", cplexCommand, append = TRUE)
  
}