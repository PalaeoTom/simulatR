#' Population genetic rate of evolution threshold inheritance model
#'
#' Changes to PGT that occur during speciation event. Tied to abundance of population (large population = broad distribution new PGT can be sampled from.
#'
#' @param p0 populations object from previous time bin.
#' @param p the numeric index of the population in p0$population.variables.
#' @param t0 time of previous time bin.
#' @param t1 time of current time bin.
#' @param SF a numeric value by which the changes in PGT at speciation event can be scaled. For flexibility.
#'
#' @return a numeric value.
#' @export
#'
#' @keywords internal
#' # make a stage
#' s <- make.stage(n.col = 5, n.row = 5, ar = 400)
#'
#' # define population variables
#' PVs <- set.pop.var.seeds(pop.gen.threshold = 0.1, new.var.name = "PV1", new.var.seed = function() runif(1,10,50))
#'
#' # generate seed populations
#' p0 <- gen.seed.pops(stage = s, pop.var.seeds = PVs, n = 10)
#'
#' # Run function
#' inherit.pop.gen.threshold(p0 = p0, p = sample(1:length(p0$population.variables), 1), t0 = 100, t1 = 90)
inherit.pop.gen.threshold <- function(p0, p, t0, t1, min = 0, max = 1, SF = 1){
  ## get PGT at previous time step
  PGT0 <- unname(p0$population.variables[[p]][which(names(p0$population.variables[[p]]) == "PGT")])
  ## if pop.gen switched on
  if(any(p0$variable.names == "PGT")){
    ## get abundance
    A0 <- unname(p0$population.variables[[p]][which(names(p0$population.variables[[p]]) == "A")])
    ## get value between min and max
    while(TRUE){
      PGT1 <- rnorm(1, mean = PGT0, sd = abs(t1-t0)*SF*(A0/10000000))
      ## If min and max employed
      if(is.numeric(min) && is.numeric(max)){
        if(PGT1 >= min && PGT1 <= max) break()
      }
      ## If just min
      if(is.numeric(min) && max == "none"){
        if(PGT1 >= min) break()
      }
      ## If just max
      if(min == "none" && is.numeric(max)){
        if(PGT1 <= max) break()
      }
    }
  } else {
    ## get value between min and max
    while(TRUE){
      ## sample new value - normal distribution with mean PGT0 and SD determine by time elapsed and SF alone.
      PGT1 <- rnorm(1, mean = PGT0, sd = abs(t1-t0)*SF)
      ## If min and max employed
      if(is.numeric(min) && is.numeric(max)){
        if(PGT1 >= min && PGT1 <= max) break()
      }
      ## If just min
      if(is.numeric(min) && max == "none"){
        if(PGT1 >= min) break()
      }
      ## If just max
      if(min == "none" && is.numeric(max)){
        if(PGT1 <= max) break()
      }
    }
  }
  return(PGT1)
}

