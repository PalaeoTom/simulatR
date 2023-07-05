#' Minimum viable population number inheritance model
#'
#' Changes to MVP that occur during speciation event. Tied to abundance of population (large population = broad distribution new MVP can be sampled from.
#'
#' @param p0 populations object from previous time bin.
#' @param p the numeric index of the population in p0$population.variables.
#' @param t0 time of previous time bin.
#' @param t1 time of current time bin.
#' @param SF a numeric value by which the changes in MVP at speciation event can be scaled. For flexibility.
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
#' inherit.min.via.pop(p0 = p0, p = sample(1:length(p0$population.variables), 1), t0 = 100, t1 = 90)
inherit.min.via.pop <- function(p0, p, t0, t1, min = 1, max = "none", SF = 1){
  ## get MVP at previous time step
  MVP0 <- unname(p0$population.variables[[p]][which(names(p0$population.variables[[p]]) == "MVP")])
  ## if pop.gen switched on
  if(any(p0$variable.names == "PGT")){
    ## get abundance
    A0 <- unname(p0$population.variables[[p]][which(names(p0$population.variables[[p]]) == "A")])
    ## get value between min and max
    while(TRUE){
      ## sample new value - normal distribution with mean MVP0 and SD determine by time elapsed, SF, and number of times abundance goes into minimum viable population number
      MVP1 <- round(rnorm(1, mean = MVP0, sd = abs(t1-t0)*SF*(A0/10000)), digits = 0)
      ## If min and max employed
      if(is.numeric(min) && is.numeric(max)){
        if(MVP1 >= min && MVP1 <= max) break()
      }
      ## If just min
      if(is.numeric(min) && max == "none"){
        if(MVP1 >= min) break()
      }
      ## If just max
      if(min == "none" && is.numeric(max)){
        if(MVP1 <= max) break()
      }
    }
  } else {
    ## get value between min and max
    while(TRUE){
      ## sample new value - normal distribution with mean MVP0 and SD determine by time elapsed and SF alone.
      MVP1 <- round(rnorm(1, mean = MVP0, sd = abs(t1-t0)*SF), digits = 0)
      ## If min and max employed
      if(is.numeric(min) && is.numeric(max)){
        if(MVP1 >= min && MVP1 <= max) break()
      }
      ## If just min
      if(is.numeric(min) && max == "none"){
        if(MVP1 >= min) break()
      }
      ## If just max
      if(min == "none" && is.numeric(max)){
        if(MVP1 <= max) break()
      }
    }
  }
  return(MVP1)
}

