#' Default abundance model
#'
#' Changes are drawn from normal distributions where the standard deviation is the product of the time passed, and a factor (SF). Capable of inducing changes based on population genetics and minimum viable population values.
#'
#' @param p0 populations object from previous time bin.
#' @param p.ID string identifying population in question.
#' @param t.series a numeric vector specifying time bins.
#' @param t0 time of previous time bin.
#' @param SF a numeric value by which the changes in abundance can be scaled. For flexibility.
#'
#' @return a numeric value.
#' @export
#'
#' @keywords internal
#' #' # make a stage
#' s <- make.stage(n.col = 5, n.row = 5, ar = 400)
#'
#' # define population variables
#' PVs <- set.pop.var.seeds(pop.gen.threshold = 0.1, new.var.name = "PV1", new.var.seed = function() runif(1,10,50))
#'
#' # generate seed populations
#' p0 <- gen.seed.pops(stage = s, pop.var.seeds = PVs, n = 10)
#'
#' # Create vector specifying times
#' t.series <- c(100,90,80,70,60,50,40,30,20,10)
#'
#' model.abundance(p0 = p0, p.ID = sample(1:length(p0$population.variables), 1), t0 = 100, t.series = t.series)
model.abundance <- function(p0, p.ID, t.series, t0, SF = 1){
  ## get new time
  t1 <- t.series[which(t.series == t0)+1]
  ## get abundance at previous time step
  a0 <- unname(p0$population.variables[[p.ID]][which(names(p0$population.variables[[p.ID]]) == "abundance")])
  ## scale SD by time elapsed and factor
  scale.F <- abs((t1-t0)*SF)
  ## if pop.gen switched on
  if(any(p0$variable.names == "PGT")){
    ## get rate
    PGR0 <- unname(p0$population.variables[[p.ID]][which(names(p0$population.variables[[p.ID]]) == "PGR")])
    ## get threshold
    PGT0 <- unname(p0$population.variables[[p.ID]][which(names(p0$population.variables[[p.ID]]) == "PGT")])
    if(PGR0 > PGT0){
      ## If min.via.pop provided, determine whether abundance is
      if(any(p0$variable.names == "MVP")){
        ## get MVP
        MVP0 <- unname(p0$population.variables[[p.ID]][which(names(p0$population.variables[[p.ID]]) == "MVP")])
        ## if a0 => MVP0
        if(a0 >= MVP0){
          a1 <- a0 + round(rnorm(1, mean = a0*(PGR0-PGT0), sd = scale.F), digits = 0)
        } else {
          a1 <- a0 + round(rnorm(1, mean = a0-(MVP0+(a0*(PGR0-PGT0))), sd = scale.F), digits = 0)
        }
        ## if a1 is negative, change to 0
        if(a1 < 1) a1 <- 0
        ## return a1
        return(a1)
      } else {
        a1 <- a0 + round(rnorm(1, mean = a0*(PGR0-PGT0), sd = scale.F), digits = 0)
        ## if a1 is negative, change to 0
        if(a1 < 1) a1 <- 0
        ## return a1
        return(a1)
      }
    } else {
      ## If min.via.pop provided, determine whether abundance is
      if(any(p0$variable.names == "MVP")){
        ## get MVP
        MVP0 <- unname(p0$population.variables[[p.ID]][which(names(p0$population.variables[[p.ID]]) == "MVP")])
        ## if a0 => MVP0
        if(a0 >= MVP0){
          a1 <- a0 + round(rnorm(1, mean = 0, sd = scale.F), digits = 0)
        } else {
          a1 <- a0 + round(rnorm(1, mean = a0-MVP0, sd = scale.F), digits = 0)
        }
        ## if a1 is negative, change to 0
        if(a1 < 1) a1 <- 0
        ## return a1
        return(a1)
      } else {
        a1 <- a0 + round(rnorm(1, mean = 0, sd = scale.F), digits = 0)
        ## if a1 is negative, change to 0
        if(a1 < 1) a1 <- 0
        ## return a1
        return(a1)
      }
    }
  } else {
    ## If min.via.pop provided, determine whether abundance is
    if(any(p0$variable.names == "MVP")){
      ## get MVP
      MVP0 <- unname(p0$population.variables[[p.ID]][which(names(p0$population.variables[[p.ID]]) == "MVP")])
      ## if a0 => MVP0
      if(a0 >= MVP0){
        a1 <- a0 + round(rnorm(1, mean = 0, sd = scale.F), digits = 0)
      } else {
        a1 <- a0 + round(rnorm(1, mean = a0-MVP0, sd = scale.F), digits = 0)
      }
      ## if a1 is negative, change to 0
      if(a1 < 1) a1 <- 0
      ## return a1
      return(a1)
    } else {
      a1 <- a0 + round(rnorm(1, mean = 0, sd = scale.F), digits = 0)
      ## if a1 is negative, change to 0
      if(a1 < 1) a1 <- 0
      ## return a1
      return(a1)
    }
  }
}

