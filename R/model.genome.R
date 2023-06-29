#' Default genome model
#'
#' Changes are drawn from normal distributions where the standard deviation is the product of the time passed, and a factor (SF).
#' Capable of inducing changes based on principles of population genetics and abundance.
#'
#' @param p0 populations object from previous time bin.
#' @param p.ID string identifying population in question.
#' @param t.series a numeric vector specifying time bins.
#' @param t0 time of previous time bin.
#' @param SF a numeric value by which the changes in mean genome can be scaled. For flexibility.
#'
#' @return a numeric value.
#' @export
#'
#' @keywords internal
model.genome <- function(p0, p.ID, t.series, t0, SF = 1){
  ## get new time
  t1 <- t.series[which(t.series == t0)+1]
  ## identify population in question
  p.ind <- which(names(p0$population.variables) == p.ID)
  ## get genome at previous time step
  g0 <- p0$population.variables[[p.ind]][which(names(p0$population.variables[[p.ind]]) == "G")]
  ## scale SD by time elapsed and factor
  scale.F <- abs((t1-t0)*SF)
  ## if pop.gen switched on
  if(any(p0$variable.names == "PGT")){
    ## identify species
    s.ID <- names(p0$species.representation)[which(sapply(1:length(p0$species.representation), function(x) any(p0$species.representation[[x]] == p.ID)))]
    ## get location of population
    r <- which(sapply(1:length(p0$populated.regions), function(x) any(p0$populated.regions[[x]] == p.ID)))
    ## get average dispersal distance of population
    ADD <- p0$population.variables[[p.ind]][which(names(p0$population.variables[[p.ind]]) == "ADD")]
    ## get location of all other populations of this species
    p0$species.representation[[which(names(p0$species.representation) == s.ID)]][!p0$species.representation[[which(names(p0$species.representation) == s.ID)]] == p.ID]
    ## get all populations of same species in range


    ## get abundance
    a0 <- p0$population.variables[[p.ind]][which(names(p0$population.variables[[p.ind]]) == "abundance")]
    ## get rate
    PGR0 <- p0$population.variables[[p.ind]][which(names(p0$population.variables[[p.ind]]) == "PGR")]
    ## get threshold
    PGT0 <- p0$population.variables[[p.ind]][which(names(p0$population.variables[[p.ind]]) == "PGT")]
    if(PGR0 > PGT0){
      g1 <- g0 + round(rnorm(1, mean = g0*(PGR0-PGT0), sd = scale.F), digits = 0)
      ## return g1
      return(g1)
    } else {
      g1 <- g0 + round(rnorm(1, mean = 0, sd = scale.F), digits = 0)
      ## return g1
      return(g1)
    }
  } else {
    g1 <- g0 + round(rnorm(1, mean = 0, sd = scale.F), digits = 0)
    ## return g1
    return(g1)
  }
}
