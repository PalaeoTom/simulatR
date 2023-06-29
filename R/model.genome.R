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
  ## get genome at previous time step
  g0 <- p0$population.variables[[p.ID]][which(names(p0$population.variables[[p.ID]]) == "G")]
  ## scale SD by time elapsed and factor
  scale.F <- (t0-t1)*SF
  ## if pop.gen switched on
  if(any(p0$variable.names == "PGT")){
    ## identify species
    s <- which(sapply(1:length(p0$species.representation), function(x) any(p0$species.representation[[x]] == p.ID)))
    ## get location of population
    r <- which(sapply(1:length(p0$populated.regions), function(x) any(p0$populated.regions[[x]] == p.ID)))
    ## initialise numerator and denominator vectors
    num <- c()
    den <- c()
    ## get location of all other populations of this species
    for (i in p0$species.representation[[s]][!p0$species.representation[[s]] == p.ID]){
      ## find location of other population
      r1 <- which(sapply(1:length(p0$populated.regions), function(x) any(p0$populated.regions[[x]] == i)))
      ## get average dispersal distance of population
      ADD1 <- p0$population.variables[[i]][which(names(p0$population.variables[[i]]) == "ADD")]
      ## is distance equal to or than average dispersal distance multiplied by time
      if(p0$distances[r,r1] <= ADD1*(t0-t1)){
        ## find abundance of considered population
        a1 <- (p0$population.variables[[i]][which(names(p0$population.variables[[i]]) == "abundance")])
        ## find disp.prop of considered population
        DP1 <- p0$population.variables[[i]][which(names(p0$population.variables[[i]]) == "DP")]
        ## find genome of considered population
        g1 <- p0$population.variables[[i]][which(names(p0$population.variables[[i]]) == "G")]
        ## calculate weighted abundance
        wa1 <- unname((((ADD1*(t0-t1))-p0$distances[r,r1])/(ADD1*(t0-t1)))*DP1*a1)
        ## calculate wg value
        wg1 <- unname(wa1*g1)
        ## add to numerators and denominators
        num <- c(num, wg1)
        den <- c(den, wa1)
      } else {
        num <- num
        den <- den
      }
    }
    ## get abundance of p.ID
    a0 <- p0$population.variables[[p.ID]][which(names(p0$population.variables[[p.ID]]) == "abundance")]
    ## calculate weighted genome for species
    wg <- sum(num+(a0*g0))/sum(den+a0)
    ## get rate



    g1 <- g0 + round(rnorm(1, mean = 0, sd = scale.F), digits = 0)
    ## return g1
    return(g1)
  } else {
    g1 <- g0 + round(rnorm(1, mean = 0, sd = scale.F), digits = 0)
    ## return g1
    return(g1)
  }
}

