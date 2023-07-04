#' Default genome model
#'
#' Changes are drawn from normal distributions where the standard deviation is the product of the time passed, and a factor (SF).
#' Capable of inducing changes based on principles of population genetics and abundance.
#'
#' @param p0 populations object from previous time bin.
#' @param p the numeric index of the population in p0$population.variables.
#' @param t0 time of previous time bin.
#' @param t1 time of current time bin.
#' @param SF a numeric value by which the changes in mean genome can be scaled. For flexibility.
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
#' model.genome(p0 = p0, p = sample(1:length(p0$population.variables), 1), t0 = 100, t1 = 90)
model.genome <- function(p0, p, t0, t1, SF = 1){
  ## get genome at previous time step
  g0 <- unname(p0$population.variables[[p]][which(names(p0$population.variables[[p]]) == "G")])
  ## if pop.gen switched on
  if(any(p0$variable.names == "PGT")){
    ## identify species
    s <- which(sapply(1:length(p0$species.representation), function(x) any(p0$species.representation[[x]] == p)))
    ## get location of population
    r <- which(sapply(1:length(p0$populated.regions), function(x) any(p0$populated.regions[[x]] == p)))
    ## initialise numerator and denominator vectors
    num <- c()
    den <- c()
    ## get location of all other populations of this species
    for (i in p0$species.representation[[s]][!p0$species.representation[[s]] == p]){
      ## find location of other population
      r1 <- which(sapply(1:length(p0$populated.regions), function(x) any(p0$populated.regions[[x]] == i)))
      ## get average dispersal distance of population
      ADD1 <- unname(p0$population.variables[[i]][which(names(p0$population.variables[[i]]) == "ADD")])
      ## is distance equal to or than average dispersal distance multiplied by time
      if(p0$distances[r,r1] <= ADD1*(t0-t1)){
        ## find abundance of considered population
        a1 <- unname((p0$population.variables[[i]][which(names(p0$population.variables[[i]]) == "A")]))
        ## find disp.prop of considered population
        DP1 <- unname(p0$population.variables[[i]][which(names(p0$population.variables[[i]]) == "DP")])
        ## find genome of considered population
        g1 <- unname(p0$population.variables[[i]][which(names(p0$population.variables[[i]]) == "G")])
        ## calculate weighted abundance
        wa1 <- (((ADD1*(t0-t1))-p0$distances[r,r1])/(ADD1*(t0-t1)))*DP1*a1
        ## calculate wg value
        wg1 <- wa1*g1
        ## add to numerators and denominators
        num <- c(num, wg1)
        den <- c(den, wa1)
      } else {
        num <- num
        den <- den
      }
    }
    ## get abundance of p
    a0 <- p0$population.variables[[p]][which(names(p0$population.variables[[p]]) == "A")]
    ## calculate weighted genome for species
    if(is.null(num)){
      wg <- g0
    } else {
      wg <- sum(num+(a0*g0))/sum(den+a0)
    }
    ## log(a0) and log(rate have an approximately linear relationship). Use y = mc+c to get rate.
    rate <- 2.718281828^((-0.61258200*log(a0))-11.64115066)
    ## calculate change
    while(TRUE){
      change <- rnorm(1, mean = (wg-g0), sd = rate*(t0-t1)*SF)
      if(change <= (1-g0) && change >= (0-g0)) break()
    }
    g1 <- g0 + change
    ## return g1
    return(g1)
  } else {
    while(TRUE){
      change <- rnorm(1, mean = 0, sd = (t0-t1)*SF)
      if(change <= (1-g0) && change >= (0-g0)) break()
    }
    g1 <- g0 + change
    ## return g1
    return(g1)
  }
}

