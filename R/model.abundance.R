#' Default abundance model
#'
#' Changes are drawn from normal distributions where the standard deviation is the product of the time passed, and a factor (SF). If min.via.pop is TRUE
#' and the abundance of the population is below MVP.value, then the mean of this normal distribution shifts by the difference.
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
model.abundance <- function(p0, p.ID, t.series, t0, SF = 1){
  ## get new time
  t1 <- t.series[which(t.series == t0)+1]
  ## identify population in question
  p.ind <- which(names(p0$population.variables) == p.ID)
  ## get abundance at previous time step
  a0 <- p0$population.variables[[p.ind]][which(names(p0$population.variables[[p.ind]]) == "abundance")]
  ## scale SD by time elapsed and factor
  scale.F <- (t1-t0)*SF
  ## if pop.gen switched on
  if(any(p0$variable.names == "PGT")){
    if(g.diff > pg.threshold){
      ## If min.via.pop provided, determine whether abundance is
      if(min.via.pop){
        ## if a0 => MVP.value
        if(a0 >= MVP.value){
          a1 <- a0 + round(rnorm(1, mean = a0*(g.diff-pg.threshold), sd = scale.F), digits = 0)
        } else {
          a1 <- a0 + round(rnorm(1, mean = a0-(MVP.value+(a0*(g.diff-pg.threshold))), sd = scale.F), digits = 0)
        }
        ## return a1
        return(a1)
      } else {
        a1 <- a0 + round(rnorm(1, mean = a0*(g.diff-pg.threshold), sd = scale.F), digits = 0)
        ## return a1
        return(a1)
      }
    } else {
      ## If min.via.pop provided, determine whether abundance is
      if(min.via.pop){
        ## if a0 => MVP.value
        if(a0 >= MVP.value){
          a1 <- a0 + round(rnorm(1, mean = 0, sd = scale.F), digits = 0)
        } else {
          a1 <- a0 + round(rnorm(1, mean = a0-MVP.value, sd = scale.F), digits = 0)
        }
        ## return a1
        return(a1)
      } else {
        a1 <- a0 + round(rnorm(1, mean = 0, sd = scale.F), digits = 0)
        ## return a1
        return(a1)
      }
    }
  } else {
    ## If min.via.pop provided, determine whether abundance is
    if(min.via.pop){
      ## if a0 => MVP.value
      if(a0 >= MVP.value){
        a1 <- a0 + round(rnorm(1, mean = 0, sd = scale.F), digits = 0)
      } else {
        a1 <- a0 + round(rnorm(1, mean = a0-MVP.value, sd = scale.F), digits = 0)
      }
      ## return a1
      return(a1)
    } else {
      a1 <- a0 + round(rnorm(1, mean = 0, sd = scale.F), digits = 0)
      ## return a1
      return(a1)
    }

  }
}

