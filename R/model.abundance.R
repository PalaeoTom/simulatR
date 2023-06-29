#' Default abundance model
#'
#' Changes are drawn from normal distributions where the standard deviation is the product of the time passed, and a factor (SF). If min.via.pop is TRUE
#' and the abundance of the population is below MVP.value, then the mean of this normal distribution shifts by the difference.
#'
#' @param a0 a numeric value specifying abundance in previous time bin.
#' @param min.via.pop if TRUE, populations with a0 below MVP.value will have a greater likelihood of declining (the further below MVP.value, the greater the likelihood). Default is FALSE.
#' @param MVP.value the minimum viable population number used if min.via.pop = TRUE.
#' @param pop.gen if TRUE, populations with g.diff > pg.threshold will tend to decrease.
#' @param g.diff A numeric value. Should be change in population across previous time interval.
#' @param pg.threshold A numeric value specifying the threshold above which change in the mean genome is considered detrimental to the survival of the population. Default is 0.1.
#' @param t0 time of previous time bin.
#' @param t1 time of current time bin.
#' @param SF a numeric value by which the changes in abundance can be scaled.
#'
#' @return a numeric value.
#' @export
#'
#' @keywords internal
#'
model.abundance <- function(a0, min.via.pop = F, MVP.value, pop.gen = F, g.diff, pg.threshold = 0.1, t0, t1, SF = 1){
  ## get a0
  a0 <- p0$population.variables
  ## scale SD by time elapsed and factor
  scale.F <- (t1-t0)*SF
  ## if pop.gen switched on
  if(pop.gen){
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

