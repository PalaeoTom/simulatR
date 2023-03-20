#' Default abundance model
#'
#' Changes are drawn from normal distributions where the standard deviation is the product of the magnitude of the previous abundance, the time passed, and a factor (SF). If min.via.pop is TRUE
#' and the abundance of the population is below MVP.value, then the mean of this normal distribution shifts by the difference.
#'
#' @param a0 a numeric value specifying abundance in previous time bin.
#' @param min.via.pop if TRUE, populations with a0 below MVP.value will have a greater likelihood of declining (the further below MVP.value, the greater the likelihood). Default is FALSE.
#' @param MVP.value the minimum viable population number used if min.via.pop = TRUE.
#' @param t0 time of previous time bin.
#' @param t1 time of current time bin.
#' @param SF a numeric value by which the changes in abundance can be scaled.
#'
#' @return a numeric value.
#' @export
#'
#' @keywords internal
#'
#' @examples
#' # Run the function
#' model.abundance(a0 = 20, t0 = 1, t1 = 2)
model.abundance <- function(a0, min.via.pop = F, MVP.value, t0, t1, SF = 1){
  ## scale SD by time, magnitude of previous abundance and factor
  scale.F <- (10^(floor(log10(a0))))*(t1-t0)*SF
  ## If min.via.pop provided, determine whether abundance is
  if(min.via.pop){
    ## if a0 => MVP.value
    if(a0 >= MVP.value){
      a1 <- a0 + rnorm(1, mean = 0, sd = scale.F)
    } else {
      a1 <- a0 + rnorm(1, mean = a0-MVP.value, sd = scale.F)
    }
    ## return a1
    return(a1)
  } else {
    a1 <- a0 + rnorm(1, mean = 0, sd = scale.F)
    ## return a1
    return(a1)
  }
}

