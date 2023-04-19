#' Default method of assigning dispersal propensity values to populations
#'
#' Values are drawn from a uniform distribution bounded between 0 and 1. Only produces nonzero values by default.
#'
#' @param cutoff a numeric value specifying the largest avg.disp.prop value that will be re-sampled. Default is 0.
#'
#' @return a numeric value.
#' @export
#'
#' @keywords internal
#'
#' @examples
#' # Run the function
#' seed.disp.prop()
seed.disp.prop <- function(cutoff = 0){
  while(T){
    DP <- runif(1, 0, 1)
    if(DP > cutoff) break()
  }
  return(DP)
}
