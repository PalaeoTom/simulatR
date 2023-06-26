#' Default method of assigning starting genetic heterogeneity values to populations
#'
#' Values are drawn from a uniform distribution bounded between 0 and 1. Only produces nonzero values by default.
#'
#' @param cutoff a numeric value specifying the largest gen.het value that will be re-sampled. Default is 0.
#'
#' @return a numeric value.
#' @export
#'
#' @examples
#' # Run the function
#' seed.gen.het()
seed.genome <- function(cutoff = 0){
  while(T){
    GH <- runif(1, 0, 1)
    if(GH > cutoff) break()
  }
  return(GH)
}
