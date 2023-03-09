#' Default method of assigning starting vagility values to populations
#'
#' Values are drawn from a uniform distribution bounded between 0 and 1. Only produces nonzero values by default.
#'
#' @param cutoff a numeric value specifying the largest vagility value that will be re-sampled. Default is 0.
#'
#' @return a numeric value.
#' @export
#'
#' @keywords internal
#'
#' @examples
#' # Run the function
#' seed.vagility()
seed.vagility <- function(cutoff = 0){
  while(T){
    V <- runif(1, 0, 1)
    if(V > cutoff) break()
  }
  return(V)
}
