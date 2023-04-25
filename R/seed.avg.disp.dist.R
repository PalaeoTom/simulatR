#' Default method of assigning starting avg.disp.dist values to populations
#'
#' Values are drawn from a gamma distribution fitted to the data of Kinlan & Gaines (2003).
#'
#' @param cutoff a numeric value specifying the largest avg.disp.dist value that will be re-sampled. Default is 0.01 (10m/yr, minimum of marine datasets, rounded up).
#'
#' @return a numeric value.
#' @export
#'
#' @references Kinlan, B. P. & Gaines, S. D. (2003). Propagule dispersal in marine and terrestrial environments: a community perspective. \emph{Ecology}, 84, 2007-2020.
#'
#' @examples
#' # Run the function
#' seed.avg.disp.dist()
seed.avg.disp.dist <- function(cutoff = 0.01){
  while(T){
    D <- rgamma(1, shape = 0.380272201114083, rate = 0.00639846233090385)
    if(D > cutoff) break()
  }
  return(D)
}
