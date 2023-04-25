#' Default method of assigning starting avg.disp.dist values to populations
#'
#' Values are drawn from a gamma distribution fitted to the data of Kinlan & Gaines (2003).
#'
#' @param cutoff a numeric value specifying the largest avg.disp.dist value that will be re-sampled. Default is 0.006 (minimum of datasets).
#'
#' @return a numeric value.
#' @export
#'
#' @references Kinlan, B. P. & Gaines, S. D. (2003). Propagule dispersal in marine and terrestrial environments: a community perspective. \emph{Ecology}, 84, 2007-2020.
#' @keywords internal
#'
#' @examples
#' # Run the function
#' seed.avg.disp.dist()
seed.avg.disp.dist <- function(cutoff = 0.006){
  while(T){
    D <- rgamma(1, shape = 0.325417052044176, rate = 0.0065853463868577)
    if(D > cutoff) break()
  }
  return(D)
}
