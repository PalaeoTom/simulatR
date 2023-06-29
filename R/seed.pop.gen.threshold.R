#' Default method of assigning starting pop.gen.threshold values to populations
#'
#' Values are drawn from a uniform distribution.
#'
#' @param minimum a numeric value specifying the minimum acceptable pop.gen.threshold number. Default is 0.1.
#'
#' @return a numeric value.
#' @export
#'
#' @examples
#' # Run the function
#' seed.pop.gen()
seed.pop.gen.threshold <- function(minimum = 0.1){
  while(T){
    PGT <- round(runif(1, min = 0, max = 1), digits = 0)
    if(PGT >= minimum) break()
  }
  return(PGT)
}
