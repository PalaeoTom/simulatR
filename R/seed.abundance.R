#' Default method of assigning starting abundance values to populations
#'
#' Values are drawn from a Weibull distribution fitted to the data of Traill et al. Minimum value returned is 10 (the minimum estimate derived from the data of Traill et al.).
#'
#' @param minimum a numeric value specifying the minimum acceptable abundance number. Default is 10.
#'
#' @return a numeric value.
#' @export
#'
#' @references Traill, L. W., Bradshaw, C. J. A., & Brook, B. W. (2007). Minimum viable population size: A meta-analysis of 30 years of published estimates. \emph{Biological Conservation}, 139, 156-166.
#'
#' @examples
#' # Run the function
#' seed.abundance()
seed.abundance <- function(minimum = 10){
  while(T){
    A <- round(rweibull(1, scale = 2723.498572, shape = 0.54345227), digits = 0)
    if(A >= minimum) break()
  }
  return(A)
}
