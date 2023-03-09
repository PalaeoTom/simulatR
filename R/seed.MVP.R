#' Default method of drawing minimum viable population values from Weibull distribution fitted to Traill et al. data
#'
#' @return a numeric value.
#' @export
#'
#' @keywords internal
#' @references Traill, L. W., Bradshaw, C. J. A., & Brook, B. W. (2007). Minimum viable population size: A meta-analysis of 30 years of published estimates. \emph{Biological Conservation}, 139, 156-166.
#'
#' @examples
#' # Run the function
#' seed.MVP()
seed.MVP <- function(){
  while(T){
    MVP <- round(rweibull(1, scale = 2723.498572, shape = 0.54345227), digits = 0)
    if(MVP >= 10) break()
    }
  return(MVP)
}

