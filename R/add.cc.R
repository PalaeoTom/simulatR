#' Add carrying capacity to stage using distributions of parameters derived from Matthews et al. data
#'
#' @param stage A stage object or, if load = TRUE, the name of the stage Rds file as a string.
#' @param load If TRUE and argument "stage" is a string, stage is read in from working directory using the latter as the file name (minus the .Rds suffix). The stage must be stored as an Rds file. Default is FALSE.
#' @param model Species-area model used to calculate carrying capacity. Default is power model (Arrhenius, 1921).
#' @param name.out If export = TRUE, a string that specifies the name of the output file.
#' @param export If TRUE, updated stage with carrying capacities is exported as an Rds file to working directory.
#'
#' @import stats
#'
#' @return A stage object with element "cc" added.
#' @export
#'
#' @examples
#' # create a mock stage object
#' s <- list("regions" = matrix(seq(1,9,1), 3, 3), "dimensions" = matrix(400, 3, 3),
#' "distances" = matrix(20, 9, 9))
#'
#' # check object and assign "stage" class if it conforms.
#' stage <- check.stage(s)
#'
#' # add carrying capacities
#' stage.cc <- add.cc(stage)
add.cc <- function(stage, load = F, model = "power", name.out = "new", export = F){
  ## if load is TRUE, read in stage from Rds object
  if(load){
    ## read in
    stage <- readRDS(paste0(stage, ".Rds"))
    ## check stage
    stage <- check.stage(stage)
    ## isolate dimensions
    dimensions <- stage$"dimensions"
  } else {
    ## check stage
    stage <- check.stage(stage)
    ## isolate dimensions
    dimensions <- stage$"dimensions"
  }
  ## transform area into carrying capacity object using specified model. Default is power.
  if(model == "power"){
    carrying.capacity <- SAR_power(dimensions)
  } else {
    carrying.capacity <- model(dimensions)
  }
  ## append carrying capacity element to new version of stage object
  out <- stage
  out[[length(stage)+1]] <- carrying.capacity
  names(out)[length(stage)+1] <- "carrying.capacity"
  ## export if set
  if(export){
    saveRDS(out, file = paste0(name.out, "_stage.Rds"))
  }
  return(out)
}

