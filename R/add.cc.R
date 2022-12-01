#' Title
#'
#' @param stage
#' @param load
#' @param model
#' @param name.out
#' @param export
#'
#' @return
#' @export
#'
#' @examples
add.cc <- function(stage, load = F, model = "power", name.out = "new", export = F){
  ## if load is TRUE, read in stage from Rds object
  if(load){
    dimensions <- readRDS(paste0(stage, ".Rds"))$"dimensions"
  } else {
    dimensions <- stage$"dimensions"
  }
  ## transform area into carrying capacity object using specified model. Default is power.
  if(model == "power"){
    carrying.capacity <- power(dimensions)
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

