#' Set population inheritance models
#'
#' set.pop.var.models generates a pop.var.inher object which specifies which population variables change at speciation events and the models that define how they do so.
#' Uses pop.var.seeds to determine which models to load. By default, defines models for changes in population avg.disp.dist, disp.prop, and min.via.pop, and sets all other core
#' population variables to fixed (i.e. do not change during speciation event). If model objects are provided, ensure they return the new numeric value for the variable at the time as their output (i.e the highest level model is continuous).
#'
#' @param pop.var.seeds A pop.var.seeds object, specifying which population variables.
#' @param abundance Either "fixed" (the default) or a model object (built using specify.model or specify.nested.model).
#' @param genome Either "fixed" (the default) or a model object (built using specify.model or specify.nested.model).
#' @param avg.disp.dist Either "default" (the default), a model object (built using specify.model or specify.nested.model), or "fixed".
#' @param disp.prop Either "default" (the default), a model object (built using specify.model or specify.nested.model), or "fixed".
#' @param min.via.pop Either "default" (the default), a model object (built using specify.model or specify.nested.model), or "fixed".
#' @param pop.gen.threshold Either "fixed" (the default), "default", or a model object (built using specify.model or specify.nested.model).
#' @param new.var.inher A list, where each element specifies the prescribed inheritance model for each non-standard variable included. Each element of the list should share the name of the variable it pertains to. List elements should either be "fixed" or a model object (built using specify.model or specify.nested.model).
#' @param name.out If export = TRUE, a string that specifies the name of the output file.
#' @param export If TRUE, updated stage with new variable is exported as an Rds file to working directory.
#'
#' @return A pop.var.inher object.
#' @export
#'
#' @examples
#' # Generate pop.var.seeds object
#' pop.seeds <- set.pop.var.seeds()
#'
#' # Generate pop.var.models object
#' pop.models <- set.pop.var.inher(pop.var.seeds = pop.seeds)
set.pop.var.inher <- function(pop.var.seeds, abundance = "fixed", genome = "fixed", avg.disp.dist = "default", disp.prop = "default", min.via.pop = "default", pop.gen.threshold = "fixed", new.var.inher = NULL, name.out = "new", export = F){
  ## base function for assigning abundance model
  if(!class(abundance) == "model" && !abundance == "fixed"){
    stop("abundance is not a model object or 'fixed'")
  } else {
    inherit.A <- abundance
  }
  ## base function for assigning genome model
  if(!class(genome) == "model" && !genome == "fixed"){
    stop("genome is not a model object or 'fixed'")
  } else {
    inherit.G <- genome
  }
  ## base function for assigning avg.disp.dist model
  if(avg.disp.dist == "default") inherit.ADD <- inherit.avg.disp.dist
  if(class(avg.disp.dist) == "model" | avg.disp.dist == "fixed") inherit.ADD <- avg.disp.dist
  if(!avg.disp.dist == "default" && !class(avg.disp.dist) == "model" && !avg.disp.dist == "fixed"){
    stop("avg.disp.dist is not a model object, 'default', or 'fixed'")
  }
  ## base function for assigning disp.prop model
  if(disp.prop == "default") inherit.DP <- inherit.disp.prop
  if(class(disp.prop) == "model" | disp.prop == "fixed") inherit.DP <- disp.prop
  if(!disp.prop == "default" && !class(disp.prop) == "model" && !disp.prop == "fixed"){
    stop("disp.prop is not a model object, 'default', or 'fixed'")
  }
  ## base function for assigning min.via.pop model
  if(min.via.pop == "default") inherit.MVP <- inherit.min.via.pop
  if(class(min.via.pop) == "model" | min.via.pop == "fixed") inherit.MVP <- min.via.pop
  if(!min.via.pop == "default" && !class(min.via.pop) == "model" && !min.via.pop == "fixed"){
    stop("min.via.pop is not a model object, 'default', or 'fixed'")
  }
  ## base function for assigning pop.gen.threshold model
  if(pop.gen.threshold == "default") inherit.PGT <- inherit.pop.gen.threshold
  if(class(pop.gen.threshold) == "model" | pop.gen.threshold == "fixed") inherit.PGT <- pop.gen.threshold
  if(!pop.gen.threshold == "default" && !class(pop.gen.threshold) == "model" && !pop.gen.threshold == "fixed"){
    stop("pop.gen.threshold is not a model object, 'default', or 'fixed'")
  }
  ## configure final output if additional variables are provided
  if(!is.null(new.var.inher)){
    # check new var seed method is a list
    if(!is.list(new.var.inher)){
      stop("new.var.inher is not a list")
    }
    # check each element is a model or "fixed"
    for(i in 1:length(new.var.inher)){
      if(!class(new.var.inher[[i]]) == "model" && !new.var.inher[[i]] == "fixed"){
        stop("1 or more elements of new.var.inher are not 'fixed' or a model object")
      }
    }
    # now checks are complete, create final output as list
    output <- c(inherit.ADD, inherit.DP, inherit.A, inherit.G, inherit.PGT, inherit.MVP, new.var.inher)
    names(output) <- c("ADD", "DP",  "A", "G", "PGT", "MVP", names(new.var.inher))
    # drop variables not included in pop.var.seeds$variable.names object
    output <- output[names(output) %in% pop.var.seeds$variable.names]
  } else {
    output <- c(inherit.ADD, inherit.DP, inherit.A, inherit.G, inherit.PGT, inherit.MVP)
    names(output) <- c("ADD", "DP", "A", "G", "PGT", "MVP")
    # drop variables not included in pop.var.seeds$variable.names object
    output <- output[names(output) %in% pop.var.seeds$variable.names]
  }
  ## add element with names
  variables <- names(output)
  ## add to end of output
  final <- c(output, "variable.names" = list(variables))
  ## assign model object class
  final <- structure(final, class = "pop.var.inher")
  ## export if set
  if(export){
    saveRDS(final, file = paste0(name.out, "_pop_var_inher.Rds"))
  } else {
    return(final)
  }
}
