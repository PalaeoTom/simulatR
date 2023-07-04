#' Set population models
#'
#' set.pop.var.models generates a pop.var.models object which specifies which population variables change over time and the models that define how they do so.
#' Uses pop.var.seeds to determine which models to load. By default, defines models for change in population abundance and mean genome, and sets all other core
#' population variables to fixed. If model objects are provided, ensure they return the new numeric value for the variable at the time as their output (i.e the highest level model is continuous).
#'
#' @param pop.var.seeds A pop.var.seeds object, specifying which population variables.
#' @param abundance Either "default" (the default), a model object (built using specify.model or specify.nested.model), or "fixed".
#' @param genome Either "default" (the default), a model object (built using specify.model or specify.nested.model),  or "fixed".
#' @param avg.disp.dist Either "fixed" (the default), or a model object (built using specify.model or specify.nested.model).
#' @param disp.prop Either "fixed" (the default), or a model object (built using specify.model or specify.nested.model).
#' @param min.via.pop Either "fixed" (the default), or a model object (built using specify.model or specify.nested.model).
#' @param pop.gen.threshold Either "fixed" (the default), or a model object (built using specify.model or specify.nested.model).
#' @param new.var.model A list, where each element specifies the prescribed model for each non-standard variable included. Each element of the list should share the name of the variable it pertains to. List elements should either be "fixed" (if variable is unchanging), a model object (built using specify.model or specify.nested.model), or a function which specifies how the new value is derived.
#' @param name.out If export = TRUE, a string that specifies the name of the output file.
#' @param export If TRUE, updated stage with new variable is exported as an Rds file to working directory.
#'
#' @return A pop.var.models object.
#' @export
#'
#' @examples
#' # Generate pop.var.seeds object
#' pop.seeds <- set.pop.var.seeds()
#'
#' # Generate pop.var.models object
#' pop.models <- set.pop.var.models(pop.var.seeds = pop.seeds)
set.pop.var.models <- function(pop.var.seeds, abundance = "default", genome = "default", avg.disp.dist = "fixed", disp.prop = "fixed", min.via.pop = "fixed", pop.gen.threshold = "fixed", new.var.model = NULL, name.out = "new", export = F){
  ## base function for assigning abundance model
  if(abundance == "default") model.A <- model.abundance
  if(class(abundance) == "model" | abundance == "fixed") model.A <- abundance
  if(!abundance == "default" && !class(abundance) == "model" && !abundance == "fixed"){
    stop("abundance is not a model object, 'default', or 'fixed'")
  }
  ## base function for assigning genome model
  if(genome == "default") model.G <- model.genome
  if(class(genome) == "model" | genome == "fixed") model.G <- genome
  if(!genome == "default" && !class(genome) == "model" && !genome == "fixed"){
    stop("genome is not a model object, 'default', or 'fixed'")
  }
  ## base function for assigning avg.disp.dist model
  if(!class(avg.disp.dist) == "model" && !avg.disp.dist == "fixed"){
    stop("avg.disp.dist is not a model object or 'fixed'")
  } else {
    model.ADD <- avg.disp.dist
  }
  ## base function for assigning disp.prop model
  if(!class(disp.prop) == "model" && !disp.prop == "fixed"){
    stop("disp.prop is not a model object or 'fixed'")
  } else {
    model.DP <- disp.prop
  }
  ## base function for assigning min.via.pop model
  if(!class(min.via.pop) == "model" && !min.via.pop == "fixed"){
    stop("min.via.pop is not a model object or 'fixed'")
  } else {
    model.MVP <- min.via.pop
  }
  ## base function for assigning pop.gen.threshold model
  if(!class(pop.gen.threshold) == "model" && !pop.gen.threshold == "fixed"){
    stop("pop.gen.threshold is not a model object or 'fixed'")
  } else {
    model.PGT <- pop.gen.threshold
  }
  ## configure final output if additional variables are provided
  if(!is.null(new.var.model)){
    # check new var seed method is a list
    if(!is.list(new.var.model)){
      stop("new.var.model is not a list")
    }
    # check each element is a model or "fixed"
    for(i in 1:length(new.var.model)){
      if(!class(new.var.model[[i]]) == "model" && !new.var.model[[i]] == "fixed"){
        stop("1 or more elements of new.var.model are not 'fixed' or a model object")
        }
    }
    # now checks are complete, create final output as list
    output <- c(model.ADD, model.DP, model.A, model.G, model.PGT, model.MVP, new.var.model)
    names(output) <- c("ADD", "DP",  "A", "G", "PGT", "MVP", names(new.var.model))
    # drop variables not included in pop.var.seeds$variable.names object
    output <- output[names(output) %in% pop.var.seeds$variable.names]
  } else {
    output <- c(model.ADD, model.DP, model.A, model.G, model.PGT, model.MVP)
    names(output) <- c("ADD", "DP", "A", "G", "PGT", "MVP")
    # drop variables not included in pop.var.seeds$variable.names object
    output <- output[names(output) %in% pop.var.seeds$variable.names]
  }
  ## assign model object class
  final <- structure(output, class = "pop.var.models")
  ## export if set
  if(export){
    saveRDS(final, file = paste0(name.out, "_pop_var_models.Rds"))
  } else {
    return(final)
  }
}
