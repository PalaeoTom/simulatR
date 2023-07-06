#' Specify nested model
#'
#' Creates a model object that specifies the details of a model to be used in simulation. Used to build model objects that draw on other models as variables (i.e. nested models).
#' Models should be nested one level at a time. For example, if you want to nest a nested model (i.e. create a level-3 model), first you need to create the level-2 nested model,
#' then submit that as a variable during the creation of the level-3 model. Note that if you submit a list of models as m, these will be referred to in the model by the names provided or
#' in the order they appear in the list. For example, if you don't assign names to your models via names(), the first model in the list will be assigned the identifier "m1", the second "m2", and so forth.
#' Several core stage and simulation elements can be called by including specific strings in the 'variables' vector: 'time' calls the time elapsed between the previous time bin and
#' the present; 'regions' calls the 'regions' stage object (a matrix specifying region numbers); 'dimensions' the 'dimensions' stage object (a matrix specifying the area of each region);
#' 'distances' the 'distances' stage object (a square matrix specifying the distances separating each region).
#'
#' @param s A stage object
#' @param p A populations object
#' @param m Either a model object, or a list of model objects.
#' @param type Either "binary" or "continuous".
#' @param variables A character vector containing the names of the population variables, stage variables, or NA (where there are no variables involved).
#' @param expression A character vector specifying the construction of the model as a single string. If m is a model, it should be called in the string via 'm'. If m is a list of models, the models should be specified by their names (names(m), or their position in the list with the prefix 'm' ('m1', 'm2', and so forth).
#' @param ID A character string assigning a unique ID to the model. Default is "new.model'. Note that all IDs provided will have a suffix appended denoting their level (e.g. a level-2 model with ID "m1" will be labelled "m1.2").
#'
#' @return a model object
#' @export
#'
#' @examples
#' #' # make stage
#' s <- make.stage(n.col = 5, n.row = 5, ar = 400)
#'
#' add a random variable to stage
#' s <- add.var.stage(s, var = matrix(runif(25, min = 10, max = 50), 5, 5), var.name = "SV1")
#'
#' # define population variables
#' PVs <- set.pop.var.seeds(min.via.pop = "off", new.var.name = "PV1", new.var.seed = function() runif(1,10,50))
#'
#' # generate seed populations
#' p0 <- gen.seed.pops(stage = s, pop.var.seeds = PVs, n = 10)
#'
#' # define model
#' m1 <- specify.model(s = s, p = p0, type = "binary", variables = c("SV1", "PV1"), expression = "PV1 <= SV1", ID = "m1")
#'
#' # define second model
#' m2 <- specify.model(s = s, p = p0, type = "binary", variables = c("SV1", "PV1"), expression = "PV1 <= SV1", ID = "m2")
#'
#' # get list
#' m.list <- list(m1, m2)
#'
#' # build nested model
#' m3 <- specify.nested.model(s = s, p = p0, m = m.list, type = "binary", variables = c("SV1", "PV1"), expression = "PV1*m1.1 <= SV1*m2.1", ID = "m3")
specify.nested.model <- function(s, p, m, type, variables, expression, ID = "new.model"){
  ## Check type is one of three possibilities
  if(!type=="binary"&&!type=="continuous"){
    stop("argument 'type' is not 'binary' or 'continuous'")
  }
  ## Check variables argument is a character vector
  if(!is.character(variables)&&!is.na(variables)){
    stop("argument 'variables' is not a character vector or NA")
  }
  ## Check expression is a character vector
  if(!is.character(expression)){
    stop("argument 'expression' is not a character vector")
  }
  ## Check expression is only one string
  if(length(expression)>1){
    stop("argument 'expression' is not a single string")
  }
  ## Check s is a stage
  if(!class(s)=="stage"){
    stop("s is not a stage object")
  }
  ## check p is a populations object
  if(!class(p)=="populations"){
    stop("p is not a populations object")
  }
  ## check m is a model object
  if(!class(m)=="model"){
    ## if not is it a list?
    if(!is.list(m)){
      stop("m is not a model or a list")
    }
    ## if it is a list, is it a list of models?
    for(i in 1:length(m)){
      if(!class(m[[i]]) == "model"){
        stop("one or more elements of list m are not model objects")
      }
    }
    ## if it is a list of models, assign IDs as names
    names(m) <- sapply(1:length(m), function(x) m[[x]]$ID)
  }
  ## Check variables are present in either s or p
  if(!all(is.na(variables))){
    if(any(is.na(match(variables,c(s$variable.names, p$variable.names, "time", "regions", "distances", "dimensions"))))){
      stop("one or more variables specified are not included in stage or populations objects provided")
    }
  }
  ## Get model level
  if(class(m) == "model"){
    level <- m$level+1
  } else {
    level <- max(sapply(1:length(m), function(x) m[[x]]$level))+1
  }
  ## Concatenate all model IDs into single vector
  # Single nested model
  if(class(m) == "model"){
    # If level 1
    if(m$level == 1){
      nested.m <- c(m$ID)
    } else {
      # if level 2 or higher
      nested.m <- c(m$ID,m$nested.models)
    }
  } else {
    nested.m <- c()
    for(i in 1:length(m)){
      # If level 1
      if(m[[i]]$level == 1){
        nested.m <- c(nested.m,m[[i]]$ID)
      } else {
        # if level 2 or higher
        nested.m <- c(nested.m,m[[i]]$ID,m[[i]]$nested.models)
      }
    }
  }
  ## re-order
  nested.m <- nested.m[order(nested.m)]
  ## Finally, if above level-2, level-2 model's models object should be combined with that of level-3 so they're all in one place
  if(level>2){
    ## if 1 nested level-2 model
    if(class(m) == "model"){
      models.out <- m$models
      m$models <- NULL
      ## combine with updated m for final models.out
      models.out <- c(m, models.out)
      ## update names
      names(models.out) <- sapply(1:length(models.out), function(z) models.out[[z]]$ID)
    } else {
      ## find the level 2 models
      l2.m <- which(sapply(1:length(m), function(x) m[[x]]$level) == 2)
      ## generate models.out object
      models.out <- c()
      ##
      for(i in l2.m){
        models.out <- c(models.out, m[[i]]$models)
        m[[i]]$models <- NULL
      }
      ## combine with updated m for final models.out
      models.out <- c(m, models.out)
      ## update names
      names(models.out) <- sapply(1:length(models.out), function(z) models.out[[z]]$ID)
    }
  } else {
    if(class(m) == "model"){
      models.out <- list(m)
    } else {
      models.out <- m
      models.out <- models.out[order(names(models.out))]
    }
  }
  ## Assemble into model structure
  out <- list(
    "ID" = paste0(ID,".",level),
    "type" = type,
    "level" = level,
    "variables" = variables,
    "nested.models" = nested.m,
    "models" = models.out,
    "expression" = expression)
  ## Assign model class
  out <- structure(out, class = "model")
}
