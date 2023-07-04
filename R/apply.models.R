#' Apply models
#'
#' Applies models described in pop.var.models object and returns an updated populations object.
#'
#' @param s a stage object.
#' @param p0 a populations object
#' @param pop.var.models a pop.var.models object
#' @param t0 a numeric specifying the time of the previous interval.
#' @param t1 a numeric specifying the time of the current interval.
#' @param export If TRUE, stage will be saved as an Rds file. Default is FALSE.
#' @param name.out A string to be included in the Rds output file name if export = TRUE. Default is "new".
#'
#' @return A populations object.
#' @export
#'
#' @examples
#' # make stage
#' s <- make.stage(n.col = 5, n.row = 5, ar = 400)
#'
#' # add a random variable to stage
#' s <- add.var.stage(s, var = matrix(runif(25, min = 10, max = 50), 5, 5), var.name = "SV1")
#'
#' # define population variable seeds
#' PVs <- set.pop.var.seeds(min.via.pop = "off", new.var.name = "PV1", new.var.seed = function() runif(1,10,50))
#'
#' # define population models
#' PVm <- set.pop.var.models(pop.var.seeds = PVs, new.var.model = list("PV1" = "fixed"))
#'
#' # generate seed populations
#' p0 <- gen.seed.pops(stage = s, pop.var.seeds = PVs, n = 10)
#'
#' # run function
#' p1 <- apply.models(s = s, p0 = p0, pop.var.models = PVm, t0 = 100, t1 = 90)
apply.models <- function(s, p0, pop.var.models, t0, t1, export = F, name.out = "new"){
  ## check s is a s object
  if(!class(s)=="stage"){
    stop("s is not a stage object")
  }
  ## check pop.var.models is a pop.var.models object
  if(!class(pop.var.models)=="pop.var.models"){
    stop("pop.var.models is not a pop.var.models object")
  }
  ## initialise new populations object
  p1 <- p0
  ## for each model included in pop.var.models
  for(i in pop.var.models$variable.names){
    ## Break if not present
    if(!i %in% p0$variable.names){
      stop(paste0(i, " is not included in p0"))
    }
    ## If fixed, leave
    if(pop.var.models[[i]] == "fixed"){
      next()
    }
    ## If a model, implement across all populations
    if(class(pop.var.models[[i]]) == "model"){
      ## if level 1, use parse.model
      if(pop.var.models[[i]]$level == 1){
        for(j in unlist(p0$populated.regions)){
          ## find r
          r <- which(sapply(1:length(p0$populated.regions), function(x) any(p0$populated.regions[[x]] == j)))
          ## update using parse.model
          p1$population.variables[[j]][i] <- parse.model(m = pop.var.models[[i]], s = s, p0 = p0, p = j, r = r)
        }
      } else {
        for(j in unlist(p0$populated.regions)){
          ## find r
          r <- which(sapply(1:length(p0$populated.regions), function(x) any(p0$populated.regions[[x]] == j)))
          ## update using parse.model
          p1$population.variables[[j]][i] <- parse.nested.model(m = pop.var.models[[i]], s = s, p0 = p0, p = j, r = r)
        }
      }
    }
    ## If a function, implement across all populations
    if(!class(pop.var.models[[i]] == "model") && is.function(pop.var.models[[i]])){
      for(j in unlist(p0$populated.regions)){
        ## update using parse.model
        p1$population.variables[[j]][i] <- pop.var.models[[i]](p0 = p0, p = j, t0 = t0, t1 = t1, SF = 1)
      }
    }
  }
  ## export if set
  if(export){
    saveRDS(p1, file = paste0(name.out, "_", t1, "_pops.Rds"))
  } else {
    return(p1)
  }
}
