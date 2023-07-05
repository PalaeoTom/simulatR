#' Apply inhertiance models
#'
#' Applies inheritance models described in pop.var.inher object and returns an updated populations object.
#'
#' @param s a stage object.
#' @param p0 a populations object
#' @param new.spec a vector specifying which populations (by their index in p0$population.variables) have speciated this time bin.
#' @param pop.var.inher a pop.var.inher object
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
#' # define population inhertiance models
#' PVi <- set.pop.var.inher(pop.var.seeds = PVs, new.var.inher = list("PV1" = "fixed"))
#'
#' # generate seed populations
#' p0 <- gen.seed.pops(stage = s, pop.var.seeds = PVs, n = 10)
#'
#' # run function
#' p1 <- apply.inher(s = s, p0 = p0, pop.var.inher = PVi, t0 = 100, t1 = 90)
apply.inher <- function(s, p0, new.spec, pop.var.inher, t0, t1, export = F, name.out = "new"){
  ## check s is a stage object
  if(!class(s)=="stage"){
    stop("s is not a stage object")
  }
  ## check pop.var.inher is a pop.var.inher object
  if(!class(pop.var.inher)=="pop.var.inher"){
    stop("pop.var.inher is not a pop.var.inher object")
  }
  ## initialise new populations object
  p1 <- p0
  ## for each model included in pop.var.inher
  for(i in pop.var.inher$variable.names){
    ## Break if not present
    if(!i %in% p0$variable.names){
      stop(paste0(i, " is not included in p0"))
    }
    ## If not a function or a model, leave
    if(!class(pop.var.inher[[i]]) == "model" && !is.function(pop.var.inher[[i]])){
      for(j in new.spec){
        ## Retain same value
        p1$population.variables[[j]][i] <- p1$population.variables[[j]][i]
      }
    }
    ## If a model, implement across all populations
    if(class(pop.var.inher[[i]]) == "model"){
      ## if level 1, use parse.model
      if(pop.var.inher[[i]]$level == 1){
        for(j in new.spec){
          ## find r
          r <- which(sapply(1:length(p0$populated.regions), function(x) any(p0$populated.regions[[x]] == j)))
          ## update using parse.model
          p1$population.variables[[j]][i] <- parse.model(m = pop.var.inher[[i]], s = s, p0 = p0, p = j, r = r)
        }
      } else {
        for(j in new.spec){
          ## find r
          r <- which(sapply(1:length(p0$populated.regions), function(x) any(p0$populated.regions[[x]] == j)))
          ## update using parse.model
          p1$population.variables[[j]][i] <- parse.nested.model(m = pop.var.inher[[i]], s = s, p0 = p0, p = j, r = r)
        }
      }
    }
    ## If a function, implement across all populations
    if(!class(pop.var.inher[[i]]) == "model" && is.function(pop.var.inher[[i]])){
      for(j in new.spec){
        ## update using parse.model
        p1$population.variables[[j]][i] <- pop.var.inher[[i]](p0 = p0, p = j, t0 = t0, t1 = t1, SF = 1)
      }
    }
  }
  ## If PGR is a variable, update to latest rate per unit time
  if(any(p0$variable.names == "PGR")){
    for(j in new.spec){
      ## update with rate from previous interval - assumed to be constant
      p1$population.variables[[j]]["PGR"] <- abs(p0$population.variables[[j]]["G"]-p1$population.variables[[j]]["G"])/abs(t1-t0)
    }
  }
  ## export if set
  if(export){
    saveRDS(p1, file = paste0(name.out, "_", t1, "_pops.Rds"))
  } else {
    return(p1)
  }
}
