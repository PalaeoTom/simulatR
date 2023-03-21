#' Set population variables
#'
#' set.pop.var.seeds generates a pop.var.seeds object which specifies the population variables for a simulation and defines how their initial values will be derived. Core variables
#' are abundance, genetic heterogeneity (gen.het), minimum viable population (min.via.pop), and vagility. By default, abundance and minimum viable population numbers are
#' drawn from a Weibull distribution fitted to the data of Traill et al., with values below 10 being resampled, while vagility and gen.het values are derived from uniform distributions
#' bounded between 0 and 1, with values of 0 being resampled. All four core variables are included in the output by default. However, they can be switched off or replaced with custom functions if desired.
#' TThis function also allows for additional custom population variables to be added as input-less functions.
#'
#' @param vagility Either "default" (the default), "off", or a function which specifies how the initial value is derived for a population.
#' @param abundance Either "default" (the default), "off", or a function which specifies how the initial value is derived for a population.
#' @param gen.het Either "default" (the default), "off", or a function which specifies how the initial value is derived for a population.
#' @param min.via.pop Either "default" (the default), "off", or a function which specifies how the initial value is derived for a population.
#' @param stable.seeds If TRUE, all seed abundance values will be greater than the minimum viable population numbers. Default is FALSE.
#' @param new.var.name If new.var.seed is a function or list of functions, a string, or list of strings, specifying the name(s) of the new population variables.
#' @param new.var.seed A function or list of functions specifying how the initial values for the new variables are derived for a population.
#' @param name.out If export = TRUE, a string that specifies the name of the output file.
#' @param export If TRUE, updated stage with new variable is exported as an Rds file to working directory.
#'
#' @references Traill, L. W., Bradshaw, C. J. A., & Brook, B. W. (2007). Minimum viable population size: A meta-analysis of 30 years of published estimates. \emph{Biological Conservation}, 139, 156-166.
#'
#' @return A pop.var.seeds object.
#' @export
#'
#' @examples
#' # Generate pop.var.seeds object
#' pop.seeds <- set.pop.var.seeds
#'
set.pop.var.seeds <- function(vagility = "default", abundance = "default", gen.het = "default", min.via.pop = "default", stable.seeds = F, new.var.name = "new.variable", new.var.seed = NULL, name.out = "new", export = F){
  ## base function for seeding population vagility
  if(vagility == "default") seed.V <- seed.vagility
  if(vagility == "off") seed.V <- NA
  if(!vagility == "default" && !vagility == "off" && is.function(vagility)){
    seed.V <- vagility
  }
  if(!vagility == "default" && !vagility == "off" && !is.function(vagility)){
    stop("provided vagility seeding method is not a function")
  }
  ## base function for seeding population gen.het
  if(gen.het == "default") seed.GH <- seed.gen.het
  if(gen.het == "off") seed.GH <- NA
  if(!gen.het == "default" && !gen.het == "off" && is.function(gen.het)){
    seed.GH <- gen.het
  }
  if(!gen.het == "default" && !gen.het == "off" && !is.function(gen.het)){
    stop("provided gen.het seeding method is not a function")
  }
  ## MVP either default or custom, abundance off - error message
  if(!min.via.pop == "off" && abundance == "off"){
    stop("abundance switched off but min.via.pop left on. Either switch min.via.pop off or change abundance to 'default'/a function")
  }
  ## default min.via.pop
  if(min.via.pop == "default") seed.MVP <- seed.min.via.pop
  ## min.via.pop off
  if(min.via.pop == "off") seed.MVP <- NA
  ## custom min.via.pop
  if(!min.via.pop == "default" && !min.via.pop == "off" && is.function(min.via.pop)){
    seed.MVP <- min.via.pop
  }
  if(!min.via.pop == "default" && !min.via.pop == "off" && !is.function(min.via.pop)){
    stop("provided min.via.pop seeding method is not a function")
  }
  ## default abundance
  if(abundance == "default") seed.A <- seed.abundance
  ## abundance off
  if(abundance == "off") seed.A <- NA
  ## custom abundance
  if(!abundance == "default" && !abundance == "off" && is.function(abundance)){
    seed.A <- abundance
  }
  if(!abundance == "default" && !abundance == "off" && !is.function(abundance)){
    stop("provided abundance seeding method is not a function")
  }
  ## if stable.seeds = T
  if(stable.seeds){
    ## if stable.seeds = T but EITHER abundance or MVP are "off", stop
    if(abundance == "off" | min.via.pop == "off"){
      stop("stable.seeds set to TRUE but abundance or min.via.pop switched off. Either change stable.seeds to FALSE, or ensure abundance and min.via.pop are 'default'/a function")
    }
    ## define function which returns both MVP and abundance
    seed.MVP.abundance <- function(){
      ## get MVP
      MVP <- seed.MVP()
      ## get abundance
      while(T){
        A <- seed.A()
        if(A >= MVP) break()
      }
      ## create out
      out <- list(MVP, A)
      names(out) <- c("min.via.pop", "abundance")
      return(out)
    }
      ## configure final output if additional variables are provided
      if(!is.null(new.var.seed)){
        # check new var seed method is a function if just one new variable provided
        if(length(new.var.seed) == 1 && !is.function(new.var.seed)){
          stop("new.var.seed is not a function")
        }
        # if list provided, check each element is a function
        if(length(new.var.seed) > 1 && any(unlist(lapply(1:length(new.var.seed), function(x) !is.function(new.var.seed[[x]]))))){
          stop("one of the elements of new.var.seed is not a function")
        }
        # now checks are complete, create final output as list
        output <- c(seed.V, seed.GH, seed.MVP.abundance, new.var.seed)
        names(output) <- c("vagility", "gen.het", "min.via.pop.abundance", new.var.name)
        output <- structure(output[!is.na(output)], class = "pop.var.seeds")
      } else {
        output <- c(seed.V, seed.GH, seed.MVP.abundance)
        names(output) <- c("vagility", "gen.het", "min.via.pop.abundance")
        output <- structure(output[!is.na(output)], class = "pop.var.seeds")
      }
    } else {
      ## configure final output if additional variables are provided
      if(!is.null(new.var.seed)){
        # check new var seed method is a function if just one new variable provided
        if(length(new.var.seed) == 1 && !is.function(new.var.seed)){
          stop("new.var.seed is not a function")
        }
        # if list provided, check each element is a function
        if(length(new.var.seed) > 1 && any(unlist(lapply(1:length(new.var.seed), function(x) !is.function(new.var.seed[[x]]))))){
          stop("one of the elements of new.var.seed is not a function")
        }
        # now checks are complete, create final output as list
        output <- c(seed.V, seed.GH, seed.MVP, seed.A, new.var.seed)
        names(output) <- c("vagility", "gen.het", "min.via.pop", "abundance", new.var.name)
        output <- structure(output[!is.na(output)], class = "pop.var.seeds")
      } else {
        output <- c(seed.V, seed.GH, seed.MVP, seed.A)
        names(output) <- c("vagility", "gen.het", "min.via.pop", "abundance")
        output <- structure(output[!is.na(output)], class = "pop.var.seeds")
      }
    }
  ## add element with names
  variables <- names(output)
  ## split names of abundance and min.via.pop
  if(any(variables == "min.via.pop.abundance")){
    variables <- c(  variables[which(which(!variables == "min.via.pop.abundance") < which(variables == "min.via.pop.abundance"))],
                     "min.via.pop", "abundance",
                     variables[which(which(!variables == "min.via.pop.abundance") > which(variables == "min.via.pop.abundance"))])
  }
  ## add to end of output
  final <- c(output, "variables" = list(variables))
  ## export if set
  if(export){
    saveRDS(final, file = paste0(name.out, "_pop_var_seeds.Rds"))
  } else {
    return(final)
  }
}
