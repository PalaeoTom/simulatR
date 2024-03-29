#' Set population variables
#'
#' set.pop.var.seeds generates a pop.var.seeds object which specifies the population variables for a simulation and defines how their initial values will be derived. Core variables
#' are abundance, mean genome, average dispersal distance (avg.disp.dist), and dispersal propensity (disp.prop). These variables are integral to the core processes of the simulation (speciation, dispersal, extirpation) and so
#' cannot be switched off. Minimum viable population (min.via.pop) is also included as an optional variable. By default, abundance and minimum viable population numbers are
#' drawn from a Weibull distribution fitted to the data of Traill et al., with values below 10 being resampled, while the avg.disp.dist values are drawn from a gamma distribution fitted to the data of Kinlan & Gaines (2003).
#' By default, the mean genome and disp.prop values are derived from uniform distributions bounded between 0 and 1, with mean genome and avg.disp.dist values of 0 resampled by default. All five variables are included in the output by default.
#' However, min.via.pop can be switched off, and all five variables can be replaced with custom functions if desired. This function also allows for additional custom population variables to be added as input-less functions.
#'
#' @param abundance Either "default" (the default) or a function which specifies how the initial value is derived for a population.
#' @param avg.disp.dist Either "default" (the default), or a function which specifies how the initial value is derived for a population.
#' @param disp.prop Either "default" (the default), or a function which specifies how the initial value is derived for a population.
#' @param genome Either "default" (the default), "off", or a function which specifies how the initial value is derived for a population.
#' @param min.via.pop Either "default" (the default), "off", or a function which specifies how the initial value is derived for a population. If shared.pop.gen = TRUE, can also be a numeric value.
#' @param pop.gen.threshold Either "default" (the default), "off", or a function which specifies how the threshold for excessive genomic change in an interval.
#' @param stable.seeds If TRUE, all seed abundance values will be greater than the minimum viable population numbers. Default is FALSE.
#' @param new.var.name If new.var.seed is a function or list of functions, a string, or list of strings, specifying the name(s) of the new population variables. Strings should not contain punctuation.
#' @param new.var.seed A function or list of functions specifying how the initial values for the new variables are derived for a population.
#' @param name.out If export = TRUE, a string that specifies the name of the output file.
#' @param export If TRUE, updated stage with new variable is exported as an Rds file to working directory.
#'
#' @references Traill, L. W., Bradshaw, C. J. A., & Brook, B. W. (2007). Minimum viable population size: A meta-analysis of 30 years of published estimates. \emph{Biological Conservation}, 139, 156-166.
#' @references Kinlan, B. P. & Gaines, S. D. (2003). Propagule dispersal in marine and terrestrial environments: a community perspective. \emph{Ecology}, 84, 2007-2020.
#'
#' @return A pop.var.seeds object.
#' @export
#'
#' @examples
#' # Generate pop.var.seeds object
#' pop.seeds <- set.pop.var.seeds()
set.pop.var.seeds <- function(avg.disp.dist = "default", disp.prop = "default", abundance = "default", genome = "default", min.via.pop = "default", pop.gen.threshold = "default", stable.seeds = F, new.var.name = "new.variable", new.var.seed = NULL, name.out = "new", export = F){
  ## base function for seeding population avg.disp.dist - always on
  if(avg.disp.dist == "default") seed.ADD <- seed.avg.disp.dist
  if(!avg.disp.dist == "default" && is.function(avg.disp.dist)){
    seed.ADD <- avg.disp.dist
  }
  if(!avg.disp.dist == "default" && !is.function(avg.disp.dist)){
    stop("provided avg.disp.dist seeding method is not a function")
  }
  ## base function for seeding population disp.prop - always on
  if(disp.prop == "default") seed.DP <- seed.disp.prop
  if(!disp.prop == "default" && is.function(disp.prop)){
    seed.DP <- disp.prop
  }
  if(!disp.prop == "default" && !is.function(disp.prop)){
    stop("provided disp.prop seeding method is not a function")
  }
  ## base function for seeding population mean genome - always on
  if(genome == "default") seed.G <- seed.genome
  if(!genome == "default" && is.function(genome)){
    seed.G <- genome
  }
  if(!genome == "default" && !is.function(genome)){
    stop("provided mean genome seeding method is not a function")
  }
  ## base function for seeding population pop.gen.threshold - can be turned off
  if(pop.gen.threshold == "default"){
    seed.PGT <- seed.pop.gen.threshold
    seed.PGR <- 0
  }
  if(is.function(pop.gen.threshold)){
    seed.PGT <- pop.gen.threshold
    seed.PGR <- 0
  }
  if(pop.gen.threshold == "off"){
    seed.PGT <- NA
    seed.PGR <- NA
  }
  if(is.numeric(pop.gen.threshold)){
    seed.PGT <- pop.gen.threshold
    seed.PGR <- 0
  }
  if(!pop.gen.threshold == "default" && !is.function(pop.gen.threshold) && !is.numeric(pop.gen.threshold) && !pop.gen.threshold == "off"){
    stop("provided pop.gen.threshold seeding method is not a function, numeric value or 'off'")
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
      out <- list(A, MVP)
      names(out) <- c("A", "MVP")
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
        output <- c(seed.ADD, seed.DP, seed.MVP.abundance, seed.G, seed.PGT, seed.PGR, new.var.seed)
        names(output) <- c("ADD", "DP", "MVPA", "G", "PGT", "PGT", new.var.name)
        output <- output[!is.na(output)]
      } else {
        output <- c(seed.ADD, seed.DP, seed.MVP.abundance, seed.G, seed.PGT, seed.PGR)
        names(output) <- c("ADD", "DP",  "MVPA", "G", "PGT", "PGR")
        output <- output[!is.na(output)]
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
        output <- c(seed.ADD, seed.DP, seed.A, seed.G, seed.PGT, seed.PGR, seed.MVP, new.var.seed)
        names(output) <- c("ADD", "DP",  "A", "G", "PGT", "PGR", "MVP", new.var.name)
        output <- output[!is.na(output)]
      } else {
        output <- c(seed.ADD, seed.DP, seed.A, seed.G, seed.PGT, seed.PGR, seed.MVP)
        names(output) <- c("ADD", "DP", "A", "G", "PGT", "PGR", "MVP")
        output <- output[!is.na(output)]
      }
    }
  ## add element with names
  variables <- names(output)
  ## split names of abundance and min.via.pop
  if(any(variables == "MVPA")){
    variables <- c(  variables[which(which(!variables == "MVPA") < which(variables == "MVPA"))],
                     "A", "MVP",
                     variables[which(which(!variables == "MVPA") > which(variables == "MVPA"))])
  }
  ## add to end of output
  final <- c(output, "variable.names" = list(variables))
  final <- structure(final, class = "pop.var.seeds")
  ## export if set
  if(export){
    saveRDS(final, file = paste0(name.out, "_pop_var_seeds.Rds"))
  } else {
    return(final)
  }
}
