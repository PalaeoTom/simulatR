init.var.pops <- function(vagility = "default", abundance = "default", gen.het = "default", min.via.pop = "default", new = NULL){
## base function for seeding population vagility
  if(vagility == "default"){
    seed.V <- function(){
      while(T){
        V <- runif(1, 0, 1)
        if(V > 0) break()
      }
      return(V)
    }
  } else {
    if(!is.function(vagility)){
      stop("vagility is not a function")
    }
    seed.V <- vagility
  }
  ## base function for seeding population abundance
  if(abundance == "default"){

  } else {
    if(!is.function(abundance)){
      stop("abundance is not a function")
    }
    seed.A <- abundance
  }
  ## base function for seeding population genetic heterogeneity
  if(gen.het == "default"){
    seed.GH <- function(){
      while(T){
        GH <- runif(1, 0, 1)
        if(GH > 0) break()
      }
      return(GH)
    }
  } else {
    if(!is.function(gen.het)){
      stop("gen.het is not a function")
    }
    seed.GH <- gen.het
  }
  ## base function for seeding population minimum viable population
  if(min.via.pop == "default"){
    seed.MVP <- function(){
      while(T){
        MVP <- round(rweibull(1, scale = 2723.498572, shape = 0.54345227), digits = 0)
        if(MVP >= 10) break()
      }
      return(MVP)
    }
  } else {
    if(!is.function(min.via.pop)){
      stop("min.via.pop is not a function")
    }
    seed.MVP <- min.via.pop
  }
  ## combine core functions and add additional functions if specify
  if(!is.null(new)){

  } else {
    output <- list("seed.V" = seed.V, "seed.A" = seed.A, "seed.GH" = seed.GH, "seed.MVP" = seed.MVP)
  }

}







