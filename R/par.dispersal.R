par.dispersal <- function(stage, p0, CC = F, MVP = F, carrying.capacity = F, t0, t1, SF = 1){
  ## check for stage
  if(!class(stage)=="stage"){
    stop("stage is not a stage object")
  }
  ## check that abundance, min.via.pop, and avg.disp.dist are included in seed.pops
  if(!any(names(p0[[3]][[1]]) == "abundance") | !any(names(p0[[3]][[1]]) == "disp.prop") |!any(names(p0[[3]][[1]]) == "avg.disp.dist")){
    stop("p0 is missing one or more of the essential population variables: abundance, disp.prop, and avg.disp.dist. Please ensure these variables are included and labelled as specified")
  }
  ## get ID of all species in
  ## if carrying capacity is included
  if(CC){
    ## if min.via.pop included
    if(MVP){
      ## get average number of propagules


    } else {

    }
  } else {
    if(MVP){

    } else {
      ## get number of propagules for each population
      n.props <- lapply(1:length(p0[[3]]), function(x) {
        ## get average
        avg <- round(as.numeric(abs(t1-t0)*(p0[[3]][[x]][2]*p0[[3]][[x]][3]), 0))
        ## draw value from Poisson distribution
        out <- rpois(1, avg)
        return(out)
      })
      names(n.props) <- names(p0[[3]])
      ## get probability of propagules ceasing journey at each distance for each species
      ## find population
      lapply()
     p0[[1]][[x]]

      names(p0[[3]][x])

      location <- lapply(1:length(p0[[1]]), function(x) )


      (p0[[1]][[1]] == names(p0[[3]])[1])



    }
  }
  ## get distances between regions
  distances <- stage[[3]]
  for (i in 1:length(p0)){
    ## find occupied regions
    loc <- which(sapply(p0[[1]], function(y) i %in% y))
    ## if one region

    # if more than one region

  }
  ## if min.via.pop

  ## if one region source

  ## if more than one source region



}
