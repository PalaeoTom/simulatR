stage <- stage.cc
p0 <- seed.pops
t0 <- times[1]
t1 <- times[2]

## Needs two variants - with and without min.via.pop.

## Return updated p0[[1]]

## Also needs with/without carrying capacity

!any(names(p0[[2]][[1]]) == "abundance") | !any(names(p0[[2]][[1]]) == "min.via.pop") | !any(names(p0[[2]][[1]]) == "avg.disp.dist")

if(!class(stage) == "stage")
  ## abundance needed
  ## average dispersal distance needed
  ## dispersal propensity needed
  ## min.via.pop optional
  ## carrying capacity optional



par.dispersal <- function(stage, p0, CC = F, MVP = F, carrying.capacity = F, t0, t1, SF = 1){
  ## check for stage
  if(!class(stage)=="stage"){
    stop("stage is not a stage object")
  }
  ## check that abundance, min.via.pop, and avg.disp.dist are included in seed.pops
  if(!any(names(p0[[2]][[1]]) == "abundance") | !any(names(p0[[2]][[1]]) == "disp.prop") |!any(names(p0[[2]][[1]]) == "avg.disp.dist")){
    stop("p0 is missing one or more of the essential population variables: abundance, disp.prop, and avg.disp.dist. Please ensure these variables are included and labelled as specified")
  }
  ## get ID of all species in
  ## if carrying capacity is included
  if(CC){
    ## if min.via.pop included
    if(MVP){

    } else {

    }
  } else {
    if(MVP){

    } else {

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
