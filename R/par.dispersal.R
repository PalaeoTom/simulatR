par.dispersal <- function(stage, p0, CC = F, MVP = F, PEPF = "default", t1, t0){
  ## check for stage
  if(!class(stage)=="stage"){
    stop("stage is not a stage object")
  }
  ## check that abundance, min.via.pop, and avg.disp.dist are included in seed.pops
  if(!any(names(p0[[3]][[1]]) == "abundance") | !any(names(p0[[3]][[1]]) == "disp.prop") |!any(names(p0[[3]][[1]]) == "avg.disp.dist")){
    stop("p0 is missing one or more of the essential population variables: abundance, disp.prop, and avg.disp.dist. Please ensure these variables are included and labelled as specified")
  }
  ## initialise output
  p1 <- p0
  ## get duration
  dur <- t1-t0
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


      ## write below for one population's propagules, then iterate
      x = 1

      ## find population location
      loc <- which(sapply(p0[[1]], function(y) names(n.props)[x] %in% y))
      ## get distances to all possible recipient regions from source region
      SR.dist.all <- stage[[3]][loc,]
      ## get Lambda from average dispersal distance for years elapsed
      lambda <- abs(log(0.50)/(p0[[3]][[which(names(p0[[3]]) == names(n.props)[x])]][1]*dur))
      ## reduce to unique distances, drop source regions (distance = 0)
      SR.dist <- sort(unique(stage[[3]][loc,][-which(stage[[3]][loc,] == 0)]))
      ## Now multiply take exponent of each distance multiplied by this value to get fraction at each distance.
      frac.props <- exp(-lambda*SR.dist)
      ## Remove theoretically impossible distances (fraction = 0), if any
      if(any(frac.props == 0)){
        frac.props <- frac.props[-which(frac.props == 0)]
        SR.dist <- SR.dist[-which(frac.props <= 0)]
      }
      ## convert to fractions that fall within increments
      frac.inc <- sapply(2:(length(frac.props)+1), function(z) c(1,frac.props)[z-1]-c(1,frac.props)[z])
      ## the above is missing the final increment: maximum distance and beyond. Add this.
      probs <- c(frac.inc, (1-sum(frac.inc)))
      ## add dispersal failure to SR.dist
      poss.SR.dist <- c(0, SR.dist)
      ## FOR EACH PROPAGULE
        ## Pick a distance
        dist.trav <- sample(poss.SR.dist, 1, prob = probs)
        ## Pick recipient region randomly - equal probability - if distance traveled is over 0
        if(dist.trav > 0){
          recep.reg <- sample(which(SR.dist.all == dist.trav), 1)
          ## if !PEPF = NA, determine if population settles
          if(!PEPF == "none"){

          } else {
            PEPF.v <- 1
          }


          ## Get population identifier
          max.pop.num <- max(as.numeric(unlist(regmatches(unlist(p1[[1]]), gregexpr("[[:digit:]]+", unlist(p1[[1]]))))))
          ## Add new population to p1
          p1[[1]][[recep.reg]] <- c(p1[[1]][[recep.reg]], paste0("p", max.pop.num+1))
          ## Add population variables

        }







    }
  }
  ## get distances between regions

    ## find occupied regions

    ## if one region

    # if more than one region


  ## if min.via.pop

  ## if one region source

  ## if more than one source region



}
