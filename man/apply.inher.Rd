% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/apply.inher.R
\name{apply.inher}
\alias{apply.inher}
\title{Apply inhertiance models}
\usage{
apply.inher(
  s,
  p0,
  new.spec,
  pop.var.inher,
  t0,
  t1,
  export = F,
  name.out = "new"
)
}
\arguments{
\item{s}{a stage object.}

\item{p0}{a populations object}

\item{new.spec}{a vector specifying which populations (by their index in p0$population.variables) have speciated this time bin.}

\item{pop.var.inher}{a pop.var.inher object}

\item{t0}{a numeric specifying the time of the previous interval.}

\item{t1}{a numeric specifying the time of the current interval.}

\item{export}{If TRUE, stage will be saved as an Rds file. Default is FALSE.}

\item{name.out}{A string to be included in the Rds output file name if export = TRUE. Default is "new".}
}
\value{
A populations object.
}
\description{
Applies inheritance models described in pop.var.inher object and returns an updated populations object.
}
\examples{
# make stage
s <- make.stage(n.col = 5, n.row = 5, ar = 400)

# add a random variable to stage
s <- add.var.stage(s, var = matrix(runif(25, min = 10, max = 50), 5, 5), var.name = "SV1")

# define population variable seeds
PVs <- set.pop.var.seeds(min.via.pop = "off", new.var.name = "PV1", new.var.seed = function() runif(1,10,50))

# define population inhertiance models
PVi <- set.pop.var.inher(pop.var.seeds = PVs, new.var.inher = list("PV1" = "fixed"))

# generate seed populations
p0 <- gen.seed.pops(stage = s, pop.var.seeds = PVs, n = 10)

# run function
p1 <- apply.inher(s = s, p0 = p0, new.spec = sample(1:length(p0$population.variables), 1), pop.var.inher = PVi, t0 = 100, t1 = 90)
}
