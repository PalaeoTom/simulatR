#### Building base dispersal models ####
## Tom Smith 10/07/2023 ##

## load library
library(simulatR)

# make stage
s <- make.stage(n.col = 5, n.row = 5, ar = 400)

# add a random variable to stage
s <- add.cc(stage = s)

# define population variables
PVs <- set.pop.var.seeds(pop.gen.threshold = 0.1, new.var.name = "PV1", new.var.seed = function() runif(1,10,50))

# generate seed populations
p0 <- gen.seed.pops(stage = s, pop.var.seeds = PVs, n = 10)

## specify models for estimating number of propagules
m1 <- specify.model(s = s, p = p0, type = "continuous", variables = c("DP", "A", "MVP", "time"), expression = "rpois(1, ((time*DP*A)/MVP))", ID = "m1")
m2 <- specify.model(s = s, p = p0, type = "continuous", variables = c("DP", "A", "time"), expression = "rpois(1, (time*DP*A))", ID = "m2")

## model for calculating lambda
lambda <- specify.model(s = s, p = p0, type = "continuous", variables = "ADD", expression = "abs(log(0.5)/ADD)", ID = "m3")

## model for using lambda to calculate probability of each distance
m4 <- specify.nested.model(s = s, p = p0, type = "continuous", m = lambda, variables = "distances", expression = "exp(-m3.1*sort(unique(distances[,r])))", ID = "m4")

## model for getting distance travelled by each propagule - with and without MVP
m5.MVP <- specify.nested.model(s = s, p = p0, type = "continuous", m = list(m1,m4), variables = "distances", expression = "if(m1.1 > 0){sample(sort(unique(distances[,r])), size = m1.1, replace = T, prob = c(abs(diff(m4.2)),1-sum(abs(diff(m4.2)))))}else{0}", ID = "m5.MVP")
m5 <- specify.nested.model(s = s, p = p0, type = "continuous", m = list(m2,m4), variables = "distances", expression = "if(m2.1 > 0){sample(sort(unique(distances[,r])), size = m2.1, replace = T, prob = c(abs(diff(m4.2)),1-sum(abs(diff(m4.2)))))}else{0}", ID = "m5")

## model for getting landing spot of each propagule
m6.MVP <- specify.nested.model(s = s, p = p0, type = "continuous", m = m5.MVP, variables = "distances", expression = "sapply(1:length(m5.MVP.3), function(x) sample(which(distances[,r] == m5.MVP.3[x]),1))", ID = "m6.MVP")
m6 <- specify.nested.model(s = s, p = p0, type = "continuous", m = m5, variables = "distances", expression = "sapply(1:length(m5.3), function(x) sample(which(distances[,r] == m5.3[x]),1))", ID = "m6")

## final set of models - reduces areas to be populated based when CC
## with MVP and CC
m7.MVP <- specify.nested.model(s = s, p = p0, type = "continuous", m = m6.MVP, variables = NA, expression = "m6.MVP.4[sapply(1:length(m6.MVP.4), function(x)if(s$stage.variables$CC[m6.MVP.4[x]] == 0){F}else{sample(c(T,F), 1, prob = c((1-(length(p0$populated.regions[[m6.MVP.4[x]]])/s$stage.variables$CC[m6.MVP.4[x]])),(length(p0$populated.regions[[m6.MVP.4[x]]])/s$stage.variables$CC[m6.MVP.4[x]])))})]", ID = "m7.MVP")
m7 <- specify.nested.model(s = s, p = p0, type = "continuous", m = m6, variables = NA, expression = "m6.4[sapply(1:length(m6.4), function(x)if(s$stage.variables$CC[m6.4[x]] == 0){F}else{sample(c(T,F), 1, prob = c((1-(length(p0$populated.regions[[m6.4[x]]])/s$stage.variables$CC[m6.4[x]])),(length(p0$populated.regions[[m6.4[x]]])/s$stage.variables$CC[m6.4[x]])))})]", ID = "m7")

## Now models complete, rebrand and export
dispersal.base <- specify.nested.model(s = s, p = p0, type = "continuous", m = m5, variables = "distances", expression = "sapply(1:length(m5.3), function(x) sample(which(distances[,r] == m5.3[x]),1))", ID = "dispersal.base")
dispersal.MVP <- specify.nested.model(s = s, p = p0, type = "continuous", m = m5.MVP, variables = "distances", expression = "sapply(1:length(m5.MVP.3), function(x) sample(which(distances[,r] == m5.MVP.3[x]),1))", ID = "dispersal.MVP")
dispersal.CC <- specify.nested.model(s = s, p = p0, type = "continuous", m = m6, variables = NA, expression = "m6.4[sapply(1:length(m6.4), function(x)if(s$stage.variables$CC[m6.4[x]] == 0){F}else{sample(c(T,F), 1, prob = c((1-(length(p0$populated.regions[[m6.4[x]]])/s$stage.variables$CC[m6.4[x]])),(length(p0$populated.regions[[m6.4[x]]])/s$stage.variables$CC[m6.4[x]])))})]", ID = "dispersal.CC")
dispersal.MVP.CC <- specify.nested.model(s = s, p = p0, type = "continuous", m = m6.MVP, variables = NA, expression = "m6.MVP.4[sapply(1:length(m6.MVP.4), function(x)if(s$stage.variables$CC[m6.MVP.4[x]] == 0){F}else{sample(c(T,F), 1, prob = c((1-(length(p0$populated.regions[[m6.MVP.4[x]]])/s$stage.variables$CC[m6.MVP.4[x]])),(length(p0$populated.regions[[m6.MVP.4[x]]])/s$stage.variables$CC[m6.MVP.4[x]])))})]", ID = "dispersal.MVP.CC")
