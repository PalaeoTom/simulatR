#### Generating distribution of ISAR parameter values ####

## Load packages
library(sars)
library(usethis)

## Read in S/A data
setwd("~/Desktop/simulatR/HI_datasets")
raw <- lapply(1:120, function(x) read.csv(paste0("PA", x, ".csv"), header = T))

## Isolate S and A area data
SA <- lapply(1:length(raw), function(y) cbind(raw[[y]][which(colnames(raw[[y]]) == "a")],raw[[y]][which(colnames(raw[[y]]) == "s")]))

## Check for NAs - returns integer if so
which(sapply(1:length(SA), function(z) any(is.na(SA[[z]]))))

## Get power model parameters for each dataset
power <- t(as.data.frame(lapply(1:length(SA), function(z) sar_power(SA[[z]])$par)))
rownames(power) = NULL

## Export parameters
setwd("~/Desktop/simulatR/datasets/")
write.csv(power, file = "power.csv")

## Now to add it to simulatR
setwd("/Users/tjs/R_packages/R_projects/simulatR")
usethis::use_data(power, overwrite = TRUE, internal = T, compress = "xz")
