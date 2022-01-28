library(tidyverse)

setwd("1. Point sources")

#1 - Get stack parameters from CEDR
source("01 CEDR_stack_parameters.R")

#2 - Update modeling parameters
#     for facilities w/ air modeling in TEMPO
source("02 Collect criteria modeling parameters.R")

#3 - Clean modeling parameters that are out-of-range
#    Replace with SCC defaults and reasonable limits
source("03 Clean_stack_parameters.R")

#4 - Update facility coordinates w/ air modeling and Egg-Hunt results
source("04 Join updated facility coords to stacks.R")

#4.5 - Update facility coordinates w/ air modeling and Egg-Hunt results
source("04_5_Check stacks are facility neighbors.R")

#5 - Check facilities that moved more than 100 meters
source("05 QC stack coordinates.R")

#6 - Update landfill coordinates
source("06 QC landfill coords.R")

#6.2 - Update POTW coordinates
source("06 QC POTW coords.R")

#7 - Combine identical stacks in same location
source("07 Group stacks with identical params.R")

#7.5 - Combine identical stacks in same location
source("07-5_Last check stacks are facility neighbors.R")

#8 - Join All point source types
source("08_Join all point sources.R")


## Optional exploratory goodness
#9 - Compare to last years modeling
source("09 Compare sources to mnrisks 2014.R")

#99 - Check BPIP files
source("99 QC BPIP files.R")


##
