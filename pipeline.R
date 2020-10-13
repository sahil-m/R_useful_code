tic("ALL")

source("pipeline_file1.R")

tic("***Inner Block'***")
message("***Start of Inner Block'\n\n")
source("pipeline_file1.R")
source("pipeline_file1.R")
toc()
message("\n\n")

source("pipeline_file1.R")

toc()

