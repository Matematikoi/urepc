a <- 1
b <- "1,12,31,231,23,123"

invisible(eval(parse(text=commandArgs(TRUE))))

foo = readRDS('results/sem/model_69.RDS')
