

a <- 2
b <- 3

invisible(eval(parse(text=commandArgs(TRUE))))

cat(a+b, "\n")
