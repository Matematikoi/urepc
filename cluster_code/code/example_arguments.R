

a <- 2
b <- 3
text <- "this is some random text\n"

invisible(eval(parse(text=commandArgs(TRUE))))

cat(a+b, "\n")
cat(text)
