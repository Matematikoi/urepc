gb_to_consume <- 5
invisible(eval(parse(text=commandArgs(TRUE))))
foo <- integer(gb_to_consume*(1100000000/4))

print(gc())
