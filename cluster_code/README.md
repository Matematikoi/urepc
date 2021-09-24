# Cluster computing cheatsheet

To see the processes you have run use
`squeue -u glozanop`

To see a list of available resources use 
`slist`

To send a program to run use
`sbatch -A standby --nodes=1 --time=00:01:00 -o file_demo_%j.out example.sub`

Or use an interactive session for five minutes
`interactive -A statdept -n 1 -N 1 -t 00:05:00`

To read arguments in R use 
`invisible(eval(parse(text=commandArgs(TRUE))))` 
inside the R script

And invoke the R script using 
`Rscript code/example_arguments.R a=7 b=21` 
