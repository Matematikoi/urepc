#foo = as.matrix(read.table("./data/new_datasets_names.txt", sep=",", header=FALSE))

library("mixtools")

faithful_data = t(as.matrix(faithful))
em_out <- repnormmixEM(faithful_data, k=2)
View(em_out[["posterior"]])

data(Waterdata)

water <- t(as.matrix(Waterdata[,3:10]))
em_out_water <- repnormmixEM(water, k = 2, verb = TRUE, epsilon = 1e-08)
#plot(em_out_water)

em_out_water_np <- npEM(Waterdata[,3:10], 
                                   mu0 = 2, 
                                   samebw = FALSE,)


View(round(em_out_water_np[["posteriors"]]))
View(round(em_out_water[["posterior"]]))