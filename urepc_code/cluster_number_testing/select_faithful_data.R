#multmixmodel.sel()
library("mixtools")

makemultdata(as.matrix(faithful),cuts = 2)

multmixmodel.sel(as.matrix(faithful), comps = c(1,2,3,4,5), epsilon = 1e-08)

repnormmixmodel.sel(t(as.matrix(faithful)), k = 5, verb = FALSE, epsilon = 1e-08)

