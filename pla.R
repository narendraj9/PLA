#!/usr/bin/env Rscript
## this script trains a perceptron to approximate
## the f used by gen_dataset and calculates average number
## of iterations required for learning and the average probability
## of disagreement

N <- 100 # number of points in the dataset
draw_plots=T
avg_icount <- 0
avg_disagreement <- 0

for (i in 1:1000) {

	cat("Experiment #",i," ")
	source("gen_dataset.R")
	data.set <- gen_dataset(N, plotf=draw_plots)

	# Plot the dataset points
	if (draw_plots)
		plot_dataset(data.set)

	# initialize weights for g
	w <- rep(0, 3)

	icount <- 0
	while (TRUE) {
		if (draw_plots) {
			plot_f()
			plot_dataset(data.set)
			plot_g(w)
		}

	# output of the hypothesis on the dataset 
	 	o <- apply(data.set[1:3], 1, g)

	# find the indices of the misclassified points
		misindex = which(o != data.set[,4])
	# break out of the loop if no misclassified point found
		if (length(misindex) == 0) break;
	# pick a misclassified point randomly from misindex and apply PLA
		i <- sample(misindex, 1)
	 	w <- w + data.set[i,1:3]*data.set[i,4]
	 	icount <- icount+1;
		if (icount > N*100) break;
}
	test.set <- gen_testset(1000)
	disagreement <- cal_disagree(f, g, test.set)
	avg_disagreement <- avg_disagreement + disagreement
	avg_icount <- avg_icount + icount;
	cat("iteration count=",icount)
	cat(" | disagreement=", disagreement)
	cat("\n")
}
cat("#####################################RESULTS#####################################\n")
cat("average iteration count=",avg_icount/1000)
cat("\naverage disagreement",avg_disagreement/1000)
cat('\n')
