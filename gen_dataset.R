#!/usr/bin/env Rscript
## This script decides on a target function and generates
## a dataset and defines some functions

# generating two randomly chosen points in [-1,1] x [-1,1]
p1 <- runif(n=2, min=-1, max=1)
p2 <- runif(n=2, min=-1, max=1)
slope <- (p2[2]-p1[2])/(p2[1]-p1[1]) 

# target function that passes through p1 and p2 points
f <- function(point){ 
	return(sign(point[3]-p1[2] - slope*(point[2]-p1[1])))
	}

# N = number of dataset points to generate
# plotf = T => plot the target function used
gen_dataset <- function(N = 100, plotf=T) {
	# plot the target function
	if (plotf == T)
		plot_f()
	# gen dataset
	data.set <- data.frame(c(rep(1,N)), runif(N, -1, 1), runif(N, -1, 1))
	data.set <- data.frame(data.set, apply(data.set, 1, f))
	names(data.set) <- c("x0", "x1", "x2", "y")

	# return the generated dataset
	return(data.set)
}

gen_testset <- function(N=100) { 
	return(data.frame(c(rep(1,N)), runif(N, -1, 1), runif(N, -1, 1)))
}

cal_disagree <- function(g, f, testdata) { 
	a <- apply(testdata, 1, f) 
	b <- apply(testdata, 1, g)
	return(length(which(a != b))/length(testdata[,1]))	
}

plot_f <- function() { 
		plot(c(-1,1), c(slope * (-1-p1[1])+p1[2], slope * (1-p1[1])+p1[2]), 
			 xlim=c(-1,1), ylim=c(-1,1), type = "l", col="darkred", xlab="x2", ylab="x3")
		legend(-1, 1, c("f=+1","f=-1"), col= c("green","blue"), pch=c(20,20))
}

plot_dataset <- function(data.set) {
	attach(data.set)
	# plot the dataset points. green => +1
	# and blue => -1
	points(data.set[y == 1, "x1"], data.set[y == 1, "x2"], xlim=c(-1,1),
		 ylim=c(-1,1), xlab="x1", ylab="x2", col="green", pch=20)
	points(data.set[y == -1, "x1"], data.set[y == -1, "x2"], xlim=c(-1,1),
		   ylim=c(-1,1), xlab="x1", ylab="x2",col="blue", pch=20)
	detach(data.set)
}

plot_g <- function(w) { 
	lines(c(-1, 1), c((-w[1]+w[2])/w[3], -(w[1]+w[2])/w[3]), col = "black");
	}
