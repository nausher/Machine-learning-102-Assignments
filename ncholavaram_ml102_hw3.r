require(mclust)
#install.packages("mclust")

# clear memory
rm(list=ls())

irisMClust <- Mclust(iris[,-5])

# Model names - http://svitsrv25.epfl.ch/R-doc/library/mclust/html/mclustModelNames.html

irisBIC <- mclustBIC(iris[, -5], G=3)
<- mclustModel(iris[,-5], irisBIC)



###############################################################################
# Code to generate the test data of 6 ellipses
# Author: mike-bowles
###############################################################################


rm(list=ls())

m <- NULL

cent <- c(0.0,0.0)
for(i in seq(100)){
  #random draw of normal variate
	# args - 2 samples, 0.0 mean, 0.5 sd
	r <- c(rnorm(1,0.0,0.3),rnorm(1,0.0,1.0))
	#build 2-vector and combine into matrix m
	#rbind - "row bind" joins two matrices row-wise
	m <- rbind(m,cent + r) 
}


cent <- c(-3,0.0)
for(i in seq(100)){
	#random draw of normal variate
	# args - 2 samples, 0.0 mean, 0.5 sd
	r <- c(rnorm(1,0.0,1.0),rnorm(1,0.0,3.0))
	#build 2-vector and combine into matrix m
	#rbind - "row bind" joins two matrices row-wise
	m <- rbind(m,cent + r) 
}


cent <- c(6,0.0)
for(i in seq(100)){
	#random draw of normal variate
	# args - 2 samples, 0.0 mean, 0.5 sd
	r <- c(rnorm(1,0.0,2.0),rnorm(1,0.0,1.5))
	#build 2-vector and combine into matrix m
	#rbind - "row bind" joins two matrices row-wise
	m <- rbind(m,cent + r) 
}

cent <- c(8,2.0)
for(i in seq(100)){
	#random draw of normal variate
	# args - 2 samples, 0.0 mean, 0.5 sd
	r <- c(rnorm(1,0.0,0.3),rnorm(1,0.0,0.2))
	#build 2-vector and combine into matrix m
	#rbind - "row bind" joins two matrices row-wise
	m <- rbind(m,cent + r) 
}

cent <- c(9,0.0)
for(i in seq(100)){
	#random draw of normal variate
	# args - 2 samples, 0.0 mean, 0.5 sd
	r <- c(rnorm(1,0.0,0.1),rnorm(1,0.0,0.1))
	#build 2-vector and combine into matrix m
	#rbind - "row bind" joins two matrices row-wise
	m <- rbind(m,cent + r) 
}

cent <- c(8,-2.0)
for(i in seq(100)){
	#random draw of normal variate
	# args - 2 samples, 0.0 mean, 0.5 sd
	r <- c(rnorm(1,0.0,0.1),rnorm(1,0.0,0.1))
	#build 2-vector and combine into matrix m
	#rbind - "row bind" joins two matrices row-wise
	m <- rbind(m,cent + r) 
}

plot(m)

sixElipseMClust <- Mclust(m)
plot(sixElipseMClust, )
#str(sixElipseMClust)
#plot(sixElipseMClust$z)

