# imports 
require(graphics)

require(adegenet)
#install.packages("adegenet", dep=TRUE)
# clear memory
rm(list=ls())

# create the 5x5 (25) centers
centers = expand.grid(1:5,  1:5)

####################
## SET UP  OF DATA
####################

# initializing vectors to hold results
random.fullmatrix <- c()
random.points.x  <- c()
random.points.y  <- c()
# loop and assign random values for points around the centers
for (i in 1:nrow(centers)) {
  random.points.x = c(random.points.x , rnorm(25, mean = centers[i,]$Var1, sd = 0.1))
  random.points.y = c(random.points.y , rnorm(25, mean = centers[i,]$Var2, sd = 0.1))
}
random.pointmatrix = c(random.points.x, random.points.y)
random.fullmatrix =  matrix(random.pointmatrix, ncol = 2)
# assign column names
colnames(random.fullmatrix) <- c("x", "y")

# START for testing only 
cl <- kmeans(random.fullmatrix, 25, nstart = 5)
plot(random.fullmatrix, col = cl$cluster)
plot(random.fullmatrix)
# show centers of the clusters
points(cl$centers, col = 1:5, pch = 8)
# END for testing only

####################
## PROBLEM 1: Plot K-values vs. Sum of Squared Error
####################

# perform K-means in loop for values 5 -> 50
kvalues <- c()
sse <- c()
for(k in 5:50){
  cl <- kmeans(random.fullmatrix, k, iter.max=20, nstart=25)
  sse <- c(sse, cl$tot.withinss)
  kvalues <- c(kvalues, k)
}
kvalue_vs_sse = matrix(c(sse,kvalues), ncol=2)
# Plot the result - K-values vs. Sum of Squared Error 
plot(kvalue_vs_sse, type="o", col="red", pch=8, xlab="Sum of Squared Error", ylab="K values")
# looks like the best improvement is at K = 30 and no further after that

####################
## PROBLEM 2: Plot K-values vs. AIC (Akaike Information Criterion)
####################

# perform K-means in loop for values 5 -> 50
kvalues2 <- c()
aic <- c()
M = 2 # Dimensionality of vector space, 2 dimension problem
for(k in 5:50){
  cl <- kmeans(random.fullmatrix, k, iter.max=20, nstart=25)
  sse <- c(sse, cl$tot.withinss)
  aic_value <- (sse + 2*M*K)
  aic <- c(aic, aic_value)
  kvalues <- c(kvalues, k)
}
kvalue_vs_sse = matrix(c(sse,kvalues), ncol=2)
# Plot the result - K-values vs. Sum of Squared Error 
plot(kvalue_vs_sse, type="o", col="red", pch=8, xlab="Sum of Squared Error", ylab="K values")
# looks like the best improvement is at K = 30 and no further after that


####################
## PROBLEM 2: Alternative approach
####################

foo.AIC <- find.clusters(random.fullmatrix, n.pca=100, choose=FALSE, stat="AIC")
plot(foo.AIC$Kstat, type="o", xlab="number of clusters (K)", ylab="AIC", col="purple", main="Detection based on AIC")



