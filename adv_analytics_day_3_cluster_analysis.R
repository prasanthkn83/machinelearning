## Day 3 : Cluster Analysis

mtcars
##All vairables must be numeric
str(mtcars)
#Must scale all dimension to the same range (say 0~1)
myScale <- function(x){
    (x-min(x))/((max(x))-min(x))
}

mt <- sapply(mtcars, myScale)
summary(mt)
rownames(mt) <- rownames(mtcars)

##calculate the distance matrix
d <- dist(mt, method="euclidean")

#complete method
h1 <- hclust(d, method="complete")
plot(h1)

#single method
h1 <- hclust(d, method="single")
plot(h1)

#ward.D method
h1 <- hclust(d, method="ward.D")
plot(h1)

rect.hclust(h1,k=4)
cutree(h1, k=4)


##Cluster means
paste(names(mtcars), collpase=",")
clus <- cutree(h1, k=4)
mtcars <- cbind(mtcars, clus)
aggregate(cbind(mpg, cyl, disp, hp, drat, wt, qsec, vs, am, gear, carb) ~ clus, mtcars, FUN=mean)
table(clus)



#K-means Clustering
stores <- read.csv("CLUSTERING_DATA.CSV")
str(stores)

stores$State <- as.integer(stores$State)
paste(names(stores), collapse="','")
##keep only the columns we require and scale
Nst <-sapply(stores[,c('Cat1','Cat2','Cat3','Cat4','Size','Sale','State')], myScale)
summary(Nst)

km1 <- kmeans(Nst, centers=5)
?kmeans

ss <- km1$totss
for (ctr in 2:15) {
  km1 <- kmeans(Nst, centers=ctr)
  ss[ctr] <- km1$tot.withinss
} 
ss
plot(ss, type='o') #plotting sum of squared errors

set.seed(0)
km1 <- kmeans(Nst, centers=5, iter.max=100)

### iris
Niris <-sapply(iris[,c(-5)], myScale)
summary(Niris)

for (ctr in 2:15) {
  km1 <- kmeans(Niris, centers=ctr)
  ss[ctr] <- km1$tot.withinss
} 
ss
plot(ss, type='o')

set.seed(0)
km1 <- kmeans(Niris, centers=3, iter.max=100)
km1

table(km1$cluster, iris$Species)
myIris <- as.data.frame(Niris)
plot(myIris, col=iris$Species)

#k-mediods clustering, pam, clara
#pamk from fpc; suggests k





