# Cincinnati Zoo Dataset Cluster Analysis and Association Rules

## loading required libraries

library(fpc) # cluster analysis
library(mclust) # model based clustering
library(arules) #to setup the association rules
library(arulesViz) #to implement visualization techniques to explore association rule
library(tidyr) # Data manipulation
library(dplyr) # Data manipulation
library(ggplot2) # Plots

## Reading the data from the sources

### data for cluster Analysis
zooc <- read.csv("J:/Data Analysis/Git Hub/AssociationRules/qry_Food_by_Month.csv")

### Data for Setting up Association Rules
TransFood <- read.csv('http://homepages.uc.edu/~maifg/DataMining/data/food_4_association.csv')

# Exploring ZOOC data set
str(zooc)
summary(zooc)
### The zooc data set comprises of 55 data points and has 7 variables. The first column
### contains the items that are sold in the zoo (there are total 55 distinct items) and the
### showcase the sale of an item over a period of 6 months starting from October 2010 to
### March 2011

## Check for NAs in the dataset
check_na <- sum(is.na(zooc))
check_na # No missing values in the dataset

## check for NANs, since is.nan can't be applied to a dataframe we use the below function
check_nan <- function(x)
  do.call(cbind, lapply(x, is.nan))
check_nan(zooc) #no missing values in the dataset

#boxplot
boxplot(zooc[,-1], ylab = 'Sales', xlab = 'months', col = rainbow(unique(6)),
        at = c(1:6)-c(1, 1.5, 2, 2.5, 3, 3.5), boxwex = 0.3, xaxt = "n")
# for october, december and march, we can observe a few outliers in the sales
legend("topright", legend = c("Oct'10", "Nov'10","Dec'10", "Jan'11", "Feb'11", "Mar'11"),
       fill = rainbow(unique(6)), cex = 0.70)
?legend

#barplot to observe the sales over months
Tot_vect <- colSums(zooc[,-1])
Tot_vect

barplot(Tot_vect, las = 0.5,yaxt = "n",ylim = c(0, 20000), ylab = "Sales", xlab = "months", 
        col = rainbow(unique(6)), width = 0.8, space = 0.3)
axis(side = 2, at = seq(0, 20000, 2500), labels = TRUE, las = 0.5)
box()
## the sales observed in the month of october far exceeds the sales observed in other months

#Preparing the data for cluster analysis
zooc_norm <- scale(zooc[,-1]) #normalizing the data 
summary(zooc_norm)
zooc_norm <- as.data.frame(zooc_norm)
str(zooc_norm)

## K-means clustering

wss <- (nrow(zooc_norm)-1)*sum(apply(zooc_norm,2,var))
for (i in 2:12) wss[i] <- sum(kmeans(zooc_norm,
                                     centers=i)$withinss)
plot(1:12, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")

### Finding the best cluster size

### Prediction strength
prediction.strength(zooc_norm, Gmin=2, Gmax=15, M=50,cutoff=0.8)

### Silhouetee coefficient and dunn index ####
d <- dist(zooc_norm, method = "euclidean")
result <- matrix(nrow = 14, ncol = 3)
for (i in 2:15){
  cluster_result = kmeans(zooc_norm, i)
  clusterstat=cluster.stats(d, cluster_result$cluster)
  result[i-1,1]=i
  result[i-1,2]=clusterstat$avg.silwidth
  result[i-1,3]=clusterstat$dunn   
}
plot(result[,c(1,2)], type="l", ylab = 'silhouette width', xlab = 'number of clusters')
plot(result[,c(1,3)], type="l", ylab = 'dunn index', xlab = 'number of clusters')

#the best cluster size choice seems to be 3. Calculating other parameters for this size

zooc_norm3=kmeans(zooc_norm,3)
plotcluster(zooc_norm, zooc_norm3$cluster)
table(zooc_norm3$cluster)
zooc_norm3$tot.withinss
zooc_norm3$betweenss
zooc_norm3$totss

