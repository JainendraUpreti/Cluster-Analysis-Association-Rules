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

boxplot(zooc[,-1], ylab = 'Sales', xlab = 'months', col = rainbow(unique(6)))
# for october, december and march, we can observe a few outliers in the sales

hist(zooc[,-1])
