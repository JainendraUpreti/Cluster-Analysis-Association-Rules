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
zooc <- read.csv("J:/Data Analysis/Git Hub/AssociationRules/Association Rules Project/qry_Food_by_Month.csv")

### Data for Setting up Association Rules
TransFood <- read.csv('http://homepages.uc.edu/~maifg/DataMining/data/food_4_association.csv')
