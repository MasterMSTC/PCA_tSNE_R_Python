# Chapter 10 Lab 1: Principal Components Analysis

states=row.names(USArrests)
states

USArrests

dimnames(USArrests)

# Who has the largest variance??

apply(USArrests, 2, mean)
apply(USArrests, 2, var)


# PCA analysis

pca.out=prcomp(USArrests, scale=FALSE)

names(pca.out)

# Principal Components: How many PCAs?

pca.out$rotation

# CHECK: can be obtained using SVD on cov()

Eigenvalues <- eigen(cov(USArrests))$values
Eigenvectors <- eigen(cov(USArrests))$vectors

Eigenvectors

# Importance to Scale

pca=prcomp(USArrests, scale=TRUE)

pca$sdev

pca$center
pca$scale
pca$rotation


### The scores

dim(pca$x)

#########################################################
#### PCA for EXPLORATORY DATA ANALYSIS
####
####     BiPlot
########################################################

windows()
biplot(pca, scale=0, cex=0.8)

#### Changing DIRECTIONS....
pca$rotation=-pca$rotation
pca$x=-pca$x

biplot(pca, scale=0, cex=0.8)


# Study the proportion of variance Explained
summary(pca)
windows()
plot(pca, type = "l")

pca_summary=summary(pca)
plot(pca_summary$importance[3,],
     xlab="Principal Component",
     ylab="Cumulative Proportion of Variance Explained", type='l')



##################################################################
################### NOW TRY with Churn Dataset ####################


# ORANGE Churn prediction USE ONLY NUMERICAL so remove:

to_remove= c('State',
             'Area.code',
             'International.plan',
             'Voice.mail.plan',
             'Total.day.charge',
             'Total.eve.charge',
             'Total.night.charge',
             'Total.intl.charge',
             'Churn')


# Load Data

file_train ='churn-bigml-80.csv'
CV_data <- read.csv(file_train, header=TRUE, sep=",")

library(data.table)
set(CV_data, j = to_remove, value = NULL)

dimnames(CV_data)


pca.churn=prcomp(...

# Analyze and try to get insights...

