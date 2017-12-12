# Chapter 10 Lab 1: Principal Components Analysis

states=row.names(USArrests)
states

USArrests

dimnames(USArrests)

#############  TO DO ###################
# What feature has the largest variance??


# PCA analysis

pca.out=prcomp(USArrests, scale=FALSE)

names(pca.out)

# Principal Components: How many PCAs?


##################TO DO #################333
# CHECK: can be obtained using SVD on cov(USArrests)

#Eigenvalues <- 
#Eigenvectors <- 

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

#### TO DO ###################################
# PLOT Cumulative Proportion of Variance Explained



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

file_train ='C:\\MSTC_BD\\NEW_2017_18\\CHURN\\DATA\\churn-bigml-80.csv'
CV_data <- read.csv(file_train, header=TRUE, sep=",")



