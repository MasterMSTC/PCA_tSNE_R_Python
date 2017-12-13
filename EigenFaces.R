# Following:
#

#http://stats.stackexchange.com/questions/127502/how-to-reconstruct-an-image-after-performing-pca-on-face-image-dataset-eigenfac

# install.packages("foreach")

file ='C:\\MSTC_BD\\PCA_FACES\\R_Example\\training.csv'
data_all = read.csv(file , stringsAsFactors=F)
dim(data_all) #7049   31

# use only 70 first images
data = data_all[1:70,]
names(data)

# extract the images data
im.train <- data$Image

# Can you extract one Image as an integer vector (256 gray scale)
# and plot it??

sample_im<-as.integer(unlist(strsplit(im.train[1], " ")))

length(sample_im)

# show picture
# each image is a vector of 96*96 pixels (96*96 = 9216).
im <- matrix(data=rev(sample_im), nrow=96, ncol=96)
windows()
image(1:96, 1:96, im, col=gray((0:255)/255))


# Now let's do it for-each of the images and store in a Matrix
library(foreach)

library(doParallel)
nodes <- detectCores()
cl <- makeCluster(nodes)
registerDoParallel(cl)

start_time <- Sys.time()
MM.train <- foreach(im = im.train, .combine=rbind) %dopar% {
  as.integer(unlist(strsplit(im, " ")))
}
end_time <- Sys.time()
end_time - start_time

# MM.train is a matrix of pixels 70x9216
dim(MM.train)

# show picture number 10
im <- matrix(data=rev(MM.train[10,]), nrow=96, ncol=96)
windows()
image(1:96, 1:96, im, col=gray((0:255)/255))

# Apply PCA
pca <- prcomp(MM.train,
              center = TRUE,
              scale. = TRUE) ## using correlation matrix

# There are in general min(n - 1, p) informative principal components
# in a data set with n observations and p variables. 
# in our case we will have 70 PCA : rotations

dim(pca$rotation)

#The summary method describe the importance of the PCs.
summary(pca)
#The first row describe again the standard deviation associated with each PC. 
#The second row shows the proportion of the variance in the data explained by each component 
#while the third row describe the cumulative proportion of explained variance. 


# plot method returns a plot of the variances (y-axis) associated with the PCs (x-axis). 
# useful to decide how many PCs to retain for further analysis. 

windows()
plot(pca, type='l')

pca_summary=summary(pca)
plot(pca_summary$importance[3,],
     xlab="Principal Component",
     ylab="Cumulative Proportion of Variance Explained", type='l')


# CHECK PCA are orthogonal
pca$rotation[,1]  %*% pca$rotation[,2]

# Show First 9 EigenFaces = Principal Components
par(mfcol=c(3,3), mar=c(1,1,2,1))

for (k in 1:9){
  rst <- matrix(data=rev(pca$rotation[,k]), nrow=96, ncol=96)
  image(1:96, 1:96, rst, col=gray((0:255)/255))
}


# Following your analysis, I use the same pca object.
# Looking at summary(pca) I can see that at 20 components, 
# 90% of the variation is explained. 
# So for demonstration purposes, that sounds like a good number
# to work with.



# reconstruct matrix
NumberPCA_comp=20

restr <- pca$x[,1:NumberPCA_comp] %*% t(pca$rotation[,1:NumberPCA_comp])

# unscale and uncenter the data
if(pca$scale != FALSE){
  restr <- scale(restr, center = FALSE , scale=1/pca$scale)
}
if(all(pca$center != FALSE)){
  restr <- scale(restr, center = -1 * pca$center, scale=FALSE)
}

# plot your original image and reconstructed image
Num_image=10

par(mfcol=c(1,2), mar=c(1,1,2,1))
im <- matrix(data=rev(MM.train[Num_image,]), nrow=96, ncol=96)
image(1:96, 1:96, im, col=gray((0:255)/255))

rst <- matrix(data=rev(restr[Num_image,]), nrow=96, ncol=96)
image(1:96, 1:96, rst, col=gray((0:255)/255))

#########################################################################
### Project other images not used for PCS

# Project new data: 6000 to 6100

data_test = data_all[6000:6100,]

# extract the images data
im.test <- data_test$Image

MM.test <- foreach(im = im.test, .combine=rbind) %dopar% {
  as.integer(unlist(strsplit(im, " ")))
}

# Scale and project onto pca rotation
scores_test=scale(MM.test, pca$center, pca$scale) %*% pca$rotation

# reconstruct test matrix
NumberPCA_comp=20

restr <- scores_test[,1:NumberPCA_comp] %*% t(pca$rotation[,1:NumberPCA_comp])

# unscale and uncenter the data
if(pca$scale != FALSE){
  restr <- scale(restr, center = FALSE , scale=1/pca$scale)
}
if(all(pca$center != FALSE)){
  restr <- scale(restr, center = -1 * pca$center, scale=FALSE)
}

# plot your original image and reconstructed image
# Warning! relative to our test sample
Num_image=20

par(mfcol=c(1,2), mar=c(1,1,2,1))
im <- matrix(data=rev(MM.test[Num_image,]), nrow=96, ncol=96)
image(1:96, 1:96, im, col=gray((0:255)/255))

rst <- matrix(data=rev(restr[Num_image,]), nrow=96, ncol=96)
image(1:96, 1:96, rst, col=gray((0:255)/255))

###### NEXT STEPS.......
##### TRY TO IMPROVE THE RESULTS USING MORE IMAGES FOR PCA and
##### more components for reconstruction
#####

