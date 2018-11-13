# Following:
#

#http://stats.stackexchange.com/questions/127502/how-to-reconstruct-an-image-after-performing-pca-on-face-image-dataset-eigenfac

install.packages("foreach")


# Download data
# https://drive.google.com/file/d/1pj9qUPNuC0ZInsnUZxnAJwZDL2qExjyd/view?usp=sharing


file ='training.csv'
data_all = read.csv(file , stringsAsFactors=F)
dim(data_all) #7049   31

# use only 70 first images
data = data_all[1:70,]
names(data)

# extract the images data
im.train <- data$Image

#### TO DO: ###########################################################33
# Can you extract the first Image as an integer vector (256 gray scale)
# and plot it??






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



#### TO DO: ############################################
# show picture number 10

im <- matrix(data=rev(MM.train[10,]), nrow=96, ncol=96)

windows()
image(1:96,1:96, im, col=gray((0:255)/255))


#### TO DO: ############################################
# 
# Apply PCA  : with center and scale

pca <- prcomp(....

  
  
###########################################################3
###### Which are and HOW MANY PCA ????



###### TO DO###################################################
#Describe the importance of the PCs.


###### TO DO###################################################
# CHECK that PC are orthogonal


### TO DO: plot one PC ... pca$rotation[,N] ...
#### and SHOW THAT PC represented as images are : EIGENFACES!!



# Show First 9 EigenFaces = Principal Components
par(mfcol=c(3,3), mar=c(1,1,2,1)),

for (k in 1:9){
  rst <- matrix(data=rev(pca$rotation[,k]), nrow=96, ncol=96)
  image(1:96, 1:96, rst, col=gray((0:255)/255))
}

# Following your analysis, I use the same pca object.
# Looking at summary(pca) I can see that at 20 components, 
# 90% of the variation is explained. 
# So for demonstration purposes, that sounds like a good number
# to work with.



#### TO DO: #################################################333333333
#####   reconstruct IMAGES using only the first 20 PCA
NumberPCA_comp=20

pca$x[,1:NumberPCA_comp]

pca$rotation[,1:NumberPCA_comp]

### Restore images... restr <- 




###### NOW TRY RECONSTRUCTING IMAGES...
# plot your original image and reconstructed image

Num_image=10

par(mfcol=c(1,2), mar=c(1,1,2,1))
im <- matrix(data=rev(MM.train[Num_image,]), nrow=96, ncol=96)
image(1:96, 1:96, im, col=gray((0:255)/255))

rst <- matrix(data=rev(restr[Num_image,]), nrow=96, ncol=96)
image(1:96, 1:96, rst, col=gray((0:255)/255))

# REMEMBER TO unscale and uncenter the data



###################### FINALLY #########################################
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
