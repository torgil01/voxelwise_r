# simple test for voxel-wise statistics

# perhaps neuroim is better ?
# https://cran.r-project.org/web/packages/neuroim/index.html
# spare classes

#https://bbuchsbaum.github.io/neuroim2/index.html
library(neuroim2)

fw_file='/home/torgil/1030/dti_fa_1.nii.gz'
mask_file='/home/torgil/1030/wm_mask_1.nii.gz'

#  for reading 4d volumes 
# vec <- read_vec(file_name)

# this works but make dummy data for testing
# vol <- read_vol(fw_file)
# mask <- read_vol(mask_file)
# indices <- which(mask!=0)
# vectorize vol
#dat <- vol[indices]
#

# dummy data 
img = array(runif(32*32*2*10,0,10), c(32,32,2,10))
age = 1:10
for (i in 1:10) {
  img[15:25,15:25,1,i] <- img[15:25,15:25,1,i]*age[i]  
}

# resahpe dat 
dat <- array(img,c(32*32*2,10)) # now 2nd dim is across volumes 
beta <- array(0,dim(dat))

# iterate
for (i in 1:length(dat)) {
    y = dat[i,]
    l <- lm(y ~ age)
  # run model for i 
  beta_age <- l$coefficients[2]
  # l[["coefficients"]][2]
  p_value <- summary(l)$coefficients[,4][2]
  
  # collect beta
  beta[i] <- mod.beta
}


# write beta as image