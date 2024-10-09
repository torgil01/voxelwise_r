test_rnifti <- function() {
  


# simple test for voxel-wise statistics


# use rat data 
# first use lm and verify with spm

# Model 
# y(img) ~ group 


# libs
library(RNifti)
# read y data 
fn <- "../voxelwise_data/treat1_vs_con1.nii.gz"
image <- readNifti(fn)
mask <- readNifti("../voxelwise_data/brainmask.nii.gz")

im_dim <- dim(image)
nx <- im_dim[1]
ny <- im_dim[2]
nz <- im_dim[3]
nt <- im_dim[4]

# reshape
image <- array(image,c(nx*ny*nz,nt))
mask_vec <- array(mask,c(nx*ny*nz))
indx <- which(mask_vec > 0)

# mk sparse
im_sparse <- image[indx,] 
sp_dim <- dim(im_sparse)
nvox <- sp_dim[1]
ntime <- sp_dim[2]

# free memory
rm(image)

# contrast 
grp <- c( 0, 0 , 0 , 0 , 0, 1 ,1 , 1 , 1, 1, 1)
  
# Note, R uses column order , so consecutive addresses are on first index
p_val_sp <- array(0, c(nvox, 1)) 
beta_sp <- array(0, c(nvox,1))
# iterate
for (i in 1:nvox) {
  y = im_sparse[i,]
  # linear regression
  l <- lm(y ~ grp)
  beta_sp[i] <- l$coefficients[2]
  # l[["coefficients"]][2]
  p_val_sp[i] <- 1 - summary(l)$coefficients[,4][2]
}


# reconstruct 
p_val <- array(0,c(nx,ny,nz))
beta <- array(0,c(nx,ny,nz))
p_val[indx] <- p_val_sp
beta[indx] <- beta_sp

# save 
pval.nii <- asNifti(p_val,reference = mask)
writeNifti(pval.nii,"../voxelwise_data/pval.nii.gz")
beta.nii <- asNifti(beta,reference = mask)
writeNifti(beta.nii,"../voxelwise_data/beta.nii.gz")

}