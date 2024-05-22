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
mask <- array(mask,c(nx*ny*nz))
indx <- which(mask > 0)

# mk sparse
im_sparse <- image[indx,] 

# take average
av_sparse <- rowMeans(im_sparse)

# reconstruct 
av <- array(0,c(nx,ny,nz))
av[indx] <- av_sparse

# save 
av.nii <- asNifti(av,reference = mask)
writeNifti(av.nii,"../voxelwise_data/average.nii.gz")
# template = mask
# this gives average, but image dims are zero for ny,nz, nt 
}