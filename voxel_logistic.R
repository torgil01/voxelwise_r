voxel_logistic <- function() {
  # voxel-wise logistic regression 
  # Inputs
  # 1. y = binary 4d mask of wmh lesions for each person
  # 2. x = 4d free water images
  # 3. cov = csv file with covariates
  # 4. brainmask 
  
  # libs
  library(RNifti)
  library(readr)
  
  # read y data 
  y_img <- readNifti("../voxelwise_data/wmh_stack.nii.gz")
  x_img <- readNifti("../voxelwise_data/fw_stack.nii.gz")
  brainmask <- readNifti("../voxelwise_data/brainmask.nii.gz")
  covars <- read_csv("../voxelwise_data/covars.nii.gz")
  
  # 
  im_dim <- dim(image)
  nx <- im_dim[1]
  ny <- im_dim[2]
  nz <- im_dim[3]
  nt <- im_dim[4]
  
  # reshape x and y 
  y_img <- array(y_img,c(nx*ny*nz,nt))
  x_img <- array(x_img,c(nx*ny*nz,nt))
  mask_vec <- array(brainmask,c(nx*ny*nz))
  indx <- which(mask_vec > 0)
  
  # mk sparse
  y_sparse <- y_img[indx,] 
  x_sparse <- x_img[indx,] 
  sp_dim <- dim(im_sparse)
  nvox <- sp_dim[1]
  ntime <- sp_dim[2]
  
  # free memory
  rm(x_img,y_img)

  # put covars in vectors
  # must be hard coded for each model 
  
  
  # allocate 
  # Note, R uses column order , so consecutive addresses are on first index
  # see : https://cran.r-project.org/web/packages/reticulate/vignettes/arrays.html
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

