vox_stats2 <- function(variables) {
  # if we are not using spare we might just as well use rnifti 
  # need to check the 4d reading capabilities
  #https://bbuchsbaum.github.io/neuroim2/index.html
  library(neuroim2)
  fn <- "../voxelwise_data/treat1_vs_con1.nii.gz"
  image <- read_vol(fn)
  mask <- read_vol("../voxelwise_data/brainmask.nii.gz")
  
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
  grp <- c( 0, 0 , 0 , 0 , 0, 1 ,1 , 1 , 1, 1, 1)
  
  # iterate
  for (i in 1:nvox) {
    y = dat[i,]
    l <- lm(y ~ age)
    # run model for i 
    beta[i] <- l$coefficients[2]
    # l[["coefficients"]][2]
    p_val[i] <- 1 - summary(l)$coefficients[,4][2]
  }
  
  beta <- array(beta,c(32,32,2))
  p_val <- array(p_val,c(32,32,2))
  
  # --
  bspace <- NeuroSpace(dim=c(32,32,2), spacing=c(1,1,1))
  beta_vol <- NeuroVol(beta, bspace)
  p_vol <- NeuroVol(p_val,bspace)
  
  write_vol(beta_vol, "beta.nii")
  write_vol(p_vol,"pvals.nii")
  
}