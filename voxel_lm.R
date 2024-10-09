vox_stats <- function() {
  # if we are not using spare we might just as well use rnifti 
  # need to check the 4d reading capabilities
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
  d <- dim(dat)
  nvox <- d[1]
  beta <- array(0,nvox)
  p_val <- array(0,nvox)
  
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