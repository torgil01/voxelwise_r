voxel_logistic <- function(df,pval_file, beta_file) {
  # voxel-wise logistic regression 
  # Inputs
  # 1. y = binary 4d mask of wmh lesions for each person
  # 2. x = 4d free water images
  # 3. cov = csv file with covariates
  # 4. brainmask
  
  # new inputs: 
  # make a df with the following fields
  #   y_path : path to wmh lesion masks
  #   x_path : path to free water files
  #   mask_path : path to white matter mask -- just one!
  #   covar_1 ...
  
  # libs
  library(RNifti)
  library(readr)
  
  # read x, y data 
  y_img <- readNifti(df$y_path)
  x_img <- readNifti(df$x_path)
  brainmask <- readNifti(df$mask_path[1])

  # put covars in vectors
  age <- df$age
  sex <- df$sex
  num_covars = 2

  # get image dims 
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
  
  # allocate 
  # Note, R uses column order , so consecutive addresses are on first index
  # see : https://cran.r-project.org/web/packages/reticulate/vignettes/arrays.html
  pval_sp <- array(0, c(nvox,covars + 1)) 
  beta_sp <- array(0, c(nvox,covars + 1))
  # iterate
  for (i in 1:nvox) {
    y = im_sparse[i,]
    # linear regression
    model <- glm( y ~ age + sex, family = binomial)
    s <- summary(model)
    beta_sp[i,] <- coef(s)[,1] # 
    pval_sp[i,] <- coef(s)[,4] # 
  }
  
  # reconstruct 
  # - first replicate index on 4th dim
  shifts <- seq(from = 0, to = nx*ny*nz*(nn-1), by = nx*ny*nz)
  indx_4d <- rep(indx, times = nn) + rep(shifts, each = length(indx))
  # - allocate arrays
  pval <- array(0,c(nx,ny,nz, covars + 1))
  beta <- array(0,c(nx,ny,nz, covars + 1))
  pval[indx_4d] <- pval_sp
  beta[indx_4d] <- beta_sp
  
  # save 
  pval.nii <- asNifti(pval,reference = mask)
  writeNifti(pval.nii,pval_file)
  beta.nii <- asNifti(beta,reference = mask)
  writeNifti(beta.nii,beta_file)
}

