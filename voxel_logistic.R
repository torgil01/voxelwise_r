voxel_logistic <- function(df,pval_file, beta_file, mask_file) {
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
  library(glm2)

  # get number of timepoints
  nt = nrow(df)

  # read brainmask 
  brainmask <- readNifti(df$mask_path[1])
  # get image dims 
  im_dim <- dim(brainmask)
  cat("im_dim = ",im_dim,"\n")
  nx <- im_dim[1]
  ny <- im_dim[2]
  nz <- im_dim[3]
  
  # read x, y data 
  cat("reading x and y ..")
  y_img <- array(0,c(nx,ny,nz,nt))
  x_img <- array(0,c(nx,ny,nz,nt))
  
  for (i in 1:nt) {
    cat(i,",")
    y_img[,,,i] <- readNifti(df$wmh_path[i])
    x_img[,,,i] <- readNifti(df$fw_path[i])
  }  
  cat("\n")

  # put covars in vectors
  age <- df$age
  sex <- df$sex
  num_covars = 3
  
  # make a mask that contains at least min_obs wmh, otherwise it is no point 
  # running the logistic model!
  min_obs = 1  # use 10*num_covars  .. "one in ten rule" 
  wmh_freq <- rowSums(y_img, dims = 3)
  wmh_freq[wmh_freq < min_obs] <- 0
  wmh_freq[wmh_freq >= min_obs] <- 1
  mask_vec <- array(wmh_freq,c(nx*ny*nz)) 
  indx <- which(mask_vec > 0)
  
  # save mask vec
  wmh_freq.nii <- asNifti(wmh_freq)
  writeNifti(wmh_freq.nii,mask_file,template = brainmask)
  rm(wmh_freq)
  
  # reshape x and y 
  y_img <- array(y_img,c(nx*ny*nz,nt))
  x_img <- array(x_img,c(nx*ny*nz,nt))
  
  # mk sparse
  y_sparse <- y_img[indx,] 
  x_sparse <- x_img[indx,] 
  sp_dim <- dim(y_sparse)
  nvox <- sp_dim[1]
  ntime <- sp_dim[2]
  cat("nii sparse image dims: nvox = ",nvox,", ntime = ",ntime,"\n")
  
  # free memory
  rm(x_img,y_img)
  
  # allocate 
  # Note, R uses column order , so consecutive addresses are on first index
  # see : https://cran.r-project.org/web/packages/reticulate/vignettes/arrays.html
  
  con <- glm.control(maxit = 200)
  pval_sp <- array(0, c(nvox,num_covars + 1)) 
  beta_sp <- array(0, c(nvox,num_covars + 1))
  cat("Running voxelwise regression\n")
  # iterate
  for (i in 1:nvox) {
    y = y_sparse[i,]
    x = x_sparse[i,]
    # linear regression
    model <- glm2( y ~ x + age + sex, family = binomial, control = con)
    s <- summary(model)
    beta_sp[i,] <- coef(s)[,1] # 
    pval_sp[i,] <- coef(s)[,4] # 
  }
  
  # reconstruct 
  # - first replicate index on 4th dim
  cat("Reconstructing arrays\n")
  shifts <- seq(from = 0, to = nx*ny*nz*(num_covars), by = nx*ny*nz)
  cat("dim shifts = ", length(shifts),"class = ",class(shifts), "\n")
  indx_4d <- rep(indx, times = nt) + rep(shifts, each = length(indx))
  cat("dim indx_4d = ", length(indx_4d),"class = ",class(indx_4d), "\n")
  # - allocate arrays
  pval <- array(0,c(nx,ny,nz, num_covars + 1))
  cat("dim pval = ", dim(pval)," class pval = ", class(pval), '\n')
  cat("dim pval_sp = ", dim(pval_sp)," class pval_sp = ", class(pval_sp), '\n')
  beta <- array(0,c(nx,ny,nz, num_covars + 1))
  pval[indx_4d] <- pval_sp
  beta[indx_4d] <- beta_sp
  
  
  # save 
  cat("Saving betas and p-values\n")
  cat("dim pval = ", dim(pval),'\n')
  cat("dim beta = ", dim(beta),'\n')
  pval.nii <- asNifti(pval)
  writeNifti(pval.nii,pval_file,template = brainmask)
  beta.nii <- asNifti(beta)
  writeNifti(beta.nii,beta_file, template = brainmask)
  
  # test
  data <- list("y" = y_sparse[5000,], "x" = x_sparse[5000,], "age" = age, "sex" = sex )
  return(data)
}

