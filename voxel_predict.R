voxel_predict <- function(df, beta_file,pred_file,mask_file, thr = 0.5) {
  # predict data from logistic model
  library(RNifti)
  library(boot)
  library(dplyr)
  
  # read the betas
  betas <- readNifti(beta_file)
  # get image dims 
  im_dim <- dim(betas)
  cat("im_dim = ",im_dim,"\n")
  nx <- im_dim[1]
  ny <- im_dim[2]
  nz <- im_dim[3]
  np <- im_dim[4] # number of predictors
  
  
  # number of new data points
  nt = nrow(df)
  cat("Number of obs = ", nt,"\n")
  
  # read new data 
  x_img <- array(0,c(nx,ny,nz,nt))
  for (i in 1:nt) {
    cat(i,",",df$fw_path[i],"\n")
    x_img[,,,i] <- readNifti(df$fw_path[i])
  }  
  cat("\n")
  
  
  cat("Reading mask : ", mask_file, "\n")
  mask <- readNifti(mask_file)
  # only look at betas != 0 
  indx <- which(mask != 0)
  #mask <- array(0,c(nx,ny,nz))
  #mask[indx] <- 1
  
  # make vector with non-zero betas
  betas <- array(betas,c(nx*ny*nz,np))
  beta_sp <- betas[indx,]
  x_img <- array(x_img,c(nx*ny*nz,nt))
  x_sparse <- x_img[indx,]
  
  # put covars in vectors
  df.c <- df %>% 
    select(age,sex) %>%
    mutate(constant = 1) %>%
    mutate(x = 0) %>%
    mutate(sex = ifelse(sex == "Male",1,0)) %>%
    relocate(constant,x,age,sex)
    
  # coef matrix
  cc <- array(unlist(df.c),c(nt,np)) # num obs x num pred
  bc <- array(0,c(np,nt) , dimnames = list(names(df.c)) )
  p <- array(0,c(length(indx),nt))
  cat("dim x_sparse = ", dim(x_sparse),"\n")
  #tmp <- rep(0,nt)
  # loop 
  for (i in 1:length(indx)) {
    # difficult to use predict since we would have to reconstruct the lm object
    # however this gives the same result as predict(model, df, type = "response")
    #tmp <-  x_sparse[i,] # can't address named col
    # cat("length tmp = ", length(tmp),"class = ", class(tmp),"tmp = ",tmp,"\n")
    cc[,2] <- x_sparse[i,]
    #cat("dim cc = ", dim(cc),"class = ",class(cc), "\n")
    b <- beta_sp[i,]
    #cat("dim b = ", length(b),"\n")
    ee <- rowSums(t(b*t(cc)))
    #cat("dim ee = ", length(ee),"\n")
    p[i,] <- inv.logit(ee)
  }
  
  # binarize p
  p[p > thr] <- 1
  p[p <= thr] <- 0
  
  
  # reconstruct array for probability 
  # - first replicate index on 4th dim
  cat("Reconstructing arrays\n")
  pval <- array(0,c(nx,ny,nz,nt))
  cat("dim pval = ", dim(pval)," class pval = ", class(pval), '\n')
  shifts <- seq(from = 0, to = nx*ny*nz*(nt-1), by = nx*ny*nz)
  cat("dim shifts = ", length(shifts), "\n")
  indx_4d <- rep(indx, times = nt) + rep(shifts, each = length(indx))
  cat("dim indx_4d = ", length(indx_4d), "\n")
  pval[indx_4d] <- p
  
  
  templ <- readNifti(df$fw_path[1])
  # save 
  cat("Saving betas and p-values\n")
  cat("dim pval = ", dim(pval),'\n')
  pval.nii <- asNifti(pval)
  writeNifti(pval.nii,pred_file,template = templ)
}