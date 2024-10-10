array_test <- function() {
  # test how to manipulate arrays
  
  # the initial image data is a 4D array 
  nx <- 4
  ny <- 4
  nz <- 4
  nn <- 3
  
  im <- array(rnorm(nx*ny*nz*nn),c(nx,ny,nz,nn))
  mask <- array(0,c(nx,ny,nz))
  
  indx <- c(1,2,21,22,63,64)
  mask[indx] <- 1
  
  # vectorize im
  im <- array(im,c(nx*ny*nz,nn))
  im_sparse <- im[indx,]
  #dim(im_sparse)
  
  # allocate new 
  p_sparse <- array(0,c(length(indx),nn))
  for (i in 1:length(indx)) {
    y <- im_sparse[i,]
    p_sparse[i,] <- y*10
  }
  
  # reconstruct p
  # - probably best to replicate index on 4th dim
  shifts <- seq(from = 0, to = nx*ny*nz*(nn-1), by = nx*ny*nz)
  indx_4d <-  rep(indx, times = nn) + rep(shifts, each = length(indx))
  
  cat("shifts = ", shifts, "\n")
  cat("add_vec = ", rep(indx, times = nn), "\n")
  cat("add_val = ",rep(shifts, each = length(indx)),"\n")
  cat("indx 4d = ", indx_4d,"\n")  
  
  p <- array(0,c(nx,ny,nz,nn))
  p[indx_4d] <- p_sparse
  
  p
  
  
}