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


vol <- read_vol(fw_file)
mask <- read_vol(mask_file)
indices <- which(mask!=0)
# vectorize vol
dat <- vol[indices]
beta <- array(0,length(dat))
# iterate
for (i in 1:length(dat)) {
  
  # run model for i 
  
  # collect beta
  beta[i] <- mod.beta
}


# write beta as image