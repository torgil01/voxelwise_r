# voxelwise_r
do voxel-wise statistics in R


## 1. implementation with RNifti package
The `RNifti` package is fast and intuitive to use, but lack support for memory mapped files. If it becomes a problem to store all files in memory, we have to 
use another package.

See `test_rnifti.R` for example with linear regression on the rat dataset.

