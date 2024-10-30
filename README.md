# voxelwise_r
Example using voxel-wise statistics in R. Functional code using logistic regression and
prediction on new data from fitted model. The code works, but the predictions are not very good.


## Running the code

### 1. Prepare csv file
Prepare the csv file with the file file path to the free water and wmh files. 

```r
df <- run_logistic()
```

### 2. Fit model 
Fit logistic model using these commands:

```r
pval_file <- "pvals.nii.gz"
beta_file <- "betas.nii.gz"
mask_file <- "mask.nii.gz"
voxel_logistic <- function(df,pval_file, beta_file, mask_file) 
```

The fitted model is represented by the beta image `betas.nii.gz`.   

### 3. Estimate model on new data 

```r
pred_file <- "predicted.nii.gz" 
voxel_predict <- function(df, beta_file,pred_file,mask_file, thr = 0.5) 
```




