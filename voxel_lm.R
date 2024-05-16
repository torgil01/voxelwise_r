# simple test for voxel-wise statistics


# use rat data 
# first use lm and verify with spm

# Model 
# y(img) ~ group 


# libs
library(RNifti)
# ants 
# read y data 

mnifilename<-getANTsRData("mni")
img<-antsImageRead(mnifilename)
antsImageWrite(img,mnifilename)
antsGetSpacing(img)
antsGetDirection(img)
antsGetOrigin(img)
print(antsGetPixels(img,50,60,44))
print(max(img))

mat<-imageListToMatrix( ilist, mask )
age<-rnorm( nrow(mat) ) # simulated age
gender<-rep( c("F","M"), nrow(mat)/2 ) # simulated gender
# this creates "real" but noisy effects to detect
mat<-mat*(age^2+rnorm(nrow(mat)))
mdl<-lm( mat ~ age + gender )
mdli<-bigLMStats( mdl, 1.e-4 )
print(names(mdli))
print(rownames(mdli$beta.t))
print(paste("age",min(p.adjust(mdli$beta.pval[1,]))))
print(paste("gen",min(p.adjust(mdli$beta.pval[2,]))))

agebetas<-makeImage( mask , mdli$beta.t[1,] )
antsImageWrite( agebetas, tempfile(fileext ='.nii.gz') )