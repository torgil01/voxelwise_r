run_logistic <- function() {
  # assemble variables for logistic regression
  #
  
  # libs
  library(dplyr)
  library(tidyr)
  library(readr)
  
  # csv file created from G100 project as follows: 
  # data <- prep_dwi_data()
  # df.wmh <- data[[2]]
  # df.ex <- df.wmh %>% filter(!is.na(wmh_fw)) %>% select(GMFID, Time_point, Age, Sex)
  # write_csv(df.ex,"data_for_voxelvise.csv")
  df <- read_csv("data/data_for_voxelwise.csv") %>% 
    filter(Time_point %in% c(1,5)) %>%
    group_by(GMFID) %>%
    filter(max(row_number()) == 2) %>% 
    ungroup()
 
  # pivot wide
 
  df.w <- pivot_wider(df,
                      id_cols=GMFID,
                      names_from = Time_point, 
                      values_from = c(Age,Sex)) %>%
    rename(Sex = Sex_1) %>%
    select(-Sex_5, -Age_5) %>%
    mutate(fw_path = "") %>%
    mutate(wmh_path = "") %>%
    mutate(mask_path = "") %>% 
    rename(age = Age_1) %>% 
    rename(sex = Sex)

  # add paths for free water at tp = 1, and wmh at tp = 5
  base_path="/media/torgil/16ccecb7-2900-422e-8ea5-ff92c8e850dc/g100_fwe/data/mni_easyreg"

  n_cases <- dim(df.w)[1]
  fw_name <- "fwe_fw_volume_fraction_1.nii.gz"
  wmh_name <- "wmh_stackgen_cerebrum_5.nii.gz" 
  mask_name <- "wm_mask_1.nii.gz"
  
  for (i in 1:n_cases) {
    df.w$fw_path[i] <- file.path(base_path,df.w$GMFID[i],fw_name)
    df.w$wmh_path[i] <- file.path(base_path,df.w$GMFID[i],wmh_name)
    df.w$mask_path[i] <- file.path(base_path,df.w$GMFID[i],mask_name)
  }
  
  
  df.train <- df.w %>% slice_sample(n = 10)
  
  
  
  
  return(df.train)
  
  
  
  
}