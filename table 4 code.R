#--------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------#
#-------------------------TODO: -------------------------------#
#--------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------#


data_frame <- read.csv('C:/Users/rpisk320/Desktop/BERD WIB/procoh1.csv')


data_frame <- data_frame[, 2:12]


############# FIND INTERACTION COEFFICIENTS ###############

find_interact_coef <- function(df, var1) {
  table1 <- summary(glm(mi ~ . + SEX*var1, data = df, family = 'binomial'))$coefficients

  coefficient <- table1[, 1]
  se <- table1[, 2]
  waldxsq <- (table1[, 3])^2
  p_val <- table1[, 4]
  
  return_df <- round(cbind(coefficient, se, waldxsq, p_val), 3)
  
  return(return_df)
}


find_interact_coef(data_frame, data_frame$AGE)
find_interact_coef(data_frame, data_frame$BMI)
find_interact_coef(data_frame, data_frame$SYSBP)
find_interact_coef(data_frame, data_frame$hdl)
find_interact_coef(data_frame, data_frame$carotid)
find_interact_coef(data_frame, data_frame$DIABETES)
find_interact_coef(data_frame, data_frame$smoker)
find_interact_coef(data_frame, data_frame$dietfat)
find_interact_coef(data_frame, data_frame$inactive)


