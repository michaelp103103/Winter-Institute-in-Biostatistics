
#---------------------------------------------------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------------------------------------------#
#-------TODO: REMEMBER TO TRY LOG10 ON YOUR CONTINUOUS VARIABLES AND PUT "VAR.EQUAL = TRUE" FOR THE T.TEST AT THE BOTTOM OF THE CODE----------------#
#---------------------------------------------------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------------------------------------------#

################################ TABLE 1 ######################################

find_data_chars <- function(my_data) {
  
  age <- data.frame(c(NA, NA, mean(my_data$AGE), sd(my_data$AGE)))
  
  bmi <- data.frame(c(NA, NA, mean(my_data$BMI), sd(my_data$BMI)))
  
  sys_bp <- data.frame(c(NA, NA, mean(my_data$SYSBP), sd(my_data$SYSBP)))
  
  hdl <- data.frame(c(NA, NA, mean(my_data$hdl), sd(my_data$hdl)))
  
  cas <- data.frame(c(NA, NA, mean(my_data$carotid), sd(my_data$carotid)))
  
  d_count <- sum(my_data$DIABETES)
  diabetes <- data.frame(c(d_count, d_count/length(my_data$DIABETES), NA, NA))
  
  s_count <- sum(my_data$smoker)
  smoker <- data.frame(c(s_count, s_count/length(my_data$smoker), NA, NA))
  
  f_count <- sum(my_data$dietfat)
  fat_diet <- data.frame(c(f_count, f_count/length(my_data$dietfat), NA, NA))
  
  i_count <- sum(my_data$inactive)
  inactive <- data.frame(c(i_count, i_count/length(my_data$inactive), NA, NA))
  
  m_count <- sum(my_data$mi)
  mi <- data.frame(c(m_count, m_count/length(my_data$mi), NA, NA))
  
  base_char <- cbind(age, bmi, sys_bp, hdl, cas, diabetes, smoker, fat_diet, inactive, mi)
  colnames(base_char) <- c('Age (years)', 'BMI (kg/m^2', 'Systolic BP (mmHg)', 'HDL-cholesterol (mg/dL)', 'Carotid artery score (units)',
                           'Diabetes (yes)', 'Current smoker (yes)', 'High fat diet (yes)', 'Physically inactive (yes)', 'Incident MI (yes)')
  row.names(base_char) <- c('Count', 'Percentage', 'Mean', 'Standard Deviation')
  
  return(base_char)
}

run_prop_test <- function(m_variable, f_variable, type_test) {
  m_positive <- sum(m_variable)
  f_positive <- sum(f_variable)
  
  prop.test(x = c(m_positive, f_positive), n = c(num_male, num_female), p = NULL, correct = FALSE, alternative = type_test, conf.level=0.95)
}

data_frame <- read.csv('C:/Users/rpisk320/Desktop/BERD WIB/procoh1.csv')
f_df <- data_frame[data_frame$SEX== 1, 1:12]
m_df <- data_frame[data_frame$SEX == 0, 1:12]

sex <- data_frame$SEX
age <- data_frame$AGE
bmi <- data_frame$BMI
sys_bp <- data_frame$SYSBP
hdl <- data_frame$hdl
cas <- data_frame$carotid
diabetes <- data_frame$DIABETES
smoker <- data_frame$smoker
fat_diet <- data_frame$dietfat
inactive <- data_frame$inactive
mi <- data_frame$mi


####### BASELINE CHARACTERISTICS OF OVERALL COHORT #######

cohort_base <- find_data_chars(data_frame)

######## BASELINE CHARACTERISTICS BY SEX ###########

##### FEMALES #####

f_diabetes <- f_df$DIABETES
f_smoker <- f_df$smoker
f_fat_diet <- f_df$dietfat
f_inactive <- f_df$inactive
f_mi <- f_df$mi

num_female <- length(f_df$RANDID)
f_base <- find_data_chars(f_df)

#### MALES ######

m_diabetes <- m_df$DIABETES
m_smoker <- m_df$smoker
m_fat_diet <- m_df$dietfat
m_inactive <- m_df$inactive
m_mi <- m_df$mi

num_male <- length(m_df$RANDID)
m_base <- find_data_chars(m_df)

########  TEST FOR NORMALITY  ##########

qqnorm(m_df$AGE); qqline(m_df$AGE)
qqnorm(f_df$AGE); qqline(f_df$AGE)
qqnorm(data_frame$AGE); qqline(data_frame$AGE)

qqnorm(log(m_df$BMI)); qqline(log(m_df$BMI))
qqnorm(log(f_df$BMI)); qqline(log(f_df$BMI))
qqnorm(log(data_frame$BMI)); qqline(log(data_frame$BMI))

qqnorm(1/m_df$SYSBP); qqline(1/m_df$SYSBP)
qqnorm(1/f_df$SYSBP); qqline(1/f_df$SYSBP)
qqnorm(1/data_frame$SYSBP); qqline(1/data_frame$SYSBP)

qqnorm(sqrt(m_df$hdl)); qqline(sqrt(m_df$hdl))
qqnorm(sqrt(f_df$hdl)); qqline(sqrt(f_df$hdl))
qqnorm(sqrt(data_frame$hdl)); qqline(sqrt(data_frame$hdl))

qqnorm(1/m_df$carotid); qqline(1/m_df$carotid)
qqnorm(1/f_df$carotid); qqline(1/f_df$carotid)
qqnorm(1/data_frame$carotid); qqline(1/data_frame$carotid)

########  DETERMINE IF VARIANCES DIFFERENT  ############

var.test(m_df$AGE, f_df$AGE)    # variances are same

var.test(log(m_df$BMI), log(f_df$BMI), alternative = 'two.sided') #variances are different

var.test(1/m_df$SYSBP, 1/f_df$SYSBP)   #variances are different

var.test(sqrt(m_df$hdl), sqrt(f_df$hdl))  #variances are different

var.test(1/m_df$carotid, 1/f_df$carotid) #variances are same




######## FIND P-VALUES #############

t.test(m_df$AGE, f_df$AGE, var.equal = TRUE, alternative = 'two.sided')

t.test(log(m_df$BMI), log(f_df$BMI), alternative = 'two.sided')

t.test(1/m_df$SYSBP, 1/f_df$SYSBP, alternative = 'two.sided')

t.test(sqrt(m_df$hdl), sqrt(f_df$hdl), alternative = 'two.sided')

t.test(m_df$carotid, f_df$carotid, var.equal = TRUE, alternative = 'two.sided')





run_prop_test(m_diabetes, f_diabetes, 'two.sided')

run_prop_test(m_smoker, f_smoker, 'two.sided')

run_prop_test(m_fat_diet, f_fat_diet, 'two.sided')

run_prop_test(m_inactive, f_inactive, 'two.sided')

run_prop_test(m_mi, f_mi, 'two.sided')
##############################################################################