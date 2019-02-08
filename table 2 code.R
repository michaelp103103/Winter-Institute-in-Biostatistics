#----------------------------------------------------------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------------------------------------------------------#
#-----------------------------------TODO: -----------------------------------------------------------#
#----------------------------------------------------------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------------------------------------------------------#



data_frame <- read.csv('C:/Users/rpisk320/Desktop/BERD WIB/procoh1.csv')


############## CREATE FUNCTION TO STANDARDIZE DATA #############

standardize_vals <- function(df) {
  
  sd_df <- data.frame(matrix(vector(mode = 'numeric',length = 3105), nrow = 3105, ncol = 1))
  
  for (i in 1:length(df)) {
    if (all(df[, i] %in% 0:1)) {
      sd_df <- data.frame(sd_df, df[, i])
    }
    else {
      no_mi_variable <- df[df$mi == 0, i]
      std_vals <- ( df[, i] - mean(no_mi_variable) )/ sd(no_mi_variable)
      sd_df <- cbind(sd_df, std_vals)
    }
  }
  
  sd_df <- sd_df[2:length(sd_df)]
  colnames(sd_df) <- colnames(data_frame)
  
  return(sd_df)
}



############### STANDARDIZE DATA #########################

data_frame <- standardize_vals(data_frame)

f_df <- data_frame[data_frame$SEX == 1, 3:12]
m_df <- data_frame[data_frame$SEX == 0, 3:12]

data_frame <- data_frame[, 3:12]


############# CREATE FUNCTION TO FIND OR, CI, AND P VAL ###############

get_OR_CI_p <- function(df, var1 = 'all.vars') {
  if (var1 == 'all.vars') {
    matrix1 <- summary(glm(mi ~ ., data = df, family = 'binomial'))$coefficients
  }
  else {
    matrix1 <- summary(glm(mi ~ var1, data = df, family = 'binomial'))$coefficients
  }
  
  or <- exp(matrix1[, 1])
  lowCI <- exp(matrix1[, 1] + qnorm(0.025) * matrix1[, 2])
  hiCI <- exp(matrix1[, 1] + qnorm(0.975) * matrix1[, 2])
  p_val <- matrix1[, 4]
  
  return(round(cbind(or, lowCI, hiCI, p_val), 3))
}

######################################################################


get_OR_CI_p(data_frame, var1 = data_frame$AGE)
get_OR_CI_p(data_frame, var1 = data_frame$BMI)
get_OR_CI_p(data_frame, var1 = data_frame$SYSBP)
get_OR_CI_p(data_frame, var1 = data_frame$hdl)
get_OR_CI_p(data_frame, var1 = data_frame$carotid)
get_OR_CI_p(data_frame, var1 = data_frame$DIABETES)
get_OR_CI_p(data_frame, var1 = data_frame$smoker)
get_OR_CI_p(data_frame, var1 = data_frame$dietfat)
get_OR_CI_p(data_frame, var1 = data_frame$inactive)
get_OR_CI_p(data_frame)


get_OR_CI_p(f_df, var1 = f_df$AGE)
get_OR_CI_p(f_df, var1 = f_df$BMI)
get_OR_CI_p(f_df, var1 = f_df$SYSBP)
get_OR_CI_p(f_df, var1 = f_df$hdl)
get_OR_CI_p(f_df, var1 = f_df$carotid)
get_OR_CI_p(f_df, var1 = f_df$DIABETES)
get_OR_CI_p(f_df, var1 = f_df$smoker)
get_OR_CI_p(f_df, var1 = f_df$dietfat)
get_OR_CI_p(f_df, var1 = f_df$inactive)
get_OR_CI_p(f_df)



get_OR_CI_p(m_df, var1 = m_df$AGE)
get_OR_CI_p(m_df, var1 = m_df$BMI)
get_OR_CI_p(m_df, var1 = m_df$SYSBP)
get_OR_CI_p(m_df, var1 = m_df$hdl)
get_OR_CI_p(m_df, var1 = m_df$carotid)
get_OR_CI_p(m_df, var1 = m_df$DIABETES)
get_OR_CI_p(m_df, var1 = m_df$smoker)
get_OR_CI_p(m_df, var1 = m_df$dietfat)
get_OR_CI_p(m_df, var1 = m_df$inactive)
get_OR_CI_p(m_df)



################ CREATE PLOT OF ODDS RATIOS WITH CI ######################

library(tidyverse)


#1) make a dataframe with variables to plot
#2) change col names to 'odds, CIHigh, CILow'
#3) add 'predictor' and 'response' cols
#4) add the code you copied to the function

detrmn_group <- function(df1) {
  if (nrow(df1) == nrow(m_df)) {return('males')} 
  else if (nrow(df1) == nrow(data_frame)) {return('whole cohort')}
  else if (nrow(df1) == nrow(f_df)) {return('females')}
}




make_ggp_df <- function(list1, col_names1) {
  
  accumulator <- NULL
  
  for (i in list1) {
    for (k in col_names1){
      odds_r_df <- get_OR_CI_p(i)[k, ]
      odds <- odds_r_df[1]
      CIHigh <- odds_r_df[2]
      CILow <- odds_r_df[3]
      predictor <- k
      response <- detrmn_group(i)
      
      loop_df <- tibble(predictor, response, odds, CIHigh, CILow)
      accumulator <- rbind.data.frame(loop_df, accumulator)
    }
  }
  
  return(accumulator)
}

list_DFs <- list(data_frame, m_df, f_df)

df <- make_ggp_df(list_DFs, c('AGE', 'SYSBP', 'hdl', 'DIABETES', 'smoker', 'inactive'))
  
signif_vars <- c('age', 'prevalent diabetes', 'hdl', 'physically inactive', 'smoker', 'systolic blood pressure')

adj = .2 # This is used in position_nudge to move the dots

ggplot(df, aes(x = odds, y = predictor, color = response)) +
  geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(data = filter(df, response== 'whole cohort'), aes(xmax = CIHigh, xmin = CILow), size = .5, height = .1, color = "gray50", position = position_nudge(y = adj)) +
  geom_point(data = filter(df, response== "whole cohort"), size = 4, position = position_nudge(y = adj)) +
  geom_errorbarh(data = filter(df, response== "males"), aes(xmax = CIHigh, xmin = CILow), size = .5, height = .1, color = "gray50") +
  geom_point(data = filter(df, response== "males"), size = 4) +
  geom_errorbarh(data = filter(df, response== "females"), aes(xmax = CIHigh, xmin = CILow), size = .5, height = .1, color = "gray50", position = position_nudge(y = - adj)) +
  geom_point(data = filter(df, response== "females"), size = 4, position = position_nudge(y = - adj)) +
  scale_x_continuous(breaks = seq(0,7,1) ) +
  coord_trans(x = "log10") +
  theme_bw() +
  ggtitle('Figure 1. Odds Ratios with Confidence Intervals of Multivariate Associations \nBetween Variables and Incident MI') +
  scale_color_hue(labels = c("females\n(p<.05)", "whole cohort\n(p<.05)", "males\n(p<.05)")) +
  #scale_y_discrete(labels= signif_vars) +
  theme(panel.grid.minor = element_blank())





#######################################################