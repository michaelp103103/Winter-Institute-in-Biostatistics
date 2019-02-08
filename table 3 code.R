#--------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------#
#-------------------------TODO: -------------------------------#
#--------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------#

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

############# CREATE FUNCTION TO FIND OR, CI, AND P VAL ###############

find_OR_CI_p <- function(df, var1='all.vars') {
  if (var1 == 'all.vars') {matrix1 <- summary(glm(mi ~ ., data = df, family = 'binomial'))$coefficients}
  
  else {matrix1 <- summary(glm(mi ~ var1, data = df, family = 'binomial'))$coefficients}
  
  OR <- exp(matrix1[, 1])
  lowCI <- exp(matrix1[, 1] + qnorm(0.025) * matrix1[, 2])
  hiCI <- exp(matrix1[, 1] + qnorm(0.975) * matrix1[, 2])
  p_val <- matrix1[, 4]
  
  return_df <- round(cbind(OR, lowCI, hiCI, p_val), 3)
  
  return(return_df)
}

#################################################################

f_df <- data_frame[data_frame$SEX == 1, 3:12]
m_df <- data_frame[data_frame$SEX == 0, 3:12]

data_frame <- data_frame[, 3:12]

median_age <- as.vector(summary(data_frame$AGE)['Median'])

f_df_y <- f_df[f_df$AGE < median_age, ]
f_df_o <- f_df[f_df$AGE >= median_age, ]

m_df_y <- m_df[m_df$AGE < median_age, ]
m_df_o <- m_df[m_df$AGE >= median_age, ]

#1) split data frames into below and above median age
#2) create function that finds OR, CI, and p value
#3) create function call lines

find_OR_CI_p(f_df_y, var1 = f_df_y$AGE)
find_OR_CI_p(f_df_y, var1 = f_df_y$BMI)
find_OR_CI_p(f_df_y, var1 = f_df_y$SYSBP)
find_OR_CI_p(f_df_y, var1 = f_df_y$hdl)
find_OR_CI_p(f_df_y, var1 = f_df_y$carotid)
find_OR_CI_p(f_df_y, var1 = f_df_y$DIABETES)
find_OR_CI_p(f_df_y, var1 = f_df_y$smoker)
find_OR_CI_p(f_df_y, var1 = f_df_y$dietfat)
find_OR_CI_p(f_df_y, var1 = f_df_y$inactive)
find_OR_CI_p(f_df_y)

find_OR_CI_p(f_df_o, var1 = f_df_o$AGE)
find_OR_CI_p(f_df_o, var1 = f_df_o$BMI)
find_OR_CI_p(f_df_o, var1 = f_df_o$SYSBP)
find_OR_CI_p(f_df_o, var1 = f_df_o$hdl)
find_OR_CI_p(f_df_o, var1 = f_df_o$carotid)
find_OR_CI_p(f_df_o, var1 = f_df_o$DIABETES)
find_OR_CI_p(f_df_o, var1 = f_df_o$smoker)
find_OR_CI_p(f_df_o, var1 = f_df_o$dietfat)
find_OR_CI_p(f_df_o, var1 = f_df_o$inactive)
find_OR_CI_p(f_df_o)

find_OR_CI_p(m_df_y, var1 = m_df_y$AGE)
find_OR_CI_p(m_df_y, var1 = m_df_y$BMI)
find_OR_CI_p(m_df_y, var1 = m_df_y$SYSBP)
find_OR_CI_p(m_df_y, var1 = m_df_y$hdl)
find_OR_CI_p(m_df_y, var1 = m_df_y$carotid)
find_OR_CI_p(m_df_y, var1 = m_df_y$DIABETES)
find_OR_CI_p(m_df_y, var1 = m_df_y$smoker)
find_OR_CI_p(m_df_y, var1 = m_df_y$dietfat)
find_OR_CI_p(m_df_y, var1 = m_df_y$inactive)
find_OR_CI_p(m_df_y)

find_OR_CI_p(m_df_o, var1 = m_df_o$AGE)
find_OR_CI_p(m_df_o, var1 = m_df_o$BMI)
find_OR_CI_p(m_df_o, var1 = m_df_o$SYSBP)
find_OR_CI_p(m_df_o, var1 = m_df_o$hdl)
find_OR_CI_p(m_df_o, var1 = m_df_o$carotid)
find_OR_CI_p(m_df_o, var1 = m_df_o$DIABETES)
find_OR_CI_p(m_df_o, var1 = m_df_o$smoker)
find_OR_CI_p(m_df_o, var1 = m_df_o$dietfat)
find_OR_CI_p(m_df_o, var1 = m_df_o$inactive)
find_OR_CI_p(m_df_o)




####################### MAKE ODDS RATIO WITH CI PLOT ########################

detrmn_group <- function(df1) {
  if (nrow(df1) == nrow(m_df_y)) {return('males')} 
  else if (nrow(df1) == nrow(f_df_y)) {return('females')}
}


make_ggp_df <- function(list1, col_names1) {
  
  accumulator <- NULL
  
  for (i in list1) {
    for (k in col_names1){
      odds_r_df <- find_OR_CI_p(i)[k, ]
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


list_of_df <- list(f_df_y, m_df_y)

df0 <- make_ggp_df(list_of_df, c('hdl', 'DIABETES', 'smoker'))

signif_vars <- c('prevalent diabetes', 'hdl', 'smoker')

adj = .2 # This is used in position_nudge to move the dots

ggplot(df0, aes(x = odds, y = predictor, color = response)) +
  geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(data = filter(df0, response== "males"), aes(xmax = CIHigh, xmin = CILow), size = .5, height = .1, color = "gray50") +
  geom_point(data = filter(df0, response== "males"), size = 4) +
  geom_errorbarh(data = filter(df0, response== "females"), aes(xmax = CIHigh, xmin = CILow), size = .5, height = .1, color = "gray50", position = position_nudge(y = - adj)) +
  geom_point(data = filter(df0, response== "females"), size = 4, position = position_nudge(y = - adj)) +
  scale_x_continuous(breaks = seq(0,7,1) ) +
  coord_trans(x = "log10") +
  theme_bw() +
  ggtitle('Figure 2. Odds Ratios with CIs of Multivariate Associations of Variables with MI for \nMales and Females Below Median Age of Cohort') +
  scale_color_hue(labels = c("females\n(p<.05)", "males\n(p<.05)")) +
  scale_y_discrete(labels= signif_vars) +
  theme(panel.grid.minor = element_blank())





list2 <- list(f_df_o, m_df_o)

data_frame2 <- make_ggp_df(list2, c('SYSBP', 'inactive'))

var_names <- c('inactive', 'systolic BP')


ggplot(data_frame2, aes(x = odds, y = predictor, color = response)) +
  geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(data = filter(data_frame2, response== "males"), aes(xmax = CIHigh, xmin = CILow), size = .5, height = .1, color = "gray50") +
  geom_point(data = filter(data_frame2, response== "males"), size = 4) +
  geom_errorbarh(data = filter(data_frame2, response== "females"), aes(xmax = CIHigh, xmin = CILow), size = .5, height = .1, color = "gray50", position = position_nudge(y = - adj)) +
  geom_point(data = filter(data_frame2, response== "females"), size = 4, position = position_nudge(y = - adj)) +
  scale_x_continuous(breaks = seq(0,7,1) ) +
  coord_trans(x = "log10") +
  theme_bw() +
  ggtitle('Figure 3. Odds Ratios with CIs of Multivariate Associations of Variables with MI for \nMales and Females Above Median Age of Cohort') +
  scale_color_hue(labels = c("females\n(p<.05)", "males\n(p<.05)")) +
  scale_y_discrete(labels= var_names) +
  theme(panel.grid.minor = element_blank())

#----------------------------------END OF CODE--------------------------------------#
#--------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------#






