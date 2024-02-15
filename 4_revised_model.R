# Load libraries
library(tidyverse)
library(haven)
library(ggplot2)
library(psych)
library(car)
library(MASS)

# Read data and select variables 

d = read_dta("/Users/mac/OneDrive - UNICEF/Zambia/3_dofiles/28_Models_Yu/Samplecoie1+weight copy.dta")
d = as.data.frame(d)

var_list = c('mean_scoret', 'mean_scorem', 'mean_scoref', 'exam_quintilest',
             'srt1_7', 'srt8_12', 'srt1_12', 
             'pri_coie_t1', 'sec_coie_t1',
             'gpi_pri_coie_1', 'gpi_sec_coie_1', 
             'gpi_sr1_7', 'gpi_sr8_12', 'gpi_sr1_12',
             'elevel', 'elevelx', 'region', 'schoolname', 'weights',
             'distance', 'class_size2017', 'class_size2018',
             'ptr2018', 'ptr2017', 'g_t_r2018', 'g_t_r2017',
             'retr_tpri', 'retr_tsec','gpi_retr_pri', 'gpi_retr_sec', 
             'G7p_txtbk_r2017', 'G12p_txtbk_r2017', 'Schp_txtbk_r2017',
             'f_index', 'e_index')
d_model = d[, var_list]

#1. Sample sizes and complete cases
## Sample size broken down by education levels
d_model %>% count(elevel)
###elevel     n
###1          2
###2         62
###3          1
###4       1545

## complete_cases for each variable of interst
cc = numeric(length = length(var_list))
names(cc) = var_list

for (var in var_list){
  cc[var] = sum(complete.cases(d_model[, var]))
}
cc

## Take a closer look at gpi_sr8-12 since it has 1610 complete cases
d_model %>%
  dplyr :: select('elevel', 'gpi_sr1_7', 'gpi_sr8_12', 'gpi_sr1_12') %>%
  filter(elevel == 1)
 ### the two primary schools have 0 on gpi_sr8_12 and gpi_sr1_12, but NA on gpi_sr1_7

############################ Dependent Variables and apply weights ####################################
##1. Apply weights
d_model %>%
  filter(weights == 0 | is.na(weights)) %>%
  tally() ### No na or 0 value for weights

weight = function(x, weights = d_model$weights) {
  x = x*weights
}

dv_list = c('mean_scoret', 'mean_scorem', 'mean_scoref', 'exam_quintilest',
            'srt1_7', 'srt8_12', 'srt1_12', 
            'pri_coie_t1', 'sec_coie_t1',
            'gpi_pri_coie_1', 'gpi_sec_coie_1', 
            'gpi_sr1_7', 'gpi_sr8_12', 'gpi_sr1_12')

head(d_model[, dv_list])

d_model = d_model%>%
  mutate(across(c('mean_scoret', 'mean_scorem', 'mean_scoref', 'exam_quintilest',
                  'srt1_7', 'srt8_12', 'srt1_12', 
                  'pri_coie_t1', 'sec_coie_t1',
                  'gpi_pri_coie_1', 'gpi_sec_coie_1', 
                  'gpi_sr1_7', 'gpi_sr8_12', 'gpi_sr1_12'), weight))

head(d_model[, dv_list])

##2. Distributions of DVs
par(mfrow = c(3, 5))

for (var in dv_list) {
  hist(d_model[, var], 
       xlab = var,
       main = paste('Histogram of', var, sep = ' '),
       freq = FALSE
  )
}

par(mfrow = c(1, 1))

##3. correlations (exclude gpi_sr1_7 because of too many missings)
(cor_dv =cor(d_model[, dv_list[-12]], use = 'pairwise.complete.obs'))
pairs(d_model[, dv_list[-12]])


############################### Independent Variables ###########################################
##1. Complete Cases (cc)
iv_list = c('distance', 'class_size2017', 'class_size2018',
            'ptr2018', 'ptr2017', 'g_t_r2018', 'g_t_r2017',
            'retr_tpri', 'retr_tsec', 'gpi_retr_pri', 'gpi_retr_sec',
            'Schp_txtbk_r2017', 'f_index', 'e_index')

cc_sum = function(x){
  cc = sum(complete.cases(x))
}
apply(d_model[, iv_list], 2, cc_sum)
### gpi_retr_sec has 0 complete cases

d_model %>%
  dplyr :: select(gpi_retr_sec, elevel) %>%
  filter(elevel == 2 | elevel == 3) 
### No secondary schools (elevel == 2|3) has gpi_retr_sec

d_model %>%
  dplyr :: select(gpi_retr_pri, elevel) %>%
  filter(elevel == 4 & is.na(gpi_retr_pri)) 
### Only one pri-secondary (elevel == 4) has na on gpi_retr_pri

### Thus, with regard to gpi_retr, maybe has to delete gpi_retr_sec in our model

##2. Correlations
(cor_iv = cor(d_model[, iv_list[-11]], use = 'pairwise.complete.obs'))
pairs(d_model[, iv_list[-11]])
cortest.bartlett(cor_iv) # Coorelations are significant, factor analysis are appropriate

##3. factor analysis of IVs
eigen(cor_iv)$values
plot(eigen(cor_iv)$values, ylab = 'Eigenvalue size', main = 'Scree plot', type = 'b')
abline(h = 1)
# It seems 5, 6 factors should be retained

### principal component method
(fa1_iv = principal(cor_iv, nfactor = 6, rotate = 'varimax'))
print.psych(fa1_iv, digits = 2, cut = .3)
#### Six factors explained 73% of total variance
#### RC1: ptr2018&2017
#### RC2: class_size2017&2018
#### RC3: g_t_r2017&2018
#### RC4: f_index & e_index + distance
#### RC5: retr_rpri&retr_tsec
#### RC6: gpi_retr_pri


#### Oblique rotation
(fa2_iv = principal(cor_iv, nfactor = 6, rotate = 'oblimin')) # correlations between factors are small
print.psych(fa2_iv, digits = 2, cut = .3)

### factor analysis using the MLE method
(fa3_iv = factanal(factors = 6, covmat = cor_iv, rotation = 'varimax'))

### with 5 factors
(fa4_iv = principal(cor_iv, nfactor = 5, rotate = 'varimax')) 
print.psych(fa4_iv, digits = 2, cut = .3)
#### explained 65% of total variance
#### RC5 is the combination of retr_tpri, retr_tsec, and gpi_retr_pri

(fa5_iv = factanal(factors = 5, covmat = cor_iv, rotation = 'varimax'))

##################################### Priliminary  Models ###################################################
#1. To what extent are educational outcomes equitable in Zambia?
 ##1.1 Independent sample t test to compare mean_scorem and mean_scoref
hist(d_model$mean_scorem, xlim = c(0, 200), freq = FALSE, breaks = 80) # right skewed
hist(d_model$mean_scoref, xlim = c(0, 200), freq = FALSE, breaks = 80) # right skewed

var(d_model$mean_scorem, na.rm = TRUE) # 2749.914
var(d_model$mean_scoref, na.rm = TRUE) #2455.871
 ###1.1.1 Because of large sample sizes (n > 800), violations of normality should not cause much concern
(t_mean_score_mf = t.test(d_model$mean_scorem, d_model$mean_scoref))
t_mean_score_mf$estimate
 ### t(1746) = 0.23138, p = 0.817, no significant differences, x_bar_m = 22.271, x_bar_f = 21.607
 
 ###1.1.2 Non-parametric test: Mann Whitney U test
wilcox.test(d_model$mean_scorem, d_model$mean_scoref)
 ### p = 0.3316, no significant difference

 ##1.2 ANOVA of mean_score by region
d_model$region = factor(d_model$region)
lm_mean_score_t_reg = lm(mean_scoret ~ region, data = d_model)
summary(lm_mean_score_t_reg) ### R^2 = 0.1284

qqPlot(lm_mean_score_t_reg)
boxplot(d_model$mean_scoret ~ d_model$region)
by(d_model$mean_scoret, d_model$region, sd)
leveneTest(lm_mean_score_t_reg) 
 ### P = 0.2565, thus, seems anova assumptions are tenable
aov_mean_scoret_region = aov(formula = mean_scoret ~ region, data = d_model)
summary(aov_mean_scoret_region)
 ### F(9, 868) = 14.21, p < .01, significant differences among regions

 ##1.2.2 Pairwise comparisons
library(emmeans)
emm1 = emmeans(object = aov_mean_scoret_region,
               specs = ~ region)
pairs(emm1, adjust = "none")
 ### Lusaka (x_bar = 75.83) significantly outperformed all other provinces

#2. Are coie equal across grades and region?
hist(d_model$pri_coie_t1, xlim = c(-5, 5))
hist(d_model$sec_coie_t1, xlim = c(-5, 5))
var(d_model$pri_coie_t1, na.rm = TRUE)
var(d_model$sec_coie_t1, na.rm = TRUE) ### Variances are very unequal
 ##2.1 Mann-Whitney t-test
mean(d_model$pri_coie_t1, na.rm = TRUE)
mean(d_model$sec_coie_t1, na.rm = TRUE)
wilcox.test(d_model$pri_coie_t1, d_model$sec_coie_t1, conf.int = TRUE)
 ### Significant differences across grades, pri_coie_t1 = 0.173
 ###                                        sec_coie_t1 = 0.136
 ### p < .01, suggesting significant differences
 
 ##2.3 ANOVA of coie by region
lm_pri_coie_t1_region = lm(pri_coie_t1 ~ region, data = d_model)
summary(lm_pri_coie_t1_region) ### R^2 = 0.1442

d_model %>% group_by(region) %>%
  summarise(count = n(),
            mean = mean(pri_coie_t1, na.rm = TRUE),
            sd = sd(pri_coie_t1, na.rm = TRUE),
            median = median(pri_coie_t1, na.rm = TRUE),
            IQR = IQR(pri_coie_t1, na.rm = TRUE))

qqPlot(lm_pri_coie_t1_region)
leveneTest(lm_pri_coie_t1_region)
boxplot(d_model$pri_coie_t1 ~ d_model$region) 
 ### The ANOVA assumptions are not tenable

 ##2.3.1 Non-parametric anova
kruskal.test(pri_coie_t1 ~ region, data = d_model)
 ## Kruskal-Wallis chi-squared = 1133.6, df = 9, p-value < 2.2e-16, suggesting significant differences 
 ## across regions
 
 ##2.3.2 pairwise comparisons
pairwise.wilcox.test(d_model$pri_coie_t1, d_model$region, p.adjust.method = 'BH')
  ### All pairwise comparisons are significant except for Northern-Muchinga, Southern-Copperbelt,
  ### and Western-Luapula

#3. Multiple regression to predict test results
lm_exam = lm(mean_scoret ~ distance + class_size2017 + class_size2018 + ptr2018 + ptr2017 + g_t_r2018 + g_t_r2017 + Schp_txtbk_r2017,
             data = d_model,
             na.action = na.exclude)
summary(lm_exam)
## only distance significantly predict mean_scoret, and the R^2 is only .032

################### Models Using "t_srt", "t_coie_t1", "t_gpi_coie1", "t_gpi_sr1" ##################

#1. Generating "t_srt", "t_coie_t1", "t_gpi_coie1", "t_gpi_sr1" for each school

##1.1 Use 'srt1_7' to represent sr for primary schools,
##        'srt8_12' for secondary schools
##        'srt1_12' to represent sr for pri-secondary schools, 
##        and create variable 't_srt' to store t_sr for all school types

d_model %>%
  dplyr::select(srt1_7, srt8_12, srt1_12, elevel) %>%
  filter(elevel == 1) 

d_model %>%
  dplyr::select(srt1_7, srt8_12, srt1_12, elevel) %>%
  filter(elevel == 2 | elevel == 3) 

d_model %>%
  dplyr::select(srt1_7, srt8_12, srt1_12, elevel) %>%
  filter(elevel == 4 & is.na(srt1_12)) 

d_model = d_model %>%
  mutate(t_srt = case_when(
    elevel == 1 ~ srt1_7,
    elevel == 2|elevel == 3 ~ srt8_12,
    elevel == 4 ~ srt1_12
  ))

## Check accuracy of transformation
d_model %>%
  mutate(sr_check_pri = t_srt - srt1_7) %>%
  filter(elevel == 1)

d_model %>%
  mutate(sr_check_sec = t_srt - srt8_12) %>%
  filter((elevel == 2|elevel ==3)&sr_check_sec !=0)

d_model %>%
  mutate(sr_check_pri_sec = t_srt - srt1_12) %>%
  filter(elevel == 4&sr_check_pri_sec !=0)

##1.2 Use 'pri_coie_t1' to represent coie for primary schools,
##        'sec_coie_t1' for secondary schools
##        'pri_coie_t1' to represent coie for pri-secondary schools, 
##         and create variable 't_coie_t1' to store coies for all types of schools
d_model %>%
  dplyr::select(elevel, pri_coie_t1, sec_coie_t1) %>%
  filter(elevel == 4 & is.na(pri_coie_t1)) ### 2 nas

d_model %>%
  dplyr::select(elevel, pri_coie_t1, sec_coie_t1) %>%
  filter(elevel == 4 & is.na(sec_coie_t1)) ### 2 nas

d_model = d_model %>%
  mutate(t_coie_t1 = case_when(
    elevel == 1 ~ pri_coie_t1,
    elevel == 2 | elevel == 3 ~ sec_coie_t1,
    elevel == 4 ~ pri_coie_t1
  ))

##1.3 Use 'gpi_pri_coie_1' to represent gpi_coie for primary schools,
##        'gpi_sec_coie_1' for secondary schools
##        'gpi_pri_coie_1' to represent coie for pri-secondary schools, 
##        and create 't_gpi_coie1' to store GPI for coies for all schools
d_model %>%
  dplyr::select(elevel, gpi_pri_coie_1, gpi_sec_coie_1) %>%
  filter(elevel == 4 & is.na(gpi_pri_coie_1)) ### 81 nas
d_model %>%
  dplyr::select(elevel, gpi_pri_coie_1, gpi_sec_coie_1) %>%
  filter(elevel == 4 & is.na(gpi_sec_coie_1)) ### 81 nas

d_model = d_model %>%
  mutate(t_gpi_coie1 = case_when(
    elevel == 1 ~ gpi_pri_coie_1,
    elevel == 2 | elevel == 3 ~ gpi_sec_coie_1,
    elevel == 4 ~ gpi_pri_coie_1
  ))

##1.3 Use 'gpi_sr1_7' to represent gpi_sr for primary schools,
##        'gpi_sr8_12' for secondary schools
##        'gpi_sr1_12' to represent gpi_sr for pri-secondary schools (gpi_sr1_7 has severe nas),
##         and create variable 't_gpi_sr1' to store GPI for Survival Rate for all types of schools

d_model %>%
  dplyr::select(elevel, gpi_sr1_7, gpi_sr8_12, gpi_sr1_12) %>%
  filter(elevel == 1 & is.na(gpi_sr1_7)) ### two pri schools have na on gpi_sr1_7 and os on the other two measures of gpi_sr

d_model %>%
  dplyr::select(elevel, gpi_sr1_7, gpi_sr8_12, gpi_sr1_12) %>%
  filter((elevel == 2 | elevel == 3) & is.na(gpi_sr8_12)) ##  na

d_model %>%
  dplyr::select(elevel, gpi_sr1_7, gpi_sr8_12, gpi_sr1_12) %>%
  filter(elevel == 4 & is.na(gpi_sr1_12)) ### 23 nas

d_model = d_model %>%
  mutate(t_gpi_sr1 = case_when(
    elevel == 1 ~ gpi_sr1_7,
    elevel == 2 | elevel == 3 ~ gpi_sr8_12,
    elevel == 4 ~ gpi_sr1_12
  ))

#3. Multivariate Regression to predict "t_srt", "t_coie_t1", "t_gpi_coie1", "t_gpi_sr1"

mreg1 = lm(cbind(t_srt, t_coie_t1, t_gpi_coie1, t_gpi_sr1) ~ distance + class_size2017 + class_size2018 + ptr2018 + ptr2017 + g_t_r2018 + g_t_r2017 + Schp_txtbk_r2017,
           data = d_model)

summary(mreg1)
coef(mreg1)

## standardized coefficients
mreg1_std = lm(cbind(scale(t_srt), scale(t_coie_t1), scale(t_gpi_coie1), scale(t_gpi_sr1)) ~ scale(distance) + scale(class_size2017) + scale(class_size2018) + scale(ptr2018) + scale(ptr2017) + scale(g_t_r2018) + scale(g_t_r2017) + scale(Schp_txtbk_r2017),
           data = d_model)
summary(mreg1_std)
coef(mreg1_std)

## It seems ptr2018/ptr2017 are consistent and significant predictors for these outcome variables exceplt for t_gpi_sr1 













