# Load libraries
library(tidyverse)
library(haven)
library(ggplot2)
library(psych)
library(car)
library(MASS)

# Read data and select variables 
##USE NEW DATASET sample1.dta AND ADD MORE VARIABLES
d = read_dta("/Users/mac/OneDrive - UNICEF/Zambia/3_dofiles/28_Models_Yu/Samplecoie1+weight copy.dta")
d = as.data.frame(d)

var_list = c('mean_scoret', 'exam_quintilest',
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

############################ Dependent Variables and apply weights ####################################

##1. Use 'srt1_7' to represent sr for pri-secondary schools, and create variable 't_srt' 
## to store t_sr for all school types

d_model = d_model %>%
  mutate(t_srt = case_when(
    elevel == 1 ~ srt1_7,
    elevel == 2|elevel == 3 ~ srt8_12,
    elevel == 4 ~ srt1_7
  ))

##2. Use 'pri_coie_t1' to represent pri-secondary schools' coie, and create variable 't_coie_t1'
## to store coies for all types of schools
d_model = d_model %>%
  mutate(t_coie_t1 = case_when(
    elevel == 1 ~ pri_coie_t1,
    elevel == 2 | elevel == 3 ~ sec_coie_t1,
    elevel == 4 ~ pri_coie_t1
  ))

##3. Use 'gpi_pri_coie_1' to represent pri-secondary schools' GPI for CoIE, and create 't_gpi_coie1'
## to store GPI for coies for all schools

d_model = d_model %>%
  mutate(t_gpi_coie1 = case_when(
    elevel == 1 ~ gpi_pri_coie_1,
    elevel == 2 | elevel == 3 ~ gpi_sec_coie_1,
    elevel == 4 ~ gpi_pri_coie_1
  ))

##4. Use 'gpi_sr1_7' to represent pri-secondary schools' GPI for Survival Rate, 
## and create variable 't_gpi_sr1' to store GPI for Survival Rate for all types of schools
d_model = d_model %>%
  mutate(t_gpi_sr1 = case_when(
    elevel == 1 ~ gpi_sr1_7,
    elevel == 2 | elevel == 3 ~ gpi_sr8_12,
    elevel == 4 ~ gpi_sr1_7
  ))

d_model %>%
  select(t_gpi_sr1, elevel, schoolname) %>%
  filter(is.na(t_gpi_sr1) | t_gpi_sr1 == 0) %>%
  group_by(elevel) %>%
  tally()

d_model %>%
  select(t_gpi_sr1, elevel, gpi_sr1_7, gpi_sr8_12, gpi_sr1_12, schoolname) %>%
  filter(is.na(t_gpi_sr1) | t_gpi_sr1 == 0) 
### 1547 NAs, two are primary school, and 1545 are primary-secondary schools
### Maybe use gpi_sr1_12 to represent pri&secondary school's t_gpi_sr1 to avoid pervasive nas???

d_model = d_model %>%
  mutate(t_gpi_sr1 = case_when(
    elevel == 1 ~ gpi_sr1_7,
    elevel == 2 | elevel == 3 ~ gpi_sr8_12,
    elevel == 4 ~ gpi_sr1_12
  ))

d_model %>%
  select(t_gpi_sr1, elevel, gpi_sr1_7, gpi_sr8_12, gpi_sr1_12, schoolname) %>%
  filter(is.na(t_gpi_sr1) | t_gpi_sr1 == 0) %>%
  count(elevel)
### After using gpi_sr1_12 to represent pri&secondary school's t_gpi_sr1, ther are 83 nas, two for pri,
### 82 for pri&secondary


##5. Apply weights
d_model %>%
  filter(weights == 0 | is.na(weights)) %>%
  tally() ### No na or 0 value for weights

weight = function(x, weights = d_model$weights) {
  x = x*weights
}

head(d_model[, c("t_srt", "t_coie_t1", "t_gpi_coie1", "t_gpi_sr1", "mean_scoret")])

d_model = d_model%>%
  mutate(across(c("t_srt", "t_coie_t1", "t_gpi_coie1", "t_gpi_sr1", "mean_scoret"), weight))

head(d_model[, c("t_srt", "t_coie_t1", "t_gpi_coie1", "t_gpi_sr1", "mean_scoret")])


##6. some exploratory analysis of DVs
names(d_model)
dv_list = c("t_srt", "t_coie_t1", "t_gpi_coie1", "t_gpi_sr1", "mean_scoret")

###Distributions of DVs
par(mfrow = c(3, 2))

for (var in dv_list) {
  hist(d_model[, var], 
       xlab = var,
       main = paste('Histogram of', var, sep = ' '),
       freq = FALSE
       )
}

par(mfrow = c(1, 1))
###6.1 correlations
(cor_dv =cor(d_model[, dv_list], use = 'pairwise.complete.obs'))
pairs(d_model[, dv_list])
cortest.bartlett(cor_dv)
####It seems there are some correlations between variables

###6.2 factor analysis
eigen(cor_dv)$values # seems 2 (or 3) factors are appropriate

#### principal component method
(fa1_dv = principal(cor_dv, nfactors = 2, rotate = 'varimax'))
##### two factors explained 79% of total variance, one is related to t_srt, t_coie_t1 , mean_scoret, 
##### the other one is associated with the two gender equity indexes
(fa2_dv = principal(cor_dv, nfactors = 2, rotate = 'oblimin')) # correlation between two factors are .3


############################### Independent Variables ###########################################
## Complete Cases (cc)
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
  select(gpi_retr_sec, elevel) %>%
  filter(elevel == 2 | elevel == 3) 
### No secondary schools (elevel == 2|3) has gpi_retr_sec
d_model %>%
  select(gpi_retr_pri, elevel) %>%
  filter(elevel == 4 & is.na(gpi_retr_pri)) 
### Only one pri-secondary (elevel == 4) has na on gpi_retr_pri

### Thus, with regard to gpi_retr, maybe has to delete gpi_retr_sec in our model

## Correlations
(cor_iv = cor(d_model[, iv_list[-11]], use = 'pairwise.complete.obs'))
pairs(d_model[, iv_list[-11]])
cortest.bartlett(cor_iv) # Coorelations are significant, factor analysis are appropriate

## factor analysis of IVs
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

######################################## Models ###################################################
#1. Multiple regression to predict test results
d_model %>%
  filter(is.na(mean_scoret)) %>%
  count(elevel) ## 732 nas

d_model %>%
  filter(is.na(exam_quintilest)) %>%
  count(elevel) ## 732 nas
names(d_model)

lm_exam = lm(mean_scoret ~ distance + class_size2017 + class_size2018 + ptr2018 + ptr2017 + g_t_r2018 + g_t_r2017 + Schp_txtbk_r2017,
             data = d_model,
             na.action = na.exclude)
summary(lm_exam)
## only distance significantly predict mean_scoret, and the R^2 is only .032

#2. Discriminant analysis to predict exam_quintilest
## homogeneity of covariance matrices
library(heplots)
boxM(d_model[, iv_list], d_model[, 'exam_quintilest']) # Unequal covariances matrix
## Use quadratic discriminant analysis
(qda_exam_q = qda(exam_quintilest ~ distance + class_size2017 + class_size2018 + ptr2018 + ptr2017 + g_t_r2018 + g_t_r2017 + Schp_txtbk_r2017,
                  data = d_model, na.action = na.omit))
predict(qda_exam_q)

#3. Multivariate Regression to predict "t_srt", "t_coie_t1", "t_gpi_coie1", "t_gpi_sr1" 
iv_list
mreg1 = lm(cbind(t_srt, t_coie_t1, t_gpi_coie1, t_gpi_sr1) ~ distance + class_size2017 + class_size2018 + ptr2018 + ptr2017 + g_t_r2018 + g_t_r2017 + Schp_txtbk_r2017,
           data = d_model)

summary(mreg1)
coef(mreg1)


















