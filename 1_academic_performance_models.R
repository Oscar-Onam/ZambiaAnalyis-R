#1. required packages and load libraries
requiredPackages = c('haven', 'tidyverse', 'skimr', 'sjPlot', 'lme4',
                     'writexl', 'openxlsx', 'reshape2', 'RColorBrewer', 'pscl', 'broom', 
                     'webshot', 'jtools', 'performance')
for (p in requiredPackages) {
  if (!require(p, character.only = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}


#2. Create a dynamic path to data for multiple users and load data
Paths = c("/Users/ysong/OneDrive - UNICEF/Zambia/4_output", 
          "C:/Users/oonam/OneDrive - UNICEF/DMS/Zambia/4_output")
names(Paths) = c("ysong", "oonam")
setwd(Paths[Sys.info()[7]])

# Load data
d = as.data.frame(read_dta('coie1.dta'))


#3. Create relevant independent variable lists and transform data
ind_l = c('z_distance', 'region', 'location',
          'log_G7p_txtbk_r2017', 'z_G7p_txtbk_r2017',
          'z_class_size2017', 'class_size2017',
          'log_g_t_r2017',
          'electricity', 'water',
          'f_index', 'f_quint', 'vpoor_furn',
          'e_index', 'e_quint', 'vpoor_equip',
          'waste_disposal', 'hand_washpractice', 'soap_handwashing',
          'mensthygiene', 'hygiene_edu',
          'sani_towels', 'sani_disposal')

skim(d[, ind_l])

## Transform variables to prepare for building models
map(d[, c('electricity', 'water', 'waste_disposal', 'hand_washpractice', 'soap_handwashing',
          'mensthygiene', 'sani_towels', 'hygiene_edu', 'sani_disposal')], ~unique(.))

### (There is no NA value in variables water and electricity, so use ifelse() to transfer them to binary 
### variables is legitimate below)
d2 = d %>%
  mutate(water_in_sch = ifelse(water == 1 | water == 2 | water == 3 | water == 4 | water == 5, 1, 0),
         elec_in_sch = ifelse(electricity == 1 | electricity == 2 | electricity == 3, 1, 0)) %>%
  mutate(water_in_sch = factor(water_in_sch, levels = c('0', '1')),
         elec_in_sch = factor(elec_in_sch, levels = c('0', '1')),
         waste_disposal = factor(waste_disposal, levels = c('0', '1')),
         hand_washpractice = factor(hand_washpractice, levels = c('0', '1')),
         soap_handwashing = factor(soap_handwashing, levels = c('0', '1')),
         mensthygiene = factor(mensthygiene, levels = c('0', '1')),
         sani_towels = factor(sani_towels, levels = c('0', '1')),
         hygiene_edu = factor(hygiene_edu, levels = c('0', '1')),
         sani_disposal = factor(sani_disposal, levels = c('0', '1'))) 


d2 = d2 %>%
  mutate(across(where(~class(.x)[1] == "haven_labelled"), haven::as_factor)) %>%
  mutate(location = factor(location, levels = c("Rural", "Urban")),
         region = factor(region)) 

d2 <- within(d2, region <- relevel(region, ref = 7))

d2%>%
  select(all_of(ind_l) & where(~class(.x) == 'factor')) %>%
  map(~table(.x))

skim(d2[, ind_l])


#4. Outcome as continuous ############################################################################
##4.1 Generalized Linear Modelling (multiple linear regression) #####################################
###Independent var list for models without interactions terms (i.e., include categorical variables 
### but exclude continuous variables for which categorical equivalents are available)
ind_l2 = c('z_distance', 'region', 'location',
           'log_G7p_txtbk_r2017','z_class_size2017', 'log_g_t_r2017',
           'elec_in_sch', 'water_in_sch',
           'vpoor_furn',
           'vpoor_equip',
           'waste_disposal', 'hand_washpractice', 'soap_handwashing',
           'mensthygiene', 'sani_towels', 'hygiene_edu', 'sani_disposal')

### Independent var list for models with interaction terms (i.e., the opposite rule applied as compared
### to the above list)
ind_l3 = c('z_distance', 'region', 'location',
           'log_G7p_txtbk_r2017', 'z_G7p_txtbk_r2017',
           'z_class_size2017', 'log_g_t_r2017',
           'elec_in_sch', 'water_in_sch',
           'f_index', 'f_quint', 'vpoor_furn',
           'e_index', 'e_quint', 'vpoor_equip',
           'waste_disposal', 'hand_washpractice', 'soap_handwashing',
           'mensthygiene', 'sani_towels', 'hygiene_edu', 'sani_disposal')

###4.1.1 mean_scoret as outcome var
glmt_varL = d2 %>%
  filter(elevel == 'Primary' | elevel == 'Primary & Secondary') %>%
  filter(!is.na(mean_scoret)) %>%
  select(all_of(ind_l2), mean_scoret)

glmt_varL_IN = d2 %>%
  filter(elevel == 'Primary' | elevel == 'Primary & Secondary') %>%
  filter(!is.na(mean_scoret)) %>%
  select(all_of(ind_l3), mean_scoret)


####No interaction
##### ?family
glmt_nint = glm(mean_scoret ~ ., data = glmt_varL, family = gaussian(link = 'identity'))

summ(glmt_nint)
model_performance(glmt_nint)
tab_model(glmt_nint)
glmt_nint_r2 = pR2(glmt_nint)

glmt_R = tidy(glmt_nint) %>%
  mutate(p.value = round(p.value, 4)) %>%
  mutate(R_squared = round(glmt_nint_r2['r2CU'], 4))

glmt_R_sorted = glmt_R %>%
  filter(p.value <= 0.05) %>%
  arrange(p.value)

wb_glmt = createWorkbook()
sname1 = 'glm_raw'
sname2 = 'glm_orted'

addWorksheet(wb_glmt, sname1)
writeData(wb_glmt, sheet = sname1, x = glmt_R)

addWorksheet(wb_glmt, sname2)
writeData(wb_glmt, sheet = sname2, x = glmt_R_sorted)


####With interactions
glmt_int = glm(mean_scoret ~ z_distance + region + location + log_G7p_txtbk_r2017 +
                z_class_size2017 + log_g_t_r2017 + elec_in_sch + water_in_sch +
                vpoor_furn + vpoor_equip + waste_disposal + hand_washpractice + soap_handwashing +
                mensthygiene + sani_towels +
                location*z_G7p_txtbk_r2017 + location*z_class_size2017 + 
                location*elec_in_sch + location*waste_disposal + location*mensthygiene +
                location*sani_disposal +
                elec_in_sch*f_index + elec_in_sch*z_class_size2017 +
                water_in_sch*hygiene_edu + water_in_sch*mensthygiene +
                water_in_sch*soap_handwashing + water_in_sch*sani_disposal +
                waste_disposal*log_g_t_r2017, data = glmt_varL_IN, 
              family = gaussian(link = 'identity'))


summary(glmt_int)
tab_model(glmt_int)
glmt_int_r2 = pR2(glmt_int)

glmt_int_R = tidy(glmt_int) %>%
  mutate(p.value = round(p.value, 4)) %>%
  mutate(R_squared = round(glmt_int_r2['r2CU'], 4))

glmt_int_R_sorted = glmt_int_R %>%
  filter(p.value <= 0.05) %>%
  arrange(p.value)

sname3 = 'glmt_INT_raw'
sname4 = 'glmt_INT_sorted'

addWorksheet(wb_glmt, sname3)
writeData(wb_glmt, sheet = sname3, x = glmt_int_R)

addWorksheet(wb_glmt, sname4)
writeData(wb_glmt, sheet = sname4, x = glmt_int_R_sorted)

file_path = "~/OneDrive - UNICEF/Zambia/4_output/Model_evaluation_yu/1_glm_mean_scoret.xlsx"
saveWorkbook(wb_glmt, file = file_path, overwrite = TRUE)


####Model evaluation
aov_glmt = anova(glmt_nint, glmt_int, test = 'Chisq')
tab_model(glmt_nint, glmt_int) ###Not significant difference
glmt_model_eval = tidy(aov_glmt)

sname5 = 'glmt_model_eval'
addWorksheet(wb_glmt, sname5)
writeData(wb_glmt, sheet = sname5, x = glmt_model_eval)

file_path = "~/OneDrive - UNICEF/Zambia/4_output/Model_evaluation_yu/1_glm_mean_scoret.xlsx"
saveWorkbook(wb_glmt, file = file_path, overwrite = TRUE)

##4.2 Generalized Linear Mixed Model (multiple linear regression) ########################
###GLMMs with random intercept only

### Models without interaction
glmmt_nint = glmer(mean_scoret ~ (. - region) + (1 | region), data = glmt_varL, 
                   family = gaussian(link = 'identity'))
summary(glmmt_nint)
tab_model(glmmt_nint)

### Models with interaction
glmmt_int = glmer(mean_scoret ~ z_distance + location + log_G7p_txtbk_r2017 +
                    z_class_size2017 + log_g_t_r2017 + elec_in_sch + water_in_sch +
                    vpoor_furn + vpoor_equip + waste_disposal + hand_washpractice + soap_handwashing +
                    mensthygiene + sani_towels +
                    location*z_G7p_txtbk_r2017 + location*z_class_size2017 + 
                    location*elec_in_sch + location*waste_disposal + location*mensthygiene +
                    location*sani_disposal +
                    elec_in_sch*f_index + elec_in_sch*z_class_size2017 +
                    water_in_sch*hygiene_edu + water_in_sch*mensthygiene +
                    water_in_sch*soap_handwashing + water_in_sch*sani_disposal +
                    waste_disposal*log_g_t_r2017 + 
                    (1 | region), data = glmt_varL_IN, 
                  family = gaussian(link = 'identity'))

summary(glmmt_int)
tab_model(glmmt_int)


### Model comparison
glmmt_base = glmer(mean_scoret ~ (1 | region), data = glmt_varL, 
                  family = gaussian(link = 'identity'))

tab_model(glmmt_base, glmmt_nint, glmmt_int, file = '2_glmm_mean_scoret.html')
### webshot('2_glmm_mean_scoret.html', '2_glmm_mean_scoret.png')


##4.3 Multivariate regression (Linear Model) ####################################################
mlr_varL = d2 %>%
  filter(elevel == 'Primary' | elevel == 'Primary & Secondary') %>%
  filter(!is.na(mean_scoret)) %>%
  select(all_of(ind_l2), mean_scorem, mean_scoref)

mvlr_varL_IN = d2 %>%
  filter(elevel == 'Primary' | elevel == 'Primary & Secondary') %>%
  filter(!is.na(mean_scoret)) %>%
  select(all_of(ind_l3), mean_scorem, mean_scoref)


### Without interaction
mlr_nint = lm(cbind(mean_scorem, mean_scoref) ~ ., data = mlr_varL)
mlr_nint_r = summary(mlr_nint)
tidy(mlr_nint)

mlrm_nint_r2 = mlr_nint_r$`Response mean_scorem`$r.squared
mlrf_nint_r2 = mlr_nint_r$`Response mean_scoref`$r.squared

mlr_nint_R = tidy(mlr_nint) %>%
  mutate(p.value = round(p.value, 4)) %>%
  mutate(R_squared = ifelse(response == 'mean_scorem', mlrm_nint_r2, mlrf_nint_r2))

mlr_nint_R_sorted = mlr_nint_R %>%
  filter(p.value <= 0.05) %>%
  arrange(response, p.value)

wb_mlr = createWorkbook()
sname_mlr1 = 'mlr_without_interaction_raw'
sname_mlr2 = 'mlr_without_interaction_sorted'

addWorksheet(wb_mlr, sname_mlr1)
writeData(wb_mlr, sheet = sname_mlr1, x = mlr_nint_R)

addWorksheet(wb_mlr, sname_mlr2)
writeData(wb_mlr, sheet = sname_mlr2, x = mlr_nint_R_sorted)


### With interaction
mlr_int = lm(cbind(mean_scorem, mean_scoref) ~ z_distance + region + location + log_G7p_txtbk_r2017 +
                z_class_size2017 + log_g_t_r2017 + elec_in_sch + water_in_sch +
                vpoor_furn + vpoor_equip + waste_disposal + hand_washpractice + soap_handwashing +
                mensthygiene + sani_towels +
                location*z_G7p_txtbk_r2017 + location*z_class_size2017 + 
                location*elec_in_sch + location*waste_disposal + location*mensthygiene +
                location*sani_disposal +
                elec_in_sch*f_index + elec_in_sch*z_class_size2017 +
                water_in_sch*hygiene_edu + water_in_sch*mensthygiene +
                water_in_sch*soap_handwashing + water_in_sch*sani_disposal +
                waste_disposal*log_g_t_r2017
              , data = mvlr_varL_IN)


mlr_int_r = summary(mlr_int)
tidy(mlr_int)

mlrm_int_r2 = mlr_int_r$`Response mean_scorem`$r.squared
mlrf_int_r2 = mlr_int_r$`Response mean_scoref`$r.squared

mlr_int_R = tidy(mlr_int) %>%
  mutate(p.value = round(p.value, 4)) %>%
  mutate(R_squared = ifelse(response == 'mean_scorem', mlrm_int_r2, mlrf_int_r2))

mlr_int_R_sorted = mlr_int_R %>%
  filter(p.value <= 0.05) %>%
  arrange(response, p.value)

sname_mlr3 = 'mlr_with_interaction_raw'
sname_mlr4 = 'mlr_with_interaction_sorted'

addWorksheet(wb_mlr, sname_mlr3)
writeData(wb_mlr, sheet = sname_mlr3, x = mlr_int_R)

addWorksheet(wb_mlr, sname_mlr4)
writeData(wb_mlr, sheet = sname_mlr4, x = mlr_int_R_sorted)

file_path = "~/OneDrive - UNICEF/Zambia/4_output/Model_evaluation_yu/3_mlr_mean_scorem_mean_scoref.xlsx"
saveWorkbook(wb_mlr, file = file_path, overwrite = TRUE)

##4.3 Generalized Multivariate regression (GLMM for multivariate) ####################################################
###GLMMs with random intercept only

### Models without interaction (ERROR BELOW)
glmm_mv_nint = glmer(cbind(mean_scorem, mean_scoref) ~ . +  (1 | region), data = mlr_varL, 
                   family = gaussian(link = 'identity'))

summary(glmm_mv_nint)
tab_model(glmm_mv_nint)

### Models with interaction
glmm_mv_int = glmer(cbind(mean_scorem, mean_scoref) ~ z_distance + location + log_G7p_txtbk_r2017 +
                    z_class_size2017 + log_g_t_r2017 + elec_in_sch + water_in_sch +
                    vpoor_furn + vpoor_equip + waste_disposal + hand_washpractice + soap_handwashing +
                    mensthygiene + sani_towels +
                    location*z_G7p_txtbk_r2017 + location*z_class_size2017 + 
                    location*elec_in_sch + location*waste_disposal + location*mensthygiene +
                    location*sani_disposal +
                    elec_in_sch*f_index + elec_in_sch*z_class_size2017 +
                    water_in_sch*hygiene_edu + water_in_sch*mensthygiene +
                    water_in_sch*soap_handwashing + water_in_sch*sani_disposal +
                    waste_disposal*log_g_t_r2017 + 
                    (1 | region), data = mvlr_varL_IN, 
                  family = gaussian(link = 'identity'))

summary(glmm_mv_int)
tab_model(glmm_mv_int)


### Model comparison
glmmt_base = glmer(mean_scoret ~ (1 | region), data = glmt_varL, 
                   family = gaussian(link = 'identity'))

tab_model(glmmt_base, glmmt_nint, glmmt_int, file = '2_glmm_mean_scoret.html')
### webshot('2_glmm_mean_scoret.html', '2_glmm_mean_scoret.png')


#5. Outcome as binary ################################################################################
bn_varL = d2 %>%
  mutate(top_quint = ifelse(exam_quintilest == 'Top', '1', '0')) %>%
  mutate(top_quint = factor(top_quint, levels = c('0', '1'))) %>%
  filter(elevel == 'Primary' | elevel == 'Primary & Secondary') %>%
  select(all_of(ind_l2), top_quint)
  

bn_varL_IN = d2 %>%
  mutate(top_quint = ifelse(exam_quintilest == 'Top', '1', '0')) %>%
  mutate(top_quint = factor(top_quint, levels = c('0', '1'))) %>%
  filter(elevel == 'Primary' | elevel == 'Primary & Secondary') %>%
  select(all_of(ind_l3), top_quint)

##5.1Classic Logistic regression
###Without interaction
log_nint = glm(top_quint ~ ., data = bn_varL, family = binomial(link = 'logit'))

summary(log_nint)
tab_model(log_nint)


###With interaction
log_int = glm(top_quint ~ z_distance + region + location + log_G7p_txtbk_r2017 +
                z_class_size2017 + log_g_t_r2017 + elec_in_sch + water_in_sch +
                vpoor_furn + vpoor_equip + waste_disposal + hand_washpractice + soap_handwashing +
                mensthygiene + sani_towels +
                location*z_G7p_txtbk_r2017 + location*z_class_size2017 + 
                location*elec_in_sch + location*waste_disposal + location*mensthygiene +
                location*sani_disposal +
                elec_in_sch*f_index + elec_in_sch*z_class_size2017 +
                water_in_sch*hygiene_edu + water_in_sch*mensthygiene +
                water_in_sch*soap_handwashing + water_in_sch*sani_disposal +
                waste_disposal*log_g_t_r2017, data = bn_varL_IN, 
                  family = binomial(link = 'logit'))

summary(log_int)
tab_model(log_int)


### Model comparison
tab_model(log_nint, log_int, file = '5_logistic_top_quint.html', show.ci = FALSE)
### webshot(5_logistic_top_quint.html', 5_logistic_top_quint.html')


##5.2Logistic regression AS generalized mixed model
###with random intercept only

### Models without interaction
log_glmm_nint = glmer(top_quint ~ (. - region) + (1 | region), data = bn_varL, 
                   family = binomial(link = 'logit'))
summary(log_glmm_nint)
tab_model(log_glmm_nint)

### Models with interaction
log_glmm_int = glmer(top_quint ~ z_distance + location + log_G7p_txtbk_r2017 +
                    z_class_size2017 + log_g_t_r2017 + elec_in_sch + water_in_sch +
                    vpoor_furn + vpoor_equip + waste_disposal + hand_washpractice + soap_handwashing +
                    mensthygiene + sani_towels +
                    location*z_G7p_txtbk_r2017 + location*z_class_size2017 + 
                    location*elec_in_sch + location*waste_disposal + location*mensthygiene +
                    location*sani_disposal +
                    elec_in_sch*f_index + elec_in_sch*z_class_size2017 +
                    water_in_sch*hygiene_edu + water_in_sch*mensthygiene +
                    water_in_sch*soap_handwashing + water_in_sch*sani_disposal +
                    waste_disposal*log_g_t_r2017 + 
                    (1 | region), data = bn_varL_IN, 
                  family = binomial(link = 'logit'))

summary(log_glmm_int)
tab_model(log_glmm_int)


### Model comparison
log_glmm_base = glmer(top_quint ~ (1 | region), data = bn_varL, 
                   family = binomial(link = 'logit'))

tab_model(log_glmm_base, log_glmm_nint, log_glmm_int, file = '6_logistic_glmm_top_quint.html', 
          show.ci = FALSE)
### webshot('6_logistic_glmm_top_quint.html', '6_logistic_glmm_top_quint.html')


##5.3Multivariate Logistic regression 
bn_mv_varL = d2 %>%
  mutate(top_quintm = ifelse(exam_quintilesm == 'Top', '1', '0'),
         top_quintf = ifelse(exam_quintilesf == 'Top', '1', '0')) %>%
  mutate(top_quintm = factor(top_quintm, levels = c('0', '1')),
         top_quintf = factor(top_quintf, levels = c('0', '1'))) %>%
  filter(elevel == 'Primary' | elevel == 'Primary & Secondary') %>%
  select(all_of(ind_l2), top_quintm, top_quintf)


bn_mv_varL_IN = d2 %>%
  mutate(top_quintm = ifelse(exam_quintilesm == 'Top', '1', '0'),
         top_quintf = ifelse(exam_quintilesf == 'Top', '1', '0')) %>%
  mutate(top_quintm = factor(top_quintm, levels = c('0', '1')),
         top_quintf = factor(top_quintf, levels = c('0', '1'))) %>%
  filter(elevel == 'Primary' | elevel == 'Primary & Secondary') %>%
  select(all_of(ind_l3), top_quintm, top_quintf)


###Without interaction
log_mv_nint = glm(c(top_quintm, top_quintf) ~ ., data = bn_mv_varL, family = binomial(link = 'logit'),
                  na.action = na.exclude)

summary(log_nint)
tab_model(log_nint)













