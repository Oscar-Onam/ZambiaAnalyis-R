# read coie1.dta
library(tools)
library(tidyverse)
library(haven)
library(ggplot2)

file = file_path_as_absolute(file.choose())

coie1 <- read_dta("/Users/mac/Desktop/UNICEF/Zambia project/4_output/coie1.dta")
coie1 <- as.data.frame(coie1)
class(coie1)
is.factor(coie1$elevel)
sum(is.na(coie1$elevel))
coie1$elevel <- factor(coie1$elevel)

#1. how many schools are in this dataset? 
#L1 - 2684
#L2-113
#L3-2
#L4-1072
#Total = 3871
coie1 %>% count(elevel) 

2684 + 113 + 2 + 1072

#2. How many schools have TRUE pri_coie_t1?
#L1 - 2384
#L2 - 0
#L3 - 0
#L4 - 1067

#2.1 How many schools have pri_coie_t1? 
#L1 - 2684
#L2 - 113 ??
#L3 - 2 ??
#L4 - 1072

coie1 %>% 
  group_by(elevel) %>%
  select('pri_coie_t1') %>%
  tally()
 
  ## Why L2 & 3 schools also have pri_coie_t1?
  ## The default value for pri_coie_t1 is 0, and then it was replaced by function to calculate the
  ## TRUE pri_coie_t1 is if elevel == 1|4, but when elevel == 2|3, it remains 0.
  ## Thus, might need to find out how many 0s, and how many NAs are in pri_coie_t1, 
  ## and then recount observations with TRUE coies

#2.2 How many schools have TRUE pri_coie_t1 != 0?
#L1 - 2384
#L2 - 0
#L3 - 0
#L4 - 1067
coie1 %>%
  group_by(elevel) %>%
  select('pri_coie_t1') %>%
  filter(pri_coie_t1 != 0) %>%
  tally()

    ## Are the difference between L1- 2684 - 2384, L4- 1072 - 1067 because of missing?

#2.3 How many NAs ?
#L1 - 300
#L2 - 5
   ##Yes, the difference between L1- 2684 - 2384, L4- 1072 - 1067 because of NAs
coie1 %>%
  group_by(elevel) %>%
  select(pri_coie_t1) %>%
  filter(is.na(pri_coie_t1)) %>%
  tally()

#2.3.1 Take a closer look at NAs
  ## It seems that the NAs are because of grads_gt7 = 0 ('Number of grade 7 male and female student')
  ## which is caused by $prom_t8list = 0
coie1 %>%
  group_by(elevel) %>%
  select(pri_coie_t1, pupil_yrs_grad7t, grads_gt7, promotees_gt7_2023, promotees_gt7_2024, promotees_gt7_2025, 
           promotees_gt7_2026, promotees_gt7_2027,promotees_gt7_2028,promotees_gt7_2029,
           promotees_gt7_2030,promotees_gt7_2031,promotees_gt7_2032,promotees_gt7_2033) %>%
  filter(is.na(pri_coie_t1))

#2.3.2 distribution of NAs across elevel and region
coie1 %>%
  group_by(elevel, region) %>%
  filter(is.na(pri_coie_t1)) %>%
  tally()

#3. How many schools have TRUE sec_coie_t1?
#L1 - 0
#L2 - 63
#L3 - 0
#L4 - 1032

#3.1 How many schools have sec_coie_t1
#L1 - 2684
#L2 - 113
#L3 - 2
#L4 - 1072

coie1 %>% 
  group_by(elevel) %>%
  select('sec_coie_t1') %>%
  tally()

#3.2 How many schools have TRUE sec_coie_t1?
#L2 = 63
#L4 = 1032
coie1 %>%
  group_by(elevel) %>%
  select('sec_coie_t1') %>%
  filter(sec_coie_t1 != 0) %>%
  tally()

#3.3 Are the difference between L2 - 113 - 63, L3 - 2 - 0, L4 - 1072 - 1032 because of NAs?
  ## Yes, they are
coie1 %>%
  group_by(elevel) %>%
  select('sec_coie_t1') %>%
  filter(is.na(sec_coie_t1)) %>%
  tally()

#3.3.1 Take a closer look at NAs
coie1 %>%
  group_by(elevel) %>%
  select('sec_coie_t1') %>%
  filter(is.na(sec_coie_t1)) 

#3.3.2 Where are the NAs
coie1 %>%
  group_by(elevel, region) %>%
  filter(is.na(sec_coie_t1)) %>%
  tally() %>%
  print(n = Inf)


#4. Create new variable s_coie_t1: s_coie_t1 = pri_coie_t1 if elevel = 1|4
#                                  s_coie_t1 = sec_coie_t1 if elevel = 2|3

coie1 %>%
  mutate(s_coie_t1 = case_when(
    elevel == 1 | elevel == 4 ~ pri_coie_t1,
    elevel == 2 | elevel == 3 ~ sec_coie_t1))
 
#4.2 check s_coie_t1
  ## Yes, this variable was created succssfully with values corresponding to expectation
coie1 %>%
  mutate(s_coie_t1 = case_when(
    elevel == 1 | elevel == 4 ~ pri_coie_t1,
    elevel == 2 | elevel == 3 ~ sec_coie_t1)) %>%
  select(s_coie_t1, pri_coie_t1, elevel) %>%
  filter(elevel == 1) %>%
  head()

coie1 %>%
  mutate(s_coie_t1 = case_when(
    elevel == 1 | elevel == 4 ~ pri_coie_t1,
    elevel == 2 | elevel == 3 ~ sec_coie_t1)) %>%
  select(s_coie_t1, sec_coie_t1, elevel) %>%
  filter(elevel == 2) %>%
  head()

coie1 %>%
  mutate(s_coie_t1 = case_when(
    elevel == 1 | elevel == 4 ~ pri_coie_t1,
    elevel == 2 | elevel == 3 ~ sec_coie_t1)) %>%
  select(s_coie_t1, sec_coie_t1, elevel) %>%
  filter(elevel == 3) %>%
  head()


coie1 %>%
  mutate(s_coie_t1 = case_when(
    elevel == 1 | elevel == 4 ~ pri_coie_t1,
    elevel == 2 | elevel == 3 ~ sec_coie_t1)) %>%
  select(s_coie_t1, pri_coie_t1, elevel) %>%
  filter(elevel == 4) %>%
  head()

coie1_edited <- coie1 %>%
  mutate(s_coie_t1 = case_when(
    elevel == 1 | elevel == 4 ~ pri_coie_t1,
    elevel == 2 | elevel == 3 ~ sec_coie_t1))

#4.2 Further clean coie1_edited by dropping observations with s_coie1_t1 == 0|s_coie_t1 == NA

# 4.2.2 Check if drop works correctly 
#After drop, number of obervations with TRUE s_coie_t1: L1 - 2384
#                                                       L2 - 63
#                                                       L3 - 0
#                                                       L4 - 1067
# Corroborated that drop worked correctly

coie1_edited %>%
  filter(!is.na(s_coie_t1) & s_coie_t1 != 0) %>%
  select(s_coie_t1, elevel) %>%
  count(elevel)

coie1_edited <- coie1_edited %>%
  filter(!is.na(s_coie_t1) & s_coie_t1 != 0)

## Further cleaning completed, coie1_edited df contains valid s_coie_t1 variable 

#5. Graphs to examine the distribution of s_coie_t1

#5.1 Hist
h <- ggplot(coie1_edited, aes(s_coie_t1))
h + geom_histogram(binwidth = 0.1) +
  xlim(-5, 5)

#5.2 boxplot
bp <- ggplot(coie1_edited, aes(s_coie_t1))
bp + geom_boxplot()

?scale

#6. Stardardize s_coie_t1
  ## mean(z_s_coie_t1) = 0, sd(z_s_coie_t1) = 1, indicating correct standardization
coie1_edited %>%
  mutate(z_s_coie_t1 = (s_coie_t1 - mean(s_coie_t1))/sd(s_coie_t1)) %>%
  select(z_s_coie_t1) %>%
  summarize(mean = mean(z_s_coie_t1),sd = sd(z_s_coie_t1))

#6.1 Filter out those whose z_s_coie_t1 are outside of the range of [-3, 3] around the mean
#After removing outliers:
#L1 2383
#L2 63
#L4 1063
coie1_edited %>%
  mutate(z_s_coie_t1 = (s_coie_t1 - mean(s_coie_t1))/sd(s_coie_t1)) %>%
  select(z_s_coie_t1, elevel) %>%
  filter(z_s_coie_t1 > -3 & z_s_coie_t1 < 3) %>%
  count(elevel)

  ## Take a close look at the cases that are eliminated as outliers
coie1_edited2 <- coie1_edited %>%
  mutate(z_s_coie_t1 = (s_coie_t1 - mean(s_coie_t1))/sd(s_coie_t1))

coie1_edited2 %>%
  select(elevel, region, z_s_coie_t1, s_coie_t1) %>%
  filter(z_s_coie_t1 < -3 | z_s_coie_t1 > 3)

#7. Count schools with z_s_coie within the range of [-3, 3] around the mean across region
coie1_edited2$region <- factor(coie1_edited2$region)
levels(coie1_edited2$region)  

coie1_edited2 %>%
  filter(z_s_coie_t1 > -3 & z_s_coie_t1 < 3) %>%
  group_by(elevel, region) %>%
  tally() %>%
  print(n = Inf)

#8. Check the distribution of coies for schools whose z_s_coies are within [-3, 3]
#8.1 hist
coie1_edited3 <- coie1_edited2 %>%
  filter(z_s_coie_t1 > -3 & z_s_coie_t1 < 3)

h3 <- ggplot(coie1_edited3, aes(s_coie_t1))
h3 + geom_histogram(bins = 100)  


#8.2How many schools have s_coie_t1 > 1 and how many have s_coie_t1 < 0 after removing outliers?
#n = 574 having s_coie_t1 > 1
#n = 10 having s_coie_t1 <= 0
coie1_edited3 %>%
  filter(s_coie_t1 > 1) %>%
  tally()

coie1_edited3 %>%
  filter(s_coie_t1 <= 0) %>%
  tally()

#9. Number of schools having 0<s_coie_t1<=1 by elevel & region before removing outliers using z_procedure
coie1_edited %>%
  group_by(elevel, region) %>%
  filter(s_coie_t1 > 0 & s_coie_t1 <= 1) %>%
  tally() %>%
  print(n = Inf)
#row total
coie1_edited %>%
  group_by(region) %>%
  filter(s_coie_t1 > 0 & s_coie_t1 <= 1) %>%
  tally() %>%
  print(n = Inf)

#column total
coie1_edited %>%
  group_by(elevel) %>%
  filter(s_coie_t1 > 0 & s_coie_t1 <= 1) %>%
  tally() %>%
  print(n = Inf)

#total
coie1_edited %>%
  filter(s_coie_t1 > 0 & s_coie_t1 <= 1) %>%
  tally()

#10. Number of schools having 0<s_coie_t1<1 by elevel & region after removing outliers using z_procedure

coie1_edited3 %>%
  group_by(elevel, region) %>%
  filter(s_coie_t1 > 0 & s_coie_t1 <= 1) %>%
  tally() %>%
  print(n = Inf)

#row total
coie1_edited3 %>%
  group_by(region) %>%
  filter(s_coie_t1 > 0 & s_coie_t1 <= 1) %>%
  tally() %>%
  print(n = Inf)

#column total
coie1_edited3 %>%
  group_by(elevel) %>%
  filter(s_coie_t1 > 0 & s_coie_t1 <= 1) %>%
  tally() %>%
  print(n = Inf)

#total
coie1_edited3 %>%
  filter(s_coie_t1 > 0 & s_coie_t1 <= 1) %>%
  tally()






  


