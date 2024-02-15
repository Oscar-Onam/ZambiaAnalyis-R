# Import Data and libraries
library(tools)
library(tidyverse)
library(haven)


file = file_path_as_absolute(file.choose())

# Convert factor variables to factor and check their levels
f_data <- read_dta(file)

f_data$region <- factor(f_data$region)
f_data$elevel <- factor(f_data$e_level)
f_data$elevelx <- factor(f_data$elevelx)

levels(f_data$region)
contrasts(f_data$region)

levels(f_data$elevel)
contrasts(f_data$elevel)

levels(f_data$elevelx)
  ## elevel has five levels ??
  ## elevelx has three levels
# Whether their are sample size changes between elevel and elevelx?

f_data %>%
  count(elevel)
  # Primary = 1850 + 3417 = 5266
  # Secondary = 646 + 14 = 660
  # Primary & Secondary = 2980 
f_data %>% 
  count(elevelx)
  ## No, the combined sample size are equal between elevel & elevelx

#1. COUNT number of schools over elevel and region
f_data %>%
  group_by(region, elevelx) %>%
  summarize(n = n()) %>%
  print(n = Inf)

#2. COUNT number of schools who have enrollment for both years (2017 and 2018) and 
#   repetition data for 2018

#2.1 How many schools don't satisfy the above criterion
f_data %>%
  filter(ultimatest == 0) %>%
  count(region, elevelx) %>%
  print(n = Inf)

#2.1.1 row total
f_data %>%
  filter(ultimatest == 0) %>%
  count(region) %>%
  print(n = Inf)

#2.1.2 column total
f_data %>%
  filter(ultimatest == 0) %>%
  count(elevelx) %>%
  print(n = Inf)


#2.2 How many schools satisfy the above criterion?
f_data %>%
  filter(ultimatest == 1) %>%
  count(region, elevelx) %>%
  print(n = Inf)

#2.2.1 row total
f_data %>%
  filter(ultimatest == 1) %>%
  count(region) %>%
  print(n = Inf)

#2.2.2 column total
f_data %>%
  filter(ultimatest == 1) %>%
  count(elevelx) %>%
  print(n = Inf)











