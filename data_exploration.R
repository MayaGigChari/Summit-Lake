install.packages("haven")
library(haven)
library(readxl)
library(tidyverse)
library(janitor)
data_hist<- read_dta("I0140l-6.dta")

install.packages("readstata13")
library(readstata13)

install.packages("foreign")
library(foreign)


#####THIS IS A SCRIPT for exploring previous summit data. 


length_dist<- read_excel("lengths_combined.xlsx")

# abundance estimates are already very bad. 

#read in data from 1889 abd 1991

length_dist


?pivot


df_long <- length_dist %>%
  pivot_longer(
    cols = everything(),
    names_to = "year",
    values_to = "value"
  )


ggplot(df_long, aes(x = value, fill = factor(year))) +
  geom_histogram(position = "identity", alpha = 0.4, bins = 15) +
  labs(fill = "Year")

df_long_recent<-df_long%>%
  filter(year %in% c("1990", "1991"))


ggplot(df_long_recent, aes(x = value, fill = factor(year))) +
  geom_histogram(position = "identity", alpha = 0.4, bins = 15) +
  labs(fill = "Year")


#these look similar enough. Probably consistent distribution across time. However, the sample size is relatively small.

#now we will load the 1990 and 1991 datasets and look at the CPUE by depth

data_1990<-read_excel("1990_dat.xlsx")

colnames(data_1990)

data_1990<-janitor::remove_empty(data_1990)

data_1990<- clean_names(data_1990)

data_1990 


# we need to apply the CPUE bootstrap to this. 

matricized_19980<- data_1990%>%
  filter(depth<=15)%>%
  group_by(trans, trap_number)%>%
  summarise(
    count = unique(number_caught_11)
  )%>%
  select(transect_number = trans, trap_number, count)%>%
  mutate(across(everything(), ~replace_na(.x,0)))


transect_numbers<- length(unique(matricized_19980$transect_number))
transect_sizes<-matricized_19980%>%
  group_by(transect_number)%>%
  summarise(
    transect_sizes= n_distinct(trap_number)
  )




max_transect_size<- max(transect_sizes$transect_sizes)


transect_matrix_1990<- matrix(NA, nrow = transect_numbers, ncol = max_transect_size)


mat_1990<- pop_mat(matricized_19980, transect_matrix_1990, transect_numbers)

#now we have the matrix. 

cpue_1990_small15<- cpue_boot(mat_1990)

cpue_1990_small15


#this is NOT normal. 

matricized_19980<- data_1990%>%
  group_by(trans, trap_number)%>%
  summarise(
    count = unique(number_caught_11)
  )%>%
  select(transect_number = trans, trap_number, count)%>%
  mutate(across(everything(), ~replace_na(.x,0)))


transect_numbers<- length(unique(matricized_19980$transect_number))
transect_sizes<-matricized_19980%>%
  group_by(transect_number)%>%
  summarise(
    transect_sizes= n_distinct(trap_number)
  )




max_transect_size<- max(transect_sizes$transect_sizes)


transect_matrix_1990<- matrix(NA, nrow = transect_numbers, ncol = max_transect_size)


mat_1990<- pop_mat(matricized_19980, transect_matrix_1990, transect_numbers)

#now we have the matrix. 

cpue_1990_alldata<- cpue_boot(mat_1990)

cpue_1990_alldata

#this is normal and follows the Rao-Wu stipulation. 


#now let's try for fish > 15 m only


matricized_19980<- data_1990%>%
  group_by(trans, trap_number)%>%
  filter(depth>15)%>%
  summarise(
    count = unique(number_caught_11)
  )%>%
  select(transect_number = trans, trap_number, count)%>%
  mutate(across(everything(), ~replace_na(.x,0)))


transect_numbers<- length(unique(matricized_19980$transect_number))
transect_sizes<-matricized_19980%>%
  group_by(transect_number)%>%
  summarise(
    transect_sizes= n_distinct(trap_number)
  )




max_transect_size<- max(transect_sizes$transect_sizes)


transect_matrix_1990<- matrix(NA, nrow = transect_numbers, ncol = max_transect_size)


mat_1990<- pop_mat(matricized_19980, transect_matrix_1990, transect_numbers)

#now we have the matrix. 

cpue_1990_large15<- cpue_boot(mat_1990)

cpue_1990_large15

#################Let's do the same thing for 1991

data_1991<-read_excel("1991_dat.xlsx")

colnames(data_1991)

data_1991<-janitor::remove_empty(data_1991)

data_1991<- clean_names(data_1991)

data_1991 


# we need to apply the CPUE bootstrap to this. 

matricized_1991<- data_1991%>%
  filter(depth<=15)%>%
  group_by(trans, trap_number)%>%
  summarise(
    count = unique(number_caught_11)
  )%>%
  select(transect_number = trans, trap_number, count)%>%
  mutate(across(everything(), ~replace_na(.x,0)))


transect_numbers<- length(unique(matricized_1991$transect_number))
transect_sizes<-matricized_1991%>%
  group_by(transect_number)%>%
  summarise(
    transect_sizes= n_distinct(trap_number)
  )




max_transect_size<- max(transect_sizes$transect_sizes)


transect_matrix_1991<- matrix(NA, nrow = transect_numbers, ncol = max_transect_size)


mat_1991<- pop_mat(matricized_1991, transect_matrix_1991, transect_numbers)

#now we have the matrix. 

cpue_1991_small15<- cpue_boot(mat_1991)

cpue_1991_small15


#this is NOT normal. 

matricized_1991<- data_1991%>%
  group_by(trans, trap_number)%>%
  summarise(
    count = unique(number_caught_11)
  )%>%
  select(transect_number = trans, trap_number, count)%>%
  mutate(across(everything(), ~replace_na(.x,0)))


transect_numbers<- length(unique(matricized_1991$transect_number))
transect_sizes<-matricized_1991%>%
  group_by(transect_number)%>%
  summarise(
    transect_sizes= n_distinct(trap_number)
  )




max_transect_size<- max(transect_sizes$transect_sizes)


transect_matrix_1991<- matrix(NA, nrow = transect_numbers, ncol = max_transect_size)


mat_1991<- pop_mat(matricized_1991, transect_matrix_1991, transect_numbers)

#now we have the matrix. 

cpue_1991_alldata<- cpue_boot(mat_1991)

cpue_1991_alldata

#this is normal and follows the Rao-Wu stipulation. 


#now let's try for fish > 15 m only


matricized_1991<- data_1991%>%
  group_by(trans, trap_number)%>%
  filter(depth>15)%>%
  summarise(
    count = unique(number_caught_11)
  )%>%
  select(transect_number = trans, trap_number, count)%>%
  mutate(across(everything(), ~replace_na(.x,0)))


transect_numbers<- length(unique(matricized_1991$transect_number))
transect_sizes<-matricized_1991%>%
  group_by(transect_number)%>%
  summarise(
    transect_sizes= n_distinct(trap_number)
  )




max_transect_size<- max(transect_sizes$transect_sizes)


transect_matrix_1991<- matrix(NA, nrow = transect_numbers, ncol = max_transect_size)


mat_1991<- pop_mat(matricized_1991, transect_matrix_1991, transect_numbers)

#now we have the matrix. 

cpue_1991_large15<- cpue_boot(mat_1991)

cpue_1991_large15

#so obviously we have an extremely zero-inflated low power estimate of the cpue for traps set < 15 m. I haven't done any statistics but it seems clear to me that CPUE is not evenly distributed throughough the lake in these two years. 
