library(reshape2)
library(data.table)
library(ggplot2)
library(ggpubr)
library(stringi)
library(dplyr)

#rm(list = ls())
path <- "C:/Users/trosh/OneDrive/jobs_Miasnikova/Oculo/TEENS/"
out_path <- "C:/Users/trosh/OneDrive/jobs_Miasnikova/Oculo/TEENS/tables/"
#eyetracker

#path <- "C:/Users/trosh/OneDrive/jobs_Miasnikova/Oculo/"
#rt <- fread(paste0(path,"norma.txt"))
#rt <- as.data.table(rt[,!c(21:25)])
#rt <- rt[block!=6]

rt <- read.csv(paste0(path,"resp_merged_teens.csv"))

rt <- as.data.table(rt)
unique(rt$fname)
#rt<- na.omit(rt)

#rename
rt$fname <- gsub('_1','',rt$fname)
rt$fname <- gsub('^p','P',rt$fname)

#filter
rt <- rt[grep('P[0-9]',rt$fname)]
rt<- rt[!(rt$RT< 300 | rt$RT> 4000),]

#shift and stay
rt$shift_from_HP_to_LP = 'other'
rt<- rt[risk == TRUE & prev_risk == FALSE, shift_from_HP_to_LP:='shift'][risk == TRUE & prev_risk == TRUE, shift_from_HP_to_LP:='stay']
rt<- rt[risk == TRUE, risks:='risk'][risk == FALSE, risks:='not_risk']
rt$first_risk = 'other'
rt<- rt[(risk == TRUE & str_split_i(info, '_', -1) == '1'), first_risk:='first_trial_risk']

#subjects
#problem info 'P719 '
rt<- filter(rt, fname %in% c("P701", "P702", "P703", "P705", "P706", "P707", "P708", "P709", "P710", "P711", "P712",
                             "P713", "P714","P715", "P716", "P717", "P718",  "P723", "P726", "P727", "P728", "P729",
                             "P730", "P731"))
#rt<- filter(rt, fname %in% c("P001", "P002", "P003", "P004", "P005",
#                                    "P006", "P007", "P008", "P009", "P065",
#                                    "P011", "P012", "P043", "P044", "P045",
#                                    "P046", "P047", "P048", "P049", "P050" ))
#to adults P065
###########################################making tables################################################
q_trials <- rt %>%
  group_by(fname, block, first_risk)%>%summarise(num_all_trials=n(),
                                     sum_risk=sum(risk),
                                    ) #### num of trials
write.csv(q_trials,paste0(out_path, "teens_trials_rt_filter.csv"))
#write.csv(q_trials,paste0(out_path, "adults_trials_no_rt_filter.csv"))

q_shift <- rt %>%
  group_by(fname, block, shift_from_HP_to_LP)%>%summarise(num_shift_stay=n()) #### num of shifts/stay to risk

write.csv(q_shift,paste0(out_path, "teens_shift_rt_filter.csv"))
#write.csv(q_shift,paste0(out_path, "adults_shift_no_rt_filter.csv"))


