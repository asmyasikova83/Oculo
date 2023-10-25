library(reshape2)
library(data.table)
library(ggplot2)
library(ggpubr)
library(stringi)
library(dplyr)
library(stringr)

#rm(list = ls())
path <- "C:/Users/trosh/OneDrive/jobs_Miasnikova/Oculo/TEENS/"

#eyetracker

rt <- read.csv(paste0(path,"resp_merged_teens_table_general_stat_w_blinks.csv"))
rt <- as.data.table(rt)
#rt<- na.omit(rt)

#rename
rt$fname <- gsub('_1','',rt$fname)
rt$fname <- gsub('^p','P',rt$fname)

rt <- rt[grep('P[0-9]',rt$fname)]
#rt<- rt[!(rt$RT< 300 | rt$RT> 4000),]

rt$text <- gsub('RESPONSE ','',rt$text)
rt$text <- gsub('[0-9]','',rt$text)
rt$text <- gsub('_','',rt$text)
rt$text <- gsub(' ','_',rt$text)
rt$text <- gsub('V_','V',rt$text)

rt[fname == 'P701' & block == 5]$text

rt[text == 'REW VAL', REW_VAL := TRUE][text != 'REW VAL', REW_VAL := FALSE]
rt[text == 'REW INV', REW_INV := TRUE][text != 'REW INV', REW_INV := FALSE]
rt[text == 'LOSE VAL', LOSE_VAL := TRUE][text != 'LOSE VAL', LOSE_VAL := FALSE]
rt[text == 'LOSE INV', LOSE_INV := TRUE][text != 'LOSE_INV', LOSE_INV := FALSE]

#subjects
#problem info 'P719 '
rt<- filter(rt, fname %in% c("P701", "P702", "P703", "P705", "P706", "P707", "P708", "P709", "P710", "P711", "P712",
                             "P713", "P714","P715", "P716", "P717", "P718",  "P723", "P726", "P727", "P728", "P729",
                             "P730", "P731"))

###########################################making tables################################################

q <- rt[block == 1] %>%
  group_by(fname, text)%>%summarise(n=n()) #### число трайлов обученных и нет

#transpose
q_cast <- dcast(as.data.table(q), 
      fname  ~ text,
      value.var = c("n"))

#num of advantageous decisions
q_block1 <- q_cast %>% mutate(advantageous_num = REW_VAL + LOSE_INV) %>% mutate(adv_proportion = (REW_VAL + LOSE_INV)/40)

out_path <- "C:/Users/trosh/OneDrive/jobs_Miasnikova/Oculo/TEENS/"
#if you want all five blocks in 1 table q <- rt%>%
#group_by(fname, text)%>%summarise(n=n()) 
#write.csv(q,paste0(out_path, "teens_block1_5.csv"))

write.csv(q_block1,paste0(out_path, "teens_block1.csv"))

q <- rt[block == 2] %>%
  group_by(fname, text)%>%summarise(n=n()) #### число трайлов обученных и нет

q_cast <- dcast(as.data.table(q), 
                fname  ~ text,
                value.var = c("n"))

q_block2 <- q_cast %>% mutate(advantageous_num = REW_VAL + LOSE_INV) %>% mutate(adv_proportion = (REW_VAL + LOSE_INV)/40)

write.csv(q_block2,paste0(out_path, "teens_block2.csv"))

q <- rt[block == 3] %>%
  group_by(fname, text)%>%summarise(n=n()) #### число трайлов обученных и нет

q_cast <- dcast(as.data.table(q), 
                fname  ~ text,
                value.var = c("n"))

q_block3 <- q_cast %>% mutate(advantageous_num = REW_VAL + LOSE_INV) %>% mutate(adv_proportion = (REW_VAL + LOSE_INV)/40)

write.csv(q_block3,paste0(out_path, "teens_block3.csv"))

q <- rt[block == 4] %>%
  group_by(fname, text)%>%summarise(n=n()) #### число трайлов обученных и нет

q_cast <- dcast(as.data.table(q), 
                fname  ~ text,
                value.var = c("n"))

q_block4 <- q_cast %>% mutate(advantageous_num = REW_VAL + LOSE_INV) %>% mutate(adv_proportion = (REW_VAL + LOSE_INV)/40)

write.csv(q_block4,paste0(out_path, "teens_block4.csv"))

q <- rt[block == 5] %>%
  group_by(fname, text)%>%summarise(n=n()) #### число трайлов обученных и нет

q_cast <- dcast(as.data.table(q), 
                fname  ~ text,
                value.var = c("n"))

q_block5 <- q_cast %>% mutate(advantageous_num = REW_VAL + LOSE_INV) %>% mutate(adv_proportion = (REW_VAL + LOSE_INV)/40)

write.csv(q_block5,paste0(out_path, "teens_block5.csv"))
