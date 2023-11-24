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
rt_teens <- read.csv(paste0(path,"resp_merged_teens_big.csv"))

rt_teens <- as.data.table(rt_teens)
rt_teens$group <- 'teens'
length(unique(rt_teens$fname))

rt_adults <- fread(paste0(path,"norma.txt"))
rt_adults <- as.data.table(rt_adults[,!c(21:25)])
adults <- c("P001_1", "P002_1", "P003_1", "P004_1", "P005_1",
            "P006_1", "P007_1", "P008_1", "P009_1", "P065_1",
            "P011_1", "P012_1", "P043_1", "P044_1", "P045_1",
            "P046_1", "P047_1", "P048_1", "P049_1", "P050_1" )
rt_adults<- rt_adults[fname %in% adults]
rt_adults <- rt_adults[block!=6]
rt_adults$group <- 'adults'
length(unique(rt_adults$fname))

rt <- rbind(rt_teens, rt_adults)
rt[, index := 1:.N, by=c('fname','block')]

rt <- as.data.table(rt)
unique(rt$fname)
#rt<- na.omit(rt)

#rename
rt$fname <- gsub('_1','',rt$fname)
rt$fname <- gsub('^p','P',rt$fname)

#filter
rt <- rt[grep('P[0-9]',rt$fname)]
#rt<- rt[!(rt$RT< 300 | rt$RT> 4000),]

rt$text <- gsub('RESPONSE ','',rt$text)
rt$text <- gsub('[0-9]','',rt$text)
rt$text <- gsub('_','',rt$text)
rt$text <- gsub(' ','_',rt$text)
rt$text <- gsub('V_','V',rt$text)

unique(rt$text)
#rt$choice <- 'other'
rt[text == 'REW_VAL' & next_risk == 0, choice := 'hp_rv'][text == 'REW_VAL' & next_risk == 1, choice := 'lp_rv']
rt[text == 'REW_INV' & next_risk == 0, choice := 'hp_ri'][text == 'REW_INV' & next_risk == 1, choice := 'lp_ri']
rt[text == 'LOSE_VAL' & next_risk == 0, choice := 'hp_lv'][text == 'LOSE_VAL' & next_risk == 1, choice := 'lp_lv']
rt[text == 'LOSE_INV' & next_risk == 0, choice := 'hp_li'][text == 'LOSE_INV' & next_risk == 1, choice := 'lp_li']


#3 RT raw and outliers
#outliers
rt[,RT_raw:=RT,by=fname]
rt[,RT_raw_sd:=sd(RT_raw),by=fname]
rt[,RT_flag:=abs(RT_raw)<3*RT_raw_sd]
flag <- 'RT_flag'
rt <- rt[rt[,get(flag)==T]]

rt[,RT_log_raw:=log10(RT)]
rt <- rt[!is.na(RT_log_raw)]
################################RT stat#################################################################

q<- rt %>%
  group_by(fname, block, choice)%>%summarise(num_trials = n(),
                                        mean_log10_RT = mean(RT_log_raw),
                                        mean_raw_RT = mean(RT_raw))
q

q <- as.data.table(q)
q
out_path
write.csv(q,paste0(out_path, "choice_types_group_log_and_raw_rt.csv"))
