# LMM analysis RT

#rm(list = ls())
library(reshape2)
library(data.table)
library(ggplot2)
library("ggpubr")
library(readxl)
library(tidyr)
library(dplyr)
library(lme4)
library("ggpubr")
library(emmeans)
library(lmerTest)
library(readxl)
library(stringi)
library(readr)

path <- "C:/Users/trosh/OneDrive/jobs_Miasnikova/Oculo/TEENS/"
out_path <- paste0(path,'pics/')

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

#fb prev
rt[prev_rew == 1, feedback_prev:='positive'][prev_rew == 0, feedback_prev:='negative']

#rt$quanity <- 'other'
#rt[prev_rew == 1 & risk == 1, quantity := 'risk_prev_wins'][prev_rew == 0 & risk == 1, quantity := 'risk_prev_losses']
#rt[prev_rew == 1 & risk == 0, quantity := 'norisk_prev_wins'][prev_rew == 0 & risk == 0, quantity := 'norisk_prev_losses']

rt$text <- gsub('RESPONSE ','',rt$text)
rt$text <- gsub('[0-9]','',rt$text)
rt$text <- gsub('_','',rt$text)
rt$text <- gsub(' ','_',rt$text)
rt$text <- gsub('V_','V',rt$text)

unique(rt$text)
rt$quantity <- 'other'
rt[text == 'REW_VAL' & next_risk == 0, quantity := 'hp_rv'][text == 'REW_VAL' & next_risk == 1, quantity := 'lp_rv']
rt[text == 'REW_INV' & next_risk == 0, quantity := 'hp_ri'][text == 'REW_INV' & next_risk == 1, quantity := 'lp_ri']
rt[text == 'LOSE_VAL' & next_risk == 0, quantity := 'hp_lv'][text == 'LOSE_VAL' & next_risk == 1, quantity := 'lp_lv']
rt[text == 'LOSE_INV' & next_risk == 0, quantity := 'hp_li'][text == 'LOSE_INV' & next_risk == 1, quantity := 'lp_li']
rt[trained == FALSE, train:='not_trained'][trained== TRUE, train:='trained']

unique(rt$quantity)

rt[,RT_log_raw:=log10(RT)]
rt <- rt[!is.na(RT_log_raw)]

rt[,RT_raw:=RT,by=fname]
rt[,RT_raw_sd:=sd(RT_raw),by=fname]
rt[,RT_flag:=abs(RT_raw)<3*RT_raw_sd]
flag <- 'RT_flag'
rt <- rt[rt[,get(flag)==T]]

q<- rt %>%
  group_by(group, train, quantity)%>%summarise(num_trials = n(),
                                             mean_log10_RT = mean(RT_log_raw),
                                             mean_raw_RT = mean(RT_raw))

q <- as.data.table(q)

################################lmem#########################################
interval <- 'RT_raw'

m <- lmer(get(interval) ~ train*quantity*group + (1|fname) + (1|block), data = rt)
anova(m)
s <- step(m)
m2 <- get_model(s)
anova(m2)


thr <- max(rt[, mean(get(interval)), by=quantity]$V1) #
#thr_min <- min(df_large_group[, mean(get(interval)) - sterr(get(interval)), by=trial_type4]$V1) 

#choice type group
#Tuk <- data.table(summary(emmeans(m, pairwise ~ quantity|train|group, adjust = 'tukey',lmer.df = "satterthwaite",lmerTest.limit=8000))$contrasts)

Tuk <- emmeans(m, pairwise ~ pairwise ~ group|train|quantity  , adjust = 'tukey',lmer.df = "satterthwaite",lmerTest.limit=8000,level = 0.99)

Tuk1<- summary(Tuk)$contrasts
Tuk1<- as.data.table(Tuk1)

Tuk1 <- Tuk1[, group1:=gsub(' -.*', '', contrast)][, group2:=gsub('.*- ', '', contrast)]
Tuk1 <- Tuk1[p.value<0.1, p_significant:=format(p.value, digits = 3)]
Tuk1 
n <- Tuk1[!is.na(p_significant), .N]

thr <- max(rt[, mean(get(interval)), by=quantity]$V1) #
thr <- thr-0.15
if (n>1){
  Tuk1 <- Tuk1[!is.na(p_significant), y.position := seq((thr+0.01), (thr+0.3), 0.29/(n-1))]
} else {
  Tuk1 <- Tuk1[!is.na(p_significant), y.position := thr+0.36]
}

Tuk1[p.value<0.001, stars:='***']
Tuk1[p.value<0.01 & p.value>0.001 , stars:='**']
Tuk1[p.value<0.05 & p.value>0.01 , stars:='*']
Tuk1[p.value>0.05 & p.value<0.1 , stars:='#']
Tuk1[,y.position:=y.position-0.1]
Tuk1

############################################################################

sequence <-data.table(quantity=c("hp_rv", "hp_li", "lp_rv", "lp_ri", "hp_lv", "hp_ri", "lp_li", "lp_lv"),number=c(1,2,3,4,5,6,7,8))

y_values_rew <- rt[group == 'teens',
                               mean(get(interval)),by='quantity']

setnames(y_values_rew,'V1','y_values_rew')

y_values_lose <- rt[group == 'adults',
                                mean(get(interval)),by='quantity']

setnames(y_values_lose,'V1','y_values_lose')

y_values <- merge(y_values_lose,y_values_rew,by='quantity')
y_values <- merge(y_values,sequence,by='quantity')
y_values[,y_max:=max(y_values_lose,y_values_rew),by=quantity]
y_values <- merge(y_values,Tuk1,by='quantity')
###################################################################
title <- 'Teens vs Adults NO LEARNING'
Tuk1  <- Tuk1[train == 'not_trained']
rt <- rt[train == 'not_trained']
y_values<- y_values[train == 'not_trained']

p <- ggline(rt, 'quantity', interval,
            color = 'group',
            add = c("mean_se"),
            #order = c("norisk_prev_losses","norisk_prev_wins","risk_prev_losses","risk_prev_wins"),
            order = c("hp_rv", "hp_li", "lp_rv", "lp_ri", "hp_lv", "hp_ri", "lp_li", "lp_lv"),
            ylab = interval, xlab = title,
            size = 1.5, 
            point.size = 1.8,
            font.label = list(size = 26, color = "black"),
            font.legend = c(20, "plain", "black"))+
  #scale_x_discrete(labels = c("No learning", 'After learning'))+
  #scale_x_discrete(name=title, labels = c("HP_prev_losses","HP_prev_wins","LP_prev_losses","LP_prev_wins"))+
  scale_color_discrete(name = "Group", labels = c("adults", "teens"))
p
pic_signif <- p+  geom_signif(y_position=c(y_values$y_max+.3),
                                   xmin=c(y_values$number), xmax=c(y_values$number),
                                   annotation=c(Tuk1$stars), 
                                   tip_length=0.05,textsize = 10,vjust = 0.4)
pic_signif
ggsave('C:/Users/trosh/OneDrive/jobs_Miasnikova/Oculo/TEENS/pics/RT/RT_all_choices_train.png',pic_signif,width =  12, height = 5)
