#rm(list = ls())

library(viridis)
library(reshape2)
library(data.table)
library(ggplot2)
library("ggpubr")
library(readxl)
library(tidyr)
library(dplyr)
library(stringi)
library("openxlsx")
library("vioplot")
library(gridExtra)

###############################normals###################################################################
database_norm<- read_excel('C:/Users/trosh/OneDrive/jobs_Miasnikova/Oculo/probability_database_norm.xlsx')

database_norm<-database_norm[2:66,]
colClean1 <- function(database_norm){ colnames(database_norm) <- gsub(" ", "_", colnames(database_norm)); database_norm }
database_norm <- colClean1(database_norm)
colClean2 <- function(database_norm){ colnames(database_norm) <- gsub("...1", "fname", colnames(database_norm)); database_norm }
database_norm <- colClean2(database_norm)

database_norm<- filter(database_norm, Code %in%c('P001','P004','P019','P021','P022','P034',
                                                 'P035','P032','P039','P040','P044','P047',
                                                 'P048','P053','P055','P058','P059','P060',
                                                 'P061','P063','P064','P065','P066'))

database_nt <- database_norm[, c(9, 16)]
colnames(database_nt)[1] <- "age"
colnames(database_nt)[2] <- "sex"
#add p066 manually
database_nt[23,1] <-33
database_nt[22:23,2] <- 'F'
database_nt$group <- 'NT'

###############################autists###################################################################

database_autists<- read_excel('C:/Users/trosh/OneDrive/jobs_Miasnikova/Oculo/probability_database.xlsx')

#remove zeros
database_autists <- subset(database_autists,  !(age %in% c(0)))
colClean2 <- function(database_autists){ colnames(database_autists) <- gsub("...1", "fname", colnames(database_autists)); database_autists }
database_autists <- colClean2(database_autists)

database_autists<- filter(database_autists,  fname %in% c('P301', 'P304','P307', 'P312','P313','P314',
                                     'P316', 'P318','P321', 'P322','P323','P324',
                                     'P325','P326','P327', 'P328','P329','Р333', 
                                     'Р334','Р335','Р338','P341', 'P342'))
database_autists$fname
database_asd <- database_autists[, c(10, 31)]
#add p0342 manually
database_asd[22:23,2] <- 'F'
colnames(database_asd)[1] <- "age"
colnames(database_asd)[2] <- "sex"
database_asd$group <- 'ASD'

##################################AQ#######################
path <- "C:/Users/trosh/OneDrive/jobs_Miasnikova/Oculo/"
question <- read_excel(paste0(path, 'Questionnares.xls'))

#replace blank spaces in colnames
colClean1 <- function(question){ colnames(question) <- gsub(" ", "_", colnames(question)); question }
question <- colClean1(question)
colClean2 <- function(question){ colnames(question) <- gsub("...1", "fname", colnames(question)); question }
question <- colClean2(question) 

question <- as.data.table(question)
question[, fname:=stri_extract_first_regex(question$fname,"[0-9]+")]
question$fname
question_asd <- question[fname %in% c('301', '304', '307', '312', '313', '314',
                                  '316', '318', '321', '322', '323', '324',
                                  '325', '326', '327', '328', '329', '333', 
                                  '334', '335', '338', '341', '342')]
question_asd$group <- 'ASD'
#add AQ sum for p0342 manually
question_asd$AQ_sum[23] <-122
question_asd <- as.data.table(question_asd)
d <- question_asd[,c(15, 21)]
d_asd_IQ <- question_asd$Performance_IQ


question_nt <- question[fname %in% c('001','004','019','021','022','034',
                                     '035','032','039','040','044','047',
                                     '048','053','055','058','059','060',
                                     '061','063','064','065','066')]
d_nt_IQ <- question_nt$Performance_IQ

p_aq <- ggplot(d, aes(x = group, y = AQ_sum)) + 
  geom_violin(size = 1.7, color="purple")+
  geom_boxplot(width=.1, outlier.size=0, fill="grey50") +
  labs(x="Group", y = 'Autistic Quotient') +
  scale_fill_viridis(discrete=T) +
  theme_classic()

p1_aq<- ggpar(p_aq,
           font.ytickslab = 30,
           font.xtickslab = 27,
           font.main = 25,
           font.submain = 25,
           font.legend = c(20, "plain", "black"),
           font.x = 27,
           font.y = 27)

p1_aq
ggsave('C:/Users/trosh/OneDrive/jobs_Miasnikova/Oculo/pics_articles/article_at_nt/AQ_sum.png',p1_aq,width =  6, height = 5)


#######################################age#####################################
database<- rbind(database_nt, database_asd)
database

p <-  database %>%
      mutate(Group = factor(group, levels=c("ASD", "NT"))) %>%
      ggplot(aes(fill=group, y=age, x=group)) + 
      geom_violin(position="dodge", alpha=0.7, size = 0.7) +
      geom_boxplot(width=.1, outlier.size=0, fill="grey50") +
      stat_summary(fun.y=median, geom="point", fill="white", shape=21, size=4) +
      scale_fill_viridis(discrete=T, name="") +
      theme_classic() +
      xlab("Group") +
      ylab("Age") +
      ylim(20,50)

p1<- ggpar(p,
                    font.ytickslab = 30,
                    font.xtickslab = 27,
                    font.main = 25,
                    font.submain = 25,
                    font.legend = c(20, "plain", "black"),
                    font.x = 27,
                    font.y = 27)

p1
ggsave('C:/Users/trosh/OneDrive/jobs_Miasnikova/Oculo/pics_articles/article_at_nt/Age.png',p1,width =  6, height = 5)

# sex
dat = database %>%
  group_by(group, sex) %>%
  summarise(n=n())
dat

p_sex <- ggplot(dat, aes(x = group, y= n, fill = sex), xlab="") +
  geom_bar(stat="identity", width=.5, position = "dodge", alpha=0.7, size = 1.7) +
  labs(x="Group", y = 'N') +
  scale_fill_viridis(discrete=T, name="Sex") +
  theme_classic()

p1_sex<- ggpar(p_sex,
           font.ytickslab = 30,
           font.xtickslab = 27,
           font.main = 25,
           font.submain = 25,
           font.legend = c(20, "plain", "black"),
           font.x = 27,
           font.y = 27)

p1_sex
ggsave('C:/Users/trosh/OneDrive/jobs_Miasnikova/Oculo/pics_articles/article_at_nt/Sex.png',p1_sex,width =  6, height = 5)

#####################################non_verbal_IQ comparison#######################################

x <- d_asd_IQ
y <- d_nt_IQ
d_asd_IQ
d_nt_IQ
t.test(x,y, paired=FALSE) 
