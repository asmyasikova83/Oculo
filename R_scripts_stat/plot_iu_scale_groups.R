#rm(list = ls())

library(reshape2)
library(data.table)
library(ggplot2)
library(readxl)
library(stringi)
library("openxlsx")
library("vioplot")

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
#IU
question_asd <- as.data.table(question_asd)
d_asd_IU <- question_asd[,c(1, 2, 4)]

question_nt <- question[fname %in% c('001','004','019','021','022','034',
                                     '035','032','039','040','044','047',
                                     '048','053','055','058','059','060',
                                     '061','063','064','065','066')]
d_nt_IQ <- question_nt$Performance_IQ

#IU
question_nt <- as.data.table(question_nt)
d_nt_IU <- question_nt[,c(1, 2, 4)]

d_asd_IU$group <- 'ASD'
d_nt_IU$group <- 'NT' 

p_iu <- ggplot(d_asd_IU, aes(x = as.factor(group), y = Intolerance_for_uncertanty)) + 
  geom_violin(size = 1.7, color="purple")+
  geom_boxplot(width=.1, outlier.size=0, fill="grey50") +
  labs(x="Group", y = 'Intolerance of Uncertainty') +
  theme_classic()
p_iu <- p_iu + geom_jitter(size = 3, shape=16, position=position_jitter(0.2))
p1_iu<- ggpar(p_iu,
              ylim = c(30, 80),
              font.ytickslab = 30,
              font.xtickslab = 27,
              font.main = 25,
              font.submain = 25,
              font.legend = c(20, "plain", "black"),
              font.x = 27,
              font.y = 27)

p1_iu
ggsave('C:/Users/trosh/OneDrive/jobs_Miasnikova/Oculo/presentations_pics_posters/article_at_nt/dec_prefin/ASD_IU_test.png',p1_iu,width =  6, height = 6)

