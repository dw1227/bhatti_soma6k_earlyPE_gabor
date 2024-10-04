rm(list=ls())
library(caret)
library(randomForest)
library(Metrics)
library(ranger)
library(tidyverse)
library(lubridate)
library(SomaDataIO)
#library(lmerTest)
library(limma)
library(Biobase)
library(mice)
library(StatisticalModels)

setwd("//prbfs25//BioInformatics//soma6k//gaurav/")
tissue="Blood"
addinfo="Controls"

load("data/ano_nomiss.Rdata")
anosafe<-ano[!duplicated(ano$Main_Index),]
rownames(anosafe)<- anosafe$Main_Index
#################read clinical data
ano<-read_csv("//prbfs25//BioInformatics//soma6k//UpdatedClinicalinformation.csv")
ano=ano[grepl("HUZ",ano$SampleId)|grepl("CHI",ano$SampleId),]


ano<-ano[order(ano$RowCheck,decreasing = T),]
ano=ano[!duplicated(ano$SampleId),] %>% as.data.frame()
rownames(ano)= ano$Field1



ano<- ano %>% 
  mutate(weight= as.numeric(as.character(Pre_Preg_Wtlb))*0.453592,
         height=as.numeric(as.character(HGTinch))*2.54,
         smoker=factor(ifelse(Smoking=="-1",1,0)),
         fsex=factor(ifelse(Sex=="Female",1,0)),
         parity=as.numeric(as.character(Term))+as.numeric(as.character(Preterm)),
         age= as.numeric(as.character(Age)),
         matrace=factor(ifelse(Race=="Black",1 ,0)),
         Nulliparous=factor(ifelse(parity=="0",1,0)),
         Chronic_HTN=factor(ifelse(Enrollment_Chronic_HTN=="Yes",1,0)),
         BMI=as.numeric(as.character(BMI))
  )
ano$BMI[which(ano$BMI<0.1)]=NA
ano$smoker<- as.character(ano$smoker)


ano$smoker<-anosafe[as.character(ano$Main_Index),"smoker"]

ano$weight<-anosafe[as.character(ano$Main_Index),"weight"]
ano$Chronic_HTN<-anosafe[as.character(ano$Main_Index),"Chronic_HTN"]

# ##########missing smokers
# smkmiss<- read.csv("//prbfs25/BioInformatics/soma6k/gaurav/missing.csv",
#                    colClasses = "character")
# smkmiss<- smkmiss[is.na(smkmiss$smoker),]
# rownames(smkmiss)<- smkmiss$X
# ano[match(rownames(smkmiss), rownames(ano)),"smoker"]=smkmiss$Smoking
# ano$smoker<- factor(as.character(ano$smoker))
# 
# ############missing weight
# ano_nodupl<- ano %>% 
#   dplyr::filter(!duplicated(Main_Index)) %>% 
#   dplyr::select(c(weight,height,smoker,fsex,parity,age,matrace,Chronic_HTN )) 
# 
# imp_mod<-  mice(ano_nodupl, m=5, maxit = 50, method = 'pmm', 
#                 seed = 500,print=F)
# 
# df_imp<- complete(imp_mod)
# df_imp$Main_Index<- ano[rownames(df_imp),"Main_Index"]
# rownames(df_imp)<- df_imp$Main_Index
# 
# ano<- ano %>% 
#   mutate(weight= df_imp[as.character(Main_Index),"weight"],
#          Chronic_HTN=df_imp[as.character(Main_Index),"Chronic_HTN"])




# complete(mod1)[names(predweights),"weight"]
# wtmod<- (with(ano[!duplicated(ano$Main_Index),],lm(log2(weight)~poly(height,2)
#                                                    +poly(age,1)
#                                                    +smoker)))
# predweights<- 2^predict(wtmod,ano[is.na(ano$weight),])
#ano[names(predweights),"weight"]=predweights
save(ano,file="data/ano_nomiss_update.Rdata")
