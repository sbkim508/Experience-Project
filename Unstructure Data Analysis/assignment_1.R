# ======================================================================================= #
## Script id   : 0.기초전처리.R
## Script Name : 데이터 기초 전처리
'## Input       : C:/R-sch/A_DeviceMotion_data/'
'## Output       : NA'

## Date        : 2021.4.12
# ======================================================================================= #
#### 0. 라이브러리 및 환경 설정
Sys.getenv("R_LIBS")
.libPaths("C:/Program Files/R/R-4.0.1/library")
.libPaths()

rm(list = ls()); gc();

library(pracma) 
library(signal)
library(seewave)
library(e1071)
library(stringr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(changepoint)

Sys.setenv(JAVA_HOME = "C:/Program Files/Java/jre1.8.0_241") # 자바 환경변수 직접 설정 
library(RWeka)
library(rJava)

setwd('C:/R-sch/A_DeviceMotion_data')
load("HAR_Assignment.RData")

# ======================================================================================= #
#### 1. 경로 설정 및 데이터 로드
# 데이터 경로 설정
d <- setwd('C:/R-sch/A_DeviceMotion_data')
fls <- dir(d, recursive = TRUE)
fls <- fls[str_detect(fls,'.csv')] ; length(fls)

# 데이터 객체 로드
i <- 1
for (f in fls) {
  a <- file.path(str_c(d,"/",f))
  print(paste0("진행 단계 : ", round(i / length(fls) * 100,2), ' %')) ; i <- i+1
  temp <- read.csv(a)
  assign(f,temp)
}
length(fls)


# ======================================================================================= #
### 2. 기초 데이터 전처리
# 사용자 정의 함수
mag <- function(df, column){
  df[,str_c("mag", column)] <- with(df, sqrt(get(str_c(column, ".x"))^2 + get(str_c(column, ".y"))^2 + get(str_c(column, ".z"))^2))
  return(df) 
}

skewness <- function(x) {
  (sum((x-mean(x))^3)/length(x)) / ((sum((x-mean(x))^2)/length(x)))^(3/2)
}

rss <- function(x) rms(x)*(length(x))^0.5

id_f <- function(x) {
  exp_no = unlist(regmatches(x, gregexpr("[[:digit:]]+", x)[1]))[1]
  id = unlist(regmatches(x, gregexpr("[[:digit:]]+",x)[1]))[2] 
  activity = unlist(str_split(x, "\\_"))[1]
  
  return(cbind(exp_no, id, activity))
}

normalize <- function(x) { 
  return ((x - min(x)) / (max(x) - min(x)))
}


# 사람 & 실험번호 & 활동별 특징 추출
HAR_total <- data.frame()
fls ; length(fls)

i <- 0
for (f in fls){
  temp <- get(f)
  
  print(paste0("진행 단계 : ", round(i / length(fls) * 100,2), ' %')) ; i <- i+1
  HAR_total <- rbind(HAR_total, 
                     temp %>% mutate(exp_no = unlist(regmatches(f, gregexpr("[[:digit:]]+",f)[1]))[1],
                                     id = unlist(regmatches(f,gregexpr("[[:digit:]]+",f)[1]))[2],
                                     activity = unlist(str_split(f,"\\_"))[1])) 
}

unique(HAR_total$activity) ; nrow(HAR_total) ; head(HAR_total , 5)

# 사용자 정의 함수 적용
HAR_total <- mag(HAR_total, "userAcceleration")
HAR_total <- mag(HAR_total, "rotationRate")

# 최종 기초 전처리 데이터 완료 
head(HAR_total) ; dim(HAR_total)


# ======================================================================================= #
### 3. 분석용 데이터 셋 전처리
# 1. 모든 변수에 대한 기술통계량 적용
HAR_summary_extend <- HAR_total %>%  
  group_by(id, exp_no, activity) %>% 
  summarize_at(.vars = c('attitude.roll', 'attitude.pitch', 'attitude.yaw', 'gravity.x','gravity.y','gravity.z',
                         "rotationRate.x",'rotationRate.y','rotationRate.z',
                         'userAcceleration.x','userAcceleration.y','userAcceleration.z', 'magrotationRate', 'maguserAcceleration'), 
               .funs = c(mad, median, mean, min, max, sd, skewness, rms, rss, IQR, e1071::kurtosis))

## 최종 전처리 데이터 완료 ##
head(HAR_summary_extend) ; dim(HAR_summary_extend)


# 2. mag 변수에 대한 피크(Peak) 변수
for (d in fls){
  f <- get(d)
  f <- mag(f, 'rotationRate')
  f <- mag(f, 'userAcceleration')
  assign(d,f)
}


Peak_rslt <- data.frame() ; Peak_rslt2 <- data.frame() ; i <- 0
for (d in fls) {
  f <- get(d)
  print(paste0("진행 단계 : ", round(i / length(fls) * 100,2), ' %')) ; i <- i+1
  p <- findpeaks(f$magrotationRate, threshold = 4)
  Peak_rslt <- rbind(Peak_rslt,
                     data.frame(d, 
                                f_n = ifelse(!is.null(p), dim(p)[1],0),
                                p_interval = ifelse(!is.null(p), ifelse(dim(p)[1]>2, mean(diff(p[,2])),0),0),
                                p_interval_std = ifelse(!is.null(p), ifelse(dim(p)[1]>2, std(diff(p[,2])),0),0),
                                p_mean = ifelse(!is.null(p), mean(p[,1]),0),
                                p_max = ifelse(!is.null(p), max(p[,1]),0),
                                p_min = ifelse(!is.null(p), min(p[,1]),0),
                                p_mstd = ifelse(!is.null(p), ifelse(dim(p)[1]>2, std(diff(p[,1])),0),0))) 
  
  p <- findpeaks(f$maguserAcceleration, threshold = 4)
  Peak_rslt2 <- rbind(Peak_rslt2, 
                     data.frame(d, 
                                f_n = ifelse(!is.null(p), dim(p)[1],0),
                                p_interval = ifelse(!is.null(p), ifelse(dim(p)[1]>2, mean(diff(p[,2])),0),0),
                                p_interval_std = ifelse(!is.null(p), ifelse(dim(p)[1]>2, std(diff(p[,2])),0),0),
                                p_mean = ifelse(!is.null(p), mean(p[,1]),0),
                                p_max = ifelse(!is.null(p), max(p[,1]),0),
                                p_min = ifelse(!is.null(p), min(p[,1]),0),
                                p_mstd = ifelse(!is.null(p), ifelse(dim(p)[1]>2, std(diff(p[,1])),0),0))) 
}

Peak_rslt <- merge(Peak_rslt, Peak_rslt2, by ='d')

## 최종 전처리 데이터 완료 ##
head(Peak_rslt) ; dim(Peak_rslt)



# 3. 모든 변수에 대한 파고율 변수
crest_ratio <- data.frame() ; i <- 0
vector_collet <- c('attitude.roll', 'attitude.pitch', 'attitude.yaw', 'gravity.x','gravity.y','gravity.z',
                   "rotationRate.x",'rotationRate.y','rotationRate.z',
                   'userAcceleration.x','userAcceleration.y','userAcceleration.z','magrotationRate','maguserAcceleration')
for (d in fls){
  f <- get(d)
  f <- mag(f, 'rotationRate')
  f <- mag(f, 'userAcceleration')
  assign(d,f)
}
for (d in fls){

  print(paste0("진행 단계 : ", round(i / length(fls) * 100,2), ' %')) ; i <- i+1

  f <- get(d)
  f <- f %>% select(vector_collet)
  cf1 <- crest(f$attitude.roll, 50)
  cf2 <- crest(f$attitude.pitch, 50)
  cf3 <- crest(f$attitude.yaw, 50)
  cf4 <- crest(f$gravity.x, 50)
  cf5 <- crest(f$gravity.y, 50)
  cf6 <- crest(f$gravity.z, 50)
  cf7 <- crest(f$rotationRate.x, 50)
  cf8 <- crest(f$rotationRate.y, 50)
  cf9 <- crest(f$rotationRate.z, 50)
  cf10 <- crest(f$userAcceleration.x, 50)
  cf11<- crest(f$userAcceleration.y, 50)
  cf12 <- crest(f$userAcceleration.z, 50)
  
  cfR <- crest(f$magrotationRate, 50)
  cfA <- crest(f$maguserAcceleration, 50)
  crest_ratio <- rbind(crest_ratio, data.frame(d, cf1 = cf1$C, cf2 = cf2$C, cf3 = cf3$C, cf4 = cf4$C, cf5 = cf5$C, cf6 = cf6$C, 
                                               cf7 = cf7$C, cf8 = cf8$C, cf9 = cf9$C, cf10 = cf10$C, cf11 = cf11$C, cf12 = cf12$C,
                                               cfR = cfR$C, cfA=cfA$C))
}

## 최종 전처리 데이터 완료 ##
head(crest_ratio) ; dim(crest_ratio)




# 4. mag 변수에 대한 변화분석(Change Point) 변수
ch_pt <- data.frame() ; i<- 0

for(d in fls){
  f <- get(d)
  
  f <- mag(f,'rotationRate')
  f <- mag(f,'userAcceleration')
  
  print(paste0("진행 단계 : ", round(i / length(fls) * 100,2), ' %')) ; i <- i+1
  
  rslt <- sapply(f %>% select(magrotationRate,maguserAcceleration), cpt.mean)
  rslt_cpts1 <- cpts(rslt$magrotationRate)
  rslt_cpts2 <- cpts(rslt$maguserAcceleration)
  
  rslt2 <- sapply(f %>% select(magrotationRate,maguserAcceleration), cpt.var)
  rslt2_cpts1 <- cpts(rslt2$magrotationRate)
  rslt2_cpts2 <- cpts(rslt2$maguserAcceleration)
  
  rslt3 <- sapply(f %>% select(magrotationRate,maguserAcceleration), cpt.meanvar)
  rslt3_cpts1 <- cpts(rslt3$magrotationRate)
  rslt3_cpts2 <- cpts(rslt3$maguserAcceleration)
  
  
  ch_pt <- rbind(ch_pt, data.frame(d, cp1=length(rslt_cpts1),
                                   cp2=length(rslt_cpts2),
                                   cp3=length(rslt2_cpts1),
                                   cp4=length(rslt2_cpts2),
                                   cp5=length(rslt3_cpts1),
                                   cp6=length(rslt3_cpts2) ))
}

# exp_no, id, activity 추가
temp <- data.frame()
for (i in 1:nrow(ch_pt)){
  temp <- rbind(temp, id_f(ch_pt$d[i]))
}
ch_pt <- cbind(ch_pt, temp)

## 최종 전처리 데이터 완료 ##
head(ch_pt) ; dim(ch_pt)





# ======================================================================================= #
### 4. 최종 데이터 병합 및 예측모델 비교 분석
# 1. 데이터 병합
dim(HAR_summary_extend) ; dim(Peak_rslt) ; dim(crest_ratio) ; dim(ch_pt)

Peak_final <- merge(Peak_rslt, crest_ratio, by = "d")
peak_crest_chpt <- merge(Peak_final, ch_pt, by='d')

final_predict_data <- merge(peak_crest_chpt, HAR_summary_extend, by = c('exp_no','id','activity'))

# 2. 변수 선택
training <- final_predict_data %>% ungroup %>%  select(-d, -exp_no, -id)
colnames(training) ; dim(training)





# 3. 모델 구축 및 평가 비교

# J48 96.39%
J48_model <- J48(as.factor(activity)~., data = training)
(e_J48_model <- evaluate_Weka_classifier(J48_model, numFolds = 10, complexity = T, class = T))

# Logistic Regression :: 85%
Logis_model <- Logistic(as.factor(activity)~., data = training)
(e_Logis_model <- evaluate_Weka_classifier(Logis_model, numFolds = 10, complexity = T, class = T))

# MLE(정규화 포함) 96.94%
model_data_set_norm <- as.data.frame(lapply(training[,-1], normalize ))
training[,-1]
colnames(model_data_set_norm)
model_data_set_norm$activity <- training[,c('activity')]

MLP <- make_Weka_classifier('weka/classifiers/functions/MultilayerPerceptron')
MLP_model <- MLP(as.factor(activity)~., data = model_data_set_norm)
(e_MLP <- evaluate_Weka_classifier(BoW_MLP_m, numFolds = 10, complexity = T, class = T))

# Random Forest 98.89%
RF_model <- RF(as.factor(activity)~., data = training)
(e_RF_model <- evaluate_Weka_classifier(RF_model, numFolds = 10, complexity = T, class = T))



save.image("HAR_Assignment.RData")


# 데이터 추출 저장
getwd()
write.table(prac_merge_data, file="../Dataset_for_prediction/01.기술통계량.csv", sep=",", append = FALSE,row.names = FALSE, col.names = TRUE)

for (i in 1:100){
  print(paste0(i, '번째 시도'))
  e <- evaluate_Weka_classifier(RF_model, numFolds = 10, complexity = T, class = T)
  
  if (e$details[1] > 98.5) {
    print(e)
    break()
  }
  else {print('실패하여 돌아갑니다.')}
}


'
현재까지 dwn과 ups를 잘 구분짓지 못하는 것 같음. 따라서 이에 대한 EDA를 실시 후 어떤 변수들이
특징이 있는지 확인 후 전처리 후 데이터 셋 형성을 해야할 것 ^^

# 3/31
모든 변수에 대한 기술통계량 O
피크 통계량 and 피크 threshold 최적값 탐색 -> EDA 과정 필요
파고율 값
심심해서 원본 데이터에 기술통계량만 했는데 98%가 나온다.
★문서정리, 최적의 피크값 2개 찾아보고, 파이썬으로 예측모델 만들어보기.

# 4/3
Peak_final2(피크통계, 파고율, 변화분석까지 넣었을 때)
98.33%까지 나왔음. 근데 변화분석 넣으니까 성능이 더 떨어지기도 했음.


# https://www.kaggle.com/malekzadeh/motionsense-dataset/code
