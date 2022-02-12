# ======================================================================================= #
## Script id   : 0.기초전처리.R
## Script Name : 데이터 기초 전처리
'## Input        : C:/R-sch/final_data'
'## Output       : NA'

## Summary 
# 1) 데이터 로드(Data Load)
'해당 데이터는 리스트 형태로 되어있어 이를 전처리하기 위해 데이터프레임으로 만드는 작업을 시행하였습니다.'

# 2) 데이터 전처리(Preprocessing)
'데이터 전처리는 혈압 데이터에 대하여 11가지의 통계 특질, 피크(Peak), 
파고율(crest), 변화 분석(Change point)을 적용하였습니다.

다음으로 class의 balance를 확인한 결과, 정상(0) 98.8%, 비정상(1) 1.2%으로 
데이터 불균형(Imbalanced class)를 확인하였습니다. 이에 따라 SMOTE의 Oversampling을 통해 
class 비율을 5:5로 맞추었습니다.'

# 3) 모델링(Modeling)
'사용한 모델은 총 3가지로 Random Forest, Logistic Regression, Multi-Layer perceptron을 사용하여 
모델의 성능을 비교하였습니다. 최종 예측 모델은 Random Forest으로 통계, 피크, 파고율, 변화 분석을 적용한 
22개 변수를 사용하였고, 성능은 10-fold 기준  99.90%으로 확인하였습니다.'

## Date        : 2021.6.15
# ======================================================================================= #
### 0. 라이브러리 및 환경 설정
# Sys.getenv("R_LIBS")
# .libPaths("C:/Program Files/R/R-4.0.1/library")
# .libPaths()

rm(list = ls()); gc();

library(pracma) ; library(signal) ; library(seewave) ; library(changepoint)
library(e1071)      # kurtosis을 위한 패키지
library(tidyverse)  # 데이터 전처리를 위한 패키지
library(ROSE)       # oversampling을 위한 SMOTE 패키지
library(corrplot)   # 상관계수 확인을 위한 패키지 

# Sys.setenv(JAVA_HOME = "C:/Program Files/Java/jre1.8.0_241") 
library(RWeka) ; library(rJava)

setwd('C:/R-sch/final_data')
# save.image("assignment_3.RData")
load("assignment_3.RData")
# ======================================================================================= #
### 1. 데이터 로드(Data Load)
## 1) 데이터 목록 리스트
d <- setwd('C:/R-sch/final_data')  
fls <- dir(d, recursive = TRUE)
flist <- fls[str_detect(fls,'.csv')] ; length(flist)


## 2) 데이터 업로드 및 시그널 전처리
SRATE <- 250 ; MINUTES_AHEAD <- 1 ; c_num <- 1
Data_set<-list() # 샘플 생성후 저장할 공간

for (file in flist){
  a <- file.path(str_c(d,"/",file))
  temp <- read.csv(a)
  read_data <- data.frame(IBP = temp[c(-1,-2),c('signal')])
  
  # 요소형 방지 및 문자열 전처리
  IBP <- as.numeric(str_trim(str_split(read_data$IBP, ',', simplify = TRUE)[,2]))
  i <- 1
  IBP_data<-data.frame()
  
  # 반복문을 통해 추가 작업
  while (i < length(IBP) - SRATE*(1+1+MINUTES_AHEAD)*60){
    segx <- IBP[i:(i+SRATE*1*60-1)]
    segy <- IBP[(i+SRATE*(1+MINUTES_AHEAD)*60):(i+SRATE*(1+1+MINUTES_AHEAD)*60-1)]
    segxd <- IBP[i:(i+SRATE*(1+MINUTES_AHEAD)*60-1)]
    if(is.na(mean(segx)) |
       is.na(mean(segy)) |
       max(segx)>200 | min(segx)<20 |
       max(segy)>200 | max(segy)<20 |
       max(segx) - min(segx) < 30 |
       max(segy) - min(segy) < 30|(min(segxd,na.rm=T) <= 50)){
    }
    else{ #나머지의 경우
      # segy <- ma(segy, 2*SRATE)
      event <- ifelse(min(segy,na.rm=T) <= 50, 1, 0)
      print(paste0("이벤트 : ", event, " || 진행 단계 : ", round(c_num / length(flist) * 100,2), ' %'))
      IBP_data<- rbind(IBP_data, cbind(t(segx), event))
    }
    
    i <- i+1*60*SRATE
  }
  Data_set[[file]]<- IBP_data
  c_num <- c_num+1
}


## 데이터 확인 ##
class(Data_set)


# ======================================================================================= #
### 2. 데이터 전처리(Preprocessing)
## 1) 기술통계량 적용
rss <- function(x) rms(x)*(length(x))^0.5
total_data <- data.frame() ; step_processing <- data.frame()

for (i in (1:length(Data_set))){
  # 데이터 프레임 형태 및 분리
  X_data <- data.frame(t(Data_set[[i]]))[1:length(Data_set[[i]])-1,]
  Y_data <- t(Data_set[[i]])[length(Data_set[[i]]),]
  cat(i,'번쨰를 실행합니다.')
  
  # 통계특질 반영 전처리
  stat_data <- data.frame(mad_col = sapply(X_data, mad),
                          median_col = sapply(X_data, median),
                          mean_col = sapply(X_data, mean),
                          min_col = sapply(X_data, min),
                          max_col = sapply(X_data, max),
                          sd_col = sapply(X_data, sd),
                          skewness_col = sapply(X_data, skewness),
                          rms_col = sapply(X_data, rms),
                          rss_col = sapply(X_data, rss),
                          IQR_col = sapply(X_data, IQR),
                          kurtosi_col = sapply(X_data, e1071::kurtosis))
  
  step_processing <- cbind(stat_data, Y_data)
  
  total_data <- rbind(total_data, step_processing)
  print('끝')
}

## 확인 ##
dim(total_data) ; head(total_data)


## 2) 피크(peak) 적용
peak_rslt <- data.frame()

for (i in (1:length(Data_set))){
  X_data <- data.frame(t(Data_set[[i]]))[1:length(Data_set[[i]])-1,]

  for (j in (1:ncol(X_data))){
    print(paste0(i,'번쨰 중 ', j, '열 실행'))
    p_t <- findpeaks(X_data[,j], threshold = 4) # i번째 j 열 
    
    f_n <- ifelse(!is.null(p_t), dim(p_t)[1],0)
    p_interval <- ifelse(!is.null(p_t), ifelse(dim(p_t)[1]>2, mean(diff(p_t[,2])),0),0)
    p_interval_std <- ifelse(!is.null(p_t), ifelse(dim(p_t)[1]>2, std(diff(p_t[,2])),0),0)
    p_mean <- ifelse(!is.null(p_t), mean(p_t[,1]),0)
    p_max <- ifelse(!is.null(p_t), max(p_t[,1]),0)
    p_min <- ifelse(!is.null(p_t), min(p_t[,1]),0)
    p_mstd <- ifelse(!is.null(p_t), ifelse(dim(p_t)[1]>2, std(diff(p_t[,1])),0),0)
  
    temp <- data.frame(f_n, p_interval, p_interval_std, p_mean, p_max, p_min, p_mstd)
    peak_rslt <- rbind(peak_rslt, temp)  
  }
}

dim(peak_rslt) ; head(peak_rslt) 

# 데이터 결합
merge_data <- cbind(peak_rslt, total_data)

## 확인 ##
dim(merge_data) ; head(merge_data)


## 3) 파고율(Crest) 적용
crest_rslt <- c()

for (i in (1:length(Data_set))){
  X_data <- data.frame(t(Data_set[[i]]))[1:length(Data_set[[i]])-1,]
  for (j in (1:ncol(X_data))){
    print(paste0(i,'번쨰 중 ', j, '열 실행'))
    crest_rslt <- append(crest_rslt, crest(X_data[,j], 50)$C)
  }
}
crest_ratio <- data.frame(crest_ratio = crest_rslt)
dim(crest_ratio) ; head(crest_ratio) 

# 데이터 결합 
merge_data2 <- cbind(crest_ratio, merge_data)

## 확인 ##
dim(merge_data2) ; head(merge_data2)


## 4) 변화분석(Change point) 적용
ch_rslt <- c()
for (i in (1:length(Data_set))){
  X_data <- data.frame(t(Data_set[[i]]))[1:length(Data_set[[i]])-1,]
  
  for (j in (1:ncol(X_data))){
    print(paste0(i,'번쨰 중 ', j, '열 실행'))
    cpt_mean <- cpts(cpt.mean(X_data[,j]))
    cpt_var <- cpts(cpt.var(X_data[,j]))
    cpt_meanvar <- cpts(cpt.meanvar(X_data[,j]))
    
    ch_rslt <- rbind(ch_rslt, data.frame(cp1=length(cpt_mean), cp2=length(cpt_var), cp3=length(cpt_meanvar)))
  }
}
dim(ch_rslt) ; head(ch_rslt) 

# 데이터 결합
merge_data3 <- cbind(ch_rslt, merge_data2)

## 확인 ##
dim(merge_data3) ; head(merge_data3)





## 5) Imbalanced class event check and Over Sampling by SMOTE 
# 데이터 불균형 확인
nrow(merge_data3[merge_data3$Y_data == 1,])
table(merge_data3$Y_data)
prop.table(table(merge_data3$Y_data))*100

# 오버샘플링 적용
N_data <- table(merge_data3$Y_data)[1] * 2
data_balanced_over <- ovun.sample(Y_data~., data = merge_data3, method = "over", N=N_data)$data

# 결과 확인
table(data_balanced_over$Y_data)
prop.table(table(data_balanced_over$Y_data))*100

## 최종 확인 ##
dim(data_balanced_over) ; head(data_balanced_over)

# 6) 상관계수 확인 -> 적용하였으나, 오히려 성능이 떨어져 사용하지 않았습니다. 
# col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
# cor_class <- function(x){ 
#   corrplot(cor(x), method = "color", col = col(200), type = "lower", order = "hclust",       
#            addCoef.col = "black", tl.col = "black", tl.srt = 45, diag = F)               
# }
# 
# cor_class(total_data[,-12]) # mad_col-sd_col, IQR-sd, IQR-mad, median-mean, median-rms, median-rss
# model_set <- data_balanced_over[,c("mean_col", "min_col",
#                                    "max_col", "skewness_col",
#                                    "IQR_col", "kurtosi_col",
#                                    "Y_data")] # mad, sd, median 제거
# cor_class(model_set)
# dim(model_set) ; head(model_set)


# ======================================================================================= #
### 4. 최종 데이터 병합 및 예측모델 비교 분석
# 1) Random Forest Model 99.8985%
RF <- make_Weka_classifier("weka/classifiers/trees/RandomForest") 
RF_model <- RF(as.factor(Y_data)~., data = data_balanced_over)
(e_RF_model <- evaluate_Weka_classifier(RF_model, numFolds = 10, complexity = T, class = T))

# 2) Logistic Regression Model 86.0223%
Logis_model <- Logistic(as.factor(Y_data)~., data = data_balanced_over)
(e_Logis_model <- evaluate_Weka_classifier(Logis_model, numFolds = 10, complexity = T, class = T))

# 3) Multi Layer Perceptron Model 97.7296 %
minmax_scale <- function(x) { return ((x - min(x)) / (max(x) - min(x)))}

model_data_set_norm <- as.data.frame(sapply(data_balanced_over[,-23], minmax_scale))
model_data_set_norm$Y_data <- data_balanced_over[,c('Y_data')]
model_data_set_norm$cp1 <- 1  ; model_data_set_norm$cp3 <- 1 # scale 적용 시 NA발생.
dim(model_data_set_norm) ; head(model_data_set_norm)

MLP <- make_Weka_classifier('weka/classifiers/functions/MultilayerPerceptron')
MLP_model <- MLP(as.factor(Y_data)~., data = model_data_set_norm)
(e_MLP <- evaluate_Weka_classifier(MLP_model, numFolds = 10, complexity = T, class = T))

## 성능 정리 ##
'
|--------------------------------------------------------------------------------------------|
|# Random Forest  |   # Logistic Regression   |    # MLP   |                                 |
|    99.8097%     |         81.0000%          |  91.0578%  | (통계 적용)                     |
|    99.8351%     |         85.1218%          |  95.8397%  | (통계+피크 적용)                |
|    99.8478%     |         85.9209%          |  97.2730%  | (통계+피크+파고율 적용)         |
|    99.8985%     |         86.0223%          |  97.7296%  | (통계+피크+파고율+변화분석 적용)|
|--------------------------------------------------------------------------------------------|
|    99.7971%     |         84.6525%          (통계+상관성 제거)                             |
|    99.6322%     |         84.3100%          (통계+피크+상관성 제거)                        |
|--------------------------------------------------------------------------------------------|
'



