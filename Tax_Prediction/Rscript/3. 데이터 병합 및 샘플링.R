# ======================================================================================= #
## Script id   : 3.데이터 병합 및 샘플링.R
## Script Name : 데이터 병합 및 샘플링
'## Input       : C:/Bigdata_project_tax/02. Result'
#   1) 10월 발급 내역 
#   C:/Bigdata_project_tax/02. Result/01.10월_발송내역_정제완료.csv

#   2) 10월 수납 내역
#   C:/Bigdata_project_tax/02. Result/01.10월_수납내역_정제완료.csv

'## Output      : C:/Bigdata_project_tax/02. Result'
#   1) 클래스 불균형 (불량 : 119,617 건,  우량 : 8,113 건)
#   C:/Bigdata_project_tax/02. Result/03.유형분석_데이터.csv

#   2) 클래스 불균형 해결 (불량 : 119,618 건, 우량 : 119,662 건)
#   C:/Bigdata_project_tax/02. Result/03.모형분석_데이터.csv

## Date        : 2020.12.31
# ======================================================================================= #
## 1) 초기화
rm(list = ls()); gc();

## 2)작업 디렉토리 경로 지정
## 사용자 작업 디렉토리 경로 확인
getwd()
## 사용자 작업 디렉토리 경로 변경 (편의상 C 드라이브를 경로로 지정)
setwd("C:/Bigdata_project_tax/02. Result")

## 3)패키지 설치 및 패키지 불러오기
library(data.table)
library(tidyverse)
library(caret)
library(ROSE)

# --------------------------------------------------------------------------------------- #
## 1) 데이터 파일 로드

# 1) 10월 발급 내역
tax_chenap <- read.csv("01.10월_발송내역_정제완료.csv")

# 2) 10월 수납 내역
tax_soonap <- read.csv("01.10월_수납내역_정제완료.csv")

# --------------------------------------------------------------------------------------- #
## 2) 데이터 병합

# 1) 사용컬럼 정의 및 우불량 부여
chenap_merge <- tax_chenap[,c(2,4,5,8:11,13:18)]
soonap_merge <- tax_soonap[,c(2,4,5,8:11,13:18)]
colnames(soonap_merge)[1] <- '체납액'

chenap_merge$여부 <- as.factor(0) # 체납자 불량 0
soonap_merge$여부 <- as.factor(1) # 수납자 우량 1

# 2) 체납자 수납자 요약
summary(chenap_merge) ; summary(soonap_merge)

# 3) 데이터 최종 병합
model_data <- rbind(chenap_merge,soonap_merge)
dim(model_data) ; table(model_data$여부)


## 3) SMOTE를 이용한 클래스 불균형 문제해결 
# 1) 데이터 불균형 확인
table(model_data$여부) # 93.6% : 6.3% 비율으로 클래스 불균형 발생 -> 향후 수납자의 더 많은 데이터 필요
prop.table(table(model_data$여부))

head(model_data,2) ; dim(model_data) ; str(model_data) ; colnames(model_data)

# 2) SMOTE의 over sampling
data_balanced_over <- ovun.sample(여부~., data = model_data, method = "over")$data
table(data_balanced_over$여부) ; prop.table(table(data_balanced_over$여부))

## 4) 이상치 변경
summary(data_balanced_over$체납액) # 체납액
data_balanced_over <- data_balanced_over[data_balanced_over$체납액 >= 10000,]
data_balanced_over$체납액 <- ifelse(data_balanced_over$체납액 >= 10000000, 10000000, data_balanced_over$체납액)
summary(data_balanced_over$체납액)

rownames(data_balanced_over) = NULL


## 3) 데이터 저장
# 1) 클래스 불균형 해결 (불량 : 119,618 건, 우량 : 119,662 건)
write.table(data_balanced_over, file="../02. Result/03.모형분석_데이터.csv", sep=",", append = FALSE,row.names = FALSE, col.names = TRUE)

# 2) 클래스 불균형 (불량 : 119,617 건, 우량 : 8,113 건)
write.table(model_data, file="../02. Result/03.유형분석_데이터.csv", sep=",", append = FALSE,row.names = FALSE, col.names = TRUE)


