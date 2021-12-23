# ======================================================================================= #
## Script id   : 5.결과 해석.R
## Script Name : 결과 해석
'## Input       : C:/Bigdata_project_tax/02. Result'
#   1) 클래스 불균형 (불량 : 119,617 건,  우량 : 8,113 건)
#   C:/Bigdata_project_tax/02. Result/03.유형분석_데이터.csv

#   2) 클래스 불균형 해결 (불량 : 119,618 건, 우량 : 119,662 건)
#   C:/Bigdata_project_tax/02. Result/03.모형분석_데이터.csv

'## Output      : C:/Bigdata_project_tax/02. Result'

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
library(caret)             # Confusion Matrix 패키지
library(rpart)
library(e1071)
library(devtools)
library(rattle)				# Fancy tree plot
library(rpart.plot)			# Enhanced tree plots
library(RColorBrewer)			# Color selection for fancy tree 
# --------------------------------------------------------------------------------------- #
## 1) 데이터 파일 로드
# 1) 클래스 불균형 해결 (불량 : 119,618 건, 우량 : 119,662 건)
up_Decision_tree_data <- read.csv("03.모형분석_데이터.csv")

head(up_Decision_tree_data)
# --------------------------------------------------------------------------------------- #
## 2) 사용변수 설정
colnames(up_Decision_tree_data)
Decision_tree_data <- up_Decision_tree_data[c('체납회수등급','체납기간','SP등급','체납건수','체납액', '여부')]
head(Decision_tree_data)

## 3) Input 데이터 확인
table(Decision_tree_data$여부)
Decision_tree_data$여부 <- as.factor(Decision_tree_data$여부)

## 4) 최적 CP값 탐색
set.seed(7640)
rpart_m <- rpart(여부~., data = Decision_tree_data, control = rpart.control(cp=0.0001))
rpart_m$cptable
plotcp(rpart_m) # 0.003646446 cp값 선택

## 5) 모델 형성
set.seed(7640)
rpart_model <- rpart(여부~., data = Decision_tree_data, control = rpart.control(cp=0.003646446)) 
rpart_model$cptable
plotcp(rpart_model)

plot(rpart_model, compress = T, margin = 0.5)
text(rpart_model, cex = 1)

## 6) 그래프 출력
fancyRpartPlot(rpart_model, type = 5)


## 7) 결과 해석 
'
7번 노드로 분류된 대상자는 전체의 35%로, 체납회수등급이 3.5%보다 작으며
체납건수가 2.5건보다 작은 대상자들로 구성된
이 그룹은 66%의 확률로 수납자에 속한다는 해석을 도출.


####
Node의 성질 : Yes or No 분류
node의 순도 : 0.1는 No, 0.9는 Yes에 속한다는 의미, 색상의 진하기로 노드의 순수함 표시
node가 전체에서 차지하는 비중 : 아래 숫자는 전체 29% 관측치

해석 :: 
종합적으로 해석해 보면 7번 node로 분류된 환자는 전체의 29%로, Thal 테스트가 Nornal이 아니면서, 
Oldpeak 관측치가 0.7보다 작지 않은 환자들로 구성된 
이 그룹은 90%의 확률로 심장병 양성에 속한다는 해석을 할 수 있습니다. 
'




#### 기대효과 ####
OOOO_class <- read.csv("00.OOOO등급_기초.csv")
head(OOOO_class)
dim(OOOO_class)
library(dplyr)
str(OOOO_class$과세년월)
a %>% group_by(체납회수등급) %>% summarise(건수 = n(), 체납합계 = sum(체납액))
# 체납회수등급 1~3 등급 , 4~7등급, 8~10등급 구분
a <- OOOO_class[OOOO_class$체납액 >= 100000 & OOOO_class$과세년월 <= 202008, ]

