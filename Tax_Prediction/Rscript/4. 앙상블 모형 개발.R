# ======================================================================================= #
## Script id   : 4.모형 개발.R
## Script Name : 모형 개발
'## Input       : C:/Bigdata_project_tax/02. Result'
#   1) 클래스 불균형 해결 (불량 : 119,618 건, 우량 : 119,662 건)
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
library(woe)               # IV 및 WOE 등의 통계수치 산출 패키지
library(sqldf)             # 전용 SQL 언어 패키지
library(corrplot)          # 상관계수 및 그래프 산출 패키지
library(ROCR)              # ROC Curve 산출 패키지
library(nnet)              # Neural Network (신경망) 패키지
library(NeuralNetTools)    # Neural Network 시각화 패키지
library(randomForest)      # Random Forest (랜덤 포레스트) 패키지
library(DT)                # 데이터 테이블 패키지
library(knitr)             # LaTeX, LyX, HTML, Markdown 등 결과/코드 정리 패키지
library(caret)             # Confusion Matrix 패키지


# --------------------------------------------------------------------------------------- #
## 1) 데이터 파일 로드
# 1) 클래스 불균형 해결 (불량 : 119,618 건, 우량 : 119,662 건)
newData_Balance <- read.csv("03.모형분석_데이터.csv")

# --------------------------------------------------------------------------------------- #
## 2) 변수 선택 과정
# 1) Information Value를 통한 변수 선택
iv.mult(newData_Balance,"여부",TRUE)
iv.plot.summary(iv.mult(newData_Balance,"여부",TRUE))
'0.10 ~ 15 이상 변수로서 의미가 있음'

# 2) 각 변수 WOE 생성
newData_balance_woe <- iv.replace.woe(newData_Balance,iv=iv.mult(newData_Balance,"여부"))
dim(newData_balance_woe) ; dim(newData_Balance)

DT::datatable(newData_balance_woe) # WOE 생성확인


# 3) Logistic Regression Stepwise
colnames(newData_balance_woe)
newData_balance_woe$여부 <- as.factor(newData_balance_woe$여부)

# 모델 형성
logit_model <- glm(여부 ~체납회수등급_woe + 
                       체납기간_woe +
                       SP등급_woe + 
                       체납건수_woe +
                       CB점수_woe +
                       체납액_woe +
                       전체가산_woe +
                       등급별가산_woe , data=newData_balance_woe, family=binomial())

# Stepwise
model_1<-step(logit_model)
summary(model_1)

# P-value 0.05 기준 최종선정 변수확인
'
Coefficients:
                  Estimate Std. Error z value Pr(>|z|)    
(Intercept)      -0.004198   0.004318  -0.972   0.3310    
체납회수등급_woe -0.607236   0.013861 -43.808   <2e-16 ***
체납기간_woe     -0.535281   0.012451 -42.992   <2e-16 ***
SP등급_woe       -0.424876   0.013934 -30.491   <2e-16 ***
체납건수_woe     -0.355609   0.019706 -18.046   <2e-16 ***
체납액_woe       -0.409431   0.016892 -24.237   <2e-16 ***
전체가산_woe      0.029179   0.014972   1.949   0.0513 .  
등급별가산_woe    0.191674   0.015747  12.172   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
'
# --------------------------------------------------------------------------------------- #
## 3) 데이터 정규화 및 분할
# 1) 변수 선택
model_data_set <- sqldf("select 여부, 체납액_woe, 체납회수등급_woe, SP등급_woe, 체납건수_woe, 체납기간_woe, 등급별가산_woe from newData_balance_woe")

model_data_set$여부 <- as.factor(model_data_set$여부)

# 2) 정규화
normalize <- function(x) { 
  return ((x - min(x)) / (max(x) - min(x)))
}

model_data_set_norm <- as.data.frame(lapply(model_data_set[,2:7], normalize ))

model_data_set_norm$여부 <- as.factor(ifelse(model_data_set$여부 == 1, 1, 0))

# 3) 최종 데이터 분할
set.seed(1234)
train_idx <- sample(1:nrow(model_data_set_norm), size=0.7*nrow(model_data_set_norm), replace=F)
train_set <- model_data_set_norm[train_idx, ]
test_set <- model_data_set_norm[-train_idx, ]

table(train_set$여부) ; table(test_set$여부)
# --------------------------------------------------------------------------------------- #
## 4.1) Model 1 : Logistic Regression
# 1) 모델 구축
model1 <- glm(여부 ~., data=train_set, family=binomial())
summary(model1)
library(car)
vif(model1)
# 2) confusion Matrix 비교
rf_p1 <- predict(model1, newdata = test_set, type = "response")
confusionMatrix(as.factor(ifelse(rf_p1 < 0.5, 0, 1)), test_set$여부) # 0.5임계값이 가장 높은 값 63.58%

for (i in 1:100){
  a <- confusionMatrix(as.factor(ifelse(rf_p1 < paste0("0.",i), 0, 1)), test_set$여부) # 63.58%
  print(paste0(paste0("0.",i), " thresh hold value : ", round(a$overall[1] * 100,2)," %"))
}


# 3) 상관계수 및 그래프 도출 ##
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
cor_class <- function(x){
  corrplot(cor(x),
           method = "color",       # 색깔로 표현
           col = col(200),         # 색상 200개 선정
           type = "lower",         # 왼쪽 아래 행렬만 표시
           order = "hclust",       # 유사한 상관계수끼리 군집화
           addCoef.col = "black",  # 상관계수 색깔
           tl.col = "black",       # 변수명 색깔
           tl.srt = 45,
           diag = T)               # 대각 행렬 제외
}

sample_corr<-sqldf("select 여부, 체납액_woe, 체납회수등급_woe, 
                        SP등급_woe, 체납건수_woe, 체납기간_woe, 등급별가산_woe from newData_balance_woe")
cor_class(sample_corr[2:7]) #0.8 이상 없는 것으로 판단


# 4) 모형검정(ROC curve, AUROC, Ks, gini계수) ##
# ROC Curve #
# score test data
newData_balance_woe$model1_score<-predict(model1,type='response',newData_balance_woe) # lm모델로 예측점수
model1_pred<-prediction(newData_balance_woe$model1_score,newData_balance_woe$여부)
model1_perf<-performance(model1_pred,"tpr","fpr")

# ROC - Model performance plot
plot(model1_perf,lwd=2,colorize=TRUE,main="ROC Model 1: Logistic Regression Performance")
lines(x=c(0, 1), y=c(0, 1), col="red", lwd=1, lty=3);
lines(x=c(1, 0), y=c(0, 1), col="green", lwd=1, lty=4)

#AUC, KS, Gini 계수#
model1_KS <- round(max(attr(model1_perf,'y.values')[[1]]-attr(model1_perf,'x.values')[[1]])*100, 2)
model1_AUROC <- round(performance(model1_pred, measure = "auc")@y.values[[1]]*100, 2)
model1_Gini <- (2*model1_AUROC - 100)
cat("AUROC: ",model1_AUROC,"\tKS: ", model1_KS, "\tGini:", model1_Gini, "\n")

# AUROC:  68.1 	  KS:  27.61 	  Gini: 36.2 
# AUROC:  68.08 	KS:  27.6   	Gini: 36.16

# 1에 가까울수록, 50이 넘을수록, 60이상일수록 우수한 모형

# --------------------------------------------------------------------------------------- #
## 4.2) Model 2 : Neural Network
# 1) 모델 구축
model2 <- nnet(여부~., data=train_set,size=20, maxit=10000, decay=.001, linout=F, trace = F)
table(test_set$여부,predict(model2,newdata=test_set, type="class"))

plotnet(model2) # 모형 Plot 확인


# 2) confusion Matrix 비교
rf_p2 <- predict(model2, newdata = test_set, type = "class")
confusionMatrix(as.factor(rf_p2), test_set$여부) # 63.71% / 64.54% / 64.39%


# 3) 모형검정(ROC curve, AUROC, Ks, gini계수)
# ROC Curve #
model2_pred <- prediction(predict(model2, newdata=test_set, type="raw"),test_set$여부)
model2_perf <- performance(model2_pred,"tpr","fpr")

# ROC - Model performance plot
plot(model2_perf,lwd=2, colerize=TRUE, main="ROC model2:ROC - Neural Network")
abline(a=0,b=1)

# AUC, KS, Gini 계수#
model2_KS <- round(max(attr(model2_perf,'y.values')[[1]]-attr(model2_perf,'x.values')[[1]])*100, 2)
model2_AUROC <- round(performance(model2_pred, measure = "auc")@y.values[[1]]*100, 2)
model2_Gini <- (2*model2_AUROC - 100)
cat("AUROC: ",model2_AUROC,"\tKS: ", model2_KS, "\tGini:", model2_Gini, "\n")

# AUROC:  69.32 	KS:  29.18  	Gini: 38.64
# AUROC:  69.19 	KS:  29.07  	Gini: 38.38
# --------------------------------------------------------------------------------------- #
## 4.3) Model 3 : Random Forest
# 1) 모델 구축
model3<-randomForest(여부~.,data=train_set, importance=TRUE)
table(test_set$여부,predict(model3,newdata=test_set, type="class"))

m <- randomForest(여부 ~., data=train_set)
importance(m)


importance(model3)
varImpPlot(m)
varImpPlot(model3, main="varImpPlot of iris")


rpart_model <- rpart(여부~., data = Decision_tree_data_test, control = rpart.control(cp=0.0023965945))
rpart_model$cptable

plot(model3, compress = T, margin = 0.5)
text(model3, cex = 1)
prp(model3, type=4, extra=2, digits=3)

# Model Fitting
model3_fitForest<-predict(model3,newdata=test_set,type="prob")[,2]


# 2) confusion Matrix 비교
rf_p3 <- predict(model3, newdata = test_set, type = "Class") # Class
confusionMatrix(rf_p3, test_set$여부) # 64.85%


# 3) 모형검정(ROC curve, AUROC, Ks, gini계수)
model3_pred<-prediction(model3_fitForest,test_set$여부)
model3_perf<-performance(model3_pred,"tpr","fpr")

# ROC - Model performance plot
plot(model3_perf,colorize=TRUE, lwd=2, main = "ROC model3: Random Forest", col = "blue")
lines(x=c(0, 1), y=c(0, 1), col="red", lwd=1, lty=3)
lines(x=c(1, 0), y=c(0, 1), col="green", lwd=1, lty=4)

# 3) AUC, KS, Gini 계수#
model3_AUROC <- round(performance(model3_pred, measure = "auc")@y.values[[1]]*100, 2)
model3_KS <- round(max(attr(model3_perf,'y.values')[[1]] - attr(model3_perf,'x.values')[[1]])*100, 2)
model3_Gini <- (2*model3_AUROC - 100)
cat("AUROC: ",model3_AUROC,"\tKS: ", model3_KS, "\tGini:", model3_Gini, "\n")


# AUROC:  67.59 	KS:  29.81 	Gini: 35.18
# --------------------------------------------------------------------------------------- #
##  5) 각 모형의 성능비교
# 1) 모델 성능비교
plot(model1_perf, col='blue', lty=1, main='ROCs: Model Performance Comparision') # logistic regression
plot(model2_perf, col='orange',lty=3, add=TRUE) # neuralnet
plot(model3_perf, col='red',lty=4, add=TRUE); # random forest
legend(0.45,0.4,
       c('m1:Logistic Regression','m2:Nerual Network','m3:Random Forest'),
       col=c('blue', 'orange', 'red'),
       lwd=3);
lines(c(0,1),c(0,1),col = "gray", lty = 4 ) # random line

# 2) 성능 테이블
models <- c('m1:Logistic Regression','m2:Nerual Network','m3:Random Forest')

# AUCs
models_AUC <- c(model1_AUROC, model2_AUROC, model3_AUROC)

# KS
models_KS <- c(model1_KS, model2_KS, model3_KS)

# Gini
models_Gini <- c(model1_Gini, model2_Gini, model3_Gini)

# Combine AUC and KS
model_performance_metric <- as.data.frame(cbind(models, models_AUC, models_KS, models_Gini))

# Colnames 
colnames(model_performance_metric) <- c("Model", "AUC", "KS", "Gini")

# Display Performance Reports
kable(model_performance_metric, caption ="Comparision of Model Performances")


confusionMatrix(as.factor(ifelse(rf_p1 < 0.5, 0, 1)), test_set$여부) # 63.83%
confusionMatrix(as.factor(rf_p2), test_set$여부) # 64.57%
confusionMatrix(as.factor(rf_p3), test_set$여부) # 64.85%


' 클래스 균형 모델
Table: Comparision of Model Performances

|Model                  |AUC   |KS    |Gini  |
|:----------------------|:-----|:-----|:-----|
|m1:Logistic Regression |68.1  |27.73 |36.2  |
|m2:Nerual Network      |69.38 |29.52 |38.76 |
|m3:Random Forest       |67.79 |29.77 |35.58 |
'

DT::datatable(model_performance_metric)



'과적합이 나오거나, 예측하기엔 변인들이 부족하다. 아울러 우불량 구분보다 회수율을 활용하여 징수활동해야함
따라서 회수등급별 회수율을 통해 선택적 고지를 통해 징수 효율성을 높여할 것'



######################################################################################
library(ROSE)
train <- read.csv("03.유형분석_데이터.csv")
train
head(train) ; dim(train)
table(train$여부)

N_data <- table(train$여부)[1] * 2
data_balanced_over <- ovun.sample(여부~., data = train, method = "over", N=N_data)$data
head(data_balanced_over) ; dim(data_balanced_over)

# 1) Random Forest Model 99.8985%
RF <- make_Weka_classifier("weka/classifiers/trees/RandomForest") 
RF_model <- RF(as.factor(여부)~., data = data_balanced_over)
(e_RF_model <- evaluate_Weka_classifier(RF_model, numFolds = 10, complexity = T, class = T))

# 2) Logistic Regression Model 86.0223%
Logis_model <- Logistic(as.factor(여부)~., data = data_balanced_over)
(e_Logis_model <- evaluate_Weka_classifier(Logis_model, numFolds = 10, complexity = T, class = T))

# 3) Multi Layer Perceptron Model 97.7296 %
minmax_scale <- function(x) { return ((x - min(x)) / (max(x) - min(x)))}

model_data_set_norm <- as.data.frame(sapply(data_balanced_over[,-14], minmax_scale))
model_data_set_norm$Y_data <- data_balanced_over[,c('여부')]
dim(model_data_set_norm) ; head(model_data_set_norm)

MLP <- make_Weka_classifier('weka/classifiers/functions/MultilayerPerceptron')
MLP_model <- MLP(as.factor(Y_data)~., data = model_data_set_norm)
(e_MLP <- evaluate_Weka_classifier(MLP_model, numFolds = 10, complexity = T, class = T))
