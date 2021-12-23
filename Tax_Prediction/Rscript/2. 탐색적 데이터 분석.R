# ======================================================================================= #
## Script id   : 2. 탐색적 데이터 분석.R
## Script Name : 탐색적 데이터 분석
'## Input       : C:/Bigdata_project_tax/02. Result'
#   1) OO광역시청 OOOO 신용등급
#   C:/Bigdata_project_tax/02. Result/01.OOOO등급_정제완료.csv

#   2) 10월 발급 내역 
#   C:/Bigdata_project_tax/02. Result/01.10월_발송내역_정제완료.csv

#   3) 10월 수납 내역
#   C:/Bigdata_project_tax/02. Result/01.10월_수납내역_정제완료.csv

#   4) OO광역시 시·군구별 체납정리 세무 공무원 현황
#   C:/Bigdata_project_tax/01. Data/OO광역시 시군구별 체납정리 세무공무원 현황.csv



'## Output       : C:/Bigdata_project_tax/02. Result'
#   1) 탐색적 데이터 분석(EDA)
#   C:/Tax/02.Result/02.탐색적 데이터 분석 결과.xlsx

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
library(devtools)
library(openxlsx)
library(ggplot2)
library(tidyverse)
library(reshape2)
library(corrplot)


# --------------------------------------------------------------------------------------- #
## 1) 데이터 파일 로드

# 1) OO광역시청 OOOO 신용등급
OOOO_class_before <- read.csv("00.OOOO등급_기초.csv")
OOOO_class <- read.csv("01.OOOO등급_정제완료.csv")

# 2) 10월 발급 내역
tax_chanap <- read.csv("01.10월_발송내역_정제완료.csv")

# 3) 10월 수납 내역
tax_soonap <- read.csv("01.10월_수납내역_정제완료.csv")

# 4) 
citynumber <- read.csv("../01. Data/OO광역시 시군구별 체납정리 세무공무원 현황.csv")

# --------------------------------------------------------------------------------------- #
## 2) 탐색적 데이터 분석(EDA)
# 1) OO광역시 연도별 체납인원/체납액/인당세목건수/인당체납건수/인당체납액 추이 (OOOO)
EDA1 <- as.data.frame(OOOO_class %>% group_by(발생년도) %>% 
                        summarise(체납인원 = n(), 체납액 = sum(체납액), 인당세목개수 = mean(세목명건수), 인당체납건수 = mean(체납건수)) %>% 
                mutate(인당체납액 = (체납액/체납인원)) %>% filter(발생년도 != '2020'))

# --------------------------------------------------------------------------------------- #
# 2) OO광역시 소득추정/체납회수등급별 체납인원/체납액/인당세목건수/인당체납건수/인당체납액

EDA2 <- as.data.frame(OOOO_class %>% group_by(소득추정등급) %>% 
  summarise(체납인원 = n(), 체납합계 = sum(체납액), 인당세목개수 = mean(세목명건수), 인당체납건수 = mean(체납건수)) %>% 
  mutate(인당체납액 = (체납합계/체납인원)))

EDA3 <- as.data.frame(OOOO_class %>% group_by(체납회수등급) %>% 
  summarise(체납인원 = n(), 체납합계 = sum(체납액), 인당세목개수 = mean(세목명건수), 인당체납건수 = mean(체납건수)) %>% 
  mutate(인당체납액 = (체납합계/체납인원)))


# 3) 10월 발송내역 중 소득추정/체납회수등급별 체납인원/체납액/인당세목건수/인당체납건수/인당체납액
EDA4 <- as.data.frame(tax_chanap %>% group_by(소득추정등급) %>% 
  summarise(체납인원 = n(), 체납합계 = sum(체납액), 인당세목개수 = mean(세목명건수), 인당체납건수 = mean(체납건수)) %>% 
  mutate(인당체납액 = (체납합계/체납인원)))

EDA5 <- as.data.frame(tax_chanap %>% group_by(체납회수등급) %>% 
  summarise(체납인원 = n(), 체납합계 = sum(체납액), 인당세목개수 = mean(세목명건수), 인당체납건수 = mean(체납건수)) %>% 
  mutate(인당체납액 = (체납합계/체납인원)))


# 4) 10월 수납내역 중 소득추정/체납회수등급별 체납인원/체납액/인당세목건수/인당체납건수/인당체납액
EDA6 <- as.data.frame(tax_soonap %>% group_by(소득추정등급) %>% 
  summarise(수납인원 = n(), 수납합계 = sum(수납액), 인당세목개수 = mean(세목명건수), 인당수납건수 = mean(체납건수)) %>% 
  mutate(인당체납액 = (수납합계/수납인원)))

EDA7 <- as.data.frame(tax_soonap %>% group_by(체납회수등급) %>% 
  summarise(수납인원 = n(), 수납합계 = sum(수납액), 인당세목개수 = mean(세목명건수), 인당수납건수 = mean(체납건수)) %>% 
  mutate(인당체납액 = (수납합계/수납인원)))


# 5) 10월 발송 대비 수납 소득추정등급 대비 체납회수등급 징수율
EDA6$징수율 <- as.numeric(round(EDA6$수납인원/EDA4$체납인원*100,2))
EDA7$징수율 <- as.numeric(round(EDA7$수납인원/EDA5$체납인원*100,2))


# 6) 상관분석 
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
cor_class <- function(x){ # 사용자 정의 함수
  corrplot(cor(x),
           method = "color",       # 색깔로 표현
           col = col(200),         # 색상 200개 선정
           type = "lower",         # 왼쪽 아래 행렬만 표시
           order = "hclust",       # 유사한 상관계수끼리 군집화
           addCoef.col = "black",  # 상관계수 색깔
           tl.col = "black",       # 변수명 색깔
           #tl.srt = 45,            # 변수명 45도 기울임
           diag = F)               # 대각 행렬 제외
}

# 1) OO광역시 소득 & 체납회수 등급
cor(EDA2) ; cor(EDA3)
cor_class(EDA2)
cor_class(EDA3)

# 2) 10월 발송 소득 & 체납회수 등급
cor(EDA4) ; cor(EDA5)
cor_class(EDA4)
cor_class(EDA5)

# 3) 10월 수납 소득 & 체납회수 등급
cor(EDA6) ; cor(EDA7) 
cor_class(EDA6[1:6])
cor_class(EDA7[1:6])

# --------------------------------------------------------------------------------------- #
# 7) 세목명 탐색적 데이터 분석 상위 5위 체납 세목명
# 1. 단일 세목명 체납액 상위 5위
unique(OOOO_class_before$세목명)
OOOO_class_before$세목명 <- ifelse(OOOO_class_before$세목명 == '구)취득세(부동산)','취득세(부동산)',OOOO_class_before$세목명)

top5_semok_before <- OOOO_class_before[OOOO_class_before$체납액 > 0,] %>% group_by(세목명) %>% 
  summarise(체납합계 = sum(체납액)) %>%  arrange(desc(체납합계)) %>%  head(5)

paste0(round(sum(top5_semok_before$체납합계)/sum(OOOO_class_before$체납액),2) * 100 , "%")


# 2. 체납회수등급별 단일 세목명 기준 체납액 10위
OOOO_class_before[OOOO_class_before$체납액 > 0,] %>% group_by(체납회수등급, 세목명) %>%
  summarise(체납액 = sum(체납액)) %>% 
  arrange(체납회수등급,desc(체납액)) %>% slice(1:10)
  
# --------------------------------------------------------------------------------------- #
# 8) 시군구코드별 소득·체날 등급을 활용한 현황 파악 -> 지방세 공무원 매칭
# 체납건수
temp_citycode <- OOOO_class_before %>% group_by(시군구코드) %>% summarise(체납건수=n())  

# 체납명수
number <- OOOO_class_before[!duplicated(OOOO_class_before$납세자번호),] %>% 
  group_by(시군구코드) %>% summarise(체납명수=n()) 
temp_citycode$체납명수 <- number$체납명수
temp_citycode$인당체납건수 <- round(temp_citycode$체납건수/number$체납명수,2)

# 체납액 추가
money <- OOOO_class_before[!duplicated(OOOO_class_before$납세자번호),] %>% 
  group_by(시군구코드) %>% summarise(체납액=sum(체납액))  
temp_citycode$체납액 <- money$체납액

# 시군구 직원
temp_citycode$시군구코드 <- c('중구', '동구', '미추홀구','연수구',
                          '남동구','부평구','계양구','서구','강화군','옹진군')

EDA_final_citycode <- left_join(temp_citycode, citynumber, by = '시군구코드') %>% 
  mutate(체납담당자당미정리체납건수 = 체납건수/체납정리인원, 체납담당자당미정리체납명수 = 체납명수/체납정리인원, 미정리체납건당투입가능시간 = 120000/체납담당자당미정리체납건수) %>% 
  arrange(미정리체납건당투입가능시간)

# --------------------------------------------------------------------------------------- #
## 5) 데이터 저장

wb <- createWorkbook()

addWorksheet(wb, "1 OO시 연도별 체납현황")
writeData(wb, sheet = "1 OO시 연도별 체납현황", x = EDA1)

addWorksheet(wb, "2.1 OO시 소득추정등급")
writeData(wb, sheet = "2.1 OO시 소득추정등급", x = EDA2)

addWorksheet(wb, "2.2 OO시 체납회수등급")
writeData(wb, sheet = "2.2 OO시 체납회수등급", x = EDA3)

addWorksheet(wb, "3.1 10월발급 소득추정등급")
writeData(wb, sheet = "3.1 10월발급 소득추정등급", x = EDA4)

addWorksheet(wb, "3.2 10월발급 체납회수등급")
writeData(wb, sheet = "3.2 10월발급 체납회수등급", x = EDA5)

addWorksheet(wb, "4.1 10월수납 소득추정등급")
writeData(wb, sheet = "4.1 10월수납 소득추정등급", x = EDA6)

addWorksheet(wb, "4.2 10월수납 체납회수등급")
writeData(wb, sheet = "4.2 10월수납 체납회수등급", x = EDA7)

addWorksheet(wb, "5 상위 5개 단일세목명 체납합계")
writeData(wb, sheet = "5 상위 5개 단일세목명 체납합계", x = top5_semok_before)

addWorksheet(wb, "6 상위 5개 다중세목명 체납합계")
writeData(wb, sheet = "6 상위 5개 다중세목명 체납합계", x = top5_semok)

addWorksheet(wb, "7 시군구코드별 소득·체납 등급기반 현황")
writeData(wb, sheet = "7 시군구코드별 소득·체납 등급기반 현황", x = EDA_final_citycode)

openXL(wb)

saveWorkbook(wb, file = "02.탐색적 데이터 분석 결과.xlsx", overwrite = TRUE)

# == 소스 보관용

'
# 시군구코드 중 다수 체납액의 소득추정등급 추출
much_income1 <- OOOO_class_before[!duplicated(OOOO_class_before$납세자번호),] %>% 
  group_by(시군구코드,소득추정등급) %>% 
  summarise(다수체납액=sum(체납액)) %>% filter(소득추정등급 != 0)

much_income2 <- much_income1 %>% group_by(시군구코드) %>% summarise(다수체납액=max(다수체납액))

# 소득 병합
much_income <- merge(much_income2, much_income1[c('다수체납액','소득추정등급')], by = "다수체납액", all.much_income2 = T)

# 시군구코드 중 다수 체납액의 체납회수등급 추출 
much_chenap1 <- OOOO_class_before[!duplicated(OOOO_class_before$납세자번호),] %>% 
  group_by(시군구코드,체납회수등급) %>% 
  summarise(다수체납액=sum(체납액))

much_chenap2 <- much_chenap1 %>% group_by(시군구코드) %>% summarise(다수체납액=max(다수체납액))

# 체납 병합
much_chenap <- merge(much_chenap2, much_chenap1[c('다수체납액','체납회수등급')], by = "다수체납액", all.much_chenap2 = T)

# 최종 데이터 병합
step1 <- merge(temp_citycode, much_income[c('시군구코드','다수체납액','소득추정등급')], by = "시군구코드", all.temp_citycode = T)
final_citycode <- merge(step1, much_chenap[c('시군구코드','다수체납액','체납회수등급')], by = "시군구코드", all.step1 = T)
colnames(final_citycode)[6] <- '소득등급기준체납액' ; colnames(final_citycode)[8] <- '체납등급기준체납액'
final_citycode
'
