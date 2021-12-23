# ======================================================================================= #
## Script id   : 1. 데이터 정제.R
## Script Name : 데이터 정제
'## Input       : C:/Bigdata_project_tax/02. Result'
#   1) OO광역시청 OOOO 신용등급
#   C:/Bigdata_project_tax/02. Result/00.OOOO등급_기초.csv

#   2) 10월 발급 내역 
#   C:/Bigdata_project_tax/02. Result/00.10월_발송내역_기초.csv

#   3) 10월 수납 내역
#   C:/Bigdata_project_tax/02. Result/00.10월_수납내역_기초.csv

'## Output      : C:/Bigdata_project_tax/02. Result'
#   1) OO광역시청 OOOO 신용등급
#   C:/Bigdata_project_tax/02. Result/01.OOOO등급_정제완료.csv

#   2) 10월 발급 내역 
#   C:/Bigdata_project_tax/02. Result/01.10월_발송내역_정제완료.csv

#   3) 10월 수납 내역
#   C:/Bigdata_project_tax/02. Result/01.10월_수납내역_정제완료.csv

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
library(stringr)
library(tidyverse)
library(data.table)

# --------------------------------------------------------------------------------------- #
## 1) 데이터 파일 로드 

#  1) OO광역시청 OOOO 신용등급
OOOO_class <- read.csv("00.OOOO등급_기초.csv")

#  2) 10월 발급 내역
tax_chanap <- read.csv("00.10월_발송내역_기초.csv")

#  3) 10월 수납 내역
tax_soonap <- read.csv("00.10월_수납내역_기초.csv")

# --------------------------------------------------------------------------------------- #
## 2) OO광역시청 OOOO 신용등급 정제
# 1) 세목명 정제
unique(OOOO_class$세목명)
OOOO_class$세목명 <- ifelse(OOOO_class$세목명 == '구)취득세(부동산)','취득세(부동산)',OOOO_class$세목명)

# 2) 카드개수 파생변수 생성
OOOO_class$카드개수 <-apply(!is.na(OOOO_class[14:16]),1,sum)
OOOO_class_b <- OOOO_class[-c(14:16)] # 카드변수 제거

# 3) 중복제거(인당 체납액 정제)
nrow(OOOO_class_b) ; length(unique(OOOO_class_b$납세자번호))

OOOO_class_s <- OOOO_class_b %>%# 부과금, 가산금, 체납액
  group_by(납세자번호) %>% 
  summarise(부과금액 = sum(부과금액), 가산금 = sum(가산금), 체납액 = sum(체납액))

# 4) 세목명건수 파생변수 생성
DT <- as.data.table(OOOO_class_b[c(4,3)]) # 중복 전
(q2 <- DT[!duplicated(DT$납세자번호),])   # 중복제거

for (i in 1:nrow(q2)){ # 다소 시간이 오래 걸림
    t <- subset(DT, 납세자번호 == unique(q2$납세자번호)[i])
    t2 <- unique(t$세목명)
    q2[i,2] <- paste(t2,collapse = ",")
    print(paste0(nrow(q2),'회 중 ', i,' 번째 완료'))
}
DT <- as.data.frame(q2)
# DT <- read.csv('semok_count.csv') # 너무 오래걸려서 저장후 업로드 
nrow(DT) ; head(DT) ; dim(DT)
semok_count <- cbind(DT[c(2,3)] ,data.frame(세목명건수 = str_count(DT$세목명,',') + 1))
# write.csv(semok_count, 'semok_count.csv') 백업용
head(semok_count)

# 파생변수 Merge
# 개발용 #DT_split <- read.csv('dev.csv') # 세목명분리한 것
new_merge_data <- merge(OOOO_class_s,semok_count, by ='납세자번호')

# 5) 체납건수 추가
temp1 <- as.data.table(OOOO_class_b[c(4,3)])
temp2 <- setDT(temp1)[, 세목명 := list(paste(세목명, collapse = ', ')), by = '납세자번호']
temp3 <- temp2[, 체납건수 := .N, by = c("납세자번호","세목명")][!duplicated(temp2$납세자번호)]
chenap_count <- as.data.frame(temp3)

# 파생변수 Merge
final_merge <- merge(new_merge_data,chenap_count[c(1,3)], by ='납세자번호')

head(final_merge) ; dim(final_merge) ; colnames(final_merge)

# 6) 파생변수와 OOOO정보 병합
OOOO_class_dup <- OOOO_class_b[!duplicated(OOOO_class_b$납세자번호),][-c(3,5,6,7)]
final_OOOO_class <- merge(final_merge, OOOO_class_dup, by = "납세자번호", all.final_merge = T)
nrow(final_OOOO_class) ; head(final_OOOO_class,3)

# 7) OOOO 정보 결측값 처리
chane_value <- function(x){ # 사용자 지정 함수 생성
  
  # 확인
  for (i in 1:length(colnames(x))){
    a <- round((nrow(x[x[colnames(x)[i]] == 0,]) / nrow(x))*100,2)
    
    if( a != 0 ) {
      print(paste0("체납자 중 ",colnames(x)[i],"의 결측값은 ", a, " % 입니다."))
    }
    
  }
  
  # 정제과정
  vec <- c('CB점수','소득추정등급','소득추정금액','SP등급') # 지정
  for (i in vec){
    t <- round(mean(x[x['체납회수등급'] == round(mean(x[x[i] == 0,]$체납회수등급)) & !x[i] == 0,][[i]]))
    x[[i]] <- ifelse(x[[i]] == 0, t ,x[[i]])
    print(paste0(i, "발송 내역 전처리 완료"))
  }
  
  # 완료
  for (i in 1:length(colnames(x))){
    a <- round((nrow(x[x[colnames(x)[i]] == 0,]) / nrow(x))*100,2)
    
    if( a != 0 ) {
      print(paste0("체납자 중 ",colnames(x)[i],"의 결측값은 ", a, " % 입니다."))
    }
  } 
  return (x)
}

dis_class <- chane_value(final_OOOO_class) # 결측치 완료

# 8) 이상치 처리(향후 검증치 낮아지면 제거)
summary(dis_class) # 이상치 확인 : 부과금액, 가산금, 체납액 만원이상, 소득추정금액 제거거
out_class <- dis_class[dis_class$부과금액 >= 0 & dis_class$가산금 >= 0 & dis_class$체납액 >= 10000 & dis_class$소득추정금액 != 99900,]


# 9) 과세년월 변경 및 부과년도 생성
out_class$과세년월 <- str_sub(out_class$과세년월,1,4)
colnames(out_class)[9] <- '발생년도'

head(out_class,3)
out_class$체납기간 <- (2020 - as.numeric(out_class$발생년도))


# 10) 가산금 비율 
handling_data <- out_class %>% mutate(가산비율 = round((가산금/체납액)*100,3))


# 11) 세목명 별 가산비율
# 세목별 가산점 기준표 
standard_semok <- OOOO_class[OOOO_class$체납액 > 0,] %>% 
  group_by(체납회수등급, 세목명) %>% summarise(체납액 = sum(체납액)) %>% 
  arrange(체납회수등급,desc(체납액)) %>% 
  mutate(등급별가산점 = 체납액/sum(체납액)) %>% mutate(전체가산점 = 체납액/32625544380)

# 체납회수등급과 세목명에 일치하는 가산점 부여 
merge_semok <- left_join(OOOO_class[,c('납세자번호','세목명','체납회수등급')], 
                         standard_semok[,c('체납회수등급','세목명','등급별가산점','전체가산점')], 
                         by = c('체납회수등급', '세목명')) %>% 
                group_by(납세자번호) %>% 
                summarise(등급별가산 = sum(등급별가산점), 전체가산 = sum(전체가산점))

# 병합
final_handling_data <- merge(handling_data, merge_semok, by = '납세자번호')



# 1. final_handling_data # 납세자번호별 + 세목명(다중) + 체납회수등급
semok1 <- final_handling_data[c('납세자번호','세목명','체납회수등급')]

# 2. 1~10 체납회수등급별 체납액 합계
#탐색적데이터 분석 # 2. 체납회수등급별 단일 세목명 기준 체납액 10위 참고
unique(OOOO_class_before$세목명)
OOOO_class_before$세목명 <- ifelse(OOOO_class_before$세목명 == '구)취득세(부동산)','취득세(부동산)',OOOO_class_before$세목명)

top5_semok_before <- OOOO_class_before[OOOO_class_before$체납액 > 0,] %>% group_by(세목명) %>% 
  summarise(체납합계 = sum(체납액)) %>%  arrange(desc(체납합계)) %>%  head(10)

paste0(round(sum(top5_semok_before$체납합계)/sum(OOOO_class_before$체납액),2) * 100 , "%")


# 2. 체납회수등급별 단일 세목명 기준 체납액 10위
semok2 <- OOOO_class_before[OOOO_class_before$체납액 > 0,] %>% group_by(체납회수등급, 세목명) %>%
  summarise(체납액 = sum(체납액)) %>% 
  arrange(체납회수등급,desc(체납액)) %>% slice(1:10)


## 최종 OOOO 등급 데이터
summary(final_handling_data)
head(final_handling_data,3) ; dim(final_handling_data) # 등급있는 체납자 135,454건 

# --------------------------------------------------------------------------------------- #
##  3) 10월 발급 내역 정제 (+ OOOO 등급 부여)
tax_chanap_merge <- tax_chanap %>%  group_by(납세자번호) %>% summarise(체납액 = sum(체납액))

# OOOO 등급부여
OOOO_10_chenap_class <- merge(tax_chanap_merge, final_handling_data[c(1,5:20)], by = "납세자번호", all.tax_chanap_merge = T)
colnames(final_handling_data)
head(OOOO_10_chenap_class) ; dim(OOOO_10_chenap_class)


# --------------------------------------------------------------------------------------- #
##  4) 10월 수납 내역 정제 (+ OOOO 등급 부여)
tax_soonap_merge <- tax_soonap %>%  group_by(납세자번호) %>% summarise(수납액 = sum(수납금액))


# OOOO 등급부여
OOOO_10_soonap_class <- merge(tax_soonap_merge, final_handling_data[c(1,5:20)], by = "납세자번호", all.tax_soonap_merge = T)
head(OOOO_10_soonap_class) ; dim(OOOO_10_soonap_class)
# 10월 발급<>수납내역 중복제거
tax_chanap_setdiff <- OOOO_10_chenap_class[!OOOO_10_chenap_class$납세자번호 %in% OOOO_10_soonap_class$납세자번호,]
head(tax_chanap_setdiff) ; dim(tax_chanap_setdiff)
# --------------------------------------------------------------------------------------- #
## 5) 데이터 저장

#  1) OO광역시청 OOOO 신용등급 # 135,454명
write.table(final_handling_data, file="../02. Result/01.OOOO등급_정제완료.csv", sep=",", append = FALSE,row.names = FALSE, col.names = TRUE)
print(paste0('OO광역시 OOOO 등급 : ', dim(final_handling_data)[1],'건 ', dim(final_handling_data)[2], '개 변수 사용'))

#  2) 10월 발급 내역 # 119,618명
write.table(tax_chanap_setdiff, file="../02. Result/01.10월_발송내역_정제완료.csv", sep=",", append = FALSE,row.names = FALSE, col.names = TRUE)
print(paste0('10월 발송 내역 : ', dim(tax_chanap_setdiff)[1],'건 ', dim(tax_chanap_setdiff)[2], '개 변수 사용'))

#  3) 10월 수납 내역 # 8,124명
write.table(OOOO_10_soonap_class, file="../02. Result/01.10월_수납내역_정제완료.csv", sep=",", append = FALSE,row.names = FALSE, col.names = TRUE)
print(paste0('10월 수납 내역 : ', dim(OOOO_10_soonap_class)[1],'건 ', dim(OOOO_10_soonap_class)[2], '개 변수 사용'))


'
# 3. 체납회수등급별 세목기준 비율(이 과정을 전체 설명)
semok3 <- final_handling_data[c('납세자번호','세목명','체납회수등급','등급별가산','전체가산')]

wb <- createWorkbook()
addWorksheet(wb, "납세자번호별 다중세목명 회수등급")
writeData(wb, sheet = "납세자번호별 다중세목명 회수등급", x = semok1)

addWorksheet(wb, "체납액 상위 세목명 10위")
writeData(wb, sheet = "체납액 상위 세목명 10위", x = top5_semok_before)

addWorksheet(wb, "1~10 체납회수등급별 체납액 합계")
writeData(wb, sheet = "1~10 체납회수등급별 체납액 합계", x = semok2)

addWorksheet(wb, "체납회수등급별 세목점수")
writeData(wb, sheet = "체납회수등급별 세목점수", x = semok3)

openXL(wb)
'
