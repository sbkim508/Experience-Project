# ======================================================================================= #
## Script id   : 0.기초전처리.R
## Script Name : 데이터 기초 전처리
'## Input       : C:/Bigdata_project_tax/01. Data'
#  1) OO광역시청 OOOO 신용등급 
#   C:/Bigdata_project_tax/(OO광역모형)OOOO체납회수등급.xlsx

#  2) 10월 발급 내역
#   C:/Bigdata_project_tax/01. Data/10월 고지서-최종.xlsx
#   C:/Bigdata_project_tax/01. Data/10월 안내문-최종.xlsx

#  3) 10월 수납 내역
#   C:/Bigdata_project_tax/01. Data/10월 수납현황.xlsx

'## Output      : C:/Bigdata_project_tax/02. Result'
#   1) OO광역시청 OOOO 신용등급
#   C:/Bigdata_project_tax/02. Result/00.OOOO등급_기초.csv

#   2) 10월 발급 내역 
#   C:/Bigdata_project_tax/02. Result/00.10월_발송내역_기초.csv

#   3) 10월 수납 내역
#   C:/Bigdata_project_tax/02. Result/00.10월_수납내역_기초.csv

## Date        : 2020.12.31

# ======================================================================================= #
## 1) 초기화
rm(list = ls()); gc();


## 2)작업 디렉토리 경로 지정
## 사용자 작업 디렉토리 경로 확인
getwd()
## 사용자 작업 디렉토리 경로 변경 (편의상 C 드라이브를 경로로 지정)
setwd("C:/Bigdata_project_tax/01. Data")

## 3)패키지 설치 및 패키지 불러오기
library(openxlsx)

# --------------------------------------------------------------------------------------- #
## 1) 데이터 파일 로드

#  1) OO광역시청 OOOO 신용등급
tax_class <- read.xlsx("(OO광역모형)OOOO체납회수등급.xlsm", sheet = 2)

#  2) 10월 발급 내역
tax_chanap_1 <- read.xlsx("10월 고지서-최종.xlsx", sheet = 1)
tax_chanap_2 <- read.xlsx("10월 안내문-최종.xlsx", sheet = 1)

#  3) 10월 수납 내역
tax_soonap <- read.xlsx("10월 수납현황.xlsx", sheet = 1)

# --------------------------------------------------------------------------------------- #
## 2) 사용 컬럼 선택

#  1) OO광역시청 OOOO 신용등급 (342,852 건, 16개 컬럼 사용)
tax_class_a <- tax_class[c(3,4,8,9,11,12,13,17:22,24,27,30)]

colnames(tax_class_a) <- c('시군구코드','과세년월','세목명','납세자번호','부과금액','가산금','체납액',
                               'CB점수','소득추정등급','소득추정금액','체납회수등급','회수율',
                               'SP등급','카드1','카드2','카드3')

tax_class_a <- tax_class_a[-1,]
row.names(tax_class_a) <- NULL
head(tax_class_a,3) ; dim(tax_class_a)
'tax_class_a'

#  2) 10월 발급 내역
tax_chanap_1$납세자번호 <- paste0(str_sub(tax_chanap_1$납세자번호,1,6),"-",str_sub(tax_chanap_1$납세자번호,7,13))
tax_chanap_2$납세자번호 <- paste0(str_sub(tax_chanap_2$납세자번호,1,6),"-",str_sub(tax_chanap_2$납세자번호,7,13))

colnames(tax_chanap_1) ; colnames(tax_chanap_2) ; nrow(tax_chanap_1) ; nrow(tax_chanap_2)

tax_chanap_1 <- tax_chanap_1[,c('납세자번호', '세목명','금액')]
tax_chanap_2 <- tax_chanap_2[,c( '납세자번호','세목명','체납합계')]

colnames(tax_chanap_1) <- c('납세자번호','세목명','체납액') ; colnames(tax_chanap_2) <- c('납세자번호','세목명','체납액')
tax_chanap <- rbind(tax_chanap_1,tax_chanap_2)

'tax_chanap'


#  3) 10월 수납 내역
tax_soonap <- tax_soonap[c('자치단체','세목','주민번호2','수납금액')]
colnames(tax_soonap) <- c('시군구코드','세목명','납세자번호','수납금액')

'tax_soonap'

# --------------------------------------------------------------------------------------- #

## 3) 데이터 저장

#  1) OO광역시청 OOOO 신용등급
write.table(tax_class_a, file="../02. Result/00.OOOO등급_기초.csv", sep=",", append = FALSE,row.names = FALSE, col.names = TRUE)
print(paste0('OO광역시 OOOO 등급 : ', dim(tax_class_a)[1],'건 ', dim(tax_class_a)[2], '개 변수 사용'))

#  2) 10월 발급 내역 
write.table(tax_chanap, file="../02. Result/00.10월_발송내역_기초.csv", sep=",", append = FALSE,row.names = FALSE, col.names = TRUE)
print(paste0('10월 발송 내역 : ', dim(tax_chanap)[1],'건 ', dim(tax_chanap)[2], '개 변수 사용'))

#  3) 10월 수납 내역
write.table(tax_soonap, file="../02. Result/00.10월_수납내역_기초.csv", sep=",", append = FALSE,row.names = FALSE, col.names = TRUE)
print(paste0('10월 수납 내역 : ', dim(tax_soonap)[1],'건 ', dim(tax_soonap)[2], '개 변수 사용'))
