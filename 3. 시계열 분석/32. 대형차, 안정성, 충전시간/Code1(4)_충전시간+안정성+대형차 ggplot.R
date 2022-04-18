### Code : 충전시간+안정성+대형차 ggplot
### Writer : Donghyun Kim
### Date : 2020.09.12

## 1. 작업공간 설정
dr <- "C:/"
foldname <- paste0(dr, "Users/mazy4/Desktop/ISTANS/5. 분석 data/1. 자동차 데이터/2. 대형차, 안정성, 충전시간 전처리/data")
setwd(foldname)

## 2. 파일 불러오기(전처리 결과물 호출)
library(readxl)

filename <- "충전시간+안정성+대형차.xlsx"
incheon <- as.numeric(read_xlsx(filename, sheet = "인천")[12, -c(1:3, 63)])
yeosu <- as.numeric(read_xlsx(filename, sheet = "여수")[1, -c(1:3, 63)])
busan <- as.numeric(read_xlsx(filename, sheet = "부산")[17, -c(1:3, 63)])
pyeongtaek <- as.numeric(read_xlsx(filename, sheet = "평택")[1, -c(1:3, 63)])
jeju <- as.numeric(read_xlsx(filename, sheet = "제주")[3, -c(1:3, 63)])

## 3. 각 지역별 데이터 프레임 제작
makedate <- function(a = 15, b = 16, c = 17, d = 18, e = 19, f = 20) {
  mydate <<- NULL
  
  for(yy in c(a, b, c, d, e, f)) {
    for(m in 1:12) {
      if(m %in% 1:9) {
        mydate <<- c(mydate, as.numeric(paste0(yy, 0, m)))
      } else {
        mydate <<- c(mydate, as.numeric(paste0(yy, m)))
      }
    }
  }
  
  mydate <<- as.factor(mydate[-c(1:7, 67:72)])
}

makedate()

## 3. 데이터 프레임(인천, 여수, 부산, 평택, 제주) 제작
incheon_car <- data.frame(date_ym = mydate, carno = incheon)
yeosu_car <- data.frame(date_ym = mydate, carno = yeosu)
busan_car <- data.frame(date_ym = mydate, carno = busan)
pyeongtaek_car <- data.frame(date_ym = mydate, carno = pyeongtaek)
jeju_car <- data.frame(date_ym = mydate, carno = jeju)

## 4. 데이터 프레임 합치기
total <- rbind(incheon_car, yeosu_car, busan_car, pyeongtaek_car, jeju_car)
total$level <- rep(factor(c("인천", "여수", "부산", "평택", "제주")), each = 59)

## 5. ggplot 그리기
library(ggplot2)
total_plot <- ggplot(data = total, aes(x = date_ym, y = carno, color = level, group = level)) +
  geom_line() + geom_point() + ggtitle("각 지역별 자동차 수 현황(충전시간, 안정성, 대형차 기준)") + labs(x = "연월", y = "자동차 수")
total_plot
