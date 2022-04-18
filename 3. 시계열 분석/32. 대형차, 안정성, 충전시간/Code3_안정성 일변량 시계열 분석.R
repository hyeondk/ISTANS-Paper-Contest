### Code : 안정성 Univariate Time Series Analysis(지역별)
### Writer : Donghyun Kim
### Date : 2020.09.17

## 1. 작업공간 설정
dr <- "C:/"
foldname <- paste0(dr, "Users/mazy4/Desktop/ISTANS/5. 분석 data/1. 자동차 데이터/2. 대형차, 안정성, 충전시간 전처리/data")
setwd(foldname)

## 2. 파일 불러오기(전처리 결과물 호출)
library(readxl)

filename <- "안정성.xlsx"
incheon <- as.numeric(read_xlsx(filename, sheet = "인천")[12, -c(1:3, 63)])
yeosu <- as.numeric(read_xlsx(filename, sheet = "여수")[1, -c(1:3, 63)])
busan <- as.numeric(read_xlsx(filename, sheet = "부산")[17, -c(1:3, 63)])
pyeongtaek <- as.numeric(read_xlsx(filename, sheet = "평택")[1, -c(1:3, 63)])
jeju <- as.numeric(read_xlsx(filename, sheet = "제주")[3, -c(1:3, 63)])

## 3. 모두 Time Series로 변환하기
incheon <- ts(incheon, start = c(2015, 8), frequency = 12)
yeosu <- ts(yeosu, start = c(2015, 8), frequency = 12)
busan <- ts(busan, start = c(2015, 8), frequency = 12)
pyeongtaek <- ts(pyeongtaek, start = c(2015, 8), frequency = 12)
jeju <- ts(jeju, start = c(2015, 8), frequency = 12)

## 4. y축 구간 정하기 위한 함수
# (1) findvalue 함수 제작(Minimum, Maximum, Range 출력)
findvalue <- function(x) {
  minv <- min(x)
  maxv <- max(x)
  difv <- maxv - minv
  return(c(minv, maxv, difv))
}

# (2) 각 지역별 함수 실행
# 이때, Range가 가장 큰 지역에 맞춰 y축 구간이 정해진다.
# y축 구간 설정 : c(각 지역별 최소값, 각 지역별 최소값 + Range가 가장 큰 지역의 Range값)
findvalue(incheon)
findvalue(yeosu)
findvalue(busan)
findvalue(pyeongtaek)
findvalue(jeju)

########################################################################################

### Time Series Analysis 1 : 인천 지역

# (1) Plotting Time Series
#ts.plot(incheon, ylab = "자동차 수", main = "인천 지역 시계열그림(대형차 기준)")

# (2) R auto fitting
library(forecast)
fit1 <- auto.arima(incheon)
fit1

# (3) Forecasting
forecast(fit1, h = 12)

# (4) Plotting Forecasting Data
autoplot(forecast(fit1, h = 12), ylab = "자동차 수",
         ylim = c(findvalue(incheon)[1], findvalue(incheon)[1] + 1200),
         main = "인천 지역 예측 시계열그림(안정성 기준)")

########################################################################################

### Time Series Analysis 2 : 여수 지역

# (1) Plotting Time Series
#ts.plot(yeosu, ylab = "자동차 수", main = "여수 지역 시계열그림(대형차 기준)")

# (2) R auto fitting
library(forecast)
fit2 <- auto.arima(yeosu)
fit2

# (3) Forecasting
forecast(fit2, h = 12)

# (4) Plotting Forecasting Data
autoplot(forecast(fit2, h = 12), ylab = "자동차 수",
         ylim = c(findvalue(yeosu)[1], findvalue(yeosu)[1] + 1200),
         main = "여수 지역 예측 시계열그림(안정성 기준)")

########################################################################################

### Time Series Analysis 3 : 부산 지역

# (1) Plotting Time Series
#ts.plot(busan, ylab = "자동차 수", main = "부산 지역 시계열그림(대형차 기준)")

# (2) R auto fitting
library(forecast)
fit3 <- auto.arima(busan)
fit3

# (3) Forecasting
forecast(fit3, h = 12)

# (4) Plotting Forecasting Data
# 예측치가 계속 떨어지는 특이 case라서, 별도로 y축 최소값을 설정해줌.
autoplot(forecast(fit3, h = 12), ylab = "자동차 수",
         ylim = c(findvalue(busan)[1], findvalue(busan)[1] + 1200),
         main = "부산 지역 예측 시계열그림(안정성 기준)")

########################################################################################

### Time Series Analysis 4 : 평택 지역

# (1) Plotting Time Series
#ts.plot(pyeongtaek, ylab = "자동차 수", main = "평택 지역 시계열그림(대형차 기준)")

# (2) R auto fitting
library(forecast)
fit4 <- auto.arima(pyeongtaek)
fit4

# (3) Forecasting
forecast(fit4, h = 12)

# (4) Plotting Forecasting Data
autoplot(forecast(fit4, h = 12), ylab = "자동차 수",
         ylim = c(findvalue(pyeongtaek)[1], findvalue(pyeongtaek)[1] + 1200),
         main = "평택 지역 예측 시계열그림(안정성 기준)")

########################################################################################

### Time Series Analysis 5 : 제주 지역

# (1) Plotting Time Series
#ts.plot(jeju, ylab = "자동차 수", main = "제주 지역 시계열그림(대형차 기준)")

# (2) R auto fitting
library(forecast)
fit5 <- auto.arima(jeju)
fit5

# (3) Forecasting
forecast(fit5, h = 12)

# (4) Plotting Forecasting Data
autoplot(forecast(fit5, h = 12), ylab = "자동차 수",
         ylim = c(findvalue(jeju)[1], findvalue(jeju)[1] + 1200),
         main = "제주 지역 예측 시계열그림(안정성 기준)")
