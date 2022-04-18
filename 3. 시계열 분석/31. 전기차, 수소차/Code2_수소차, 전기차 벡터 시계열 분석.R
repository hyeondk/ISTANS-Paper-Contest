### Code : 수소차, 전기차 시계열 분석
### Writer : Donghyun Kim
### Date : 2020.08.17

## 1. 작업공간 설정
dr <- "C:/"
foldname <- paste0(dr, "Users/mazy4/Desktop/ISTANS/5. 분석 data/1. 수소차 데이터/result")
setwd(foldname)

## 2. 파일 불러오기(전처리 결과물 호출)
library(readxl)

elec <- NULL # 전기차 데이터
hydro <- NULL # 수소차 데이터

for(i in 2:59) {
  elec <- c(elec, read_xlsx("전기, 수소차 전처리 결과.xlsx", sheet = i)[15, ]$소계)
  hydro <- c(hydro, read_xlsx("전기, 수소차 전처리 결과.xlsx", sheet = i)[30, ]$소계)
}

## 3. Convert to Univariate Time-Series
elec <- ts(elec, start = c(2015, 9), frequency = 12)
hydro <- ts(hydro, start = c(2015, 9), frequency = 12)

## 3-2. Make Vector Time-Series
cardata <- cbind(elec, hydro)

###########################################################################################

### Vector Time-Series Analysis ###

## 1. 시계열 그림 확인
ts.plot(cardata, col = c("red", "blue"), xlab = "시간", ylab = "자동차 등록현황(대)", main = "전기, 수소차 Time Series Plot")
legend("topleft", lty = 1, col = c("red", "blue"), c("전기", "수소"))

# 결과 해석
# 증가 추세로 보여, nonstationary로 보여진다.
# 차분이 필요해보인다.

## 2. 1차 비계절 차분 & 로그 변환
ld_car <- diff(log(cardata))

ts.plot(ld_car, col = c("red", "blue"), xlab = "시간", ylab = "자동차 등록현황(대)",
        main = "전기, 수소차 1차 차분 & 로그 변환 Plot")
legend("topright", lty = 1, col = c("red", "blue"), c("전기", "수소"))

# 결과 해석
# 로그 변환 및 1차 차분하였으며, stationary 형태로 보인다.
# 이제 VAR modeling을 해보자.

## 3. VAR order 선택
library(vars) # library for function VARselect
VARselect(ld_car, lag.max = 7, type = "const")

# 결과 해석
# type = "const"를 통해 constant를 포함시키도록 했다.
# selection 부분에서 나오는 AIC 부분의 결과를 토대로 VAR(3)를 택할 수 있을 것으로 보인다.

## 4. VAR(3) 모형 적합
fit <- VAR(ld_car, p = 3, type = "const")
summary(fit)

# 결과 해석
# elec(전기차)과 hydro(수소차)의 equation을 따로 보여주고 있다.
# Estimate(추정값)를 보면 p-value가 커서 유의하지 못한 모수 추정값들이 많이 보인다.
# Covariance, Correlation 결과값은 행렬 형태로 나왔다. 해당 값은 R Console 참조.

## 5. 유의하지 못한 변수 빼버리기
fit2 <- restrict(fit, method = "ser") # equation(elec, hydro) 둘 다 적용
round(Bcoef(fit2), 2)

# 결과 해석
# 위의 fit2에 대한 결과를 소수점 2번째 자리까지 반올림하였다.

# 계수
# elec.l1, hydro.l1 : 1번째 coef
# elec.l2, hydro.l2 : 2번째 coef
# elec.l3, hydro.l3 : 3번째 coef
# const : constant

## 6. 잔차 검정
acf(resid(fit2, 12))

# 결과 해석
# 일부 lag에서 WN을 만족하지 못한 것도 보이지만 대부분의 lag에서 2 std error line 안에 들어가는 것으로 보인다.
# 따라서, White Noise라는 가정은 만족하는 것으로 판단된다.

## 7. Granger Causality test
# (1) cause : elec
causality(fit2, cause = "elec")

# (1) 결과 해석
# (요인 1) Granger : granger-cause 확인
# p-value가 크므로 H0를 기각할 수 없다.
# 따라서, elec(전기차)은 hydro(수소차)에 그랜저 인과성을 가지지 않는 것으로 보여진다.

# (요인 2) Instant : 동행상관관계 확인
# p-value가 크므로 H0를 기각할 수 없다.
# 따라서, elec(전기차)과 hydro(수소차)는 서로 관련이 있는 것으로 보여진다.

# (2) cause : hydro
causality(fit2, cause = "hydro")

# (2) 결과 해석
# (요인 1) Granger : granger-cause 확인
# p-value가 크므로 H0를 기각할 수 없다.
# 따라서, hydro(수소차)는 elec(전기차)에 그랜저 인과성을 가지지 않는 것으로 보여진다.

# (요인 2) Instant : 동행상관관계 확인
# p-value가 크므로 H0를 기각할 수 없다.
# 따라서, hydro(수소차)와 elec(전기차)는 서로 관련이 있는 것으로 보여진다.

# (1), (2) 결과 해석에 따른 결론
# elec(전기차)과 hydro(수소차)는 서로 피드백 관계(feedback)가 있다는 것을 알 수 있다.
# feedback : 입력 시계열과 출력 시계열이 서로 영향을 미친다는 의미

## 8. 예측
# 공적분 VAR 모형이 아닌 이상, 예측은 R에서 불가능하다.
# 그러나, 시계열 그림을 토대로 판단했을 때, 수소차와 전기차 데이터는 향후에도 계속 증가할 것으로 보인다.
