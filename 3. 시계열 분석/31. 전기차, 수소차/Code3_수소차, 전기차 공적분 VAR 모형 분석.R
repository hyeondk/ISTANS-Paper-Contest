### Code : 수소차, 전기차 공적분 VAR 모형 분석
### Writer : Donghyun Kim
### Date : 2020.08.24

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

### Vector Time-Series(Cointegration VAR model) Analysis ###

## 1. Check Time-Series Plot
ts.plot(cardata, col = c("red", "blue"), xlab = "시간", ylab = "자동차 등록현황(대)", main = "전기, 수소차 Time Series Plot")
legend("topleft", lty = 1, col = c("red", "blue"), c("전기", "수소"))

# 결과 해석
# 전기차, 수소차 모두 증가하는 비정상적인 모습을 보여주고 있다.

## 2. Information Criterions for VAR(p) model
library(vars) # library for function VARselect
library(urca) # library for function ca**

VARselect(cardata, lag.max = 5, type = "const")

# 결과 해석
# type = "const" : 상수항 포함
# 공적분 계수검정을 할 때 case 2라고 생각하면 된다.
# case 2 : 두 비정상 시계열들은 선형추세를 가지고 있으며 그 차이는 일정한 간격이 계속 유지되어
#          공적분 관계에 상수항이 있는 것으로 파악
# 모수 절약의 원칙에 따라 VAR(3)으로 설정하고자 한다.

## 3. Cointegration Tests(trace and max. eig. tests)
## 3-1. Trace test
ctest1 <- ca.jo(cardata, type = c("trace"), ecdet = c("none"), K = 3)
summary(ctest1)

# 결과 해석
# (1) 전반적인 결과 해석
# with linar trend -> original time series가 linear trend를 가지고 있다는 의미이다.
# Eigenvalues(lambda) 부분을 통해 고유값 확인 가능
# 오차수정항은 0이 아닌 어떤 constant가 들어간 형태(case 2)

# (2) 검정 결과 해석
# r = 0일 때, 40.31 > 5%(17.95)이므로 H0 reject -> r = 1 고려 가능
# r = 1일 때, 0.11 > 5%(8.18)이 성립하지 않으므로 H0 reject 불가 -> cointegrating rank를 1로 삼을 수 있음

## 3-2. Maximum Eigenvalue test
ctest2 <- ca.jo(cardata, type = c("eigen"), ecdet = c("none"), K = 3)
summary(ctest2)

# 결과 해석
# (1) 전반적인 결과 해석
# 마찬가지로, Eigenvalues(lambda) 부분을 통해 고유값을 확인할 수 있다.

# (2) 검정 결과 해석
# r = 0 : 40.20 > 5%(14.90)이므로 H0 reject -> r = 1 고려 가능
# r = 1 : 0.11 > 5%(8.18)이 성립하지 않으므로 H0 reject 불가 -> cointegrating rank를 1로 삼을 수 있음


## 4. Fitting vector error correction model
cajorls(ctest1, r = 1)

# 결과 해석
# (1) Coefficients
# ect1 : error correction term의 약자로, alpha 추정값(vector 형태)
# beta 값은 $beta에 따로 출력되었다. 추정값은 vector 형태이다.
# constant : intercept(vector 형태)
# elec.dl1 ~ hydro.dl2 : phi* 행렬 추정값(matrix 형태)

# (2) 주의점
# R은 출력이 행 방향인데, 실제로 모델 작성 시에는 열 방향으로 작성한다.
# 따라서, elec.dl1부터 hydro.dl2까지 나온 4 X 2 행렬을 transpose시켜서 행렬을 작성해야 한다.

## 5. 미래 1년 동안의 예측값 및 신뢰구간(값만 확인 가능)
pred <- predict(vec2var(ctest1, r = 1), n.ahead = 24, ci = 0.95)
pred

# 예측값만 따로 저장
pd_elec <- pred$fcst$elec[1:24, 1]
pd_hydro <- pred$fcst$hydro[1:24, 1]

# 예측값을 elec, hydro에 업데이트
up_elec <- ts(c(elec, pd_elec), start = c(2015, 9), frequency = 12)
up_hydro <- ts(c(hydro, pd_hydro), start = c(2015, 9), frequency = 12)

# 데이터셋 업데이트
up_cardata <- cbind(up_elec, up_hydro)

### 6. 예측값 포함한 시계열 그림
ts.plot(up_cardata, col = c("red", "blue"), xlab = "시간", ylab = "자동차 등록현황(대)",
        main = "전기, 수소차 Time Series Plot(2년 예측값 포함)")
legend("topleft", lty = 1, col = c("red", "blue"), c("전기", "수소"))
abline(v = 2020.5, lty = 2) # 예측값 구분하기 위한 점선
