### Code : 수소차, 전기차 데이터 전처리(시게열 분석 목적)
### Writer : Donghyun Kim
### Date : 2020.08.04

## 모듈 1 : 수소차, 전기차 데이터 추출 및 엑셀 파일 저장
carprocess <- function(filename) {
  
  # 1. 불러올 엑셀 파일명 설정(결과물 : filename, 전역변수)
  makefile <- function(year) {
    result <- NULL # 파일 이름 저장할 변수 설정
    
    for(y in year) {
      if(y == 2015) { # 2015년도 자료 : 8 ~ 12월
        for(m in 8:12) {
          if(m %in% c(8, 9)) {
            result <- c(result, paste0(y, "년_0", m, "월_자동차_등록자료_통계.xlsx"))
          } else {
            result <- c(result, paste0(y, "년_", m, "월_자동차_등록자료_통계.xlsx"))
          }
        }
      } else if(y == 2020) { # 2020년도 자료 : 1 ~ 6월
        for(m in 1:6) {
          result <- c(result, paste0(y, "년_0", m, "월_자동차_등록자료_통계.xlsx"))
        }
      } else { # 그 외 연도 자료 : 1 ~ 12월
        for(m in 1:12) {
          if(m %in% 1:9) {
            result <- c(result, paste0(y, "년_0", m, "월_자동차_등록자료_통계.xlsx"))
          } else {
            result <- c(result, paste0(y, "년_", m, "월_자동차_등록자료_통계.xlsx"))
          }
        }
      }
    }
    return(result)
  }
  
  filename <- makefile(2015:2020)
  
  # 2. 엑셀 파일 불러들이기
  library(readxl)
  for(i in 1:length(filename)) {
    
    # 행 : 전기차(72~86행), 수소차(242~256행)
    # 열 : 연료별, 종별, 용도별, 소계(1, 2, 3, 21열)
    mydata <- read_xlsx(paste0(foldname, "/data/", filename[i]), sheet = 10, skip = 2)[c(72:86, 242:256), c(1:3, 21)]
    names(mydata) <- c("연료별", "종별", "용도별", "소계") # 열 이름 부여
    
    # NA 채우기
    mydata[1:15, 1] <- "전기"
    mydata[16:nrow(mydata), 1] <- "수소"
    for(j in 1:nrow(mydata)) {
      if(is.na(mydata$종별[j]) == TRUE) {
        mydata$종별[j] <- mydata$종별[j-1]
      }
    }
    
    # 3. 엑셀 파일 쓰기(for문 안에서 실행)
    library(openxlsx)
    if(file.exists("전기, 수소차 전처리 결과.xlsx") == FALSE) {
      write.xlsx(mydata, file = "전기, 수소차 전처리 결과.xlsx", sheetName = paste0(substr(filename[i], 1, 5), " ", substr(filename[i], 7, 9)))
    } else {
      workbook <- loadWorkbook("전기, 수소차 전처리 결과.xlsx")
      addWorksheet(workbook, sheetName = paste0(substr(filename[i], 1, 5), " ", substr(filename[i], 7, 9)))
      writeData(workbook, sheet = paste0(substr(filename[i], 1, 5), " ", substr(filename[i], 7, 9)), mydata)
      saveWorkbook(workbook, "전기, 수소차 전처리 결과.xlsx", overwrite = T)
      rm(workbook); rm(mydata)
    }
    
  }
}

## 함수 실행(Function execution)
# 1. 작업공간 설정
dr <- "C:/"
foldname <- paste0(dr, "Users/mazy4/Desktop/ISTANS/5. 분석 data/1. 수소차 데이터")
setwd(foldname)

# 2. 폴더 생성 이후 다시 작업 공간 설정
makefold <- "result"
dir.create(makefold) # 폴더 생성
newfoldname <- paste0(foldname, "/", makefold)
setwd(newfoldname)

# 3. 함수 실행
carprocess(filename)
