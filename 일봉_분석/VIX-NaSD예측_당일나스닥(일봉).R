library(readxl)
library(dplyr)

# 파일 업로드
VIX_d <- read_excel(file.choose())
NaSD_d_익일 <- read_excel(file.choose())
NaSD_d_당일 <- read_excel(file.choose())



V <- VIX_d[c(1,5)]
N <- NaSD_d_당일[c(1,3,4)]

View(V)
View(N)

# 파생변수 생성
N$변동폭 <- N$고가-N$저가

N_n <- N[c(1,4)]

View(N_n)

# 데이터 결합
merge <- merge(V, N_n, by='일자 / 시간')
View(merge)
head(merge)


# 상관 관계 분석
cor(merge$종가, merge$변동폭)
cor.test(merge$종가, merge$변동폭) 
#p-value < 0.5(유의수준) 이므로 두 변수는 상관관계를 가짐.



# 회귀 분석
m1 <- lm(변동폭~종가, merge)

# y=11.33x-60.57

summary(m1)
# p-값은 0.5 이하로 회귀계수의 추정치들이 통계적으로 유의
# 결정계수(R-squared)=0.5711로, 회귀식이 데이터를 적절하게 설명하고 있다고 볼 수 없다.


plot(변동폭 ~ 종가, data= merge)
abline( h = mean(merge$변동폭), lty=2, col = 'blue')
abline( v = mean(merge$종가), lty=2, col = 'blue')
abline( m1, lty=2, col='red')

# 특정값 x의 y 예측
fitted(m1)
View(fitted(m1))
predict(m1, newdata = data.frame(종가 = 4))

# 예측값 다뤄보기
preidct <- fitted(m1)
pre_merge <- cbind(merge,preidct)
View(pre_merge)
colnames(pre_merge)[4] <- '예측값'
str(pre_merge)
pre_merge$차이 <-abs(pre_merge$'변동폭' - pre_merge$'예측값')


# 엑셀 파일로 저장
install.packages("writexl")
library(writexl)
write_xlsx(pre_merge,path = "/Users/A/Desktop/일봉예측값_당일나스닥.xlsx")