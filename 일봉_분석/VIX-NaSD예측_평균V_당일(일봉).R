library(readxl)
library(dplyr)

# 파일 업로드
VIX_d <- read_excel(file.choose())
NaSD_d_익일 <- read_excel(file.choose())
NaSD_d_당일 <- read_excel(file.choose())

View(VIX_d)
# 전처리
NaSD_d_익일 <- na.omit(NaSD_d_익일)

V <- VIX_d[c(1,3,4)]
N <- NaSD_d_당일[c(1,3,4)]

View(V)
View(N)

# 파생변수 생성
N$변동폭 <- N$고가-N$저가
V$평균 <- (V$고가+V$저가)/2

N_n <- N[c(1,4)]
V_n <- V[c(1,4)]

View(N_n)

# 데이터 결합
merge <- merge(V_n, N_n, by='일자 / 시간')
View(merge)
head(merge)


# 상관 관계 분석
cor(merge$평균, merge$변동폭)
cor.test(merge$평균, merge$변동폭) 
#p-value < 0.5(유의수준) 이므로 두 변수는 상관관계를 가짐.



# 선형 회귀 분석
m1 <- lm(변동폭~평균, merge)
# y=11.23x-59.97



summary(m1)
# p-값은 0.5 이하로 회귀계수의 추정치들이 통계적으로 유의
# 결정계수(R-squared)=0.5597로, 회귀식이 데이터를 적절하게 설명하고 있다고 볼 수 없다.


plot(변동폭 ~ 평균, data= merge)
abline( h = mean(merge$변동폭), lty=2, col = 'blue')
abline( v = mean(merge$평균), lty=2, col = 'blue')
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
max(pre_merge$차이)

# 엑셀 파일로 저장
install.packages("writexl")
library(writexl)
write_xlsx(pre_merge,path = "/Users/A/Desktop/일봉예측값_평균(당일).xlsx")

#------------------------------------------
# 비선형 회귀 분석
a = merge$평균
b = merge$평균^2
m2 <- lm(merge$변동폭~a+b)
# y=-0.005779x^2+11.596945x-64.706552

summary(m2)
#R-squared :0.5598

predict02 <- fitted(m2)
pre_merge02 <- cbind(pre_merge,predict02)
View(pre_merge02)

colnames(pre_merge02)[6] <- '제곱예측값'

