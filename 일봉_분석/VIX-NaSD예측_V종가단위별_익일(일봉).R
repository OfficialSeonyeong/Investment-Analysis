library(readxl)
library(dplyr)

# 파일 업로드
VIX_d <- read_excel(file.choose())
NaSD_d_익일 <- read_excel(file.choose())
NaSD_d_당일 <- read_excel(file.choose())

View(VIX_d)
# 전처리
NaSD_d_익일 <- na.omit(NaSD_d_익일)

V <- VIX_d[c(1,3,4,5)]
N <- NaSD_d_익일[c(1,3,4)]

View(V)
View(N)

# 파생변수 생성
N$변동폭 <- N$고가-N$저가
V$VIX변동 <- V$고가-V$저가

N_n <- N[c(1,4)]
V_n <- V[c(1,4,5)]

View(N_n)
View(V_n)

# 데이터 결합
merge <- merge(V_n, N_n, by='일자 / 시간')
View(merge)
head(merge)


# 종가 범위 나누기
j10_15 <- merge %>% subset(10 <=merge$종가 & merge$종가<15)
j15_20 <- merge %>% subset(15 <=merge$종가 & merge$종가<20)
j20_25 <- merge %>% subset(20 <=merge$종가 & merge$종가<25)
j25_30 <- merge %>% subset(25 <=merge$종가 & merge$종가<30)
j30_35 <- merge %>% subset(30 <=merge$종가 & merge$종가<35)
j35_40 <- merge %>% subset(35 <=merge$종가 & merge$종가<40)
j40_ <- merge %>% subset(40 <=merge$종가)

View(j40_)
# 10<= 종가 < 15 일 때, 변동성 확인

# 상관 관계 분석
cor(j10_15$VIX변동, j10_15$변동폭)
cor.test(j10_15$VIX변동, j10_15$변동폭) 
#p-value < 0.5(유의수준) 이므로 두 변수는 상관관계를 가짐.


# 회귀 분석
m1 <- lm(변동폭~VIX변동, j10_15)

# y=5.182x+85.101

summary(m1)
# p-값은 0.5 이하로 회귀계수의 추정치들이 통계적으로 유의
# 결정계수(R-squared)=0.003422로, 회귀식이 데이터를 적절하게 설명하고 있다고 볼 수 없다.


plot(변동폭 ~ VIX변동, data= j10_15)
abline( h = mean(j10_15$변동폭), lty=2, col = 'blue')
abline( v = mean(j10_15$VIX변동), lty=2, col = 'blue')
abline( m1, lty=2, col='red')




# 20<= 종가 < 25 일 때, 변동성 확인

# 상관 관계 분석
cor(j20_25$VIX변동, j20_25$변동폭)
cor.test(j20_25$VIX변동, j20_25$변동폭) 
#p-value < 0.5(유의수준) 이므로 두 변수는 상관관계를 가짐.


# 회귀 분석
m1 <- lm(변동폭~VIX변동, j20_25)

# y=46.55x+139.83

summary(m1)
# p-값은 0.5 이하로 회귀계수의 추정치들이 통계적으로 유의
# 결정계수(R-squared)=0.2281로, 회귀식이 데이터를 적절하게 설명하고 있다고 볼 수 없다.


plot(변동폭 ~ VIX변동, data= j20_25)
abline( h = mean(j20_25$변동폭), lty=2, col = 'blue')
abline( v = mean(j20_25$VIX변동), lty=2, col = 'blue')
abline( m1, lty=2, col='red')

# 결론: 범위를 나눴을 때 데이터 값이 적어 유의미한 결과가 나오지 않아 중단.