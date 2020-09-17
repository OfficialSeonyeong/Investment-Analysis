library(readxl)
library(dplyr)

# 파일 업로드
VIX_1m <- read_excel(file.choose())
NaSD_1m <- read_excel(file.choose())

# 전처리
NaSD_1m <- na.omit(NaSD_1m)

V <- VIX_1m[c(1,5)]
N <- NaSD_1m[c(1,3,4)]

View(V)
View(N)

# 파생변수 생성
N$차이 <- N$고가-N$저가

N_n <- N[c(1,4)]

View(N_n)

# 데이터 결합
merge <- merge(V, N_n, by='일자 / 시간')
View(merge)
head(merge)

proj01 <- merge %>% select('종가','변동폭')
head(proj01)
View(proj01)

# 상관 관계 분석
cor(proj01)
cor.test(proj01$종가, proj01$변동폭) 
#p-value < 0.5(유의수준) 이므로 두 변수는 상관관계를 가짐.



# 회귀 분석
m1 <- lm(변동폭~종가, proj01)

y=0.40007x-3.69782

summary(m1)
# p-값은 0.5 이하로 회귀계수의 추정치들이 통계적으로 유의
# 결정계수(R-squared)=0.03345로, 회귀식이 데이터를 적절하게 설명하고 있다고 볼 수 없다.


plot(변동폭 ~ 종가, data= proj01)
abline( h = mean(proj01$변동폭), lty=2, col = 'blue')
abline( v = mean(proj01$종가), lty=2, col = 'blue')
abline( m1, lty=2, col='red')

# 특정값 x의 y 예측
fitted(m1)[1]

predict(m1, newdata = data.frame(종가 = 4))
