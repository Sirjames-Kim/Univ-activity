# 각 범주와 표준화잔차를 plot으로 그리기 위해서 6개의 범주를 만든다.

# for문을 사용하기위해서는 먼저 값을 할당해줘야한다.
var1<-rep(0,46)


for(i in 1:46){
  if (data[i,3]==1&&data[i,4]==0){
    var1[i]=1 
  }
  else if(data[i,3]==1&&data[i,4]==1){
    var1[i]=2 
  }
  else  if(data[i,3]==2&&data[i,4]==0){
    var1[i]=3 
  }
  else  if(data[i,3]==2&&data[i,4]==1){
    var1[i]=4 
  }
  else  if(data[i,3]==3&&data[i,4]==0){
    var1[i]=5 
  }
  else var1[i]=6
  
}



# 고용전 검사 프로그램 데이터
data.2 <- read.table("preemployment_testing.txt",header=T)

attach(data.2)

# TEST : 고용전검사점수 (설명변수)
# RACE : 지원자, 1:소수민족 지원자, 0: 백인 지원자 (설명변수)
# JPERF : 업무수행능력 (반응변수)





# 2차항 추가모델
# 2차항 변수 지정
X2 <- X^2
B <- lm(log(Y) ~ X + X2)

# 다른방법

B <- lm(log(Y) ~ X + I(X^2))

# 2차항이 추가된 모델의 잔차 플랏에서는 2차항의 모양이 보이지않는다.

plot(X,rstandard(B))
