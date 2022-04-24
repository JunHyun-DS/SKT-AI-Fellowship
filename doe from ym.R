library(SixSigma)

# Design the experiment (2^3)
ExperimentDesign <- expand.grid(A = gl(2, 1, labels = c("-", "+")),
                                B = gl(2, 1, labels = c("-", "+")),
                                C = gl(2, 1, labels = c("-", "+")))

# Randomize the experiment
ExperimentDesign$ord <- sample(1:8, 8)
ExperimentDesign[order(ExperimentDesign$ord), ]

# Create replicates
ss.data.doe1 <- data.frame(repl = rep(1:2, each = 8),
                           rbind(ExperimentDesign))
ss.data.doe1

# Add responses
ss.data.doe1$response <- c(5.33, 6.99, 4.23, 6.61,
                           2.26, 5.75, 3.26, 6.24,
                           5.7, 7.71, 5.13, 6.76,
                           2.79, 4.57, 2.48, 6.18)
ss.data.doe1




# Get the average score for each experiment design
df=aggregate(response ~ A + B + C,
          FUN = mean, data = ss.data.doe1)

#   A B C response
# 1 - - -    5.515
# 2 + - -    7.350
# 3 - + -    4.680
# 4 + + -    6.685
# 5 - - +    2.525
# 6 + - +    5.160
# 7 - + +    2.870
# 8 + + +    6.210

# Get restuls

model=lm(품질척도~.,data=df)

summary(model)

#품질척도=하중*2.45+위치*-0.02+속도*-1.86

model$coefficients

res=as.matrix(cbind(1,iris[,1:3]))%*%t(t(model$coefficients))


simul=cbind(iris[,1:3],res)




#하중이랑 위치,속도 2^k설계
#하중의 - 는 20kg, + 100kg
#위치의 - 는 5, + 10
#속도의 - 는 3, + 5

#인자가 세개고 #레벨이 두개



names(df)=c("하중","위치","속도","품질척도")
df

#조금의데이터로 많은데이터를만든다? -> 모델을만들면,,,, 내가 무슨값을넣었을때 아우풋이나오잖어,,,,, 그럼 그게데이터지,,,,

