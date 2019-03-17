ggplotColours <- function(n = 15, h = c(0, 360) + 15){
  if ((diff(h) %% 360) < 1) h[2] <- h[2] - 360/n
  hcl(h = (seq(h[1], h[2], length = n)), c = 100, l = 65)
}

summerizeModel <- function(x) 
{
  print(summary(x))
  print(x)
  print(AIC(x))
  print(BIC(x))
  print(anova(x, test="Chisq"))
  #print(pR2(x))
  
}

factorizeData <- function(x) 
{
  x$SEX <- factor(x$SEX, levels=c("1", "2"), labels=c("Male", "Female"))
  x$EDUCATION <- factor(x$EDUCATION)
  x$MARRIAGE <- factor(x$MARRIAGE)
  #x$Pay0 <- factor(x$Pay0)
  #x$Pay2 <- factor(x$Pay2)
  #x$Pay3 <- factor(x$Pay3)
  #x$Pay4 <- factor(x$Pay4)
  #x$Pay5 <- factor(x$Pay5)
  #x$Pay6 <- factor(x$Pay6)
  x$DEFAULT <- factor(x$DEFAULT, labels=c("0", "1"))
  
  x$BILL_AMT1[is.na(x$BILL_AMT1)] <- mean(x$BILL_AMT1,na.rm=T)
  x$BILL_AMT2[is.na(x$BILL_AMT2)] <- mean(x$BILL_AMT2,na.rm=T)
  x$BILL_AMT3[is.na(x$BILL_AMT3)] <- mean(x$BILL_AMT3,na.rm=T)
  x$BILL_AMT4[is.na(x$BILL_AMT4)] <- mean(x$BILL_AMT4,na.rm=T)
  x$BILL_AMT5[is.na(x$BILL_AMT5)] <- mean(x$BILL_AMT5,na.rm=T)
  x$BILL_AMT6[is.na(x$BILL_AMT6)] <- mean(x$BILL_AMT6,na.rm=T)
  
  summary(x)
  str(x)
  x
  
}

normalizeData <- function(x) 
{
  origNames <- names(x)
  
  preObj <- caret::preProcess(x[, -c(2:4,6:11,24)], method=c("center", "scale"))
  newData <- predict(preObj, x[, -c(2:4,6:11,24)])
  
  y <- cbind(newData$LIMIT_BAL, x$SEX, x$EDUCATION, x$MARRIAGE, 
             newData$AGE, x$PAY_1, x$PAY_2, x$PAY_3, x$PAY_4, x$PAY_5, x$PAY_6, 
             newData$BILL_AMT1, newData$BILL_AMT2, newData$BILL_AMT3, newData$BILL_AMT4, newData$BILL_AMT5, newData$BILL_AMT6,
             newData$PAY_AMT1, newData$PAY_AMT2, newData$PAY_AMT3, newData$PAY_AMT4, newData$PAY_AMT5, newData$PAY_AMT6,
             x$DEFAULT)
  
  colnames(y) <- origNames
  y <- as.data.frame(y)
  
  str(y)
  summary(y)
  y
  
}


removeIdFeature <- function(x) 
{
  y <- x[,c(2:24)]
  str(y)
  summary(y)
  y
  
}