library("tidyverse")
library(gmp)
#loading data
data<-read.csv('C:\\Users\\91824\\Downloads\\Salaries.csv')#loads data


#selecting only necessary columns

data <- data %>% select(c("OvertimePay","OtherPay","Benefits","BasePay","Year"))
head(data)
tail(data)
#selectiong the data in 2014

data <- data %>% filter(Year==2014)


#removing the columns Which are Not Provided
data <- data %>% filter(BasePay!="Not Provided")
data <- data %>% filter(OtherPay!="Not Provided")
data <- data %>% filter(Benefits!="Not Provided")
data <- data %>% filter(OvertimePay!="Not Provided")

data$BasePay <- as.numeric(data$BasePay)
data$OtherPay <- as.numeric(data$OtherPay)
data$Benefits <- as.numeric(data$Benefits)
data$OvertimePay<- as.numeric(data$OvertimePay)


#removing the unnecessary columns
data <- data %>% filter(OtherPay >= 0) 



data <- data %>% filter(BasePay >= 0)
data <- data %>% filter(OvertimePay >= 0)
data <- data %>% filter(Benefits >= 0)
head(data)
tail(data)

#dropping year column
data <- data %>% select(-c('Year'))
head(data)


data[data==""] <- 0

#Add Overtime Pay, Other Pay and benefits together to create only one new column 
data$Response <- data$OtherPay + data$OvertimePay + data$Benefits

#plottings

plot(x=data$BasePay, y=data$OtherPay,main =   'base pay vs other pay' , xlab = 'basepay', ylab = 'otherpay',col= 'blue')
plot(x=data$BasePay, y=data$OvertimePay,main =   'base pay vs overtime pay' , xlab = 'basepay', ylab = 'overtimepay',col= 'blue')
plot(x=data$BasePay, y=data$Benefits,main =   'base pay vs bebefits' , xlab = 'basepay', ylab = 'Benefits',col= 'blue')
plot(x=data$BasePay, y=data$Response,main =   'base pay vs ALLTOGRTHER' , xlab = 'basepay', ylab = 'alltogether',col= 'red')


#splitting in train and testing data
set.seed(222)

sample_size = round(nrow(data)*.70) # setting what is 70%
index <- sample(seq_len(nrow(data)), size = sample_size)

train <- data[index, ]
test <- data[-index, ]
head(train)
head(test)

x<-train$BasePay
y<-train$Response


# simple linear Regression
b_val <- function(x,y){
  m_x= mean(x)
  m_y = mean(y)
  b_num = sum((x-m_x)*(y-m_y))
  b_din = sum((x-m_x)^2)
  b= b_num/b_din
  return(b)
}

a_val <- function(x,y,b){
  xm = mean(x)
  ym = mean(y)
  a= ym - b*xm
  return(a)
}


plot_regression_line <- function(x,y,a,b){
  print(a)
  print(b)
  y_pred = a + b*x
  plot(x, y,main =   'base pay vs ALLTOGRTHER' , xlab = 'basepay(x)', ylab = 'alltogether(Y)',col= 'green')
  lines(x,y_pred,col="blue")
  
  return(y_pred)
}


R2_val <- function(y,y_pred){
  ym= mean(y)
  sst = sum((y-ym)^2)
  print(sst)
  sse = sum((y-y_pred)^2)
  print(sse)
  r2 = 1-(sse/sst)
  r2
  return(r2)
}
  
b <- b_val(x,y)
cat("the value of b in linear is =",b)
a <- a_val(x,y,b)
cat("the value of a in linear is =",a)
y_pred_linear<-plot_regression_line(x,y,a,b)
R2_linear <- R2_val(y,y_pred_linear)
cat("the value of R^2 in linear is ",R2)


#non linear regression with 2 degree

deg2_coeff<-function(x,y){
        A <- rbind(c(sum((x)^4), sum((x)^3),sum((x)^2)),
           c(sum((x)^3), sum((x)^2), sum(x)),
           c(sum((x)^2), sum(x), length(x)))
        B <- c(sum(((x)^2)*y), sum(x*y),sum(y)  )
        b_deg2<-solve(A,B,tol = 1e-21)
        return(b_deg2)
}

plot_deg_2<-function(x,y,b){
  print(b[1])
  print(b[2])
  print(b[3])

  y_pred2 = b[3] + b[2]*x + b[1]*x**2
  matplot(x, cbind(y,y_pred2),col=c("blue","red"),pch = 20,ylab = "All Together",xlab ="BasePay",main="non linear degree 2")
  return(y_pred2)
  lines
  
}

b_deg2 <- deg2_coeff(x,y)
cat("The coefficients in non linear deg 2 are : ",b_deg2)
y_pred_deg_2<-plot_deg_2(x,y,b_deg2)
#y_pred_deg_2
R2_de2 <- R2_val(y,y_pred_deg_2)
cat("the value of R^2 in Non linear in degree 2 = ",R2_de2)

#non linear regression with degree 3

deg3_coeff<-function(x,y){
        A3 <- rbind(c(sum((x)^6), sum((x)^5),sum((x)^4),sum((x)^3)),
            c(sum((x)^5), sum((x)^4),sum((x)^3),sum((x)^2)),
            c(sum((x)^4), sum((x)^3), sum((x)^2),sum((x))),
            c(sum((x)^3), sum((x)^2), sum(x),length(x)))
        B3 <- c(sum(((x)^3)*y),sum(((x)^2)*y),sum(x*y),sum(y) )
        b_deg3<-solve(A3,B3,tol = 1e-32)
        return(b_deg3)
}



plot_deg_3<-function(x,y,b){
  print(b[1])
  print(b[2])
  print(b[3])
  print(b[4])
   y_pred3 = b[4] + b[3]*x**1 + b[2]*x**2 + b[1]*x**3
   matplot(x, cbind(y,y_pred3),col=c("blue","red"),pch = 20,main="Non Linear Degree 3",xlab="BasePay",ylab="All Together")
  return(y_pred3)
  
}



b_deg3 <- deg3_coeff(x,y)
cat("The coefficients in non linear deg 2 are : ",b_deg3)
y_pred_deg_3<-plot_deg_3(x,y,b_deg3)
R2_de3 <- R2_val(y,y_pred_deg_3)
cat("the value of R^2 in Non linear in degree 3 = ",R2_de3)


##testing
xtest<-test$BasePay
ytest<-test$Response

predict_dist<-function(x,b){
  print(b[1])
  print(b[2])
  print(b[3])
  print(b[4])
  y_pred3 = b[4] + b[3]*x**1 + b[2]*x**2 + b[1]*x**3
  return(y_pred3)
  
}


predicted_distance<-predict_dist(xtest,b_deg3)
actual_prediction <- data.frame(cbind(actuals=ytest,predicted=predicted_distance))
head(actual_prediction,50)


#model3 <- lm(ytest ~ poly(xtest,degree = 3,raw = T))
#predict_dist <- predict(model3,test)
#actual_prediction_inbult <- data.frame(cbind(actuals=ytest,predicted=predicted_distance))


plotX<-c(1,2,3)

plotY<-c(R2_linear,R2_de2,R2_de3)
plot(plotX, plotY, 
     main= "deg vs R2",
     xlab= "degree",
     ylab= "R2 values",
     col= "green", pch = 19, cex = 1, lwd = 5,type="b")
