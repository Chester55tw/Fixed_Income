#408 HW4 Cohort2 Group1
#Chester Chen (yu-chen.chen.2023@anderson.ucla.edu)
#Jay Singh Chauhan (jay.chauhan.2023@anderson.ucla.edu)
#Ao Zhan (ao.zhan.2023@anderson.ucla.edu)
#Xuefei (Faye) Sun (xuefei.sun.2023@anderson.ucla.edu)
#Dongyu Huang (dongyu.huang.2023@anderson.ucla.edu)

library(readxl)
dat = read_excel('/Users/chester/Desktop/Quarter 2/Fixed Income/HW/４/Homework_4_Data_Paths.xlsx',skip = 3)
##1
aar<-(rowSums(dat))/11
aar
D<-matrix(nrow = 10, ncol = length(aar))
for (i in 1:10) {
  D[i,] <- exp(-i * aar)
}
print(D)
##2
cap = matrix(nrow = 10)
for(i in 1:10){
  cap[i]= 1/nrow(dat)*sum(exp(-dat[,i]*i)*(abs((dat[,i]-0.037))+(dat[,i]-0.037))/2)
}
print(cap[1:5])
##3
floor = matrix(nrow = 10)
for(i in 1:10){
  floor[i]= 1/nrow(dat)*sum(exp(-dat[,i]*i)*(abs((0.02-dat[,i]))+(0.02-dat[,i]))/2)
}
print(floor[1:5])

##4
dat[!sapply(dat, function(x) !is.na(x)&x >= 0.02 & x <= 0.04)] <- 0
PV = rep(0,200)
for (i in 1:200){
  PV[i] = sum(D[,i]*dat[i,]*100) +D[,i][10]*100
}
PV

##5
inv_floater = rep(0,200)
for(i in 1:200){
  cash_flow = 2*dat[i,1] - 0.06
  inv_floater[i] = sum(D[,i]*cash_flow*100) + D[,i][10]*100
}
inv_floater

##6
K = 0.04
caplet = rep(0,200)
for(i in 1:200){
  caplet[i] = exp(-0.03*1) * max(dat[i,5]-K, 0)
}
C_0_1 = mean(caplet)
#just making sure dat[i,5]-K is negative so C_0_1= 0 is normal
dat[i,5]-K

K = 0.04
call_avg = rep(0,200)
for(i in 1:200){
  r_avg = sum(dat[i,2:6])/5
  call_avg[i] = exp(-0.03*5) * max(r_avg-K, 0)
}
C_0_5 = mean(call_avg)

print(C_0_1)
print(C_0_5)
#Compare the outcome of two values
#The bigger number has more value






