row <- c(19.78,	24.22,	13.35,	18.12,	20.98,	19.56,	20.95,	22.94,	17.97,	31.40,
         13.76,	23.97,	12.42,	16.92,	13.85,	29.04,	17.24,	12.47,	13.23,	20.41,
         17.71,	23.68,	18.34,	21.56,	19.76,	38.44,	12.95,	21.06,	30.71,	26.70,
         22.76,	18.41,	5.50,	  16.62,	19.35,	17.31,	25.27,	14.45,	20.54,	20.93,
         22.36,	17.73,	15.92,	23.63,	15.38,	17.28,	18.86,	18.38,	25.28,	14.69,
         25.28,	17.32,	11.01,	20.86,	36.36,	24.46,	21.44,	27.90,	20.43,	22.45,
         15.64,	12.74,	17.89,	27.84,	8.97,	  0.00,	  11.07,	24.50,	21.98,	23.28,
         11.44,	25.69,	22.20,	23.80,	21.19,	17.84,	15.72,	21.93,	24.30,	15.78,
         8.62,	15.05,	23.24,	29.32,	13.66,	13.19,	29.54,	27.60,	24.67,	33.40,
         7.53,	8.10,	  9.99,	  24.94,	27.03, 	27.50,	 18.85, 15.11,	33.32,	30.17)


rowV <- sort(row)
rowV

frec <- table(rowV)
frec

el1 = rowV[1]
el1

k = round(1 + log2(100))
k = 7

h = round((max(rowV) - min(rowV))/k, 1)
h

f = function (x){
  return(el1 + h*x)
}

Ci1 = c(f(0), f(1), f(2), f(3), f(4), f(5), f(6))
Ci1
Ci = c(f(1), f(2), f(3), f(4), f(5), f(6), f(7))
Ci
x0i = c((Ci1 + Ci)/2) 
x0i

ni = c(1, 6, 22, 34, 23, 10, 4)
devNi_N = c(ni / 100)
devNi_N

mi = c(4, 17, 43, 71, 91, 98, 100)
devMi_N = c(mi / 100)
devMi_N

intV = c()



#Tabl

par(mfrow=c(2,2))
plot(x0i,ni, col = c('green'), lwd = 2, xlab = "x0i", main = "Yakovenko O, KI-214", ylab = "ni", type = 'o')

plot(Ci,devMi_N, col = c('blue'), lwd = 2, xlab = "Ci", main = "Yakovenko O, KI-214", ylab = "devMi_N", type = 'o')

hist(row, breaks = seq(min(Ci1), max(Ci), length.out = 8), col = "grey", main = "Yakovenko O, KI-214")


x0i[3]

exd = ni/100
exd

xc <- devMi_N 
xt <- xc[xc >= x0i]    
xt2 <- xt[xt < x0i]
xt2

install.packages("truncgof")
library(truncgof)
plot(edf(rowV, H=7))
plot(edf(xt2, H=7), main = "Yakovenko O, KI-214",xlab = "", ylab = "",col = c('purple'))

#skew(rowV) асиметрия

library(moments)
library(sjstats)
library(psych)
library(lubridate)
library(bpgmm)
library(PerformanceAnalytics)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


xser = mean(rowV, trim = 0.0001)

vector1 = c("Середнє","Стандартна помилка","Медіана","Мода", "Стандртне відхилення", "Дисперсія виборки", "Ексцес", "Асиметрія", "Інтервал", "Мінімум", "Максимум", "Сумма", "Кількість")
vector2 = c(xser, parameters::standard_error(rowV) , median(rowV), getmode(rowV), sd(rowV), var(rowV), kurtosis(rowV, method = "sample_excess"), skew(rowV), max(rowV) - min(rowV), min(rowV), max(rowV),sum(rowV), length(rowV))
m <- matrix(c(vector1, vector2),nrow = 13, ncol = 2)
data3 = as.data.frame(m)
colnames(data3) = NULL
rownames(data3) = NULL
data3

XmedV = (rowV[51] + rowV[52])/2
XmedI = Ci1[4] + h*(100/2 - mi[3])/ni[4]
XmodV = x0i
XmodI = XmodV + h*((ni[4] - ni[3])/(2*ni[4] - ni[3] - ni[5]))

vector3 = c("Мода вибірки за точковими варіаційним рядом за серединами інтервалів: ",
            "Мода вибірки за інтервальним варіаційним рядом: ",
            "Медіана вибірки за варіаційним рядом: ",
            "Медіана вибірки за інтервальним варіаційним рядом: ",
            "Незм. оцінка мет. сподівання нормальної випадкової величини: ",
            "Незміщенна оцінка дисперсії нормальної випадкової величини: ")

vector4 = c(XmodV, XmodI, XmedV, XmedI, mean(rowV, trim = 0.0001), var(rowV))

m <- matrix(c(vector3, vector4),nrow = 6, ncol = 2)
data4 = as.data.frame(m)
colnames(data4) = NULL
rownames(data4) = NULL
data4

y = 0.95
ty = qt((1 + 0.95)/2, 99)
u1 = qchisq(((1 - y)/2), df = 99)
u2 = qchisq((1 + y)/2, 99)

intA1 = xser - ty* (sd(rowV)/sqrt(100))

intA2 =  xser + ty* (sd(rowV)/sqrt(100))

intO2 = (sd(rowV)^2 * 99)/qchisq(((1 - y)/2), df = 99)

intO1 = (sd(rowV)^2 * 99)/qchisq(((1 + y)/2), df = 99)

message("ty: ", ty)
message("U1: ", u1)
message("U2: ", u2)

vector1 = c(round(intA1, digits = 3), round(intO1, digits = 3))
vector2 = c("< a < ", " < o^2 <")
vector3 = c(round(intA2, digits = 3) ,round(intO2, digits = 2))
m <- matrix(c(vector1,vector2, vector3),nrow = 2, ncol = 3)
data5 = as.data.frame(m)
colnames(data5) = NULL
rownames(data5) = NULL
data5

pi = pnorm(Ci,  xser, sd(rowV)) - pnorm(Ci1,  xser, sd(rowV))
pi 


nipi = pi*100
nipi
SummP = (ni - nipi)^2/(nipi)

sumNi = sum(ni)
sumNi_N = sum(devNi_N)
sumPi = round(sum(pi), digits = 5)
sumPiNi = round(sum(nipi), digits = 5)
sumPirs = round(sum(SummP), digits = 5)
Xstat = round(qchisq(1 - 0.05, 7 - 2 - 1), digits = 6)

c.names = c(1,2,3,4,5,6,7)
r.names = c("Ci-1", "Ci", "x0i", "ni", "ni/n", "mi", "mi/n", "pi", "ni*pi", "summP")
m.names = c("Interval val row")
final = array(c(Ci1, Ci, x0i, ni, ni/100, mi, mi/100, pi, nipi, SummP), dim = c(7, 10, 1), dimnames = list(c.names, r.names, m.names))
data8 = as.data.frame(final)

vector1 = c("Sum ni:", "Sum ni/n:", "Sum pi:", "Sum nipi:" , "Sum puason:", "x^2:")
vector2 = c(100, sumNi_N, sumPi, sumPiNi, sumPirs, Xstat)
m <- matrix(c(vector1,vector2),nrow = 6, ncol = 2)
data7 = as.data.frame(m)
colnames(data7) = NULL
rownames(data7) = NULL
data7
data8

