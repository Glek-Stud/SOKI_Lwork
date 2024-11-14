f = function(x){
  ifelse(x <= 5, 0, ifelse(5 < x & x <= 7, (((x - 5)^2)/4), ifelse(x > 7, 1, 0)))
}

x = seq(5,7,0.001)
y = f(x)

plot(x , y , col = c('red'), lwd = 2, xlab = "x", main = "Yakovenko O, KI-214", ylab = "y", type = 's')

P = f(beta) - f(alfa) 
P


deriv(f(x), "x")

var1 = (D(0, "x"))
var1
#0

f1 = expression(((x - 5)^2)/4)
var2 = D(f1, "x")
var2
#(2 * (x - 5)/4)

var3 = D(1, "x")
var3
#0

f2 = function(x){
  ifelse(x <= 5, 0, ifelse(5 < x & x <= 7, (2 * (x - 5)/4), ifelse(x > 7, 1, 0)))
}

x1 = seq(5,7,0.001)
y1 = f2(x)
plot(x1 , y1 , col = c('red'), lwd = 2, xlab = "x", main = "Yakovenko O, KI-214", ylab = "y", type = 's')

par(mfrow=c(2,1))

integrand = function(x) {(2 * (x - 5)/4)*x}
Inter = integrate(integrand, lower = 5, upper = 7)
V1 = Inter$value
V1

integrand = function(x) {(2 * (x - 5)/4)*(x^2)}
Inter = integrate(integrand, lower = 5, upper = 7)
V2 = Inter$value
V2

integrand = function(x) {(2 * (x - 5)/4)*(x^3)}
Inter = integrate(integrand, lower = 5, upper = 7)
V3 = Inter$value
V3

integrand = function(x) {(2 * (x - 5)/4)*(x^4)}
V4 = integrate(integrand, lower = 5, upper = 7)$value
V4

Mx = V1
Mx

Dx = V2 - V1^2
Dx

ox =sqrt(Dx)
ox

u3 = V3 - 3*V2*V1 + 2*V1^3 
u3

u4 = V4 - 4*V3*V1 + 6*V2*V1^2 - 3*V1^4
u4

A = u3/(ox^3)
A
E = u4/(ox^4) - 3
E

u4 = V4 - 4*V3*V1 + 6*V2*V1^2 - 3*V1^4

#Mediana

#((x - 5)^2)/4 = 0.5
#x^2 - 10x + 23 = 0

sqD = sqrt((-10)^2 - 4 * 1 * 23)
sqD

x1 = (10 + sqD)/ 2
x2 = (10 - sqD)/ 2
x1
x2
# 5 < x2 < 7
Me = x2
Me 
