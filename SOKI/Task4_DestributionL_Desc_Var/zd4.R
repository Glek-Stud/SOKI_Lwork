install.packages('cubatue')
require(cubature)

#1
integrand = function(x, y) {(20/7)*(x + y)}
fi = adaptIntegrate(integrand , c(0, 0), c(x , y), x = x, y = y)$integral
fi

#3
f = function(x,y) {x*((20/7)*(x + y))}
F = integrate(function(y) { 
  sapply(y, function(y) {
    integrate(function(x) f(x,y), 0, x^2)$value
  })
}, 0, 1)$value
F

#2
InnerFunc = function(x) {(20/7)*(x + y)}
InnerIntegral = Vectorize(function(y) { integrate(InnerFunc, 0, y)$value})
integrate(InnerIntegral , 0, x)$value

str(integrate(InnerIntegral , 0, x))


#2
InnerFunc = function(x) {(20/7)*(x + y)}
InnerIntegral = Vectorize(function(y) { integrate(InnerFunc, 0, y)$value})
integrate(InnerIntegral , 0, x)$value

integrate(InnerIntegral , 0, x)$value

#//////////////////////////////////////////////////////////////////////////



f = function(x,y)
  return(a*x^2*y)

Mx = integrate(function(y) { 
  sapply(y, function(y) {
    integrate(function(x) x*f(x,y), 0, 1)$value
  })
}, 0, 1)$value
Mx
