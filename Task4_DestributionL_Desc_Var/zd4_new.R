f = function(x,y)
  return(a*x*y*exp(-(x^2+y^2)))

Fx = integrate(function(y) { 
  sapply(y, function(y) {
    integrate(function(x) f(x,y), 0, 1)$value
  })
}, 0, 1)$value
Fx

Mx = integrate(function(y) { 
  sapply(y, function(y) {
    integrate(function(x) x*f(x,y), 0, 1)$value
  })
}, 0, 1)$value
Mx

My = integrate(function(y) { 
  sapply(y, function(y) {
    integrate(function(x) y*f(x,y), 0, 1)$value
  })
}, 0, 1)$value
My

Dx = integrate(function(y) { 
  sapply(y, function(y) {
    integrate(function(x) (x^2)*f(x,y), 0, 1)$value
  })
}, 0, 1)$value
Dx

Dy = integrate(function(y) { 
  sapply(y, function(y) {
    integrate(function(x) (y^2)*f(x,y), 0, 1)$value
  })
}, 0, 1)$value
Dy

ox = sqrt(Dx)
ox

oy = sqrt(Dy)
oy 

Mxy = integrate(function(y) { 
  sapply(y, function(y) {
    integrate(function(x) x*y*f(x,y), 0, 1)$value
  })
}, 0, 1)$value
Mxy

uxy = Mxy - Mx * My
uxy

r = uxy/(ox*oy)
r
