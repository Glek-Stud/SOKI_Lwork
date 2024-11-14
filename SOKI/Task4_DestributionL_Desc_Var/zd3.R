a = 10
o = 5
alph = 8
beta = 15
gama = 10

P1 = pnorm(beta, a, o) - pnorm(alph, a, o)
P1 

P2 = 2*(pnorm(gama, 0, o) - 0.5)
P2