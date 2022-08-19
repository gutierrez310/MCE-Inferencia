# -*- coding: utf-8 -*-
"""
Created on Thu Aug 18 13:50:32 2022

@author: carlos
"""

""" PSEUDOCODIGO"""
"""
Funcion p(n, i)
  prod <- 0
  Para j <- 0 Hasta i Hacer
    prod <- prod / (n - j)
  Fin Para
  Retornar prod
Fin p

Funcion factorial(n)
  prod <- 1
  Para i <- n Hasta 1 Paso -1 Hacer
    prod <- prod * i
  Fin Para
  Retornar prod
Fin factorial

Funcion sol(n)
  sol <- 0
  Para i <- 1 Hasta n + 1 Hacer
    sol <- sol + factorial(n)/(factorial(n-i)*factorial(i)) * p(n, i) * (-1)^(i-1)
Fin sol

Escribir sol(5)
Escribir sol(10)
Escribir sol(20)
Escribir sol(100)

"""

from math import comb


def p(n, i):
    prod = 1
    for j in range(i):
        prod /= n - j
    return prod

def sol(n):
    sol = 0
    for i in range(1,n+1):
        sol += comb(n, i)*p(n, i)*pow(-1,i-1)
    return sol

def factorr(n):
    prod=1
    for i in range(n,1,-1):
        prod*=i
    return prod

print(sol(5))
print(sol(10))
print(sol(20))
print(sol(100))
