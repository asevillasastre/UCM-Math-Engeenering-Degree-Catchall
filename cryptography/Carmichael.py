import math

def isprime(n):
    """
    funcion auxiliar que permite comprobar de manera
    eficiente la primalidad de un numero
    """
    for i in range(2, int(math.sqrt(n)) + 1):
        #buscamos intensivamente sus posibles divisores
        if n/i == n//i:
            return False
    return True

def iscarmichael(n):
    """
    permite determinar si un numero es de Carmichael
    """
    #descartamos los no compuestos
    if not isprime(n):
        #probamos todas las bases a posibles
        for a in range(1, n):
            #descartamos las que no son coprimas
            if math.gcd(a, n) == 1:
                #a**(n-1)%n es demqasiado lento
                #por eso usamos la exponenciacion rapida que
                #viene implementada en Python
                if pow(a, n-1, n) != 1:
                    #si encontramos un contraejemplo a
                    #entonces n no es de Carmichael
                    return False
        return True
    return False

#simplemente buscamos los 10 primeros
buenos = []; i = 1
while len(buenos) < 10:
    #si encontramos uno lo añadimos a la lista
    if iscarmichael(i):
        buenos.append(i)
    i+=1

print(buenos)

#efectivamente los encontrados son:
#[561, 1105, 1729, 2465, 2821, 6601, 8911, 10585, 15841, 29341]

