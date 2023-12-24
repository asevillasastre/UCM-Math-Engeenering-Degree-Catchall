def bellman(grafo, fuente):
    
    anterior ={}
    d = {}
    for i in grafo:
        d[i] = 99
        anterior[i] = 0
    d[fuente]=0

    for i in range(len(grafo) - 1):
        for n in grafo:
            for adyacente in grafo[n]:
                if d[n]+grafo[n][adyacente]<d[adyacente]:
                    anterior[adyacente]=n
                    d[adyacente]=d[n]+grafo[n][adyacente]
    for i in grafo:
        for adyacente in grafo[i]:
            assert d[adyacente] <= d[i] + grafo[i][adyacente]
    return (d, anterior)
    

grafo = {"a": {"b": 99, "d":  10, }, "b": {"a":  10, "c":  -8},"c": {"a":  -7},"d": {"c":  22},"e": {"a": -11, "c" : 63}}
d, anterior = bellman(grafo, fuente = "a")
print(d)
grafo 