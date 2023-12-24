class G():
    def __init__(self, vertices):
        self.V = vertices
        self.graph = [[0 for column in range(vertices)] for row in range(vertices)]
    def result(self, p):
        print ("\narista:   peso:\n")
        for i in range(1,self.V):
            print (p[i], ",", i, "    ", self.graph[i][p[i]])
    def bueno(self, c, T):
        min = 99
        for v in range(self.V):
            if c[v] < min and T[v] == False:
                min=c[v]
                elmaspeque=v
        return elmaspeque
    def Prim(self):
        c = [99]*self.V
        p = [None]*self.V
        c[0] = 0
        T = [False]*self.V
        p[0] = -1
        for cout in range(self.V):
            u =self.bueno(c, T)
            T[u] = True
            for v in range(self.V):
                if T[v] == False:
                    if c[v] > self.graph[u][v]:
                        if self.graph[u][v] > 0:
                            p[v]=u
                            c[v]=self.graph[u][v]
        self.result(p)
g = G(5)
g.graph = [[0, 0, 1, 0, 0], [0, 0, 0, 2, 5], [4, 98, 0, 20, 0], [0, 5, 2, 11, 0], [0, 0, 3, 3, 0],[8, 3, 1, 0, 0]]
g.Prim()