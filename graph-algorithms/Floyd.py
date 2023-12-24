def Floyd(dist):     
    for a in range(6):   
        for i in range(6): 
            for j in range(6):  
                dist[i][j] = min(dist[i][j], dist[i][a] + dist[a][j]) 
    for i in range(6): 
        for j in range(6): 
            if(dist[i][j] == 999): 
                dist[i][j] = ("∞")
    print(dist)
  
dist =[[0,999,999,999,10,999], 
       [999,0,999,9,999,999], 
       [999,15,0,999,6,999], 
       [999,999,7,0,999,999], 
       [999,15,999,999,0,5], 
       [8,999,999,999,999,0]]

Floyd(dist)
