import numpy as np

ll = [0.76, 0.06, 0.16, 0.79, 0.54, 0.59, 0.17, 0.88, 0.50, 0.93, \
      0.33 ,0.64 ,0.51, 0.73 ,0.23 ,0.62]
llaux, llbien = -np.log(ll), []
for k in llaux:
    llbien.append(round(k, 2))

c1unif = [0.12, 0.03, 0.30, 0.38, 0.79, 0.46, 0.19, 0.50, 0.14, \
          0.60, 0.44, 0.79, 0.01, 0.99, 0.63, 0.37]; c1bien = []
for i in c1unif:
    if i <= 0.2:
        c1bien.append(0.6)
    if i > 0.2 and i <= 0.7:
        c1bien.append(0.7)
    else:
        c1bien.append(0.8)

c2unif = [0.74, 0.73, 0.42, 0.91, 0.02, 0.49, 0.71, 0.62, 0.98, \
          0.59, 0.05, 0.17, 0.24, 0.78, 0.84, 0.40]; c2bien = []
for j in c2unif:
    if j <= 0.1:
        c2bien.append(0.8)
    if j > 0.1 and j <= 0.8:
        c2bien.append(0.9)
    else:
        c2bien.append(1)

print(llbien); print(c1bien); print(c2bien)

"""
[0.27, 2.81, 1.83, 0.24, 0.62, 0.53, 1.77, 0.13, 0.69, 0.07, 1.11, 0.45, 0.67, 0.31, 1.47, 0.48]
[0.6, 0.8, 0.6, 0.8, 0.7, 0.7, 0.8, 0.7, 0.6, 0.8, 0.7, 0.6, 0.8, 0.7, 0.7, 0.8, 0.6, 0.8, 0.8, 0.7, 0.7]
[0.9, 0.9, 0.9, 1, 0.8, 1, 0.9, 0.9, 0.9, 1, 0.9, 0.8, 1, 0.9, 0.9, 0.9, 1, 0.9]
"""
