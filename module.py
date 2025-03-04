
for i in range(25):
    n=int(input('ENTER the number, whose factorial you wants', ))
    fact=1
    for i in range(1,n+1):
        fact=fact*i
    print(fact)
    

import statistics
a=[input('enter the list of digits, separated by comma',)]
# a=[2, 3, 5, 6, 9, 25, 2, 3, 6]
print("Mean of the given data is", statistics.mean(a))
print("Median of the given data is", statistics.median(a))
print("Mode of the given data is", statistics.mode(a))