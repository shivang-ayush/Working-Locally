x= int(input('Enter the value of x', ))
n=int(input('Enter the exponential value of x', ))
s=x
for i in range(2, n+1):
    f=1
    for k in range(1, i+1):
        f=f*i
    term=(x**i)/f
    s=s+term
print(f"The sum of the series up to {n} terms is: {s}")


x= int(input('Enter the year', ))
a= x%4
if a == 0:
    print(x, "is a leap year.")
else:
    print(x, "is not a leap year.")