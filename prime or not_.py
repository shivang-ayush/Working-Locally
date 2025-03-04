a= int(input('enter the number whose nature you wants to find!', ))
lim= int(a/2)+1
for i in range(2, lim):
    rem=a%i
    if rem==0:
        print('the entered number is not a prime/1 i.e.,', a)
        break
    else:
        print('the entered number is a prime/1 i.e.,', a)