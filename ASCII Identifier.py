# Work in progrees! Need modification! Will updated soon.

for i in range(50):
    k= input("Wants to Enter the ASCII code: OR Enter the digit or character or symbol:"
             "Chosse 1 OR 2:    ", )
    q= input("Enter the ASCII code or digit or character or symbol whose reverse is to be find out.", )

    if k == 1:
        if q.isdigit()== True:
            print("The ASCII value of", q, "is", chr(q))
        else:
            print("The ASCII value of", q, "is", chr("q"))

    else: #k=2
        if q.isalpha()== True:
            print("The ASCII value of", q, "is", ord("q"))
        else:
            print("The ASCII value of", q, "is", ord("q"))