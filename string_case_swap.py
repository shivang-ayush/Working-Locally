for i in range(25):
    i_string = input("Enter a string OR type 'q' to quit.")
    if i_string.lower() == 'q':
        print()
    else:    
        s_string = i_string.swapcase()
        print("Swapped case string:", s_string)
