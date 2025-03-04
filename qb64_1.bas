Cls
Print "!WELCOME TO THE CITIES FAMOUS DANCE PARTY!"
Input "what is your name"; name$
Print "put 1 if happy else put 2"
Input "are you feeling happy"; h
If 1 < h Then
    Print "Bear,"; Tab(1), name$, "Sorry, you are not allowed to enter the dance party! As, you will spoil the mode."
Else
    Print "Dear,"; Tab(1), name$, "you are allowed to enter the dance party!"
End If
End
