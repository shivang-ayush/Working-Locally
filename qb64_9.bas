Cls
Print Tab(500); "CALCULATOR"
Print Tab(500); "BY SHIVANG AYUSH"
Dim NUM1 As Single
Dim NUM2 As Single
Do While REP$ <> "N"
    Input "ENTER THE FIRST NUMBER"; NUM1
    Input "ENTER THE OPERATION (+,-,*,/)"; OPERATOR$
    Input "ENTER THE SECOND NUMBER"; NUM2
    If OPERATOR$ = "+" Then
        Print NUM1 + NUM2
    End If
    If OPERATOR$ = "-" Then
        Print NUM1 - NUM2
    End If
    If OPERATOR$ = "*" Then
        Print NUM1 * NUM2
    End If
    If OPERATOR$ = "/" Then
        Print NUM1 / NUM2
    End If
    Input "PLEASE PRESS ENTER TO CALCULATE AGAIN"; REP$
    REP$ = UCase$(REP$)
Loop
End


