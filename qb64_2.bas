Let COUNT = 1
Input "YOUR NAME"; NAME$
BEGIN:
Print COUNT; "HELLO"; NAME$
COUNT = COUNT + 1
If COUNT <= 10 Then Goto BEGIN:
Print "LOOP OVER"
End
