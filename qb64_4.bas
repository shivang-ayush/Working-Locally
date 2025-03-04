_Title "Chess 2017-11-22 T .bas"

' 22 10 2017 New fix to Incheck function, I hope the last!
' 22 10 2017  empowered promotion by mouse
' 22 10 2017  fix promotion visible only after move of black
' 22 10 2017 castle by mouse
' 22 10 2017  empowered castling control, now you cannot castle if they are in check
'             or across a square that is under Black control
' 22 10 2017 translated in sub Info_Help screen
' 22 10 2017  translate in sub Fellippe's Wait click and key
' 22 10 2017 added logical end to the main of program
' 22 10 2017 put away all code REMMED not yet used

'2017 10 24 changed board graphics, arranged alpha order subs, updated highlite square
' Thanks Adrian Huang
' http://www.thejoyfulprogrammer.com/qb64/forum/showthread.php?tid=401&rndtime=1508625987687357559

'2017 10 24 Incorporate TempodiBasic's legalShow and legalHide routines
' fix pawn promotions see Ppromote$
' count pieces captured and show with board
' install game recorder

' 27 10 2017 color of chessboard are CONST
' resizing square and character size to  42
' moving list of piece captured at top right
' moving output at bottom right
' new SUB ShowSetup that shows whole setup to choose pieces for white and black,
' or only pieces capturable (No king because when a King falls game stops!)
' or only pieces for promotion (Knight Bishop Rook Queen) for both colors
' now if Black resigns the flow of program ends after main do loop to let replay the game
' play again label structure and initialization instruction (it is possible to use a DO LOOP)
' Added ButtonMenuBar at bottom using MakeButton SUB
' Button QUIT working, button PLAY works for now let us play only as WHITE
' Build an area for list of moves
' build an area for output/feedback of program
' Changed Helpscreen instruction including mouse selection
' >>>> wow lots of new stuff

'2017-10-29/30 fixed dots in empty squares, highlight piece clicked, thing crawling under board while computer thinks
' now if we can only get the colors ;-)) Outer loop to restart game, elimate some boxes, less cluttered looking.
' change title to version = date easier to track

'2017 10 31 T  Yes I follow you in using date also if in my country I use DD-MM-YYYY   :-)
'  YES fixed BIG BUG about restart and Play Again lasting the last moves in memory... and in File Recorder

'2017-11-01 B review TempodiBasic's BIG BUG fix, clean up lines, comments and variables
' I (bplus) see some work also done in IO for checking checks, I assume.

'2017-11-03 B Move Intro to before main program loop, fix some things with promotion
' prompt for pieces, pWflag$, pBflag$ to add to move list

'2017-11-04 B hack AI this is just an experiment for my cat who is always curious specially
' because it exists in another universe.

'2017-11-06 A 2nd post (really posted 11-05) Added/revised code for rotating board to play Black.
' Adrain has change 5000 number to 9000 im several places and I don't know if this fixes castle
' It did fix notation in the moves list correctly

'2017-11-05 B  from Adrian's 2nd post 2017-11-06( posted 11-05) A #2   continuing with hackBlack hacks!
' I noticed that code changes were very alike and one fuction would save quite a bit of code FIXED
' I also noticed the captured pieces were backwards when Black plays FIXED
' I noticed Black cant castle yet BIGGEST FIX of them all!!!
' I noticed Ctrl+K for human playing Black did not work, well had to translate move back so
' IO can reverse again!!  FIXED!
' Check pawn promotion and track down source of mystery q added to computer's moves.
' Still allot of goofy things...

'2017-11-07 A - removed fake 1st move while playing black

'2017-11-07 B hack 2 using above mod but with EXIT SUB instead of GOTO
' fix capture counts so that only positive counts show
' Ah found source of allot of goofyness!  hackBlack > playBlack per Adrian
' I think I have this working correctly through Play Again Y/N?

'2017-11-07 T put away GOTO for label 100 to play again
' moved in main loop AI resign (strange as white it capture by King and then when you takes King it resign!!!
' GOOD first AI goes on playing without king :-)
'
'2017-11-07 2B removed 100 next to loop since no goto, fixed comments on separate line
' and added some comments, I think I have finally tracked down source of extra q's!!! FIXED

' 2017-11-10 T  fixed promotion  failure with strange choose made clicking on the other side of window of program
' at the Y of piece to get by promotion
'   Restored  MAXLEVEL at beginning of program to set power of AI
'
' 2017-11-10 2T cutting some old comments no more useful for use about issues yet resolved
' packing the 2 initboard to one (they do the same work with 2 different labels)
'
'2017-11-11 B Put T comments on separate line from code.
'Recommend making MAXLEVEL a CONST if we will not change it ever.
'Tell why two different boards are needed to initialize the board positions.
'Fix pWfalg$ again! that AI has no business changing BUT OK save ElSE block because AI
'may need to plan for eventual pawn promotion and assuming a Queen is smart assumption.
'
' 2017-11-13 T  fixed incheck failure and strange moves of king under check using a new shared flag Turn
' to let analize only the important pieces by INCHECK Function, they are different for White and for Black
' re-written in MAKEMOVE SUB (sub that makes the move on the board and check for promotion and makes the promotion if is the case
' the ELSE.. END IF part for white (Human) player is needed together INTFLAG  =1 humang chooses by himself piece in promotion, =0 autochoosed Queen
' also fixed extra q by inizilization of pWflag$ at start of SUB
'
'2017-11-15 B start work on Undo, Save and Load
' 2 functions cp2n(piece$), cn2p$(number), for converting a board to a bString$ letter and vice versa
' bString$ will get all board positions loaded in 64 character string
' bSetyp will setup a board from a bString$ used for UNDO, Save, Load and future SETUP
' Center text in a button
' Now reworked initBoard so initBlack sub can be eliminated and initBoard shrunk, now eliminated also!
' Now add stuff to make UNDO possible. new Boards$() array works along with Move$(), Boards$() has bString$ of board positions
' Incorp TempodiBasic changes to AI checking for Check (Chess 2_17-11-13 T) :
'    Use new glabal varaible Turn = 1 for White's and -1 for Black, might come in handy! Yes on loading Game
'        Eliminate the infinite crap I'd rather have AI King commiting suicide as a form of Resign for testing all other stuff
'           Except Then AI plays old trick of putting oppenent into Check for which human is not allowed to take AI KING because in check!!! phooey!
' Moved ShowButton Bar and showMoveList into ShowBd and gave showMoveList a diet
' No more File recorder, eliminate Datetime, fix UNDO for trying to go less than 1
' Remove InitBoard sub, all done in getInput button section

' Fix a ton of bugs to get all this integrated and working together but more keep popping up, blah!
' Make Result global to fix Y/N question to start over when Y
' Modify restart to clean slate the entire program, then use buttons to set particular settings needed
' Bug Loading a game right after Loading a game may cause an error
' Bug sometimes QB64 _FILEEXISTS says a real file that you typed correctly can't be found, just type it in again.
' I put a double _FILEEXISTS call in with delay between, helps but still the bug remains!!!
' Bug missing line when human loads a file playing black and starts game from there.
' Ugly fix that I doubt will work in other cases

'2017-11-16 A Tried to fix bug when AI is checkmated by modifying INCHECK function
'             Minor changes - removed 1 set of board labels. display pieces centrally on squares

'2017-11-16 B Adrian some nice changes to board but please no PlayAgain: label, no GOTO unless best way
' I do not like ending the whole application in the InCheck sub,  nor do I think one time result <-2500 is cause to quit??
' Remember AI checks many moves with InCheck, I am afraid it might quit first time it gets result < -2500 with better result left.
' Also I fix my omission with loadFlag code in IO, and it turns out with UNDO we have to clear the capture amounts, fixed.

'2017-11-18 A1: Changed White King Value to 4500, Black King value to -9000. Added test for AI's resignation in IO SUB. This should fix the AI check bug, and probably means that the
'           IN CHECK function need not check for checks to AI's king.

'2017-11-19 T: fixing the covering of the left border of buttonbar (Movelist too large passed from 700 to 680)
' 2017-11-22 T: fixing dummy _fileexists, it seems we don't need it yet
' 2017 11 22 T : coded manageDummySystem on save file, 1. now you can save only a game in progress  (no game = nothing to save)
'                                                      2. now you can overwrite an existing file only if you confirm to overwrite it
'                coded  feedback LoadFile in areaoutput


_Define A-Z As INTEGER
Randomize Timer
Const XMAX = 900
Const YMAX = 600
Const WHITE& = &HFFDDDDDD
Const BLACK& = &HFF000000
Const LITE& = &HFFFFFF00
Const LITE2& = _RGB32(78, 161, 72)
Const WHITES& = _RGB32(140, 160, 190)
Const BLACKS& = _RGB32(0, 130, 70)
Const SQ = 42
'T here (B out side of restart sub) we need a Maxlevel
Const MAXLEVEL = 5
'B might as well make constant!

'B For fonts
Common Shared FW, FH, normal&, maxCol, bArial&, bFW, bFH

'B For human playing Black
Common Shared playBlack, bmoves$, bFirst

'B from original QB64 samples: chess.bas
Dim Shared BOARD(0 To 7, 0 To 7)
Dim Shared BESTA(0 To 7), BESTB(0 To 7), BESTX(0 To 7), BESTY(0 To 7)
Dim Shared LEVEL, SCORE, result
Dim Shared wcKsflag, wcQsflag, INTFLAG
Dim Shared wcKsold, wcQsold

'B For saving moves to file
Dim Shared whiteMove$, blackMove$, pWflag$, pBflag$, GameFile$, Turn

'B For displaying T's on screen list of moves, last 8 shown from Moves$() array
Dim Shared InGame, countMove, loadFlag
ReDim Shared Move$(1 To 300)

'B for Undo
ReDim Shared Boards$(1 To 300)

'B Using updated Graphics Screen instead of Screen 0 text program
Screen _NewImage(XMAX, YMAX, 32)
_ScreenMove 360, 60

'B Checking fonts normal, big, and chess
'B load and check our normal font
normal& = _LoadFont("C:\windows\fonts\arial.ttf", 20)
If normal& <= 0 Then Print "Trouble with arial.ttf size 16 file, goodbye.": Sleep: End
_Font normal&
FW = 11: FH = _FontHeight(normal&)
maxCol = XMAX / FW

'B load and check SQ size font
bArial& = _LoadFont("C:\windows\fonts\arial.ttf", SQ, "MONOSPACE")
If bArial& <= 0 Then Print "Trouble with arial.ttf size "; SQ; " file, goodbye.": Sleep: End
bFW = _FontWidth(bArial&): bFH = _FontHeight(bArial&)

Intro
Wait_Click_Key
Cls
Do
    SCORE = 0
    Call IO(A, b, x, Y, result)
    'B   HERE IS WHERE CHECKMATE NEEDS TO BE DETERMINED!!!
    If result < -2500 Then
        'T & B Human has won
        AreaOutput "I RESIGN!! YOU WIN!!!", " Play Again? Y/N "
        Do
            Revenge$ = UCase$(InKey$)
        Loop Until Revenge$ = "Y" Or Revenge$ = "N"
        If Revenge$ = "N" Then
            AreaOutput "Thanks for playing,", "Good Bye!"
            Sleep 2
            System
        End If
        restart
        InGame = 0
    Else
        result = EVALUATE(-1, 10000)
        A = BESTA(1)
        b = BESTB(1)
        x = BESTX(1)
        Y = BESTY(1)
    End If
    Call SHOWBD
Loop
End

'==========================================================

'B sub for user communications area, T has made it for two strings
Sub AreaOutput (outText$, out2$)
    Line (480, 510)-(XMAX, YMAX), BLACK&, BF
    lp 26, 46, outText$
    lp 27, 46, out2$
End Sub

Sub BISHOP (A, B, XX(), YY(), NDX)
    ID = Sgn(BOARD(B, A))
    For DXY = 1 To 7
        X = A - DXY
        Y = B + DXY
        If X < 0 Or X > 7 Or Y < 0 Or Y > 7 Then Exit For
        GoSub 3
        If BOARD(Y, X) <> 0 Then Exit For
    Next
    For DXY = 1 To 7
        X = A + DXY
        Y = B + DXY
        If X < 0 Or X > 7 Or Y < 0 Or Y > 7 Then Exit For
        GoSub 3
        If BOARD(Y, X) <> 0 Then Exit For
    Next
    For DXY = 1 To 7
        X = A - DXY
        Y = B - DXY
        If X < 0 Or X > 7 Or Y < 0 Or Y > 7 Then Exit For
        GoSub 3
        If BOARD(Y, X) <> 0 Then Exit For
    Next
    For DXY = 1 To 7
        X = A + DXY
        Y = B - DXY
        If X < 0 Or X > 7 Or Y < 0 Or Y > 7 Then Exit For
        GoSub 3
        If BOARD(Y, X) <> 0 Then Exit For
    Next
    Exit Sub

    'sub gosub subroutine
    3 Rem
    If ID <> Sgn(BOARD(Y, X)) Then
        NDX = NDX + 1
        XX(NDX) = X
        YY(NDX) = Y
    End If
    Return
End Sub

Sub bSetup (bStr$)
    For X = 0 To 7
        For Y = 0 To 7
            p$ = Mid$(bStr$, 8 * X + Y + 1, 1)
            BOARD(X, Y) = cp2n(p$)
        Next
    Next
End Sub

Function bString$
    r$ = ""
    For X = 0 To 7
        For Y = 0 To 7
            num = BOARD(X, Y)
            r$ = r$ + cn2p$(num)
        Next
    Next
    bString$ = r$
End Function

Function Castleincheck (x)
    'T added to improve short castle control
    If playBlack Then
        BOARD(7, 1) = 4500
        'T first square acrossed
        null = INCHECK(x)
        BOARD(7, 1) = 0
        'T original square void
        If null = 0 Then
            'T if already in first test King is in check we skip second test
            BOARD(7, 2) = 4500
            'T second square acrossed
            null = INCHECK(x)
            BOARD(7, 2) = 0
        End If
        Castleincheck = null
    Else
        BOARD(7, 6) = 4500
        'T first square acrossed
        null = INCHECK(x)
        BOARD(7, 6) = 0
        'T original square void
        If null = 0 Then
            'T if already in first test King is in check we skip second test
            BOARD(7, 5) = 4500
            'T second square acrossed
            null = INCHECK(x)
            BOARD(7, 5) = 0
        End If
        Castleincheck = null
    End If
End Function

Function CastleincheckL (x)
    'T added to improve long castle control
    If playBlack Then
        BOARD(7, 4) = 4500
        null = INCHECK(x)
        BOARD(7, 4) = 0
        'T original square void
        If null = 0 Then
            'T if already in first test King is in check we skip second test
            BOARD(7, 5) = 4500
            'T C1 square
            null = INCHECK(x)
            BOARD(7, 5) = 0
        End If
        CastleincheckL = null
    Else
        BOARD(7, 3) = 4500
        'T or B or A D1 square
        null = INCHECK(x)
        BOARD(7, 3) = 0
        'T original square void
        If null = 0 Then
            'T if already in first test King is in check we skip second test
            BOARD(7, 2) = 4500
            'T C1 square
            null = INCHECK(x)
            BOARD(7, 2) = 0
        End If
        CastleincheckL = null
    End If
End Function

Function cn2p$ (n)
    Select Case n
        Case 0: r$ = "z"
        Case 100: r$ = "P"
        Case 270: r$ = "N"
        Case 300: r$ = "B"
        Case 500: r$ = "R"
        Case 900: r$ = "Q"
        Case 4500: r$ = "K"
        Case -100: r$ = "p"
        Case -270: r$ = "n"
        Case -300: r$ = "b"
        Case -500: r$ = "r"
        Case -900: r$ = "q"
        Case -9000: r$ = "k"
    End Select
    cn2p$ = r$
End Function

Function cp2n (piece$)
    Select Case piece$
        Case "z": r = 0
        Case "P": r = 100
        Case "N": r = 270
        Case "B": r = 300
        Case "R": r = 500
        Case "Q": r = 900
        Case "K": r = 4500
        Case "p": r = -100
        Case "n": r = -270
        Case "b": r = -300
        Case "r": r = -500
        Case "q": r = -900
        Case "k": r = -9000
    End Select
    cp2n = r
End Function

Sub cP (row, txt$)
    'B on row center Print txt$
    col = (maxCol - Len(txt$)) / 2
    _PrintString ((XMAX - _PrintWidth(txt$)) / 2, row * FH), txt$
End Sub

Function EVALUATE (ID, PRUNE)
    Dim XX(0 To 26), YY(0 To 26)
    LEVEL = LEVEL + 1
    BESTSCORE = 10000 * ID
    For b = 7 To 0 Step -1
        For A = 7 To 0 Step -1
            If Sgn(BOARD(b, A)) <> ID Then GoTo 1
            'Orig IF (LEVEL = 1) THEN CALL SHOWMAN(A, B)
            'B this might be human versus human level?

            Call MOVELIST(A, b, XX(), YY(), NDX)
            For I = 0 To NDX
                X = XX(I)
                Y = YY(I)
                If LEVEL = 1 Then
                    AreaOutput "TRYING: " + Chr$(65 + A) + Right$(Str$(8 - b), 1) + "-" + Chr$(65 + X) + Right$(Str$(8 - Y), 1), ""
                    'B Might as well make this look nice too, without the space
                End If
                OLDSCORE = SCORE
                MOVER = BOARD(b, A)
                TARGET = BOARD(Y, X)
                Call MAKEMOVE(A, b, X, Y)
                If (LEVEL < MAXLEVEL) Then SCORE = SCORE + EVALUATE(-ID, BESTSCORE - TARGET + ID * (8 - Abs(4 - X) - Abs(4 - Y)))
                SCORE = SCORE + TARGET - ID * (8 - Abs(4 - X) - Abs(4 - Y))
                If (ID < 0 And SCORE > BESTSCORE) Or (ID > 0 And SCORE < BESTSCORE) Then
                    BESTA(LEVEL) = A
                    BESTB(LEVEL) = b
                    BESTX(LEVEL) = X
                    BESTY(LEVEL) = Y
                    BESTSCORE = SCORE
                    If (ID < 0 And BESTSCORE >= PRUNE) Or (ID > 0 And BESTSCORE <= PRUNE) Then
                        BOARD(b, A) = MOVER
                        BOARD(Y, X) = TARGET
                        SCORE = OLDSCORE
                        LEVEL = LEVEL - 1
                        EVALUATE = BESTSCORE
                        Exit Function
                    End If
                End If
                BOARD(b, A) = MOVER
                BOARD(Y, X) = TARGET
                SCORE = OLDSCORE
            Next
        1 Next
    Next
    LEVEL = LEVEL - 1
    EVALUATE = BESTSCORE
End Function

Function getInput$
    Dim pieceChosen As _Byte
    Do
        'B Update board
        SHOWBD

        'B gather mouse input
        Do While _MouseInput
            mouseButton = _MouseButton(1)
            tx = _MouseX \ SQ - 1: ty = _MouseY \ SQ - 1
            ux = tx: uy = ty
        Loop

        'T area of managing Button Bar
        If _MouseButton(1) Then
            If _MouseX > 700 Then
                If _MouseY < 120 Then
                    'B PLAY WHITE
                    restart
                    InGame = -1
                    Turn = 1
                    playBlack = 0
                    bSetup "rnbqkbnrppppppppzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzPPPPPPPPRNBQKBNR"
                    AreaOutput "Your move.", ""
                    getInput$ = ""
                    Exit Function
                ElseIf _MouseY < 180 Then
                    'B PLAY BLACK there was a FEN around here also

                    'T this is the FEN of initial game setup
                    '  [rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1]
                    restart
                    InGame = -1
                    Turn = -1
                    playBlack = -1
                    bSetup "rnbkqbnrppppppppzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzPPPPPPPPRNBKQBNR"
                    getInput$ = ""
                    Exit Function
                ElseIf _MouseY < 240 Then
                    'B UNDO
                    If countMove - 1 > 1 Then
                        Move$(countMove) = ""
                        Boards$(countMove) = ""
                        countMove = countMove - 1
                        whiteMove$ = "": blackMove$ = "": bmoves$ = "": bFirst = -1
                        bSetup Boards$(countMove)
                        If playBlack = 0 Then AreaOutput "Your move.", ""
                        '_DISPLAY
                        Exit Function
                    End If
                ElseIf _MouseY < 300 Then
                    'B SAVE BOARD
                    'T if you are not in game what are you saving in a file?
                    If InGame = 0 Then Exit Function
                    ' T file exists, overwirte? Y/N
                    Do
                        in$ = ""
                        in$ = screenInput(50 * FW, 4 * FH, "Enter Save Filename > ")
                        If _FileExists(in$) = -1 And in$ <> "" Then
                            Color LITE&
                            AreaOutput "File exists...", " Overwrite Y/N?"
                            _Display
                            Color WHITE&
                            choice$ = UCase$(Input$(1))
                            If choice$ = "Y" Then Exit Do
                        Else
                            ' case _fileexists(in$) = 0
                            Exit Do
                        End If

                    Loop

                    Open in$ For Output As #1
                    If playBlack Then Print #1, "Black" Else Print #1, "White"
                    Print #1, blackMove$
                    'we need AI's move made if any since last Move$() entry
                    For i = 1 To countMove
                        Print #1, Move$(i)
                    Next
                    For i = 1 To countMove
                        Print #1, Boards$(i)
                    Next
                    Close #1
                    AreaOutput "File " + ins$, "loaded"
                    _Delay 1
                    If playBlack = 0 Then
                        AreaOutput "Your move.", ""
                    Else
                        bmove$ = blackMove$
                        bFirst = 0
                    End If
                    getInput$ = ""
                    Exit Function
                ElseIf _MouseY < 360 Then
                    'B  LOAD Board
                    in$ = screenInput(50 * FW, 4 * FH, "Enter Load Filename > ")
                    'B  for some damn reason the first time you try _FILEEXISTS with real file it says 0 nope!
                    'B                 but try again and is OK ?????????????????????????????????????????
                    'B                 So f... IT!
                    ' dummy = _FILEEXISTS(in$)
                    '_DELAY 1
                    'B  once is not enough, damn this sucks!!!!!!
                    'dummy = _FILEEXISTS(in$)
                    '_DELAY 1
                    'B  nope didn't help with 2nd call and delay, just try LOAD GAME again!
                    If _FileExists(in$) = -1 Then

                        count = 0
                        Open in$ For Input As #1
                        While EOF(1) = 0
                            Input #1, l$
                            count = count + 1
                        Wend
                        Close #1

                        ' T feedback to user
                        Color LITE&
                        AreaOutput "File loaded", in$
                        Color WHITE&
                        _Display
                        _Delay 1

                        restart
                        countMove = (count - 2) / 2
                        'B This gets needed data items before loading 2 arrays of size countMove
                        Open in$ For Input As #1
                        Input #1, BW$
                        If Left$(BW$, 1) = "B" Then playBlack = -1 Else playBlack = 0
                        Input #1, blackMove$
                        'B  this gets AI's last move (if any) not recorded in Move$()
                        '   OK maybe we have to pretend the blackMove$ is whiteMove$ so IO can reverse it when recording in Move$()
                        For i = 1 To countMove
                            Input #1, Move$(i)
                        Next
                        For i = 1 To countMove
                            Input #1, Boards$(i)
                        Next
                        Close #1
                        Cls
                        bSetup Boards$(countMove)
                        'B loadFlag is ugly way to fix a missing line in move list that occurs loading a game with human playing Black
                        If playBlack = 0 Then AreaOutput "Your move.", "" Else loadFlag = -1
                        InGame = -1
                    Else
                        AreaOutput in$, "File not found."
                    End If
                ElseIf _MouseY < 420 Then
                    'B MANUAL SET
                ElseIf _MouseY < 480 Then
                    'T quit
                    getInput$ = "QUIT"
                    Exit Function
                End If
            End If
        End If

        If InGame = -1 Then
            If pieceChosen = 0 Then
                If 1 <= ty And ty <= 8 Then
                    'Fellippe or B translate hovered coordinate to chess notation letter + digit
                    d$ = Right$(Str$(9 - ty), 1)
                    If 1 <= tx And tx <= 8 Then
                        l$ = Chr$(64 + tx)
                        ld$ = l$ + d$
                        'B letter + digit
                        ld2xy ld$, bx, by
                        'B translate notation to board$(x, y)
                        If BOARD(by, bx) > 0 Then
                            LegalShow bx, by
                            highLightSq bx, by, LITE2&
                            'Fellippe hover highlight
                            If mouseButton Then
                                Do While mouseButton
                                    'Fellippe wait for release
                                    i = _MouseInput
                                    mouseButton = _MouseButton(1)
                                    newtx = _MouseX \ SQ - 1: newty = _MouseY \ SQ - 1
                                Loop
                                If newtx = tx And newty = ty Then
                                    'Fellippe the mouse was released in the same square
                                    pieceChosen = -1: chosenBX = bx: chosenBY = by
                                End If
                            End If
                        End If
                    End If
                End If
            Else
                LegalShow chosenBX, chosenBY
                highLightSq chosenBX, chosenBY, LITE&
                If 1 <= uy And uy <= 8 Then
                    'B translate click to chess notation letter + digit
                    d2$ = Right$(Str$(9 - uy), 1)
                    If 1 <= ux And ux <= 8 Then
                        l2$ = Chr$(64 + ux)
                        ld2$ = l2$ + d2$
                        'B letter + digit
                        ld2xy ld2$, bx2, by2
                        highLightSq bx2, by2, LITE2&
                        'Fellippe hover highlight
                        If mouseButton Then
                            Do While mouseButton
                                'Fellippe wait for release
                                i = _MouseInput
                                mouseButton = _MouseButton(1)
                                newtx = _MouseX \ SQ - 1: newty = _MouseY \ SQ - 1
                            Loop
                            If newtx = tx And newty = ty Then
                                'Fellippe the mouse was released in the same square
                                If ld$ <> ld2$ Then
                                    getInput$ = ld$ + "-" + ld2$
                                    'T this let AI to castle for white
                                    If BOARD(by, bx) = 4500 Then
                                        If ld$ = "E1" And ld2$ = "G1" Then getInput$ = "O-O"
                                        If ld$ = "E1" And ld2$ = "C1" Then getInput$ = "O-O-O"
                                        If playBlack = -1 Then
                                            If ld$ = "D1" And ld2$ = "B1" Then getInput$ = "O-O"
                                            If ld$ = "D1" And ld2$ = "F1" Then getInput$ = "O-O-O"
                                        End If
                                    End If
                                    _AutoDisplay
                                    Exit Function
                                Else
                                    LegalHide bx, by
                                    SHOWMAN bx, by
                                    Exit Do
                                End If
                                'B ld compare
                            End If
                        End If
                    End If
                    'B ux compare
                End If
                'B uy compare
            End If
            'B piece chosen yet

            'B handle keyboard input
            k$ = InKey$
            If k$ <> "" Then
                If Len(k$) = 1 Then
                    If Asc(k$) = 11 Then
                        in$ = screenInput(50 * FW, 4 * FH, "(Esc to quit) Enter Move > ")
                        in$ = UCase$(in$)
                        spac = InStr(in$, " ")
                        If spac Then in$ = Mid$(in$, 1, spac - 1) + "-" + Mid$(in$, spac + 1)
                        If playBlack Then in$ = w2b$(in$)
                        getInput$ = in$
                        Exit Function
                    ElseIf Asc(k$) = 27 Then
                        End
                    End If
                End If
            End If
        End If
        'B if InGame
        _Display
    Loop
    lastLD$ = ""
    getInput$ = in$
End Function

Sub highLightSq (bx, by, c&)
    Line ((bx + 2) * SQ, (by + 2) * SQ)-((bx + 3) * SQ, (by + 3) * SQ), , B
    Line ((bx + 2) * SQ + 1, (by + 2) * SQ + 1)-((bx + 3) * SQ - 1, (by + 3) * SQ - 1), c&, B
    Line ((bx + 2) * SQ + 2, (by + 2) * SQ + 2)-((bx + 3) * SQ - 2, (by + 3) * SQ - 2), c&, B
End Sub

Function INCHECK (X)
    Dim XX(27), YY(27), NDX
    For b = 0 To 7
        For A = 0 To 7

            If BOARD(b, A) = 0 Then GoTo 6
            'T original code BOARD(b,A) >= 0  if white piece or void square skip test
            'A: omit square skip test
            'B Adrian next line is OK, it just skips empty spaces in board

            Call MOVELIST(A, b, XX(), YY(), NDX)
            For I = 0 To NDX Step 1
                X = XX(I)
                Y = YY(I)
                If BOARD(Y, X) = 4500 And Turn = 1 Then
                    'B ^^^ 2017-11-13 T has added and turn = 1 but turn = 1 is same as playBlack = 0
                    AreaOutput "YOU ARE IN CHECK!", ""
                    INCHECK = 1
                    Exit Function
                End If
                If BOARD(Y, X) = -9000 And Turn = -1 Then
                    'B ^^^ 2017-11-13 T has added and turn = -1 but turn = -1 is same as playBlack = -1
                    ' T in my last read of code posted playBack is used to note that Human plays as black
                    ' T Turn is used for knowing if the move  has been made by black Turn = -1 or by White Turn = 1
                    AreaOutput "I AM IN CHECK!", ""

                    'T this show Black status incheck
                    INCHECK = -1 'A: this is probably no longer needed
                    'T this should stop failed moves under check attack
                    'EXIT FUNCTION
                    'B exit now and get infinite loop?
                    ' T AI force must exit from loop
                End If
            Next
        6 Next
    Next
    INCHECK = 0
End Function

Sub initBoard
    If playBlack Then
        b$ = "rnbkqbnrppppppppzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzPPPPPPPPRNBKQBNR"
    Else
        b$ = "rnbqkbnrppppppppzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzPPPPPPPPRNBQKBNR"
    End If
    bSetup b$
End Sub

Sub Intro
    'T better have a subroutine that we can use as many as we want
    Color WHITE&, BLACK&
    cP 3, "QB64 CHESS"
    lp 5, 8, "CHESS is a game played between two players on a board of 64 squares."
    lp 6, 4, "Chess was first invented in its current form in Europe during the late"
    lp 7, 4, "fifteenth century. It evolved from much earlier forms invented in India"
    lp 8, 4, "and Persia."
    lp 9, 8, "The pieces are divided into Black and White.  Each player has 16 pieces:"
    lp 10, 4, "1 king, 1 queen, 2 rooks, 2 bishops, 2 knights, and 8 pawns.  White makes"
    lp 11, 4, "the first move.  The players alternate moving one piece at a time.  Pieces"
    lp 12, 4, "are moved to an unoccupied square, or moved onto a square occupied by an"
    lp 13, 4, "opponent's piece, capturing it.  When the king is under attack, he is in"
    lp 14, 4, "CHECK.  The player cannot put his king in check.  The object is to CHECKMATE"
    lp 15, 4, "the opponent.  This occurs when the king is in check and there is no way to"
    lp 16, 4, "remove the king from attack."
    lp 17, 8, "To move the pieces on the chessboard, click by mouse or type after Ctrl+K"
    lp 18, 4, "notation, e.g. E2-E4 (not in English notation like P-K4).  To castle, type O-O"
    lp 19, 4, "to castle kingside or O-O-O to castle queenside like in English notation."
    lp 20, 4, "To exit the game, type QUIT or press ESCAPE key."
    cP 25, "Click or press any key to continue."
End Sub

Sub IO (A, B, X, Y, RESULT)
    Dim XX(0 To 26), YY(0 To 26)
    If InGame Then
        'B ugly fix to get a missing line recorded in move list when load file and human playing black
        If loadFlag And blackMove$ <> "" Then
            loadFlag = 0
            countMove = countMove + 1
            Move$(countMove) = blackMove$ + pBflag$ + "  " + whiteMove$ + pWflag$
            'B this above is so ugly I even have to reverse black and white to get it right!
            Boards$(countMove) = bString$
            'B this above was omitted in versions before 11-16, still not right???
        End If

        If A >= 0 Then
            Turn = -1
            If RESULT < -2500 Then Exit Sub 'AI should resign
            PIECE = BOARD(Y, X)
            Call MAKEMOVE(A, B, X, Y)
            'T (chess2_17-11-13 T) this will fix illegal moves of AI under check
            NULL = INCHECK(0)
            'T (chess2_17-11-13 T) we must search for check after choosing a move

            'B Adrian, can't have game end here, many moves are checked can't quit if one is bad
            'IF NULL = -1 AND RESULT < -2500 THEN
            '    AreaOutput "AI resigns!", ""
            '    EXIT SUB
            'END IF

            If NULL Then
                'T (chess2_17-11-13 T) if there is a check for AI we must restore situation before move
                BOARD(B, A) = BOARD(Y, X)
                BOARD(Y, X) = PIECE
                Exit Sub
                'T (chess2_17-11-13 T) if it is check move is illegal
            End If
            'T this show Black status incheck

            mymove$ = Chr$(65 + A) + Right$(Str$(8 - B), 1) + "-" + Chr$(65 + X) + Right$(Str$(8 - Y), 1)

            'B ??? next line not used
            'AICHECK = 0 'reset AI check flag

            If playBlack Then mymove$ = w2b$(mymove$)
            AreaOutput "MY MOVE: " + mymove$, ""
            blackMove$ = mymove$
            If whiteMove$ <> "" Then
                If playBlack Then whiteMove$ = w2b$(whiteMove$)
            End If
            WriteEntry
        End If
        'B & T >>> it saves the last moves to file and to list I move this IF HERE TO GET THE COUPLE WHITE+BLACK

        If PIECE <> 0 Then
            s$ = "I TOOK YOUR "
            If PIECE = 100 Then s$ = s$ + "PAWN           "
            If PIECE = 270 Then s$ = s$ + "KNIGHT         "
            If PIECE = 300 Then s$ = s$ + "BISHOP         "
            If PIECE = 500 Then s$ = s$ + "ROOK           "
            If PIECE = 900 Then s$ = s$ + "QUEEN          "
            If PIECE = 4500 Then s$ = s$ + "KING          "
            AreaOutput "", s$
        End If

    End If

    Do
        'B I think this was help from Adrian, so we didn't have to fake a move
        If playBlack = -1 And countMove = 0 Then countMove = 1: Exit Sub

        'B Here we get Human's move but might be illegal so AI has to check before shown
        in$ = getInput$
        'T getinput$ takes user's input also for BUTTONBAR
        'B which is why we have to have to check InGame

        If UCase$(in$) = "QUIT" Then End

        If InGame = -1 Then
            whiteMove$ = in$
            'B ^^^ Human's move who now plays Black or White, don't be fooled by variable name>
            'B Originally human always played white>
            Turn = 1
            If UCase$(in$) = "O-O" Or in$ = "0-0" Then
                'T short castle rules... here we improve control of check and moves
                If wcKsflag <> 0 Then GoTo 16
                ' T it skips white castle king
                If playBlack Then
                    If BOARD(7, 0) <> 500 Then GoTo 16
                    If BOARD(7, 1) <> 0 Or BOARD(7, 2) <> 0 Then GoTo 16
                Else
                    If BOARD(7, 7) <> 500 Then GoTo 16
                    If BOARD(7, 6) <> 0 Or BOARD(7, 5) <> 0 Then GoTo 16
                End If
                'T now we test if there is a check along the path of king
                NULL = Castleincheck(0)
                If NULL = 0 Then
                    'B you can castle king side
                    If playBlack Then
                        BOARD(7, 1) = 4500
                        BOARD(7, 3) = 0
                        BOARD(7, 2) = 500
                        BOARD(7, 0) = 0
                        wcKsflag = -1
                        'T black castle king side
                        whiteMove$ = "O-O"
                        Exit Sub
                    Else
                        BOARD(7, 6) = 4500
                        BOARD(7, 4) = 0
                        BOARD(7, 5) = 500
                        BOARD(7, 7) = 0
                        wcKsflag = -1
                        'T white castle king side
                        whiteMove$ = "O-O"
                        Exit Sub
                    End If
                End If
            End If
            If UCase$(in$) = "O-O-O" Or in$ = "0-0-0" Then
                'T long castle rules... here we improve control of check and moves
                If wcQsflag <> 0 Then GoTo 16
                If playBlack Then
                    If BOARD(7, 7) <> 500 Then GoTo 16
                    If BOARD(7, 6) <> 0 Or BOARD(7, 5) <> 0 Or BOARD(7, 4) <> 0 Then GoTo 16
                Else
                    If BOARD(7, 0) <> 500 Then GoTo 16
                    If BOARD(7, 1) <> 0 Or BOARD(7, 2) <> 0 Or BOARD(7, 3) <> 0 Then GoTo 16
                End If
                'T now we test if there is a check along the path of king
                NULL = CastleincheckL(0)
                If NULL = 0 Then
                    'B you can castle queen side
                    If playBlack Then
                        BOARD(7, 5) = 4500
                        BOARD(7, 3) = 0
                        BOARD(7, 4) = 500
                        BOARD(7, 7) = 0
                        wcQsflag = -1
                        'T black castle queen side
                        whiteMove$ = "O-O-O"
                        Exit Sub
                    Else
                        'T you can castle if there are no check to the king to the start or during the movement of castle
                        BOARD(7, 2) = 4500
                        BOARD(7, 4) = 0
                        BOARD(7, 3) = 500
                        BOARD(7, 0) = 0
                        wcQsflag = -1
                        'T white castle queen side
                        whiteMove$ = "O-O-O"
                        Exit Sub
                    End If
                End If
            End If
            If Len(in$) < 5 Then GoTo 16
            B = 8 - (Asc(Mid$(in$, 2, 1)) - 48)
            A = Asc(UCase$(Mid$(in$, 1, 1))) - 65
            X = Asc(UCase$(Mid$(in$, 4, 1))) - 65
            Y = 8 - (Asc(Mid$(in$, 5, 1)) - 48)
            If B > 7 Or B < 0 Or A > 7 Or A < 0 Or X > 7 Or X < 0 Or Y > 7 Or Y < 0 Then GoTo 16
            If BOARD(B, A) <= 0 Then GoTo 16
            If Y = 2 And B = 3 And (X = A - 1 Or X = A + 1) Then
                If BOARD(B, A) = 100 And BOARD(Y, X) = 0 And BOARD(Y + 1, X) = -100 Then
                    If BESTB(1) = 1 And BESTA(1) = X Then
                        MOVER = BOARD(B, A)
                        TARGET = BOARD(Y, X)
                        Call MAKEMOVE(A, B, X, Y)
                        BOARD(Y + 1, X) = 0
                        ENPASSANT = -1
                        GoTo 15
                    End If
                End If
            End If
            Call MOVELIST(A, B, XX(), YY(), NDX)
            For K = 0 To NDX Step 1
                If X = XX(K) And Y = YY(K) Then
                    MOVER = BOARD(B, A)
                    TARGET = BOARD(Y, X)

                    INTFLAG = -1
                    'B so this is where INTFLAG is set

                    Call MAKEMOVE(A, B, X, Y)
                    If MOVER = 4500 Then
                        wcQsold = wcQsflag
                        wcKsold = wcKsflag
                        wcKsflag = -1
                        wcQsflag = -1
                    End If
                    If (A = 0) And (B = 7) And (MOVER = 500) Then
                        wcQsold = wcQsflag
                        wcQsflag = -1
                    End If
                    If (A = 7) And (B = 7) And (MOVER = 500) Then
                        wcKsold = wcKsflag
                        wcKsflag = -1
                    End If

                    INTFLAG = 0
                    'B and this is where INTFLAG is unset!

                    15 If INCHECK(0) = 0 Then Exit Sub

                    BOARD(B, A) = MOVER
                    BOARD(Y, X) = TARGET
                    If ENPASSANT Then BOARD(Y + 1, X) = -100: ENPASSANT = 0
                    If (A = 0) And (B = 7) And (MOVER = 500) Then wcQsflag = wcQsold
                    If (A = 7) And (B = 7) And (MOVER = 500) Then wcKsflag = wcKsold
                    If MOVER = 4500 Then wcQsflag = wcQsold
                    GoTo 16
                End If
            Next
        End If
        16

    Loop
    'B OK so this keeps looping until white makes legal move?

End Sub

Function isWhite (x, y)
    'B for squares and old for chess font
    yes = 0
    If y Mod 2 = 0 Then
        If x Mod 2 = 0 Then
            yes = -1
        End If
    Else
        If x Mod 2 = 1 Then
            yes = -1
        End If
    End If
    isWhite = yes
End Function

Sub KING (A, B, XX(), YY(), NDX)
    ID = Sgn(BOARD(B, A))
    For DY = -1 To 1
        If B + DY < 0 Or B + DY > 7 Then GoTo 12
        For DX = -1 To 1
            If A + DX < 0 Or A + DX > 7 Then GoTo 11
            If ID <> Sgn(BOARD(B + DY, A + DX)) Then
                NDX = NDX + 1
                XX(NDX) = A + DX
                YY(NDX) = B + DY
            End If
        11 Next
    12 Next
End Sub

Sub KNIGHT (A, B, XX(), YY(), NDX)
    ID = Sgn(BOARD(B, A))
    X = A - 1
    Y = B - 2
    GoSub 5
    X = A - 2
    Y = B - 1
    GoSub 5
    X = A + 1
    Y = B - 2
    GoSub 5
    X = A + 2
    Y = B - 1
    GoSub 5
    X = A - 1
    Y = B + 2
    GoSub 5
    X = A - 2
    Y = B + 1
    GoSub 5
    X = A + 1
    Y = B + 2
    GoSub 5
    X = A + 2
    Y = B + 1
    GoSub 5
    Exit Sub
    5 Rem
    If X < 0 Or X > 7 Or Y < 0 Or Y > 7 Then Return
    If ID <> Sgn(BOARD(Y, X)) Then NDX = NDX + 1: XX(NDX) = X: YY(NDX) = Y
    Return
End Sub

Sub ld2xy (ld$, dx, dy)
    'B dx and dy are going to be changed to find
    'B position (and thus type) of piece on the board from ld$
    letter$ = UCase$(Left$(ld$, 1))
    dx = Asc(letter$) - 65
    digit = Val(Right$(ld$, 1))
    dy = 8 - digit
End Sub

Sub LegalHide (x, y)
    Dim XX(0 To 26), YY(0 To 26)
    Call MOVELIST(x, y, XX(), YY(), NDX)
    For a = 0 To NDX Step 1
        If XX(a) >= 0 And YY(a) >= 0 Then SHOWMAN YY(a), XX(a)
    Next
End Sub

'T THIS SUB calculates legal position of piece in the board cell x,y
Sub LegalShow (x, y)
    Dim XX(0 To 26), YY(0 To 26)
    Call MOVELIST(x, y, XX(), YY(), NDX)
    For a = 0 To NDX Step 1
        If XX(a) >= 0 And YY(a) >= 0 Then highLightSq XX(a), YY(a), LITE2&
    Next
End Sub

'B graphics version of Locate col, row : Print txt$
Sub lp (row, col, txt$)
    _PrintString (col * FW, row * FH), txt$
End Sub

Sub MakeButton (x1, y1, x2, y2, txt$, Col&)
    Line (x1, y1)-(x2, y2), Col&, BF
    Line (x1, y1)-(x2, y2), WHITE&, B
    Line (x1 + 4, y2 - 4)-(x2 - 4, y2 - 4), _RGB32(222, 238, 227), B
    Line (x2 - 4, y2 - 4)-(x2 - 4, y1 + 4), _RGB32(222, 238, 227), B
    _PrintMode _KeepBackground
    'B VVV let's print button labels in middle of button
    _PrintString (x1 + 15, y2 - 1.35 * FH), txt$
    _PrintMode _FillBackground
End Sub

Sub MAKEMOVE (A, B, X, Y)
    'B makemove is called many times, the last decides whether pBflag$ gets set or NOT
    'B the pWflag$ should only be set by user, no automatic setting allowed by AI.
    pBflag$ = ""
    BOARD(Y, X) = BOARD(B, A)
    BOARD(B, A) = 0
    If Y = 0 And BOARD(Y, X) = 100 Then
        ' T it is the row 8
        If INTFLAG Then
            Do
                AreaOutput "Promote to:", ""
                I$ = Ppromote$
                Select Case UCase$(I$)
                    Case "KNIGHT", "N", "KT", "KT.", "N."
                        PROMOTE = 270: pWflag$ = "N"
                    Case "BISHOP", "B", "B."
                        PROMOTE = 300: pWflag$ = "B"
                    Case "ROOK", "R", "R."
                        PROMOTE = 500: pWflag$ = "R"
                    Case "QUEEN", "Q", "Q."
                        PROMOTE = 900: pWflag$ = "Q"
                End Select
            Loop Until PROMOTE <> 0
            If playBlack Then pWflag$ = LCase$(pWflag$)
            'B       only the human can set the pWflag$

            BOARD(Y, X) = PROMOTE
            Cls
            SHOWBD
            _Display
        Else
            BOARD(Y, X) = 900
            'B ^^^^ OK AI need the line for checking FUTURE!!! moves
        End If
    End If

    If Y = 7 And BOARD(Y, X) = -100 Then
        rap = -1
        BOARD(Y, X) = -900
        If playBlack Then pBflag$ = "Q" Else pBflag$ = "q"
    End If

End Sub

Sub MOVELIST (A, B, XX(), YY(), NDX)
    PIECE = Int(Abs(BOARD(B, A)))
    NDX = -1
    Select Case PIECE
        Case Is = 100
            Call PAWN(A, B, XX(), YY(), NDX)
        Case Is = 270
            Call KNIGHT(A, B, XX(), YY(), NDX)
        Case Is = 300
            Call BISHOP(A, B, XX(), YY(), NDX)
        Case Is = 500
            Call ROOK(A, B, XX(), YY(), NDX)
        Case Is = 900
            Call QUEEN(A, B, XX(), YY(), NDX)
        Case Is = 4500
            Call KING(A, B, XX(), YY(), NDX)
        Case Is = 9000
            Call KING(A, B, XX(), YY(), NDX)
    End Select
End Sub

Sub PAWN (A, B, XX(), YY(), NDX)
    ID = Sgn(BOARD(B, A))
    ' T ID 1 for white piece and -1 for black piece
    If (A - 1) >= 0 And (A - 1) <= 7 And (B - ID) >= 0 And (B - ID) <= 7 Then
        If Sgn(BOARD((B - ID), (A - 1))) = -ID Then
            NDX = NDX + 1
            XX(NDX) = A - 1
            YY(NDX) = B - ID
        End If
    End If
    If (A + 1) >= 0 And (A + 1) <= 7 And (B - ID) >= 0 And (B - ID) <= 7 Then
        If Sgn(BOARD((B - ID), (A + 1))) = -ID Then
            NDX = NDX + 1
            XX(NDX) = A + 1
            YY(NDX) = B - ID
        End If
    End If
    If A >= 0 And A <= 7 And (B - ID) >= 0 And (B - ID) <= 7 Then
        If BOARD((B - ID), A) = 0 Then
            NDX = NDX + 1
            XX(NDX) = A
            YY(NDX) = B - ID
            If (ID < 0 And B = 1) Or (ID > 0 And B = 6) Then
                If BOARD((B - ID - ID), A) = 0 Then
                    NDX = NDX + 1
                    XX(NDX) = A
                    YY(NDX) = B - ID - ID
                End If
            End If
        End If
    End If

End Sub

'B a pawn needs promotion to a piece, which? use mouse or keyboard
Function Ppromote$
    INP$ = "": ky = 0: oldtext$ = prompt$ + " {" + INP$ + "}"
    newText$ = oldtext$
    Do While ky <> 13
        i = _MouseInput
        ty = (_MouseY + 24) / SQ
        ' T we must control also X dimension not only Y dimension for mouse in Area Promotion
        If _MouseButton(1) = -1 And (_MouseX >= 500 And _MouseX <= 700) Then
            If ty > 1 Then
                If ty = 2 Then INP$ = "Q": Exit Do
                If ty = 3 Then INP$ = "R": Exit Do
                If ty = 4 Then INP$ = "B": Exit Do
                If ty = 5 Then INP$ = "N": Exit Do
            Else
                INP$ = ""
                'T no good click
            End If
        End If
        AreaOutput "Promote Enter Q R B N", newText$
        _Display
        oldtext$ = newText$
        k$ = InKey$
        If Len(k$) Then
            ky = Asc(Right$(k$, 1))
            If 31 < ky And ky < 127 Then
                INP$ = INP$ + Chr$(ky)
            ElseIf ky = 8 Then
                If Len(INP$) Then INP$ = Left$(INP$, Len(INP$) - 1)
            End If
            newText$ = prompt$ + " {" + INP$ + "}"
        End If
    Loop
    Ppromote$ = INP$
    'B don't worry about case, it gets checked later
End Function

Sub QUEEN (A, B, XX(), YY(), NDX)
    CALL BISHOP(A, B, XX(), YY(), NDX)
    CALL ROOK(A, B, XX(), YY(), NDX)
END SUB

SUB restart
    'B restart variables
    CLS
    ERASE BOARD
    REDIM Move$(1 TO 300)
    REDIM Boards$(1 TO 300)
    'B need to start array at 1 not 0
    result = -2500
    wcKsflag = 0: wcQsflag = 0: wcKsold = 0: wcQsold = 0
    LEVEL = 0: INTFLAG = 0: countMove = 0
    whiteMove$ = "": blackMove$ = "": bmoves$ = "": bFirst = -1
END SUB

SUB ROOK (A, B, XX(), YY(), NDX)
    ID = SGN(BOARD(B, A))
    FOR X = A - 1 TO 0 STEP -1
        IF ID <> SGN(BOARD(B, X)) THEN
            NDX = NDX + 1
            XX(NDX) = X
            YY(NDX) = B
        END IF
        IF (BOARD(B, X)) <> 0 THEN EXIT FOR
    NEXT
    FOR X = A + 1 TO 7 STEP 1
        IF ID <> SGN(BOARD(B, X)) THEN
            NDX = NDX + 1
            XX(NDX) = X
            YY(NDX) = B
        END IF
        IF (BOARD(B, X)) <> 0 THEN EXIT FOR
    NEXT
    FOR Y = B - 1 TO 0 STEP -1
        IF ID <> SGN(BOARD(Y, A)) THEN
            NDX = NDX + 1
            XX(NDX) = A
            YY(NDX) = Y
        END IF
        IF (BOARD(Y, A)) <> 0 THEN EXIT FOR
    NEXT
    FOR Y = B + 1 TO 7 STEP 1
        IF ID <> SGN(BOARD(Y, A)) THEN
            NDX = NDX + 1
            XX(NDX) = A
            YY(NDX) = Y
        END IF
        IF (BOARD(Y, A)) <> 0 THEN EXIT FOR
    NEXT
END SUB

'B This is INPUT for graphic screens
FUNCTION screenInput$ (pixelX, pixelY, prompt$)
    INP$ = ""
    ky = 0: oldtext$ = prompt$ + " {" + INP$ + "}"
    newText$ = oldtext$
    COLOR LITE&
    WHILE ky <> 13
        AreaOutput newText$, ""
        _DISPLAY
        oldtext$ = newText$
        k$ = INKEY$
        IF LEN(k$) THEN
            ky = ASC(RIGHT$(k$, 1))
            IF 31 < ky AND ky < 127 THEN
                INP$ = INP$ + CHR$(ky)
            ELSEIF ky = 8 THEN
                IF LEN(INP$) THEN INP$ = LEFT$(INP$, LEN(INP$) - 1)
            END IF
            newText$ = prompt$ + " {" + INP$ + "}    "
        END IF
    WEND
    COLOR WHITE&
    screenInput$ = INP$
END FUNCTION

'B show entire board captured pieces also used for pawn promotion, Move List, Buttons, Debug Info
SUB SHOWBD
    COLOR WHITE&, 0
    _FONT bArial&
    'B print board labels for files
    LOCATE 2, 3:
    IF playBlack = -1 THEN PRINT "HGFEDCBA" ELSE PRINT "ABCDEFGH";
    'LOCATE 11, 3:                                                          ' A: display 1 set of labels only
    'IF playBlack = -1 THEN PRINT "HGFEDCBA" ELSE PRINT "ABCDEFGH";
    'B print board labels for ranks
    FOR i = 8 TO 1 STEP -1
        BLR$ = RIGHT$(STR$(i), 1)
        IF playBlack THEN BLR$ = w2b$(BLR$)
        LOCATE 8 - i + 3, 2: PRINT BLR$;
        '    LOCATE 8 - i + 3, 11: PRINT BLR$;
    NEXT
    'B Count captures by start of standard set on board and deduct each piece on board
    DIM c(-6 TO 6)
    c(-6) = 1: c(-5) = 2: c(-4) = 2: c(-3) = 2: c(-2) = 8: c(-1) = 1
    c(6) = 1: c(5) = 2: c(4) = 2: c(3) = 2: c(2) = 8: c(1) = 1
    FOR x = 0 TO 7
        FOR y = 0 TO 7
            SHOWMAN x, y
            _FONT bArial&
            SELECT CASE BOARD(x, y)
                CASE -900: IF c(-6) THEN c(-6) = c(-6) - 1
                CASE -500: IF c(-5) THEN c(-5) = c(-5) - 1
                CASE -300: IF c(-4) THEN c(-4) = c(-4) - 1
                CASE -270: IF c(-3) THEN c(-3) = c(-3) - 1
                CASE -100: IF c(-2) THEN c(-2) = c(-2) - 1
                CASE -9000: IF c(-1) THEN c(-1) = c(-1) - 1
                CASE 4500: IF c(1) THEN c(1) = c(1) - 1
                CASE 100: IF c(2) THEN c(2) = c(2) - 1
                CASE 270: IF c(3) THEN c(3) = c(3) - 1
                CASE 300: IF c(4) THEN c(4) = c(4) - 1
                CASE 500: IF c(5) THEN c(5) = c(5) - 1
                CASE 900: IF c(6) THEN c(6) = c(6) - 1
            END SELECT
        NEXT
    NEXT
    'B below need to blackout captures in case UNDO undoes one
    LINE (12 * SQ, 0)-(700, 9 * SQ), BLACK&, BF
    'Draw Capture pieces section
    FOR b = 0 TO 4
        FOR a = 1 TO 2
            IF isWhite(a, b) THEN COLOR WHITES& ELSE COLOR BLACKS&
            LINE (((a * 2) + 10) * SQ, (b + 1) * SQ)-STEP(SQ, SQ), , BF
            PRESET (((a * 2) + 10) * SQ + 8, (b + 1) * SQ + 36) 'A: centralise pieces
            IF a = 2 THEN DRAW "C" + STR$(BLACK&) ELSE DRAW "C" + STR$(WHITE&)
            SELECT CASE b
                'A  draw outlines for captured area
                CASE 0: DRAW "R26U5H2L6E9U11G4H6G4H4G6H4D11F9L6G2D5"
                CASE 1: DRAW "R26U5H2L5U7E3R4U10L6D3L4U3L6D3L4U3L6D10R4F3D7L5G2D5"
                CASE 2: DRAW "R26U5H2L8E6U9H2G8H2E8H2L6G6D9F6L8G2D5"
                CASE 3: DRAW "R26U5H2U4E2U9H6L9G10D4F2R4E3R4G8L4G2D5"
                CASE 4: DRAW "R26U5H2L6U7E3U6H3L10G3D6F3D7L6G2D5"
                CASE 5: DRAW "R26U5H2L5E7U6H4L5G3U5R2U2L2U2L2D2L2D2R2D5H3L5G4D6F7L5G2D5"
            END SELECT
            DRAW "BE2"
            'A  MOVE PEN INSIDE
            IF a = 2 THEN DRAW "P" + STR$(BLACK&) + "," + STR$(BLACK&)
            IF a = 1 THEN DRAW "P" + STR$(WHITE&) + "," + STR$(WHITE&)
            COLOR WHITE&, BLACK&
            IF a = 1 THEN cindex = 6 - b ELSE cindex = -1 * (6 - b)
            IF playBlack THEN cindex = cindex * -1
            digit$ = RIGHT$(STR$(c(cindex)), 1)
            IF digit$ <> "0" THEN LOCATE b + 2, (a * 2) + 12: PRINT digit$;
        NEXT
    NEXT
    COLOR WHITE&, BLACK&
    _FONT normal&
    showButtonBar
    showMoveList
    'B Some debug stuff also needed for UNDO  Save file
    LINE (0, 25 * FH)-(46 * FW, YMAX), BLACK&, BF
    lp 25, 2, "Last move by AI: " + blackMove$
    lp 26, 2, "Move Count:" + STR$(countMove) + "   Turn:" + STR$(Turn) + "   Result:" + STR$(result)
    lp 27, 2, "Castle: K flag:" + STR$(wcKsflag) + "   Q flag:" + STR$(wcQsflag) + "   K old:" + STR$(wcKsold) + "   Q old:" + STR$(wcQsold)
    lp 28, 2, "Last move by Human: " + whiteMove$
END SUB

SUB showButtonBar
    MakeButton 700, 60, 880, 100, "PLAY WHITE", LITE2&
    MakeButton 700, 120, 880, 160, "PLAY BLACK", LITE2&
    MakeButton 700, 180, 880, 220, "UNDO", LITE2&
    MakeButton 700, 240, 880, 280, "SAVE GAME", LITE2&
    MakeButton 700, 300, 880, 340, "LOAD GAME", LITE2&
    MakeButton 700, 360, 880, 400, "MANUAL SETUP", LITE2&
    MakeButton 700, 420, 880, 460, "QUIT", LITE2&
END SUB

'B set this up with Adrian's Draw Strings
SUB SHOWMAN (A, B)
    IF isWhite(B, A) THEN COLOR WHITES& ELSE COLOR BLACKS&
    LINE ((A + 2) * SQ, (B + 2) * SQ)-STEP(SQ, SQ), , BF
    ZZ = ABS(BOARD(B, A))
    IF ZZ THEN
        PRESET ((A + 2) * SQ + 8, (B + 2) * SQ + 36) 'A: centralise pieces
        IF BOARD(B, A) < 0 THEN
            IF playBlack THEN DRAW "C" + STR$(WHITE&) ELSE DRAW "C" + STR$(BLACK&)
        ELSE
            IF playBlack THEN DRAW "C" + STR$(BLACK&) ELSE DRAW "C" + STR$(WHITE&)
        END IF
        SELECT CASE ZZ
            'A  draw outlines for pieces on board
            CASE 100: DRAW "R26U5H2L6U7E3U6H3L10G3D6F3D7L6G2D5"
            CASE 500: DRAW "R26U5H2L5U7E3R4U10L6D3L4U3L6D3L4U3L6D10R4F3D7L5G2D5"
            CASE 270: DRAW "R26U5H2U4E2U9H6L9G10D4F2R4E3R4G8L4G2D5"
            CASE 300: DRAW "R26U5H2L8E6U9H2G8H2E8H2L6G6D9F6L8G2D5"
            CASE 900: DRAW "R26U5H2L6E9U11G4H6G4H4G6H4D11F9L6G2D5"
            CASE 4500: DRAW "R26U5H2L5E7U6H4L5G3U5R2U2L2U2L2D2L2D2R2D5H3L5G4D6F7L5G2D5"
            CASE 9000: DRAW "R26U5H2L5E7U6H4L5G3U5R2U2L2U2L2D2L2D2R2D5H3L5G4D6F7L5G2D5"
        END SELECT
        DRAW "BE2"
        'A  MOVE PEN INSIDE and color fill
        IF BOARD(B, A) < 0 THEN
            IF playBlack THEN DRAW "P" + STR$(WHITE&) + "," + STR$(WHITE&) ELSE DRAW "P" + STR$(BLACK&) + "," + STR$(BLACK&)
        END IF
        IF BOARD(B, A) > 0 THEN
            IF playBlack THEN DRAW "P" + STR$(BLACK&) + "," + STR$(BLACK&) ELSE DRAW "P" + STR$(WHITE&) + "," + STR$(WHITE&)
        END IF
    END IF
    COLOR WHITE&, BLACK&
    _FONT normal&
END SUB

'B  T set this up to show last 8 moves of White and Black
SUB showMoveList
    IF countMove < 9 THEN z = 8 ELSE z = countMove
    LINE (500, 300)-(680, 500), BLACK&, BF ' T if we use 700 it covers left border of buttonbar
    COLOR _RGB(0, 180, 220)
    FOR a = 0 TO 7
        lp 22 - a, 46, Move$(z - a)
    NEXT
    COLOR WHITE&
END SUB

'B convert BINGO for human playing Black
FUNCTION w2b$ (s$)
    b$ = ""
    FOR i = 1 TO LEN(s$)
        here = INSTR("ABCDEFGH12345678", MID$(s$, i, 1))
        IF here THEN b$ = b$ + MID$("HGFEDCBA87654321", here, 1) ELSE b$ = b$ + MID$(s$, i, 1)
    NEXT
    w2b$ = b$
END FUNCTION

SUB Wait_Click_Key
    'B handy sub to reuse in other programs
    DO
        k = _KEYHIT
        WHILE _MOUSEINPUT: WEND
        _LIMIT 30
    LOOP UNTIL k <> 0 OR _MOUSEBUTTON(1)
END SUB

SUB WriteEntry
    'B  Record game in both Move$() and Boards$() at countMove
    IF playBlack THEN
        IF bFirst THEN
            bFirst = 0
            bmoves$ = blackMove$ + pBflag$
        ELSE
            r$ = bmoves$ + "   " + whiteMove$ + pWflag$
            countMove = countMove + 1
            Move$(countMove) = r$
            bmoves$ = blackMove$ + pBflag$
        END IF
    ELSE
        countMove = countMove + 1
        Move$(countMove) = whiteMove$ + pWflag$ + " " + blackMove$ + pBflag$
    END IF
    Boards$(countMove) = bString$
    'B clear flags for promoted pawns
    pWflag$ = "": pBflag$ = ""
END SUB

