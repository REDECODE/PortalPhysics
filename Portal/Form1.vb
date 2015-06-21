Public Class frmPortal
    Dim obj() As block
    Dim Schermo As Graphics
    Dim ausObj As New block
    Dim KUp, KDw, KSx, KDx, dis, trascina, copy As Boolean
    Dim maxRett, controllo, selez As Integer
    Dim attAria, Gravita As Single
    Dim ado As adoNet
    Dim bds As New BindingSource
    Private Sub frmPortal_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        adoNet.CreaConnessione("Mappa.mdb")
        maxRett = -1
        selez = -1
        Info()
        CaricaOggetti()
    End Sub
    Private Sub Info()

        ado = New adoNet("Info")
        ado.LeggiTabella()
        bds.DataSource = ado.daTable
        Gravita = bds.Current("Gravita")
        attAria = bds.Current("attrito")
        txtGravita.Text = Gravita
        txtAttAria.Text = attAria
    End Sub
    Private Sub CaricaOggetti()
        Dim i As Integer
        Dim colore As String

        ado = New adoNet("Oggetti")
        ado.LeggiTabella()
        bds.DataSource = ado.daTable
        'dgv1.DataSource = bds
        maxRett = bds.Count - 1
        ReDim obj(100)

        For i = 0 To bds.Count - 1
            Dim objAus As New block
            obj(i) = objAus
            obj(i).dimX = 1
            obj(i).dimY = 1
            obj(i).X = bds.Current("X")
            obj(i).Y = bds.Current("Y")
            obj(i).W = bds.Current("Width")
            obj(i).H = bds.Current("Height")
            obj(i).Elastico = bds.Current("Elast")
            obj(i).Attrito = bds.Current("Attr")
            obj(i).Tipo = bds.Current("Tipo")
            colore = bds.Current("Colore")
            obj(i).port.collegato = bds.Current("Collegato")
            obj(i).Bloccato = bds.Current("Bloccato")
            bds.MoveNext()

            obj(i).setVett(maxRett)
            If obj(i).port.collegato <> -1 Then
                obj(i).port.setVett(maxRett)
            End If

            Select Case colore
                Case "blu" : obj(i).Colore = Brushes.Blue
                Case "rosso" : obj(i).Colore = Brushes.Red
                Case "verde" : obj(i).Colore = Brushes.Green
                Case "giallo" : obj(i).Colore = Brushes.Yellow
                Case "arancio" : obj(i).Colore = Brushes.Orange
                Case Else : obj(i).Colore = Brushes.Black

            End Select
        Next
    End Sub
    Private Sub MostraDatiObj(ByVal i As Integer)
        If i <> -1 Then
            lblN.Text = i
            txtLeft.Text = obj(i).X
            txtTop.Text = obj(i).Y
            txtWidth.Text = obj(i).W
            txtHeight.Text = obj(i).H
            txtElast.Text = obj(i).Elastico
            txtAttr.Text = obj(i).Attrito
            txtTipo.Text = obj(i).Tipo
            txtCollegato.Text = obj(i).port.collegato
            chbBloccato.Checked = obj(i).Bloccato
            txtVelX.Text = obj(i).Vel.X
            txtVelY.Text = obj(i).Vel.Y
        End If
    End Sub

    Private Sub Disegna()
        Dim i As Integer
        For i = 0 To maxRett
            If Not obj(i) Is Nothing Then
                If Not obj(i).Forma.Equals(obj(i).FormaPrec) Then
                    Schermo.FillRectangle(Brushes.White, obj(i).FormaPrec)
                End If
            End If
        Next
        For i = 0 To maxRett
            If Not obj(i) Is Nothing Then
                'If Not obj(i).Forma.Equals(obj(i).FormaPrec) Then
                Schermo.FillRectangle(obj(i).Colore, obj(i).Forma)
                'End If
            End If
        Next
        If timTempo.Enabled = False And selez <> -1 Then
            Schermo.FillRectangle(Brushes.DarkBlue, obj(selez).Forma)
        End If
    End Sub
    Private Sub frmPortal_Paint(ByVal sender As System.Object, ByVal e As System.Windows.Forms.PaintEventArgs) Handles MyBase.Paint
        Dim i As Integer
        If dis = False Then
            Schermo = Me.CreateGraphics
            Schermo.Clear(Color.White)
            For i = 0 To maxRett
                Schermo.FillRectangle(obj(i).Colore, obj(i).Forma)
            Next
            dis = True
        End If
    End Sub
    Private Sub timTempo_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles timTempo.Tick
        Dim i As Integer
        'timTempo.Enabled = False
        For i = 0 To maxRett
            If Not obj(i) Is Nothing Then
                VerificaScontro(i)
            End If
        Next
        For i = 0 To maxRett
            If Not obj(i) Is Nothing Then
                If KUp And obj(i).Tipo = 1 Then
                    obj(i).Vel.Y -= 1
                End If
                If KDw And obj(i).Tipo = 1 Then
                    obj(i).Vel.Y += 1
                End If
                If KSx And obj(i).Tipo = 1 Then
                    obj(i).Vel.X -= 1
                End If
                If KDx And obj(i).Tipo = 1 Then
                    obj(i).Vel.X += 1
                End If
                obj(i).FormaPrec = obj(i).Forma
                obj(i).ausVel.clear()
                If obj(i).Bloccato = False Then
                    obj(i).addGravita(Gravita)
                    obj(i).addAttrito(attAria)
                End If
            End If
        Next

        Scontri()

        For i = 0 To maxRett
            If Not obj(i) Is Nothing Then
                obj(i).Vel.X = obj(i).Vel.X
                obj(i).Vel.Y = obj(i).Vel.Y

                obj(i).PX = Math.Truncate(obj(i).Vel.X * obj(i).dimX)
                obj(i).PY = Math.Truncate(obj(i).Vel.Y * obj(i).dimY)
                obj(i).dimX = 1
                obj(i).dimY = 1
            End If
        Next

        Sposta()

        Disegna()
    End Sub
    Private Sub Sposta()
        Dim num As Integer

        Do
            num = 0

            VerificaToccato()
            num += spostaX(1)
            num += spostaY(1)

            VerificaToccato()
            num += spostaX(-1)
            num += spostaY(-1)
            PortaleToccato()
        Loop While num > 0

    End Sub
    Private Sub PortaleToccato()
        Dim i, j, k As Integer
        Dim r As Rectangle
        Dim rapp As Single
        For i = 0 To maxRett
            If Not obj(i) Is Nothing Then
                k = obj(i).port.collegato
                If k <> -1 Then
                    For j = 0 To maxRett
                        If Not obj(j) Is Nothing Then
                            r = obj(j).Forma
                            Select Case obj(i).port.toccato(j)
                                Case 1
                                    rapp = CalcolaRapporto(i, k, j, 1)
                                    If rapp <> -1 Then
                                        r.X = obj(k).Forma.Right
                                        r.Y = obj(k).Forma.Top + (obj(k).Forma.Height - obj(j).Forma.Height) * rapp
                                        If Not Interseca(r) Then
                                            obj(j).Forma = r
                                        End If
                                    End If
                                Case -1
                                    rapp = CalcolaRapporto(i, k, j, -1)
                                    If rapp <> -1 Then
                                        r.X = obj(k).Forma.Left - r.Width
                                        r.Y = obj(k).Forma.Top + (obj(k).Forma.Height - obj(j).Forma.Height) * rapp
                                        If Not Interseca(r) Then
                                            obj(j).Forma = r
                                        End If
                                    End If
                                Case 2
                                    rapp = CalcolaRapporto(i, k, j, 2)
                                    If rapp <> -1 Then
                                        r.Y = obj(k).Forma.Bottom
                                        r.X = obj(k).Forma.Left + (obj(k).Forma.Width - obj(j).Forma.Width) * rapp
                                        If Not Interseca(r) Then
                                            obj(j).Forma = r
                                        End If
                                    End If
                                Case -2
                                    rapp = CalcolaRapporto(i, k, j, -2)
                                    If rapp <> -1 Then
                                        r.Y = obj(k).Forma.Top - r.Height
                                        r.X = obj(k).Forma.Left + (obj(k).Forma.Width - obj(j).Forma.Width) * rapp
                                        If Not Interseca(r) Then
                                            obj(j).Forma = r
                                        End If
                                    End If
                            End Select
                            obj(i).port.toccato(j) = 0
                        End If
                    Next
                End If
            End If
        Next
    End Sub
    Private Function CalcolaRapporto(ByVal s As Integer, ByVal d As Integer, ByVal o As Integer, ByVal dir As Integer) As Single
        Dim ret As Single
        If Math.Abs(dir) = 1 Then
            If obj(o).Forma.Top >= obj(s).Forma.Top And obj(o).Forma.Bottom <= obj(s).Forma.Bottom Then
                If obj(o).Forma.Height <= obj(d).Forma.Height Then
                    ret = (obj(o).Forma.Top - obj(s).Forma.Top) / (obj(s).Forma.Height - obj(o).Forma.Height)
                Else
                    ret = -1
                End If
            Else
                ret = -1
            End If
        Else
            If obj(o).Forma.Left >= obj(s).Forma.Left And obj(o).Forma.Right <= obj(s).Forma.Right Then
                If obj(o).Forma.Width <= obj(d).Forma.Width Then
                    ret = (obj(o).Forma.Left - obj(s).Forma.Left) / (obj(s).Forma.Width - obj(o).Forma.Width)
                Else
                    ret = -1
                End If
            Else
                ret = -1
            End If
        End If
        Return ret
    End Function
    Private Function Interseca(ByVal rett As Rectangle)
        Dim trovato As Boolean
        Dim i As Integer
        For i = 0 To maxRett
            If Not obj(i) Is Nothing Then
                If rett.IntersectsWith(obj(i).Forma) Then
                    trovato = True
                End If
            End If
        Next
        Return trovato
    End Function
    Private Function Interseca(ByVal k As Integer) As Integer
        Dim ret As Integer
        Dim i As Integer

        ret = -1
        For i = 0 To maxRett
            If i <> k And Not obj(i) Is Nothing Then
                If obj(k).Forma.IntersectsWith(obj(i).Forma) Then
                    ret = i
                End If
            End If
        Next
        Return ret
    End Function
    Private Function BloccoLibero()
        Dim ret As Integer
        Do Until obj(ret) Is Nothing
            ret += 1
        Loop
        Return ret
    End Function
    Private Sub VerificaToccato()

        Dim trovato As Boolean
        Dim i, j As Integer
        For i = 0 To maxRett
            If Not obj(i) Is Nothing Then
                For j = 0 To maxRett
                    obj(i).Contatto(j) = 0
                Next
            End If
        Next
        Do
            trovato = False
            For i = 0 To maxRett
                If Not obj(i) Is Nothing Then
                    If VerificaScontro2(i) Then
                        trovato = True
                    End If
                End If
            Next
        Loop While trovato
    End Sub
    Private Function spostaX(ByVal dir As Integer)
        Dim i, num As Integer
        For i = 0 To maxRett
            If Not obj(i) Is Nothing Then
                If dir * obj(i).PX > 0 Then
                    If (obj(i).scontroR And dir = 1) Or (obj(i).scontroL And dir = -1) Then
                        obj(i).dimX = 1 - obj(i).PX / obj(i).Vel.X
                        obj(i).PX = 0
                    Else
                        obj(i).PX -= dir
                        obj(i).X += dir
                    End If
                    num += obj(i).PX * dir
                End If
            End If
        Next
        Return num
    End Function
    Private Function spostaY(ByVal dir As Integer)
        Dim i, num As Integer
        For i = 0 To maxRett
            If Not obj(i) Is Nothing Then
                If dir * obj(i).PY > 0 Then

                    If (obj(i).scontroD And dir = 1) Or (obj(i).scontroU And dir = -1) Then
                        obj(i).dimY = 1 - obj(i).PY / obj(i).Vel.Y
                        obj(i).PY = 0
                    Else
                        obj(i).PY -= dir
                        obj(i).Y += dir
                    End If
                    num += obj(i).PY * dir
                End If
            End If
        Next
        Return num
    End Function

    Private Sub Scontri()
        Dim i, j As Integer
        Dim ausV As Single
        Dim trovato As Boolean
        Do
            trovato = False
            For i = 0 To maxRett
                If Not obj(i) Is Nothing Then
                    If VerificaScontro(i) Then
                        If obj(i).scontroR Or obj(i).scontroL Or obj(i).scontroD Or obj(i).scontroU Then
                            trovato = True
                        End If
                    End If
                End If
            Next
            If trovato Then
                For i = 0 To maxRett
                    If Not obj(i) Is Nothing Then
                        If obj(i).scontroR Then
                            obj(i).ausVel.R = 0
                        Else
                            PassaVel(obj(i).Vel.R, obj(i).ausVel.R)
                        End If
                        If obj(i).scontroL Then
                            obj(i).ausVel.L = 0
                        Else
                            PassaVel(obj(i).Vel.L, obj(i).ausVel.L)
                        End If
                        If obj(i).scontroD Then
                            obj(i).ausVel.D = 0
                        Else
                            PassaVel(obj(i).Vel.D, obj(i).ausVel.D)
                        End If
                        If obj(i).scontroU Then
                            obj(i).ausVel.U = 0
                        Else
                            PassaVel(obj(i).Vel.U, obj(i).ausVel.U)
                        End If
                    End If
                Next
                For i = 0 To maxRett
                    If Not obj(i) Is Nothing Then
                        For j = 0 To maxRett
                            If Not obj(j) Is Nothing Then
                                Select Case obj(i).Contatto(j)
                                    Case 1
                                        If obj(j).Bloccato Then
                                            PassaVel(obj(i).Vel.R, obj(i).ausVel.L)
                                        Else
                                            PassaVel(obj(i).Vel.R, obj(j).ausVel.R)
                                        End If
                                    Case -1
                                        If obj(j).Bloccato Then
                                            PassaVel(obj(i).Vel.L, obj(i).ausVel.R)
                                        Else
                                            PassaVel(obj(i).Vel.L, obj(j).ausVel.L)
                                        End If
                                    Case 2
                                        If obj(j).Bloccato Then
                                            ausV = TogliGr(obj(i).Vel.D)
                                            PassaVel(ausV, obj(i).ausVel.U)
                                        Else
                                            PassaVel(obj(i).Vel.D, obj(j).ausVel.D)
                                        End If
                                    Case -2
                                        If obj(j).Bloccato Then
                                            PassaVel(obj(i).Vel.U, obj(i).ausVel.D)
                                        Else
                                            PassaVel(obj(i).Vel.U, obj(j).ausVel.U)
                                        End If

                                End Select
                            End If
                        Next
                    End If
                Next
                For i = 0 To maxRett
                    If Not obj(i) Is Nothing Then
                        obj(i).Vel.R = obj(i).ausVel.R
                        obj(i).Vel.L = obj(i).ausVel.L
                        obj(i).Vel.D = obj(i).ausVel.D
                        obj(i).Vel.U = obj(i).ausVel.U
                        'For j = 0 To maxRett
                        '        If obj(i).Contatto(j) <> 0 And obj(i).Bloccato = False Then
                        '            ElaboraScontro(i, j, obj(i).Contatto(j))
                        '        End If
                        'Next
                    End If
                Next
            End If
        Loop While trovato
    End Sub
    Private Function TogliGr(ByVal vel As Single) As Single
        If vel > (Gravita - attAria) Then
            Return vel - (Gravita - attAria)
        Else
            Return 0
        End If
    End Function
    Private Sub PassaVel(ByVal vel As Single, ByRef ausVel As Single)
        If vel > ausVel Then
            ausVel = vel
        End If
    End Sub
    Private Function VerificaScontro(ByVal i As Integer) As Boolean
        Dim j As Integer
        Dim trovato As Boolean

        For j = 0 To maxRett
            If Not obj(j) Is Nothing Then
                obj(i).Contatto(j) = 0
                If i <> j Then
                    If obj(i).R = obj(j).X Then
                        If obj(i).Y < obj(j).B And obj(i).B > obj(j).Y And obj(i).Vel.R > obj(j).Vel.R Then
                            obj(i).Contatto(j) = 1 'Destra
                            trovato = True
                        End If
                    ElseIf obj(i).X = obj(j).R Then
                        If obj(i).Y < obj(j).B And obj(i).B > obj(j).Y And obj(i).Vel.L > obj(j).Vel.L Then
                            obj(i).Contatto(j) = -1 'Sinistra
                            trovato = True
                        End If
                    ElseIf obj(i).B = obj(j).Y Then
                        If obj(i).X < obj(j).R And obj(i).R > obj(j).X And obj(i).Vel.D > obj(j).Vel.D Then
                            obj(i).Contatto(j) = 2 'Giu
                            trovato = True
                        End If
                    ElseIf obj(i).Y = obj(j).B Then
                        If obj(i).X < obj(j).R And obj(i).R > obj(j).X And obj(i).Vel.U > obj(j).Vel.U Then
                            obj(i).Contatto(j) = -2 'Su
                            trovato = True
                        End If
                    End If
                    End If
                End If
        Next
        Return trovato
    End Function
    Private Function VerificaScontro2(ByVal i As Integer) As Boolean
        Dim j As Integer
        Dim trovato As Boolean
        For j = 0 To maxRett
            If i <> j And Not obj(j) Is Nothing Then
                If obj(i).R = obj(j).X Then
                    If obj(i).Contatto(j) <> 1 And obj(i).Y < obj(j).B And obj(i).B > obj(j).Y And obj(i).PX > 0 And (obj(j).PX <= 0 Or obj(j).scontroR) Then
                        obj(i).Contatto(j) = 1
                        trovato = True
                        If obj(j).port.collegato <> -1 Then
                            obj(j).port.toccato(i) = 1
                        End If
                    End If
                ElseIf obj(i).X = obj(j).R Then
                    If obj(i).Contatto(j) <> -1 And obj(i).Y < obj(j).B And obj(i).B > obj(j).Y And obj(i).PX < 0 And (obj(j).PX >= 0 Or obj(j).scontroL) Then
                        obj(i).Contatto(j) = -1
                        trovato = True
                        If obj(j).port.collegato <> -1 Then
                            obj(j).port.toccato(i) = -1
                        End If
                    End If
                ElseIf obj(i).B = obj(j).Y Then
                    If obj(i).Contatto(j) <> 2 And obj(i).X < obj(j).R And obj(i).R > obj(j).X And obj(i).PY > 0 And (obj(j).PY <= 0 Or obj(j).scontroD) Then
                        obj(i).Contatto(j) = 2
                        trovato = True
                        If obj(j).port.collegato <> -1 Then
                            obj(j).port.toccato(i) = 2
                        End If
                    End If
                ElseIf obj(i).Y = obj(j).B Then
                    If obj(i).Contatto(j) <> -2 And obj(i).X < obj(j).R And obj(i).R > obj(j).X And obj(i).PY < 0 And (obj(j).PY >= 0 Or obj(j).scontroU) Then
                        obj(i).Contatto(j) = -2
                        trovato = True
                        If obj(j).port.collegato <> -1 Then
                            obj(j).port.toccato(i) = -2
                        End If
                    End If
                End If
            End If
        Next
        Return trovato
    End Function
    Private Sub frmPortal_KeyDown(ByVal sender As System.Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles MyBase.KeyDown

        If timTempo.Enabled Then
            If e.KeyValue = 37 Then
                KSx = True
            ElseIf e.KeyValue = 38 Then
                KUp = True
            ElseIf e.KeyValue = 39 Then
                KDx = True
            ElseIf e.KeyValue = 40 Then
                KDw = True
            ElseIf e.KeyCode = Keys.Escape Then
                selez = -1
                gbxStrumenti.Visible = True
                timTempo.Stop()
            End If
        Else


            If selez <> -1 Then
                obj(selez).FormaPrec = obj(selez).Forma
                If e.Control Then
                    If e.KeyCode = Keys.Left Then
                        spostaWH(1, -1)
                        Disegna()
                    ElseIf e.KeyCode = Keys.Right Then
                        spostaWH(1, 1)
                        Disegna()
                    ElseIf e.KeyCode = Keys.Up Then
                        spostaWH(2, -1)
                        Disegna()
                    ElseIf e.KeyCode = Keys.Down Then
                        spostaWH(2, 1)
                        Disegna()
                    ElseIf e.KeyCode = Keys.C Then ' Copia un oggetto
                        copy = True
                        trascina = True
                        ausObj.copy(obj(selez), maxRett)
                        AddObj()
                    End If
                Else
                    If e.KeyCode = Keys.Left Then
                        spostaXY(1, -1)
                        Disegna()
                    ElseIf e.KeyCode = Keys.Right Then
                        spostaXY(1, 1)
                        Disegna()
                    ElseIf e.KeyCode = Keys.Up Then
                        spostaXY(2, -1)
                        Disegna()
                    ElseIf e.KeyCode = Keys.Down Then
                        spostaXY(2, 1)
                        Disegna()
                    ElseIf e.KeyCode = Keys.Delete Then
                        Schermo.FillRectangle(Brushes.White, obj(selez).Forma)
                        obj(selez) = Nothing
                        selez = -1
                    End If
                End If
            End If
        End If
    End Sub
    Private Sub spostaXY(ByVal dir As Integer, ByVal n As Integer)
        ausObj.Forma = obj(selez).Forma
        If dir = 1 Then
            obj(selez).X += n
            If Interseca(selez) <> -1 Then
                obj(selez).X -= n
            End If
        Else
            obj(selez).Y += n
            If Interseca(selez) <> -1 Then
                obj(selez).Y -= n
            End If
        End If
    End Sub
    Private Sub spostaWH(ByVal dir As Integer, ByVal n As Integer)
        If dir = 1 Then
            obj(selez).W += n
            If Interseca(selez) <> -1 Then
                obj(selez).W -= n
            End If
        Else
            obj(selez).H += n
            If Interseca(selez) <> -1 Then
                obj(selez).H -= n
            End If
        End If
    End Sub
    Private Sub frmPortal_KeyUp(ByVal sender As System.Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles MyBase.KeyUp
        If e.KeyValue = 37 Then
            KSx = False
        ElseIf e.KeyValue = 38 Then
            KUp = False
        ElseIf e.KeyValue = 39 Then
            KDx = False
        ElseIf e.KeyValue = 40 Then
            KDw = False
        End If
    End Sub
    Private Sub frmPortal_MouseDown(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles Me.MouseDown
        Dim i As Integer
        gbxStrumenti.Focus()
        If Not timTempo.Enabled Then
            If Not trascina Then
                ausObj.X = e.X
                ausObj.Y = e.Y
                selez = -1
                trascina = False
                For i = 0 To maxRett
                    If Not obj(i) Is Nothing Then
                        If obj(i).Forma.Contains(e.X, e.Y) Then
                            selez = i
                            Schermo.FillRectangle(Brushes.DarkBlue, obj(i).Forma)
                        End If
                    End If
                Next
            End If
        End If
    End Sub
    Private Sub frmPortal_MouseDoubleClick(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles Me.MouseDoubleClick
        If Not timTempo.Enabled Then
            If trascina Then
                If Interseca(selez) = -1 Then
                    trascina = False
                End If
            Else
                If selez <> -1 Then
                    obj(selez).FormaPrec = obj(selez).Forma
                    trascina = True
                End If
            End If
        End If
    End Sub
    Private Sub frmPortal_MouseMove(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles Me.MouseMove
        If trascina = True And timTempo.Enabled = False Then
            obj(selez).FormaPrec = obj(selez).Forma
            obj(selez).X = e.X - obj(selez).W / 2
            obj(selez).Y = e.Y - obj(selez).H / 2
            Disegna()
        End If
    End Sub
    Private Sub frmPortal_MouseUp(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles Me.MouseUp
        Dim i As Integer
        Dim trovato As Boolean
        If Not timTempo.Enabled Then
            If trascina Then
                If copy Then
                    If Interseca(selez) = -1 Then
                        trascina = False
                        copy = False
                    End If
                End If
            Else
                If e.X <> ausObj.X And e.Y <> ausObj.Y Then
                    If e.X < ausObj.X Then
                        ausObj.W = ausObj.X - e.X
                        ausObj.X = e.X
                    Else
                        ausObj.W = e.X - ausObj.X
                    End If
                    If e.Y < ausObj.Y Then
                        ausObj.H = ausObj.Y - e.Y
                        ausObj.Y = e.Y
                    Else
                        ausObj.H = e.Y - ausObj.Y
                    End If
                    ausObj.Colore = Brushes.Black
                    For i = 0 To maxRett
                        If Not obj(i) Is Nothing Then
                            If ausObj.Forma.IntersectsWith(obj(i).Forma) Then
                                trovato = True
                            End If
                        End If
                    Next
                    If Not trovato Then
                        AddObj()
                        Disegna()
                    End If
                End If
            End If
            MostraDatiObj(selez)
            Disegna()
        End If
    End Sub
    Private Sub AddObj()
        Dim k, i As Integer
        Dim objAus As New block

        k = BloccoLibero()
        If k > maxRett Then
            For i = 0 To maxRett
                If Not obj(i) Is Nothing Then
                    obj(i).setVett(k)
                    If obj(i).port.collegato <> -1 Then
                        obj(i).port.setVett(k)
                    End If
                End If
            Next
            maxRett = k
        End If

        objAus.X = ausObj.X
        objAus.Y = ausObj.Y
        objAus.W = ausObj.W
        objAus.H = ausObj.H
        objAus.Colore = ausObj.Colore
        objAus.Bloccato = ausObj.Bloccato
        objAus.port.collegato = -1
        objAus.setVett(maxRett)
        ' obj(maxRett).port.setVett(maxRett)

        obj(k) = objAus
        selez = k
        MostraDatiObj(selez)
    End Sub

    Private Sub chbBloccato_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles chbBloccato.CheckedChanged
        If selez <> -1 Then
            obj(selez).Bloccato = chbBloccato.Checked
        End If
    End Sub

    Private Sub txtGravita_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles txtGravita.TextChanged
        Try
            Gravita = txtGravita.Text
        Catch ex As Exception
        End Try
    End Sub

    Private Sub txtAttAria_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles txtAttAria.TextChanged
        Try
            attAria = txtAttAria.Text
        Catch ex As Exception
        End Try
    End Sub

    Private Sub btnSalva_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnSalva.Click
        Dim i As Integer
        For i = 0 To bds.Count - 1
            bds.RemoveCurrent()
        Next

        bds.MoveFirst()
        For i = 0 To 100
            If Not obj(i) Is Nothing Then
                bds.AddNew()
                bds.Current("X") = obj(i).X
                bds.Current("Y") = obj(i).Y
                bds.Current("Width") = obj(i).W
                bds.Current("Height") = obj(i).H
                bds.Current("Elast") = obj(i).Elastico
                bds.Current("Attr") = obj(i).Attrito
                bds.Current("Tipo") = obj(i).Tipo
                bds.Current("Colore") = obj(i).Colore
                bds.Current("Collegato") = obj(i).port.collegato
                bds.Current("Bloccato") = obj(i).Bloccato
                bds.MoveNext()
            End If
        Next
        bds.EndEdit()
        ado.Salva()
    End Sub

    Private Sub txtCollegato_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles txtCollegato.TextChanged
        Try
            obj(selez).port.collegato = txtCollegato.Text
            If obj(selez).port.collegato <> -1 Then
                obj(selez).port.setVett(maxRett)
            End If

        Catch ex As Exception
        End Try
    End Sub

    Private Sub txtTipo_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles txtTipo.TextChanged
        Try
            obj(selez).Tipo = txtTipo.Text
        Catch ex As Exception
        End Try
    End Sub

    Private Sub btnStart_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnStart.Click
        If timTempo.Enabled = False Then
            gbxStrumenti.Visible = False
            timTempo.Start()
        Else
            gbxStrumenti.Visible = True
            timTempo.Stop()
        End If
    End Sub

    Private Sub txtVelX_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles txtVelX.TextChanged
        Try
            obj(selez).Vel.X = txtVelX.Text
        Catch ex As Exception
        End Try
    End Sub

    Private Sub txtVelY_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles txtVelY.TextChanged
        Try
            obj(selez).Vel.Y = txtVelY.Text
        Catch ex As Exception
        End Try
    End Sub
End Class
