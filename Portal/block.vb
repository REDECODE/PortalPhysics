Public Class block
    Private type, mass, piX, piY As Integer
    Private dX, dY, forzaX, forzaY As Single
    Private att, elast As Single
    Private vett() As Integer
    Private lock, sopra As Boolean
    Private col As Brush
    Private rett, rettPrec As Drawing.Rectangle

    Public port As New portale
    Public Vel, ausVel As New velocita
    Public Sub copy(ByRef ausobj As block, ByVal n As Integer)
        Me.X = ausobj.X
        Me.Y = ausobj.Y
        Me.port.collegato = ausobj.port.collegato
        Me.Forma = ausobj.Forma
        Me.FormaPrec = ausobj.FormaPrec
        Me.Attrito = ausobj.Attrito
        Me.Bloccato = ausobj.Bloccato
        Me.Colore = ausobj.Colore
        Me.dimX = ausobj.dimX
        Me.dimY = ausobj.dimY
        Me.setVett(n)
        Me.port.direz = ausobj.port.direz
        Me.port.setVett(n)

    End Sub

    Public Sub setVett(ByVal n As Integer)
        ReDim vett(n)
    End Sub
    Public Property Over() As Boolean
        Get
            Return sopra
        End Get
        Set(ByVal value As Boolean)
            sopra = value
        End Set
    End Property
    Public Property PX() As Integer
        Get
            Return piX
        End Get
        Set(ByVal value As Integer)
            piX = value
        End Set
    End Property
    Public Property PY() As Integer
        Get
            Return piY
        End Get
        Set(ByVal value As Integer)
            piY = value
        End Set
    End Property
    Public Property dimX() As Single
        Get
            Return dX
        End Get
        Set(ByVal value As Single)
            dX = value
        End Set
    End Property
    Public Property dimY() As Single
        Get
            Return dY
        End Get
        Set(ByVal value As Single)
            dY = value
        End Set
    End Property
    Public Property X() As Integer
        Get
            Return rett.X
        End Get
        Set(ByVal value As Integer)
            rett.X = value
        End Set
    End Property
    Public ReadOnly Property R() As Integer
        Get
            Return rett.Right
        End Get
    End Property
    Public ReadOnly Property B() As Integer
        Get
            Return rett.Bottom
        End Get
    End Property
    Public Property Y() As Integer
        Get
            Return rett.Y
        End Get
        Set(ByVal value As Integer)
            rett.Y = value
        End Set
    End Property
    Public Property W() As Integer
        Get
            Return rett.Width
        End Get
        Set(ByVal value As Integer)
            rett.Width = value
        End Set
    End Property
    Public Property H() As Integer
        Get
            Return rett.Height
        End Get
        Set(ByVal value As Integer)
            rett.Height = value
        End Set
    End Property
    Public Property FX() As Single
        Get
            Return forzaX
        End Get
        Set(ByVal value As Single)
            forzaX = value
        End Set
    End Property
    Public Property FY() As Single
        Get
            Return forzaY
        End Get
        Set(ByVal value As Single)
            forzaY = value
        End Set
    End Property
    Public Property Tipo() As Integer
        Get
            Return type
        End Get
        Set(ByVal value As Integer)
            type = value
            If type = 2 Then
                port = New portale
            End If
        End Set
    End Property
    Public Property Attrito() As Single
        Get
            Return att
        End Get
        Set(ByVal value As Single)
            att = value
        End Set
    End Property
    Public Property Elastico() As Single
        Get
            Return elast
        End Get
        Set(ByVal value As Single)
            elast = value
        End Set
    End Property
    Public Property Massa() As Integer
        Get
            Return mass
        End Get
        Set(ByVal value As Integer)
            mass = value
        End Set
    End Property
    Public Property Bloccato() As Boolean
        Get
            Return lock
        End Get
        Set(ByVal value As Boolean)
            lock = value
        End Set
    End Property
    Public Property Colore() As Brush
        Get
            Return col
        End Get
        Set(ByVal value As Brush)
            col = value
        End Set
    End Property
    Public Property Forma() As Drawing.Rectangle
        Get
            Return rett
        End Get
        Set(ByVal value As Drawing.Rectangle)
            rett = value
        End Set
    End Property
    Public Property FormaPrec() As Drawing.Rectangle
        Get
            Return rettPrec
        End Get
        Set(ByVal value As Drawing.Rectangle)
            rettPrec = value
        End Set
    End Property
    Public Property Contatto(ByVal i As Integer) As Integer
        Get
            Return vett(i)
        End Get
        Set(ByVal value As Integer)
            vett(i) = value
        End Set
    End Property
    Public Function Appoggiato() As Boolean
        Dim x As Integer
        Dim ret As Boolean
        For Each x In vett
            If x = 2 Then
                ret = True
            End If
        Next
        Return ret
    End Function
    Public Function scontroR() As Boolean
        Dim x As Integer
        Dim ret As Boolean
        For Each x In vett
            If x = 1 Then
                ret = True
            End If
        Next
        Return ret
    End Function
    Public Function scontroL() As Boolean
        Dim x As Integer
        Dim ret As Boolean
        For Each x In vett
            If x = -1 Then
                ret = True
            End If
        Next
        Return ret
    End Function
    Public Function scontroD() As Boolean
        Dim x As Integer
        Dim ret As Boolean
        For Each x In vett
            If x = 2 Then
                ret = True
            End If
        Next
        Return ret
    End Function
    Public Function scontroU() As Boolean
        Dim x As Integer
        Dim ret As Boolean
        For Each x In vett
            If x = -2 Then
                ret = True
            End If
        Next
        Return ret
    End Function
    Public Function scontroX() As Boolean
        Dim x As Integer
        Dim ret As Boolean
        For Each x In vett
            If Math.Abs(x) = 1 Then
                ret = True
            End If
        Next
        Return ret
    End Function
    Public Function scontroY() As Boolean
        Dim x As Integer
        Dim ret As Boolean
        For Each x In vett
            If Math.Abs(x) = 2 Then
                ret = True
            End If
        Next
        Return ret
    End Function
    Public Sub addGravita(ByVal gr As Single)
        Me.Vel.Y += gr
    End Sub
    Public Sub salto(ByVal vel As Single)
        Me.Vel.Y = -10
    End Sub
    Public Sub addAttrito(ByVal at As Single)
        AddAttr(at, Me.Vel.L)
        AddAttr(at, Me.Vel.R)
        AddAttr(at, Me.Vel.U)
        AddAttr(at, Me.Vel.D)
    End Sub
    Private Sub AddAttr(ByVal at As Single, ByRef vel As Single)
        If vel > 0 Then
            If vel > at Then
                vel -= at
            Else
                vel = 0
            End If
        End If
    End Sub
End Class
