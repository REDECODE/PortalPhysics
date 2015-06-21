Public Class velocita
    Private vR, vL, vU, vD As Single
    Public Property L() As Single
        Get
            Return vL
        End Get
        Set(ByVal value As Single)
            vL = value
        End Set
    End Property
    Public Property R() As Single
        Get
            Return vR
        End Get
        Set(ByVal value As Single)
            vR = value
        End Set
    End Property
    Public Property U() As Single
        Get
            Return vU
        End Get
        Set(ByVal value As Single)
            vU = value
        End Set
    End Property
    Public Property D() As Single
        Get
            Return vD
        End Get
        Set(ByVal value As Single)
            vD = value
        End Set
    End Property
    Public Property X() As Single
        Get
            Return Me.R - Me.L
        End Get
        Set(ByVal value As Single)
            If value > 0 Then
                Me.L = 0
                Me.R = value
            Else
                Me.L = -value
                Me.R = 0
            End If
        End Set
    End Property
    Public Property Y() As Single
        Get
            Return Me.D - Me.U
        End Get
        Set(ByVal value As Single)
            If value > 0 Then
                Me.U = 0
                Me.D = value
            Else
                Me.U = -value
                Me.D = 0
            End If
        End Set
    End Property
    Public Sub clear()
        Me.U = 0
        Me.D = 0
        Me.R = 0
        Me.L = 0
    End Sub

End Class
