Public Class portale
    Private vett() As Integer
    Private dir, colleg, esci As Integer
    Public Sub setVett(ByVal n As Integer)
        ReDim vett(n)
    End Sub
    Public Property direz() As Integer
        Get
            Return dir
        End Get
        Set(ByVal value As Integer)
            dir = value
        End Set
    End Property
    Public Property collegato() As Integer
        Get
            Return colleg
        End Get
        Set(ByVal value As Integer)
            colleg = value
        End Set
    End Property
    Public Property uscita() As Integer
        Get
            Return esci
        End Get
        Set(ByVal value As Integer)
            esci = value
        End Set
    End Property
    Public Property toccato(ByVal i As Integer) As Integer
        Get
            Return vett(i)
        End Get
        Set(ByVal value As Integer)
            vett(i) = value
        End Set
    End Property

End Class
