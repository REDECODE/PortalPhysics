Imports System.Data
Public Class adoNet                'rev 1.1
    Private Shared filename As String
    Private Shared connection As String = "Provider=Microsoft.Jet.OLEDB.4.0;Data Source="
    Private Shared cn As OleDb.OleDbConnection
    Private cmd As OleDb.OleDbCommand
    Private adp As OleDb.OleDbDataAdapter
    Public daTable As New DataTable
    Public Shared Sub CreaConnessione(ByVal file As String)
        filename = file       'Non si può utilizzare il Me per accedere alle proprietà Shared
        cn = New OleDb.OleDbConnection
        cn.ConnectionString = connection & filename
    End Sub
    Public Sub New(ByVal dbTab As String)
        cmd = New OleDb.OleDbCommand
        cmd.Connection = cn
        cmd.CommandType = CommandType.TableDirect
        cmd.CommandText = dbTab
        adp = New OleDb.OleDbDataAdapter(cmd)
    End Sub
    Public Sub LeggiTabella()
        Try
            daTable.Clear()
        Catch
        End Try
        adp.MissingSchemaAction = MissingSchemaAction.AddWithKey
        adp.Fill(daTable)
    End Sub
    Public Sub Salva()
        Dim adoBuilder As New OleDb.OleDbCommandBuilder(adp)
        adp.Update(daTable)
    End Sub
End Class


