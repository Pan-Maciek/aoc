Imports System
Imports System.IO

Module One
    
    Sub Main(args As String())
        
        If args.Length <> 1
            Console.Error.WriteLine("Usage: ./one.out <input>")
            Environment.Exit(1)
        End If
        
        Dim text = ""
        Try
            text = File.ReadAllText(args(0))
        Catch ex As Exception
            Console.Error.WriteLine(ex.Message)
            Environment.Exit(1)
        End Try
        
        Dim state as State = State.FromString(text)
        state.SimulateDays(80)
        Console.WriteLine(state.Count)
        
    End Sub
    
    Structure State
        Private _fishCount() As Integer
        Private _dayNumber As Integer
        
        Public ReadOnly Property Count As Integer
            Get 
                return _fishCount.Sum() - _fishCount(8)
            End Get
        End Property
        
        Public Sub SimulateDays(days) 
            for j = _dayNumber to (_dayNumber + days)
                Dim born = _fishCount(_dayNumber mod 7)
                _fishCount(_dayNumber mod 7) += _fishCount(7)
                _fishCount(7) = _fishCount(8)
                _fishCount(8) = born
                _dayNumber += 1
            Next
        End Sub

        Public Shared Function FromString(text As String) As State
            Dim state as State
            ReDim state._fishCount(8)
            For Each value In text.Split(",").Select(Function(s) Convert.ToInt32(s))
                state._fishCount(value) += 1
            Next
            return state
        End Function
        
    End Structure
    
End Module
