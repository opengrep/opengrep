Module M
    Sub Bar
        ' ... should match across #If/#Else blocks

        ' ERROR:
        sink(data)
        #If DEBUG
        x = 1
        #Else
        y = 2
        #End If
        use(data)

        ' No match: different variable in sink vs use
        sink(data2)
        #If DEBUG
        x = 1
        #End If
        use(other)
    End Sub
End Module
