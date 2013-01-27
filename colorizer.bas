REM  *****  BASIC  *****

Sub ColorRange()
	Dim oDoc As Object
	Dim InRange As Object, OutRange As Object
	Dim i As Long, j As Long, c As Long
	Dim v As Double
	
	oDoc = ThisComponent
	
	InRange = oDoc.sheets(0).getCellRangeByName("s2:ag1061")
	OutRange = oDoc.sheets(0).getCellRangeByName("d2:R1061")
	
	for i = 0 to InRange.Rows.Count - 1
		for j = 0 to InRange.Columns.Count - 1
			v = InRange.getCellByPosition(j,i).Value
			if v <> 0 then
				c = getColor(v)
				OutRange.getCellByPosition(j,i).CellBackColor = c
			endif
		next j
	next i

End Sub


Function getColor(x As Double) As Long
	Dim Reds
	Dim Greens
	
	Reds = Array(16773103,16768991,16764879,16760767,16756655,16752543,16748431,16744576)
	Greens = Array(15728623,14680031,13631439,12582847,11534255,10485663,9437071,8454016)

	if (x < 0) then
		getColor = Reds(getColorIndex(x))
	else
		getColor = Greens(getColorIndex(x))
	end if
End Function


Function getColorIndex(x As Double) As Long
	Dim x1 As Double
	Dim x2 As Long
	x1 = 4*Abs(x)
	if x1 > 7 then
		x1 = 7
	end if
	x2 = Val(Format(x1,"0"))
	getColorIndex = x2
End Function


