program InsertionSort;

Var x: Integer;
Var numbers : array[1..5] of Integer;

procedure InsertionSort(size : Integer );
Var i, j, index : Integer;
Begin
   For i := 2 to size do
   Begin
      index := numbers[i];
      j := i;
      While ((j > 1) AND (numbers[j-1] > index)) do
      Begin
         numbers[j] := numbers[j-1];
         j := j - 1;
      End;
      numbers[j] := index;
   End;
End;

Begin
   writeln('Insertion Example: ');

   numbers[1] := 9001;
   numbers[2] := 42;
   numbers[3] := 32;
   numbers[4] := 64;
   numbers[5] := 2;

   for x:= 0 to 5 do
      writeln('unsorted[', x, '] = ', numbers[x] );

   InsertionSort(5);

   writeln('=== sorted ===');

   for x:= 0 to 5 do
      writeln('sorted[', x, '] = ', numbers[x] );

End.