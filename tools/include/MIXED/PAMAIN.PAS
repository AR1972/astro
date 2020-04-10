
program Asmtest(input, output);
function Power2(a:integer; b:integer): integer; extern;
begin
	writeln('3 times 2 to the power of 5 is ', Power2(3,5));
end.
