program KasiskiProj;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Kasiski in 'Kasiski.pas';

var
   sourceText : string;

function readText() : string;
var
   F : text;
   s : string;
begin
   s := '';
   AssignFile(F, 'sourceText.txt');
   if fileExists('sourceText.txt') then
   begin
      reset(F);
      while not Eof(F) do
         readln(F, s);
      closeFile(F);
   end;
   Result := s;
end;

begin
  try
     Kasiski.KasiskiMethod(readText, STATIC_KEY);
     Readln;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
