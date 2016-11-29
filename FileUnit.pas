unit FileUnit;

interface

procedure saveTextToFile(const fileName : string; const sourceText : string);
function loadTextFromFile(const fileName : string) : string;

implementation

uses
   System.SysUtils;

procedure saveTextToFile(const fileName : string; const sourceText : string);
var
   T : Text;
begin
   AssignFile(T, fileName);
   rewrite(T);
   writeln(T, sourceText);
   closeFile(T);
end;


function loadTextFromFile(const fileName : string) : string;
var
   T : text;
   buffer, resultString : string;
begin
   buffer := '';
   AssignFile(T, fileName);
   resultString := '';
   if fileExists(fileName) then
   begin
      reset(T);
      while not Eof(T) do
      begin
         readln(T, buffer);
         resultString := resultString + buffer + ' ';
      end;
      CloseFile(T);
   end;
   Result := resultString;
end;


end.
