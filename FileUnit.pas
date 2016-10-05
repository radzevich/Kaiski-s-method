unit FileUnit;

interface

procedure saveTextToFile(const fileName, sourceText : string);

implementation

uses
   System.SysUtils;

procedure saveTextToFile(const fileName, sourceText : string);
var
   T : Text;
begin
   AssignFile(T, fileName);
   rewrite(T);
   write(T, sourceText);
   closeFile(T);
end;


function loadTextFromFile(const fileName : string) : string;
var
   T : text;
   buffer : string;
begin
   buffer := '';
   AssignFile(T, fileName);
   if fileExists(fileName) then
   begin
      reset(T);
      while not Eof(T) do
         read(T, buffer);
      CloseFile(T);
   end;
   Result := buffer;
end;


end.
