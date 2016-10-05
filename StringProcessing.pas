unit StringProcessing;

interface

Uses
  System.SysUtils, Unit1;

const
  SOURCE_TEXT_FILE_NAME = 'source.txt';
  ENGLISH_ALPHABET_SIZE = 26;
  RUSSIAN_ALPHABET_SIZE = 33;

var
  alphabetSize : integer;

function GetAlphabetSize : integer;
procedure EncipherText;
procedure DecipherText;

implementation

Uses
  Enchipher, KeyCheck;

procedure ConvertStringToUpperCase(var sourceText : string); forward;
procedure RemoveUnexpectedSymbols(var sourceText : string); forward;
procedure ConvertSymbolToUpperCase(var symbol : char); forward;
procedure SaveEncipheredText(fileName, encipheredText : string); forward;
function UnexpectedSymbol(const symbol : char) : boolean; forward;
function OpenSourceText(fileName : string) : string; forward;

function GetAlphabetSize : integer;
begin
  if Form1.GetLanguage = english then Result := ENGLISH_ALPHABET_SIZE
  else Result := RUSSIAN_ALPHABET_SIZE;
end;

function GetAlphabet : TAlphabet;
var
  alphabet : TAlphabet;
  i : integer;
begin
  if Form1.GetLanguage = english then
  begin
    SetLength(alphabet, 26);
    for i := 0 to 25 do
      alphabet[i] := ENGLISH_ALPHABET[i];
  end else
  begin
    SetLength(alphabet, 33);
    for i := 0 to 32 do
      alphabet[i] := RUSSIAN_ALPHABET[i];
  end;
    
  Result := alphabet;
end;

procedure OpenSourceTextFile;
var
  textFile : Text;
  sourceText : string;
begin
  try
    AssignFile(textFile, SOURCE_TEXT_FILE_NAME);
    Reset(textFile);
    while (not EOF(textFile)) do
      Read(textFile, sourceText);
    CloseFile(textFile);
  except
    //добавить исключение
  end;
end;

procedure EncipherText;
var
  alphabet : TAlphabet;
  sourceText, encipheredText : string;
begin
  sourceText := OpenSourceText('inputFile.txt');
  ConvertStringToUpperCase(sourceText);
  RemoveUnexpectedSymbols(sourceText);
  alphabet := GetAlphaBet;
  SaveEncipheredText('outputFile.txt', GetEnchipheredText(alphabet, sourceText, KeyCheck.Form2.GetKey, true));
  encipheredText := OpenSourceText('outputFile.txt');
  Enchipher.Analize(encipheredText);
end;

procedure DecipherText;
var
  alphabet : TAlphabet;
  enciphedText : string;
begin
  enciphedText := OpenSourceText('outputFile.txt');
  alphabet := GetAlphaBet;
  SaveEncipheredText('deciferedText.txt', GetEnchipheredText(alphabet, enciphedText, KeyCheck.Form2.GetKey, false));
end;
  
//процедура преобразовани€ строки из нижнего регистра в верхний
procedure ConvertStringToUpperCase(var sourceText : string);
var
  i : integer;
begin
  //ветка дл€ латинских букв
  if Form1.GetLanguage = english then
    for i := 0 to Length(sourceText) do
    begin
      if (ord(sourceText[i]) >= 97) then ConvertSymbolToUpperCase(sourceText[i])
    end
  //ветка дл€ кириллицы
  else
    for i := 0 to Length(sourceText) do
      if (ord(sourceText[i]) >= 160) and (sourceText[i] <> 'Є')
        then ConvertSymbolToUpperCase(sourceText[i])
      else if (sourceText[i] = 'Є') then sourceText[i] := '®';
end;

//процедура преобразовани€ символа нижнего регистра в верхний
procedure ConvertSymbolToUpperCase(var symbol : char);
begin
  symbol := chr(ord(symbol) - 32);
end;

//процедура удалени€ ненужных(игнорируемых) символов из строки
procedure RemoveUnexpectedSymbols(var sourceText : string);
var
  i, shiftCount, len : integer;
begin
  shiftCount := 0;
  len := Length(sourceText);
  i := 1;
  while i <= len do
  begin
    if UnexpectedSymbol(sourceText[i + shiftCount]) then inc(shiftCount);
    sourceText[i] := sourceText[i + shiftCount];
    len := Length(sourceText) - shiftCount;
    inc(i);
  end;

  SetLength(sourceText, len);
end;

//функци€, определ€юща€ несоответствие символа алфавиту
//не соответствует - true, соответствует - false
function UnexpectedSymbol(const symbol : char) : boolean;
begin
  //дл€ латиницы
  if Form1.GetLanguage = english then
    Result := not ((ord(symbol) >= 65) and (ord(symbol) <= 90))
  //дл€ кириллицы
  else
    Result := not ((ord(symbol) >= 128) and (ord(symbol) <= 159) or (ord(symbol) = 240));
end;

procedure SaveEncipheredText(fileName, encipheredText : string);
var
  T : Text;
begin
  AssignFile(T, fileName);
  Rewrite(T);
  Writeln(T, encipheredText);
  CloseFile(T);
end;

function OpenSourceText(fileName : string) : string;
var
  T : Text;
  sourceText : string;
  i : integer;
begin
  AssignFile(T, fileName);
  Reset(T);
  SetLength(sourceText, 0);
  Readln(T, sourceText);
  CloseFile(T);

  Result := sourceText;
end;

end.
