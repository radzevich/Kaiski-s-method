unit StringProcessing;

interface

Uses
  System.SysUtils, Unit1;

const
  ENGLISH_ALPHABET_SIZE = 26;
  RUSSIAN_ALPHABET_SIZE = 32;

var
  alphabetSize : integer;

function GetAlphabetSize : integer;
function DecipherText(const fileName : string) : string;
procedure EncipherText(var sourceText : string; const outputFileName : string);


implementation

Uses
  Enchipher, KeyCheck, FileUnit;

procedure ConvertStringToUpperCase(var sourceText : string); forward;
procedure RemoveUnexpectedSymbols(var sourceText : string); forward;
procedure ConvertSymbolToUpperCase(var symbol : char); forward;
procedure SaveEncipheredText(fileName, encipheredText : string); forward;
function UnexpectedSymbol(const symbol : char) : boolean; forward;
//function OpenSourceText(fileName : string) : string; forward;

function GetAlphabetSize : integer;
begin
  if LanguageForm.GetLanguage = english then Result := ENGLISH_ALPHABET_SIZE
  else Result := RUSSIAN_ALPHABET_SIZE;
end;

function GetAlphabet : TAlphabet;
var
  alphabet : TAlphabet;
  i : integer;
begin
  if LanguageForm.GetLanguage = english then
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

{procedure OpenSourceTextFile;
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
end;    }

procedure EncipherText(var sourceText : string; const outputFileName : string);
var
  alphabet : TAlphabet;
  encipheredText : string;
begin
  ConvertStringToUpperCase(sourceText);
  RemoveUnexpectedSymbols(sourceText);
  alphabet := GetAlphaBet;
  FileUnit.saveTextToFile(outputFileName, GetEnchipheredText(alphabet, sourceText, KeyCheck.KeyCheckForm.GetKey, true));
  encipheredText := FileUnit.loadTextFromFile(outputFileName);
  Enchipher.Analize(encipheredText);
end;

function DecipherText(const fileName : string) : string;
var
  alphabet : TAlphabet;
  enciphedText : string;
begin
  enciphedText := FileUnit.loadTextFromFile(fileName);
  alphabet := GetAlphabet;
  SaveEncipheredText(fileName, GetEnchipheredText(alphabet, enciphedText, KeyCheck.KeyCheckForm.GetKey, DECIPHER));
  Result := FileUnit.loadTextFromFile(fileName);
end;
  
//процедура преобразования строки из нижнего регистра в верхний
procedure ConvertStringToUpperCase(var sourceText : string);
var
  i : integer;
begin
  //ветка для латинских букв
  if LanguageForm.GetLanguage = english then
    for i := 1 to Length(sourceText) do
    begin
      if (ord(sourceText[i]) >= 97) then ConvertSymbolToUpperCase(sourceText[i])
    end
  //ветка для кириллицы
  else
    for i := 1 to Length(sourceText) do
      if (sourceText[i] >= 'а') and (sourceText[i] <= 'я') and (sourceText[i] <> 'ё')
        then ConvertSymbolToUpperCase(sourceText[i])
      else if (sourceText[i] = 'ё') then sourceText[i] := 'Ё';
end;

//процедура преобразования символа нижнего регистра в верхний
procedure ConvertSymbolToUpperCase(var symbol : char);
begin
  symbol := chr(ord(symbol) - 32);
end;

//процедура удаления ненужных(игнорируемых) символов из строки
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

//функция, определяющая несоответствие символа алфавиту
//не соответствует - true, соответствует - false
function UnexpectedSymbol(const symbol : char) : boolean;
begin
  //для латиницы
  if LanguageForm.GetLanguage = english then
    Result := not ((ord(symbol) >= 65) and (ord(symbol) <= 90))
  //для кириллицы
  else
    Result := not ((symbol >= 'А') and (symbol <= 'Я') or (symbol = 'Ё'));
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
