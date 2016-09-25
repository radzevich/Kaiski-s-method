unit Kasiski;

interface

implementation

const SUBSTRING_SIZE = 3;

type
  PTAnalizeTable = ^TAnalizeTable;
  TAnalizeTable = record
    trigram : string[3];
    numberOfOccurences : integer;
    distances : integer;
    next : PTAnalizeTable;
  end;

function stringsAreEqual(var encipheredText : string; position1, position2 : integer) : boolean; forward;
procedure addItemToAnalizeTable(var analizeTable : PTAnalizeTable; const trigram : string[3]; 
                                const numberOfOccurences : integer; const distance : integer); forward;

function createAnalizeTable() : PTAnalizeTable;
var
  analizeTable : PTAnalizeTable;
begin
  new(analizeTable);
  analizeTable^.next := nil;

  Result := analizeTable;
end;

procedure addItemToAnalizeTable(var analizeTable : PTAnalizeTable; const trigram : string[3];
                                 const numberOfOccurences : integer; const distance : integer);
begin
  //проход до конца таблицы
  while analizeTable^.next = nil do analizeTable := analizeTable^.next;
  //создание в конце таблицы новой записи и её инициализация
  new(analizeTable^.next);
  analizeTable^.next^.trigram := trigram;
  analizeTable^.next^.numberOfOccurences := numberOfOccurences;
  analizeTable^.next^.distances := distance;
  analizeTable^.next^.next := nil;
end;

procedure FindSubString(const encipheredText : string);
var
  i, j, stringLength : integer;
begin
  stringLength := length(encipheredText);
  

  for i := 1 to (stringLength - SUBSTRING_SIZE + 1) do
    for j := 1 to stringLength do
      if stringsAreEqual(encipheredText, i, j) then
        saveDataToAnalizeTable(analizeTable, copy(encipheredText, i, SUBSTRING_SIZE),
        );
end;

function stringsAreEqual(var encipheredText : string; position1, position2 : integer) : boolean;
begin
  Result := ((encipheredText[position1] - encipheredText[position2]) =
            (encipheredText[position1 + 1] - encipheredText[position2 + 1])) and
            ((encipheredText[position1] - encipheredText[position2]) =
            (encipheredText[position1 + 2] - encipheredText[position2 + 2]));
end;

procedure checkSubString(const enchipheredText : string; const staticLeft, scanLeft, subStringSize : integer);
var
  gcd : integer;
  target : string;
begin
  gcd := Evklid(staticLeft, scanLeft);
  target := Copy(enchipheredText, staticLeft, scanLeft - staticLeft + 1);
  if gcd >= 3 then SaveResults(target, staticLeft, scanLeft, gcd);
  //здесь мог бы быть ваш частотный анализ;
end;

function Evklid(a, b : integer) : integer;
begin
  if a = b then Result := a
  else if (a > b) then Result := Evklid(a - b, b)
  else Result := Evklid(a, b - a);
end;

end.
