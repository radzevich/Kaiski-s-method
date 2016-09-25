unit Kasiski;

interface

implementation

const SUBSTRING_SIZE = 3;

type
  PTAnalizeTable = ^TAnalizeTable;
  TAnalizeTable = record
    trigram : string[3];
    distance : integer;
    next : PTAnalizeTable;
  end;

function stringsAreEqual(const subString1, subString2 : string[3]) : boolean; forward;
function greatestCommonDivisor(a, b : integer) : integer; forward; inline;
function greatestCommonDisvisionExists(const savedDistance, distance : integer) : boolean; forward; inline;
procedure compareDistances(item : PTAnalizeTable; const  savedDistance, distance : integer); inline; forward;
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

procedure addItemToAnalizeTable(const analizeTable : PTAnalizeTable; const trigram : string[3]; 
                                const distance : integer);
begin
  new(analizeTable^.next);
  analizeTable^.next^.trigram := trigram;
  analizeTable^.next^.distance := distance;
  analizeTable^.next^.next := nil;
end;

procedure saveDataToAnalizeTable(analizeTable : PTAnalizeTable; const trigram : string[SUBSTRING_SIZE]; 
                                 const distance : integer);
var
  equal : boolean;
begin
  equal := false;    
  
  while ((analizeTable^.next <> nil) and not equal) do
  begin
    if stringsAreEqual(analizeTable^.trigram, trigram) then
    begin
      equal := true;
      continue;
    end;
    analizeTable := analizeTable^.next;
  end;

  if equal and greatestCommonDisvisionExists(analizeTable^.distance, distance) then
    compareDistances(analizeTable^.distance, distance)
  else
    addItemToAnalizeTable(analizeTable, trigram, distance);    
end;

function greatestCommonDisvisiorExists(const savedDistance, distance : integer) : boolean; inline;
begin
  Result := (greatestCommonDivisor(savedDistance, distance) >= 2);
end;

procedure compareDistances(item : PTAnalizeTable; const  savedDistance, distance : integer); inline;
begin
  if distance < savedDistance then
    item^.distance := distance;
end;

procedure FindSubString(const encipheredText : string);
var
  i, j, stringLength : integer;
  analizeTable : PTAnalizeTable;
begin
  stringLength := length(encipheredText);
  analizeTable := createAnalizeTable;

  for i := 1 to (stringLength - SUBSTRING_SIZE + 1) do
    for j := i + 3 to stringLength do
      if stringsAreEqual(copy(encipheredText, i, SUBSTRING_SIZE), copy(encipheredText, j, SUBSTRING_SIZE))
      then saveDataToAnalizeTable(analizeTable, copy(encipheredText, i, SUBSTRING_SIZE), j - i);
end;

function stringsAreEqual(const subString1, subString2 : string[3]) : boolean;
begin
  Result := ((subString1[1] - subString2[1]) = (subString1[2] - subString1[2])) and
            ((subString1[2] - subString1[2]) = (subString1[3] - subString1[3]));
end;

procedure checkSubString(const enchipheredText : string; const staticLeft, scanLeft, subStringSize : integer);
var
  gcd : integer;
  target : string;
begin
  gcd := greatestCommonDivisor(staticLeft, scanLeft);
  target := Copy(enchipheredText, staticLeft, scanLeft - staticLeft + 1);
  if gcd >= 3 then SaveResults(target, staticLeft, scanLeft, gcd);
  //здесь мог бы быть ваш частотный анализ;
end;

function greatestCommonDivisor(a, b : integer) : integer;
begin
  if a = b then Result := a
  else if (a > b) then Result := greatestCommonDivisor(a - b, b)
  else Result := greatestCommonDivisor(a, b - a);
end;

end.
