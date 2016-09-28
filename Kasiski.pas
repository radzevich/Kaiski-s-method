unit Kasiski;

interface

implementation

uses
  System.SysUtils;

const
  SUBSTRING_LENGTH = 3;
  STATISTIC_FILE_NAME = 'statistic.kss';

type
  PTStatisticTable = ^TStatisticTable;
  TStatisticTable = record
    trigram : string[3];
    distance : integer;
    next : PTStatisticTable;
  end;

var
  progressiveKeyStepShift : byte;

function stringsAreEqual(var encipheredText : string; const i, j, SUBSTRING_LENGTH : integer) : boolean; forward;
function greatestCommonDivisor(a, b : integer) : integer; inline; forward;
function greatestCommonDisvisionExists(const savedDistance, distance : integer) : boolean; forward; inline;
procedure compareDistances(item : PTStatisticTable; const  savedDistance, distance : integer); inline; forward;
procedure addItemToAnalizeTable(var analizeTable : PTStatisticTable; const trigram : string[3];
                                const numberOfOccurences : integer; const distance : integer); forward;

function createAnalizeTable : PTStatisticTable;
var
  analizeTable : PTStatisticTable;
begin
  new(analizeTable);
  analizeTable^.next := nil;

  Result := analizeTable;
end;

procedure addItemToAnalizeTable(const analizeTable : PTStatisticTable; const trigram : string[3];
                                const distance : integer);
begin
  new(analizeTable^.next);
  analizeTable^.next^.trigram := trigram;
  analizeTable^.next^.distance := distance;
  analizeTable^.next^.next := nil;
end;

procedure saveSubstringToAnalizeTable(analizeTable : PTStatisticTable; const trigram : string[SUBSTRING_LENGTH];
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

procedure compareDistances(item : PTStatisticTable; const  savedDistance, distance : integer); inline;
begin
  if distance < savedDistance then
    item^.distance := distance;
end;

procedure CompareSubstrings(const encipheredText : string);
var
  i, j, stringLength : integer;
  analizeTable : PTStatisticTable;
begin
  stringLength := length(encipheredText);
  analizeTable := createAnalizeTable;

  for i := 1 to (stringLength - SUBSTRING_LENGTH + 1) do
    for j := i + 3 to stringLength do
      if stringsAreEqual(encipheredText, i, j, SUBSTRING_LENGTH)
      then saveSubstringToAnalizeTable(analizeTable, copy(encipheredText, i, SUBSTRING_LENGTH), j - i);
end;

function stringsAreEqual(var encipheredText : string; const i, j, SUBSTRING_LENGTH : integer) : boolean;
var
  k : integer;
  flag : boolean;
begin
  flag := true;
  k := 0;

  while (k < SUBSTRING_LENGTH - 1) and (flag) do
  begin
    flag := (encipheredText[i + k] - encipheredText[j + k]) xor
            (encipheredText[i + k + 1] - encipheredText[j + k + 1]) = 0;
    inc(k);
  end;

  Result := flag;
end;

procedure saveStatisticToFile(var statisticTable : PTStatisticTable; const trigram : string[3]; ditance : integer);
var
  F : file of TStatisticTable;
  tempItem : TStatisticTable;
begin
  assignFile(F, STATISTIC_FILE_NAME);

  if fileExists(STATISTIC_FILE_NAME) then
  begin
    Rewrite(F);
    while statisticTable^.next <> nil do
    begin
      tempItem.trigram := statisticTable^.next^.trigram;
      tempItem.distance := statisticTable^.next^.distance;
      write(F, tempItem);
      statisticTable := statisticTable^.next;
    end;
    CloseFile(F);
  end;
end;

{
procedure checkSubString(const enchipheredText : string; const staticLeft, scanLeft, subStringSize : integer);
var
  gcd : integer;
  trigram : string;
begin
  gcd := greatestCommonDivisor(staticLeft, scanLeft);
  trigram := Copy(enchipheredText, staticLeft, scanLeft - staticLeft + 1);
  if gcd >= 3 then SaveResults(trigram, staticLeft, scanLeft, gcd);
  //здесь мог бы быть ваш частотный анализ;
end;

procedure saveResults()
}

function greatestCommonDivisor(a, b : integer) : integer;
begin
  if a = b then Result := a
  else if (a > b) then Result := greatestCommonDivisor(a - b, b)
  else Result := greatestCommonDivisor(a, b - a);
end;

end.
