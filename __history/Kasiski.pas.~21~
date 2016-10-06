unit Kasiski;

interface

const
   PROGRESSIVE_KEY = TRUE;
   STATIC_KEY = FALSE;

type
   PTStatisticTable = ^TStatisticTable;
   TStatisticTable = record
      trigram : string[3];
      distance : integer;
      left : integer;
      next : PTStatisticTable;
   end;

var
   statisticTable : PTStatisticTable;

procedure KasiskiMethod(const encipheredText : string; const key : boolean);

implementation

uses
   System.SysUtils;

const
   SUBSTRING_LENGTH = 3;
   STATISTIC_FILE_NAME = 'statistic.kas';
   CHECK_LETTER = 'E';
   ALPHABET_SHIFT = 65;
   ALPHABET_SIZE = 26;

type
   TCipherTextTable = array of array of char;
   TGCDTable = array of array of boolean;

var
   progressiveKeyStepShift : byte;
   progressiveKey : boolean;


function stringsAreEqual(const encipheredText : string; const i, j : integer) : boolean; overload; forward;
function stringsAreEqual(trigram1, trigram2 : string) : boolean; overload; forward;
function greatestCommonDivisor(a, b : integer) : integer; inline; forward;
function greatestCommonDisvisionExists(const savedDistance, distance : integer) : boolean; inline; forward;
function getMostFrequentLetter(const cipherColumn : array of char; const columnSize : integer) : char; forward;
function chooseGCD(var gcdTable : TGCDTable; const row, column : integer) : integer; forward;
function getGreatCommonDivisior(statisticTable : PTStatisticTable) : integer; forward;
procedure compareDistances(item : PTStatisticTable; const distance : integer); inline; forward;
procedure addItemToAnalizeTable(statisticTable : PTStatisticTable; const trigram : string;
                                const distance : integer); forward;
procedure fillGCDTable(var gcdTable : TGCDTable; statisticTable : PTStatisticTable;
                       const rowCount, columnCount : integer); forward;
procedure setProgressiveKeyStepShift(const keyStepShift : byte); inline; forward;
procedure sortArrays(var countArray : array of integer; var charArray : array of char; const arraySize : integer); overload; forward;
procedure swap(var num1, num2 : integer); overload; forward;
procedure swap(var char1, char2 : char); overload; forward;
procedure CleanAnalizeTable(statisticTable : PTStatisticTable); forward;
procedure frequencyAnalysis(const cipherTable : TCipherTextTable; const columnCount, rowCount : integer); forward;



function createStatisticTable : PTStatisticTable;
begin
   new(statisticTable);
   statisticTable^.next := nil;
   Result := statisticTable;
end;


procedure addItemToAnalizeTable(statisticTable : PTStatisticTable; const trigram : string;
                                const distance : integer);
begin
   while statisticTable^.next <> nil do
      statisticTable := statisticTable^.next;
   new(statisticTable^.next);
   statisticTable^.next^.trigram := trigram;
   statisticTable^.next^.distance := distance;
   statisticTable^.next^.next := nil;
end;


procedure saveSubstringToAnalizeTable(statisticTable : PTStatisticTable; const trigram : string;
                                      const left, distance : integer);
var
   equal : boolean;
begin
   equal := false;

   while ((statisticTable^.next <> nil) and not equal) do
   begin
      if stringsAreEqual(statisticTable^.next^.trigram, trigram) then
      begin
         equal := true;
         break;
      end;
      statisticTable := statisticTable^.next;
   end;

   if not (equal and greatestCommonDisvisionExists(statisticTable^.next^.distance, distance)) then
      addItemToAnalizeTable(statisticTable, trigram, distance);
end;


function greatestCommonDisvisionExists(const savedDistance, distance : integer) : boolean; inline;
begin
   Result := (greatestCommonDivisor(savedDistance, distance) >= 2);
end;


procedure compareDistances(item : PTStatisticTable; const distance : integer); inline;
begin
   if distance < item.distance then
      item^.distance := distance;
end;


procedure compareSubstrings(var statisticTable : PTStatisticTable; const encipheredText : string);
var
   i, j, stringLength : integer;
begin
   stringLength := length(encipheredText);
   statisticTable := createStatisticTable;

   for i := 1 to (stringLength - 2 * SUBSTRING_LENGTH + 1) do
      for j := i + 3 to stringLength do
         if stringsAreEqual(encipheredText, i, j) then
            saveSubstringToAnalizeTable(statisticTable, copy(encipheredText, i, SUBSTRING_LENGTH), i, j - i);

   //CleanAnalizeTable(statisticTable)
end;


procedure CleanAnalizeTable(statisticTable : PTStatisticTable);
var
   pnt : PTStatisticTable;
begin
   while statisticTable^.next <> nil do
   begin
      //if not statisticTable^.next^.repeated then
      begin
         pnt := statisticTable^.next;
         statisticTable^.next := statisticTable^.next^.next;
         dispose(pnt);
      end
     // else
         //statisticTable := statisticTable^.next;
   end;
end;


function stringsAreEqual(const encipheredText : string; const i, j : integer) : boolean; overload;
var
   k : integer;
   flag : boolean;
begin
   flag := true;
   k := 0;

   while (k < SUBSTRING_LENGTH - 1) and (flag) do
   begin
      flag := ((ord(encipheredText[i + k]) - ord(encipheredText[j + k])) xor
              ( ord(encipheredText[i + k + 1]) - ord(encipheredText[j + k + 1]))) = 0;
      inc(k);
   end;

   setProgressiveKeyStepShift(ord(encipheredText[i]) - ord(encipheredText[j]));
   Result := flag;
end;


function stringsAreEqual(trigram1, trigram2 : string) : boolean; overload;
var
   k : integer;
   flag : boolean;
begin
   flag := true;
   k := 0;

   while (k < SUBSTRING_LENGTH - 1) and (flag) do
   begin
      flag := ((ord(trigram1[k]) - ord(trigram1[k])) xor
              ( ord(trigram1[k + 1]) - ord(trigram1[k + 1]))) = 0;
      inc(k);
   end;

   Result := flag;
end;


procedure saveStatisticToFile(statisticTable : PTStatisticTable);
var
   F : file of TStatisticTable;
   tempItem : TStatisticTable;
begin
   assignFile(F, STATISTIC_FILE_NAME);

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


function greatestCommonDivisor(a, b : integer) : integer;
begin
   if a = b then Result := a
   else if (a > b) then Result := greatestCommonDivisor(a - b, b)
   else Result := greatestCommonDivisor(a, b - a);
end;


procedure setProgressiveKeyStepShift(const keyStepShift : byte); inline;
begin
   if progressiveKey and ((keyStepShift < progressiveKeyStepShift) or (progressiveKeyStepShift = 0))
   then progressiveKeyStepShift := keyStepShift;
end;


function getProgressiveKeyStepShift : integer; inline;
begin
   Result := progressiveKeyStepShift;
end;


procedure setKeyType(const key : boolean);
begin
   progressiveKey := key;
end;


procedure getResult();
var
   F : file of TStatisticTable;
   tempItem : TStatisticTable;
begin
   assignFile(F, STATISTIC_FILE_NAME);
   Reset(F);
   while not Eof(F) do
   begin
      read(F, tempItem);
      writeln(tempItem.trigram,'   ', tempItem.distance : 6);
   end;
   CloseFile(F);
end;


procedure printTable(const cipherTable : TCipherTextTable; const gcd, rowCount : integer);
var
   i, j : integer;
begin
   for i := 0 to rowCount - 1 do
   begin
      for j := 0 to gcd - 1 do
         write(cipherTable[j, i],' ');
      writeln;
   end;
end;


procedure fillCipherTextTable(const encipheredText : string; const gcd : integer);
var
   textLength, rowCount, i, j, k : integer;
   cipherTable : TCipherTextTable;
begin
   textLength := length(encipheredText);
   rowCount := textLength div gcd + 1;
   setLength(cipherTable, gcd);
   k := 1;

   for i := 0 to gcd - 1 do
      setLength(cipherTable[i], rowCount);

   for i := 0 to gcd - 1 do
      for j := 0 to rowCount - 1 do
      begin
        cipherTable[i, j] := encipheredText[k];
        inc(k);
      end;

   printTable(cipherTable, gcd, rowCount);
   frequencyAnalysis(cipherTable, gcd, rowCount);
end;


function findKey(const frequencyArray : array of char; const keyLength : integer) : string;
var
   i : integer;
   key : string;
begin
   setLength(key, keyLength);
   for i := 1 to keyLength do
      key[i] := chr((ord(frequencyArray[i]) - ord(CHECK_LETTER)+ ALPHABET_SIZE) mod ALPHABET_SIZE + ALPHABET_SHIFT);
   Result := key;
end;


procedure frequencyAnalysis(const cipherTable : TCipherTextTable; const columnCount, rowCount : integer);
var
   i, j : integer;
   frequencyArray : array of char;
   key : string;
begin
   setLength(frequencyArray, columnCount);

   for i := 0 to columnCount - 1 do
      frequencyArray[i] := getMostFrequentLetter(cipherTable[i], rowCount);

   key := findKey(frequencyArray, columnCount);
   writeln(key);
   Readln;
end;


function getMostFrequentLetter(const cipherColumn : array of char; const columnSize : integer) : char;
var
   charArray : array of char;
   countArray : array of integer;
   i, j : integer;
begin
   setLength(charArray, columnSize);
   setLength(countArray, columnSize);

   for i := 0 to columnSize - 1 do
   begin
      countArray[i] := 0;
      for j := i + 1 to columnSize - 1 do
         if cipherColumn[i] = cipherColumn[j] then
         begin
            charArray[i] := cipherColumn[i];
            inc(countArray[i]);
         end;
   end;

   sortArrays(countArray, charArray, columnSize);
   Result := charArray[0];
end;


procedure sortArrays(var countArray : array of integer; var charArray : array of char; const arraySize : integer); overload;
var
   i, j, flag : integer;
begin
   flag := arraySize;
   for i := 0 to arraySize - 2 do
      for j := i + 1 to flag do
         if countArray[i] < countArray[j] then
         begin
            swap(countArray[i], countArray[j]);
            swap(charArray[i], charArray[j]);
            flag := j;
         end;
end;


procedure sortArrays(var countArray, gcdArray : array of integer; const arraySize : integer); overload;
var
   i, j, flag : integer;
begin
   flag := arraySize;
   for i := 1 to arraySize - 1 do
      for j := i + 1 to flag do
         if countArray[i] < countArray[j] then
         begin
            swap(countArray[i], countArray[j]);
            swap(gcdArray[i], gcdArray[j]);
            flag := j;
         end;
end;


procedure sortHighestGCD(var countArray, gcdArray : array of integer; const arraySize : integer);
var
   i, j : integer;
begin
   i := 1;
   while (i < arraySize) and (countArray[i] = countArray[i + 1]) do
   begin
      j := i + 1;
      while (j < arraySize) and (countArray[j] = countArray[j + 1]) do
      begin
         if gcdArray[i] < gcdArray[j] then
         begin
            swap(countArray[i], countArray[j]);
            swap(gcdArray[i], gcdArray[j]);
         end;
         inc(j);
      end;
      inc(i);
   end;
end;


procedure swap(var num1, num2 : integer); overload;
var
   temp : integer;
begin
   temp := num1;
   num1 := num2;
   num2 := temp;;
end;


procedure swap(var char1, char2 : char); overload;
var
   temp : char;
begin
   temp := char1;
   char1 := char2;
   char2 := temp;
end;


function statisticTableGetSize(statisticTable : PTStatisticTable) : integer;
var
   counter : integer;
begin
   statisticTable := statisticTable;
   counter := 0;
   while statisticTable^.next <> nil do
   begin
      inc(counter);
      statisticTable := statisticTable^.next;
   end;
   Result := counter;
end;

 {
function getGreatCommonDivisior(statisticTable : PTStatisticTable) : integer;
var
   gcdArray, countArray : array of integer;
   i, j, statisticTableSize : integer;
begin
   statisticTableSize := statisticTableGetSize(statisticTable);
   setLength(gcdArray, statisticTableSize);
   setLength(countArray, statisticTableSize);
   i := 1;

   while statisticTable^.next <> nil do
   begin
      gcdArray[i] := statisticTable^.next^.distance;
      countArray[i] := 0;
      for j := 1 to statisticTableSize do
         if gcdArray[j] = statisticTable^.next^.distance then
            inc(countArray[j]);
      inc(i);
   end;

   sortArrays(countArray, gcdArray, statisticTableSize);
   sortHighestGCD(gcdArray, countArray, statisticTableSize);
   Result := gcdArray[1];
end;
            }

procedure kasiskiMethod(const encipheredText : string; const key : boolean);
var
   gcd : integer;
begin
   setKeyType(key);
   compareSubstrings(statisticTable, encipheredText);
   saveStatisticToFile(statisticTable);
   gcd := getGreatCommonDivisior(statisticTable);
   fillCipherTextTable(encipheredText, gcd);
   //getResult;
   //Writeln(#13, #10, gcd);
end;


function getMaxDistance(statisticTable : PTStatisticTable) : integer;
var
   max : integer;
begin
   max := 0;
   while statisticTable^.next <> nil do
   begin
      if statisticTable^.next^.distance > max then
         max := statisticTable^.next^.distance;
      statisticTable := statisticTable^.next;
   end;
   Result := max;
end;


function getGreatCommonDivisior(statisticTable : PTStatisticTable) : integer;
var
   mid, statisticTableSize, i : integer;
   gcdTable : TGCDTable;
begin
   mid := round(sqrt(getMaxDistance(statisticTable)));
   statisticTableSize := statisticTableGetSize(statisticTable);
   setLength(gcdTable, statisticTableSize);
   for i := 0 to statisticTableSize - 1 do
      setLength(gcdTable[i], mid);
   fillGCDTable(gcdTable, statisticTable, statisticTableSize, mid);
   Result := chooseGCD(gcdTable, statisticTableSize, mid);
end;


procedure fillGCDTable(var gcdTable : TGCDTable; statisticTable : PTStatisticTable;
                       const rowCount, columnCount : integer);
var
   i, j : integer;
begin
   for i := 0 to rowCount - 1 do
   begin
      for j := 3 to columnCount - 1 do
         if statisticTable^.next^.distance mod j = 0 then
            gcdTable[i, j] := true
         else
            gcdTable[i, j] := false;
       statisticTable := statisticTable^.next;
   end;
end;


function chooseGCD(var gcdTable : TGCDTable; const row, column : integer) : integer;
var
   i, j, maxGCD, gcd, sum : integer;
begin
   maxGCD := 0;
   for i := 0 to column - 1 do
   begin
      sum := 0;
      for j := 0 to row - 1 do
         if gcdTable[j, i] then inc(sum);
      if 1.3 * sum > maxGCD then
      begin
        maxGCD := sum;
        gcd := i;
      end;
   end;
   Result := gcd;
end;



end.
