unit Results;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

const
   SOURCE_FILE_NAME = 'E:\Универ\2 course\IT\Лабы\лаба 1_Виженер\kasiski\Win32\Debug\sourceText.txt';
   ENCIPHERED_TEXT_FILE_NAME = 'output.txt';

type
  TResultsForm = class(TForm)
    SourceMemo: TMemo;
    EncipherMemo: TMemo;
    DecipherMemo: TMemo;
    SourceLbl: TLabel;
    EncipheredLbl: TLabel;
    DecipheredLbl: TLabel;
    EncipherButton: TButton;
    DecipherButton: TButton;
    ClearButton: TButton;
    KasiskiButton: TButton;
    BackButton: TButton;
    CloseButton: TButton;
    procedure ClearButtonClick(Sender: TObject);
    procedure EncipherButtonClick(Sender: TObject);
    procedure DecipherButtonClick(Sender: TObject);
    procedure KasiskiButtonClick(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
    procedure BackButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ResultsForm: TResultsForm;

implementation

{$R *.dfm}

uses
   FileUnit, StringProcessing, KasiskiGUI, KeyCheck;

type
  PTStat = ^TStat;
  TStat = record
    keyLength : integer;
    founded : integer;
    trys : integer;
    part : integer;
  end;


procedure TResultsForm.BackButtonClick(Sender: TObject);
begin
  KeyCheck.KeyCheckForm.Visible := true;
  ResultsForm.Visible := false;
end;

procedure TResultsForm.ClearButtonClick(Sender: TObject);
begin
   SourceMemo.Clear;
   EncipherMemo.Clear;
   DecipherMemo.Clear;
end;


procedure encipherText(var sourceText : string);
begin
   StringProcessing.EncipherText(sourceText, ENCIPHERED_TEXT_FILE_NAME);
   Results.ResultsForm.EncipherMemo.Text := FileUnit.loadTextFromFile('enciphered.txt');
end;


procedure initializeSourceText(var sourceText : string);
begin
   if ResultsForm.SourceMemo.Text <> '' then
      sourceText := ResultsForm.SourceMemo.Text
   else
   begin
      sourceText := FileUnit.loadTextFromFile(SOURCE_FILE_NAME);
      ResultsForm.SourceMemo.Text := sourceText;
   end;
end;


procedure DecipherText;
var
   decipheredText : string;
begin
   decipheredText := StringProcessing.DecipherText(ENCIPHERED_TEXT_FILE_NAME);
   ResultsForm.DecipherMemo.Text := decipheredText;
end;


procedure TResultsForm.CloseButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TResultsForm.DecipherButtonClick(Sender: TObject);
begin
   decipherText;
end;


procedure TResultsForm.EncipherButtonClick(Sender: TObject);
var
   sourceText : string;
begin
   initializeSourceText(sourceText);
   if sourceText <> '' then encipherText(sourceText);
end;

procedure TResultsForm.KasiskiButtonClick(Sender: TObject);
begin
   KasiskiGUI.KasiskiGUIForm.Visible := true;
   KasiskiGUIForm.StatisticButtonClick(Sender)
end;

procedure addToList(stat : PTStat; const keyLength, gcd, textLength : integer);
begin
  {while (stat^.next <> nil) and (stat^.next^.keyLength <> keyLength) do
    stat := stat^.next;
  if stat^.next^.keyLength = keyLength then
    if gcd =  }
end;

procedure statistic(const keyLength, gcd, textLength : integer);
var
  T : Text;
  fileName : string;
  stat : array [1..20] of TStat;
  i : integer;
begin
  fileName := 'stat.txt';
  Assign(T, fileName);
  if fileExists(fileName) then
  begin
    Reset(T);
    i := 1;
    while not(eof(T)) do
    begin
      readln(T, stat[i].keyLength, stat[i].founded, stat[i].trys, stat[i].part);
      inc(i);
    end;
  end;
  closeFile(T);
  if keyLength = gcd then
    inc(stat[keyLength].founded);
  inc(stat[keyLength].trys);
  stat[keyLength].part := round(100 * (stat[keyLength].founded / stat[keyLength].trys));
  Rewrite(T);
  i := 1;
  for i := 1 to 20 do
    writeln(T, stat[i].keyLength, '   ',stat[i].founded,'   ', stat[i].trys,'   ', stat[i].part);
  CloseFile(T);
end;

end.
