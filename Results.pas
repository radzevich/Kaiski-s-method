unit Results;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

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
    procedure ClearButtonClick(Sender: TObject);
    procedure EncipherButtonClick(Sender: TObject);
    procedure DecipherButtonClick(Sender: TObject);
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
   FileUnit, StringProcessing;

const
   SOURCE_FILE_NAME = 'E:\������\2 course\IT\����\���� 1_�������\kasiski\Win32\Debug\sourceText.txt';
   ENCIPHERED_TEXT_FILE_NAME = 'output.txt';


procedure TResultsForm.ClearButtonClick(Sender: TObject);
begin
   SourceMemo.Clear;
   EncipherMemo.Clear;
   DecipherMemo.Clear;
end;


procedure encipherText(var sourceText : string);
begin
   StringProcessing.EncipherText(sourceText, ENCIPHERED_TEXT_FILE_NAME);
   Results.ResultsForm.EncipherMemo.Text := FileUnit.loadTextFromFile(ENCIPHERED_TEXT_FILE_NAME);
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

end.
