unit KeyCheck;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Unit1;

type
  TKeyCheckForm = class(TForm)
    InorderRBtn: TRadioButton;
    ProgressiveRBtn: TRadioButton;
    SelfGeneratedRBtn: TRadioButton;
    KeyCheckLbl: TLabel;
    KeyEdt: TEdit;
    CancelBtn: TButton;
    CloseBtn: TButton;
    NextBtn: TButton;
    procedure CloseBtnClick(Sender: TObject);
    procedure SelfGeneratedRBtnClick(Sender: TObject);
    procedure InorderRBtnClick(Sender: TObject);
    procedure ProgressiveRBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure NextBtnClick(Sender: TObject);
    procedure ProgresKey(var key : string);
    function GenerateKey : string;
    function GetKey : string;
  private
    key : string;
  public
    { Public declarations }
  end;

var
  KeyCheckForm: TKeyCheckForm;


implementation

{$R *.dfm}

Uses
  Enchipher, StringProcessing, Results;

procedure TKeyCheckForm.CancelBtnClick(Sender: TObject);
begin
  Unit1.LanguageForm.Visible := true;
  KeyCheckForm.Visible := false;
end;

procedure TKeyCheckForm.CloseBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TKeyCheckForm.InorderRBtnClick(Sender: TObject);
begin
  KeyEdt.Enabled := true;
end;

procedure TKeyCheckForm.NextBtnClick(Sender: TObject);
begin
  if KeyCheckForm.KeyEdt.Text <> '' then
  begin
     Results.ResultsForm.visible := true;
     self.Visible := false;

  end;
end;

procedure TKeyCheckForm.ProgressiveRBtnClick(Sender: TObject);
begin
  KeyEdt.Enabled := true;
end;

procedure TKeyCheckForm.SelfGeneratedRBtnClick(Sender: TObject);
begin
  KeyEdt.Enabled := false;
  KeyCheckForm.KeyEdt.Text := KeyCheckForm.GenerateKey;
end;

function TKeyCheckForm.GenerateKey : string;
var
  size, i, low, high : integer;
  key : string;
begin
  Randomize;
  if LanguageForm.GetLanguage = english then
  begin
    size := random(23) + 3;
    SetLength(key, size);
    for i := 0 to size do
      key[i] :=  ENGLISH_ALPHABET[random(90 - 64)];
  end else
  begin
    size := random(30) + 3;
    SetLength(key, size);
    for i := 0 to size do
      key[i] :=  RUSSIAN_ALPHABET[random(159 - 127)];
  end;

  Result := key;
end;

function TKeyCheckForm.GetKey : string;
begin
  key := KeyCheckForm.KeyEdt.Text;
  Result := key;
  if ProgressiveRBtn.Checked then KeyCheckForm.ProgresKey(key);
end;

procedure TKeyCheckForm.ProgresKey(var key: string);
var
  i : integer;
begin
 //// for i := 0 to Length(key) - 1 do
   // key[i] := ord()
end;

end.
