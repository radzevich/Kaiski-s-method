unit KeyCheck;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Unit1;

type
  TForm2 = class(TForm)
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
  Form2: TForm2;


implementation

{$R *.dfm}

Uses
  Enchipher, StringProcessing, Results;

procedure TForm2.CancelBtnClick(Sender: TObject);
begin
  Unit1.Form1.Visible := true;
  Form2.Visible := false;
end;

procedure TForm2.CloseBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TForm2.InorderRBtnClick(Sender: TObject);
begin
  KeyEdt.Enabled := true;
end;

procedure TForm2.NextBtnClick(Sender: TObject);
begin
  if self.Caption <> '' then
  begin
    StringProcessing.EncipherText;
    StringProcessing.DecipherText;
  end;
  Results.Form3.visible := true;
end;

procedure TForm2.ProgressiveRBtnClick(Sender: TObject);
begin
  KeyEdt.Enabled := true;
end;

procedure TForm2.SelfGeneratedRBtnClick(Sender: TObject);
begin
  KeyEdt.Enabled := false;
  Form2.KeyEdt.Text := Form2.GenerateKey;
end;

function TForm2.GenerateKey : string;
var
  size, i, low, high : integer;
  key : string;
begin
  Randomize;
  if Form1.GetLanguage = english then
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

function TForm2.GetKey : string;
begin
  key := Form2.KeyEdt.Text;
  Result := key;
  if ProgressiveRBtn.Checked then Form2.ProgresKey(key);
end;

procedure TForm2.ProgresKey(var key: string);
var
  i : integer;
begin
 //// for i := 0 to Length(key) - 1 do
   // key[i] := ord()
end;

end.
