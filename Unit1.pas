unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    EnglishBtn: TRadioButton;
    RussianBtn: TRadioButton;
    LanguageCheckLbl: TLabel;
    CloseBtn: TButton;
    Nextbtn: TButton;
    procedure RussianBtnClick(Sender: TObject);
    procedure EnglishBtnClick(Sender: TObject);
    procedure CloseBtnClick(Sender: TObject);
    procedure NextbtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    type
      TLanguage = (english, russian);
    var
      language : TLanguage;
      procedure SetLanguage(const languageIsEnglish : boolean);
  public
    function GetLanguage : TLanguage;
  end;

var
  Form1: TForm1;


implementation

{$R *.dfm}
Uses
  KeyCheck;

procedure TForm1.CloseBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TForm1.EnglishBtnClick(Sender: TObject);
begin
  Form1.LanguageCheckLbl.Caption := 'Choose your language';
  Form1.CloseBtn.Caption := 'Close';
  Form1.Nextbtn.Caption := 'Next';
  KeyCheck.Form2.KeyCheckLbl.Caption := 'Choose a key';
  KeyCheck.Form2.InorderRBtn.Caption := 'Inorder';
  KeyCheck.Form2.ProgressiveRBtn.Caption := 'Progressive';
  KeyCheck.Form2.SelfGeneratedRBtn.Caption := 'Self-generated';
  KeyCheck.Form2.CancelBtn.Caption := 'Cancel';
  KeyCheck.Form2.NextBtn.Caption := 'Cipher';
  KeyCheck.Form2.CloseBtn.Caption := 'Close';
  language := english;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  language := english;
end;

procedure TForm1.NextbtnClick(Sender: TObject);
begin
  Form1.Visible := false;
  KeyCheck.Form2.Visible := true;
end;

procedure TForm1.RussianBtnClick(Sender: TObject);
begin
  LanguageCheckLbl.Caption := 'Выберите язык';
  Form1.CloseBtn.Caption := 'Закрыть';
  Form1.Nextbtn.Caption := 'Далее';
  KeyCheck.Form2.KeyCheckLbl.Caption := 'Выберите ключ';
  KeyCheck.Form2.InorderRBtn.Caption := 'Прямой';
  KeyCheck.Form2.ProgressiveRBtn.Caption := 'Прогрессивный';
  KeyCheck.Form2.SelfGeneratedRBtn.Caption := 'Самогенерирующийся';
  KeyCheck.Form2.CancelBtn.Caption := 'Назад';
  KeyCheck.Form2.NextBtn.Caption := 'Шифровать';
  KeyCheck.Form2.CloseBtn.Caption := 'Закрыть';
  language := english;
end;

function TForm1.GetLanguage : TLanguage;
begin
  Result := self.language;
end;

procedure TForm1.SetLanguage(const languageIsEnglish : Boolean);
begin
  if languageIsEnglish then self.language := english
  else self.language := russian;
end;

end.
