unit Results;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm3 = class(TForm)
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
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

{$R *.dfm}


procedure TForm3.ClearButtonClick(Sender: TObject);
begin
   SourceMemo.Clear;
   EncipherMemo.Clear;
   DecipherMemo.Clear;
end;


end.
