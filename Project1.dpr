program Project1;

uses
  Vcl.Forms,
  StringProcessing in 'StringProcessing.pas',
  Enchipher in 'Enchipher.pas',
  KeyCheck in 'KeyCheck.pas' {Form2},
  Unit1 in 'Unit1.pas' {Form1},
  Results in 'Results.pas' {Form3},
  FileUnit in 'FileUnit.pas',
  Kasiski in 'Kasiski.pas',
  KasiskiGUI in 'KasiskiGUI.pas' {KasiskiGUIForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TLanguageForm, LanguageForm);
  Application.CreateForm(TKeyCheckForm, KeyCheckForm);
  Application.CreateForm(TResultsForm, ResultsForm);
  Application.CreateForm(TKasiskiGUIForm, KasiskiGUIForm);
  Application.Run;
end.
