program Visioneer;

uses
  Vcl.Forms,
  StringProcessing in 'StringProcessing.pas',
  Enchipher in 'Enchipher.pas',
  KeyCheck in 'KeyCheck.pas' {KeyCheckForm},
  Unit1 in 'Unit1.pas' {LanguageForm},
  Results in 'Results.pas' {ResultsForm},
  FileUnit in 'FileUnit.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TLanguageForm, LanguageForm);
  Application.CreateForm(TKeyCheckForm, KeyCheckForm);
  Application.CreateForm(TResultsForm, ResultsForm);
  Application.Run;
end.
