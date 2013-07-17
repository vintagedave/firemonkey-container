program Example;

uses
  Vcl.Forms,
  VCLForm in 'VCLForm.pas' {Form1},
  FMXForm in 'FMXForm.pas' {FireMonkeyForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TFireMonkeyForm, FireMonkeyForm);
  Application.Run;
end.
