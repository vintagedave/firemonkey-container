unit VCLForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, FMXForm, FMXContainer, Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    FireMonkeyContainer1: TFireMonkeyContainer;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

end.
