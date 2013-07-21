unit VCLForm;

interface

uses
  FMX.Forms, Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, FMXForm, FMXContainer, Vcl.ExtCtrls,
  Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    FireMonkeyContainer1: TFireMonkeyContainer;
    Label1: TLabel;
    Label2: TLabel;
    btnOpenAnotherForm: TButton;
    procedure FireMonkeyContainer1CreateFMXForm(var Form: TCommonCustomForm);
    procedure FireMonkeyContainer1DestroyFMXForm(var Form: TCommonCustomForm;
      var Action: TCloseHostedFMXFormAction);
    procedure btnOpenAnotherFormClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btnOpenAnotherFormClick(Sender: TObject);
begin
  TForm1.Create(Application).Show;
end;

procedure TForm1.FireMonkeyContainer1CreateFMXForm(var Form: TCommonCustomForm);
begin
  if not Assigned(Form) then Form := TFireMonkeyForm.Create(nil);
end;

procedure TForm1.FireMonkeyContainer1DestroyFMXForm(var Form: TCommonCustomForm;
  var Action: TCloseHostedFMXFormAction);
begin
  Action := fcaFree;
end;

end.
