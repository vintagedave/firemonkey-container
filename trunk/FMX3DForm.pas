unit FMX3DForm;

interface

uses
  System.SysUtils, System.Types, System.Variants, System.UITypes,
  System.Classes, FMX.Types, FMX.Dialogs, FMX.Types3D, FMX.Forms,
  FMX.Forms3D, FMX.Controls3D, FMX.Objects3D, FMX.StdCtrls, FMX.Controls, FMX.Layers3D, FMX.Ani,
  FMX.MaterialSources;

type
  TFormExample3D = class(TForm3D)
    Camera1: TCamera;
    RoundCube1: TRoundCube;
    Layer3D1: TLayer3D;
    Button1: TButton;
    LightMaterialSource1: TLightMaterialSource;
    animRotateX: TFloatAnimation;
    animRotateY: TFloatAnimation;
    Light1: TLight;
    Light2: TLight;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormExample3D: TFormExample3D;

implementation

{$R *.fmx}

procedure TFormExample3D.Button1Click(Sender: TObject);
begin
  MessageDlg('Hello from ' + (Sender as TComponent).Name, TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK], 0);
end;

end.
