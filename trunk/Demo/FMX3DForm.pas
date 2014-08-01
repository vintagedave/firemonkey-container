unit FMX3DForm;

interface

uses
  System.SysUtils, System.Types, System.Variants, System.UITypes,
  System.Classes, FMX.Types, FMX.Dialogs, FMX.Types3D, FMX.Forms,
  FMX.Forms3D, FMX.Controls3D, FMX.Objects3D, FMX.StdCtrls, FMX.Controls, FMX.Ani,
  FMX.MaterialSources;

type
  TFormExample3D = class(TForm3D)
    Camera1: TCamera;
    RoundCube1: TRoundCube;
    LightMaterialSource1: TLightMaterialSource;
    animRotateX: TFloatAnimation;
    animRotateY: TFloatAnimation;
    Light1: TLight;
    Light2: TLight;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormExample3D: TFormExample3D;

implementation

{$R *.fmx}

end.
