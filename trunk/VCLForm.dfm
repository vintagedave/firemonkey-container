object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'VCL form'
  ClientHeight = 340
  ClientWidth = 680
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    680
    340)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 465
    Top = 18
    Width = 203
    Height = 19
    Anchors = [akTop, akRight]
    Caption = 'TFireMonkeyContainer demo'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ExplicitLeft = 551
  end
  object Label2: TLabel
    Left = 465
    Top = 48
    Width = 203
    Height = 177
    Anchors = [akTop, akRight, akBottom]
    AutoSize = False
    Caption = 
      'This is a VCL form holding VCL controls. On the left is a TFireM' +
      'onkeyContainer inside a TPanel (to show a border.)  It is holdin' +
      'g a FireMonkey form.'
    WordWrap = True
  end
  object btnOpenAnotherForm: TButton
    Left = 465
    Top = 302
    Width = 203
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Open another form'
    TabOrder = 0
    OnClick = btnOpenAnotherFormClick
  end
  object PageControl1: TPageControl
    Left = 16
    Top = 18
    Width = 443
    Height = 309
    ActivePage = TabSheet1
    TabOrder = 1
    object TabSheet1: TTabSheet
      Caption = 'TabSheet1'
      ExplicitWidth = 443
      ExplicitHeight = 291
      object Panel1: TPanel
        AlignWithMargins = True
        Left = 1
        Top = 3
        Width = 431
        Height = 275
        Margins.Left = 1
        Align = alClient
        BevelInner = bvLowered
        Caption = 'Panel1'
        TabOrder = 0
        ExplicitLeft = 0
        ExplicitTop = -30
        ExplicitWidth = 435
        ExplicitHeight = 311
        object FireMonkeyContainer1: TFireMonkeyContainer
          Left = 2
          Top = 2
          Width = 427
          Height = 271
          OnCreateFMXForm = FireMonkeyContainer1CreateFMXForm
          OnDestroyFMXForm = FireMonkeyContainer1DestroyFMXForm
          Align = alClient
          ExplicitWidth = 431
          ExplicitHeight = 307
        end
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'TabSheet2'
      ImageIndex = 1
      ExplicitWidth = 443
      ExplicitHeight = 291
      object Panel2: TPanel
        AlignWithMargins = True
        Left = 1
        Top = 3
        Width = 431
        Height = 275
        Margins.Left = 1
        Align = alClient
        BevelInner = bvLowered
        Caption = 'Panel1'
        TabOrder = 0
        ExplicitLeft = 4
        ExplicitTop = 6
        object FireMonkeyContainer2: TFireMonkeyContainer
          Left = 2
          Top = 2
          Width = 427
          Height = 271
          OnCreateFMXForm = FireMonkeyContainer2CreateFMXForm
          OnDestroyFMXForm = FireMonkeyContainer2DestroyFMXForm
          Align = alClient
        end
      end
    end
  end
end
