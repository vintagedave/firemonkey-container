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
  object Panel1: TPanel
    Left = 16
    Top = 16
    Width = 435
    Height = 311
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelInner = bvLowered
    Caption = 'Panel1'
    TabOrder = 0
    object FireMonkeyContainer1: TFireMonkeyContainer
      Left = 2
      Top = 2
      Width = 431
      Height = 307
      OnCreateFMXForm = FireMonkeyContainer1CreateFMXForm
      OnDestroyFMXForm = FireMonkeyContainer1DestroyFMXForm
      Align = alClient
    end
  end
  object btnOpenAnotherForm: TButton
    Left = 465
    Top = 302
    Width = 203
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Open another form'
    TabOrder = 1
    OnClick = btnOpenAnotherFormClick
  end
end
