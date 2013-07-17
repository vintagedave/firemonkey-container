object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'VCL form'
  ClientHeight = 369
  ClientWidth = 681
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    681
    369)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 466
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
    Left = 466
    Top = 48
    Width = 203
    Height = 300
    Anchors = [akTop, akRight, akBottom]
    AutoSize = False
    Caption = 
      'This is a VCL form holding VCL controls. On the left is a TFireM' +
      'onkeyContainer inside a TPanel (to show a border.)  It is holdin' +
      'g a FireMonkey form.'
    WordWrap = True
    ExplicitLeft = 551
    ExplicitHeight = 343
  end
  object Panel1: TPanel
    Left = 16
    Top = 16
    Width = 436
    Height = 334
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelInner = bvLowered
    Caption = 'Panel1'
    TabOrder = 0
    ExplicitWidth = 521
    ExplicitHeight = 377
    object FireMonkeyContainer1: TFireMonkeyContainer
      Left = 2
      Top = 2
      Width = 432
      Height = 330
      FireMonkeyForm = FireMonkeyForm.Owner
      Align = alClient
      ExplicitWidth = 517
      ExplicitHeight = 373
    end
  end
end
