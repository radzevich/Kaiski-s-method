object KasiskiGUIForm: TKasiskiGUIForm
  Left = 0
  Top = 0
  Caption = 'Visioneer cipher'
  ClientHeight = 503
  ClientWidth = 329
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object StatisticMemo: TMemo
    Left = 8
    Top = 8
    Width = 313
    Height = 442
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object BackButton: TButton
    Left = 32
    Top = 456
    Width = 97
    Height = 39
    Caption = 'Back'
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Times New Roman'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    OnClick = BackButtonClick
  end
  object StatisticButton: TButton
    Left = 200
    Top = 456
    Width = 97
    Height = 39
    Caption = 'Statistic'
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Times New Roman'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    OnClick = StatisticButtonClick
  end
end
