object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 135
  ClientWidth = 255
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object LanguageCheckLbl: TLabel
    Left = 25
    Top = 16
    Width = 182
    Height = 22
    Caption = 'Choose your language'
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Times New Roman'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object EnglishBtn: TRadioButton
    Left = 25
    Top = 56
    Width = 113
    Height = 17
    Caption = 'English'
    Checked = True
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Times New Roman'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    TabStop = True
    OnClick = EnglishBtnClick
  end
  object RussianBtn: TRadioButton
    Left = 150
    Top = 56
    Width = 113
    Height = 17
    Caption = 'Russian'
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Times New Roman'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    OnClick = RussianBtnClick
  end
  object CloseBtn: TButton
    Left = 144
    Top = 88
    Width = 89
    Height = 31
    Caption = 'Close'
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Times New Roman'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    OnClick = CloseBtnClick
  end
  object Nextbtn: TButton
    Left = 25
    Top = 88
    Width = 97
    Height = 31
    Caption = 'Next'
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Times New Roman'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    OnClick = NextbtnClick
  end
end
