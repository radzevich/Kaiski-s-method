object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Form2'
  ClientHeight = 175
  ClientWidth = 505
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object KeyCheckLbl: TLabel
    Left = 192
    Top = 16
    Width = 102
    Height = 21
    Caption = 'Choose a key'
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Times New Roman'
    Font.Style = []
    ParentFont = False
  end
  object InorderRBtn: TRadioButton
    Left = 24
    Top = 48
    Width = 113
    Height = 17
    Caption = 'Inorder'
    Checked = True
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Times New Roman'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    TabStop = True
    OnClick = InorderRBtnClick
  end
  object ProgressiveRBtn: TRadioButton
    Left = 352
    Top = 48
    Width = 129
    Height = 17
    Caption = 'Progressive'
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Times New Roman'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    OnClick = ProgressiveRBtnClick
  end
  object SelfGeneratedRBtn: TRadioButton
    Left = 176
    Top = 43
    Width = 170
    Height = 31
    Caption = 'Self-generated'
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Times New Roman'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    OnClick = SelfGeneratedRBtnClick
  end
  object KeyEdt: TEdit
    Left = 24
    Top = 80
    Width = 457
    Height = 21
    TabOrder = 3
  end
  object CancelBtn: TButton
    Left = 24
    Top = 120
    Width = 113
    Height = 25
    Caption = 'Cancel'
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Times New Roman'
    Font.Style = []
    ParentFont = False
    TabOrder = 4
    OnClick = CancelBtnClick
  end
  object CloseBtn: TButton
    Left = 368
    Top = 120
    Width = 113
    Height = 25
    Caption = 'Close'
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Times New Roman'
    Font.Style = []
    ParentFont = False
    TabOrder = 5
    OnClick = CloseBtnClick
  end
  object NextBtn: TButton
    Left = 192
    Top = 120
    Width = 113
    Height = 25
    Caption = 'Cipher'
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Times New Roman'
    Font.Style = []
    ParentFont = False
    TabOrder = 6
    OnClick = NextBtnClick
  end
end
