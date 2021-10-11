object SourceEditorCustomizeDialogForm: TSourceEditorCustomizeDialogForm
  Left = 472
  Top = 436
  BorderStyle = bsDialog
  Caption = 'Editor properties...'
  ClientHeight = 392
  ClientWidth = 522
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 522
    Height = 353
    ActivePage = TabSheet1
    Align = alTop
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'General'
      object Label4: TLabel
        Left = 0
        Top = 147
        Width = 49
        Height = 13
        Caption = 'Undo limit:'
      end
      object Label5: TLabel
        Left = 0
        Top = 171
        Width = 76
        Height = 13
        Caption = 'Hover time (ms):'
      end
      object Label6: TLabel
        Left = 0
        Top = 195
        Width = 74
        Height = 13
        Caption = 'Max line length:'
      end
      object Label12: TLabel
        Left = 0
        Top = 219
        Width = 51
        Height = 13
        Caption = 'Tab length'
      end
      object GroupBox2: TGroupBox
        Left = 0
        Top = 0
        Width = 273
        Height = 137
        Caption = 'Editor settings'
        TabOrder = 0
        object CheckBox5: TCheckBox
          Left = 8
          Top = 16
          Width = 129
          Height = 17
          Caption = 'Insert mode'
          TabOrder = 0
          OnClick = CheckBox5Click
        end
        object CheckBox6: TCheckBox
          Left = 8
          Top = 40
          Width = 129
          Height = 17
          Caption = 'Preserve line ends'
          TabOrder = 1
          OnClick = CheckBox5Click
        end
        object CheckBox7: TCheckBox
          Left = 8
          Top = 64
          Width = 129
          Height = 17
          Caption = 'Overwrite blocks'
          TabOrder = 2
          OnClick = CheckBox5Click
        end
        object CheckBox8: TCheckBox
          Left = 8
          Top = 88
          Width = 129
          Height = 17
          Caption = 'Persistent blocks'
          TabOrder = 3
          OnClick = CheckBox5Click
        end
        object CheckBox9: TCheckBox
          Left = 8
          Top = 112
          Width = 129
          Height = 17
          Caption = 'Auto indent'
          TabOrder = 4
          OnClick = CheckBox5Click
        end
      end
      object RadioGroup1: TRadioGroup
        Left = 280
        Top = 0
        Width = 113
        Height = 137
        Caption = 'Double click'
        Items.Strings = (
          'Disabled'
          'Word select'
          'Line select')
        TabOrder = 1
        OnClick = CheckBox5Click
      end
      object RadioGroup2: TRadioGroup
        Left = 400
        Top = 0
        Width = 113
        Height = 137
        Caption = 'Triple click'
        Items.Strings = (
          'Disabled'
          'Word select'
          'Line select')
        TabOrder = 2
        OnClick = CheckBox5Click
      end
      object ComboBox1: TComboBox
        Left = 88
        Top = 144
        Width = 73
        Height = 21
        ItemHeight = 13
        ItemIndex = 0
        TabOrder = 3
        Text = '32000'
        OnExit = CheckBox5Click
        Items.Strings = (
          '32000')
      end
      object ComboBox2: TComboBox
        Left = 88
        Top = 168
        Width = 73
        Height = 21
        ItemHeight = 13
        ItemIndex = 0
        TabOrder = 4
        Text = '100'
        OnExit = CheckBox5Click
        Items.Strings = (
          '100')
      end
      object ComboBox3: TComboBox
        Left = 88
        Top = 192
        Width = 73
        Height = 21
        ItemHeight = 13
        ItemIndex = 0
        TabOrder = 5
        Text = '256'
        OnExit = CheckBox5Click
        Items.Strings = (
          '256')
      end
      object ComboBox9: TComboBox
        Left = 88
        Top = 216
        Width = 73
        Height = 21
        ItemHeight = 13
        ItemIndex = 0
        TabOrder = 6
        Text = '256'
        OnExit = CheckBox5Click
        Items.Strings = (
          '256')
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Display'
      ImageIndex = 1
      object GroupBox3: TGroupBox
        Left = 136
        Top = 0
        Width = 129
        Height = 73
        Caption = 'Gutter'
        TabOrder = 0
        object Label7: TLabel
          Left = 8
          Top = 43
          Width = 31
          Height = 13
          Caption = 'Width:'
        end
        object CheckBox10: TCheckBox
          Left = 8
          Top = 16
          Width = 57
          Height = 17
          Caption = 'Visible'
          TabOrder = 0
          OnClick = CheckBox5Click
        end
        object ComboBox4: TComboBox
          Left = 48
          Top = 40
          Width = 73
          Height = 21
          ItemHeight = 13
          ItemIndex = 0
          TabOrder = 1
          Text = '30'
          OnExit = CheckBox5Click
          Items.Strings = (
            '30')
        end
      end
      object GroupBox4: TGroupBox
        Left = 0
        Top = 0
        Width = 129
        Height = 73
        Caption = 'Right margin'
        TabOrder = 1
        object Label8: TLabel
          Left = 8
          Top = 43
          Width = 31
          Height = 13
          Caption = 'Width:'
        end
        object CheckBox11: TCheckBox
          Left = 8
          Top = 16
          Width = 57
          Height = 17
          Caption = 'Visible'
          TabOrder = 0
          OnClick = CheckBox5Click
        end
        object ComboBox5: TComboBox
          Left = 48
          Top = 40
          Width = 73
          Height = 21
          ItemHeight = 13
          ItemIndex = 0
          TabOrder = 1
          Text = '20'
          OnExit = CheckBox5Click
          Items.Strings = (
            '20')
        end
      end
      object GroupBox5: TGroupBox
        Left = 0
        Top = 80
        Width = 513
        Height = 191
        Caption = 'Font'
        TabOrder = 2
        object Label9: TLabel
          Left = 104
          Top = 16
          Width = 31
          Height = 13
          Caption = 'Name:'
        end
        object Label10: TLabel
          Left = 8
          Top = 16
          Width = 23
          Height = 13
          Caption = 'Size:'
        end
        object Label13: TLabel
          Left = 8
          Top = 94
          Width = 38
          Height = 13
          Caption = 'Sample:'
        end
        object ComboBox6: TComboBox
          Left = 104
          Top = 32
          Width = 401
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 0
          OnChange = CheckBox5Click
        end
        object ComboBox7: TComboBox
          Left = 8
          Top = 32
          Width = 57
          Height = 21
          ItemHeight = 13
          ItemIndex = 0
          TabOrder = 1
          Text = '15'
          OnExit = CheckBox5Click
          Items.Strings = (
            '15')
        end
        object Panel2: TPanel
          Left = 8
          Top = 110
          Width = 497
          Height = 73
          BorderStyle = bsSingle
          Caption = 'AaBbCc...xXyYzZ'
          Color = clWhite
          TabOrder = 2
        end
        object CheckBox16: TCheckBox
          Left = 10
          Top = 60
          Width = 141
          Height = 17
          Caption = 'Show space character'
          TabOrder = 3
          OnClick = CheckBox5Click
        end
        object CheckBox17: TCheckBox
          Left = 160
          Top = 60
          Width = 131
          Height = 17
          Caption = 'Show tab character'
          TabOrder = 4
          OnClick = CheckBox5Click
        end
      end
      object GroupBox6: TGroupBox
        Left = 272
        Top = 0
        Width = 241
        Height = 73
        Caption = 'Caret'
        TabOrder = 3
        object Label11: TLabel
          Left = 120
          Top = 17
          Width = 31
          Height = 13
          Caption = 'Width:'
        end
        object Label14: TLabel
          Left = 120
          Top = 41
          Width = 34
          Height = 13
          Caption = 'Height:'
        end
        object CheckBox12: TCheckBox
          Left = 8
          Top = 16
          Width = 105
          Height = 17
          Caption = 'Use system width'
          TabOrder = 0
          OnClick = CheckBox5Click
        end
        object ComboBox8: TComboBox
          Left = 160
          Top = 13
          Width = 65
          Height = 21
          ItemHeight = 13
          ItemIndex = 0
          TabOrder = 1
          Text = '2'
          OnExit = CheckBox5Click
          Items.Strings = (
            '2')
        end
        object ComboBox10: TComboBox
          Left = 160
          Top = 38
          Width = 65
          Height = 21
          ItemHeight = 13
          ItemIndex = 0
          TabOrder = 2
          Text = '2'
          OnExit = CheckBox5Click
          Items.Strings = (
            '2')
        end
        object CheckBox15: TCheckBox
          Left = 8
          Top = 40
          Width = 105
          Height = 17
          Caption = 'Use line height'
          TabOrder = 3
          OnClick = CheckBox5Click
        end
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Colors'
      ImageIndex = 2
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 241
        Height = 325
        Align = alLeft
        BevelOuter = bvNone
        TabOrder = 0
        object Label3: TLabel
          Left = 0
          Top = 0
          Width = 241
          Height = 13
          Align = alTop
          Caption = 'Element:'
        end
        object GroupBox1: TGroupBox
          Left = 0
          Top = 211
          Width = 241
          Height = 114
          Align = alBottom
          Caption = 'Attributes'
          TabOrder = 0
          object Label1: TLabel
            Left = 88
            Top = 16
            Width = 54
            Height = 13
            Caption = 'Foreground'
          end
          object Label2: TLabel
            Left = 88
            Top = 64
            Width = 58
            Height = 13
            Caption = 'Background'
          end
          object CheckBox1: TCheckBox
            Left = 8
            Top = 16
            Width = 73
            Height = 17
            Caption = 'Bold'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 0
            OnClick = ColorBox1Change
          end
          object CheckBox2: TCheckBox
            Left = 8
            Top = 40
            Width = 73
            Height = 17
            Caption = 'Italic'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsItalic]
            ParentFont = False
            TabOrder = 1
            OnClick = ColorBox1Change
          end
          object CheckBox3: TCheckBox
            Left = 8
            Top = 64
            Width = 73
            Height = 17
            Caption = 'Underline'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsUnderline]
            ParentFont = False
            TabOrder = 2
            OnClick = ColorBox1Change
          end
          object CheckBox4: TCheckBox
            Left = 8
            Top = 88
            Width = 73
            Height = 17
            Caption = 'Strike out'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsStrikeOut]
            ParentFont = False
            TabOrder = 3
            OnClick = ColorBox1Change
          end
          object ColorBox1: TColorBox
            Left = 88
            Top = 32
            Width = 145
            Height = 22
            Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbPrettyNames]
            ItemHeight = 16
            TabOrder = 4
            OnChange = ColorBox1Change
          end
          object ColorBox2: TColorBox
            Left = 88
            Top = 80
            Width = 145
            Height = 22
            Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbPrettyNames]
            ItemHeight = 16
            TabOrder = 5
            OnChange = ColorBox1Change
          end
        end
        object ListBox1: TListBox
          Left = 0
          Top = 13
          Width = 241
          Height = 198
          Align = alClient
          BevelKind = bkFlat
          BorderStyle = bsNone
          ItemHeight = 13
          TabOrder = 1
          OnClick = ListBox1Click
        end
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'Helpers'
      ImageIndex = 3
      object CheckBox13: TCheckBox
        Left = 8
        Top = 8
        Width = 129
        Height = 17
        Caption = 'Allow symbol hints'
        TabOrder = 0
        OnClick = CheckBox5Click
      end
      object CheckBox14: TCheckBox
        Left = 8
        Top = 32
        Width = 129
        Height = 17
        Caption = 'Allow code hotlinks'
        TabOrder = 1
        OnClick = CheckBox5Click
      end
    end
  end
  object BitBtn1: TBitBtn
    Left = 336
    Top = 360
    Width = 75
    Height = 25
    TabOrder = 1
    OnClick = BitBtn1Click
    Kind = bkOK
  end
  object BitBtn2: TBitBtn
    Left = 432
    Top = 359
    Width = 75
    Height = 25
    TabOrder = 2
    Kind = bkCancel
  end
end
