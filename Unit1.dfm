object Form1: TForm1
  Left = 221
  Top = 128
  Width = 870
  Height = 640
  Caption = 'Pascal Editor'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Icon.Data = {
    0000010001002020100000000000E80200001600000028000000200000004000
    0000010004000000000080020000000000000000000000000000000000000000
    0000000080000080000000808000800000008000800080800000C0C0C0008080
    80000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00CCC0
    000CCCC0000000000CCCC7777CCCCCCC0000CCCC00000000CCCC7777CCCCCCCC
    C0000CCCCCCCCCCCCCC7777CCCCC0CCCCC0000CCCCCCCCCCCC7777CCCCC700CC
    C00CCCC0000000000CCCC77CCC77000C0000CCCC00000000CCCC7777C7770000
    00000CCCC000000CCCC777777777C000C00000CCCC0000CCCC77777C777CCC00
    CC00000CCCCCCCCCC77777CC77CCCCC0CCC000CCCCC00CCCCC777CCC7CCCCCCC
    CCCC0CCCCCCCCCCCCCC7CCCCCCCCCCCC0CCCCCCCCCCCCCCCCCCCCCC7CCC70CCC
    00CCCCCCCC0CC0CCCCCCCC77CC7700CC000CCCCCC000000CCCCCC777CC7700CC
    0000CCCC00000000CCCC7777CC7700CC0000C0CCC000000CCC7C7777CC7700CC
    0000C0CCC000000CCC7C7777CC7700CC0000CCCC00000000CCCC7777CC7700CC
    000CCCCCC000000CCCCCC777CC7700CC00CCCCCCCC0CC0CCCCCCCC77CC770CCC
    0CCCCCCCCCCCCCCCCCCCCCC7CCC7CCCCCCCC0CCCCCCCCCCCCCC7CCCCCCCCCCC0
    CCC000CCCCC00CCCCC777CCC7CCCCC00CC00000CCCCCCCCCC77777CC77CCC000
    C00000CCCC0000CCCC77777C777C000000000CCCC000000CCCC777777777000C
    0000CCCC00000000CCCC7777C77700CCC00CCCC0000000000CCCC77CCC770CCC
    CC0000CCCCCCCCCCCC7777CCCCC7CCCCC0000CCCCCCCCCCCCCC7777CCCCCCCCC
    0000CCCC00000000CCCC7777CCCCCCC0000CCCC0000000000CCCC7777CCC0000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000000000000000000000}
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object SyntaxHighlightMemo1: TSyntaxHighlightMemo
    Left = 0
    Top = 0
    Width = 862
    Height = 594
    Lines.Strings = (
      '')
    KeyShortCuts = <>
    MouseDownShortCuts = <>
    MouseMoveShortCuts = <>
    EditOptions.GutterMargin = 45
    EditOptions.AutoSaveOptionsFile = 'UserData.dat'
    Align = alClient
    TabOrder = 0
    Highlighter = SyntaxHighlighter1
  end
  object TextEditorFooter1: TTextEditorFooter
    Left = 0
    Top = 594
    Width = 862
    Height = 19
    TextEditor = SyntaxHighlightMemo1
  end
  object SyntaxHighlighter1: TSyntaxHighlighter
    OnTokenizeLineClass = SyntaxHighlighter1TokenizeLineClass
    OnTokenizeLine = SyntaxHighlighter1TokenizeLine
    CharClassHighlights = <
      item
        Caption = 'Symbol'
        BackgroundColor = clNavy
        FontColor = clWhite
        FontStyle = []
      end
      item
        Caption = 'Parenthesis'
        BackgroundColor = clNavy
        FontColor = clWhite
        FontStyle = []
      end
      item
        Caption = 'Reserved word'
        BackgroundColor = clNavy
        FontColor = clWhite
        FontStyle = [fsBold]
      end
      item
        Caption = 'Assembly language'
        BackgroundColor = clNavy
        FontColor = clLime
        FontStyle = []
      end
      item
        Caption = 'Identificator'
        BackgroundColor = clNavy
        FontColor = clYellow
        FontStyle = []
      end
      item
        Caption = 'Integer number'
        BackgroundColor = clNavy
        FontColor = clLime
        FontStyle = []
      end
      item
        Caption = 'Float number'
        BackgroundColor = clNavy
        FontColor = clLime
        FontStyle = []
      end
      item
        Caption = 'Hexadecimal number'
        BackgroundColor = clNavy
        FontColor = clLime
        FontStyle = []
      end
      item
        Caption = 'String'
        BackgroundColor = clNavy
        FontColor = clRed
        FontStyle = []
      end
      item
        Caption = 'Preprocessor directive'
        BackgroundColor = clBlack
        FontColor = clWhite
        FontStyle = []
      end
      item
        Caption = 'Comment'
        BackgroundColor = clNavy
        FontColor = clAqua
        FontStyle = []
      end>
    Left = 70
    Top = 20
  end
end
