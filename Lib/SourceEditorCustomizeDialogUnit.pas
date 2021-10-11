unit SourceEditorCustomizeDialogUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, VirtualScrollingWinControl, SyntaxHighlightMemo,
  StdCtrls, ExtCtrls, Buttons, DummySyntaxHighlightMemo, TextEditorFooter;

type
  TSourceEditorCustomizeDialogForm = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    Panel1: TPanel;
    GroupBox1: TGroupBox;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    Label1: TLabel;
    ColorBox1: TColorBox;
    Label2: TLabel;
    ColorBox2: TColorBox;
    Label3: TLabel;
    ListBox1: TListBox;
    GroupBox2: TGroupBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    CheckBox8: TCheckBox;
    CheckBox9: TCheckBox;
    RadioGroup1: TRadioGroup;
    RadioGroup2: TRadioGroup;
    ComboBox1: TComboBox;
    Label4: TLabel;
    Label5: TLabel;
    ComboBox2: TComboBox;
    Label6: TLabel;
    ComboBox3: TComboBox;
    GroupBox3: TGroupBox;
    CheckBox10: TCheckBox;
    Label7: TLabel;
    ComboBox4: TComboBox;
    GroupBox4: TGroupBox;
    Label8: TLabel;
    CheckBox11: TCheckBox;
    ComboBox5: TComboBox;
    GroupBox5: TGroupBox;
    Label9: TLabel;
    ComboBox6: TComboBox;
    Label10: TLabel;
    ComboBox7: TComboBox;
    GroupBox6: TGroupBox;
    Label11: TLabel;
    CheckBox12: TCheckBox;
    ComboBox8: TComboBox;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    TabSheet4: TTabSheet;
    CheckBox13: TCheckBox;
    CheckBox14: TCheckBox;
    Label12: TLabel;
    ComboBox9: TComboBox;
    Panel2: TPanel;
    Label13: TLabel;
    Label14: TLabel;
    ComboBox10: TComboBox;
    CheckBox15: TCheckBox;
    CheckBox16: TCheckBox;
    CheckBox17: TCheckBox;
    procedure ListBox1Click(Sender: TObject);
    procedure ColorBox1Change(Sender: TObject);
    procedure CheckBox5Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
  private
    FMemo:TDummySyntaxHighlightMemo;
    FSource:TCustomSyntaxHighlightMemo;
    FOldData:TMemoryStream;

    procedure LoadFromSource;
    procedure SaveToSource;
    procedure Init;
  public
    procedure Execute(Memo:TCustomSyntaxHighlightMemo);
  end;

var
  SourceEditorCustomizeDialogForm: TSourceEditorCustomizeDialogForm=nil;

procedure CustomizeEditor(Memo:TCustomSyntaxHighlightMemo);

implementation

uses SyntaxHighlighter;

{$R *.dfm}

procedure CustomizeEditor(Memo:TCustomSyntaxHighlightMemo);
begin
  if not Assigned(SourceEditorCustomizeDialogForm) then
    Application.CreateForm(TSourceEditorCustomizeDialogForm,SourceEditorCustomizeDialogForm);
  SourceEditorCustomizeDialogForm.Execute(Memo);
end;

{ TSourceEditorCustomizeDialogForm }

procedure TSourceEditorCustomizeDialogForm.Execute(
  Memo: TCustomSyntaxHighlightMemo);
begin
  FOldData:=TMemoryStream.Create;
  TabSheet3.DestroyComponents;
  FSource:=Memo;
  FMemo:=TDummySyntaxHighlightMemo.Create(TabSheet3,Memo);
  FMemo.Parent:=TabSheet3;
  FMemo.Align:=alClient;
  Init;
  if ShowModal<>mrOk then begin
    FOldData.Position:=0;
    FSource.EditOptions.ReadCustomData(FOldData);
  end;
  FreeAndNil(FOldData);
end;

procedure TSourceEditorCustomizeDialogForm.ListBox1Click(Sender: TObject);
var
  t:Boolean;
begin
  if ListBox1.ItemIndex>-1 then begin
    Tag:=1;
    with FMemo.HighlightData[ListBox1.ItemIndex] do begin
      CheckBox1.Checked:=fsBold in FontStyle;
      CheckBox2.Checked:=fsItalic in FontStyle;
      CheckBox3.Checked:=fsUnderline in FontStyle;
      CheckBox4.Checked:=fsStrikeOut in FontStyle;
      ColorBox1.Selected:=FontColor;
      ColorBox2.Selected:=BackgroundColor;
    end;
    t:=FMemo.HighlightClassHasText[ListBox1.ItemIndex];
  end else
    t:=False;
  ColorBox1.Enabled:=ListBox1.ItemIndex>-1;
  ColorBox2.Enabled:=t;
  CheckBox1.Enabled:=t;
  CheckBox2.Enabled:=t;
  CheckBox3.Enabled:=t;
  CheckBox4.Enabled:=t;
  Tag:=0;
end;

procedure TSourceEditorCustomizeDialogForm.ColorBox1Change(
  Sender: TObject);
var
  h:TCharClassHighlight;
begin
  if Tag>0 then
    Exit;
  h:=FMemo.HighlightData[ListBox1.ItemIndex];
  h.FontColor:=ColorBox1.Selected;
  h.BackgroundColor:=ColorBox2.Selected;
  h.FontStyle:=[];
  if CheckBox1.Checked then
    Include(h.FontStyle,fsBold);
  if CheckBox2.Checked then
    Include(h.FontStyle,fsItalic);
  if CheckBox3.Checked then
    Include(h.FontStyle,fsUnderline);
  if CheckBox4.Checked then
    Include(h.FontStyle,fsStrikeOut);
  FMemo.HighlightData[ListBox1.ItemIndex]:=h;
end;

procedure TSourceEditorCustomizeDialogForm.LoadFromSource;
begin
  Tag:=1;
  with FMemo,FMemo.EditOptions do begin
    CheckBox5.Checked:=WriteMode=emInsert;
    CheckBox6.Checked:=PreserveLineEnd;
    CheckBox7.Checked:=OverwriteBlocks;
    CheckBox8.Checked:=PersistentBlocks;
    CheckBox9.Checked:=AutoIndent;
    RadioGroup1.ItemIndex:=Integer(DoubleClickAction);
    RadioGroup2.ItemIndex:=Integer(TripleClickAction);
    ComboBox1.Text:=IntToStr(UndoLimit);
    CheckBox16.Checked:=ShowSpace;
    CheckBox17.Checked:=ShowTab;
    ComboBox2.Text:=IntToStr(HoverTime);
    ComboBox3.Text:=IntToStr(MaxLineSize);
    CheckBox10.Checked:=GutterVisible;
    ComboBox4.Text:=IntToStr(GutterMargin);
    CheckBox11.Checked:=RightMarginVisible;
    ComboBox5.Text:=IntToStr(RightMarginWidth);
    ComboBox6.ItemIndex:=ComboBox6.Items.IndexOf(Font.Name);
    ComboBox7.Text:=IntToStr(Font.Size);
    Panel2.Font.Name:=Font.Name;
    Panel2.Font.Size:=Font.Size;
    ComboBox8.Text:=IntToStr(CaretWidth);
    CheckBox12.Checked:=UseSystemCaretWidth;
    ComboBox10.Text:=IntToStr(CaretHeight);
    CheckBox15.Checked:=UseSystemCaretHeight;
    ComboBox9.Text:=IntToStr(TabLength);
    CheckBox13.Checked:=AllowSymbolHints;
    CheckBox14.Checked:=AllowCodeLinks;
  end;
  Tag:=0;
end;

procedure TSourceEditorCustomizeDialogForm.SaveToSource;
begin
  if Tag>0 then
    Exit;
  with FMemo,FMemo.EditOptions do begin
    if CheckBox5.Checked then
      WriteMode:=emInsert
    else
      WriteMode:=emOverwrite;
    PreserveLineEnd:=CheckBox6.Checked;
    OverwriteBlocks:=CheckBox7.Checked;
    PersistentBlocks:=CheckBox8.Checked;
    AutoIndent:=CheckBox9.Checked;
    DoubleClickAction:=TMultiClickAction(RadioGroup1.ItemIndex);
    TripleClickAction:=TMultiClickAction(RadioGroup2.ItemIndex);
    UndoLimit:=StrToInt(ComboBox1.Text);
    ShowSpace:=CheckBox16.Checked;
    ShowTab:=CheckBox17.Checked;
    HoverTime:=StrToInt(ComboBox2.Text);
    MaxLineSize:=StrToInt(ComboBox3.Text);
    GutterVisible:=CheckBox10.Checked;
    GutterMargin:=StrToInt(ComboBox4.Text);
    RightMarginVisible:=CheckBox11.Checked;
    RightMarginWidth:=StrToInt(ComboBox5.Text);
    Font.Name:=ComboBox6.Text;
    Font.Size:=StrToInt(ComboBox7.Text);
    Panel2.Font.Name:=Font.Name;
    Panel2.Font.Size:=Font.Size;
    CaretWidth:=StrToInt(ComboBox8.Text);
    UseSystemCaretWidth:=CheckBox12.Checked;
    CaretHeight:=StrToInt(ComboBox10.Text);
    UseSystemCaretHeight:=CheckBox15.Checked;
    TabLength:=StrToInt(ComboBox9.Text);
    AllowSymbolHints:=CheckBox13.Checked;
    AllowCodeLinks:=CheckBox14.Checked;
    FSource.EditOptions.Assign(FMemo.EditOptions);
    FSource.Font.Assign(Font);
  end;
end;

function FontCallBack(const LF:PLogFont;const p:Pointer;dwType:DWORD;lpData:TSourceEditorCustomizeDialogForm):Integer;stdcall;
begin
  if LF.lfPitchAndFamily and FIXED_PITCH=FIXED_PITCH then
    lpData.ComboBox6.Items.Add(LF.lfFaceName);
  Result:=1;
end;

procedure TSourceEditorCustomizeDialogForm.Init;
var
  a:Integer;
  LF:TLogFont;
begin
  Tag:=1;
  FSource.EditOptions.WriteCustomData(FOldData);
  FMemo.EditOptions.Assign(FSource.EditOptions);
  ComboBox6.Clear;
  ZeroMemory(@LF,SizeOf(LF));
  EnumFontFamiliesEx(Canvas.Handle,LF,@FontCallBack,Cardinal(Self),0);
  ListBox1.Clear;
  with FMemo do
    for a:=0 to HighlightClassCount-1 do
      ListBox1.Items.Add(HighlightClassName[a]);
  ListBox1Click(nil);
  LoadFromSource;
end;

procedure TSourceEditorCustomizeDialogForm.CheckBox5Click(Sender: TObject);
begin
  SaveToSource;
end;

procedure TSourceEditorCustomizeDialogForm.BitBtn1Click(Sender: TObject);
begin
  SaveToSource;
end;

end.
