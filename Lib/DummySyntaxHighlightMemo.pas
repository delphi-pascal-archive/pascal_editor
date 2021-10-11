unit DummySyntaxHighlightMemo;

interface

uses
  SysUtils,Windows,Messages,Classes,SyntaxHighlightMemo,SyntaxHighlighter,
  Controls,Forms;

type
  TDummySyntaxHighlightMemo=class(TCustomSyntaxHighlightMemo)
  private
    FSource:TCustomSyntaxHighlightMemo;

    procedure WMSetCursor(var Message:TMessage);message WM_SETCURSOR;
  protected
    function PosClass(p:TPoint):TCharClass;override;

    function GetHighlightClassCount:Integer;override;
    function GetHighlightClassName(Index:Integer):string;override;
    function GetHighlightData(Index:Integer):TCharClassHighlight;override;
    procedure SetHighlightData(Index:Integer;const Value:TCharClassHighlight);override;

    procedure MouseDown(Button:TMouseButton;Shift:TShiftState;X,Y:Integer);override;
    procedure MouseMove(Shift:TShiftState;X,Y:Integer);override;
    procedure MouseUp(Button:TMouseButton;Shift:TShiftState;X,Y:Integer);override;
    procedure KeyDown(var Key:Word;Shift:TShiftState);override;
    procedure KeyUp(var Key:Word;Shift:TShiftState);override;
    procedure KeyPress(var Key:Char);override;
//    function DoMouseWheelDown(Shift:TShiftState;MousePos:TPoint):Boolean;override;
//    function DoMouseWheelUp(Shift:TShiftState;MousePos:TPoint):Boolean;override;
    procedure DblClick;override;
  public
    constructor Create(AOwner:TComponent;ASource:TCustomSyntaxHighlightMemo);reintroduce;virtual;

    procedure ClearMarks;override;

    function CharClassHighlight(ClassID:TCharClass):TCharClassHighlight;override;
    function CharClassCanJump(ClassID:TCharClass):Boolean;override;
    procedure CharClassJump(ClassID:TCharClass;Text:string;Pos:TPoint);override;
    function CharClassCanHint(ClassID:TCharClass):Boolean;override;
    function CharClassHint(ClassID:TCharClass;Text:string;Pos:TPoint):string;override;
    procedure TokenizeLineClass(const LastLineClass:TLineClass;var NewLineClass:TLineClass;const TextBuffer:PChar;const TextLength:Integer);override;
    procedure TokenizeLine(const LineClass:TLineClass;const TextBuffer:PChar;const TextLength:Integer;CharClass:PCharClassArray);override;
  end;

implementation

uses
  SourceEditorCustomizeDialogUnit;

{ TDummySyntaxHighlightMemo }

function TDummySyntaxHighlightMemo.CharClassCanHint(
  ClassID: TCharClass): Boolean;
begin
  Result:=False;
end;

function TDummySyntaxHighlightMemo.CharClassCanJump(
  ClassID: TCharClass): Boolean;
begin
  Result:=FSource.CharClassCanJump(ClassID);
end;

function TDummySyntaxHighlightMemo.CharClassHighlight(
  ClassID: TCharClass): TCharClassHighlight;
begin
  Result:=FSource.CharClassHighlight(ClassID);
end;

function TDummySyntaxHighlightMemo.CharClassHint(ClassID: TCharClass;
  Text: string; Pos: TPoint): string;
begin

end;

procedure TDummySyntaxHighlightMemo.CharClassJump(ClassID: TCharClass;
  Text: string; Pos: TPoint);
begin

end;

procedure TDummySyntaxHighlightMemo.ClearMarks;
begin

end;

constructor TDummySyntaxHighlightMemo.Create(AOwner: TComponent;
  ASource: TCustomSyntaxHighlightMemo);
var
  h:THighlightData;
  t:TTrackingData;
begin
  inherited Create(AOwner);
  Lines.BeginUpdate;
  DoubleBuffered:=True;
  FSource:=ASource;
  Lines.Assign(FSource.CustomText);
  Lines.Insert(0,'Selected text');
  Lines.Insert(0,'Hot link');
  Lines.Insert(0,'Warning line');
  Lines.Insert(0,'Execute error line');
  Lines.Insert(0,'Compile error line');
  Lines.Insert(0,'Search match');
  SelKind:=skLinear;
  Select(Point(0,5),Point(13,5));
  h:=_HighlightData;
  with h do begin
    BlockPositions[hbSearch].Y:=0;
    BlockPositions[hbCompileError].Y:=1;
    BlockPositions[hbExecuteError].Y:=2;
    BlockPositions[hbWarning].Y:=3;
  end;
  _HighlightData:=h;
  t:=_TrackingData;
  with t do begin
    JumpHighlight:=True;
    p1:=Point(0,4);
    p2:=Point(7,4);
  end;
  _TrackingData:=t;
  Lines.EndUpdate;
  BevelKind:=bkFlat;
end;

procedure TDummySyntaxHighlightMemo.DblClick;
begin

end;

function TDummySyntaxHighlightMemo.GetHighlightClassCount: Integer;
begin
  Result:=FSource.HighlightClassCount;
end;

function TDummySyntaxHighlightMemo.GetHighlightClassName(
  Index: Integer): string;
begin
  Result:=FSource.HighlightClassName[Index];
end;

function TDummySyntaxHighlightMemo.GetHighlightData(
  Index: Integer): TCharClassHighlight;
begin
  Result:=FSource.HighlightData[Index];
end;

procedure TDummySyntaxHighlightMemo.KeyDown(var Key: Word;
  Shift: TShiftState);
begin

end;

procedure TDummySyntaxHighlightMemo.KeyPress(var Key: Char);
begin

end;

procedure TDummySyntaxHighlightMemo.KeyUp(var Key: Word;
  Shift: TShiftState);
begin

end;

procedure TDummySyntaxHighlightMemo.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  p:TPoint;
  c:TCharClass;
begin
  p:=ClientToText(Point(X,Y));
  c:=PosClass(p);
  SourceEditorCustomizeDialogForm.ListBox1.ItemIndex:=c;
  SourceEditorCustomizeDialogForm.ListBox1Click(nil);
end;

procedure TDummySyntaxHighlightMemo.MouseMove(Shift: TShiftState; X,
  Y: Integer);
begin

end;

procedure TDummySyntaxHighlightMemo.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin

end;

function TDummySyntaxHighlightMemo.PosClass(p: TPoint): TCharClass;
begin
  Result:=inherited PosClass(p);
end;

procedure TDummySyntaxHighlightMemo.SetHighlightData(Index: Integer;
  const Value: TCharClassHighlight);
begin
  FSource.HighlightData[Index]:=Value;
  if Index<=Integer(hbWarning) then
    inherited;
  Invalidate;
end;

procedure TDummySyntaxHighlightMemo.TokenizeLine(
  const LineClass: TLineClass; const TextBuffer: PChar;
  const TextLength: Integer; CharClass: PCharClassArray);
begin
  FSource.TokenizeLine(LineClass,TextBuffer,TextLength,CharClass);
end;

procedure TDummySyntaxHighlightMemo.TokenizeLineClass(
  const LastLineClass: TLineClass; var NewLineClass: TLineClass;
  const TextBuffer: PChar; const TextLength: Integer);
begin
  FSource.TokenizeLineClass(LastLineClass,NewLineClass,TextBuffer,TextLength);
end;

procedure TDummySyntaxHighlightMemo.WMSetCursor(var Message: TMessage);
begin
  SetCursor(LoadCursor(0,IDC_ARROW));
  Message.Result:=1;
end;

end.
