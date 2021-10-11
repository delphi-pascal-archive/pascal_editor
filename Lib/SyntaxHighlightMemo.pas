unit SyntaxHighlightMemo;

interface

uses
  SysUtils,Windows,Messages,Classes,Controls,Forms,Graphics,SyntaxHighlighter,
  SyntaxHighlightThread,VirtualScrollingWinControl,Math,ClipBrd,
  ShortCutCollection,CompileErrorManager,StdCtrls,Menus,COntnrs,
  TextEditorFooter;

type
  TPrivateKeyProc=procedure(var Key:Word;Shift:TShiftState) of object;

  TPrivateKeyShortCut=class(TKeyShortCut)
  private
    FExecuteProc:TPrivateKeyProc;
  public
    procedure Execute(var Key:Word;Shift:TShiftState);override;
  end;

  TPrivateMouseProc=procedure(X,Y:Integer;Shift:TShiftState) of object;

  TPrivateMouseShortCut=class(TMouseShortCut)
  private
    FExecuteProc:TPrivateMouseProc;
  public
    procedure Execute(X,Y:Integer;Shift:TShiftState);override;
  end;

  TSelKind=(skNone,skLinear,skRect);
  TMultiClickAction=(maNone,maWord,maLine);

  TSelData=packed record
    Kind:TSelKind;
    UpdateCount:Integer;
    Origin:TPoint;
    case Byte of
      0:(X1,Y1,X2,Y2:Integer);
      1:(Rect:TRect);
  end;

  TCaretData=packed record
    Created:Boolean;
    case Byte of
      0:(X,Y:Longint);
      1:(Pos:TPoint);
  end;

  TCustomSyntaxHighlightMemo=class;

  TEditOptions=class(TPersistent)
  private
    FOwner:TCustomSyntaxHighlightMemo;
    FPreserveLineEnd: Boolean;
    FOverwriteBlocks: Boolean;
    FAutoIndent: Boolean;
    FPersistentBlocks: Boolean;
    FHoverTime: Cardinal;
    FUndoLimit: Integer;
    FTabLength: Integer;
    FMaxLineSize: Integer;
    FRightMarginWidth: Integer;
    FGutterMargin: Integer;
    FWriteMode: TEditMode;
    FDoubleClickAction: TMultiClickAction;
    FTripleClickAction: TMultiClickAction;
    FCaretWidth: Integer;
    FRightMarginVisible: Boolean;
    FUseSystemCaretWidth: Boolean;
    FAllowSymbolHints: Boolean;
    FGutterVisible: Boolean;
    FAllowCodeLinks: Boolean;
    FUseSystemCaretHeight: Boolean;
    FCaretHeight: Integer;
    FAutoSaveOptionsFile: string;
    FShowSpace: Boolean;
    FShowTab: Boolean;

    procedure SetAutoIndent(const Value: Boolean);
    procedure SetGutterMargin(const Value: Integer);
    procedure SetHoverTime(const Value: Cardinal);
    procedure SetMaxLineSize(const Value: Integer);
    procedure SetOverwriteBlocks(const Value: Boolean);
    procedure SetPersistentBlocks(const Value: Boolean);
    procedure SetPreserveLineEnd(const Value: Boolean);
    procedure SetRightMarginWidth(const Value: Integer);
    procedure SetTabLength(const Value: Integer);
    procedure SetUndoLimit(const Value: Integer);
    procedure SetWriteMode(const Value: TEditMode);
    procedure SetDoubleClickAction(const Value: TMultiClickAction);
    procedure SetTripleClickAction(const Value: TMultiClickAction);
    procedure SetCaretWidth(const Value: Integer);
    procedure SetAllowCodeLinks(const Value: Boolean);
    procedure SetAllowSymbolHints(const Value: Boolean);
    procedure SetCaretHeight(const Value: Integer);
    procedure SetGutterVisible(const Value: Boolean);
    procedure SetRightMarginVisible(const Value: Boolean);
    procedure SetUseSystemCaretHeight(const Value: Boolean);
    procedure SetUseSystemCaretWidth(const Value: Boolean);
    procedure SetAutoSaveOptionsFile(const Value: string);
    procedure SetShowSpace(const Value: Boolean);
    procedure SetShowTab(const Value: Boolean);
  protected
    procedure Changed;
  public
    constructor Create(AOwner:TCustomSyntaxHighlightMemo);

    procedure WriteCustomData(Dest:TStream);
    procedure ReadCustomData(Source:TStream);

    procedure Assign(Source:TPersistent);override;

    destructor Destroy;override;
  published
    property WriteMode:TEditMode read FWriteMode write SetWriteMode default emInsert;

    property PersistentBlocks:Boolean read FPersistentBlocks write SetPersistentBlocks default False;
    property OverwriteBlocks:Boolean read FOverwriteBlocks write SetOverwriteBlocks default True;

    property PreserveLineEnd:Boolean read FPreserveLineEnd write SetPreserveLineEnd default False;

    property AutoIndent:Boolean read FAutoIndent write SetAutoIndent default True;

    property TabLength:Integer read FTabLength write SetTabLength default 4;

    property UndoLimit:Integer read FUndoLimit write SetUndoLimit default $10000;

    property ShowSpace:Boolean read FShowSpace write SetShowSpace default False;
    property ShowTab:Boolean read FShowTab write SetShowTab default False;

    property GutterMargin:Integer read FGutterMargin write SetGutterMargin default 30;
    property GutterVisible:Boolean read FGutterVisible write SetGutterVisible default True;
    property RightMarginWidth:Integer read FRightMarginWidth write SetRightMarginWidth default 70;
    property RightMarginVisible:Boolean read FRightMarginVisible write SetRightMarginVisible default True;

    property CaretWidth:Integer read FCaretWidth write SetCaretWidth default 3;
    property UseSystemCaretWidth:Boolean read FUseSystemCaretWidth write SetUseSystemCaretWidth default True;
    property CaretHeight:Integer read FCaretHeight write SetCaretHeight default 16;
    property UseSystemCaretHeight:Boolean read FUseSystemCaretHeight write SetUseSystemCaretHeight default True;

    property MaxLineSize:Integer read FMaxLineSize write SetMaxLineSize default $100;

    property HoverTime:Cardinal read FHoverTime write SetHoverTime default 100;

    property AllowSymbolHints:Boolean read FAllowSymbolHints write SetAllowSymbolHints default True;
    property AllowCodeLinks:Boolean read FAllowCodeLinks write SetAllowCodeLinks default True;

    property DoubleClickAction:TMultiClickAction read FDoubleClickAction write SetDoubleClickAction default maWord;
    property TripleClickAction:TMultiClickAction read FTripleClickAction write SetTripleClickAction default maLine;

    property AutoSaveOptionsFile:string read FAutoSaveOptionsFile write SetAutoSaveOptionsFile;
  end;

  TSelHistoryData=record
    SelStart,SelEnd,CaretPos:TPoint;
    SelKind:TSelKind;
    SavePos:Boolean;
  end;

  THistoryActionMode=(hmInsert,hmDelete);
  THistoryAction=record
    Mode:THistoryActionMode;
    Pos:TPoint;
    Text:string;
    Length:Integer;
  end;

  TEditHistoryItem=class
  private
    FSelBefore,FSelAfter:TSelHistoryData;
    FActions:array of THistoryAction;
  public
    constructor Create;

    procedure Undo(m:TCustomSyntaxHighlightMemo);
    procedure Redo(m:TCustomSyntaxHighlightMemo);

    procedure AddAction(AMode:THistoryActionMode;APos:TPoint;AText:string);

    destructor Destroy;override;
  end;

  TEditHistory=class(TPersistent)
  private
    FOwner:TCustomSyntaxHighlightMemo;
    FHistoryID:Integer;
    FHistory:TObjectList;
    FUpdating:Boolean;
    FGroupLevel:Cardinal;
    FCurrentItem:TEditHistoryItem;
  protected
  public
    constructor Create(AOwner:TCustomSyntaxHighlightMemo);

    procedure Undo;
    procedure Redo;
    procedure Clear;

    procedure BeginGroup;
    procedure EndGroup;
    procedure FlushGroup;

    procedure ClearModification;

    procedure AddInsertItem(Pos:TPoint;Text:string);
    procedure AddDeleteItem(Pos:TPoint;Text:string);

    destructor Destroy;override;
  end;

  TShortCutData=record
    PrivateKeyShortCuts,PublicKeyShortCuts:TKeyShortCutCollection;
    PrivateMouseDownShortCuts,PrivateMouseMoveShortCuts,PublicMouseDownShortCuts,PublicMouseMoveShortCuts:TMouseShortCutCollection;
  end;

  TTrackingData=record
    TrackMove:Boolean;
    DraggingText,JumpHighlight:Boolean;
    p1,p2,LastClickPos:TPoint;
    LastClickTick:Cardinal;
    ClickLevel:Cardinal;
  end;

  THighlightBlockType=(hbWhiteSpace,hbSelection,hbRightMargin,hbSymbolJump,hbSearch,hbCompileError,hbExecuteError,hbWarning);

  THighlightData=record
    BlockHighlights:array[hbWhiteSpace..hbWarning] of TCharClassHighlight;
    BlockPositions:array[hbSearch..hbWarning] of TPoint;
  end;

  TPopupMenuData=record
    PopupMenu:TPopupMenu;
    SymbolJumpMenuItem,UndoMenuItem,RedoMenuItem,CopyMenuItem,CutMenuItem,PasteMenuItem:TMenuItem;
  end;

  TCustomSyntaxHighlightMemo=class(TVirtualScrollingWinControl,IHighlightControl,ISourceEditor,ITextEditor)
  private
    FCharSize:TSize;
    FOnEditOptionsChanged: TNotifyEvent;
    FCanvas:TControlCanvas;
    FLines: TStringList;
    FCustomText:TStringList;
    FThread:TSyntaxHighlightThread;
    FSelData:TSelData;
    FCaretData:TCaretData;
    FShortCutData:TShortCutData;
    FTrackingData:TTrackingData;
    FHighlightData:THighlightData;
    FPopupMenuData:TPopupMenuData;

    FEditOptions:TEditOptions;
    FEditHistory:TEditHistory;
    FEditStateListeners:TComponentList;
    FOnEditStateChanged: TNotifyEvent;
    FModified: Boolean;

    function GetCanvas: TCanvas;
    procedure UpdateCaret;

    function GetText: string;
    procedure SetText(const Value: string);
    function GetLines: TStrings;
    procedure SetLines(const Value: TStrings);
    procedure SetSelKind(const Value: TSelKind);

    procedure WMPaint(var Message:TWMPaint);message WM_PAINT;
    procedure WMSetFocus(var Message:TMessage);message WM_SETFOCUS;
    procedure WMKillFocus(var Message:TMessage);message WM_KILLFOCUS;
    procedure WMDestroy(var Message:TMessage);message WM_DESTROY;
    procedure WMGetDLGCode(var Message:TMessage);message WM_GETDLGCODE;
    procedure WMMouseHover(var Message:TMessage);message WM_MOUSEHOVER;
    procedure WMSetCursor(var Message:TMessage);message WM_SETCURSOR;

    procedure CMFontChanged(var Message:TMessage);message CM_FONTCHANGED;

    procedure LaunchTracking;

    procedure BaseMenuItemClick(Sender:TObject);
    procedure BuildBaseMenu;

    function GetSelRect: TRect;
    function GetSelText: string;
    function GetSelEmpty: Boolean;
    procedure SetCaretPos(const Value: TPoint);
    function GetSelKind: TSelKind;
    function GetSelLength: TSize;
    function GetSelStart: TPoint;
    function GetSelEnd: TPoint;
    function GetSelOrigin: TPoint;
    procedure SetSelOrigin(const Value: TPoint);
    function GetHighlightClassHasText(Index: Integer): Boolean;
    procedure SetEditOptions(const Value: TEditOptions);
    function GetCustomText: TStrings;
    procedure SetCustomText(const Value: TStrings);
    function GetKeyShortCuts: TKeyShortCutCollection;
    procedure SetKeyShortCuts(const Value: TKeyShortCutCollection);
    function GetMouseDownShortCuts: TMouseShortCutCollection;
    function GetMouseMoveShortCuts: TMouseShortCutCollection;
    procedure SetMouseDownShortCuts(const Value: TMouseShortCutCollection);
    procedure SetMouseMoveShortCuts(const Value: TMouseShortCutCollection);
    procedure SetOnEditOptionsChanged(const Value: TNotifyEvent);
    procedure SetOnEditStateChanged(const Value: TNotifyEvent);
    procedure SetModified(const Value: Boolean);
  protected
    property _Thread:TSyntaxHighlightThread read FThread;
    property _HighlightData:THighlightData read FHighlightData write FHighlightData;
    property _TrackingData:TTrackingData read FTrackingData write FTrackingData;

    procedure RegisterEditStateListener(Listener:IEditStateListener);
    procedure UnregisterEditStateListener(Listener:IEditStateListener);
    function GetCaretPos:TPoint;
    function GetEditMode:TEditMode;
    function GetModified:Boolean;
    procedure EditStateChanged;

    function GetHighlightClassCount:Integer;virtual;
    function GetHighlightClassName(Index:Integer):string;virtual;
    function GetHighlightData(Index:Integer):TCharClassHighlight;virtual;
    procedure SetHighlightData(Index:Integer;const Value:TCharClassHighlight);virtual;
    function PosHighlight(p:TPoint):TCharClassHighlight;virtual;
    function PosClass(p:TPoint):TCharClass;virtual;

    function CharOffsetToPos(CharOffset:Integer):TPoint;
    procedure MarkBlock(BlockType:THighlightBlockType;CharOffset,CharLength:Integer);
    procedure MarkCompileError(CharOffset,CharLength:Integer);
    procedure MarkExecuteError(CharOffset,CharLength:Integer);
    procedure MarkWarning(CharOffset,CharLength:Integer);

    function GetCode:string;

    function GetComponent:TComponent;

    procedure PaintWindow(DC:HDC);override;
    procedure PaintGutter;virtual;

    procedure UpdateScrollBars;override;
    procedure ScrollPosChanged;override;
    procedure CreateHandle;override;

    procedure InvalidateLine(Index:Integer);
    procedure InvalidateLines(Index1,Index2:Integer);
    procedure InvalidateSelection;
    procedure ContentModified;virtual;

    function TextToClient(p:TPoint):TPoint;
    function ClientToText(p:TPoint):TPoint;
    function TextClientOrigin:TPoint;
    function VisibleTextSize:TSize;
    function VisibleTextRect:TRect;
    function VisibleTextOrigin:TPoint;
    function TotalTextSize:TSize;
    function CharSize:TSize;
    function IsInSelection(p:TPoint):Boolean;

    procedure MouseDown(Button:TMouseButton;Shift:TShiftState;X,Y:Integer);override;
    procedure MouseMove(Shift:TShiftState;X,Y:Integer);override;
    procedure MouseUp(Button:TMouseButton;Shift:TShiftState;X,Y:Integer);override;
    procedure KeyDown(var Key:Word;Shift:TShiftState);override;
    procedure KeyUp(var Key:Word;Shift:TShiftState);override;
    procedure KeyPress(var Key:Char);override;
    function DoMouseWheelDown(Shift:TShiftState;MousePos:TPoint):Boolean;override;
    function DoMouseWheelUp(Shift:TShiftState;MousePos:TPoint):Boolean;override;
    procedure DblClick;override;
    procedure MultiClickAction(Action:TMultiClickAction);

    procedure DoContextPopup(MousePos:TPoint;var Handled: Boolean);override;

    procedure RegisterPrivateShortCuts;virtual;
    procedure RegisterPrivateKeyShortCut(AProc:TPrivateKeyProc;AKey:Word;AShiftState:TShiftState);overload;
    procedure RegisterPrivateKeyShortCut(AProc:TPrivateKeyProc;Keys:array of Word;ShiftStates:array of TShiftState);overload;
    procedure RegisterPrivateMouseDownShortCut(AProc:TPrivateMouseProc;AShiftState:TShiftState);
    procedure RegisterPrivateMouseMoveShortCut(AProc:TPrivateMouseProc;AShiftState:TShiftState);

    procedure _SCKArrow(var Key:Word;Shift:TShiftState);
    procedure _SCKDelete(var Key:Word;Shift:TShiftState);
    procedure _SCKClipBoard(var Key:Word;Shift:TShiftState);
    procedure _SCKExtremi(var Key:Word;Shift:TShiftState);
    procedure _SCKScroll(var Key:Word;Shift:TShiftState);
    procedure _SCKTab(var Key:Word;Shift:TShiftState);
    procedure _SCKEnter(var Key:Word;Shift:TShiftState);
    procedure _SCKEscape(var Key:Word;Shift:TShiftState);
    procedure _SCKCtrl(var Key:Word;Shift:TShiftState);
    procedure _SCKMenu(var Key:Word;Shift:TShiftState);
    procedure _SCKInsert(var Key:Word;Shift:TShiftState);

    procedure _SCMM(X,Y:Integer;Shift:TShiftState);
    procedure _SCMD(X,Y:Integer;Shift:TShiftState);
    procedure _SCMDJump(X,Y:Integer;Shift:TShiftState);
    procedure _SCMMJump(X,Y:Integer;Shift:TShiftState);
  public
    constructor Create(AOwner:TComponent);override;
    procedure AfterConstruction;override;

    function CharClassHighlight(ClassID:TCharClass):TCharClassHighlight;virtual;abstract;
    function CharClassCanJump(ClassID:TCharClass):Boolean;virtual;
    procedure CharClassJump(ClassID:TCharClass;Text:string;Pos:TPoint);virtual;abstract;
    function CanJumpAtPos(p:TPoint):Boolean;
    function CanJumpAtCaret:Boolean;
    procedure JumpAtPos(p:TPoint);
    procedure JumpAtCaret;
    function CharClassCanHint(ClassID:TCharClass):Boolean;virtual;
    function CharClassHint(ClassID:TCharClass;Text:string;Pos:TPoint):string;virtual;abstract;
    function CharClassText(Pos:TPoint):string;overload;
    procedure CharClassText(Pos:TPoint;var p1,p2:TPoint);overload;

    procedure TokenizeLineClass(const LastLineClass:TLineClass;var NewLineClass:TLineClass;const TextBuffer:PChar;const TextLength:Integer);virtual;abstract;
    procedure TokenizeLine(const LineClass:TLineClass;const TextBuffer:PChar;const TextLength:Integer;CharClass:PCharClassArray);virtual;abstract;

    procedure Customize;

    property Canvas:TCanvas read GetCanvas;

    property Text:string read GetText write SetText;

    procedure ClearMarks;virtual;

    property HighlightClassCount:Integer read GetHighlightClassCount;
    property HighlightClassName[Index:Integer]:string read GetHighlightClassName;
    property HighlightClassHasText[Index:Integer]:Boolean read GetHighlightClassHasText;
    property HighlightData[Index:Integer]:TCharClassHighlight read GetHighlightData write SetHighlightData;

    property SelStart:TPoint read GetSelStart;
    property SelEnd:TPoint read GetSelEnd;
    property SelLength:TSize read GetSelLength;
    property SelKind:TSelKind read GetSelKind write SetSelKind;
    property SelRect:TRect read GetSelRect;
    property SelText:string read GetSelText;
    property SelEmpty:Boolean read GetSelEmpty;
    property SelOrigin:TPoint read GetSelOrigin write SetSelOrigin;
    procedure Select(PStart,PEnd:TPoint);overload;
    procedure Select(r:TRect);overload;
    procedure SelectAll;

    property CaretPos:TPoint read GetCaretPos write SetCaretPos;

    procedure ClearSelection;
    procedure DeleteSelection;

    procedure AddTextAtCaret(s:string);
    procedure AddTextAtPos(var p:TPoint;s:string);
    procedure DeleteTextAtCaret(Offset:Integer);
    procedure DeleteTextAtPos(var p:TPoint;Offset:Integer);
    procedure ScrollToCaret;
    function TextJumpPos(p:TPoint;ToRight:Boolean):TPoint;
    function TabJumpPos(p:TPoint):TPoint;
    function IndentJumpPos(p:TPoint):TPoint;
    procedure FormatTextLine(LineId:Integer);

    property Modified:Boolean read FModified write SetModified;
    procedure MarkModified;
    procedure MarkUnmodified;

    procedure Copy;
    procedure Cut;
    procedure Paste;

    procedure Undo;
    procedure Redo;
    function CanUndo:Boolean;
    function CanRedo:Boolean;
    procedure ClearUndoHistory;

    property Font;

    destructor Destroy;override;
  published
    property OnEditOptionsChanged:TNotifyEvent read FOnEditOptionsChanged write SetOnEditOptionsChanged;
    property OnEditStateChanged:TNotifyEvent read FOnEditStateChanged write SetOnEditStateChanged;

    property Lines:TStrings read GetLines write SetLines;
    property CustomText:TStrings read GetCustomText write SetCustomText;

    property KeyShortCuts:TKeyShortCutCollection read GetKeyShortCuts write SetKeyShortCuts;
    property MouseDownShortCuts:TMouseShortCutCollection read GetMouseDownShortCuts write SetMouseDownShortCuts;
    property MouseMoveShortCuts:TMouseShortCutCollection read GetMouseMoveShortCuts write SetMouseMoveShortCuts;

    property EditOptions:TEditOptions read FEditOptions write SetEditOptions;

    property Cursor default crIBeam;

    property DoubleBuffered default True;

    property BevelKind;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;

    property Align;
    property Anchors;
    property Constraints;
    property Enabled;
    property PopupMenu;
    property TabOrder;
    property Visible;
  end;

  TSyntaxHighlightMemo=class(TCustomSyntaxHighlightMemo)
  private
    FHighlighter:TCustomSyntaxHighlighter;
    FOnChange: TNotifyEvent;
    procedure SetHighlighter(const Value: TCustomSyntaxHighlighter);
    procedure SetOnChange(const Value: TNotifyEvent);
  protected
    function GetHighlightClassCount:Integer;override;
    function GetHighlightClassName(Index:Integer):string;override;
    function GetHighlightData(Index:Integer):TCharClassHighlight;override;
    procedure SetHighlightData(Index:Integer;const Value:TCharClassHighlight);override;

    procedure ContentModified;override;
  public
    procedure Notification(AComponent:TComponent;Operation:TOperation);override;

    function CharClassHighlight(ClassID:TCharClass):TCharClassHighlight;override;
    function CharClassCanJump(ClassID:TCharClass):Boolean;override;
    procedure CharClassJump(ClassID:TCharClass;Text:string;Pos:TPoint);override;
    function CharClassCanHint(ClassID:TCharClass):Boolean;override;
    function CharClassHint(ClassID:TCharClass;Text:string;Pos:TPoint):string;override;

    procedure TokenizeLineClass(const LastLineClass:TLineClass;var NewLineClass:TLineClass;const TextBuffer:PChar;const TextLength:Integer);override;
    procedure TokenizeLine(const LineClass:TLineClass;const TextBuffer:PChar;const TextLength:Integer;CharClass:PCharClassArray);override;

    destructor Destroy;override;
  published
    property OnChange:TNotifyEvent read FOnChange write SetOnChange;

    property Highlighter:TCustomSyntaxHighlighter read FHighlighter write SetHighlighter;
 end;

implementation

uses Types, StrUtils, SourceEditorCustomizeDialogUnit;

function Size(cx,cy:Integer):TSize;
begin
  Result.cx:=cx;
  Result.cy:=cy;
end;

function IsBlank(s:string):Boolean;
var
  a:Integer;
begin
  Result:=True;
  for a:=1 to Length(s) do
    Result:=Result and (s[a]=' ');
end;

{ TPrivateKeyShortCut }

procedure TPrivateKeyShortCut.Execute(var Key: Word; Shift: TShiftState);
begin
  FExecuteProc(Key,Shift);
end;

{ TPrivateMouseShortCut }

procedure TPrivateMouseShortCut.Execute(X, Y: Integer; Shift: TShiftState);
begin
  FExecuteProc(X,Y,Shift);
end;

{ TEditOptions }

procedure TEditOptions.Assign(Source: TPersistent);
begin
  if Source is TEditOptions then
    with TEditOptions(Source) do begin
      Self.PreserveLineEnd:=FPreserveLineEnd;
      Self.OverwriteBlocks:=FOverwriteBlocks;
      Self.AutoIndent:=FAutoIndent;
      Self.PersistentBlocks:=FPersistentBlocks;
      Self.HoverTime:=FHoverTime;
      Self.UndoLimit:=FUndoLimit;
      Self.ShowSpace:=FShowSpace;
      Self.ShowTab:=FShowTab;
      Self.TabLength:=FTabLength;
      Self.MaxLineSize:=FMaxLineSize;
      Self.RightMarginWidth:=FRightMarginWidth;
      Self.GutterMargin:=FGutterMargin;
      Self.WriteMode:=FWriteMode;
      Self.DoubleClickAction:=FDoubleClickAction;
      Self.TripleClickAction:=FTripleClickAction;
      Self.CaretWidth:=FCaretWidth;
      Self.RightMarginVisible:=FRightMarginVisible;
      Self.UseSystemCaretWidth:=FUseSystemCaretWidth;
      Self.AllowSymbolHints:=FAllowSymbolHints;
      Self.GutterVisible:=FGutterVisible;
      Self.AllowCodeLinks:=FAllowCodeLinks;
      Self.UseSystemCaretHeight:=FUseSystemCaretHeight;
      Self.CaretHeight:=FCaretHeight;
    end
  else
    inherited;
  Changed;
end;

procedure TEditOptions.Changed;
begin
  if Assigned(FOwner.FOnEditOptionsChanged) then
    FOwner.FOnEditOptionsChanged(FOwner);
  FOwner.EditStateChanged;
end;

constructor TEditOptions.Create(AOwner: TCustomSyntaxHighlightMemo);
begin
  inherited Create;
  FOwner:=AOwner;

  FOverwriteBlocks:=True;

  FAutoIndent:=True;

  FTabLength:=4;

  FUndoLimit:=$10000;

  FGutterMargin:=30;
  FGutterVisible:=True;
  FRightMarginWidth:=70;
  FRightMarginVisible:=True;

  FCaretWidth:=3;
  FUseSystemCaretWidth:=True;
  FCaretHeight:=16;
  FUseSystemCaretHeight:=True;

  FMaxLineSize:=$100;

  FHoverTime:=100;

  FDoubleClickAction:=maWord;
  FTripleClickAction:=maLine;

  FAllowSymbolHints:=True;
  FAllowCodeLinks:=True;
end;

destructor TEditOptions.Destroy;
var
  f:TFileStream;
  s:string;
begin
  if (FAutoSaveOptionsFile<>'') {and not (csDesigning in FOwner.ComponentState)} then begin
    SetLength(s,MAX_PATH+1);
    SetLength(s,GetModuleFileName(HInstance,PChar(s),MAX_PATH));
    try
      f:=TFileStream.Create(ExtractFilePath(s)+FAutoSaveOptionsFile,fmCreate or fmOpenWrite);
      WriteCustomData(f);
      f.Destroy;
    except
      Messagebox(0,'Cannot save editor options','error',MB_ICONERROR or MB_OK);
    end;
  end;
  inherited;
end;

procedure TEditOptions.ReadCustomData(Source: TStream);
type
  PCharClassHighlight=^TCharClassHighlight;
var
  a,b,c:Integer;
  s:string;
  p:PCharClassHighlight;
  l:TStringList;
begin
  Source.Read(FPreserveLineEnd,SizeOf(FPreserveLineEnd));
  Source.Read(FOverwriteBlocks,SizeOf(FOverwriteBlocks));
  Source.Read(FAutoIndent,SizeOf(FAutoIndent));
  Source.Read(FPersistentBlocks,SizeOf(FPersistentBlocks));
  Source.Read(FHoverTime,SizeOf(FHoverTime));
  Source.Read(FUndoLimit,SizeOf(FUndoLimit));
  Source.Read(FShowSpace,SizeOf(FShowSpace));
  Source.Read(FShowTab,SizeOf(FShowTab));
  Source.Read(FTabLength,SizeOf(FTabLength));
  Source.Read(FMaxLineSize,SizeOf(FMaxLineSize));
  Source.Read(FRightMarginWidth,SizeOf(FRightMarginWidth));
  Source.Read(FGutterMargin,SizeOf(FGutterMargin));
  Source.Read(FWriteMode,SizeOf(FWriteMode));
  Source.Read(FDoubleClickAction,SizeOf(FDoubleClickAction));
  Source.Read(FTripleClickAction,SizeOf(FTripleClickAction));
  Source.Read(FCaretWidth,SizeOf(FCaretWidth));
  Source.Read(FRightMarginVisible,SizeOf(FRightMarginVisible));
  Source.Read(FUseSystemCaretWidth,SizeOf(FUseSystemCaretWidth));
  Source.Read(FAllowSymbolHints,SizeOf(FAllowSymbolHints));
  Source.Read(FGutterVisible,SizeOf(FGutterVisible));
  Source.Read(FAllowCodeLinks,SizeOf(FAllowCodeLinks));
  Source.Read(FUseSystemCaretHeight,SizeOf(FUseSystemCaretHeight));
  Source.Read(FCaretHeight,SizeOf(FCaretHeight));
  Source.Read(a,SizeOf(a));
  FOwner.Font.Size:=a;
  Source.Read(a,SizeOf(a));
  SetLength(s,a);
  Source.Read(s[1],a);
  FOwner.Font.Name:=s;
  l:=TStringList.Create;
  Source.Read(b,SizeOf(b));
  for a:=0 to b-1 do begin
    Source.Read(c,SizeOf(c));
    SetLength(s,c);
    Source.Read(s[1],c);
    New(p);
    Source.Read(p^,SizeOf(p^));
    l.AddObject(s,TObject(p))
  end;
  for a:=0 to FOwner.HighlightClassCount-1 do begin
    b:=l.IndexOf(FOwner.HighlightClassName[a]);
    if b>-1 then
      FOwner.HighlightData[a]:=PCharClassHighlight(l.Objects[b])^;
  end;
  for a:=0 to l.Count-1 do
    Dispose(PCharClassHighlight(l.Objects[a]));
  l.Destroy;
  UndoLimit:=UndoLimit;
  CaretWidth:=CaretWidth;
  GutterMargin:=GutterMargin;
  Changed;
end;

procedure TEditOptions.SetAllowCodeLinks(const Value: Boolean);
begin
  FAllowCodeLinks := Value;
  Changed;
end;

procedure TEditOptions.SetAllowSymbolHints(const Value: Boolean);
begin
  FAllowSymbolHints := Value;
  Changed;
end;

procedure TEditOptions.SetAutoIndent(const Value: Boolean);
begin
  FAutoIndent := Value;
  Changed;
end;

procedure TEditOptions.SetAutoSaveOptionsFile(const Value: string);
var
  f:TFileStream;
  s:string;
begin
  FAutoSaveOptionsFile := Value;
  if (FAutoSaveOptionsFile<>'') {and not (csDesigning in FOwner.ComponentState)} then begin
    SetLength(s,MAX_PATH+1);
    SetLength(s,GetModuleFileName(HInstance,PChar(s),MAX_PATH));
    if not FileExists(ExtractFilePath(s)+FAutoSaveOptionsFile) then
      Exit;
    try
      f:=TFileStream.Create(ExtractFilePath(s)+FAutoSaveOptionsFile,fmOpenRead);
      f.Position:=0;
      ReadCustomData(f);
      f.Destroy;
    except
      Messagebox(0,'Cannot load editor options','error',MB_ICONERROR or MB_OK);
    end;
  end;
end;

procedure TEditOptions.SetCaretHeight(const Value: Integer);
begin
  FCaretHeight := Value;
  if FCaretHeight<0 then
    FCaretHeight:=0;
  FOwner.FCaretData.Created:=False;
  FOwner.UpdateCaret;
  Changed;
end;

procedure TEditOptions.SetCaretWidth(const Value: Integer);
begin
  FCaretWidth := Value;
  if FCaretWidth<0 then
    FCaretWidth:=0;
  FOwner.FCaretData.Created:=False;
  FOwner.UpdateCaret;
  Changed;
end;

procedure TEditOptions.SetDoubleClickAction(
  const Value: TMultiClickAction);
begin
  FDoubleClickAction := Value;
  Changed;
end;

procedure TEditOptions.SetGutterMargin(const Value: Integer);
begin
  FGutterMargin := Value;
  if FGutterMargin<0 then
    FGutterMargin:=0;
  FOwner.UpdateCaret;
  FOwner.Invalidate;
  Changed;
end;

procedure TEditOptions.SetGutterVisible(const Value: Boolean);
begin
  FGutterVisible := Value;
  FOwner.UpdateCaret;
  FOwner.Invalidate;
  Changed;
end;

procedure TEditOptions.SetHoverTime(const Value: Cardinal);
begin
  FHoverTime := Value;
  if FHoverTime<=0 then
    FHoverTime:=1;
  Changed;
end;

procedure TEditOptions.SetMaxLineSize(const Value: Integer);
var
  a:Integer;
begin
  FMaxLineSize := Value;
  if FMaxLineSize<=1 then
    FHoverTime:=1;
  FOwner.Lines.BeginUpdate;
  try
    for a:=0 to FOwner.FLines.Count-1 do
      FOwner.FLines[a]:=Copy(FOwner.FLines[a],1,FMaxLineSize);
  finally
    FOwner.Lines.EndUpdate;
  end;
  FOwner.UpdateScrollBars;
  Changed;
  //TODO
end;

procedure TEditOptions.SetOverwriteBlocks(const Value: Boolean);
begin
  FOverwriteBlocks := Value;
  Changed;
end;

procedure TEditOptions.SetPersistentBlocks(const Value: Boolean);
begin
  FPersistentBlocks := Value;
  Changed;
end;

procedure TEditOptions.SetPreserveLineEnd(const Value: Boolean);
begin
  FPreserveLineEnd := Value;
  Changed;
end;

procedure TEditOptions.SetRightMarginVisible(const Value: Boolean);
begin
  FRightMarginVisible := Value;
  FOwner.Invalidate;
  Changed;
end;

procedure TEditOptions.SetRightMarginWidth(const Value: Integer);
begin
  FRightMarginWidth := Value;
  if FRightMarginWidth<0 then
    FHoverTime:=0;
  FOwner.Invalidate;
  Changed;
end;

procedure TEditOptions.SetShowSpace(const Value: Boolean);
begin
  FShowSpace := Value;
  FOwner.Invalidate;
  Changed;
end;

procedure TEditOptions.SetShowTab(const Value: Boolean);
begin
  FShowTab := Value;
  FOwner.Invalidate;
  Changed;
end;

procedure TEditOptions.SetTabLength(const Value: Integer);
begin
  FTabLength := Value;
  if FTabLength<=0 then
    FHoverTime:=1;
  FOwner.Invalidate;
  Changed;
end;

procedure TEditOptions.SetTripleClickAction(
  const Value: TMultiClickAction);
begin
  FTripleClickAction := Value;
  Changed;
end;

procedure TEditOptions.SetUndoLimit(const Value: Integer);
begin
  FUndoLimit := Value;
  if FUndoLimit<=0 then
    FUndoLimit:=1;
  //TODO
  Changed;
end;

procedure TEditOptions.SetUseSystemCaretHeight(const Value: Boolean);
begin
  FUseSystemCaretHeight := Value;
  FOwner.FCaretData.Created:=False;
  FOwner.UpdateCaret;
  Changed;
end;

procedure TEditOptions.SetUseSystemCaretWidth(const Value: Boolean);
begin
  FUseSystemCaretWidth := Value;
  FOwner.FCaretData.Created:=False;
  FOwner.UpdateCaret;
  Changed;
end;

procedure TEditOptions.SetWriteMode(const Value: TEditMode);
begin
  FWriteMode := Value;
  Changed;
end;

procedure TEditOptions.WriteCustomData(Dest: TStream);
var
  a,b:Integer;
  s:string;
  h:TCharClassHighlight;
begin
  Dest.Write(FPreserveLineEnd,SizeOf(FPreserveLineEnd));
  Dest.Write(FOverwriteBlocks,SizeOf(FOverwriteBlocks));
  Dest.Write(FAutoIndent,SizeOf(FAutoIndent));
  Dest.Write(FPersistentBlocks,SizeOf(FPersistentBlocks));
  Dest.Write(FHoverTime,SizeOf(FHoverTime));
  Dest.Write(FUndoLimit,SizeOf(FUndoLimit));
  Dest.Write(FShowSpace,SizeOf(FShowSpace));
  Dest.Write(FShowTab,SizeOf(FShowTab));
  Dest.Write(FTabLength,SizeOf(FTabLength));
  Dest.Write(FMaxLineSize,SizeOf(FMaxLineSize));
  Dest.Write(FRightMarginWidth,SizeOf(FRightMarginWidth));
  Dest.Write(FGutterMargin,SizeOf(FGutterMargin));
  Dest.Write(FWriteMode,SizeOf(FWriteMode));
  Dest.Write(FDoubleClickAction,SizeOf(FDoubleClickAction));
  Dest.Write(FTripleClickAction,SizeOf(FTripleClickAction));
  Dest.Write(FCaretWidth,SizeOf(FCaretWidth));
  Dest.Write(FRightMarginVisible,SizeOf(FRightMarginVisible));
  Dest.Write(FUseSystemCaretWidth,SizeOf(FUseSystemCaretWidth));
  Dest.Write(FAllowSymbolHints,SizeOf(FAllowSymbolHints));
  Dest.Write(FGutterVisible,SizeOf(FGutterVisible));
  Dest.Write(FAllowCodeLinks,SizeOf(FAllowCodeLinks));
  Dest.Write(FUseSystemCaretHeight,SizeOf(FUseSystemCaretHeight));
  Dest.Write(FCaretHeight,SizeOf(FCaretHeight));
  a:=FOwner.Font.Size;
  Dest.Write(a,SizeOf(a));
  s:=FOwner.Font.Name;
  a:=Length(s);
  Dest.Write(a,SizeOf(a));
  Dest.Write(s[1],a);
  a:=FOwner.HighlightClassCount;
  Dest.Write(a,SizeOf(a));
  for a:=0 to FOwner.HighlightClassCount-1 do begin
    s:=FOwner.HighlightClassName[a];
    b:=Length(s);
    Dest.Write(b,SizeOf(b));
    Dest.Write(s[1],b);
    h:=FOwner.HighlightData[a];
    Dest.Write(h,SizeOf(h));
  end;
end;

{ TEditHistoryItem }

procedure TEditHistoryItem.AddAction(AMode: THistoryActionMode;
  APos: TPoint; AText: string);
begin
  SetLength(FActions,High(FActions)+2);
  with FActions[High(FActions)] do begin
    Mode:=AMode;
    Pos:=APos;
    Text:=AText;
    Length:=System.Length(Text);
  end;
end;

constructor TEditHistoryItem.Create;
begin
  inherited;
end;

destructor TEditHistoryItem.Destroy;
begin
  SetLength(FActions,0);
  inherited;
end;

procedure TEditHistoryItem.Redo(m: TCustomSyntaxHighlightMemo);
var
  a:Integer;
  w:TEditMode;
  p:TPoint;
begin
  w:=m.EditOptions.FWriteMode;
  m.EditOptions.FWriteMode:=emInsert;
  try
    for a:=0 to High(FActions) do
      with FActions[a] do begin
        p:=Pos;
        case Mode of
          hmInsert:m.AddTextAtPos(p,Text);
          hmDelete:m.DeleteTextAtPos(p,Length);
        end;
      end;
    with FSelAfter do begin
      m.Select(SelStart,SelEnd);
      m.SelKind:=SelKind;
      m.CaretPos:=CaretPos;
      if SavePos then
        m.MarkUnmodified;
    end;
  finally
    m.EditOptions.WriteMode:=w;
  end;
end;

procedure TEditHistoryItem.Undo(m: TCustomSyntaxHighlightMemo);
var
  a:Integer;
  w:TEditMode;
  p:TPoint;
begin
  w:=m.EditOptions.FWriteMode;
  m.EditOptions.FWriteMode:=emInsert;
  try
    for a:=High(FActions) downto 0 do
      with FActions[a] do begin
        p:=Pos;
        case Mode of
          hmInsert:m.DeleteTextAtPos(p,Length);
          hmDelete:m.AddTextAtPos(p,Text);
        end;
      end;
    with FSelBefore do begin
      m.Select(SelStart,SelEnd);
      m.SelKind:=SelKind;
      m.CaretPos:=CaretPos;
      if SavePos then
        m.MarkUnmodified;
    end;
  finally
    m.EditOptions.WriteMode:=w;
  end;
end;

{ TEditHistory }

procedure TEditHistory.AddDeleteItem(Pos: TPoint; Text: string);
begin
  if FUpdating then
    Exit;
  BeginGroup;
  try
    FCurrentItem.AddAction(hmDelete,Pos,Text);
  finally
    EndGroup;
  end;
end;

procedure TEditHistory.AddInsertItem(Pos: TPoint; Text: string);
begin
  if FUpdating then
    Exit;
  BeginGroup;
  try
    FCurrentItem.AddAction(hmInsert,Pos,Text);
  finally
    EndGroup;
  end;
end;

procedure TEditHistory.BeginGroup;
begin
  if FGroupLevel=0 then begin
    while (FHistory.Count>FHistoryID+1) and (FHistory.Count>0) do
      FHistory.Delete(FHistory.Count-1);
    FCurrentItem:=TEditHistoryItem.Create;
    with FCurrentItem.FSelBefore do begin
      SavePos:=(FHistory.Count=0) or ((FHistoryID>0) and (FHistoryID<FHistory.Count) and TEditHistoryItem(FHistory[FHistoryID-1]).FSelAfter.SavePos);
      SelStart:=FOwner.SelStart;
      SelEnd:=FOwner.SelEnd;
      CaretPos:=FOwner.CaretPos;
      SelKind:=FOwner.SelKind;
    end;
  end;
  Inc(FGroupLevel);
end;

procedure TEditHistory.Clear;
begin
  FHistory.Clear;
  FHistoryID:=-1;
end;

procedure TEditHistory.ClearModification;
var
  a:Integer;
begin
  if FUpdating then
    Exit;
  for a:=0 to FHistory.Count-1 do
    with TEditHistoryItem(FHistory[a]) do begin
      FSelBefore.SavePos:=False;
      FSelAfter.SavePos:=False;
    end;
  if (FHistoryID>-1) and (FHistoryID<=FHistory.Count-1) then
    TEditHistoryItem(FHistory[FHistoryID]).FSelAfter.SavePos:=True;
  if (FHistoryID<FHistory.Count-1) and (FHistoryID>=-1) then
    TEditHistoryItem(FHistory[FHistoryID+1]).FSelBefore.SavePos:=True;
end;

constructor TEditHistory.Create(AOwner: TCustomSyntaxHighlightMemo);
begin
  inherited Create;
  FOwner:=AOwner;
  FHistoryID:=-1;
  FHistory:=TObjectList.Create(True);
end;

destructor TEditHistory.Destroy;
begin
  FHistory.Destroy;
  inherited;
end;

procedure TEditHistory.EndGroup;
begin
  Assert(FGroupLevel>0,'Negative lock index');
  Dec(FGroupLevel);
  if (FGroupLevel=0) {and (High(FCurrentItem.FActions)>-1)} then begin
    with FCurrentItem.FSelAfter do begin
      SelStart:=FOwner.SelStart;
      SelEnd:=FOwner.SelEnd;
      CaretPos:=FOwner.CaretPos;
      SelKind:=FOwner.SelKind;
    end;
    FHistoryID:=FHistory.Add(FCurrentItem);
    FCurrentItem:=nil;
  end;
end;

procedure TEditHistory.FlushGroup;
var
  a:Cardinal;
begin
  a:=FGroupLevel;
  FGroupLevel:=1;
  EndGroup;
  BeginGroup;
  FGroupLevel:=a;
end;

procedure TEditHistory.Redo;
begin
  FUpdating:=True;
  Inc(FGroupLevel);
  FOwner.FLines.BeginUpdate;
  try
    Inc(FHistoryID);
//    if FHistoryID<0 then
//      FHistoryID:=0;
    if FHistoryID<FHistory.Count then begin
      TEditHistoryItem(FHistory[FHistoryID]).Redo(FOwner);
    end;
  finally
    FOwner.FLines.EndUpdate;
    Dec(FGroupLevel);
    FUpdating:=False;
  end;
end;

procedure TEditHistory.Undo;
begin
  FUpdating:=True;
  Inc(FGroupLevel);
  FOwner.FLines.BeginUpdate;
  try
    if FHistoryID>=FHistory.Count-1 then
      FHistoryID:=FHistory.Count-1;
    if FHistoryID>=0 then begin
      TEditHistoryItem(FHistory[FHistoryID]).Undo(FOwner);
      Dec(FHistoryID);
    end;
  finally
    FOwner.FLines.EndUpdate;
    Dec(FGroupLevel);
    FUpdating:=False;
  end;
end;

{ TCustomSyntaxHighlightMemo }

procedure TCustomSyntaxHighlightMemo.AddTextAtCaret(s: string);
var
  p:TPoint;
begin
  if s='' then
    Exit;
  if not SelEmpty then begin
    if not FEditOptions.PersistentBlocks then begin
      if FEditOptions.OverwriteBlocks then begin
        CaretPos:=SelStart;
        DeleteSelection;
      end else
        ClearSelection;
    end;
  end;
  FEditHistory.BeginGroup;
  p:=CaretPos;
  AddTextAtPos(p,s);
  CaretPos:=p;
  ScrollToCaret;
  FEditHistory.EndGroup;
end;

procedure TCustomSyntaxHighlightMemo.AddTextAtPos(var p: TPoint;
  s: string);
var
  a,b:Integer;
  l:TStringList;
  q:TPoint;

  procedure InsertTextAtLine(LineId:Integer;s:string;Pos:Integer;LineBreak:Boolean=False);
  var
    a,b:Integer;
    t:string;
  begin
    while LineID>=FLines.Count do
      FLines.Add('');
    if FEditOptions.WriteMode=emOverWrite then
      FEditHistory.AddDeleteItem(Point(Pos,LineId),System.Copy(FLines[LineID],Pos+1,Length(s)));
    if LineBreak {and (FEditOptions.WriteMode=emInsert)} then
      FEditHistory.AddInsertItem(Point(Pos,LineId),s+#13)
    else
      FEditHistory.AddInsertItem(Point(Pos,LineId),s);
    Assert(FLines.Count>LineID);
    t:=FLines[LineID];
    b:=Length(t);
    case FEditOptions.WriteMode of
      emInsert:begin
        if Pos>=b then begin
          SetLength(t,Pos);
          for a:=b+1 to Pos do
            t[a]:=' ';
          FLines[LineID]:=t+s;
          if LineBreak then
            FLines.Insert(LineID+1,'');
        end else begin
          if LineBreak then begin
            FLines[LineID]:=System.Copy(t,1,Pos)+s;
            FLines.Insert(LineID+1,System.Copy(t,Pos+1,Length(t)));
            Invalidate;
          end else
            FLines[LineID]:=System.Copy(t,1,Pos)+s+System.Copy(t,Pos+1,Length(t));
        end;
      end;
      emOverwrite:begin
        if Pos>=b then begin
          SetLength(t,Pos);
          for a:=b+1 to Pos do
            t[a]:=' ';
          FLines[LineID]:=t+s;
        end else begin
          if LineBreak then begin
            FLines[LineID]:=System.Copy(t,1,Pos)+s;
            //FLines.Insert(LineID+1,System.Copy(t,Pos+1,Length(t)));
          end else
            FLines[LineID]:=System.Copy(t,1,Pos)+s+System.Copy(t,Pos+1+Length(s),Length(t));
        end;
      end;
    end;
    //FormatTextLine(LineID);
  end;

begin
  if s='' then
    Exit;
  MarkModified;
  if p.X<0 then
    p.X:=0;
  if p.Y<0 then
    p.Y:=0;
  if p.Y>=FLines.Count then
    p.Y:=FLines.Count-1;
  b:=0;
  l:=TStringList.Create;
  for a:=1 to Length(s) do begin
    if s[a] in [#10,#13] then begin
      if (a=1) or (s[a]=#13) or ((s[a]=#10) and not (s[a-1]=#13)) then
        l.Add(System.Copy(s,b+1,a-b-1));
      b:=a;
    end;
  end;
  l.Add(System.Copy(s,b+1,Length(s)-b));
  q:=p;
  FEditHistory.BeginGroup;
  FLines.BeginUpdate;
  try
    case FEditOptions.WriteMode of
      emInsert,emOverwrite:begin
        if l.Count>1 then begin
          InsertTextAtLine(p.Y,l[0],p.X,True);
          for a:=1 to l.Count-2 do 
            InsertTextAtLine(p.Y+a,l[a],0,True);
          InsertTextAtLine(p.Y+l.Count-1,l[l.Count-1],0);
          InvalidateLines(p.Y,FLines.Count);
          p.X:=Length(l[l.Count-1]);
          p.Y:=p.Y+l.Count-1;
        end else begin
          InsertTextAtLine(p.Y,s,p.X);
          InvalidateLine(p.Y);
          Inc(p.X,Length(s));
        end;
      end;
    end;
    if FEditOptions.PersistentBlocks and (SelKind=skLinear) and not SelEmpty then begin
      with SelEnd do
        if IsInSelection(q) then
          Select(SelStart,Point(X+p.X-q.X,Y+p.Y-q.Y))
        else
          if (q.Y<SelRect.Top) or ((q.Y=SelRect.Bottom) and (q.X<X)) then
            Select(Point(SelStart.X+p.X-q.X,SelStart.Y+p.Y-q.Y),Point(X+p.X-q.X,Y+p.Y-q.Y));
    end;
  finally
    FEditHistory.EndGroup;
    FLines.EndUpdate;
    l.Destroy;
  end;
end;

procedure TCustomSyntaxHighlightMemo.AfterConstruction;
begin
  inherited;
  FThread.Resume;
end;

procedure TCustomSyntaxHighlightMemo.BaseMenuItemClick(Sender: TObject);
var
  p:TPoint;
begin
  if Sender=FPopupMenuData.SymbolJumpMenuItem then begin
    p.X:=LoWord(TMenuItem(Sender).Tag);
    p.Y:=HiWord(TMenuItem(Sender).Tag);
    JumpAtPos(p);
    Exit;
  end;
  case TMenuItem(Sender).Tag of
    1:Cut;
    2:Copy;
    3:Paste;
    4:Undo;
    5:Redo;
    6:SelectAll;
    7:Customize;
  end;
end;

procedure TCustomSyntaxHighlightMemo.BuildBaseMenu;
var
  m:TMenuItem;

  procedure MakeItem(Caption:string;OnClick:TNotifyEvent;Tag:Integer;ShortCut:TShortCut);
  begin
    m:=TMenuItem.Create(PopupMenu);
    m.Caption:=Caption;
    m.OnClick:=OnClick;
    m.Tag:=Tag;
    m.AutoHotkeys:=maParent;
    m.ShortCut:=ShortCut;
    FPopupMenuData.PopupMenu.Items.Add(m);
  end;

begin
  with FPopupMenuData do
    if not Assigned(PopupMenu) then begin
      PopupMenu:=TPopupMenu.Create(Self);
      PopupMenu.AutoHotkeys:=maAutomatic;
      MakeItem('Jump to symbol info',BaseMenuItemClick,0,ShortCut(VK_F1,[ssCtrl]));
      SymbolJumpMenuItem:=m;
      MakeItem('-',nil,0,0);
      MakeItem('Cut',BaseMenuItemClick,1,ShortCut(Ord('X'),[ssCtrl]));
      CutMenuItem:=m;
      MakeItem('Copy',BaseMenuItemClick,2,ShortCut(Ord('C'),[ssCtrl]));
      CopyMenuItem:=m;
      MakeItem('Paste',BaseMenuItemClick,3,ShortCut(Ord('V'),[ssCtrl]));
      PasteMenuItem:=m;
      MakeItem('-',nil,0,0);
      MakeItem('Undo',BaseMenuItemClick,4,ShortCut(Ord('Z'),[ssCtrl]));
      UndoMenuItem:=m;
      MakeItem('Redo',BaseMenuItemClick,5,ShortCut(Ord('Y'),[ssCtrl]));
      RedoMenuItem:=m;
      MakeItem('-',nil,0,0);
      MakeItem('Select all',BaseMenuItemClick,6,ShortCut(Ord('A'),[ssCtrl]));
      MakeItem('-',nil,0,0);
      MakeItem('Properties...',BaseMenuItemClick,7,ShortCut(Ord('P'),[ssCtrl]));
    end;
end;

function TCustomSyntaxHighlightMemo.CanRedo: Boolean;
begin
  Result:=(FEditHistory.FHistoryID<FEditHistory.FHistory.Count-1) and (FEditHistory.FHistory.Count>0);
end;

function TCustomSyntaxHighlightMemo.CanUndo: Boolean;
begin
  Result:=(FEditHistory.FHistoryID>=0) and (FEditHistory.FHistory.Count>0);
end;

function TCustomSyntaxHighlightMemo.CharClassCanHint(
  ClassID: TCharClass): Boolean;
begin
  Result:=False;
end;

function TCustomSyntaxHighlightMemo.CharClassCanJump(
  ClassID: TCharClass): Boolean;
begin
  Result:=False;
end;

function TCustomSyntaxHighlightMemo.CharClassText(Pos: TPoint): string;
var
  p1,p2:TPoint;
begin
  CharClassText(Pos,p1,p2);
  Result:=System.Copy(FLines[Pos.Y],p1.X+1,p2.X-p1.X+1);
end;

procedure TCustomSyntaxHighlightMemo.CharClassText(Pos: TPoint; var p1,
  p2: TPoint);
var
  c:TCharClass;
begin
  c:=FThread.CharClass[Pos.Y,Pos.X];
  p1.Y:=Pos.Y;
  p1.X:=Pos.X-1;
  while FThread.CharClass[p1.Y,p1.X]=c do
    Dec(p1.X);
  Inc(p1.X);
  p2.Y:=Pos.Y;
  p2.X:=Pos.X+1;
  while FThread.CharClass[p2.Y,p2.X]=c do
    Inc(p2.X);
  Dec(p2.X);
end;

function TCustomSyntaxHighlightMemo.CharOffsetToPos(
  CharOffset: Integer): TPoint;
begin
  Dec(CharOffset);
  Result.Y:=0;
  Result.X:=0;
  while (Result.Y<FLines.Count) and (CharOffset>Length(FLines[Result.Y])) do begin
    Result.X:=CharOffset;
    Dec(CharOffset,Length(FLines[Result.Y])+2);
    Inc(Result.Y);
  end;
  if Result.Y=FLines.Count then begin
    Result.Y:=FLines.Count-1;
    if Result.X>Length(FLines[Result.Y]) then
      Result.X:=Length(FLines[Result.Y]);
  end else
    Result.X:=CharOffset;
end;

function TCustomSyntaxHighlightMemo.CharSize: TSize;
begin
  Result:=FCharSize;
end;

procedure TCustomSyntaxHighlightMemo.ClearMarks;
var
  t:THighlightBlockType;
begin
  with FHighlightData do begin
    for t:=hbSearch to hbWarning do
      if BlockPositions[t].Y>-1 then begin
        InvalidateLine(BlockPositions[t].Y);
        BlockPositions[t]:=Point(-1,-1);
      end;
  end;
end;

procedure TCustomSyntaxHighlightMemo.ClearSelection;
begin
  if not SelEmpty then begin
    InvalidateSelection;
    SelKind:=skNone;
  end;
end;

function TCustomSyntaxHighlightMemo.ClientToText(p: TPoint): TPoint;
var
  q:TPoint;
begin
  q:=TextClientOrigin;
  with CharSize do begin
    Result.X:=(p.X-q.X) div cx;
    Result.Y:=(p.Y-q.Y) div cy;
  end;
end;

procedure TCustomSyntaxHighlightMemo.CMFontChanged(var Message: TMessage);
var
  c:Char;
begin
  FCharSize.cx:=1;
  FCharSize.cy:=1;
  if Assigned(FCanvas) and HandleAllocated then begin
    FCanvas.Font.Name:=Font.Name;
    FCanvas.Font.Size:=Font.Size;
    for c:=#0 to #255 do
      with Canvas.TextExtent(c) do begin
        if cx>FCharSize.cx then
          FCharSize.cx:=cx;
        if cy>FCharSize.cy then
          FCharSize.cy:=cy;
      end;
    UpdateScrollBars;
    Invalidate;
  end;
  inherited;
end;

procedure TCustomSyntaxHighlightMemo.ContentModified;
begin
  ClearMarks;
  if FLines.Count=0 then begin
    FThread.Lock;
    try
      FLines.Add('');
    finally
      FThread.Unlock;
    end;
  end;
  UpdateScrollState;
end;

procedure TCustomSyntaxHighlightMemo.Copy;
begin
  if not SelEmpty then
    ClipBoard.AsText:=SelText;
end;

constructor TCustomSyntaxHighlightMemo.Create(AOwner: TComponent);
begin
  inherited;
  FEditStateListeners:=TComponentList.Create(False);
  FEditHistory:=TEditHistory.Create(Self);
  ControlStyle:=ControlStyle+[csOpaque,csDoubleClicks];
  DoubleBuffered:=True;
  FCanvas:=TControlCanvas.Create;
  FCanvas.Control:=Self;
  TabStop:=True;
  Font.Name:='Courier new';
  Font.Size:=10;
  FLines:=TStringList.Create;
  FCustomText:=TStringList.Create;
  with FShortCutData do begin
    PrivateKeyShortCuts:=TKeyShortCutCollection.Create(Self,TPrivateKeyShortCut);
    PrivateMouseDownShortCuts:=TMouseShortCutCollection.Create(Self,TPrivateMouseShortCut);
    PrivateMouseMoveShortCuts:=TMouseShortCutCollection.Create(Self,TPrivateMouseShortCut);
    RegisterPrivateShortCuts;
    PublicKeyShortCuts:=TKeyShortCutCollection.Create(Self,TPublicKeyShortCut);
    PublicMouseDownShortCuts:=TMouseShortCutCollection.Create(Self,TPublicMouseShortCut);
    PublicMouseMoveShortCuts:=TMouseShortCutCollection.Create(Self,TPublicMouseShortCut);
  end;
  FEditOptions:=TEditOptions.Create(Self);
  with FHighlightData do begin
    with BlockHighlights[hbWhiteSpace] do begin
      BackgroundColor:=clNavy;
      FontColor:=clYellow;
      FontStyle:=[];
    end;
    with BlockHighlights[hbSelection] do begin
      BackgroundColor:=clSilver;
      FontColor:=clNavy;
      FontStyle:=[];
    end;
    with BlockHighlights[hbSearch] do begin
      BackGroundColor:=0;
      FontColor:=clLime;
      FontStyle:=[];
    end;
    with BlockHighlights[hbSymbolJump] do begin
      BackGroundColor:=clNavy;
      FontColor:=clWhite;
      FontStyle:=[fsBold,fsItalic,fsUnderline];
    end;
    with BlockHighlights[hbRightMargin] do begin
      BackGroundColor:=0;
      FontColor:=clSilver;
      FontStyle:=[];
    end;
    with BlockHighlights[hbCompileError] do begin
      BackgroundColor:=clRed;
      FontColor:=clWhite;
      FontStyle:=[fsBold];
    end;
    with BlockHighlights[hbExecuteError] do begin
      BackgroundColor:=clRed;
      FontColor:=clWhite;
      FontStyle:=[];
    end;
    with BlockHighlights[hbWarning] do begin
      BackgroundColor:=clSilver;
      FontColor:=0;
      FontStyle:=[fsBold];
    end;
  end;
  Cursor:=crIBeam;
  ShowHint:=True;
  FThread:=TSyntaxHighlightThread.Create(FLines,Self);
  ContentModified;
end;

procedure TCustomSyntaxHighlightMemo.CreateHandle;
begin
  inherited;
  Perform(CM_FONTCHANGED,0,0);
end;

procedure TCustomSyntaxHighlightMemo.Customize;
begin
  CustomizeEditor(Self);
end;

procedure TCustomSyntaxHighlightMemo.Cut;
begin
  Copy;
  DeleteSelection;
end;

procedure TCustomSyntaxHighlightMemo.DblClick;
begin
  inherited;
  MultiClickAction(FEditOptions.DoubleClickAction);
end;

procedure TCustomSyntaxHighlightMemo.DeleteSelection;
var
  a:Integer;
begin
  if SelEmpty then
    Exit;
  MarkModified;
  FLines.BeginUpdate;
  FEditHistory.BeginGroup;
  try
    InvalidateSelection;
    if FEditOptions.PersistentBlocks and (SelKind=skLinear) then begin
      if IsInSelection(CaretPos) then
        CaretPos:=SelStart
      else
        with SelRect do begin
          if (CaretPos.Y>SelRect.Bottom) or ((CaretPos.Y>SelRect.Bottom) and (CaretPos.X>SelEnd.X)) then
            with CaretPos do
              CaretPos:=Point(X-Right+Left,Y-Bottom+Top);
        end;
    end else
      CaretPos:=SelStart;
    with SelRect do begin
      case SelKind of
        skLinear:begin
          if Bottom>Top then begin
            FEditHistory.AddDeleteItem(Point(0,Bottom),System.Copy(FLines[Bottom],1,SelEnd.X));
            for a:=Bottom-1 downto Top+1 do
              FEditHistory.AddDeleteItem(Point(0,a),FLines[a]+#13);
            FEditHistory.AddDeleteItem(Point(SelStart.X,Top),System.Copy(FLines[Top],SelStart.X+1,Length(FLines[Top]))+#13);
            FLines[Top]:=System.Copy(FLines[Top],1,SelStart.X)+System.Copy(FLines[Bottom],SelEnd.X+1,Length(FLines[Bottom]));
            for a:=Bottom downto Top+1 do
              FLines.Delete(a);
            InvalidateLines(Top,FLines.Count+VisibleTextSize.cy);
          end else begin
            FEditHistory.AddDeleteItem(TopLeft,System.Copy(FLines[Bottom],Left+1,Right-Left));
            FLines[Top]:=System.Copy(FLines[Top],1,Left)+System.Copy(FLines[Top],Right+1,Length(FLines[Top]));
            InvalidateLine(Top);
          end;
          //FormatTextLine(Top);
        end;
        skRect:begin
          for a:=Bottom downto Top do
            FEditHistory.AddDeleteItem(Point(Left,a),System.Copy(FLines[a],Left+1,Right-Left));
          for a:=Top to Bottom do begin
            FLines[a]:=System.Copy(FLines[a],1,Left)+System.Copy(FLines[a],Right+1,Length(FLines[a]));
            //FormatTextLine(a);
          end;
        end;
      end;
    end;
    ClearSelection;
  finally
    FLines.EndUpdate;
    FEditHistory.EndGroup;
  end;
end;

procedure TCustomSyntaxHighlightMemo.DeleteTextAtCaret(Offset: Integer);
var
  p:TPoint;
begin
  FEditHistory.BeginGroup;
  p:=CaretPos;
  DeleteTextAtPos(p,Offset);
  CaretPos:=p;
  FEditHistory.EndGroup;
end;

procedure TCustomSyntaxHighlightMemo.DeleteTextAtPos(var p: TPoint;
  Offset: Integer);
var
  s:string;
begin
  if Offset=0 then
    Exit;
  MarkModified;
  FEditHistory.BeginGroup;
  if Offset>0 then begin
    while (Offset>0) and (p.Y<FLines.Count) and ((p.Y<FLines.Count-1) or (p.X<Length(FLines[FLines.Count-1]))) do begin
      if p.X>=Length(FLines[p.Y]) then begin
        if Length(FLines[p.Y])+Length(FLines[p.Y+1])<=FEditOptions.MaxLineSize then begin
          s:=FLines[p.Y];
          SetLength(s,p.X);
          if Length(FLines[p.Y])<Length(s) then
            FillChar(s[Length(FLines[p.Y])+1],p.X-Length(FLines[p.Y]),Ord(' '));
          FLines[p.Y]:=s+FLines[p.Y+1];
          FEditHistory.AddDeleteItem(p,#13);
          FLines.Delete(p.Y+1);
          InvalidateLines(p.Y,FLines.Count);
        end;
        Dec(Offset);
      end else begin
        if Offset>=Length(FLines[p.Y])-p.X then begin
          FEditHistory.AddDeleteItem(p,System.Copy(FLines[p.Y],p.X+1,Length(FLines[p.Y])-p.X));
          Dec(Offset,Length(FLines[p.Y])-p.X);
          FLines[p.Y]:=System.Copy(FLines[p.Y],1,p.X);
          InvalidateLine(p.Y);
        end else begin
          FEditHistory.AddDeleteItem(p,System.Copy(FLines[p.Y],p.X+1,Offset));
          FLines[p.Y]:=System.Copy(FLines[p.Y],1,p.X)+System.Copy(FLines[p.Y],p.X+Offset+1,Length(FLines[p.Y]));
          Offset:=0;
          InvalidateLine(p.Y);
        end;
      end;
      //FormatTextLine(p.Y);
    end;
  end else begin
    while (Offset<0) and ((p.Y>0) or (p.X>0)) and (p.Y<FLines.Count) do begin
      if p.X=0 then begin
        if Length(FLines[p.Y-1])+Length(FLines[p.Y])<=FEditOptions.MaxLineSize then begin
          p.X:=Length(FLines[p.Y-1]);
          FLines[p.Y-1]:=FLines[p.Y-1]+FLines[p.Y];
          FLines.Delete(p.Y);
          Dec(p.Y);
          FEditHistory.AddDeleteItem(p,#13);
          InvalidateLines(p.Y,FLines.Count);
        end;
        Inc(Offset);
      end else begin
        if -Offset>=p.X then begin
          Inc(Offset,p.X);
          FEditHistory.AddDeleteItem(Point(0,p.Y),System.Copy(FLines[p.Y],1,p.X));
          FLines[p.Y]:=System.Copy(FLines[p.Y],p.X+1,Length(FLines[p.Y]));
          p.X:=0;
          InvalidateLine(p.Y);
        end else begin
          FEditHistory.AddDeleteItem(Point(p.X+Offset,p.Y),System.Copy(FLines[p.Y],p.X+Offset+1,-Offset));
          FLines[p.Y]:=System.Copy(FLines[p.Y],1,p.X+Offset)+System.Copy(FLines[p.Y],p.X+1,Length(FLines[p.Y]));
          p.X:=p.X+Offset;
          Offset:=0;
          InvalidateLine(p.Y);
        end;
      end;
      //FormatTextLine(p.Y);
    end;
  end;
  FEditHistory.EndGroup;
end;

destructor TCustomSyntaxHighlightMemo.Destroy;
begin
  FEditOptions.Destroy;
  FThread.Terminate;
  FThread.Destroy;
  FLines.Destroy;
  FCustomText.Destroy;
  FCanvas.Destroy;
  with FShortCutData do begin
    PrivateKeyShortCuts.Destroy;
    PrivateMouseDownShortCuts.Destroy;
    PrivateMouseMoveShortCuts.Destroy;
    PublicKeyShortCuts.Destroy;
    PublicMouseDownShortCuts.Destroy;
    PublicMouseMoveShortCuts.Destroy;
  end;
  inherited;
end;

procedure TCustomSyntaxHighlightMemo.DoContextPopup(MousePos: TPoint;
  var Handled: Boolean);
var
  c:TCharClass;
  p:TPoint;
begin
  if not Assigned(PopupMenu) then begin
    BuildBaseMenu;
    p:=ClientToText(MousePos);
    c:=FThread.CharClass[p.Y,p.X];
    with FPopupMenuData do begin
      SymbolJumpMenuItem.Enabled:=FEditOptions.AllowCodeLinks and CharClassCanJump(c);
      SymbolJumpMenuItem.Tag:=MakeLong(p.X,p.Y);
      CopyMenuItem.Enabled:=not SelEmpty;
      CutMenuItem.Enabled:=not SelEmpty;
      PasteMenuItem.Enabled:=Clipboard.AsText<>'';
      UndoMenuItem.Enabled:=CanUndo;
      RedoMenuItem.Enabled:=CanRedo;
      with ClientToScreen(MousePos) do
        PopupMenu.Popup(X,Y);
    end;
  end else
    Handled:=False;
end;

function TCustomSyntaxHighlightMemo.DoMouseWheelDown(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  Result:=True;
  if Shift+[ssCtrl]=[ssCtrl] then begin
    if ssCtrl in Shift then
      VertScrollBar.Position:=VertScrollBar.Position+VisibleTextSize.cy-1
    else
      VertScrollBar.Position:=VertScrollBar.Position+3;
    Invalidate;
    UpdateCaret;
  end;
  if Shift+[ssCtrl]=[ssShift,ssCtrl] then begin
    with CaretPos do
      if ssCtrl in Shift then
        CaretPos:=Point(X,Y+VisibleTextSize.cy-1)
      else
        CaretPos:=Point(X,Y+1);
  end;
end;

function TCustomSyntaxHighlightMemo.DoMouseWheelUp(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  Result:=True;
  if Shift+[ssCtrl]=[ssCtrl] then begin
    if ssCtrl in Shift then
      VertScrollBar.Position:=VertScrollBar.Position-VisibleTextSize.cy+1
    else
      VertScrollBar.Position:=VertScrollBar.Position-3;
    Invalidate;
    UpdateCaret;
  end;
  if Shift+[ssCtrl]=[ssShift,ssCtrl] then begin
    with CaretPos do
      if ssCtrl in Shift then
        CaretPos:=Point(X,Y-VisibleTextSize.cy+1)
      else
        CaretPos:=Point(X,Y-1);
  end;
end;

procedure TCustomSyntaxHighlightMemo.EditStateChanged;
var
  a:Integer;
begin
  for a:=0 to FEditStateListeners.Count-1 do
    with (FEditStateListeners[a] as IEditStateListener) do
      EditStateChanged;
  if Assigned(FOnEditStateChanged) then
    FOnEditStateChanged(Self);
end;

procedure TCustomSyntaxHighlightMemo.FormatTextLine(LineId: Integer);
var
  a:Integer;
begin
  if (LineId<0) or (LineID>=FLines.Count) or (FLines[LineID]='') then
    Exit;
  if (FLines[LineID][Length(FLines[LineID])]=' ') and not FEditOptions.PreserveLineEnd then begin
    a:=Length(FLines[LineID]);
    while (a>0) and (FLines[LineID][a]=' ') do
      Dec(a);
    if a<=0 then 
      FLines[LineID]:=''
    else
      FLines[LineID]:=System.Copy(FLines[LineId],1,a);
  end;
  if Length(FLines[LineID])>FEditOptions.MaxLineSize then
    FLines[LineID]:=System.Copy(FLines[LineId],1,FEditOptions.MaxLineSize);
end;

function TCustomSyntaxHighlightMemo.GetCanvas: TCanvas;
begin
  Result:=FCanvas;
end;

function TCustomSyntaxHighlightMemo.GetCaretPos: TPoint;
begin
  Result:=FCaretData.Pos;
end;

function TCustomSyntaxHighlightMemo.GetCode: string;
begin
  Result:=Flines.Text;
end;

function TCustomSyntaxHighlightMemo.GetComponent: TComponent;
begin
  Result:=Self;
end;

function TCustomSyntaxHighlightMemo.GetCustomText: TStrings;
begin
  Result:=FCustomText;
end;

function TCustomSyntaxHighlightMemo.GetEditMode: TEditMode;
begin
  Result:=FEditOptions.WriteMode;
end;

function TCustomSyntaxHighlightMemo.GetHighlightClassCount: Integer;
begin
  Result:=8;
end;

function TCustomSyntaxHighlightMemo.GetHighlightClassHasText(
  Index: Integer): Boolean;
begin
  Result:=Index<>2;
end;

function TCustomSyntaxHighlightMemo.GetHighlightClassName(
  Index: Integer): string;
const
  T:array[0..7] of string=(
    'White space',
    'Text selection',
    'Right margin',
    'Hot link',
    'Search match',
    'Compile error line',
    'Execution error line',
    'Warning'
  );
begin
  Result:=T[Index];
end;

function TCustomSyntaxHighlightMemo.GetHighlightData(
  Index: Integer): TCharClassHighlight;
begin
  Result:=FHighlightData.BlockHighlights[THighlightBlockType(Index)];
end;

function TCustomSyntaxHighlightMemo.GetKeyShortCuts: TKeyShortCutCollection;
begin
  Result:=FShortCutData.PublicKeyShortCuts;
end;

function TCustomSyntaxHighlightMemo.GetLines: TStrings;
begin
  Result:=FLines;
end;

function TCustomSyntaxHighlightMemo.GetModified: Boolean;
begin
  Result:=FModified;
end;

function TCustomSyntaxHighlightMemo.GetMouseDownShortCuts: TMouseShortCutCollection;
begin
  Result:=FShortCutData.PublicMouseDownShortCuts;
end;

function TCustomSyntaxHighlightMemo.GetMouseMoveShortCuts: TMouseShortCutCollection;
begin
  Result:=FShortCutData.PublicMouseMoveShortCuts;
end;

function TCustomSyntaxHighlightMemo.GetSelEmpty: Boolean;
begin
  with FSelData do
    Result:=(SelKind=skNone) or ((y2<=y1) and (x2<=x1));
end;

function TCustomSyntaxHighlightMemo.GetSelEnd: TPoint;
begin
  with FSelData do
    Result:=Point(X2,Y2);
end;

function TCustomSyntaxHighlightMemo.GetSelKind: TSelKind;
begin
  Result:=FSelData.Kind;
end;

function TCustomSyntaxHighlightMemo.GetSelLength: TSize;
begin
  with FSelData do
    Result:=Size(X2-X1,Y2-Y1);
end;

function TCustomSyntaxHighlightMemo.GetSelOrigin: TPoint;
begin
  Result:=FSelData.Origin;
end;

function TCustomSyntaxHighlightMemo.GetSelRect: TRect;
begin
  with FSelData,Result do begin
    Top:=Y1;
    Bottom:=Y2;
    if X2>=X1 then begin
      Left:=X1;
      Right:=X2;
    end else begin
      Left:=X2;
      Right:=X1;
    end;
  end;
end;

function TCustomSyntaxHighlightMemo.GetSelStart: TPoint;
begin
  with FSelData do
    Result:=Point(X1,Y1);
end;

function TCustomSyntaxHighlightMemo.GetSelText: string;
var
  a:Integer;
const
  sCRLF=#13;
begin
  Result:='';
  if not SelEmpty then
    with SelRect do
      case SelKind of
        skLinear:begin
          if Bottom>Top then begin
            Result:=System.Copy(FLines[Top],SelStart.X+1,Length(FLines[Top]))+sCRLF;
            for a:=Top+1 to Bottom-1 do
              Result:=Result+FLines[a]+sCRLF;
            Result:=Result+System.Copy(FLines[Bottom],1,SelEnd.X);
          end else
            Result:=System.Copy(FLines[Top],Left+1,Right-Left);
        end;
        skRect:begin
          for a:=Top to Bottom do begin
            Result:=Result+System.Copy(FLines[a],Left+1,Right-Left);
            if a<Bottom then
              Result:=Result+sCRLF;
          end;
        end;
      end;
end;

function TCustomSyntaxHighlightMemo.GetText: string;
begin
  Result:=FLines.Text;
  Result:=System.Copy(Result,1,Length(Result)-2);
end;

function TCustomSyntaxHighlightMemo.IndentJumpPos(p: TPoint): TPoint;
var
  a,b:Integer;
begin
  with p do begin
    if Y>=FLines.Count then begin
      X:=0;
      Y:=FLines.Count-1;
    end;
    if Y<0 then begin
      X:=0;
      Y:=0;
    end;
    a:=Y-1;
    Result:=p;
    if a<0 then
      Exit;
    b:=X+1;
    while (b<=Length(FLines[a])) and (FLines[a][b]=' ') do
      Inc(b);
    if b<=Length(FLines[a]) then
      Result.X:=b-1;
  end;
end;

procedure TCustomSyntaxHighlightMemo.InvalidateLine(Index: Integer);
begin
  InvalidateLines(Index,Index);
end;

procedure TCustomSyntaxHighlightMemo.InvalidateLines(Index1,
  Index2: Integer);
var
  a:Integer;
  r:TRect;
begin
  if not HandleAllocated then
    Exit;
  if Index1>Index2 then begin
    a:=Index1;
    Index1:=Index2;
    Index2:=a;
  end;
  with Point(HorzScrollBar.Position,VertScrollBar.Position),VisibleTextSize do begin
    if Index1<Y then
      Index1:=Y;
    if Index2>Y+cy then
      Index2:=Y+cy;
    r.Left:=0;
    r.Right:=ClientWidth;
    with TextToClient(Point(0,Index1)) do
      r.Top:=Y;
    with TextToClient(Point(0,Index2)) do
      r.Bottom:=Y+CharSize.cy;
    InvalidateRect(Handle,@r,False);
  end;
end;

procedure TCustomSyntaxHighlightMemo.InvalidateSelection;
begin
  InvalidateLines(FSelData.Y1,FSelData.Y2);
end;

function TCustomSyntaxHighlightMemo.IsInSelection(p: TPoint): Boolean;

  function IsInInterval(x,x1,x2:Integer):Boolean;
  begin
    if x1<=x2 then
      Result:=(x>=x1) and (x<=x2)
    else
      Result:=(x>=x2) and (x<=x1);
  end;

begin
  Result:=False;
  case SelKind of
    skLinear:begin
      Result:=IsInInterval(p.Y,FSelData.Y1,FSelData.Y2);
      if Result then begin
        if p.Y=FSelData.Y1 then
          Result:=p.X>=FSelData.X1;
        if p.Y=FSelData.Y2 then
          Result:=Result and (p.X<=FSelData.X2);
      end;
    end;
    skRect:Result:=IsInInterval(p.X,FSelData.X1,FSelData.X2) and IsInInterval(p.Y,FSelData.Y1,FSelData.Y2);
  end;
end;

procedure TCustomSyntaxHighlightMemo.JumpAtCaret;
begin
  JumpAtPos(CaretPos);
end;

procedure TCustomSyntaxHighlightMemo.JumpAtPos(p: TPoint);
var
  c:TCharClass;
begin
  c:=FThread.CharClass[p.Y,p.X];
  if CharClassCanJump(c) then
    CharClassJump(c,CharClassText(p),p)
  else begin
    Dec(p.X);
    c:=FThread.CharClass[p.Y,p.X];
    if CharClassCanJump(c) then
      CharClassJump(c,CharClassText(p),p)
  end;
end;

procedure TCustomSyntaxHighlightMemo.KeyDown(var Key: Word;
  Shift: TShiftState);
var
  a:Integer;
begin
  inherited;
  with FShortCutData.PublicKeyShortCuts do
    for a:=Count-1 downto 0 do
      if (Item[a].ShiftState=Shift) and (Item[a].Key=Key) then
        Item[a].Execute(Key,Shift);
  with FShortCutData.PrivateKeyShortCuts do
    for a:=Count-1 downto 0 do
      if (Item[a].ShiftState=Shift) and (Item[a].Key=Key) then
        Item[a].Execute(Key,Shift);
end;

procedure TCustomSyntaxHighlightMemo.KeyPress(var Key: Char);
begin
  inherited;
  if (Ord(Key)>27) and not (Ord(Key) in [VK_BACK,VK_F16]) then
    AddTextAtCaret(Key);
  SelOrigin:=CaretPos;
//  TForm(Parent).Caption:=IntTOHex(Ord(Key),2);
end;

procedure TCustomSyntaxHighlightMemo.KeyUp(var Key: Word;
  Shift: TShiftState);
begin
  inherited;
  with FTrackingData do
    if JumpHighlight then begin
      InvalidateLine(p1.Y);
      JumpHighlight:=False;
    end;
end;

procedure TCustomSyntaxHighlightMemo.LaunchTracking;
var
  TME:TTrackMouseEvent;
begin
  with FTrackingData do begin
    with TME do begin
      cbSize:=SizeOf(TME);
      dwFlags:=TME_HOVER;
      hwndTrack:=Handle;
      dwHoverTime:=FEditOptions.HoverTime;
    end;
    TrackMouseEvent(TME);
  end;
end;

procedure TCustomSyntaxHighlightMemo.MarkBlock(
  BlockType: THighlightBlockType; CharOffset, CharLength: Integer);
begin
  CaretPos:=CharOffsetToPos(CharOffset);
  with FHighlightData,CaretPos do begin
    InvalidateLine(Y);
    BlockPositions[BlockType]:=CaretPos;
    Select(CaretPos,CharOffsetToPos(CharOffset+CharLength));
  end;
  SelKind:=skLinear;
  SetFocus;
end;

procedure TCustomSyntaxHighlightMemo.MarkCompileError(CharOffset, CharLength: Integer);
begin
  MarkBlock(hbCompileError,CharOffset,CharLength);
end;

procedure TCustomSyntaxHighlightMemo.MarkExecuteError(CharOffset, CharLength: Integer);
begin
  MarkBlock(hbExecuteError,CharOffset,CharLength);
end;

procedure TCustomSyntaxHighlightMemo.MarkModified;
begin
  Modified:=True;
end;

procedure TCustomSyntaxHighlightMemo.MarkUnmodified;
begin
  Modified:=False;
end;

procedure TCustomSyntaxHighlightMemo.MarkWarning(CharOffset, CharLength: Integer);
begin
  MarkBlock(hbWarning,CharOffset,CharLength);
end;

procedure TCustomSyntaxHighlightMemo.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  a:Integer;
begin
  inherited;
  with FTrackingData do begin
    if (Abs(X-LastClickPos.X)<GetSystemMetrics(SM_CXDOUBLECLK)) and (Abs(Y-LastClickPos.Y)<GetSystemMetrics(SM_CYDOUBLECLK)) and (GetTickCount-LastClickTick<GetDoubleClickTime) then begin
      Inc(ClickLevel);
      if ClickLevel=3 then
        ClickLevel:=0;
      if CLickLevel=2 then begin
        MultiClickAction(FEditOptions.FTripleClickAction);
        Exit;
      end;
    end else
      ClickLevel:=0;
  end;
  FTrackingData.LastClickPos:=Point(X,Y);
  FTrackingData.LastClickTick:=GetTickCount;
  with FShortCutData.PrivateMouseDownShortCuts do
    for a:=0 to Count-1 do
      with Item[a] do
        if ShiftState=Shift then
          Execute(X,Y,ShiftState);
  with FShortCutData.PublicMouseDownShortCuts do
    for a:=0 to Count-1 do
      with Item[a] do
        if ShiftState=Shift then
          Execute(X,Y,ShiftState);
end;

procedure TCustomSyntaxHighlightMemo.MouseMove(Shift: TShiftState; X,
  Y: Integer);
var
  a:Integer;
begin
  LaunchTracking;
  inherited;
  with FShortCutData.PrivateMouseMoveShortCuts do
    for a:=0 to Count-1 do
      with Item[a] do
        if ShiftState=Shift then
          Execute(X,Y,ShiftState);
  with FShortCutData.PublicMouseMoveShortCuts do
    for a:=0 to Count-1 do
      with Item[a] do
        if ShiftState=Shift then
          Execute(X,Y,ShiftState);
end;

procedure TCustomSyntaxHighlightMemo.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  m:TEditMode;
  s:string;
  p,q:TPoint;
begin
  inherited;
  with FEditOptions,FTrackingData do begin
    if DraggingText then begin
      p:=ClientToText(Point(X,Y));
      if p.Y>=FLines.Count then
        p.Y:=FLines.Count-1;
      if p.X<0 then
        p.X:=0;
      if not (IsInSelection(p) and IsInSelection(Point(p.X+1,p.Y))) then begin
        s:=SelText;
        m:=WriteMode;
        WriteMode:=emInsert;
        q:=p;
        if (p.Y>SelStart.Y) or ((p.Y=SelStart.Y) and (p.X>=SelEnd.X)) then begin
          Dec(p.Y,SelLength.cy);
          //Dec(p.X,SelEnd.x);
          DeleteSelection;
          AddTextAtPos(p,s);
        end else begin
          DeleteSelection;
          AddTextAtPos(p,s);
        end;
        CaretPos:=q;
        WriteMode:=m;
      end;
      DraggingText:=False;
      SetCursor(Screen.Cursors[crArrow]);
      Invalidate;
    end;
  end;
end;

procedure TCustomSyntaxHighlightMemo.MultiClickAction(
  Action: TMultiClickAction);
var
  s:string;
  x1,x2,x3,x4:Integer;

  function FindIdent(ToRight:Boolean;x:Integer;var x1,x2:Integer):Boolean;
  var
    Delta:Integer;
  begin
    if ToRight then
      Delta:=1
    else
      Delta:=-1;
    if x>Length(s) then
      x:=Length(s);
    while (x>0) and (x<=Length(s)) and IsCharAlphaNumeric(s[x]) do
      x:=x-Delta;
    while (x>0) and (x<=Length(s)) and not IsCharAlphaNumeric(s[x]) do
      x:=x+Delta;
    if ToRight then
      x1:=x-1
    else
      x2:=x;
    while (x>0) and (x<=Length(s)) and IsCharAlphaNumeric(s[x]) do
      x:=x+Delta;
    if ToRight then
      x2:=x-1
    else
      x1:=x;
    Result:=x1<>x2;
  end;

begin
  with CaretPos do
    case Action of
      maWord:begin
        s:=FLines[Y];
        if s='' then
          Exit;
        if FindIdent(False,X,x1,x2) then begin
          if FindIdent(True,X,x3,x4) then begin
            if Abs(X-X2)<Abs(x3-X) then
              Select(Point(x1,Y),Point(x2,Y))
            else
              Select(Point(x3,Y),Point(x4,Y));
          end else
            Select(Point(x1,Y),Point(x2,Y));
        end else begin
          if FindIdent(True,X,x3,x4) then
            Select(Point(x3,Y),Point(x4,Y))
          else
            Exit;
        end;
        SelKind:=skLinear;
      end;
      maLine:begin
        if Y=FLines.Count-1 then
          Select(Point(0,Y),Point(Length(FLines[Y]),Y))
        else
          Select(Point(0,Y),Point(0,Y+1));
        SelKind:=skLinear;
      end;
    end;
end;

procedure TCustomSyntaxHighlightMemo.PaintGutter;
var
  r:TRect;
  a:Integer;
  s:string;
begin
  if not FEditOptions.GutterVisible then
    Exit;
  with FEditOptions do
    r:=Rect(GutterMargin-5,0,GutterMargin-1,ClientHeight);
  DrawEdge(Canvas.Handle,r,EDGE_RAISED,{BF_FLAT or }BF_LEFT or BF_RIGHT);
  Canvas.Brush.Style:=bsClear;
  with VisibleTextRect do
    for a:=Top to Bottom do
      with TextToClient(Point(0,a)) do begin
        if a<FLines.Count then begin
          s:=IntToStr(a+1);
          if IsInSelection(Point(0,a)) and IsInSelection(Point(Length(FLines[a]),a)) and not SelEmpty then begin
            Canvas.Brush.Style:=bsSolid;
            with FHighlightData.BlockHighlights[hbSelection] do begin
              Canvas.Font.Color:=ColorToRGB(FontColor) xor $FFFFFF;
              Canvas.Brush.Color:=ColorToRGB(BackgroundColor) xor $FFFFFF;
            end;
            Canvas.FillRect(Rect(0,Y,FEditOptions.GutterMargin-3,Y+CharSize.cy));
            Canvas.Brush.Style:=bsClear;
            //ColorToRGB(Color) xor $FFFFFF
          end else
            Canvas.Font.Color:=Font.Color;
          Canvas.TextOut(FEditOptions.FGutterMargin-Canvas.TextWidth(s)-5,Y,s);
        end;
      end;
end;

procedure TCustomSyntaxHighlightMemo.PaintWindow(DC: HDC);
var
  p1,p2:TPoint;
  a,b,d:Integer;
  c:Char;
  r:TRect;
  SaveDC:HDC;
  g,h:TCharClassHighlight;
  s:string;
  u,v,w:Boolean;
begin
  v:=False;
  w:=False;
  with VisibleTextRect do begin
    p1:=TopLeft;
    p2:=BottomRight;
  end;
  SaveDC:=Canvas.Handle;
  Canvas.Handle:=DC;
  FThread.Lock;
  try
    Canvas.Font.Name:=Font.Name;
    Canvas.Font.Size:=Font.Size;
    PaintGutter;
    Canvas.Brush.Style:=bsSolid;
    Canvas.Pen.Mode:=pmCopy;
    for a:=p1.Y to p2.Y do
      for b:=p1.X to p2.X+1 do begin
        if (a>=0) and (a<FLines.Count) and (b>=0) and (b<Length(FLines[a])) then begin
          c:=FLines[a][b+1];
          u:=c=' ';
        end else begin
          c:=' ';
          u:=False;
        end;
        g:=PosHighlight(Point(b,a));      
        if b=p1.X then begin
          s:='';
          h:=g;
          v:=False;
          w:=True;
          with TextToClient(Point(b,a)),CharSize do
            r:=Rect(X,Y,X+cx,Y+cy);
        end;
        if CompareMem(@g,@h,SizeOf(g)) and (b<=p2.X) and (v=u) then begin
          with CharSize do
            Inc(r.Right,cx);
//          if not u then
            s:=s+c;
        end else begin
          with Canvas do begin
            Brush.Color:=h.BackgroundColor;
            FillRect(r);
            with TextToClient(Point(FEditOptions.RightMarginWidth,a)) do
              if FEditOptions.RightMarginVisible and (X>=r.Left) and (x<r.Right) then begin
                Pen.Color:=FHighlightData.BlockHighlights[hbRightMargin].FontColor;
                MoveTo(X,Y);
                LineTo(X,Y+CharSize.cy);
              end;
            if v then begin
              Pen.Color:=FHighlightData.BlockHighlights[hbWhiteSpace].FontColor;
              for d:=1 to Length(s) do begin
                if FEditOptions.FShowSpace then  
                  Pixels[r.Left+(CharSize.cx*(2*d-1)) div 2,r.Top+CharSize.cy div 2]:=FHighlightData.BlockHighlights[hbWhiteSpace].FontColor;//TODO
                if FEditOptions.FShowTab and w and ((d+p1.X) mod FEditOptions.TabLength=0) then begin
                  MoveTo(r.Left+(CharSize.cx*(2*d-1)) div 2,r.Top+1);
                  LineTo(r.Left+(CharSize.cx*(2*d-1)) div 2,r.Bottom-1);
                end;
              end;
              w:=False;
            end else begin
              if s<>'' then begin
                Font.Color:=h.FontColor;
                Font.Style:=h.FontStyle;
                Brush.Style:=bsClear;
                TextOut(r.Left,r.Top,s);
                Brush.Style:=bsSolid;
              end;
            end;
          end;
          s:=c;
          h:=g;
          with TextToClient(Point(b,a)),CharSize do
            r:=Rect(X,Y,X+cx,Y+cy);
          v:=u;
        end;
      end;
  finally
    FThread.unlock;
    Canvas.CopyMode:=cmSrcCopy;
    Canvas.Handle:=SaveDC;
  end;
end;

procedure TCustomSyntaxHighlightMemo.Paste;
var
  s:string;
begin
  s:=ClipBoard.AsText;
  if s<>'' then
    AddTextAtCaret(s);
end;

function TCustomSyntaxHighlightMemo.PosHighlight(
  p: TPoint): TCharClassHighlight;
var
  t:THighlightBlockType;
begin
  if IsInSelection(p) and IsInSelection(Point(p.X+1,p.Y)) then begin
    Result:=FHighlightData.BlockHighlights[hbSelection];
    Exit;
  end;
  with FHighlightData do
    for t:=hbSearch to hbWarning do
      if p.Y=BlockPositions[t].Y then begin
        Result:=FHighlightData.BlockHighlights[t];
        Exit;
      end;
  with FTrackingData do
    if (JumpHighlight) and (p.Y=p1.Y) and (p.X>=p1.X) and (p.X<=p2.X) then begin
      Result:=FHighlightData.BlockHighlights[hbSymbolJump];
      Exit;
    end;
  Result:=CharClassHighlight(FThread.CharClass[p.Y,p.X]);
end;

procedure TCustomSyntaxHighlightMemo.Redo;
begin
  FEditHistory.Redo;
end;

procedure TCustomSyntaxHighlightMemo.RegisterPrivateKeyShortCut(
  AProc: TPrivateKeyProc; AKey: Word; AShiftState: TShiftState);
begin
  with FShortCutData.PrivateKeyShortCuts.Add as TPrivateKeyShortCut do begin
    FExecuteProc:=AProc;
    Key:=AKey;
    ShiftState:=AShiftState;
  end;
end;

procedure TCustomSyntaxHighlightMemo.RegisterEditStateListener(
  Listener: IEditStateListener);
begin
  FEditStateListeners.Add(Listener.GetComponent);
end;

procedure TCustomSyntaxHighlightMemo.RegisterPrivateKeyShortCut(
  AProc: TPrivateKeyProc; Keys: array of Word;
  ShiftStates: array of TShiftState);
var
  a,b:Integer;
begin
  for a:=Low(Keys) to High(Keys) do
    for b:=Low(ShiftStates) to High(ShiftStates) do
      RegisterPrivateKeyShortCut(AProc,Keys[a],ShiftStates[b]);
end;

procedure TCustomSyntaxHighlightMemo.RegisterPrivateMouseDownShortCut(
  AProc: TPrivateMouseProc; AShiftState:TShiftState);
begin
  with FShortCutData.PrivateMouseDownShortCuts.Add as TPrivateMouseShortCut do begin
    FExecuteProc:=AProc;
    ShiftState:=AShiftState;
  end;
end;

procedure TCustomSyntaxHighlightMemo.RegisterPrivateMouseMoveShortCut(
  AProc: TPrivateMouseProc; AShiftState:TShiftState);
begin
  with FShortCutData.PrivateMouseMoveShortCuts.Add as TPrivateMouseShortCut do begin
    FExecuteProc:=AProc;
    ShiftState:=AShiftState;
  end;
end;

procedure TCustomSyntaxHighlightMemo.RegisterPrivateShortCuts;
const
  T1:array[0..3] of TShiftState=([],[ssCtrl],[ssShift],[ssShift,ssCtrl]);
  T2:array[0..1] of TShiftState=([],[ssCtrl]);
  T3:array[0..0] of TShiftState=([ssCtrl]);
  T4:array[0..1] of TShiftState=([],[ssShift]);
  T5:array[0..0] of TShiftState=([]);
  S1:TShiftState=[ssShift];
begin
  RegisterPrivateKeyShortCut(_SCKArrow,[VK_LEFT,VK_RIGHT,VK_UP,VK_DOWN],T1);
  RegisterPrivateKeyShortCut(_SCKDelete,[VK_DELETE,VK_BACK],T2);
  RegisterPrivateKeyShortCut(_SCKClipBoard,[Ord('C'),Ord('V'),Ord('X')],T3);
  RegisterPrivateKeyShortCut(_SCKExtremi,[VK_END,VK_HOME],T1);
  RegisterPrivateKeyShortCut(_SCKScroll,[VK_PRIOR,VK_NEXT],T1);
  RegisterPrivateKeyShortCut(_SCKTab,[VK_TAB],T4);
  RegisterPrivateKeyShortCut(_SCKEnter,[VK_RETURN],T5);
  RegisterPrivateKeyShortCut(_SCKEscape,[VK_ESCAPE],T5);
  RegisterPrivateKeyShortCut(_SCKCtrl,[Ord('A'),Ord('P'),Ord('Y'),Ord('Z')],T3);
  RegisterPrivateKeyShortCut(_SCKMenu,[VK_F1,VK_F2,VK_F3,VK_F4,VK_F5,VK_F6,VK_F7,VK_F8,VK_F9,VK_F10,VK_F11,VK_F12],T5);
  RegisterPrivateKeyShortCut(_SCKInsert,[VK_INSERT,VK_NUMLOCK,VK_CAPITAL],T5);

  RegisterPrivateMouseMoveShortCut(_SCMM,[ssLeft]);
  RegisterPrivateMouseMoveShortCut(_SCMM,[ssShift,ssLeft]);
  RegisterPrivateMouseMoveShortCut(_SCMM,[ssAlt,ssLeft]);

  RegisterPrivateMouseDownShortCut(_SCMD,[ssLeft]);
  RegisterPrivateMouseDownShortCut(_SCMD,[ssLeft,ssAlt]);
  RegisterPrivateMouseDownShortCut(_SCMD,[ssLeft,ssShift]);

  RegisterPrivateMouseDownShortCut(_SCMDJump,[ssLeft,ssCtrl]);

  RegisterPrivateMouseMoveShortCut(_SCMMJump,[ssCtrl]);
end;

procedure TCustomSyntaxHighlightMemo.ScrollPosChanged;
begin
  inherited;
  UpdateCaret;
end;

procedure TCustomSyntaxHighlightMemo.ScrollToCaret;
begin
  with VisibleTextSize,CaretPos do begin
    if X<HorzScrollBar.Position then begin
      HorzScrollBar.Position:=X;
      Invalidate;
    end;
    if X>HorzScrollBar.Position+cx-2 then begin
      HorzScrollBar.Position:=X-(3*cx) div 4;
      Invalidate;
    end;
    if Y<VertScrollBar.Position then begin
      VertScrollBar.Position:=Y;
      Invalidate;
    end;
    if Y>VertScrollBar.Position+cy-2 then begin
      VertScrollBar.Position:=Y-cy+2;
      Invalidate;
    end;
  end;
  UpdateCaret;
end;

procedure TCustomSyntaxHighlightMemo.Select(PStart, PEnd: TPoint);
begin
  Select(Rect(PStart.X,PStart.Y,PEnd.X,PEnd.Y));
end;

procedure TCustomSyntaxHighlightMemo.Select(r: TRect);
var
  a:Integer;
  s:TRect;

  procedure Normalize(var X,Y:Integer);
  begin
    if Y>=Lines.Count then
      Y:=Lines.Count-1;
    if Y<0 then
      Y:=0;
    if X<0 then
      X:=0;
    if X>Length(Lines[Y]) then
      X:=Length(Lines[Y]);
  end;

begin
  s:=FSelData.Rect;
  with FSelData,r do begin
    Origin:=TopLeft;
    if Bottom>Top then begin
      X1:=Left;
      Y1:=Top;
      X2:=Right;
      Y2:=Bottom;
    end else begin
      X1:=Right;
      Y1:=Bottom;
      X2:=Left;
      Y2:=Top;
    end;
    Normalize(X1,Y1);
    Normalize(X2,Y2);
    if (Y1=Y2) and (X1>X2) then begin
      a:=X1;
      X1:=X2;
      X2:=a;
    end;
    if not CompareMem(@s,@FSelData.Rect,SizeOf(TRect)) then
      InvalidateLines(Min(s.Top,Y1),Max(s.Bottom,Y2));
  end;
end;

procedure TCustomSyntaxHighlightMemo.SelectAll;
begin
  Select(Point(0,0),Point(Length(FLines[FLines.Count-1]),FLines.Count-1));
  SelKind:=skLinear;
end;

procedure TCustomSyntaxHighlightMemo.SetCaretPos(const Value: TPoint);
begin
  ClearMarks;
  if (Value.X<>FCaretData.X) or (Value.Y<>FCaretData.Y) then begin
    {if Value.Y<>FCaretData.Y then}
      FormatTextLine(FCaretData.Y);
    FCaretData.Pos:=Value;
    if FCaretData.Y>=FLines.Count then
      FCaretData.Y:=FLines.Count-1;
    if FCaretData.X>FEditOptions.MaxLineSize then
      FCaretData.X:=FEditOptions.MaxLineSize;
    if FCaretData.Y<0 then
      FCaretData.Y:=0;
    if FCaretData.X<0 then
      FCaretData.X:=0;
    UpdateCaret;
    ScrollToCaret;
  end;
  EditStateChanged;
end;

procedure TCustomSyntaxHighlightMemo.SetCustomText(const Value: TStrings);
begin
  FCustomText.Assign(Value);
end;

procedure TCustomSyntaxHighlightMemo.SetEditOptions(
  const Value: TEditOptions);
begin
  FEditOptions.Assign(Value);
end;

procedure TCustomSyntaxHighlightMemo.SetHighlightData(Index: Integer;
  const Value: TCharClassHighlight);
begin
  FHighlightData.BlockHighlights[THighlightBlockType(Index)]:=Value;
  Invalidate;
end;

procedure TCustomSyntaxHighlightMemo.SetKeyShortCuts(
  const Value: TKeyShortCutCollection);
begin
  FShortCutData.PublicKeyShortCuts.Assign(Value);
end;

procedure TCustomSyntaxHighlightMemo.SetLines(const Value: TStrings);
begin
  FLines.Assign(Value);
end;

procedure TCustomSyntaxHighlightMemo.SetMouseDownShortCuts(
  const Value: TMouseShortCutCollection);
begin
  FShortCutData.PublicMouseDownShortCuts.Assign(Value);
end;

procedure TCustomSyntaxHighlightMemo.SetMouseMoveShortCuts(
  const Value: TMouseShortCutCollection);
begin
  FShortCutData.PublicMouseMoveShortCuts.Assign(Value);
end;

procedure TCustomSyntaxHighlightMemo.SetOnEditOptionsChanged(
  const Value: TNotifyEvent);
begin
  FOnEditOptionsChanged := Value;
end;

procedure TCustomSyntaxHighlightMemo.SetSelKind(
  const Value: TSelKind);
begin
  FSelData.Kind:=Value;
  InvalidateSelection;
end;

procedure TCustomSyntaxHighlightMemo.SetSelOrigin(const Value: TPoint);
begin
  FSelData.Origin:=Value;
end;

procedure TCustomSyntaxHighlightMemo.SetText(const Value: string);
begin
  FLines.Text:=Value;
end;

function TCustomSyntaxHighlightMemo.TabJumpPos(p: TPoint): TPoint;
var
  a,b:Integer;
begin
  with p do begin
    if Y>=FLines.Count then begin
      X:=0;
      Y:=FLines.Count-1;
    end;
    if Y<0 then begin
      X:=0;
      Y:=0;
    end;
    Result:=p;
    a:=Y-1;
    while (a>=0) and (Length(FLines[a])<X) do
      Dec(a);
    if a<0 then
      Inc(Result.X,FEditOptions.TabLength)
    else begin
      b:=X+1;
      while (b<=Length(FLines[a])) and (FLines[a][b]<>' ') do
        Inc(b);
      while (b<=Length(FLines[a])) and (FLines[a][b]=' ') do
        Inc(b);
      if b>Length(FLines[a]) then
        Result.X:=Max(Length(FLines[a]),X+FEditOptions.TabLength-(X mod FEditOptions.TabLength))
      else
        Result.X:=b-1;
    end;
  end;
end;


function TCustomSyntaxHighlightMemo.TextClientOrigin: TPoint;
begin
  with CharSize do
    if FEditOptions.GutterVisible then
      Result:=Point(FEditOptions.GutterMargin-cx*HorzScrollBar.Position,-cy*VertScrollBar.Position)
    else
      Result:=Point(-cx*HorzScrollBar.Position,-cy*VertScrollBar.Position);
end;

function TCustomSyntaxHighlightMemo.TextJumpPos(p: TPoint;
  ToRight: Boolean): TPoint;
var
  a:Integer;

  function FirstAlphaNumCharPos(LineID:Integer):Integer;
  var
    s:string;
  begin
    Result:=1;
    s:=FLines[LineID];
    while (Result<=Length(s)) and not IsCharAlphaNumeric(s[Result]) do
      Inc(Result);
    if Result>Length(s) then
      Result:=0;
  end;

begin
  with p do
    if ToRight then begin
      if Y<0 then
        Y:=0;
      if Y>=FLines.Count then begin
        Y:=FLines.Count-1;
        X:=Length(FLines[Y]);
      end;
      if X>=Length(FLines[Y]) then begin
        if Y=FLines.Count-1 then
          Result:=Point(Length(FLines[Y]),Y)
        else
          Result:=Point(FirstAlphaNumCharPos(Y+1)-1,Y+1)
      end else begin
        a:=1;
        while (X+a<Length(FLines[y])) and IsCharAlphaNumeric(FLines[y][X+a+1]) do
          Inc(a);
        while (X+a<Length(FLines[y])) and not IsCharAlphaNumeric(FLines[y][X+a+1]) do
          Inc(a);
        Result:=Point(X+a,Y);
      end;
    end else begin
      if Y<0 then begin
        Y:=0;
        X:=0;
      end;
      if Y>=FLines.Count then
        Y:=FLines.Count-1;
      if X>Length(FLines[Y]) then
        X:=Length(FLines[Y]);
      a:=FirstAlphaNumCharPos(y);
      if X<=a then begin
        if Y=0 then
          Result:=Point(a-1,0)
        else
          Result:=Point(Length(FLines[Y-1]),Y-1);
      end else begin
        a:=-1;
        while (X+a>=0) and not IsCharAlphaNumeric(FLines[y][X+a+1]) do
          Dec(a);
        while (X+a>=0) and IsCharAlphaNumeric(FLines[y][X+a+1]) do
          Dec(a);
        Result:=Point(X+a+1,Y);
      end;
    end;
end;

function TCustomSyntaxHighlightMemo.TextToClient(p: TPoint): TPoint;
var
  q:TPoint;
begin
  q:=TextClientOrigin;
  with CharSize do begin
    Result.X:=cx*p.X+q.X;
    Result.Y:=cy*p.Y+q.Y;
  end;
end;

function TCustomSyntaxHighlightMemo.TotalTextSize: TSize;
begin
  Result.cx:=FEditOptions.MaxLineSize-VisibleTextSize.cx;
  Result.cy:=FLines.Count;
end;

procedure TCustomSyntaxHighlightMemo.Undo;
begin
  FEditHistory.Undo;
end;

procedure TCustomSyntaxHighlightMemo.UnregisterEditStateListener(
  Listener: IEditStateListener);
begin
  FEditStateListeners.Remove(Listener.GetComponent);
end;

procedure TCustomSyntaxHighlightMemo.UpdateCaret;
var
  w,h:Integer;
begin
  with TextToClient(CaretPos) do begin
    if (Focused and ((X>=FEditOptions.FGutterMargin) or (not FEditOptions.GutterVisible))) xor FCaretData.Created then begin
      if not FCaretData.Created then begin
        if FEditOptions.UseSystemCaretWidth then
          w:=0
        else
          w:=FEditOptions.CaretWidth;
        if FEditOptions.UseSystemCaretHeight then
          h:=CharSize.cy
        else
          h:=FEditOptions.CaretHeight;
        CreateCaret(Handle,0,w,h);
        ShowCaret(Handle);
      end else
        DestroyCaret;
      FCaretData.Created:=not FCaretData.Created;
    end;
    if FCaretData.Created then
      Windows.SetCaretPos(X,Y);
  end;
end;

procedure TCustomSyntaxHighlightMemo.UpdateScrollBars;
begin
  with VisibleTextSize do begin
    VertScrollBar.Range:=TotalTextSize.cy+cy;
    VertScrollBar.PageSize:=2+cy;
    HorzScrollBar.Range:=TotalTextSize.cx+cx+2;
    HorzScrollBar.PageSize:=2+cx;
  end;
end;

function TCustomSyntaxHighlightMemo.VisibleTextOrigin: TPoint;
begin
  Result.X:=HorzScrollBar.Position;
  Result.Y:=VertScrollBar.Position;
end;

function TCustomSyntaxHighlightMemo.VisibleTextRect: TRect;
begin
  with VisibleTextOrigin,VisibleTextSize,Result do
    Result:=Rect(X,Y,X+cx,Y+cy);
end;

function TCustomSyntaxHighlightMemo.VisibleTextSize: TSize;
begin
  with CharSize do begin
    if FEditOptions.GutterVisible then
      Result.cx:=Ceil((ClientWidth-FEditOptions.GutterMargin)/cx)
    else
      Result.cx:=Ceil(ClientWidth/cx);
    Result.cy:=Ceil(ClientHeight/cy);
  end;
end;

procedure TCustomSyntaxHighlightMemo.WMDestroy(var Message: TMessage);
begin
  inherited;
end;

procedure TCustomSyntaxHighlightMemo.WMGetDLGCode(var Message: TMessage);
begin
  Message.Result:=DLGC_WANTARROWS or DLGC_WANTTAB;
end;

procedure TCustomSyntaxHighlightMemo.WMKillFocus(var Message: TMessage);
begin
  inherited;
  FTrackingData.JumpHighlight:=False;
  UpdateCaret;
end;

procedure TCustomSyntaxHighlightMemo.WMMouseHover(var Message: TMessage);
var
  s:TShiftState;
  X,Y:Integer;
  c:TCharClass;
  p:TPoint;
begin
  with Message,FTrackingData do begin
    s:=[];
    if wParam and MK_CONTROL=MK_CONTROL then
      Include(s,ssCtrl);
    if wParam and MK_LBUTTON=MK_LBUTTON then
      Include(s,ssLeft);
    if wParam and MK_MBUTTON=MK_MBUTTON then
      Include(s,ssMiddle);
    if wParam and MK_RBUTTON=MK_RBUTTON then
      Include(s,ssRight);
    if wParam and MK_SHIFT=MK_SHIFT then
      Include(s,ssShift);
    x:=SmallInt(lParam);
    y:=SMallInt(lParam shr 16);
    if s=[] then begin
      p:=ClientToText(Point(X,Y));
      c:=FThread.CharClass[p.Y,p.X];
      if FEditOptions.AllowSymbolHints and CharClassCanHint(c) and not (IsInSelection(p) and IsInSelection(Point(p.X+1,p.Y))) then begin
        Hint:=CharClassHint(c,CharClassText(p),p);
        Application.ActivateHint(ClientToScreen(Point(X,Y)));
      end else
        Hint:='';
    end else
      Hint:='';
    TrackMove:=True;
    MouseMove(s,X,Y);
    TrackMove:=False;
  end;
end;

procedure TCustomSyntaxHighlightMemo.WMPaint(var Message: TWMPaint);
begin
  ControlState:=ControlState+[csCustomPaint];
  inherited;
  ControlState:=ControlState-[csCustomPaint];
end;

procedure TCustomSyntaxHighlightMemo.WMSetCursor(var Message: TMessage);
var
  p,q:TPoint;
begin
  GetCursorPos(q);
  q:=ScreenToClient(q);
  p:=ClientToText(q);
  if p.X<0 then
    p.X:=0;
  if IsInSelection(p) and IsInSelection(Point(p.X+1,p.Y)) then begin
    if FTrackingData.DraggingText then
      SetCursor(Screen.Cursors[crDrag])
    else
      SetCursor(Screen.Cursors[crArrow]);
    Message.Result:=1;
  end else begin
    if FTrackingData.JumpHighlight then
      SetCursor(LoadCursor(0,IDC_HAND))
    else begin
      if (q.Y<=ClientHeight) and (q.X<=FEditOptions.GutterMargin) and FEditOptions.GutterVisible then
        SetCursor(LoadCursor(0,IDC_UPARROW))
      else
        inherited;
    end;
  end;
end;

procedure TCustomSyntaxHighlightMemo.WMSetFocus(var Message: TMessage);
begin
  inherited;
  UpdateCaret;
  EditStateChanged;
end;

procedure TCustomSyntaxHighlightMemo._SCKArrow(var Key: Word;
  Shift: TShiftState);
const
  T:array[VK_LEFT..VK_DOWN,0..1] of Integer=((-1,0),(0,-1),(1,0),(0,1));
var
  p:TPoint;
begin
  inherited;
  p:=CaretPos;
  if ssCtrl in Shift then
    case Key of
      VK_LEFT:CaretPos:=TextJumpPos(p,False);
      VK_RIGHT:CaretPos:=TextJumpPos(p,True);
    end
  else
    CaretPos:=Point(p.X+T[Key,0],p.Y+T[Key,1]);
  if ssShift in Shift then begin
    Select(FSelData.Origin,CaretPos);
    SelKind:=skLinear;
  end else begin
    SelOrigin:=CaretPos;
    if not FEditOptions.PersistentBlocks then
      ClearSelection;
  end;
end;

procedure TCustomSyntaxHighlightMemo._SCKClipBoard(var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    Ord('C'):Copy;
    Ord('X'):Cut;
    Ord('V'):Paste;
  end;
end;

procedure TCustomSyntaxHighlightMemo._SCKCtrl(var Key: Word;
  Shift: TShiftState);
begin
  case Chr(Key) of
    'A':SelectAll;
    'P':Customize;
    'Y':Redo;
    'Z':Undo;
  end;
end;

procedure TCustomSyntaxHighlightMemo._SCKDelete(var Key: Word;
  Shift: TShiftState);
var
  a,b,c,d:Integer;
  p:TPoint;
begin
  if SelEmpty or FEditOptions.PersistentBlocks then begin
    a:=0;
    if Shift=[] then
      case Key of
        VK_BACK:begin
          with CaretPos do
            if FEditOptions.AutoIndent and (X>0) and (IsBlank(System.Copy(FLines[Y],1,X)) and ((X>=Length(FLines[Y])) or (FLines[Y][X+1]<>' '))) then begin
              b:=Y-1;
              c:=-1;
              while (b>=0) and (c=-1) do begin
                for d:=1 to Min(X,Length(FLines[b])) do
                  if FLines[b][d]<>' ' then begin
                    c:=d-1;
                    Break;
                  end;
                Dec(b);
              end;
              if c=-1 then
                a:=-X
              else
                a:=c-X;
            end else
              a:=-1;
        end;
        VK_DELETE:a:=1;
      end
    else begin
      p:=TextJumpPos(CaretPos,Key=VK_DELETE);
      with CaretPos do
        if p.Y=Y then
          a:=p.X-X
        else begin
          if Key=VK_BACK then
            a:=-X
          else
            a:=p.X+1;
        end;
    end;
    DeleteTextAtCaret(a);
  end else
    DeleteSelection;
end;

procedure TCustomSyntaxHighlightMemo._SCKEnter(var Key: Word;
  Shift: TShiftState);
var
  a,b:Integer;
  s:string;
begin
  case FEditOptions.WriteMode of
    emInsert:begin
      AddTextAtCaret(#13);
      a:=CaretPos.Y-1;
      while (a>=0) and IsBlank(FLines[a]) do
        Dec(a);
      if a>=0 then begin
        b:=1;
        while FLines[a][b]=' ' do
          Inc(b);
        if b>1 then begin
          SetLength(s,b-1);
          for a:=1 to Length(s) do
            s[a]:=' ';
          AddTextAtCaret(s);
        end;
      end;
    end;
    emOverwrite:AddTextAtCaret(#13);//with CaretPos do CaretPos:=Point(X,Y+1);
  end;
end;

procedure TCustomSyntaxHighlightMemo._SCKEscape(var Key: Word;
  Shift: TShiftState);
begin
  if FTrackingData.DraggingText then
    FTrackingData.DraggingText:=False;
  ReleaseCapture;
end;

procedure TCustomSyntaxHighlightMemo._SCKExtremi(var Key: Word;
  Shift: TShiftState);
begin
  if ssCtrl in Shift then
    case Key of
      VK_END:CaretPos:=Point(Length(FLines[FLines.Count-1]),FLines.Count-1);
      VK_HOME:CaretPos:=Point(0,0);
    end
  else
    with CaretPos do
      case Key of
        VK_END:CaretPos:=Point(Length(FLines[Y]),Y);
        VK_HOME:CaretPos:=Point(0,Y);
      end;
  if ssShift in Shift then begin
    Select(FSelData.Origin,CaretPos);
    SelKind:=skLinear;
  end else begin
    SelOrigin:=CaretPos;
    if not FEditOptions.PersistentBlocks then
      ClearSelection;
  end;
end;

procedure TCustomSyntaxHighlightMemo._SCKInsert(var Key: Word;
  Shift: TShiftState);
begin
  if Key=VK_INSERT then
    FEditOptions.WriteMode:=TEditMode(1-Integer(FEditOptions.WriteMode))
  else
    EditStateChanged;
end;

procedure TCustomSyntaxHighlightMemo._SCKMenu(var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_F1:JumpAtCaret;
  end;
end;

procedure TCustomSyntaxHighlightMemo._SCKScroll(var Key: Word;
  Shift: TShiftState);
begin
  if ssCtrl in Shift then
    with VisibleTextRect,CaretPos do
      case Key of
        VK_PRIOR:CaretPos:=Point(X,Top);
        VK_NEXT:CaretPos:=Point(X,Bottom-2);
      end
  else
    with CaretPos,VisibleTextSize do
      case Key of
        VK_PRIOR:CaretPos:=Point(X,Y-cy+1);
        VK_NEXT:CaretPos:=Point(X,Y+cy-1);
      end;
  if ssShift in Shift then begin
    Select(FSelData.Origin,CaretPos);
    SelKind:=skLinear;
  end else begin
    SelOrigin:=CaretPos;
    if not FEditOptions.PersistentBlocks then
      ClearSelection;
  end;
end;

procedure TCustomSyntaxHighlightMemo._SCKTab(var Key: Word;
  Shift: TShiftState);
var
  s:string;
  p:TPoint;
  a:Integer;
begin
  if Shift=[] then begin
    p:=TabJumpPos(CaretPos);
    case FEditOptions.WriteMode of
      emInsert:begin
        SetLength(s,p.X-CaretPos.X);
        for a:=1 to Length(s) do
          s[a]:=' ';
        AddTextAtCaret(s);
      end;
      emOverwrite:CaretPos:=p;
    end;
    SelOrigin:=CaretPos;
  end else begin

  end;
end;

procedure TCustomSyntaxHighlightMemo._SCMD(X, Y: Integer;
  Shift: TShiftState);
var
  p:TPoint;
begin
  if not Focused then
    SetFocus;
  FTrackingData.LastClickPos:=Point(X,Y);
  FTrackingData.LastClickTick:=GetTickCount;
  p:=ClientToText(Point(X,Y));
  if p.X<0 then
    p.X:=0;
  if IsInSelection(p) and IsInSelection(Point(p.X+1,p.Y)) then begin
    FTrackingData.DraggingText:=True;
    SetCursor(Screen.Cursors[crDrag]);
  end else begin
    if Shift<=[ssLeft,ssAlt] then
      SelOrigin:=p;
    CaretPos:=p;
    if Shift=[ssAlt,ssLeft] then
      SelKind:=skRect;
    if Shift+[ssShift]=[ssLeft,ssShift] then begin
      SelKind:=skLinear;
      Select(SelOrigin,p);
    end;
  end;
end;

procedure TCustomSyntaxHighlightMemo._SCMDJump(X, Y: Integer;
  Shift: TShiftState);
begin
  ReleaseCapture;
  JumpAtPos(ClientToText(Point(X,Y)));
  FTrackingData.JumpHighlight:=False;
  Invalidate;
end;

procedure TCustomSyntaxHighlightMemo._SCMM(X, Y: Integer;
  Shift: TShiftState);
var
  p:TPoint;
begin
  p:=ClientToText(Point(X,Y));
  if FTrackingData.DraggingText then
    CaretPos:=p
  else begin
    if not FTrackingData.TrackMove then begin
      with VisibleTextRect do
        CaretPos:=Point(Max(Min(Right-2,p.X),Left),Max(Min(Bottom-2,p.Y),Top));
    end else
      CaretPos:=p;
    Select(SelOrigin,p);
  end;
end;

procedure TCustomSyntaxHighlightMemo._SCMMJump(X, Y: Integer;
  Shift: TShiftState);
var
  c:TCharClass;
  p,p1,p2:TPoint;
begin
  p:=ClientToText(Point(X,Y));
  c:=FThread.CharClass[p.Y,p.X];
  if FEditOptions.AllowCodeLinks and (CharClassCanJump(c) or FTrackingData.JumpHighlight) then begin
    if FTrackingData.JumpHighlight then
      InvalidateLine(FTrackingData.p1.Y);
    if CharClassCanJump(c) then begin
      CharClassText(p,p1,p2);
      FTrackingData.JumpHighlight:=True;
      FTrackingData.p1:=p1;
      FTrackingData.p2:=p2;
      InvalidateLine(p1.Y);
    end else
      FTrackingData.JumpHighlight:=False;
  end;
end;

procedure TCustomSyntaxHighlightMemo.SetOnEditStateChanged(
  const Value: TNotifyEvent);
begin
  FOnEditStateChanged := Value;
end;

procedure TCustomSyntaxHighlightMemo.SetModified(const Value: Boolean);
begin
  FModified := Value;
  if not FModified then
    FEditHistory.ClearModification;
  EditStateChanged;
end;

procedure TCustomSyntaxHighlightMemo.ClearUndoHistory;
begin
  FEditHistory.Clear;
  FEditHistory.FHistoryID:=-1;
end;

function TCustomSyntaxHighlightMemo.CanJumpAtCaret: Boolean;
begin
  Result:=CanJumpAtPos(CaretPos);
end;

function TCustomSyntaxHighlightMemo.CanJumpAtPos(p: TPoint): Boolean;
var
  c:TCharClass;
begin
  c:=FThread.CharClass[p.Y,p.X];
  if CharClassCanJump(c) then
    Result:=True
  else begin
    Dec(p.X);
    c:=FThread.CharClass[p.Y,p.X];
    Result:=CharClassCanJump(c);
  end;
end;

function TCustomSyntaxHighlightMemo.PosClass(p: TPoint): TCharClass;
var
  t:THighlightBlockType;
begin
  if IsInSelection(p) and IsInSelection(Point(p.X+1,p.Y)) then begin
    Result:=TCharClass(hbSelection);
    Exit;
  end;
  with FHighlightData do
    for t:=hbSearch to hbWarning do
      if p.Y=BlockPositions[t].Y then begin
        Result:=TCharClass(t);
        Exit;
      end;
  with FTrackingData do
    if (JumpHighlight) and (p.Y=p1.Y) and (p.X>=p1.X) and (p.X<=p2.X) then begin
      Result:=TCharClass(hbSymbolJump);
      Exit;
    end;
  Result:=FThread.CharClass[p.Y,p.X];
  if Result>0 then
    Result:=Result+TCharClass(hbWarning);
end;

{ TSyntaxHighlightMemo }

function TSyntaxHighlightMemo.CharClassCanHint(
  ClassID: TCharClass): Boolean;
begin
  Result:=(ClassId>0) and Assigned(FHighlighter) and FHighlighter.CharClassCanHint(ClassID-1);
end;

function TSyntaxHighlightMemo.CharClassCanJump(
  ClassID: TCharClass): Boolean;
begin
  Result:=(ClassID>0) and Assigned(FHighlighter) and FHighlighter.CharClassCanJump(ClassID-1);;
end;

function TSyntaxHighlightMemo.CharClassHighlight(
  ClassID: TCharClass): TCharClassHighlight;
begin
  if (ClassID=0) or not Assigned(FHighlighter) then
    Result:=FHighlightData.BlockHighlights[hbWhiteSpace]
  else
    with FHighlighter.CharClassHighlight[ClassID-1] do begin
      Result.BackgroundColor:=BackgroundColor;
      Result.FontColor:=FontColor;
      Result.FontStyle:=FontStyle;
    end;
end;

function TSyntaxHighlightMemo.CharClassHint(ClassID: TCharClass;
  Text: string; Pos: TPoint): string;
begin
  if (ClassID=0) or not Assigned(FHighlighter) then
    Result:=''
  else
    Result:=FHighlighter.CharClassHint(ClassID-1,Text,Pos);
end;

procedure TSyntaxHighlightMemo.CharClassJump(ClassID: TCharClass;
  Text: string; Pos: TPoint);
begin
  if (ClassID>0) and Assigned(FHighlighter) then
    FHighlighter.CharClassJump(ClassID-1,Text,Pos);
end;

procedure TSyntaxHighlightMemo.ContentModified;
begin
  inherited;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

destructor TSyntaxHighlightMemo.Destroy;
begin
  Highlighter:=nil;
  inherited;
end;

function TSyntaxHighlightMemo.GetHighlightClassCount: Integer;
begin
  Result:=8;
  if Assigned(FHighlighter) then
    Inc(Result,FHighlighter.HighlightClassCount);
end;

function TSyntaxHighlightMemo.GetHighlightClassName(
  Index: Integer): string;
begin
  if Index<8 then
    Result:=inherited GetHighlightClassName(Index)
  else
    Result:=FHighlighter.HighlightClassName[Index-8];
end;

function TSyntaxHighlightMemo.GetHighlightData(
  Index: Integer): TCharClassHighlight;
begin
  if Index<8 then
    Result:=inherited GetHighlightData(Index)
  else 
    Result:=FHighlighter.HighlightData[Index-8];
end;

procedure TSyntaxHighlightMemo.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if AComponent=FHighlighter then
    FHighlighter:=nil;
  inherited;
end;

procedure TSyntaxHighlightMemo.SetHighlightData(Index: Integer;
  const Value: TCharClassHighlight);
begin
  if Index<8 then
    inherited SetHighlightData(Index,Value)
  else
    FHighlighter.HighlightData[Index-8]:=Value;
  Invalidate;
end;

procedure TSyntaxHighlightMemo.SetHighlighter(
  const Value: TCustomSyntaxHighlighter);
begin
  FHighlighter := Value;
  if Assigned(FHighlighter) then
    FHighlighter.FreeNotification(Self);
end;

procedure TSyntaxHighlightMemo.SetOnChange(const Value: TNotifyEvent);
begin
  FOnChange := Value;
end;

procedure TSyntaxHighlightMemo.TokenizeLine(const LineClass: TLineClass;
  const TextBuffer: PChar; const TextLength: Integer;
  CharClass: PCharClassArray);
begin
  if Assigned(FHighlighter) then
    FHighlighter.TokenizeLine(LineClass,TextBuffer,TextLength,CharClass);
end;

procedure TSyntaxHighlightMemo.TokenizeLineClass(
  const LastLineClass: TLineClass; var NewLineClass: TLineClass;
  const TextBuffer: PChar; const TextLength: Integer);
begin
  if Assigned(FHighlighter) then
    FHighlighter.TokenizeLineClass(LastLineClass,NewLineClass,TextBuffer,TextLength);
end;

end.
