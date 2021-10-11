unit CompileErrorManager;

interface

uses
  SysUtils,Windows,Classes,StdCtrls,COW_RunTime,Graphics;

type
  ISourceEditor=interface(IInterfaceComponentReference)
    ['{70D607DE-2DBA-40A9-8186-B7FD1F9D5ED1}']
    procedure MarkCompileError(CharOffset,CharLength:Integer);
    procedure MarkExecuteError(CharOffset,CharLength:Integer);
    procedure MarkWarning(CharOffset,CharLength:Integer);

    function GetCode:string;
  end;

  TErrorType=(etParse,etCompile,etWarning);

  TErrorData=record
    ErrorType:TErrorType;
    CharOffset,CharLength:Integer;
    Message:string;
  end;

  PErrorData=^TErrorData;

  TCompileErrorManager=class(TCustomListBox,IParserExceptionHandler,IExecuterExceptionHandler)
  private
    FSourceEditor: ISourceEditor;
    procedure SetSourceEditor(const Value: ISourceEditor);
    function GetError(Index: Integer): TErrorData;
    function GetErrorCount: Integer;
  protected
    procedure Notification(AComponent:TComponent;Operation:TOperation);override;

    procedure RaiseException(SoftMode:Boolean;AParser:IParser;ALexer:ILexer;ABuffer:ICharBuffer;AFound:Word;AExpected:array of Word;DefError:string);overload;
    function RaiseException(ExceptionObject:TObject;CharOffset:Cardinal):Boolean;overload;
    function RaiseUnknownException(CharOffset:Cardinal):Boolean;

    procedure BeforeCompile;
    procedure AfterCompile;

    procedure MeasureItem(Index:Integer;var Height:Integer);override;
    procedure DrawItem(Index:Integer;Rect:TRect;State:TOwnerDrawState);override;

    procedure DblClick;override;

    procedure DestroyWND;override;
  public
    constructor Create(AOwner:TComponent);override;

    procedure AddError(ErrorType:TErrorType;CharOffset,CharLength:Integer;Message:string);

    procedure Clear;override;

    property ErrorCount:Integer read GetErrorCount;
    property Error[Index:Integer]:TErrorData read GetError;

    destructor Destroy;override;
  published
    property SourceEditor:ISourceEditor read FSourceEditor write SetSourceEditor;

    property Align;
    property BevelKind;
    property BorderStyle;
    property Anchors;       
  end;

implementation

{ TCompileErrorManager }

procedure TCompileErrorManager.AddError(ErrorType: TErrorType;
  CharOffset, CharLength: Integer; Message: string);
var
  p:PErrorData;
begin
  New(p);
  p.ErrorType:=ErrorType;
  p.CharOffset:=CharOffset;
  p.CharLength:=CharLength;
  p.Message:=Message;
  Items.AddObject(Message,TObject(p));
  if Assigned(FSourceEditor) then
    case ErrorType of
      etParse:FSourceEditor.MarkCompileError(CharOffset,CharLength);
      etCompile:FSourceEditor.MarkExecuteError(CharOffset,CharLength);
      etWarning:FSourceEditor.MarkWarning(CharOffset,CharLength);
    end;
end;

procedure TCompileErrorManager.AfterCompile;
begin

end;

procedure TCompileErrorManager.BeforeCompile;
begin
  while Items.Count>0 do begin
    Dispose(PErrorData(Items.Objects[0]));
    Items.Delete(0);
  end;
end;

procedure TCompileErrorManager.Clear;
var
  a:Integer;
begin
  for a:=0 to Items.Count-1 do
    Dispose(PErrorData(Items.Objects[a]));
  inherited;
end;

constructor TCompileErrorManager.Create(AOwner: TComponent);
begin
  inherited;
  Style:=lbOwnerDrawVariable;
  Color:=0;
end;

procedure TCompileErrorManager.DblClick;
begin
  inherited;
  if (ItemIndex>-1) and (ItemIndex<Items.Count) and Assigned(FSourceEditor) then
    with PErrorData(Items.Objects[ItemIndex])^ do
      case ErrorType of
        etParse:FSourceEditor.MarkCompileError(CharOffset,CharLength);
        etCompile:FSourceEditor.MarkExecuteError(CharOffset,CharLength);
        etWarning:FSourceEditor.MarkWarning(CharOffset,CharLength);
      end;
end;

destructor TCompileErrorManager.Destroy;
begin
  FSourceEditor:=nil;
  inherited;
end;

procedure TCompileErrorManager.DestroyWND;
begin
  Clear;
  inherited;
end;

procedure TCompileErrorManager.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
var
  a:Integer;
  s:string;
begin
  with Canvas,PErrorData(Items.Objects[Index])^ do begin
    case ErrorType of
      etParse,etCompile:begin
        Brush.Color:=RGB(150,0,0);
        Font.Color:=clWhite;
      end;
      etWarning:begin
        Brush.Color:=RGB(150,100,0);
        Font.Color:=clWhite;
      end;
    end;
    if Index=ItemIndex then begin
      case ErrorType of
        etParse,etCompile:Pen.Color:=RGB(255,100,100);
        etWarning:Pen.Color:=RGB(255,150,100);
      end;
      Rectangle(Rect);
    end else
      FillRect(Rect);
    Font.Name:=Self.Font.Name;
    Font.Size:=Self.Font.Size;
    Font.Style:=[fsBold];
    case ErrorType of
      etParse:s:='Syntax error';
      etCompile:s:='Fatal error';
      etWarning:s:='Warning';
    end;
    s:=s+' : ';
    TextOut(Rect.Left+2,Rect.Top+2,s);
    a:=TextWidth(s);
    Font.Style:=[];
    TextOut(Rect.Left+a+2,Rect.Top+2,Message);
    if Focused and (Index=ItemIndex) then
      DrawFocusRect(Rect);
  end;
end;

function TCompileErrorManager.GetError(Index: Integer): TErrorData;
begin
  Result:=PErrorData(Items.Objects[Index])^;
end;

function TCompileErrorManager.GetErrorCount: Integer;
begin
  Result:=Items.Count;
end;

procedure TCompileErrorManager.MeasureItem(Index: Integer;
  var Height: Integer);
begin
  Height:=2*Abs(Font.Size)+2;
end;

procedure TCompileErrorManager.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if Assigned(FSourceEditor) and (AComponent=FSourceEditor.GetComponent) then
    FSourceEditor:=nil;
  inherited;
end;

procedure TCompileErrorManager.RaiseException(SoftMode: Boolean;
  AParser: IParser; ALexer: ILexer; ABuffer: ICharBuffer; AFound: Word;
  AExpected: array of Word; DefError: string);
begin
  AddError(etParse,ALexer.LastLexerPos,ALexer.LexerPos-ALexer.LastLexerPos,DefError);
  if not SoftMode then
    raise Exception.Create(DefError);
end;

function TCompileErrorManager.RaiseException(ExceptionObject: TObject;
  CharOffset: Cardinal): Boolean;
begin
  if Assigned(ExceptionObject) and (ExceptionObject is Exception) then
    AddError(etCompile,CharOffset,0,(ExceptionObject as Exception).Message)
  else
    AddError(etCompile,CharOffset,0,'Unknown error');
  Result:=False;
end;

function TCompileErrorManager.RaiseUnknownException(
  CharOffset: Cardinal): Boolean;
begin
  AddError(etCompile,CharOffset,0,'Unknown error');
  Result:=False;
end;

procedure TCompileErrorManager.SetSourceEditor(const Value: ISourceEditor);
begin
  FSourceEditor := Value;
  if Assigned(FSourceEditor) then
    FSourceEditor.GetComponent.FreeNotification(Self);
end;

end.
