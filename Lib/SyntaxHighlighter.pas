unit SyntaxHighlighter;

interface

uses
  SysUtils,Windows,Classes,Graphics;

type
  TLineClass=Byte;

  TCharClass=Byte;
  TCharClassArray=array[0..$FFFF] of TCharClass;
  PCharClassArray=^TCharClassArray;

  TCharClassHighlight=record
    BackgroundColor,FontColor:TColor;
    FontStyle:TFontStyles;
  end;

  TCustomSyntaxHighlighter=class(TComponent)
  private
  protected
    function GetCharClassHighlight(Index: TCharClass): TCharClassHighlight;virtual;abstract;

    function GetHighlightClassCount: Integer;virtual;
    function GetHighlightClassHasText(Index: Integer): Boolean;virtual;
    function GetHighlightClassName(Index: Integer): string;virtual;
    function GetHighlightData(Index: Integer): TCharClassHighlight;virtual;
    procedure SetHighlightData(Index: Integer; const Value: TCharClassHighlight);virtual;
  public
    procedure TokenizeLineClass(const LastLineClass:TLineClass;var NewLineClass:TLineClass;const TextBuffer:PChar;const TextLength:Integer);virtual;abstract;
    procedure TokenizeLine(const LineClass:TLineClass;const TextBuffer:PChar;const TextLength:Integer;CharClass:PCharClassArray);virtual;abstract;

    property CharClassHighlight[Index:TCharClass]:TCharClassHighlight read GetCharClassHighlight;
    function CharClassCanHint(ClassID:TCharClass):Boolean;virtual;
    function CharClassHint(ClassID:TCharClass;Text:string;Pos:TPoint):string;virtual;
    function CharClassCanJump(ClassID:TCharClass):Boolean;virtual;
    procedure CharClassJump(ClassID:TCharClass;Text:string;Pos:TPoint);virtual;

    property HighlightClassCount:Integer read GetHighlightClassCount;
    property HighlightClassName[Index:Integer]:string read GetHighlightClassName;
    property HighlightClassHasText[Index:Integer]:Boolean read GetHighlightClassHasText;
    property HighlightData[Index:Integer]:TCharClassHighlight read GetHighlightData write SetHighlightData;
  end;

  TTokenizeLineClassEvent=procedure(Sender:TObject;const LastLineClass:TLineClass;var NewLineClass:TLineClass;const TextBuffer:PChar;const TextLength:Integer) of object;
  TTokenizeLineEvent=procedure(Sender:TObject;const LineClass:TLineClass;const TextBuffer:PChar;const TextLength:Integer;CharClass:PCharClassArray) of object;

  TGetHintEvent=procedure(Sender:TObject;var Hint:string;const Text:string;const Pos:TPoint) of object;
  TJumpEvent=procedure(Sender:TObject;const Text:string;const Pos:TPoint) of object;

  TCharClassHighlightItem=class(TCollectionItem)
  private
    FBackgroundColor: TColor;
    FFontColor: TColor;
    FFontStyle: TFontStyles;
    FCaption: string;
    FOnGetHint: TGetHintEvent;
    FOnJump: TJumpEvent;
    procedure SetBackgroundColor(const Value: TColor);
    procedure SetFontColor(const Value: TColor);
    procedure SetFontStyle(const Value: TFontStyles);
    procedure SetCaption(const Value: string);
    procedure SetOnGetHint(const Value: TGetHintEvent);
    procedure SetOnJump(const Value: TJumpEvent);
  public
    function GetNamePath:string;override;
    constructor Create(Collection:TCollection);override;
  published
    property Caption:string read FCaption write SetCaption;
    property BackgroundColor:TColor read FBackgroundColor write SetBackgroundColor;
    property FontColor:TColor read FFontColor write SetFontColor;
    property FontStyle:TFontStyles read FFontStyle write SetFontStyle;

    property OnGetHint:TGetHintEvent read FOnGetHint write SetOnGetHint;
    property OnJump:TJumpEvent read FOnJump write SetOnJump;
  end;

  TCharClassHighlightCollection=class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TCharClassHighlightItem;
    procedure SetItem(Index: Integer;
      const Value: TCharClassHighlightItem);
  protected
  public
    property Items[Index:Integer]:TCharClassHighlightItem read GetItem write SetItem;default;
  end;

  TSyntaxHighlighter=class(TCustomSyntaxHighlighter)
  private
    FOnTokenizeLineClass: TTokenizeLineClassEvent;
    FOnTokenizeLine: TTokenizeLineEvent;
    FCharClassHighlights: TCharClassHighlightCollection;

    procedure SetOnTokenizeLine(const Value: TTokenizeLineEvent);
    procedure SetOnTokenizeLineClass(const Value: TTokenizeLineClassEvent);
    function GetCharClassHighlights: TCharClassHighlightCollection;
    procedure SetCharClassHighlights(
      const Value: TCharClassHighlightCollection);
  protected
    function GetCharClassHighlight(Index: TCharClass): TCharClassHighlight;override;

    function GetHighlightClassCount: Integer;override;
    function GetHighlightClassHasText(Index: Integer): Boolean;override;
    function GetHighlightClassName(Index: Integer): string;override;
    function GetHighlightData(Index: Integer): TCharClassHighlight;override;
    procedure SetHighlightData(Index: Integer; const Value: TCharClassHighlight);override;
  public
    constructor Create(AOwner:TComponent);override;

    function CharClassCanHint(ClassID:TCharClass):Boolean;override;
    function CharClassHint(ClassID:TCharClass;Text:string;Pos:TPoint):string;override;
    function CharClassCanJump(ClassID:TCharClass):Boolean;override;
    procedure CharClassJump(ClassID:TCharClass;Text:string;Pos:TPoint);override;

    destructor Destroy;override;
  published
    procedure TokenizeLineClass(const LastLineClass:TLineClass;var NewLineClass:TLineClass;const TextBuffer:PChar;const TextLength:Integer);override;
    procedure TokenizeLine(const LineClass:TLineClass;const TextBuffer:PChar;const TextLength:Integer;CharClass:PCharClassArray);override;

    property OnTokenizeLineClass:TTokenizeLineClassEvent read FOnTokenizeLineClass write SetOnTokenizeLineClass;
    property OnTokenizeLine:TTokenizeLineEvent read FOnTokenizeLine write SetOnTokenizeLine;

    property CharClassHighlights:TCharClassHighlightCollection read GetCharClassHighlights write SetCharClassHighlights;
  end;

implementation

{ TCustomSyntaxHighlighter }

function TCustomSyntaxHighlighter.CharClassCanHint(
  ClassID: TCharClass): Boolean;
begin
  Result:=False;
end;

function TCustomSyntaxHighlighter.CharClassCanJump(
  ClassID: TCharClass): Boolean;
begin
  Result:=False;
end;

function TCustomSyntaxHighlighter.CharClassHint(ClassID: TCharClass;
  Text: string; Pos: TPoint): string;
begin
  Result:='';
end;

procedure TCustomSyntaxHighlighter.CharClassJump(ClassID: TCharClass;
  Text: string; Pos: TPoint);
begin

end;

function TCustomSyntaxHighlighter.GetHighlightClassCount: Integer;
begin
  Result:=0;
end;

function TCustomSyntaxHighlighter.GetHighlightClassHasText(
  Index: Integer): Boolean;
begin
  Result:=False;
end;

function TCustomSyntaxHighlighter.GetHighlightClassName(
  Index: Integer): string;
begin
  Result:='';
end;

function TCustomSyntaxHighlighter.GetHighlightData(
  Index: Integer): TCharClassHighlight;
begin
  with Result do begin
    BackgroundColor:=0;
    FontColor:=0;
    FontStyle:=[];
  end;
end;

procedure TCustomSyntaxHighlighter.SetHighlightData(Index: Integer;
  const Value: TCharClassHighlight);
begin

end;

{ TCharClassHighlightItem }

constructor TCharClassHighlightItem.Create(Collection: TCollection);
begin
  inherited;
  FBackgroundColor:=clWhite;
  FFontColor:=0;
  FFontStyle:=[];
end;

function TCharClassHighlightItem.GetNamePath: string;
begin
  if FCaption<>'' then
    Result:=Collection.GetNamePath+'_'+FCaption
  else
    Result:=inherited GetNamePath;
end;

procedure TCharClassHighlightItem.SetBackgroundColor(const Value: TColor);
begin
  FBackgroundColor := Value;
end;

procedure TCharClassHighlightItem.SetCaption(const Value: string);
begin
  FCaption := Value;
end;

procedure TCharClassHighlightItem.SetFontColor(const Value: TColor);
begin
  FFontColor := Value;
end;

procedure TCharClassHighlightItem.SetFontStyle(const Value: TFontStyles);
begin
  FFontStyle := Value;
end;

procedure TCharClassHighlightItem.SetOnGetHint(const Value: TGetHintEvent);
begin
  FOnGetHint := Value;
end;

procedure TCharClassHighlightItem.SetOnJump(const Value: TJumpEvent);
begin
  FOnJump := Value;
end;

{ TCharClassHighlightCollection }

function TCharClassHighlightCollection.GetItem(
  Index: Integer): TCharClassHighlightItem;
begin
  Result:=(inherited GetItem(Index)) as TCharClassHighlightItem;
end;

procedure TCharClassHighlightCollection.SetItem(Index: Integer;
  const Value: TCharClassHighlightItem);
begin
  inherited SetItem(Index,Value);
end;

{ TSyntaxHighlighter }

function TSyntaxHighlighter.CharClassCanHint(ClassID: TCharClass): Boolean;
begin
  Result:=Assigned(FCharClassHighlights[ClassID].OnGetHint);
end;

function TSyntaxHighlighter.CharClassCanJump(ClassID: TCharClass): Boolean;
begin
  Result:=Assigned(FCharClassHighlights[ClassID].OnJump);
end;

function TSyntaxHighlighter.CharClassHint(ClassID: TCharClass;
  Text: string; Pos: TPoint): string;
begin
  Result:='';
  if Assigned(FCharClassHighlights[ClassID].OnGetHint) then
    FCharClassHighlights[ClassID].OnGetHint(FCharClassHighlights[ClassID],Result,Text,Pos);
end;

procedure TSyntaxHighlighter.CharClassJump(ClassID: TCharClass;
  Text: string; Pos: TPoint);
begin
  if Assigned(FCharClassHighlights[ClassID].OnJump) then
    FCharClassHighlights[ClassID].OnJump(FCharClassHighlights[ClassID],Text,Pos);
end;

constructor TSyntaxHighlighter.Create(AOwner: TComponent);
begin
  inherited;
  FCharClassHighlights:=TCharClassHighlightCollection.Create(Self,TCharClassHighlightItem);
end;

destructor TSyntaxHighlighter.Destroy;
begin
  FCharClassHighlights.Destroy;
  inherited;
end;

function TSyntaxHighlighter.GetCharClassHighlight(
  Index: TCharClass): TCharClassHighlight;
begin
  with FCharClassHighlights[Index],Result do begin
    BackgroundColor:=FBackgroundColor;
    FontColor:=FFontColor;
    FontStyle:=FFontStyle;
  end;
end;

function TSyntaxHighlighter.GetCharClassHighlights: TCharClassHighlightCollection;
begin
  Result:=FCharClassHighlights;
end;

function TSyntaxHighlighter.GetHighlightClassCount: Integer;
begin
  Result:=FCharClassHighlights.Count;
end;

function TSyntaxHighlighter.GetHighlightClassHasText(
  Index: Integer): Boolean;
begin
  Result:=True;
end;

function TSyntaxHighlighter.GetHighlightClassName(Index: Integer): string;
begin
  Result:=FCharClassHighlights[Index].FCaption;
end;

function TSyntaxHighlighter.GetHighlightData(
  Index: Integer): TCharClassHighlight;
begin
  with FCharClassHighlights[Index],Result do begin
    BackgroundColor:=FBackgroundColor;
    FontColor:=FFontColor;
    FontStyle:=FFontStyle;
  end;
end;

procedure TSyntaxHighlighter.SetCharClassHighlights(
  const Value: TCharClassHighlightCollection);
begin
  FCharClassHighlights.Assign(Value);
end;

procedure TSyntaxHighlighter.SetHighlightData(Index: Integer;
  const Value: TCharClassHighlight);
begin
  with FCharClassHighlights[Index],Value do begin
    FBackgroundColor:=BackgroundColor;
    FFontColor:=FontColor;
    FFontStyle:=FontStyle;
  end;
end;

procedure TSyntaxHighlighter.SetOnTokenizeLine(
  const Value: TTokenizeLineEvent);
begin
  FOnTokenizeLine := Value;
end;

procedure TSyntaxHighlighter.SetOnTokenizeLineClass(
  const Value: TTokenizeLineClassEvent);
begin
  FOnTokenizeLineClass := Value;
end;

procedure TSyntaxHighlighter.TokenizeLine(const LineClass: TLineClass;
  const TextBuffer: PChar; const TextLength: Integer;
  CharClass: PCharClassArray);
begin
  if Assigned(FOnTokenizeLine) then
    FOnTokenizeLine(Self,LineClass,TextBuffer,TextLength,CharClass);
end;

procedure TSyntaxHighlighter.TokenizeLineClass(
  const LastLineClass: TLineClass; var NewLineClass: TLineClass;
  const TextBuffer: PChar; const TextLength: Integer);
begin
  if Assigned(FOnTokenizeLineClass) then
    FOnTokenizeLineClass(Self,LastLineClass,NewLineClass,TextBuffer,TextLength);
end;

end.
