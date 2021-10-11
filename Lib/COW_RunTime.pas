{$WEAKPACKAGEUNIT ON}

unit COW_RunTime;

interface

uses
  SysUtils,Windows;

type

IReference=interface
  ['{A607ADFF-E4C7-47DA-A12E-324262C3128D}']
  function GetReference:TObject;
  property Reference:TObject read GetReference;
end;

TTableItemType=(titShift,titReduce,titGoto,titError,titAccept);
TTableItem=packed record
  case ItemType:TTableItemType of
    titShift,titReduce,titGoto:(Value:Word);
end;

TTableItems=array[0..65535] of TTableItem;
PTableItems=^TTableItems;

TCharMap=array[#0..#255] of Word;

TLexerTable=packed record
  Width,Height,EOFId:Word;
  TableItems:PTableItems;
  CharMap:TCharMap;
  Stored:Boolean;
end;

TLexerTables=array[0..65535] of TLexerTable;
PLexerTables=^TLexerTables;

TLexerLaw=packed record
  ChildsCount,ID:Word;
end;

TLexerLaws=array[0..65535] of TLexerLaw;
PLexerLaws=^TLexerLaws;

TLexerDefinition=packed record
  LexerTables:PLexerTables;
  LexerTablesCount:Cardinal;
  LexerEOFId:Word;
  LexerLaws:PLexerLaws;
  LexerLawsCount:Cardinal;
  StaticResource:Boolean;
end;

TLexerStackItem=packed record
  State,Node:Word;
end;

TLexerStackItems=array[0..65535] of TLexerStackItem;
PLexerStackItems=^TLexerStackItems;

TLexerStack=packed record
  LexerStackItems:PLexerStackItems;
  LexerStackItemsCount:Word;
end;

PParserNode=^TParserNode;
TParserNode=packed record
  CharOffset,CharLength:Cardinal;
  ID:Word;
  Count:Byte;
  Parent:PParserNode;
  case IsLeaf:Boolean of
    True:(Value:PString);
    False:(SubNodes:PParserNode);
end;

TParserStackItem=packed record
  State:Word;
  Node:TParserNode;
end;

TParserStackItems=array[0..65535] of TParserStackItem;
PParserStackItems=^TParserStackItems;

TParserStack=packed record
  ParserStackItems:PParserStackItems;
  ParserStackItemsCount:Cardinal;
end;

TBooleanArray=array[0..65535] of Boolean;
PBooleanArray=^TBooleanArray;

TParserLaw=packed record
  ChildsCount,ID,UsageCount:Word;
  Usage:PBooleanArray;
end;

TParserLaws=array[0..65535] of TParserLaw;
PParserLaws=^TParserLaws;

TStringArray=array[0..65535] of string;
PStringArray=^TStringArray;

TParserDefinition=packed record
  ParserTable:PTableItems;
  TableWidth,TableHeight:Word;
  ParserLaws:PParserLaws;
  ParserLawsCount:Word;
  TokenNames:PStringArray;
  TokenNamesCount:Word;
  StaticResource:Boolean;
end;

ICharBuffer=interface
  ['{F09FCE2E-5F44-46CA-875B-D8BEBF83EF44}']
  function GetChar(var Index:Cardinal;t:TLexerTable):Word;
  function GetLength:Cardinal;
  property Length:Cardinal read GetLength;
  procedure BufferHint(Length:Cardinal);
  function GetBufferText(Start,Length:Cardinal):string;
end;

ILexer=interface
  ['{F5C8EF66-CAEF-44B6-8C38-6EBF77A6713A}']
  function GetNewLexem:Word;
  procedure Init(ABuffer:ICharBuffer);
  function GetLexerPos:Cardinal;
  property LexerPos:Cardinal read GetLexerPos;
  function GetLastLexerPos:Cardinal;
  property LastLexerPos:Cardinal read GetLastLexerPos;
  function IsLexemStored(Lexem:Word):Boolean;
  function GetErrorID:Word;
  property ErrorID:Word read GetErrorID;
  function GetEOFID:Word;
  property EOFID:Word read GetEOFID;
end;

IParsed=interface
  ['{4E245B93-1671-42CD-94B0-71FB33E4BBD4}']
  procedure GoToRoot;
  function GetCount:Cardinal;
  property Count:Cardinal read GetCount;
  procedure GoUp(Index:Cardinal);
  procedure GoDown;
  function GetID:Word;
  property ID:Word read GetID;
  function GetTextStart:Cardinal;
  property TextStart:Cardinal read GetTextStart;
  function GetTextLength:Cardinal;
  property TextLength:Cardinal read GetTextLength;
  function GetText:string;
  property Text:string read GetText;
  function GetName:string;
  property Name:string read GetName;
end;

TProgressCallBackProc=procedure(Position:Cardinal);

IParser=interface;

IParserExceptionHandler=interface
  ['{68AE8989-DBE3-46D7-9019-27F5679A345E}']
  procedure RaiseException(SoftMode:Boolean;AParser:IParser;ALexer:ILexer;ABuffer:ICharBuffer;AFound:Word;AExpected:array of Word;DefError:string);
end;

IParser=interface
  ['{2246D9B5-3CD2-4E8A-BB18-577EEDE7E43D}']
  function Parse(ABuffer:ICharBuffer;AHandler:IParserExceptionHandler=nil):IParsed;overload;
  function Parse(AString:string;AHandler:IParserExceptionHandler=nil;AProgressCallBackProc:TProgressCallBackProc=nil):IParsed;overload;
end;

IExecuterExceptionHandler=interface
  ['{8A000234-7CAF-4F62-8DD8-1B9F902E33A3}']
  function RaiseException(ExceptionObject:TObject;CharOffset:Cardinal):Boolean;
  function RaiseUnknownException(CharOffset:Cardinal):Boolean;
end;

IExecuter=interface
  ['{E2A57D4A-D90A-4138-B7A7-F9160D178D94}']
end;

TDefaultCharBuffer=class(TInterfacedObject,ICharBuffer)
private
  FBuffer:string;
  FBufferLength:Cardinal;
  FProgressCallBackProc:TProgressCallBackProc;
public
  constructor Create(ABuffer:string;AProgressCallBack:TProgressCallBackProc=nil);

  function GetChar(var Index:Cardinal;t:TLexerTable):Word;
  function GetLength:Cardinal;
  property Length:Cardinal read GetLength;
  procedure BufferHint(Length:Cardinal);
  function GetBufferText(Start,Length:Cardinal):string;
end;

TDefaultLexer=class(TInterfacedObject,ILexer)
private
  FLexerDefinition:TLexerDefinition;
  FLexerStack:TLexerStack;
  FLexerPos,FLastLexerPos:Cardinal;
  FCharBuffer:ICharBuffer;

  function GetLexerStackState:Word;
  procedure SetLexerStackState(State:Word);
  function PopLexerStackNode:Word;
  procedure PushLexerStackNode(Node,State:Word);
  procedure PrepareNewLexerStack;
public
  constructor Create(ADefinition: TLexerDefinition);

  function GetNewLexem:Word;
  procedure Init(ABuffer:ICharBuffer);
  function GetLexerPos:Cardinal;
  function GetLastLexerPos:Cardinal;
  procedure SetLastLexerPos(Value:Integer);
  function IsLexemStored(Lexem:Word):Boolean;
  function GetErrorId:Word;
  function GetEOFId:Word;

  destructor Destroy;override;
end;

TDefaultParsed=class(TInterfacedObject,IParsed,IReference)
private
  FRoot,FCurrent:TParserNode;
  FNames:PStringArray;
  FNamesCount:Word;
public
  constructor Create(ANode:TParserNode;ANames:PStringArray;ANamesCount:Word);

  procedure GoToRoot;
  function GetCount:Cardinal;
  property Count:Cardinal read GetCount;
  procedure GoUp(Index:Cardinal);
  procedure GoDown;
  function GetID:Word;
  property ID:Word read GetID;
  function GetTextStart:Cardinal;
  property TextStart:Cardinal read GetTextStart;
  function GetTextLength:Cardinal;
  property TextLength:Cardinal read GetTextLength;
  function GetText:string;
  property Text:string read GetText;
  function GetName:string;
  function GetReference:TObject;

  destructor Destroy;override;
end;

TWordArray=array of Word;

TDefaultParser=class(TInterfacedObject,IParser)
private
  FLexer:ILexer;
  FParserDefinition:TParserDefinition;
  FParserStack:TParserStack;

  procedure PrepareNewParserStack;
  function GetParserStackState:Word;
  procedure SetParserStackState(State:Word);
  procedure PushParserStackNode(Node:TParserNode;State:Word);
  function PopParserStackNode:TParserNode;

  procedure BuildExpected(State,Lexem:Word;var Expected:TWordArray;var ExpectedStr:string);

  procedure ReduceParserException(Handler:IParserExceptionHandler;Buffer:ICharBuffer;var State,Lexem:Word);
  procedure RaiseParserException(Handler:IParserExceptionHandler;Buffer:ICharBuffer;State,Found:Word);
public
  constructor Create(ALexer:ILexer;AParserDefinition:TParserDefinition);

  function Parse(ABuffer:ICharBuffer;AHandler:IParserExceptionHandler=nil):IParsed;overload;
  function Parse(AString:string;AHandler:IParserExceptionHandler=nil;AProgressCallBackProc:TProgressCallBackProc=nil):IParsed;overload;

  destructor Destroy;override;
end;

TDefaultExecuter=class(TInterfacedObject,IExecuter)
private
  FParsed:IParsed;
protected
  FNode:TParserNode;
  FHandler:IExecuterExceptionHandler;
  FErrorOffset:Cardinal;

  function ExecuteLeaf(Node:TParserNode):string;

  class function GetParser:IParser;virtual;abstract;
public
  constructor Create(Buffer:ICharBuffer;AHandler:IParserExceptionHandler=nil);overload;
  constructor Create(s:string;AHandler:IParserExceptionHandler=nil;AProgressCallBackProc:TProgressCallBackProc=nil);overload;

  destructor Destroy;override;
end;

function ExtractSubParserNode(Node:TParserNode;Index:Cardinal):TParserNode;

procedure FreeLexerDefinition(x:TLexerDefinition);
procedure FreeParserDefinition(x:TParserDefinition);
implementation

procedure FreeLexerDefinition(x:TLexerDefinition);
var
  a:Integer;
begin
  if not x.StaticResource then begin
    for a:=0 to x.LexerTablesCount-1 do
      FreeMem(x.LexerTables[a].TableItems);
    FreeMem(x.LexerTables);
    FreeMem(x.LexerLaws);
  end;
end;

procedure FreeParserDefinition(x:TParserDefinition);
var
  a:Integer;
begin
  if not x.StaticResource then begin
    FreeMem(x.ParserTable);
    for a:=0 to x.ParserLawsCount-1 do
      FreeMem(x.ParserLaws[a].Usage);
    FreeMem(x.ParserLaws);
    for a:=0 to x.TokenNamesCount-1 do
      x.TokenNames[a]:='';
    FreeMem(x.TokenNames);
  end;
end;

{ TDefaultCharBuffer }

procedure TDefaultCharBuffer.BufferHint(Length: Cardinal);
begin
  if Assigned(FProgressCallBackProc) then
    FProgressCallBackProc(Length);
end;

constructor TDefaultCharBuffer.Create(ABuffer: string; AProgressCallBack:
  TProgressCallBackProc);
begin
  inherited Create;
  FBuffer:=ABuffer;
  FBufferLength:=System.Length(FBuffer);
  FProgressCallBackProc:=AProgressCallBack;
end;

function TDefaultCharBuffer.GetBufferText(Start, Length: Cardinal): string;
begin
  Result:=Copy(FBuffer,Start,Length);
end;

function TDefaultCharBuffer.GetLength: Cardinal;
begin
  Result:=FBufferLength;
end;

function TDefaultCharBuffer.GetChar(var Index: Cardinal; t: TLexerTable): Word;
begin
  Assert(Index>0);
  if Index>FBufferLength then
    Result:=t.EOFID
  else
    Result:=t.CharMap[FBuffer[Index]];
  Inc(Index);
end;

{ TDefaultLexer }

constructor TDefaultLexer.Create(ADefinition: TLexerDefinition);
begin
  inherited Create;
  FLexerDefinition:=ADefinition;
end;

destructor TDefaultLexer.Destroy;
begin
  FreeLexerDefinition(FLexerDefinition);
  PrepareNewLexerStack;
  inherited;
end;

function TDefaultLexer.GetEOFId: Word;
begin
  Result:=FLexerDefinition.LexerEOFId;
end;

function TDefaultLexer.GetErrorId: Word;
begin
  Result:=FLexerDefinition.LexerEOFId+1;
end;

function TDefaultLexer.GetLastLexerPos: Cardinal;
begin
  Result:=FLastLexerPos;
end;

function TDefaultLexer.GetLexerPos: Cardinal;
begin
  Result:=FLexerPos;
end;

function TDefaultLexer.GetLexerStackState: Word;
begin
  if FLexerStack.LexerStackItemsCount>0 then
    Result:=FLexerStack.LexerStackItems[FLexerStack.LexerStackItemsCount-1].State
  else
    Result:=0;
end;

function TDefaultLexer.GetNewLexem: Word;
var
  Node,Char,CurrentChar,OldChar:Word;
  t:TLexerTable;
  a,b,c,i,j,k:Cardinal;
  FullReduceMode:Boolean;
  OldLexerStack:TLexerStack;
  OldLexerPos:Cardinal;
label
  ThrowSeparator;
begin
  FCharBuffer.BufferHint(FLexerPos);
  ThrowSeparator:
  FLastLexerPos:=FLexerPos;
  Result:=FLexerDefinition.LexerEOFId;
  if FLexerPos>FCharBuffer.Length then
    Exit;
  OldChar:=0;
  OldLexerStack.LexerStackItems:=nil;
  OldLexerStack.LexerStackItemsCount:=0;
  OldLexerPos:=0;
  FullReduceMode:=False;
  k:=FLastLexerPos;
  for a:=0 to FLexerDefinition.LexerTablesCount-1 do begin
    t:=FLexerDefinition.LexerTables[a];
    PrepareNewLexerStack;
    Char:=FCharBuffer.GetChar(FLexerPos,t);
    CurrentChar:=Char;
    i:=FLexerPos;
    j:=i;
    repeat
      c:=GetLexerStackState*t.Width+Char;
      case t.TableItems[c].ItemType of
        titReduce:begin
          Node:=t.TableItems[c].Value;
          for b:=0 to FLexerDefinition.LexerLaws[Node].ChildsCount-1 do
            PopLexerStackNode;
          PushLexerStackNode(FLexerDefinition.LexerLaws[Node].ID,GetLexerStackState);
          Char:=FLexerDefinition.LexerLaws[Node].ID;
        end;
        titShift:begin
          Node:=Char;
          PushLexerStackNode(Node,t.TableItems[c].Value);
          if FullReduceMode then begin
            Char:=t.EOFId;
            FLexerPos:=OldLexerPos+1;
          end else begin
            FullReduceMode:=True;
            OldLexerStack.LexerStackItems:=FLexerStack.LexerStackItems;
            FLexerStack.LexerStackItems:=nil;
            GetMem(FLexerStack.LexerStackItems,FLexerStack.LexerStackItemsCount*SizeOf(TLexerStackItem));
            Move(OldLexerStack.LexerStackItems^,FLexerStack.LexerStackItems^,FLexerStack.LexerStackItemsCount*SizeOf(TLexerStackItem));
            OldLexerStack.LexerStackItemsCount:=FLexerStack.LexerStackItemsCount;
            OldChar:=FCharBuffer.GetChar(FLexerPos,t);
            OldLexerPos:=FLexerPos;
            Char:=t.EOFId;
          end;
          CurrentChar:=Char;
        end;
        titGoto:begin
          SetLexerStackState(t.TableItems[c].Value);
          Char:=CurrentChar;
        end;
        titError,titAccept:begin
          if FullReduceMode then begin
            if t.TableItems[c].ItemType=titAccept then
              j:=FLexerPos;
            FreeMem(FLexerStack.LexerStackItems);
            FLexerStack.LexerStackItems:=OldLexerStack.LexerStackItems;
            FLexerStack.LexerStackItemsCount:=OldLexerStack.LexerStackItemsCount;
            Char:=OldChar;
            CurrentChar:=OldChar;
            FLexerPos:=OldLexerPos;
            FullReduceMode:=False;
          end else
            Break;
        end;
      end;
    until False;
    if a=FLexerDefinition.LexerTablesCount-1 then begin
      if k=FLastLexerPos then begin
        if j-1>k then begin
          FLexerPos:=j-1;
          goto ThrowSeparator;
        end else begin
          Result:=GetErrorId;
          //Inc(FLexerPos);
          Exit;
        end;
      end;
    end else begin
      if j-1>k then begin
        k:=j-1;
        Result:=a;
      end;
    end;
  end;
  FLexerPos:=k;
end;

procedure TDefaultLexer.Init(ABuffer: ICharBuffer);
begin
  FLexerPos:=1;
  FLastLexerPos:=1;
  FCharBuffer:=ABuffer;
end;

function TDefaultLexer.IsLexemStored(Lexem: Word): Boolean;
begin
  Result:=FLexerDefinition.LexerTables[Lexem].Stored;
end;

function TDefaultLexer.PopLexerStackNode: Word;
begin
  Assert(FLexerStack.LexerStackItemsCount>0);
  Result:=FLexerStack.LexerStackItems[FLexerStack.LexerStackItemsCount-1].Node;
  Dec(FLexerStack.LexerStackItemsCount);
end;

procedure TDefaultLexer.PrepareNewLexerStack;
begin
  FLexerStack.LexerStackItemsCount:=0;
  ReallocMem(FLexerStack.LexerStackItems,0);
  FLexerPos:=FLastLexerPos;
end;

procedure TDefaultLexer.PushLexerStackNode(Node, State: Word);
begin
  Inc(FLexerStack.LexerStackItemsCount);
  ReallocMem(FLexerStack.LexerStackItems,FLexerStack.LexerStackItemsCount*SizeOf(TLexerStackItem));
  FLexerStack.LexerStackItems[FLexerStack.LexerStackItemsCount-1].State:=State;
  FLexerStack.LexerStackItems[FLexerStack.LexerStackItemsCount-1].Node:=Node;
end;

procedure TDefaultLexer.SetLastLexerPos(Value: Integer);
begin
  FLastLexerPos:=Value;
  FLexerPos:=Value;
end;

procedure TDefaultLexer.SetLexerStackState(State: Word);
begin
  Assert(FLexerStack.LexerStackItemsCount>0);
  FLexerStack.LexerStackItems[FLexerStack.LexerStackItemsCount-1].State:=State;
end;

{ TDefaultParsed }

function ExtractSubParserNode(Node:TParserNode;Index:Cardinal):TParserNode;
begin
  Result:=PParserNode(Cardinal(Node.SubNodes)+Index*SizeOf(TParserNode))^
end;

procedure FreeParserNode(n:TParserNode);
var
  a:Integer;
begin
  if n.IsLeaf then begin
    if n.Value<>nil then begin
      n.Value^:='';
      Dispose(n.Value);
    end;
  end else begin
    for a:=0 to n.Count-1 do
      FreeParserNode(ExtractSubParserNode(n,a));
    ReallocMem(n.SubNodes,0);
  end;
end;

constructor TDefaultParsed.Create(ANode: TParserNode; ANames: PStringArray;
  ANamesCount: Word);

  procedure RecFill(Node:PParserNode;Parent:PParserNode);
  var
    a:Word;
  begin
    Node.Parent:=Parent;
    if not Node.IsLeaf then
      for a:=0 to Node.Count-1 do
        RecFill(PParserNode(Cardinal(Node.SubNodes)+a*SizeOf(TParserNode)),Node);
  end;

var
  a:Integer;
begin
  FRoot:=ANode;
  RecFill(@FRoot,nil);
  FCurrent:=FRoot;
  FNamesCount:=ANamesCount;
  GetMem(FNames,ANamesCount*SizeOf(string));
  ZeroMemory(FNames,ANamesCount*SizeOf(string));
  for a:=0 to ANamesCount-1 do begin
    FNames[a]:=ANames[a];
  end;
end;

destructor TDefaultParsed.Destroy;
var
  a:Integer;
begin
  for a:=0 to FNamesCount-1 do
    FNames[a]:='';
  FreeMem(FNames);
  FreeParserNode(FRoot);
  inherited;
end;

function TDefaultParsed.GetCount: Cardinal;
begin
  Result:=FCurrent.Count;
end;

function TDefaultParsed.GetID: Word;
begin
  Result:=FCurrent.ID;
end;

function TDefaultParsed.GetName: string;
begin
  if FCurrent.IsLeaf then
    Result:=FNames[FCurrent.Id]
  else
    Result:=FNames[FCurrent.Id];
end;

function TDefaultParsed.GetReference: TObject;
begin
  Result:=Self;
end;

function TDefaultParsed.GetText: string;
begin
  Result:='';
  if FCurrent.IsLeaf and Assigned(FCurrent.Value) then
    Result:=PChar(FCurrent.Value^);
end;

function TDefaultParsed.GetTextLength: Cardinal;
begin
  Result:=FCurrent.CharLength;
end;

function TDefaultParsed.GetTextStart: Cardinal;
begin
  Result:=FCurrent.CharOffset;
end;

procedure TDefaultParsed.GoDown;
begin
  Assert(Assigned(FCurrent.Parent),'Already on root');
  FCurrent:=FCurrent.Parent^;
end;

procedure TDefaultParsed.GoToRoot;
begin
  FCurrent:=FRoot;
end;

procedure TDefaultParsed.GoUp(Index: Cardinal);
begin
  Assert(Index<FCurrent.Count);
  FCurrent:=ExtractSubParserNode(FCurrent,Index);
end;

{ TDefaultParser }

procedure TDefaultParser.BuildExpected(State, Lexem: Word;
  var Expected: TWordArray; var ExpectedStr: string);
var
  a,b:Integer;
  t:Boolean;
begin
  SetLength(Expected,0);
  for a:=0 to FParserDefinition.TableWidth-2 do
    if (FParserDefinition.ParserTable[State*FParserDefinition.TableWidth+a].ItemType in [titShift,titReduce]) and
       (a<>FLexer.ErrorID) then begin
      t:=False;
      for b:=0 to High(Expected) do
        t:=t or (FParserDefinition.TokenNames[Expected[b]]=FParserDefinition.TokenNames[a]);
      if not t then begin
        SetLength(Expected,High(Expected)+2);
        Expected[High(Expected)]:=a;
      end;
    end;
  ExpectedStr:=FParserDefinition.TokenNames[Lexem]+' found, but ';
  ExpectedStr[1]:=UpCase(ExpectedStr[1]);
  for a:=Low(Expected) to High(Expected) do begin
    if (a>Low(Expected)) and (a<High(Expected)) then
      ExpectedStr:=ExpectedStr+', '
    else begin
      if (a=High(Expected)) and (a>0) then
        ExpectedStr:=ExpectedStr+' or '
    end;
    ExpectedStr:=ExpectedStr+FParserDefinition.TokenNames[Expected[a]];
  end;
  ExpectedStr:=ExpectedStr+' expected.';
end;

constructor TDefaultParser.Create(ALexer: ILexer;
  AParserDefinition: TParserDefinition);
begin
  FLexer:=ALexer;
  FParserDefinition:=AParserDefinition;
end;

destructor TDefaultParser.Destroy;
begin
  FreeParserDefinition(FParserDefinition);
  FLexer:=nil;
  inherited;
end;

function TDefaultParser.GetParserStackState: Word;
begin
  if FParserStack.ParserStackItemsCount>0 then
    Result:=FParserStack.ParserStackItems[FParserStack.ParserStackItemsCount-1].State
  else
    Result:=0;
end;

function TDefaultParser.Parse(ABuffer: ICharBuffer; AHandler: IParserExceptionHandler): IParsed;
var
  State,Lexem,CurrentLexem:Word;
  Node:TParserNode;
  a,b,c:Integer;
begin
  FLexer.Init(ABuffer);
  PrepareNewParserStack;
  Lexem:=FLexer.GetNewLexem;
  CurrentLexem:=Lexem;
  repeat
    State:=GetParserStackState;
    case FParserDefinition.ParserTable[State*FParserDefinition.TableWidth+Lexem].ItemType of
      titReduce:begin
        Node.ID:=FParserDefinition.ParserTable[State*FParserDefinition.TableWidth+Lexem].Value;
        b:=FParserDefinition.ParserLaws[Node.ID].ChildsCount;
        c:=FParserDefinition.ParserLaws[Node.ID].UsageCount;
        Node.IsLeaf:=False;
        Node.Count:=c;
        GetMem(Node.SubNodes,c*SizeOf(TParserNode));
        for a:=b-1 downto 0 do
          if FParserDefinition.ParserLaws[Node.ID].Usage[a] then begin
            Dec(c);
            PParserNode(Cardinal(Node.SubNodes)+Cardinal(c)*SizeOf(TParserNode))^:=PopParserStackNode;
          end else
            FreeParserNode(PopParserStackNode);
        Node.CharOffset:=Node.SubNodes^.CharOffset;
        PushParserStackNode(Node,GetParserStackState);
        Lexem:=FParserDefinition.ParserLaws[Node.ID].ID;
       end;
      titShift:begin
        Node.ID:=Lexem;
        Node.Count:=0;
        Node.IsLeaf:=True;
        Node.CharOffset:=FLexer.LastLexerPos;
        if FLexer.IsLexemStored(Lexem) then begin
          New(Node.Value);
          ZeroMemory(Node.Value,SizeOf(string));
          Node.Value^:=ABuffer.GetBufferText(FLexer.LastLexerPos,FLexer.LexerPos-FLexer.LastLexerPos);
        end else
          Node.Value:=nil;
        PushParserStackNode(Node,FParserDefinition.ParserTable[State*FParserDefinition.TableWidth+Lexem].Value);
        Lexem:=FLexer.GetNewLexem;
        CurrentLexem:=Lexem;
      end;
      titGoto:begin
        SetParserStackState(FParserDefinition.ParserTable[State*FParserDefinition.TableWidth+Lexem].Value);
        Lexem:=CurrentLexem;
      end;
      titError:begin
        ReduceParserException(AHandler,ABuffer,State,Lexem);
       //RaiseParserException(State,Lexem);
      end;
    end;
  until FParserDefinition.ParserTable[State*FParserDefinition.TableWidth+Lexem].ItemType=titAccept;
  FLexer.Init(nil);
  Result:=TDefaultParsed.Create(PopParserStackNode,FParserDefinition.TokenNames,FParserDefinition.TokenNamesCount);
end;

function TDefaultParser.Parse(AString: string; AHandler: IParserExceptionHandler;
  AProgressCallBackProc: TProgressCallBackProc): IParsed;
begin
  Result:=Parse(TDefaultCharBuffer.Create(AString,AProgressCallBackProc),AHandler);
end;

function TDefaultParser.PopParserStackNode: TParserNode;
begin
  Assert(FParserStack.ParserStackItemsCount>0);
  Result:=FParserStack.ParserStackItems[FParserStack.ParserStackItemsCount-1].Node;
  Dec(FParserStack.ParserStackItemsCount);
end;

procedure TDefaultParser.PrepareNewParserStack;
var
  a:Integer;
begin
  for a:=0 to Integer(FParserStack.ParserStackItemsCount)-1 do
    FreeParserNode(FParserStack.ParserStackItems[a].Node);
  FParserStack.ParserStackItemsCount:=0;
  ReallocMem(FParserStack.ParserStackItems,0);
end;

procedure TDefaultParser.PushParserStackNode(Node: TParserNode;
  State: Word);
begin
  Inc(FParserStack.ParserStackItemsCount);
  ReallocMem(FParserStack.ParserStackItems,FParserStack.ParserStackItemsCount*SizeOf(TParserStackItem));
  FParserStack.ParserStackItems[FParserStack.ParserStackItemsCount-1].State:=State;
  FParserStack.ParserStackItems[FParserStack.ParserStackItemsCount-1].Node:=Node;
end;

procedure TDefaultParser.RaiseParserException(Handler: IParserExceptionHandler;
  Buffer: ICharBuffer; State, Found: Word);
var
  s:string;
  t:TWordArray;
begin
  BuildExpected(State,Found,t,s);
  if Assigned(Handler) then begin
    try
      Handler.RaiseException(False,Self,FLexer,Buffer,Found,t,s)
    finally
      Handler:=nil;
    end;
  end else
    Assert(False,s);
end;

procedure TDefaultParser.ReduceParserException(Handler: IParserExceptionHandler; Buffer: ICharBuffer; var State, Lexem: Word);
var
  n:TParserNode;
  t:TWordArray;
  s:string;
begin
  while (FParserStack.ParserStackItemsCount>0) and
        ((FParserStack.ParserStackItems[FParserStack.ParserStackItemsCount-1].Node.ID=FLexer.ErrorID) or
         (FParserDefinition.ParserTable[GetParserStackState*FParserDefinition.TableWidth+FLexer.ErrorID-1].ItemType=titError)) do begin
    n:=PopParserStackNode;
    FreeParserNode(n);
  end;
  if FParserDefinition.ParserTable[GetParserStackState*FParserDefinition.TableWidth+FLexer.ErrorID].ItemType=titError then
    RaiseParserException(Handler,Buffer,State,Lexem)
  else begin
    BuildExpected(State,Lexem,t,s);
    if Assigned(Handler) then
      Handler.RaiseException(True,Self,FLexer,Buffer,Lexem,t,s)
    else
      MessageBox(0,PChar(s),nil,0);
    Lexem:=FLexer.ErrorID;
  end;
end;

procedure TDefaultParser.SetParserStackState(State: Word);
begin
  Assert(FParserStack.ParserStackItemsCount>0);
  FParserStack.ParserStackItems[FParserStack.ParserStackItemsCount-1].State:=State;
end;

{ TDefaultExecuter }

constructor TDefaultExecuter.Create(Buffer: ICharBuffer; AHandler: IParserExceptionHandler);
begin
  inherited Create;
  FParsed:=GetParser.Parse(Buffer,AHandler);
  FNode:=((FParsed as IReference).Reference as TDefaultParsed).FRoot;
end;

constructor TDefaultExecuter.Create(s: string; AHandler: IParserExceptionHandler;
  AProgressCallBackProc: TProgressCallBackProc);
begin
  Create(TDefaultCharBuffer.Create(s,AProgressCallBackProc),AHandler);
end;

destructor TDefaultExecuter.Destroy;
begin
  FParsed:=nil;
  inherited;
end;

function TDefaultExecuter.ExecuteLeaf(Node: TParserNode): string;
begin
  Result:=Node.Value^;
end;

end.
