unit COW_Design;

interface

uses
  Dialogs,SysUtils,Classes,Contnrs,Windows,COW_RunTime,COW_Utils,StrUtils;

type
  TGrammar=class;
  TNodeGrammarItem=class;
  TAndGrammarItem=class;
  TOrGrammarItem=class;

  TObjectSet=class(TObjectList)
  public
    function Add(AObject:TObject):Boolean;
    function Contains(AObject:TObject):Boolean;
    function AddSet(ASet:TObjectSet):Boolean;
  end;

  PNullItem=^TNullItem;
  TNullItem=record
    Law:TAndGrammarItem;
    LawParent:TOrGrammarItem;
    Position:Cardinal;
  end;

  TNullItemSet=class(TList)
  private
    function GetItems(Index: Integer): TNullItem;
  public
    function Add(Law:TAndGrammarItem;Position:Cardinal;Grammar:TGrammar):Boolean;

    property Items[Index:Integer]:TNullItem read GetItems;default;
    procedure Close;
    function Transition(ALaw:TNodeGrammarItem):TNullItemSet;
    function IsEqualTo(ASet:TNullItemSet):Boolean;
    function IsEmpty:Boolean;

    destructor Destroy;override;
  end;

  TNullItemSetCollection=class(TObjectList)
  private
    function GetItems(Index: Integer): TNullItemSet;
  public
    function Add(ASet:TNullItemSet):Boolean;
    function IndexOf(ASet:TNullItemSet):Integer;
    function Contains(ASet:TNullItemSet):Boolean;
    property Items[Index:Integer]:TNullItemSet read GetItems;default;
  end;

  TTableItemType=(tipAccept,tipError,tipShift,tipReduce,tipGoto);

  PTableItem=^TTableItem;
  TTableItem=packed record
    case ItemType:TTableItemType of
      tipAccept:();
      tipError:();
      tipShift:(Count:Cardinal);
      tipReduce:(ID:Cardinal);
      tipGoto:(Index:Cardinal);
  end;

  TAnalyseRow=class(TList)
  private
    function GetItem(Index: Integer): TTableItem;
    procedure SetItem(Index: Integer; const Value: TTableItem);
  public
    constructor Create(ACount:Integer);

    property Item[Index:Integer]:TTableItem read GetItem write SetItem;

    destructor Destroy;override;
  end;

  TAnalyseTable=class
  private
    FList:TObjectList;
    FColCount:Integer;
    FCollection:TNullItemSetCollection;
    FGrammar:TGrammar;

    function GetItem(ARow, ACol: Integer): TTableItem;
    function GetRowCount: Integer;
    procedure SetItem(ARow, ACol: Integer; const Value: TTableItem);
    procedure CheckReduceConflict(ARow,ACol:Integer;Item:TTableItem);
  public
    constructor Create(ACollection:TNullItemSetCollection;AGrammar:TGrammar);

    property Item[ARow,ACol:Integer]:TTableItem read GetItem write SetItem;default;
    property RowCount:Integer read GetRowCount;
    property ColCount:Integer read FColCount;

    destructor Destroy;override;
  end;

  TAndGrammarItem=class
  private
    FSubItems:TObjectList;
    FUsage:Array of Boolean;

    function GetLaw(Index: Integer): TNodeGrammarItem;
    function GetLawCount: Integer;
    function GetUsedLawItem(Index: Integer): Boolean;
    procedure SetUsedLawItem(Index: Integer; const Value: Boolean);
    function GetUsageCount: Integer;
  protected
    FImplementationBlock:string;
  public
    constructor Create(SubItems:array of TNodeGrammarItem;AImplementationBlock:string='');

    procedure InsertSubItem(Index:Integer;Item:TNodeGrammarItem);
    property LawCount:Integer read GetLawCount;
    property Law[Index:Integer]:TNodeGrammarItem read GetLaw;
    property UsedLawItem[Index:Integer]:Boolean read GetUsedLawItem write SetUsedLawItem;
    property UsageCount:Integer read GetUsageCount;

    destructor Destroy;override;
  end;

  TNodeGrammarItem=class
  private
    FGrammar:TGrammar;
    FFirst:TObjectSet;

    function GetID:Cardinal;
    function GetName:string;virtual;abstract;
    procedure SetName(const Value: string);virtual;abstract;
    function GetUserFriendlyName: string;virtual;abstract;
    procedure SetUserFriendlyName(const Value: string);virtual;abstract;
  public
    constructor Create(AGrammar:TGrammar);

    function IsLeaf:Boolean;virtual;abstract;
    property Name:string read GetName write SetName;
    property UserFriendlyName:string read GetUserFriendlyName write SetUserFriendlyName;
    property ID:Cardinal read GetID;

    destructor Destroy;override;
  end;

  TTokenGrammarItem=class(TNodeGrammarItem)
  private
    FName,FUserFriendlyName:string;

    function GetName:string;override;
    procedure SetName(const Value: string);override;
    function GetUserFriendlyName: string;override;
    procedure SetUserFriendlyName(const Value: string);override;
  public
    constructor Create(AGrammar:TGrammar;AName:string);

    function IsLeaf:Boolean;override;
  end;

  TEOFGrammarItem=class(TNodeGrammarItem)
  private
    function GetName:string;override;
    procedure SetName(const Value: string);override;
    function GetUserFriendlyName: string;override;
    procedure SetUserFriendlyName(const Value: string);override;
  public
    function IsLeaf:Boolean;override;
  end;

  TErrorGrammarItem=class(TNodeGrammarItem)
  private
    function GetName:string;override;
    procedure SetName(const Value: string);override;
    function GetUserFriendlyName: string;override;
    procedure SetUserFriendlyName(const Value: string);override;
  public
    function IsLeaf:Boolean;override;
  end;

  TOrGrammarItem=class(TNodeGrammarItem)
  private
    FName,FType,FDeclarationBlock:string;
    FSubItems:TObjectList;
    FNext:TObjectSet;
    FUserFriendlyName: string;
    FStorable: Boolean;

    function GetName:string;override;
    procedure SetName(const Value: string);override;
    function GetUserFriendlyName: string;override;
    procedure SetUserFriendlyName(const Value: string);override;
    function GetCount: Integer;
    function GetItem(Index: Integer): TAndGrammarItem;
  public
    constructor Create(AGrammar:TGrammar;SubItems:array of TAndGrammarItem;AName:string;AType:string='';ADeclarationBlock:string='');

    function IsLeaf:Boolean;override;
    property Count:Integer read GetCount;
    property Item[Index:Integer]:TAndGrammarItem read GetItem;default;
    procedure InsertItem(Index:Integer;Item:TAndGrammarItem);
    property Storable:Boolean read FStorable write FStorable;

    destructor Destroy;override;
  end;

  TGrammar=class
  private
    FEOF:TEOFGrammarItem;

    function GetLawCount: Integer;
    function GetLawItem(Name: string): TOrGrammarItem;
    function GetTokenItem(Name: string): TTokenGrammarItem;

    procedure BuildFirst;
    procedure BuildNext;
    function GetItem(Name: string): TNodeGrammarItem;
  protected
    FNodes:TStringList;
    FLaws:TObjectList;
    FMain:TOrGrammarItem;
  public
    constructor Create(ATokenList:TStringList);

    property LawCount:Integer read GetLawCount;
    property LawItem[Name:string]:TOrGrammarItem read GetLawItem;
    property TokenItem[Name:string]:TTokenGrammarItem read GetTokenItem;
    property Item[Name:string]:TNodeGrammarItem read GetItem;default;
    function IndexOfLaw(Law:TAndGrammarItem):Integer;
    function BuildAnalyseTable:TAnalyseTable;

    destructor Destroy;override;
  end;

  TCharMap=array[#0..#255] of TNodeGrammarItem;

  TLexerItemGrammar=class(TGrammar)
  private
    FCharMap:TCharMap;
    FStored:Boolean;
  public
    constructor Create(TokenList,LawList:TStringList);

    property Stored:Boolean read FStored;
  end;

  TLexer=class(TGrammar)
  private
    FRootLexerItems:TStringList;
    FBuilt:Boolean;

    procedure MakeProper;
    procedure BuildSeparateGrammars;
  public
    constructor Create(ALaws:TStringList);

    function BuildLexerDefinition:TLexerDefinition;
    function BuildPascalLexerDefinitions(Prefix:string;CRLF:string=#10#13):string;

    destructor Destroy;override;
  end;

  TParser=class(TGrammar)
  protected
    FPreImplementationBlock,FPostImplementationBlock:string;
    FLexer:TLexer;
    FError:TErrorGrammarItem;
  public
    constructor Create(ALexer:TLexer;APreImplementationBlock:string='';APostImplementationBlock:string='');

    property Main:TOrGrammarItem read FMain;

    function BuildLexerDefinition:TLexerDefinition;
    function BuildParserDefinition:TParserDefinition;
    function BuildIParser:IParser;

    property PreImplementationBlock:string read FPreImplementationBlock;
    property PostImplementationBlock:string read FPostImplementationBlock;

    function BuildPascalLexerDefinitions(Prefix:string;CRLF:string=#10#13):string;
    function BuildPascalParserDefinitions(Prefix:string;CRLF:string=#10#13):string;
    function BuildPascalDefinitions(Prefix:string;CRLF:string=#10#13):string;
    function BuildPascalClassDeclaration(Prefix:string;CRLF:string=#10#13):string;
    function BuildPascalClassImplementation(Prefix:string;CRLF:string=#10#13):string;
  end;

//procedure DisplayLaws(Grammar:TGrammar);

procedure DefaultEmitWarning(s:string);
procedure DefaultEmitError(s:string);
procedure DefaultPushProgress;
procedure DefaultUpdateProgress(Position:Single;Caption:string);
procedure DefaultPopProgress;

var
  EmitWarning:procedure(s:string)=DefaultEmitWarning;
  EmitError:procedure(s:string)=DefaultEmitError;
  PushProgress:procedure=DefaultPushProgress;
  UpdateProgress:procedure(Position:Single;Caption:string)=DefaultUpdateProgress;
  PopProgress:procedure=DefaultPopProgress;

implementation

procedure DefaultEmitWarning(s:string);
begin
  ShowMessage('WARNING: '+s);
end;

procedure DefaultEmitError(s:string);
begin
  Assert(False,'ERROR: '+s);
end;

procedure DefaultPushProgress;
begin

end;

procedure DefaultUpdateProgress(Position:Single;Caption:string);
begin

end;

procedure DefaultPopProgress;
begin

end;

function CenterString(s:string;l:Integer):string;
var
  t:Boolean;
begin
  Result:=s;
  t:=False;
  while Length(Result)<l do begin
    if t then
      Result:=Result+' '
    else
      Result:=' '+Result;
    t:=not t;
  end;
end;

function TableItemToString(Item:TTableItem):string;
begin
  case Item.ItemType of
    tipAccept:Result:='ACC';
    tipError:Result:='';
    tipShift:Result:='d'+IntToStr(Item.Count);
    tipReduce:Result:='r'+IntToStr(Item.ID);
    tipGoto:Result:=IntToStr(Item.Index);
  end;
end;

{procedure DisplayLaws(Grammar:TGrammar);
var
  a,b,c,d:Integer;
  s,t:string;
const
  l=10;
begin
  Grammar.BuildFirst;
  Grammar.BuildNext;
  d:=0;
  for a:=0 to Grammar.FLaws.Count-1 do
    with TOrGrammarItem(Grammar.FLaws[a]) do begin
      s:=Name+' -->';
      for b:=0 to FSubItems.Count-1 do begin
        t:='';
        with TAndGrammarItem(FSubItems[b]) do
          for c:=0 to FSubItems.Count-1 do
            t:=t+' '+TNodeGrammarItem(FSubItems[c]).Name;
        Form1.Memo5.Lines.Add('('+IntToStr(d)+') '+s+t);
        Inc(d);
      end;
    end;
  Form1.Memo5.Lines.Add('');
  for a:=0 to Grammar.FLaws.Count-1 do
    with TOrGrammarItem(Grammar.FLaws[a]) do begin
      s:=Name+' : [';
      t:='';
      for b:=0 to FFirst.Count-1 do begin
        s:=s+t+TTokenGrammarItem(FFirst[b]).Name;
        t:=' , ';
      end;
      s:=s+'] ; [';
      t:='';
      for b:=0 to FNext.Count-1 do begin
        s:=s+t+TTokenGrammarItem(FNext[b]).Name;
        t:=' , ';
      end;
      Form1.Memo5.Lines.Add(s+']');
    end;
  Form1.Memo5.Lines.Add('');
  s:='|'+CenterString('état',l)+'|';
  for a:=0 to Grammar.FNodes.Count-1 do
    s:=s+CenterString(Grammar.FNodes[a],l)+'|';
  Form1.Memo5.Lines.Add(s);
  with Grammar.BuildAnalyseTable do
    for a:=0 to RowCount-1 do begin
      s:='|'+CenterString(IntToStr(a),l)+'|';
      for b:=0 to ColCount-1 do
        s:=s+CenterString(TableItemToString(Item[a,b]),l)+'|';
      Form1.Memo5.Lines.Add(s);
    end;
  Form1.Memo5.Lines.Add('');
end;}

{ TObjectSet }

function TObjectSet.Add(AObject: TObject): Boolean;
begin
  Result:=not Contains(AObject);
  if Result then
    inherited Add(AObject);
end;

function TObjectSet.AddSet(ASet: TObjectSet):Boolean;
var
  a:Integer;
begin
  Result:=False;
  for a:=0 to ASet.Count-1 do
    Result:=Add(ASet[a]) or Result;
end;

function TObjectSet.Contains(AObject: TObject): Boolean;
begin
  Result:=IndexOf(AObject)>-1;
end;

{ TNullItemSet }

function TNullItemSet.Add(Law: TAndGrammarItem; Position: Cardinal; Grammar: TGrammar): Boolean;
var
  p:PNullItem;
  a:Integer;
begin
  Result:=False;
  for a:=0 to Count-1 do
    if (Items[a].Law=Law) and (Items[a].Position=Position) then
      Exit;
  New(p);
  p^.Law:=Law;
  p^.Position:=Position;
  p^.LawParent:=nil;
  for a:=0 to Grammar.FLaws.Count-1 do
    with Grammar.FLaws[a] as TOrGrammarItem do
      if FSubItems.IndexOf(Law)>-1 then
        p^.LawParent:=Grammar.FLaws[a] as TOrGrammarItem;
  Assert(Assigned(p^.LawParent));
  inherited Add(p);
  Result:=True;
end;

procedure TNullItemSet.Close;
var
  a,b:Integer;
  t:Boolean;
begin
  repeat
    t:=False;
    for a:=0 to Count-1 do
      with Items[a] do
        if Position<Cardinal(Law.FSubItems.Count) then
          with Law.FSubItems[Position] as TNodeGrammarItem do
            if FGrammar.FLaws.IndexOf(Law.FSubItems[Position])>-1 then
              with Law.FSubItems[Position] as TOrGrammarItem do
                for b:=0 to Count-1 do
                  t:=Add(Item[b],0,FGrammar) or t;
  until not t;
end;

destructor TNullItemSet.Destroy;
begin
  while Count>0 do begin
    Dispose(inherited Items[0]);
    Delete(0);
  end;
  inherited;
end;

function TNullItemSet.GetItems(Index: Integer): TNullItem;
begin
  Result:=PNullItem(inherited Items[Index])^;
end;

function TNullItemSet.IsEmpty: Boolean;
begin
  Result:=Count=0;
end;

function TNullItemSet.IsEqualTo(ASet: TNullItemSet): Boolean;
var
  a,b:Integer;
begin
  Result:=Count=ASet.Count;
  if not Result then
    Exit;
  for a:=0 to Count-1 do begin
    Result:=False;
    for b:=0 to ASet.Count-1 do
      if (Items[a].Law=ASet[b].Law) and (Items[a].Position=ASet[b].Position) then begin
        Result:=True;
        Break;
      end;
    if not Result then
      Exit;
  end;
end;

function TNullItemSet.Transition(ALaw: TNodeGrammarItem): TNullItemSet;
var
  a:Integer;
begin
  Result:=TNullItemSet.Create;
  for a:=0 to Count-1 do
    with Items[a] do
      if (Position<Cardinal(Law.LawCount)) and (Law.Law[Position]=ALaw) then
        Result.Add(Law,Position+1,LawParent.FGrammar);
  Result.Close;
end;

{ TNullItemSetCollection }

function TNullItemSetCollection.Add(ASet: TNullItemSet): Boolean;
begin
  Result:=not Contains(ASet);
  if Result then
    inherited Add(ASet);
end;

function TNullItemSetCollection.Contains(ASet: TNullItemSet): Boolean;
var
  a:Integer;
begin
  Result:=False;
  for a:=0 to Count-1 do
    Result:=Result or Items[a].IsEqualTo(ASet);
end;

function TNullItemSetCollection.GetItems(Index: Integer): TNullItemSet;
begin
  Result:=(inherited Items[Index]) as TNullItemSet;
end;

function TNullItemSetCollection.IndexOf(ASet: TNullItemSet): Integer;
var
  a:Integer;
begin
  Result:=-1;
  for a:=0 to Count-1 do
    if Items[a].IsEqualTo(ASet) then begin
      Result:=a;
      Exit;
    end;
end;

{ TAnalyseRow }

constructor TAnalyseRow.Create(ACount: Integer);
var
  a:Integer;
  p:PTableItem;
begin
  inherited Create;
  for a:=0 to ACount-1 do begin
    New(p);
    FillChar(p^,SizeOf(TTableItem),0);
    p^.ItemType:=tipError;
    Add(p);
  end;
end;

destructor TAnalyseRow.Destroy;
begin
  while Count>0 do begin
    Dispose(inherited Items[0]);
    Delete(0);
  end;
  inherited;
end;

function TAnalyseRow.GetItem(Index: Integer): TTableItem;
begin
  Result:=PTableItem(inherited Items[Index])^;
end;

procedure TAnalyseRow.SetItem(Index: Integer; const Value: TTableItem);
begin
  PTableItem(inherited Items[Index])^:=Value;
end;

{ TAnalyseTable }

function LawByIndex(AGrammar:TGrammar;Index:Integer):TAndGrammarItem;
var
  a:Integer;
begin
  Result:=nil;
  for a:=0 to AGrammar.FLaws.Count-1 do
    with (AGrammar.FLaws[a] as TOrGrammarItem) do
      if FSubItems.Count>Index then
        Result:=Item[Index]
      else
        Dec(Index,FSubItems.Count);
end;

function LawToString(AGrammar:TGrammar;Index,LawIndex:Integer):string;
var
  l:TOrGrammarItem;
  a:Integer;
begin
  l:=nil;
  for a:=0 to AGrammar.FLaws.Count-1 do
    with (AGrammar.FLaws[a] as TOrGrammarItem) do
      if Count>Index then begin
        l:=AGrammar.FLaws[a] as TOrGrammarItem;
        Break;
      end else
        Dec(Index,Count);
  if Assigned(l) then begin
    Result:=l.FName+' ->';
    with l.FSubItems[Index] as TAndGrammarItem do
      for a:=0 to LawCount-1 do
        Result:=Result+' '+Law[a].Name;
    if LawIndex<AGrammar.FNodes.Count then
      with AGrammar.FNodes.Objects[LawIndex] as TNodeGrammarItem do
        Result:=Result+' | '+Name;
  end else
    Result:='Error';
end;

function CompareLaw(u,v:TAndGrammarItem):Boolean;
var
  a:Integer;
begin
  Result:=Assigned(u) and
          Assigned(v) and
          (u.LawCount=v.LawCount) and
          (u.FImplementationBlock=v.FImplementationBlock);
  if Result then
    for a:=0 to u.LawCount-1 do
      Result:=Result and (u.Law[a]=v.Law[a]);
end;

procedure TAnalyseTable.CheckReduceConflict(ARow, ACol: Integer;
  Item: TTableItem);
var
  i:TTableItem;
  s:string;
begin
  i:=Self.Item[ARow,ACol];
  case i.ItemType of
    tipAccept:EmitWarning('Accept/Reduce conflict');
    tipError:;
    tipShift:begin
      s:='Shift/Reduce conflict'#13;
      s:=s+LawToString(FGrammar,Item.ID,ACol)+#13;
      EmitWarning(s);
    end;
    tipReduce:begin
      if not CompareLaw(LawByIndex(FGrammar,i.ID),LawByIndex(FGrammar,Item.ID)) then begin
        s:='Reduce/Reduce conflict'#13;
        s:=s+LawToString(FGrammar,i.ID,ACol)+#13;
        s:=s+LawToString(FGrammar,Item.ID,ACol)+#13;
        EmitWarning(s);
      end;
    end;
    tipGoto:EmitWarning('Goto/Reduce conflict');
  end;
  Self.Item[ARow,ACol]:=Item
end;

constructor TAnalyseTable.Create(ACollection: TNullItemSetCollection;
  AGrammar: TGrammar);
var
  a,b,c,d:Integer;
  Item:TNullItemSet;
  i:TTableItem;
begin
  inherited Create;
  FGrammar:=AGrammar;
  FColCount:=AGrammar.FNodes.Count-1;
  FList:=TObjectList.Create(True);
  FCollection:=ACollection;
  for a:=0 to FCollection.Count-1 do
    FList.Add(TAnalyseRow.Create(FColCount));
  PushProgress;
  try
    for a:=0 to FCollection.Count-1 do begin
      UpdateProgress(a/FCollection.Count,'Building row '+IntToStr(a)+'/'+IntToStr(FCollection.Count));
      for b:=0 to AGrammar.FNodes.Count-1 do begin
        Item:=FCollection[a].Transition(AGrammar.FNodes.Objects[b] as TNodeGrammarItem);
        c:=FCollection.IndexOf(Item);
        if c>-1 then begin
          if (AGrammar.FNodes.Objects[b] as TNodeGrammarItem).IsLeaf then begin
            i.ItemType:=tipShift;
            i.Count:=c;
          end else begin
            i.ItemType:=tipGoto;
            i.Index:=c;
          end;
          Self[a,b]:=i;
        end;
      end;
      for b:=0 to FCollection[a].Count-1 do
        with FCollection[a].Items[b] do
          if Position=Cardinal(Law.LawCount) then
            for c:=0 to LawParent.FNext.Count-1 do begin
              i.ItemType:=tipReduce;
              i.ID:=AGrammar.IndexOfLaw(Law);
              d:=AGrammar.FNodes.IndexOfObject(LawParent.FNext[c] as TNodeGrammarItem);
              CheckReduceConflict(a,d,i);
            end;
    end;
  finally
    PopProgress;
  end;
  a:=AGrammar.FNodes.IndexOfObject(AGrammar.FMain);
  Assert(Self[0,a].ItemType=tipGoto);
  b:=AGrammar.FNodes.IndexOf('$');
  i.ItemType:=tipAccept;
  Self[Self[0,a].Index,b]:=i;
end;

destructor TAnalyseTable.Destroy;
begin
  FList.Destroy;
  FCollection.Destroy;
  inherited;
end;

function TAnalyseTable.GetItem(ARow, ACol: Integer): TTableItem;
begin
  Result:=(FList[ARow] as TAnalyseRow).Item[ACol];
end;

function TAnalyseTable.GetRowCount: Integer;
begin
  Result:=FList.Count;
end;

procedure TAnalyseTable.SetItem(ARow, ACol: Integer;
  const Value: TTableItem);
begin
  (FList[ARow] as TAnalyseRow).Item[ACol]:=Value;
end;

{ TAndGrammarItem }

constructor TAndGrammarItem.Create(SubItems: array of TNodeGrammarItem;
  AImplementationBlock: string);
var
  a:Integer;
begin
  inherited Create;
  FSubItems:=TObjectList.Create(False);
  for a:=Low(SubItems) to High(SubItems) do
    FSubItems.Add(SubItems[a]);
  FImplementationBlock:=AImplementationBlock;
  SetLength(FUsage,0);
end;

destructor TAndGrammarItem.Destroy;
begin
  SetLength(FUsage,0);
  FSubItems.Destroy;
  inherited;
end;

function TAndGrammarItem.GetLaw(Index: Integer): TNodeGrammarItem;
begin
  Result:=FSubItems[Index] as TNodeGrammarItem;
end;

function TAndGrammarItem.GetLawCount: Integer;
begin
  Result:=FSubItems.Count;
end;

function TAndGrammarItem.GetUsageCount: Integer;
var
  a:Integer;
begin
  Result:=LawCount-High(FUsage)-1;
  for a:=0 to High(FUsage) do
    if FUsage[a] then
      Inc(Result);
end;

function TAndGrammarItem.GetUsedLawItem(Index: Integer): Boolean;
begin
  if Index>High(FUsage) then
    Result:=True
  else
    Result:=FUsage[Index];
end;

procedure TAndGrammarItem.InsertSubItem(Index: Integer;
  Item: TNodeGrammarItem);
begin
  FSubitems.Insert(Index,Item);
end;

procedure TAndGrammarItem.SetUsedLawItem(Index: Integer;
  const Value: Boolean);
var
  a,b:Integer;
begin
  if (Index>High(FUsage)) and not Value then begin
    b:=High(FUsage);
    SetLength(FUsage,Index+1);
    for a:=b+1 to Index-1 do
      FUsage[a]:=True;
  end;
  if Index<=high(FUsage) then
    FUsage[Index]:=Value;
end;

{ TNodeGrammarItem }

constructor TNodeGrammarItem.Create(AGrammar: TGrammar);
begin
  inherited Create;
  FGrammar:=AGrammar;
  FGrammar.FNodes.AddObject(Name,Self);
  FFirst:=TObjectSet.Create(False);
end;

destructor TNodeGrammarItem.Destroy;
begin
  FFirst.Destroy;
  FGrammar.FNodes.Delete(ID);
  inherited;
end;

function TNodeGrammarItem.GetID: Cardinal;
begin
  Result:=FGrammar.FNodes.IndexOfObject(Self);
end;

{ TTokenGrammarItem }

constructor TTokenGrammarItem.Create(AGrammar: TGrammar; AName: string);
begin
  FName:=AName;
  inherited Create(AGrammar);
end;

function TTokenGrammarItem.GetName: string;
begin
  Result:=FName;
end;

function TTokenGrammarItem.GetUserFriendlyName: string;
begin
  if FUserFriendlyName='' then
    Result:=Name
  else
    Result:=FUserFriendlyName;
end;

function TTokenGrammarItem.IsLeaf: Boolean;
begin
  Result:=True;
end;

procedure TTokenGrammarItem.SetName(const Value: string);
begin
  FName:=Value;
  FGrammar.FNodes[ID]:=FName;
end;

procedure TTokenGrammarItem.SetUserFriendlyName(const Value: string);
begin
  FUserFriendlyName:=Value;
end;

{ TEOFGrammarItem }

function TEOFGrammarItem.GetName: string;
begin
  Result:='$';
end;

function TEOFGrammarItem.GetUserFriendlyName: string;
begin
  Result:='end of file';
end;

function TEOFGrammarItem.IsLeaf: Boolean;
begin
  Result:=True;
end;

procedure TEOFGrammarItem.SetName(const Value: string);
begin
  Assert(False);
end;

procedure TEOFGrammarItem.SetUserFriendlyName(const Value: string);
begin
  Assert(False);
end;

{ TErrorGrammarItem }

function TErrorGrammarItem.GetName: string;
begin
  Result:='Error';
end;

function TErrorGrammarItem.GetUserFriendlyName: string;
begin
  Result:='error';
end;

function TErrorGrammarItem.IsLeaf: Boolean;
begin
  Result:=True;
end;

procedure TErrorGrammarItem.SetName(const Value: string);
begin
  Assert(False);
end;

procedure TErrorGrammarItem.SetUserFriendlyName(const Value: string);
begin
  Assert(False);
end;

{ TOrGrammarItem }

constructor TOrGrammarItem.Create(AGrammar: TGrammar;
  SubItems: array of TAndGrammarItem; AName, AType, ADeclarationBlock: string);
var
  a:Integer;
begin
  if AName='' then
    FName:='Item'+IntToStr(AGrammar.FNodes.Count-1)
  else
    FName:=AName;
  inherited Create(AGrammar);
  FGrammar.FLaws.Add(Self);
  FSubItems:=TObjectList.Create(True);
  for a:=Low(SubItems) to High(SubItems) do
    FSubItems.Add(SubItems[a]);
  FType:=AType;
  FDeclarationBlock:=ADeclarationBlock;
  FNext:=TObjectSet.Create(False);
  if Name='Main' then
    FGrammar.FMain:=Self;
end;

destructor TOrGrammarItem.Destroy;
begin
  FNext.Destroy;
  FSubItems.Destroy;
  FGrammar.FLaws.Remove(Self);
  inherited;
end;

function TOrGrammarItem.GetCount: Integer;
begin
  Result:=FSubItems.Count;
end;

function TOrGrammarItem.GetItem(Index: Integer): TAndGrammarItem;
begin
  Result:=FSubItems[Index] as TAndGrammarItem;
end;

function TOrGrammarItem.GetName: string;
begin
  Result:=FName;
end;

function TOrGrammarItem.GetUserFriendlyName: string;
begin
  if FUserFriendlyName='' then
    Result:=Name
  else
    Result:=FUserFriendlyName;
end;

procedure TOrGrammarItem.InsertItem(Index: Integer; Item: TAndGrammarItem);
begin
  FSubItems.Insert(Index,Item);
end;

function TOrGrammarItem.IsLeaf: Boolean;
begin
  Result:=False;
end;

procedure TOrGrammarItem.SetName(const Value: string);
begin
  FName:=Value;
  FGrammar.FNodes[ID]:=FName;
end;

procedure TOrGrammarItem.SetUserFriendlyName(const Value: string);
begin
  FUserFriendlyName:=Value;
end;

{ TGrammar }

function TGrammar.BuildAnalyseTable: TAnalyseTable;
var
  Collection:TNullItemSetCollection;
  Item:TNullItemSet;
  NullLaw:TAndGrammarItem;
  NullAxiom:TOrGrammarItem;
  a,b:Integer;
  t:Boolean;
begin
  BuildFirst;
  BuildNext;
  Collection:=TNullItemSetCollection.Create(True);
  NullLaw:=TAndGrammarItem.Create([FMain]);
  NullAxiom:=TOrGrammarItem.Create(Self,[NullLaw],'NullAxiom');
  Item:=TNullItemSet.Create;
  Item.Add(NullLaw,0,Self);
  Item.Close;
  Collection.Add(Item);
  PushProgress;
  try
    repeat
      UpdateProgress(0,'Enumerating states ('+IntToStr(Collection.Count)+' found so far)');
      t:=False;
      for a:=0 to Collection.Count-1 do
        for b:=0 to FNodes.Count-1 do begin
          Item:=Collection[a].Transition(FNodes.Objects[b] as TNodeGrammarItem);
          if Item.IsEmpty then begin
            Item.Destroy;
            Continue;
          end;
          if Collection.Add(Item) then
            t:=True
          else
            Item.Destroy;
        end;
    until not t;
  finally
    PopProgress;
  end;
  Result:=TAnalyseTable.Create(Collection,Self);
  NullAxiom.Destroy;
end;

procedure TGrammar.BuildFirst;
var
  a,b:Integer;
  t:Boolean;
begin
  for a:=0 to FNodes.Count-1 do
    with FNodes.Objects[a] as TNodeGrammarItem do
      if IsLeaf then
        FFirst.Add(FNodes.Objects[a]);
  repeat
    t:=False;
    for a:=0 to FLaws.Count-1 do
      with (FLaws[a] as TOrGrammarItem) do
        for b:=0 to Count-1 do
          with Item[b] do
            t:=FFirst.AddSet((FSubItems[0] as TNodeGrammarItem).FFirst) or t;
  until not t;
end;

procedure TGrammar.BuildNext;
var
  a,b,c:Integer;
  t:Boolean;
//  n:TNodeGrammarItem;
begin
  Assert(Assigned(FMain));
  FMain.FNext.Add(FEOF);
  for a:=0 to FLaws.Count-1 do
    with FLaws[a] as TOrGrammarItem do
      for b:=0 to Count-1 do
        with Item[b].FSubItems do
          for c:=0 to Count-2 do
            with Items[c] as TNodeGrammarItem do
              if not IsLeaf then
                with Items[c] as TOrGrammarItem do
                  with Items[c+1] as TNodeGrammarItem do
                    FNext.AddSet(FFirst);




{          for c:=0 to Count-2 do begin
            n:=Items[c] as TNodeGrammarItem;
            if not n.IsLeaf then
              with n as TOrGrammarItem do
                with (FLaws[a] as TOrGrammarItem).FSubItems[c+1] as TNodeGrammarItem do
                  FNext.AddSet(FFirst);
          end;}
  repeat
    t:=False;
    for a:=0 to FLaws.Count-1 do
      with FLaws[a] as TOrGrammarItem do 
        for b:=0 to Count-1 do
          with Item[b].FSubItems do
            with Items[Count-1] as TNodeGrammarItem do
              if not IsLeaf then
                with Items[Item[b].FSubItems.Count-1] as TOrGrammarItem do
                  t:=FNext.AddSet((FLaws[a] as TOrGrammarItem).FNext) or t;
  until not t;
end;

constructor TGrammar.Create(ATokenList: TStringList);
var
  a:Integer;
begin
  inherited Create;
  FNodes:=TStringList.Create;
  FNodes.CaseSensitive:=True;
  FLaws:=TObjectList.Create(False);
  for a:=0 to ATokenList.Count-1 do
    TTokenGrammarItem.Create(Self,ATokenList[a]);
  FEOF:=TEOFGrammarItem.Create(Self);
end;

destructor TGrammar.Destroy;
begin
  FLaws.Destroy;
  FNodes.Destroy;
  inherited;
end;

function TGrammar.GetItem(Name: string): TNodeGrammarItem;
var
  a:Integer;
begin
  Result:=nil;
  a:=FNodes.IndexOf(Name);
  if a>-1 then
    Result:=FNodes.Objects[a] as TNodeGrammarItem
  else
    Assert(False,'"'+Name+'" is not defined.');
end;

function TGrammar.GetLawCount: Integer;
begin
  Result:=FLaws.Count;
end;

function TGrammar.GetLawItem(Name: string): TOrGrammarItem;
var
  a:Integer;
begin
  Result:=nil;
  a:=FNodes.IndexOf(Name);
  if a>-1 then
    Result:=TOrGrammarItem(FNodes.Objects[a]);
  if Assigned(Result) and not (TObject(Result) is TOrGrammarItem) then
    Result:=nil;
  Assert(Assigned(Result),'Law "'+Name+'" is not defined.');
end;

function TGrammar.GetTokenItem(Name: string): TTokenGrammarItem;
var
  a:Integer;
begin
  Result:=nil;
  for a:=0 to FNodes.Count-1 do
    if (FNodes.Objects[a] is TTokenGrammarItem) and ((FNodes.Objects[a] as TTokenGrammarItem).Name=Name) then begin
      Result:=FNodes.Objects[a] as TTokenGrammarItem;
      Exit;
    end;
  Assert(False,'Token "'+Name+'" is not defined.');
end;

function TGrammar.IndexOfLaw(Law: TAndGrammarItem): Integer;
var
  a,b,c:Integer;
begin
  Result:=-1;
  c:=0;
  for a:=0 to FLaws.Count-1 do
    with FLaws[a] as TOrGrammarItem do
      for b:=0 to Count-1 do begin
        if Item[b]=Law then begin
          Result:=c;
          Exit;
        end;
        Inc(c);
      end;
end;

{ TLexerItemGrammar }

function AreTokenEquivalent(t1,t2:TTokenGrammarItem;LawList:TStringList):Boolean;
var
  a,b,c,d:Integer;
  l:TObjectList;
  t:Boolean;
begin
  Result:=True;
  l:=TObjectList.Create(False);
  for a:=0 to LawList.Count-1 do
    with LawList.Objects[a] as TOrGrammarItem do
      for b:=0 to Count-1 do begin
        t:=False;
        with Item[b] do
          for c:=0 to LawCount-1 do
            t:=t or (Law[c]=t1) or (Law[c]=t2);
        if not t then
          Continue;
        l.Clear;
        for c:=0 to Count-1 do
          if Item[b].LawCount=Item[c].LawCount then
            l.Add(Item[c]);
        with Item[b] do
          for c:=0 to LawCount-1 do
            for d:=l.Count-1 downto 0 do
              if not ((((l[d] as TAndGrammarItem).Law[c]=Law[c]) and (Law[c]<>t1) and (Law[c]<>t2)) or
                      (((l[d] as TAndGrammarItem).Law[c]=t1) and (Law[c]=t2)) or
                      (((l[d] as TAndGrammarItem).Law[c]=t2) and (Law[c]=t1))) then
                l.Delete(d);
        if l.Count=0 then
          Result:=False;
      end;
  l.Destroy;
end;

constructor TLexerItemGrammar.Create(TokenList, LawList: TStringList);
var
  a,b,c,d:Integer;
  i:TAndGrammarItem;
  j:TTokenGrammarItem;
  EquivalentClasses,l:TObjectList;
  NewTokens:TStringList;
  s:string;
  t:Boolean;

  function FindTokenClass(i:TTokenGrammarItem):TTokenGrammarItem;
  var
    a,b:Integer;
  begin
    Result:=nil;
    for a:=0 to EquivalentClasses.Count-1 do
      with EquivalentClasses[a] as TObjectList do
        for b:=0 to Count-1 do
          if Items[b]=i then
            Result:=FNodes.Objects[a] as TTokenGrammarItem;
  end;

  procedure Substitute(FromItem:TOrGrammarItem;ToItem:TNodeGrammarItem);
  var
    a,b,c:Integer;
  begin
    for a:=0 to FLaws.Count-1 do
      with FLaws[a] as TOrGrammarItem do
        for b:=0 to Count-1 do
          with Item[b] do
            for c:=0 to LawCount-1 do
              if Law[c]=FromItem then
                FSubItems[c]:=ToItem;
  end;

begin
  EquivalentClasses:=TObjectList.Create(True);
  l:=TObjectList.Create(False);
  for a:=0 to TokenList.Count-1 do
    l.Add(TokenList.Objects[a]);
  EquivalentClasses.Add(l);
  repeat
    l:=TObjectList.Create(False);
    with EquivalentClasses[EquivalentClasses.Count-1] as TObjectList do begin
      j:=Items[0] as TTokenGrammarItem;
      for b:=Count-1 downto 1 do begin
        if not AreTokenEquivalent(j,Items[b] as TTokenGrammarItem,LawList) then begin
          l.Add(Items[b]);
          Delete(b);
        end;
      end;
    end;
    if l.Count>0 then
      EquivalentClasses.Add(l);
  until l.Count=0;
  l.Destroy;
  FillChar(FCharMap,SizeOf(FCharMap),0);
  NewTokens:=TStringList.Create;
  NewTokens.CaseSensitive:=True;
  for a:=0 to EquivalentClasses.Count-1 do
    with EquivalentClasses[a] as TObjectList do begin
      s:='';
      for b:=0 to Count-1 do
        with Items[b] as TTokenGrammarItem do
          if s='' then
            s:=Name
          else
            s:=s+','+Name;
      NewTokens.Add('{'+s+'}');
    end;
  inherited Create(NewTokens);
  NewTokens.Destroy;
  for a:=0 to EquivalentClasses.Count-1 do
    with EquivalentClasses[a] as TObjectList do
      for b:=0 to Count-1 do
        with Items[b] as TTokenGrammarItem do
          FCharMap[Chr(ID)]:=FNodes.Objects[a] as TTokenGrammarItem;
  for a:=0 to 255 do
    if not Assigned(FCharMap[Chr(a)]) then
      FCharMap[Chr(a)]:=FEOF;
  for a:=0 to LawList.Count-1 do
    TOrGrammarItem.Create(Self,[],LawList[a]);
  FMain:=FLaws[0] as TOrGrammarItem;
  FStored:=(LawList.Objects[0] as TOrGrammarItem).Storable;
  for a:=0 to LawList.Count-1 do
    with LawList.Objects[a] as TOrGrammarItem do
      for b:=0 to Count-1 do begin
        i:=TAndGrammarItem.Create([]);
        with Self.Item[Name] as TOrGrammarItem do
          InsertItem(Count,i);
        with Item[b] do
          for c:=0 to LawCount-1 do
            with Law[c] do
              if IsLeaf then
                i.InsertSubItem(i.LawCount,FindTokenClass(Law[c] as TTokenGrammarItem))
              else
                i.InsertSubItem(i.LawCount,Self.LawItem[Name]);
      end;
  EquivalentClasses.Destroy;
  for a:=0 to FLaws.Count-1 do
    with FLaws[a] as TOrGrammarItem do
      for b:=0 to Count-1 do
        for c:=Count-1 downto b+1 do begin
          t:=Item[b].LawCount=Item[c].LawCount;
          if t then
            for d:=0 to Item[b].LawCount-1 do
              t:=t and (Item[b].Law[d]=Item[c].Law[d]);
          if t then
            FSubItems.Delete(c);
        end;
  for a:=FLaws.Count-1 downto 0 do
    with FLaws[a] as TOrGrammarItem do
      if Count=1 then
        with Item[0] do
          if (LawCount=1) and ((FLaws[a]<>FMain) or not Law[0].IsLeaf) then begin
            if FLaws[a]=FMain then begin
              FMain:=Law[0] as TOrGrammarItem;
              FMain.Name:=(FLaws[a] as TOrGrammarItem).Name;
            end;
            Substitute(FLaws[a] as TOrGrammarItem,Law[0]);
            FLaws[a].Destroy;
          end;
end;

{ TLexer }

function TLexer.BuildLexerDefinition: TLexerDefinition;
var
  a,b,c:Integer;
  d:Word;
  t:TAnalyseTable;
  u:TLexerTable;
begin
  BuildSeparateGrammars;
  Result.LexerTablesCount:=FRootLexerItems.Count;
  GetMem(Result.LexerTables,FRootLexerItems.Count*SizeOf(TLexerTable));
  d:=0;
  PushProgress;
  try
    for a:=0 to FRootLexerItems.Count-1 do begin
      UpdateProgress(a/FRootLexerItems.Count,'LEXER: building table '+IntToStr(a)+'/'+IntToStr(FRootLexerItems.Count)+' ('+FRootLexerItems[a]+')');
      t:=(FRootLexerItems.Objects[a] as TGrammar).BuildAnalyseTable;
      u.Height:=t.RowCount;
      u.Width:=t.ColCount;
      GetMem(u.TableItems,t.RowCount*t.ColCount*SizeOf(TTableItem));
      with FRootLexerItems.Objects[a] as TGrammar do
        for b:=0 to FNodes.Count-1 do
          with FNodes.Objects[b] as TNodeGrammarItem do
            if not IsLeaf then
              with FNodes.Objects[b] as TOrGrammarItem do
                Assert(Count>0);
      for b:=0 to t.RowCount-1 do
        for c:=0 to t.ColCount-1 do
          case t[b,c].ItemType of
            tipAccept:u.TableItems[b*t.ColCount+c].ItemType:=titAccept;
            tipError:u.TableItems[b*t.ColCount+c].ItemType:=titError;
            tipShift:begin
              u.TableItems[b*t.ColCount+c].ItemType:=titShift;
              u.TableItems[b*t.ColCount+c].Value:=t[b,c].Count;
            end;
            tipReduce:begin
              u.TableItems[b*t.ColCount+c].ItemType:=titReduce;
              u.TableItems[b*t.ColCount+c].Value:=t[b,c].ID+d;
            end;
            tipGoto:begin
              u.TableItems[b*t.ColCount+c].ItemType:=titGoto;
              u.TableItems[b*t.ColCount+c].Value:=t[b,c].Index;
            end;
          end;
      with FRootLexerItems.Objects[a] as TLexerItemGrammar do begin
        u.EOFId:=FNodes.IndexOfObject(FEOF);
        for b:=0 to 255 do
          u.CharMap[Chr(b)]:=FCharMap[Chr(b)].ID;
        u.Stored:=Stored;
      end;
      t.Destroy;
      with FRootLexerItems.Objects[a] as TGrammar do
        for b:=0 to FLaws.Count-1 do
          with FLaws[b] as TOrGrammarItem do
            Inc(d,Count);
      Result.LexerTables[a]:=u;
    end;
  finally
    PopProgress;
  end;
  Result.LexerEOFId:=FRootLexerItems.Count;
  Result.LexerLawsCount:=d;
  GetMem(Result.LexerLaws,d*SizeOf(TLexerLaw));
  d:=0;
  for a:=0 to FRootLexerItems.Count-1 do
    with FRootLexerItems.Objects[a] as TGrammar do
      for b:=0 to FLaws.Count-1 do
        with FLaws[b] as TOrGrammarItem do
          for c:=0 to Count-1 do
            with Item[c] do begin
              Result.LexerLaws[d].ChildsCount:=LawCount;
              Result.LexerLaws[d].ID:=FNodes.IndexOfObject(FLaws[b]);
              Inc(d);
            end;
  Result.StaticResource:=False;
end;

function TLexer.BuildPascalLexerDefinitions(Prefix, CRLF: string): string;
begin
  Result:=ConvertLexerDefinitionToPascalDeclaration(BuildLexerDefinition,Prefix,'',CRLF);
end;

procedure TLexer.BuildSeparateGrammars;
var
  l1,l2:TStringList;
  a,b,c,d,e:Integer;
begin
  if FBuilt then
    Exit;
  MakeProper;
  l1:=TStringList.Create;
  l2:=TStringList.Create;
  for a:=0 to FRootLexerItems.Count-1 do begin
    l1.Clear;
    l2.Clear;
    l2.AddObject(FRootLexerItems[a],LawItem[FRootLexerItems[a]]);
    repeat
      e:=l2.Count;
      for b:=l2.Count-1 downto 0 do
        with l2.Objects[b] as TOrGrammarItem do
          for c:=0 to Count-1 do
            with Item[c] do
              for d:=0 to LawCount-1 do
                with Law[d] do
                  if IsLeaf then begin
                    if l1.IndexOf(Name)=-1 then
                      l1.AddObject(Name,Law[d])
                  end else begin
                    if l2.IndexOf(Name)=-1 then
                      l2.AddObject(Name,Law[d]);
                  end;
    until l2.Count=e;
    FRootLexerItems.Objects[a]:=TLexerItemGrammar.Create(l1,l2);
  end;
  l1.Destroy;
  l2.Destroy;
  FBuilt:=True;
end;

constructor TLexer.Create(ALaws:TStringList);
var
  a:Integer;
  l:TStringList;
begin
  l:=TStringList.Create;
  for a:=0 to 255 do
    l.Add('#'+IntToStr(a));
  inherited Create(l);
  l.Destroy;
  FRootLexerItems:=TStringList.Create;
  FRootLexerItems.CaseSensitive:=True;
  FRootLexerItems.AddStrings(ALaws);
  for a:=0 to FRootLexerItems.Count-1 do
    TOrGrammarItem.Create(Self,[],FRootLexerItems[a]);
end;

destructor TLexer.Destroy;
begin
  FRootLexerItems.Destroy;
  inherited;
end;

procedure AddNullSubstitution(o:TOrGrammarItem;a:TAndGrammarItem;Index,Placement:Integer);
var
  b,c:Integer;
  t:Boolean;
  i:TAndGrammarItem;
begin
  t:=False;
  for b:=0 to o.Count-1 do
    if o.Item[b].LawCount=a.LawCount-1 then begin
      t:=True;
      for c:=0 to a.LawCount-2 do
        if c<Index then
          t:=t and (o.Item[b].Law[c]=a.Law[c])
        else
          t:=t and (o.Item[b].Law[c]=a.Law[c+1]);
      if t then
        Break;
    end;
  if not t then begin
    i:=TAndGrammarItem.Create([]);
    for b:=0 to a.LawCount-1 do
      if b<>Index then
        i.InsertSubItem(i.LawCount,a.Law[b]);
    o.InsertItem(Placement,i);
  end;
end;

procedure TLexer.MakeProper;
var
  a,b,c,d:Integer;
  l:TObjectList;
begin
  l:=TObjectList.Create(False);
  repeat
    d:=l.Count;
    for a:=0 to FLaws.Count-1 do
      with FLaws[a] as TOrGrammarItem do
        for b:=Count-1 downto 0 do begin
          with Item[b] do
            for c:=0 to LawCount-1 do
              if l.IndexOf(Law[c])>-1 then
                AddNullSubstitution(FLaws[a] as TOrGrammarItem,Item[b],c,b+1);
          if Item[b].LawCount=0 then
            if l.IndexOf(FLaws[a])=-1 then
              l.Add(FLaws[a]);
        end;
  until l.Count=d;
  l.Destroy;
  for a:=0 to FLaws.Count-1 do
    with FLaws[a] as TOrGrammarItem do begin
      for b:=Count-1 downto 0 do
        if Item[b].LawCount=0 then
           FSubItems.Delete(b);
      if Count=0 then
        EmitError('Single null production not allowed.');
    end;
end;

{ TParser }

function TParser.BuildIParser: IParser;
var
  i:ILexer;
begin
  i:=TDefaultLexer.Create(FLexer.BuildLexerDefinition);
  Result:=TDefaultParser.Create(i,BuildParserDefinition);
end;

function TParser.BuildLexerDefinition: TLexerDefinition;
begin
  Result:=FLexer.BuildLexerDefinition;
end;

function TParser.BuildParserDefinition: TParserDefinition;
var
  a,b,c,d:Integer;
begin
  FLexer.BuildSeparateGrammars;
  with BuildAnalyseTable do begin
    Result.TableWidth:=ColCount;
    Result.TableHeight:=RowCount;
    GetMem(Result.ParserTable,ColCount*RowCount*Sizeof(TTableItem));
    for a:=0 to RowCount-1 do
      for b:=0 to ColCount-1 do
        with Item[a,b] do
          case ItemType of
            tipAccept:Result.ParserTable[a*ColCount+b].ItemType:=titAccept;
            tipError:Result.ParserTable[a*ColCount+b].ItemType:=titError;
            tipShift:begin
              Result.ParserTable[a*ColCount+b].ItemType:=titShift;
              Result.ParserTable[a*ColCount+b].Value:=Count;
            end;
            tipReduce:begin
              Result.ParserTable[a*ColCount+b].ItemType:=titReduce;
              Result.ParserTable[a*ColCount+b].Value:=ID;
            end;
            tipGoto:begin
              Result.ParserTable[a*ColCount+b].ItemType:=titGoto;
              Result.ParserTable[a*ColCount+b].Value:=Index;
            end;
          end;
    b:=0;
    for a:=0 to FNodes.Count-1 do
      with FNodes.Objects[a] as TNodeGrammarItem do
        if not IsLeaf then
          with (FNodes.Objects[a] as TOrGrammarItem) do
            Inc(b,Count);
    Result.ParserLawsCount:=b;
    GetMem(Result.ParserLaws,b*SizeOf(TParserLaw));
    b:=0;
    for a:=0 to FNodes.Count-1 do
      with FNodes.Objects[a] as TNodeGrammarItem do
        if not IsLeaf then
          with (FNodes.Objects[a] as TOrGrammarItem) do begin
            for c:=0 to Count-1 do
              with Item[c] do begin
                Result.ParserLaws[b+c].ChildsCount:=LawCount;
                Result.ParserLaws[b+c].ID:=a;
                Result.ParserLaws[b+c].UsageCount:=UsageCount;
                GetMem(Result.ParserLaws[b+c].Usage,LawCount*SizeOf(Boolean));
                for d:=0 to LawCount-1 do
                  Result.ParserLaws[b+c].Usage[d]:=UsedLawItem[d];
              end;
            Inc(b,Count)
          end;
    Result.TokenNamesCount:=FNodes.Count;
    GetMem(Result.TokenNames,FNodes.Count*SizeOf(string));
    ZeroMemory(Result.TokenNames,FNodes.Count*SizeOf(string));
    for a:=0 to FNodes.Count-1 do
      with FNodes.Objects[a] as TNodeGrammarItem do
        if FLexer.FNodes.IndexOf(Name)>-1 then
          Result.TokenNames[a]:=FLexer.Item[Name].UserFriendlyName
        else
          Result.TokenNames[a]:=UserFriendlyName;
    Destroy;
  end;
  Result.StaticResource:=False;
end;

function TParser.BuildPascalClassDeclaration(Prefix, CRLF: string): string;
var
  a:Integer;
begin
  Result:='type'+Crlf+Crlf+
          '  T'+Prefix+'Executer=class(TDefaultExecuter)'+CRLF+
          '  private'+CRLF;
  for a:=0 to FLaws.Count-1 do
    with FLaws[a] as TOrGrammarItem do
      if FType='' then
        Result:=Result+
                '    procedure ExecuteNode'+IntToStr(a)+'(Node:TParserNode);'+CRLF
      else
        Result:=Result+
                '    function ExecuteNode'+IntToStr(a)+'(Node:TParserNode):'+FType+';'+CRLF;
  Result:=Result+'  protected'+CRLF+
                 '    class function GetParser:IParser;override;'+CRLF+
                 '  public'+CRLF;
  if FMain.FType='' then
    Result:=Result+
            '    procedure Execute(Handler:IExecuterExceptionHandler);'+CRLF
  else
    Result:=Result+
            '    function Execute(Handler:IExecuterExceptionHandler):'+FMain.FType+';'+CRLF;
  Result:=Result+
          '  end;'+CRLF+CRLF;
end;

function TParser.BuildPascalClassImplementation(Prefix,
  CRLF: string): string;
var
  a,b,c,d,e:Integer;
  s,t,u,v:string;
begin
  c:=0;
  for a:=0 to FLaws.Count-1 do
    with FLaws[a] as TOrGrammarItem do begin
      if FType='' then
        Result:=Result+
                'procedure T'+Prefix+'Executer.ExecuteNode'+IntToStr(a)+'(Node:TParserNode);'+CRLF
      else
        Result:=Result+
                'function T'+Prefix+'Executer.ExecuteNode'+IntToStr(a)+'(Node:TParserNode):'+FType+';'+CRLF;
      if FDeclarationBlock<>'' then
        Result:=Result+
                FDeclarationBlock+CRLF;
      Result:=Result+
              'begin'+CRLF+
              '  FErrorOffset:=Node.CharOffset;'+CRLF+
              '  case Node.ID of'+CRLF;
      for b:=0 to Count-1 do
        with Item[b] do begin
          s:='    '+IntToStr(c)+':begin'+CRLF+
             FImplementationBlock+CRLF+
             '    end;'+CRLF;
          s:=AnsiReplaceStr(s,'##','Result');
          s:=AnsiReplaceStr(s,'@@','Node.CharOffset');
          s:=AnsiReplaceStr(s,'§§','Node.CharLength');
          e:=0;
          for d:=0 to LawCount-1 do begin
            t:='';
            if UsedLawItem[d] then begin
              if Law[d].IsLeaf and (Law[d]<>FError) then begin
                with FLexer.FRootLexerItems.Objects[Law[d].ID] as TLexerItemGrammar do
                  if Stored then begin
                    if e=0 then
                      t:='ExecuteLeaf(Node.SubNodes^)'
                    else
                      t:='ExecuteLeaf(ExtractSubParserNode(Node,'+IntToStr(e)+'))';
                    if e=0 then
                      u:='Node.SubNodes^.CharOffset'
                    else
                      u:='ExtractSubParserNode(Node,'+IntToStr(e)+').CharOffset';
                    if e=0 then
                      v:='Node.SubNodes^.CharLength'
                    else
                      v:='ExtractSubParserNode(Node,'+IntToStr(e)+').CharLength';
                  end;
              end else begin
                if d=0 then
                  t:='ExecuteNode'+IntToStr(FLaws.IndexOf(Law[d]))+'(Node.SubNodes^)'
                else
                  t:='ExecuteNode'+IntToStr(FLaws.IndexOf(Law[d]))+'(ExtractSubParserNode(Node,'+IntToStr(e)+'))';
                if d=0 then
                  u:='Node.SubNodes^.CharOffset'
                else
                  u:='ExtractSubParserNode(Node,'+IntToStr(e)+').CharOffset';
                if d=0 then
                  v:='Node.SubNodes^.CharLength'
                else
                  v:='ExtractSubParserNode(Node,'+IntToStr(e)+').CharLength';
              end;
              Inc(e);
            end;
            if (t='') and (Pos('#'+IntToStr(d+1),s)>0) then
              EmitWarning('Parameter #'+IntToStr(d+1)+' is used but not defined in the code section number '+IntToStr(d+1)+' of '+Name+'.');
            s:=AnsiReplaceStr(s,'#'+IntToStr(d+1),t);
            if (t='') and (Pos('@'+IntToStr(d+1),s)>0) then
              EmitWarning('Parameter position @'+IntToStr(d+1)+' is used but not defined in the code section number '+IntToStr(d+1)+' of '+Name+'.');
            s:=AnsiReplaceStr(s,'@'+IntToStr(d+1),u);
            if (t='') and (Pos('§'+IntToStr(d+1),s)>0) then
              EmitWarning('Parameter length §'+IntToStr(d+1)+' is used but not defined in the code section number '+IntToStr(d+1)+' of '+Name+'.');
            s:=AnsiReplaceStr(s,'§'+IntToStr(d+1),u);
          end;
          Result:=Result+s;
          Inc(c);
        end;
      Result:=Result+
              '  else'+CRLF+
              '    raise TObject.Create;'+CRLF+
//              '    Assert(False,''Fatal parser error'');'+CRLF+
              '  end;'+CRLF+
              'end;'+CRLF+CRLF;
    end;
  if FMain.FType='' then
    Result:=Result+
            'procedure T'+Prefix+'Executer.Execute(Handler:IExecuterExceptionHandler);'+CRLF+
            'begin'+CRLF+
            '  FHandler:=Handler;'+CRLF+
            '  try'+CRLF+
            '    try'+CRLF+
            '      ExecuteNode'+IntToStr(FLaws.IndexOf(FMain))+'(FNode);'+CRLF+
            '    except'+CRLF+
            '    on e:TObject do'+CRLF+
            '      if (not Assigned(FHandler)) or (not FHandler.RaiseException(e,FErrorOffset)) then'+CRLF+
            '        raise;'+CRLF+
            '    else'+CRLF+
            '      if (not Assigned(FHandler)) or (not FHandler.RaiseUnknownException(FErrorOffset)) then'+CRLF+
            '        raise;'+CRLF+
            '    end;'+CRLF+
            '  finally'+CRLF+
            '    FHandler:=nil;'+CRLF+
            '  end;'+CRLF+
            'end;'+CRLF+CRLF
  else
    Result:=Result+
            'function T'+Prefix+'Executer.Execute(Handler:IExecuterExceptionHandler):'+FMain.FType+';'+CRLF+
            'begin'+CRLF+
            '  FHandler:=Handler;'+CRLF+
            '  try'+CRLF+
            '    try'+CRLF+
            '      Result:=ExecuteNode'+IntToStr(FLaws.IndexOf(FMain))+'(FNode);'+CRLF+
            '    except'+CRLF+
            '    on e:TObject do'+CRLF+
            '      if (not Assigned(FHandler)) or (not FHandler.RaiseException(e,FErrorOffset)) then'+CRLF+
            '        raise;'+CRLF+
            '    else'+CRLF+
            '      if (not Assigned(FHandler)) or (not FHandler.RaiseUnknownException(FErrorOffset)) then'+CRLF+
            '        raise;'+CRLF+
            '    end;'+CRLF+
            '  finally'+CRLF+
            '    FHandler:=nil;'+CRLF+
            '  end;'+CRLF+
            'end;'+CRLF+CRLF;
  Result:=Result+
          'class function T'+Prefix+'Executer.GetParser:IParser;'+CRLF+
          'begin'+CRLF+
          '  Result:=TDefaultParser.Create(TDefaultLexer.Create('+Prefix+'LexerDefinition),'+Prefix+'ParserDefinition);'+CRLF+
          'end;'+CRLF+CRLF
end;

function TParser.BuildPascalDefinitions(Prefix, CRLF: string): string;
begin
  Result:=ConvertLexerDefinitionToPascalDeclaration(FLexer.BuildLexerDefinition,Prefix,'',CRLF);
  Result:=Result+ConvertParserDefinitionToPascalDeclaration(BuildParserDefinition,Prefix,'',CRLF);
end;

function TParser.BuildPascalLexerDefinitions(Prefix, CRLF: string): string;
begin
  Result:=ConvertLexerDefinitionToPascalDeclaration(FLexer.BuildLexerDefinition,Prefix,'',CRLF);
end;

function TParser.BuildPascalParserDefinitions(Prefix,
  CRLF: string): string;
begin
  Result:=ConvertParserDefinitionToPascalDeclaration(BuildParserDefinition,Prefix,'',CRLF);
end;

constructor TParser.Create(ALexer: TLexer; APreImplementationBlock,
  APostImplementationBlock: string);
begin
  inherited Create(ALexer.FRootLexerItems);
  FError:=TErrorGrammarItem.Create(Self);
  FLexer:=ALexer;
  FPreImplementationBlock:=APreImplementationBlock;
  FPostImplementationBlock:=APostImplementationBlock;
end;

end.
