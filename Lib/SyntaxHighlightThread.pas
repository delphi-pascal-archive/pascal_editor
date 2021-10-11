unit SyntaxHighlightThread;

interface

uses
  SysUtils,Windows,Classes,Controls,Graphics,SyntaxHighlighter,SyncObjs;

type
  IHighlightControl=interface
    ['{B1E3B5B2-CB8B-4D89-8AA6-81FDA48E5A89}']
    procedure InvalidateLine(Index:Integer);
    procedure TokenizeLineClass(const LastLineClass:TLineClass;var NewLineClass:TLineClass;const TextBuffer:PChar;const TextLength:Integer);
    procedure TokenizeLine(const LineClass:TLineClass;const TextBuffer:PChar;const TextLength:Integer;CharClass:PCharClassArray);
    procedure ContentModified;
  end;

  TLineData=record
    ClassValid:Boolean;
    LineClass,PreviousLineClass:TLineClass;
    LineData:PCharClassArray;
    LineLength:Integer;
  end;
  TLineDataArray=array[0..$FFFF] of TLineData;
  PLineDataArray=^TLineDataArray;

  TLinesData=record
    Count:Integer;
    Lines:PLineDataArray;
  end;

  TSyntaxHighlightThread=class(TThread)
  private
    FStrings:TStringList;
    FControl:IHighlightControl;
    FLinesData:TLinesData;
    FSection:TCriticalSection;
    FEvent:TEvent;
    FLastStrings:TStrings;

    procedure StringsChanging(Sender:TObject);
    procedure StringsChanged(Sender:TObject);
    function GetCharClass(Line, Column: Integer): TCharClass;
  protected
    procedure Execute;override;
    procedure UpdateState;
  public
    constructor Create(AStrings:TStringList;AControl:IHighlightControl);

    procedure Update;

    procedure Lock;
    procedure UnLock;

    property CharClass[Line,Column:Integer]:TCharClass read GetCharClass;

    procedure Terminate;

    destructor Destroy;override;
  end;

implementation

{ TSyntaxHighlightThread }

constructor TSyntaxHighlightThread.Create(AStrings: TStringList;
  AControl: IHighlightControl);
begin
  inherited Create(True);
  IsMultiThread:=True;
  FControl:=AControl;
  FStrings:=AStrings;
  FStrings.OnChange:=StringsChanged;
  FStrings.OnChanging:=StringsChanging;
  FLastStrings:=TStringList.Create;
  FEvent:=TEvent.Create(nil,False,True,'');
  FSection:=TCriticalSection.Create;
  Priority:=tpIdle;
end;

destructor TSyntaxHighlightThread.Destroy;
begin
  FLastStrings.Destroy;
  FSection.Destroy;
  FEvent.Destroy;
  FStrings.OnChange:=nil;
  FStrings.OnChanging:=nil;
  inherited;
end;

procedure TSyntaxHighlightThread.Execute;
begin
  while not Terminated do begin
    UpdateState;
    FEvent.WaitFor(INFINITE);
  end;
end;

function TSyntaxHighlightThread.GetCharClass(Line,
  Column: Integer): TCharClass;
begin
  Lock;
  try
    if (Line<0) or (Line>=FLastStrings.Count) then
      Result:=0
    else begin
      if (Column<0) or (Column>=FLinesData.Lines[Line].LineLength) then
        Result:=0
      else
        Result:=FLinesData.Lines[Line].LineData[Column];
    end;
  finally
    UnLock;
  end;
end;

procedure TSyntaxHighlightThread.Lock;
begin
  FSection.Enter;
end;

procedure TSyntaxHighlightThread.StringsChanged(Sender: TObject);
begin
  FEvent.SetEvent;
  FControl.ContentModified;
  FSection.Leave;
end;

procedure TSyntaxHighlightThread.StringsChanging(Sender: TObject);
begin
  FSection.Enter;
end;

procedure TSyntaxHighlightThread.Terminate;
begin
  inherited;
  Update;
end;

procedure TSyntaxHighlightThread.UnLock;
begin
  FSection.Leave;
end;

procedure TSyntaxHighlightThread.Update;
begin
  FEvent.SetEvent;
end;

procedure TSyntaxHighlightThread.UpdateState;
var
  a,b,c:Integer;
  l,m:TLineClass;
begin
  FSection.Enter;
  try
    b:=0;
    while (b<FLastStrings.Count) and
          (b<FStrings.Count) and
          (FLastStrings[b]=FStrings[b]) do
      Inc(b);
    Dec(b);
    c:=1;
    while (FLastStrings.Count-c>b) and
          (FStrings.Count-c>=0) and
          (FLastStrings[FLastStrings.Count-c]=FStrings[FStrings.Count-c]) do
      Inc(c);
    Dec(c);
    if FStrings.Count>=FLastStrings.Count then begin
      ReallocMem(FLinesData.Lines,FStrings.Count*SizeOf(TLineData));
      CopyMemory(@FLinesData.Lines[FStrings.Count-c],@FLinesData.Lines[FLastStrings.Count-c],c*SizeOf(TLineData));
      for a:=FLastStrings.Count-c to FStrings.Count-c-1 do
        with FLinesData.Lines[a] do begin
          ClassValid:=False;
          LineClass:=0;
          PreviousLineClass:=0;
          LineLength:=Length(FStrings[a]);
          GetMem(LineData,LineLength*SizeOf(TCharClass));
          ZeroMemory(LineData,LineLength*SizeOf(TCharClass));
        end;
    end else begin
      for a:=FStrings.Count-c to FLastStrings.Count-c-1 do
        with FLinesData.Lines[a] do
          FreeMem(LineData);
      CopyMemory(@FLinesData.Lines[FStrings.Count-c],@FLinesData.Lines[FLastStrings.Count-c],c*SizeOf(TLineData));
      ReallocMem(FLinesData.Lines,FStrings.Count*SizeOf(TLineData));
    end;
    FLastStrings.Assign(FStrings);
    FLinesData.Count:=FStrings.Count;
    for a:=b+1 to FStrings.Count-c-1 do
      with FLinesData.Lines[a] do begin
        ClassValid:=False;
        LineClass:=0;
        if LineLength<>Length(FStrings[a]) then begin
          LineLength:=Length(FStrings[a]);
          ReallocMem(LineData,LineLength*SizeOf(TCharClass));
        end;
      end;
    a:=b+1;
    if a=0 then
      l:=0
    else
      l:=FLinesData.Lines[a-1].LineClass;
    while (a<FStrings.Count) and (not FLinesData.Lines[a].ClassValid) do
      with FLinesData.Lines[a] do begin
        m:=0;
        FControl.TokenizeLineClass(l,m,PChar(FStrings[a]),LineLength);
        ZeroMemory(LineData,LineLength*SizeOf(TCharClass));
        FControl.TokenizeLine(l,PChar(FStrings[a]),LineLength,LineData);
        FControl.InvalidateLine(a);
        if a<FStrings.Count-1 then
          FLinesData.Lines[a+1].ClassValid:=FLinesData.Lines[a+1].ClassValid and (m=FLinesData.Lines[a+1].PreviousLineClass);
        ClassValid:=True;
        PreviousLineClass:=l;
        LineClass:=m;
        l:=m;
        Inc(a);
      end;
  finally
    FSection.Leave;
  end;
end;

end.
