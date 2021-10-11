unit COW_Utils;

interface

uses
  SysUtils,Classes,COW_RunTime;

function ConvertLexerDefinitionToPascalDeclaration(x:TLexerDefinition;Prefix:string='';Indent:string='';CRLF:string=#10#13):string;
function ConvertParserDefinitionToPascalDeclaration(x:TParserDefinition;Prefix:string='';Indent:string='';CRLF:string=#10#13):string;
procedure WriteLexerDefinitionToStream(x:TLexerDefinition;s:TStream);
function ReadLexerDefinitionFromStream(s:TStream):TLexerDefinition;
procedure WriteParserDefinitionToStream(x:TParserDefinition;s:TStream);
function ReadParserDefinitionFromStream(s:TStream):TParserDefinition;
function ConvertLexerDefinitionToBinaryPascalDeclaration(s:TStream;Prefix:string;CRLF:string=#10#13):string;
function ConvertLexerDefinitionToBinaryPascalImplementation(s:TStream;Prefix:string;CRLF:string=#10#13):string;
function ConvertParserDefinitionToBinaryPascalDeclaration(s:TStream;Prefix:string;CRLF:string=#10#13):string;
function ConvertParserDefinitionToBinaryPascalImplementation(s:TStream;Prefix:string;CRLF:string=#10#13):string;
function CompareLexerDefinitions(x,y:TLexerDefinition):Boolean;
function CompareParserDefinitions(x,y:TParserDefinition):Boolean;

implementation

uses
  COW_Design,Math;

function TableItemToStr(i:COW_RunTime.TTableItem):string;
begin
  Result:='(ItemType:';
  case i.ItemType of
    titShift:Result:=Result+'titShift;Value:'+IntToStr(i.Value);
    titReduce:Result:=Result+'titReduce;Value:'+IntToStr(i.Value);
    titGoto:Result:=Result+'titGoto;Value:'+IntToStr(i.Value);
    titError:Result:=Result+'titError';
    titAccept:Result:=Result+'titAccept';
  end;
  Result:=Result+')';
end;

function BoolToStr(b:Boolean):string;
begin
  if b then
    Result:='True'
  else
    Result:='False';
end;

function ConvertLexerDefinitionToPascalDeclaration(x:TLexerDefinition;Prefix:string='';Indent:string='';CRLF:string=#10#13):string;
var
  a,b:Integer;
  s,t:string;
begin
  Result:=Indent+'const'+CRLF+CRLF;
  for a:=0 to x.LexerTablesCount-1 do begin
    Result:=Result+Indent+'  '+Prefix+'LexerTableItems'+IntToStr(a)+':array[0..'+IntToStr(x.LexerTables[a].Width*x.LexerTables[a].Height-1)+'] of TTableItem=('+CRLF;
    for b:=0 to x.LexerTables[a].Width*x.LexerTables[a].Height-1 do begin
      Result:=Result+Indent+'      '+TableItemToStr(x.LexerTables[a].TableItems[b]);
      if b<x.LexerTables[a].Width*x.LexerTables[a].Height-1 then
        Result:=Result+','+CRLF
      else
        Result:=Result+');'+CRLF+CRLF;
    end;
  end;
  Result:=Result+Indent+'  '+Prefix+'LexerTables:array[0..'+IntToStr(x.LexerTablesCount-1)+'] of TLexerTable=('+CRLF;
  for a:=0 to x.LexerTablesCount-1 do begin
    t:='';
    s:=Indent+'     CharMap:(';
    for b:=0 to 255 do begin
      s:=s+IntToStr(x.LexerTables[a].CharMap[Chr(b)]);
      if b<255 then begin
        s:=s+',';
        if Length(s)>100 then begin
          t:=t+s+CRLF;
          s:=Indent+'              ';
        end;
      end;
    end;
    t:=t+s+')';
    Result:=Result+Indent+'    (Width:'+IntToStr(x.LexerTables[a].Width)+';'+CRLF+
                   Indent+'     Height:'+IntToStr(x.LexerTables[a].Height)+';'+CRLF+
                   Indent+'     EOFId:'+IntToStr(x.LexerTables[a].EOFId)+';'+CRLF+
                   Indent+'     TableItems:@'+Prefix+'LexerTableItems'+IntToStr(a)+';'+CRLF+
                   t+';'+CRLF+
                   Indent+'     Stored:'+BoolToStr(x.LexerTables[a].Stored)+')';
    if a<Integer(x.LexerTablesCount)-1 then
      Result:=Result+','+CRLF
    else
      Result:=Result+');'+CRLF+CRLF;
  end;
  Result:=Result+Indent+'  '+Prefix+'LexerLaws:array[0..'+IntToStr(x.LexerLawsCount-1)+'] of TLexerLaw=('+CRLF;
  for a:=0 to x.LexerLawsCount-1 do begin
    Result:=Result+Indent+'    (ChildsCount:'+IntToStr(x.LexerLaws[a].ChildsCount)+';ID:'+IntToStr(x.LexerLaws[a].ID)+')';
    if a<Integer(x.LexerLawsCount)-1 then
      Result:=Result+','+CRLF
    else
      Result:=Result+');'+CRLF+CRLF;
  end;
  Result:=Result+Indent+'  '+Prefix+'LexerDefinition:TLexerDefinition='+CRLF+
                 Indent+'    (LexerTables:@'+Prefix+'LexerTables;'+CRLF+
                 Indent+'     LexerTablesCount:'+IntToStr(x.LexerTablesCount)+';'+CRLF+
                 Indent+'     LexerEOFId:'+IntToStr(x.LexerEOFId)+';'+CRLF+
                 Indent+'     LexerLaws:@'+Prefix+'LexerLaws;'+CRLF+
                 Indent+'     LexerLawsCount:'+IntToStr(x.LexerLawsCount)+';'+CRLF+
                 Indent+'     StaticResource:True);'+CRLF+CRLF;
end;

function ConvertParserDefinitionToPascalDeclaration(x:TParserDefinition;Prefix:string='';Indent:string='';CRLF:string=#10#13):string;
var
  a,b:Integer;
  s,t:string;
begin
  Result:=Indent+'const'+CRLF+CRLF;
  Result:=Result+Indent+'  '+Prefix+'ParserTable:array[0..'+IntToStr(x.TableWidth*x.TableHeight-1)+'] of TTableItem=('+CRLF;
  for a:=0 to x.TableWidth*x.TableHeight-1 do begin
    Result:=Result+Indent+'    '+TableItemToStr(x.ParserTable[a]);
    if a<x.TableWidth*x.TableHeight-1 then
      Result:=Result+','+CRLF
    else
      Result:=Result+');'+CRLF+CRLF;
  end;
  for a:=0 to x.ParserLawsCount-1 do begin
    Result:=Result+Indent+'  '+Prefix+'ParserLawUsage'+IntToStr(a)+':array[0..'+IntToStr(x.ParserLaws[a].ChildsCount-1)+'] of Boolean=('+CRLF;
    s:=Indent+'    ';
    t:='';
    for b:=0 to x.ParserLaws[a].ChildsCount-1 do begin
      s:=s+BoolToStr(x.ParserLaws[a].Usage[b]);
      if b<x.ParserLaws[a].ChildsCount-1 then begin
        s:=s+',';
        if Length(s)>100 then begin
          t:=t+s+CRLF;
          s:=Indent+'    ';
        end;
      end;
    end;
    Result:=Result+t+s+');'+CRLF+CRLF;
  end;
  Result:=Result+Indent+'  '+Prefix+'ParserLaws:array[0..'+IntToStr(x.ParserLawsCount-1)+'] of TParserLaw=('+CRLF;
  for a:=0 to x.ParserLawsCount-1 do begin
    Result:=Result+Indent+'    (ChildsCount:'+IntToStr(x.ParserLaws[a].ChildsCount)+';'+CRLF+
                   Indent+'     ID:'+IntToStr(x.ParserLaws[a].ID)+';'+CRLF+
                   Indent+'     UsageCount:'+IntToStr(x.ParserLaws[a].UsageCount)+';'+CRLF+
                   Indent+'     Usage:@'+Prefix+'ParserLawUsage'+IntToStr(a)+')';
    if a<x.ParserLawsCount-1 then
      Result:=Result+','+CRLF
    else
      Result:=Result+');'+CRLF+CRLF;
  end;
  Result:=Result+Indent+'  '+Prefix+'TokenNames:array[0..'+IntToStr(x.TokenNamesCount-1)+'] of string=('+CRLF;
  for a:=0 to x.TokenNamesCount-1 do begin
    Result:=Result+Indent+'    '''+x.TokenNames[a]+'''';
    if a<x.TokenNamesCount-1 then
      Result:=Result+','+CRLF
    else
      Result:=Result+');'+CRLF+CRLF;
  end;
  Result:=Result+Indent+'  '+Prefix+'ParserDefinition:TParserDefinition='+CRLF+
                 Indent+'    (ParserTable:@'+Prefix+'ParserTable;'+CRLF+
                 Indent+'     TableWidth:'+IntToStr(x.TableWidth)+';'+CRLF+
                 Indent+'     TableHeight:'+IntToStr(x.TableHeight)+';'+CRLF+
                 Indent+'     ParserLaws:@'+Prefix+'ParserLaws;'+CRLF+
                 Indent+'     ParserLawsCount:'+IntToStr(x.ParserLawsCount)+';'+CRLF+
                 Indent+'     TokenNames:@'+Prefix+'TokenNames;'+CRLF+
                 Indent+'     TokenNamesCount:'+IntToStr(x.TokenNamesCount)+';'+CRLF+
                 Indent+'     StaticResource:True);'+CRLF+CRLF;
end;

procedure WriteLexerDefinitionToStream(x:TLexerDefinition;s:TStream);
var
  a,b:Integer;
begin
  s.Write(x.LexerTablesCount,SizeOf(Cardinal));
  for a:=0 to Integer(x.LexerTablesCount)-1 do begin
    s.Write(x.LexerTables[a].Width,SizeOf(Word));
    s.Write(x.LexerTables[a].Height,SizeOf(Word));
    s.Write(x.LexerTables[a].EOFId,SizeOf(Word));
    s.Write(x.LexerTables[a].CharMap,SizeOf(COW_RunTime.TCharMap));
    s.Write(x.LexerTables[a].Stored,SizeOf(Boolean));
    for b:=0 to Integer(x.LexerTables[a].Width)*Integer(x.LexerTables[a].Height)-1 do
      s.Write(x.LexerTables[a].TableItems[b],SizeOf(COW_RunTime.TTableItem));
  end;
  s.Write(x.LexerEOFId,SizeOf(Word));
  s.Write(x.LexerLawsCount,SizeOf(Cardinal));
  for a:=0 to Integer(x.LexerLawsCount)-1 do
    s.Write(x.LexerLaws[a],SizeOf(COW_RunTime.TLexerLaw));
end;

function ReadLexerDefinitionFromStream(s:TStream):TLexerDefinition;
var
  a,b:Integer;
begin
  s.Read(Result.LexerTablesCount,SizeOf(Cardinal));
  GetMem(Result.LexerTables,Result.LexerTablesCount*SizeOf(COW_RunTime.TLexerTable));
  for a:=0 to Integer(Result.LexerTablesCount)-1 do begin
    s.Read(Result.LexerTables[a].Width,SizeOf(Word));
    s.Read(Result.LexerTables[a].Height,SizeOf(Word));
    s.Read(Result.LexerTables[a].EOFId,SizeOf(Word));
    s.Read(Result.LexerTables[a].CharMap,SizeOf(COW_RunTime.TCharMap));
    s.Read(Result.LexerTables[a].Stored,SizeOf(Boolean));
    GetMem(Result.LexerTables[a].TableItems,Integer(Result.LexerTables[a].Width)*Integer(Result.LexerTables[a].Height)*SizeOf(COW_RunTime.TTableItem));
    for b:=0 to Integer(Result.LexerTables[a].Width)*Integer(Result.LexerTables[a].Height)-1 do
      s.Read(Result.LexerTables[a].TableItems[b],SizeOf(COW_RunTime.TTableItem));
  end;
  s.Read(Result.LexerEOFId,SizeOf(Word));
  s.Read(Result.LexerLawsCount,SizeOf(Cardinal));
  GetMem(Result.LexerLaws,Integer(Result.LexerLawsCount)*SizeOf(COW_RunTime.TLexerLaw));
  for a:=0 to Integer(Result.LexerLawsCount)-1 do
    s.Read(Result.LexerLaws[a],SizeOf(COW_RunTime.TLexerLaw));
  Result.StaticResource:=False;
end;

procedure WriteStringToStream(Str:string;s:TStream);
var
  a:Integer;
begin
  a:=Length(Str);
  s.Write(a,SizeOf(a));
  s.Write(Str[1],a);
end;

function ReadStringFromStream(s:TStream):string;
var
  a:Integer;
begin
  s.Read(a,SizeOf(a));
  SetLength(Result,a);
  s.Read(Result[1],a);
end;

procedure WriteParserDefinitionToStream(x:TParserDefinition;s:TStream);
var
  a,b:Integer;
begin
  s.Write(x.TableWidth,SizeOf(Word));
  s.Write(x.TableHeight,SizeOf(Word));
  for a:=0 to Integer(x.TableWidth)*Integer(x.TableHeight)-1 do
    s.Write(x.ParserTable[a],SizeOf(COW_RunTime.TTableItem));
  s.Write(x.ParserLawsCount,SizeOf(Word));
  for a:=0 to Integer(x.ParserLawsCount)-1 do begin
    s.Write(x.ParserLaws[a].ChildsCount,SizeOf(Word));
    s.Write(x.ParserLaws[a].ID,SizeOf(Word));
    s.Write(x.ParserLaws[a].UsageCount,SizeOf(Word));
    for b:=0 to Integer(x.ParserLaws[a].ChildsCount)-1 do
      s.Write(x.ParserLaws[a].Usage[b],SizeOf(Boolean));
  end;
  s.Write(x.TokenNamesCount,SizeOf(Word));
  for a:=0 to Integer(x.TokenNamesCount)-1 do
    WriteStringToStream(x.TokenNames[a],s);
end;

function ReadParserDefinitionFromStream(s:TStream):TParserDefinition;
var
  a,b:Integer;
begin
  s.Read(Result.TableWidth,SizeOf(Word));
  s.Read(Result.TableHeight,SizeOf(Word));
  GetMem(Result.ParserTable,Integer(Result.TableWidth)*Integer(Result.TableHeight)*SizeOf(TTableItem));
  for a:=0 to Integer(Result.TableWidth)*Integer(Result.TableHeight)-1 do
    s.Read(Result.ParserTable[a],SizeOf(COW_RunTime.TTableItem));
  s.Read(Result.ParserLawsCount,SizeOf(Word));
  GetMem(Result.ParserLaws,Integer(Result.ParserLawsCount)*SizeOf(COW_RunTime.TParserLaw));
  for a:=0 to Integer(Result.ParserLawsCount)-1 do begin
    s.Read(Result.ParserLaws[a].ChildsCount,SizeOf(Word));
    s.Read(Result.ParserLaws[a].ID,SizeOf(Word));
    s.Read(Result.ParserLaws[a].UsageCount,SizeOf(Word));
    GetMem(Result.ParserLaws[a].Usage,Integer(Result.ParserLaws[a].ChildsCount)*Sizeof(Boolean));
    for b:=0 to Integer(Result.ParserLaws[a].ChildsCount)-1 do
      s.Read(Result.ParserLaws[a].Usage[b],SizeOf(Boolean));
  end;
  s.Read(Result.TokenNamesCount,SizeOf(Word));
  GetMem(Result.TokenNames,Integer(Result.TokenNamesCount)*Sizeof(string));
  FillChar(Result.TokenNames^,Integer(Result.TokenNamesCount)*Sizeof(string),0);
  for a:=0 to Integer(Result.TokenNamesCount)-1 do
    Result.TokenNames[a]:=ReadStringFromStream(s);
  Result.StaticResource:=False;
end;

function ConvertRawDataToByteArray(s:TStream;CRLF:string=#10#13):string;
var
  a,x,y,z:Integer;
  b:Byte;
  u:string[2];
const
  N=25;
begin
  s.Position:=0;
  PushProgress;
  x:=s.Size mod N;
  y:=Ceil(s.Size/N)-1;
  if x=0 then
    x:=N;
  SetLength(Result,(4+Length(CRLF)+N*4)*y+3+4*x);
  FillChar(Result[1],Length(Result),Ord(' '));
  z:=5;
  for a:=0 to s.Size-1 do begin
    if (a mod 100)=0 then
      UpdateProgress(a/s.Size,'Converting data...');
    s.Read(b,1);
    u:=IntToHex(b,2);
    Result[z]:='$';
    Result[z+1]:=u[1];
    Result[z+2]:=u[2];
    if a<>s.Size-1 then begin
      Result[z+3]:=',';
      if ((a+1) mod N)=0 then begin
        Move(CRLF[1],Result[z+4],Length(CRLF));
        Inc(z,4+4+Length(CRLF));
      end else
        Inc(z,4);
    end;
  end;
  PopProgress;
end;

function ConvertLexerDefinitionToBinaryPascalDeclaration(s:TStream;Prefix:string;CRLF:string=#10#13):string;
begin
  Result:='function '+Prefix+'LexerDefinition:TLexerDefinition;'+CRLF+CRLF+
          'const'+CRLF+
          '  '+Prefix+'LexerBinaryData:array[1..'+IntToStr(s.Size)+'] of Byte=('+CRLF+
          ConvertRawDataToByteArray(s,CRLF)+');'+CRLF+CRLF;

end;

function ConvertLexerDefinitionToBinaryPascalImplementation(s:TStream;Prefix:string;CRLF:string=#10#13):string;
begin
  Result:='function '+Prefix+'LexerDefinition:TLexerDefinition;'+CRLF+
          'var'+CRLF+
          '  m:TMemoryStream;'+CRLF+
          'begin'+CRLF+
          '  m:=TMemoryStream.Create;'+CRLF+
          '  m.Write('+Prefix+'LexerBinaryData,'+IntToStr(s.Size)+');'+CRLF+
          '  m.Position:=0;'+CRLF+
          '  Result:=ReadLexerDefinitionFromStream(m);'+CRLF+
          '  m.Destroy;'+CRLF+
          'end;'+CRLF+CRLF;
end;

function ConvertParserDefinitionToBinaryPascalDeclaration(s:TStream;Prefix:string;CRLF:string=#10#13):string;
begin
  Result:='function '+Prefix+'ParserDefinition:TParserDefinition;'+CRLF+CRLF+
          'const'+CRLF+
          '  '+Prefix+'ParserBinaryData:array[1..'+IntToStr(s.Size)+'] of Byte=('+CRLF+
          ConvertRawDataToByteArray(s,CRLF)+');'+CRLF+CRLF;
end;

function ConvertParserDefinitionToBinaryPascalImplementation(s:TStream;Prefix:string;CRLF:string=#10#13):string;
begin
  Result:='function '+Prefix+'ParserDefinition:TParserDefinition;'+CRLF+
          'var'+CRLF+
          '  m:TMemoryStream;'+CRLF+
          'begin'+CRLF+
          '  m:=TMemoryStream.Create;'+CRLF+
          '  m.Write('+Prefix+'ParserBinaryData,'+IntToStr(s.Size)+');'+CRLF+
          '  m.Position:=0;'+CRLF+
          '  Result:=ReadParserDefinitionFromStream(m);'+CRLF+
          '  m.Destroy;'+CRLF+
          'end;'+CRLF+CRLF;
end;

function CompareTableItem(x,y:COW_RunTime.TTableItem):Boolean;
begin
  Result:=(x.ItemType=y.ItemType) and
          ((x.ItemType in [titError,titAccept]) or (x.Value=y.Value));
end;

function CompareLexerDefinitions(x,y:TLexerDefinition):Boolean;
var
  a,b:Integer;
begin
  Result:=(x.LexerTablesCount=y.LexerTablesCount) and
          (x.LexerEOFId=y.LexerEOFId) and
          (x.LexerLawsCount=y.LexerLawsCount);
  for a:=0 to Integer(x.LexerTablesCount)-1 do begin
    Result:=Result and
            (x.LexerTables[a].Stored=y.LexerTables[a].Stored) and
            (x.LexerTables[a].EOFId=y.LexerTables[a].EOFID) and
            CompareMem(@(x.LexerTables[a].CharMap),@(y.LexerTables[a].CharMap),SizeOf(COW_RunTime.TCharMap)) and
            (x.LexerTables[a].Width=y.LexerTables[a].Width) and
            (x.LexerTables[a].Height=y.LexerTables[a].Height);
    for b:=0 to Integer(x.LexerTables[a].Width)*Integer(x.LexerTables[a].Height)-1 do
      Result:=Result and CompareTableItem(x.LexerTables[a].TableItems[b],y.LexerTables[a].TableItems[b]);
  end;
  for a:=0 to Integer(x.LexerLawsCount)-1 do begin
    Result:=Result and
            ((x.LexerLaws[a].ChildsCount=y.LexerLaws[a].ChildsCount) and
            (x.LexerLaws[a].ID=y.LexerLaws[a].ID));
  end;
end;

function CompareParserDefinitions(x,y:TParserDefinition):Boolean;
var
  a,b:Integer;
begin
  Result:=(x.TableWidth=y.TableWidth) and
          (x.TableHeight=y.TableHeight) and
          (x.ParserLawsCount=y.ParserLawsCount) and
          (x.TokenNamesCount=y.TokenNamesCount);
  for a:=0 to Integer(x.TableWidth)*Integer(x.TableHeight)-1 do
    Result:=Result and CompareTableItem(x.ParserTable[a],y.ParserTable[a]);
  for a:=0 to Integer(x.ParserLawsCount)-1 do begin
    Result:=Result and
            (x.ParserLaws[a].ChildsCount=y.ParserLaws[a].ChildsCount) and
            (x.ParserLaws[a].ID=y.ParserLaws[a].ID) and
            (x.ParserLaws[a].UsageCount=y.ParserLaws[a].UsageCount);
    for b:=0 to Integer(x.ParserLaws[a].ChildsCount)-1 do
      Result:=Result and
              (x.ParserLaws[a].Usage[b]=y.ParserLaws[a].Usage[b]);
  end;
  for a:=0 to Integer(x.TokenNamesCount)-1 do
    Result:=Result and
            (x.TokenNames[a]=y.TokenNames[a]);
end;

end.
