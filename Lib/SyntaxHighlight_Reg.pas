unit SyntaxHighlight_Reg;

interface

uses
  Classes,SyntaxHighlighter,SyntaxHighlightMemo,TextEditorFooter,DesignEditors,DesignIntf,Menus,
  SysUtils,ShortCutCollection,Windows;

type
  TShortCutProperty=class(TPropertyEditor)
  public
    function GetValue:string;override;
    procedure SetValue(const Value:string);override;

    procedure GetProperties(Proc: TGetPropProc); override;

    function GetAttributes:TPropertyAttributes;override;
  end;

  TNestedShiftStateProperty = class(TNestedProperty)
  protected
    FParent:TShortCutProperty;
  public
    constructor Create(Parent: TPropertyEditor);

    function GetAttributes: TPropertyAttributes; override;
    function GetName: string; override;
    procedure GetProperties(Proc: TGetPropProc); override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  TShift=ssShift..ssDouble;

  TNestedShiftProperty = class(TNestedProperty)
  protected
    FParent:TShortCutProperty;
    FShift:TShift;
  public
    constructor Create(Parent: TPropertyEditor; AShift: TShift);

    function GetAttributes: TPropertyAttributes; override;
    function GetName: string; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  TNestedKeyProperty=class(TNestedProperty)
  protected
    FParent:TShortCutProperty;
  public
    constructor Create(Parent: TPropertyEditor);

    function GetAttributes: TPropertyAttributes; override;
    function GetName: string; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  TSyntaxHighlightMemoEditor=class(TComponentEditor)
  public
    procedure Edit;override;
    procedure ExecuteVerb(Index:Integer);override;
    function GetVerb(Index:Integer):string;override;
    function GetVerbCount:Integer;override;
  end;

const
  GShiftNames:array[ssShift..ssDouble] of string=(
    'Shift','Alt','Ctrl','LeftButton','RightButton','MiddleButton','DoubleButton'
  );

  GKeys:array[0..106] of Integer=(
    0,
    VK_BACK,
    VK_TAB,
    VK_CLEAR,
    VK_RETURN,
    VK_SHIFT,
    VK_CONTROL,
    VK_MENU,
    VK_PAUSE,
    VK_CAPITAL,
    VK_ESCAPE,
    VK_SPACE,
    VK_PRIOR,
    VK_NEXT,
    VK_END,
    VK_HOME,
    VK_LEFT,VK_UP,VK_RIGHT,VK_DOWN,
    VK_SELECT,
    VK_PRINT,
    VK_EXECUTE,
    VK_SNAPSHOT,
    VK_INSERT,
    VK_DELETE,
    VK_HELP,
    Ord('0'),Ord('1'),Ord('2'),Ord('3'),Ord('4'),Ord('5'),Ord('6'),Ord('7'),Ord('8'),Ord('9'),
    Ord('A'),Ord('B'),Ord('C'),Ord('D'),Ord('E'),Ord('F'),Ord('G'),Ord('H'),Ord('I'),Ord('J'),Ord('K'),Ord('L'),Ord('M'),
    Ord('N'),Ord('O'),Ord('P'),Ord('Q'),Ord('R'),Ord('S'),Ord('T'),Ord('U'),Ord('V'),Ord('W'),Ord('X'),Ord('Y'),Ord('Z'),
    VK_LWIN,
    VK_RWIN,
    VK_APPS,
    VK_NUMPAD0,VK_NUMPAD1,VK_NUMPAD2,VK_NUMPAD3,VK_NUMPAD4,VK_NUMPAD5,VK_NUMPAD6,VK_NUMPAD7,VK_NUMPAD8,VK_NUMPAD9,
    VK_MULTIPLY,VK_ADD,VK_SEPARATOR,VK_SUBTRACT,VK_DECIMAL,VK_DIVIDE,
    VK_F1,VK_F2,VK_F3,VK_F4,VK_F5,VK_F6,VK_F7,VK_F8,VK_F9,VK_F10,VK_F11,VK_F12,
    VK_F13,VK_F14,VK_F15,VK_F16,VK_F17,VK_F18,VK_F19,VK_F20,VK_F21,VK_F22,VK_F23,VK_F24,
    VK_NUMLOCK
  );

  GKeyNames:array[0..106] of string=(
    '<none>',
    'BACK',
    'TAB',
    'CLEAR',
    'RETURN',
    'SHIFT',
    'CONTROL',
    'MENU',
    'PAUSE',
    'CAPITAL',
    'ESCAPE',
    'SPACE',
    'PRIOR',
    'NEXT',
    'END',
    'HOME',
    'LEFT','UP','RIGHT','DOWN',
    'SELECT',
    'PRINT',
    'EXECUTE',
    'SNAPSHOT',
    'INSERT',
    'DELETE',
    'HELP',
    '0','1','2','3','4','5','6','7','8','9',
    'A','B','C','D','E','F','G','H','I','J','K','L','M',
    'N','O','P','Q','R','S','T','U','V','W','X','Y','Z',
    'LWIN',
    'RWIN',
    'APPS',
    'NUMPAD0','NUMPAD1','NUMPAD2','NUMPAD3','NUMPAD4','NUMPAD5','NUMPAD6','NUMPAD7','NUMPAD8','NUMPAD9',
    'MULTIPLY','ADD','SEPARATOR','SUBTRACT','DECIMAL','DIVIDE',
    'F1','F2','F3','F4','F5','F6','F7','F8','F9','F10','F11','F12',
    'F13','F14','F15','F16','F17','F18','F19','F20','F21','F22','F23','F24',
    'NUMLOCK'
  );


procedure Register;

implementation

uses TypInfo;

{$R SyntaxHighlight.dcr}

procedure Register;
begin
  RegisterComponents('Editors',[TSyntaxHighlighter,
                                TSyntaxHighlightMemo,
                                TTextEditorFooter]);
  RegisterPropertyEditor(TypeInfo(TShortCut),nil,'',TShortCutProperty);
  RegisterComponentEditor(TCustomSyntaxHighlightMemo,TSyntaxHighlightMemoEditor);
end;

{ TShortCutProperty }

function TShortCutProperty.GetAttributes: TPropertyAttributes;
begin
  Result:=[paSubProperties,paReadOnly];
end;

procedure TShortCutProperty.GetProperties(Proc: TGetPropProc);
begin
  Proc(TNestedShiftStateProperty.Create(Self));
  Proc(TNestedKeyProperty.Create(Self));
end;

function TShortCutProperty.GetValue: string;
begin
  Result:=ShortCutToText(GetOrdValue);
end;

procedure TShortCutProperty.SetValue(const Value: string);
begin
  ;
end;

{ TNestedShiftStateProperty }

constructor TNestedShiftStateProperty.Create(Parent: TPropertyEditor);
begin
  inherited Create(Parent);
  FParent:=Parent as TShortCutProperty;
end;

function TNestedShiftStateProperty.GetAttributes: TPropertyAttributes;
begin
  Result:=[paSubProperties,paValueList,paSortList];
end;

function TNestedShiftStateProperty.GetName: string;
begin
  Result:='ShiftState';
end;

procedure TNestedShiftStateProperty.GetProperties(Proc: TGetPropProc);
var
  u:TShift;
begin
  for u:=ssShift to ssDouble do
    Proc(TNestedShiftProperty.Create(Self,u));
end;

function TNestedShiftStateProperty.GetValue: string;
var
  k:Word;
  s:TShiftState;
  u:TShift;
begin
  Result:='';
  ShortCutToKey(FParent.GetOrdValue,k,s);
  if s=[] then
    Result:='<none>'
  else
    for u:=ssShift to ssDouble do
      if u in s then begin
        if Result<>'' then
          Result:=Result+'+';
        Result:=Result+GShiftNames[u];
      end;
end;

procedure TNestedShiftStateProperty.GetValues(Proc: TGetStrProc);
begin
  Proc('<None>');
  Proc('Shift');
  Proc('Alt');
  Proc('Ctrl');
  Proc('Alt+Ctrl');
  Proc('Alt+Shift');
  Proc('Shift+Ctrl');
  Proc('Shift+Alt+Ctrl');
end;

procedure TNestedShiftStateProperty.SetValue(const Value: string);
var
  u:TShift;
  s,t:TShiftState;
  k:Word;
begin
  s:=[];
  for u:=ssShift to ssDouble do
    if Pos(AnsiLowerCase(GShiftNames[u]),AnsiLowerCase(Value))>0 then
      Include(s,u);
  ShortCutToKey(FParent.GetOrdValue,k,t);
  FParent.SetOrdValue(ShortCut(k,s));
end;

{ TNestedShiftProperty }

constructor TNestedShiftProperty.Create(Parent: TPropertyEditor; AShift: TShift);
begin
  inherited Create(Parent);
  FParent:=(Parent as TNestedShiftStateProperty).FParent;
  FShift:=AShift;
end;

function TNestedShiftProperty.GetAttributes: TPropertyAttributes;
begin
  Result:=[paValueList,paSortList];
end;

function TNestedShiftProperty.GetName: string;
begin
  Result:=GShiftNames[FShift];
end;

function TNestedShiftProperty.GetValue: string;
var
  k:Word;
  s:TShiftState;
begin
  ShortCutToKey(FParent.GetOrdValue,k,s);
  if FShift in s then
    Result:='True'
  else
    Result:='False';
end;

procedure TNestedShiftProperty.GetValues(Proc: TGetStrProc);
begin
  Proc('True');
  Proc('False');
end;

procedure TNestedShiftProperty.SetValue(const Value: string);
var
  k:Word;
  s:TShiftState;
begin
  ShortCutToKey(FParent.GetOrdValue,k,s);
  if AnsiLowerCase(Value)='true' then
    Include(s,FShift)
  else
    Exclude(s,FShift);
  FParent.SetOrdValue(ShortCut(k,s));
end;

{ TNestedKeyProperty }

constructor TNestedKeyProperty.Create(Parent: TPropertyEditor);
begin
  inherited Create(Parent);
  FParent:=Parent as TShortCutProperty;
end;

function TNestedKeyProperty.GetAttributes: TPropertyAttributes;
begin
  Result:=[paValueList,paSortList];
end;

function TNestedKeyProperty.GetName: string;
begin
  Result:='Key';
end;

function TNestedKeyProperty.GetValue: string;
var
  k:Word;
  s:TShiftState;
  a:Integer;
begin
  ShortCutToKey(FParent.GetOrdValue,k,s);
  Result:='0x'+IntToHex(k,2);
  for a:=0 to High(GKeys) do
    if GKeys[a]=k then
      Result:=GKeyNames[a];
end;

procedure TNestedKeyProperty.GetValues(Proc: TGetStrProc);
var
  a:Integer;
begin
  for a:=0 to High(GKeyNames) do
    Proc(GKeyNames[a]);
end;

function HexToInt(s:string):Cardinal;
var
  a:Integer;
  u:Cardinal;
begin
  Result:=0;
  s:=LowerCase(s);
  u:=1;
  for a:=Length(s) downto 1 do begin
    case s[a] of
      '0'..'9':Result:=Result+u*(Cardinal(Ord(s[a]))-Cardinal(Ord('0')));
      'a'..'f':Result:=Result+u*(10+Cardinal(Ord(s[a]))-Cardinal(Ord('a')));
    else
      raise Exception.Create('"'+s+'" is not a valid hexadecimal value');
    end;
    u:=u*16;
  end;
end;

procedure TNestedKeyProperty.SetValue(const Value: string);
var
  k:Word;
  s:TShiftState;
  a:Integer;
  t:Boolean;
begin
  t:=False;
  ShortCutToKey(FParent.GetOrdValue,k,s);
  for a:=0 to High(GKeys) do
    if LowerCase(GKeyNames[a])=LowerCase(Value) then begin
      k:=GKeys[a];
      t:=True;
    end;
  if not t then begin
    if LowerCase(Copy(Value,1,2))<>'0x' then
      raise EConvertError.Create('Unrecognized key: "'+Value+'"');
    k:=HexToInt(Copy(Value,3,Length(Value)));
  end;
  FParent.SetOrdValue(ShortCut(k,s));
end;

{ TSyntaxHighlightMemoEditor }

procedure TSyntaxHighlightMemoEditor.Edit;
begin
  TCustomSyntaxHighlightMemo(Component).Customize;
end;

procedure TSyntaxHighlightMemoEditor.ExecuteVerb(Index: Integer);
begin
  TCustomSyntaxHighlightMemo(Component).Customize;
end;

function TSyntaxHighlightMemoEditor.GetVerb(Index: Integer): string;
begin
  Result:='Customize';
end;

function TSyntaxHighlightMemoEditor.GetVerbCount: Integer;
begin
  Result:=1;
end;

end.
