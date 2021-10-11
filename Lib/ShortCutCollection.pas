unit ShortCutCollection;

interface

uses
  SysUtils,Windows,Classes,Menus;

type
  TKeyShortCut=class(TCollectionItem)
  private
    FShiftState: TShiftState;
    FKey: Word;
    procedure SetKey(const Value: Word);
    procedure SetShiftState(const Value: TShiftState);
  public
    procedure Execute(var Key:Word;Shift:TShiftState);virtual;abstract;
    property ShiftState:TShiftState read FShiftState write SetShiftState;
    property Key:Word read FKey write SetKey;
  end;

  TKeyShortCutEvent=procedure(Sender:TObject;var Key:Word;Shift:TShiftState) of object;

  TPublicKeyShortCut=class(TKeyShortCut)
  private
    FOnShortCut: TKeyShortCutEvent;
    FName: string;
    procedure SetShortCut(const Value: TShortCut);
    function GetShortCut: TShortCut;
    procedure SetOnShortCut(const Value: TKeyShortCutEvent);
    procedure SetName(const Value: string);
  public
    function GetNamePath:string;override;
    procedure Execute(var Key:Word;Shift:TShiftState);override;
  published
    property ShortCut:TShortCut read GetShortCut write SetShortCut;

    property OnShortCut:TKeyShortCutEvent read FOnShortCut write SetOnShortCut;

    property Name:string read FName write SetName;
  end;

  TMouseShortCut=class(TCollectionItem)
  private
    FShiftState: TShiftState;
    procedure SetShiftState(const Value: TShiftState);
  public
    procedure Execute(X,Y:Integer;Shift:TShiftState);virtual;abstract;

    property ShiftState:TShiftState read FShiftState write SetShiftState;
  end;

  TMouseShortCutEvent=procedure(Sender:TObject;X,Y:Integer;Shift:TShiftState) of object;

  TPublicMouseShortCut=class(TMouseShortCut)
  private
    FOnShortCut: TMouseShortCutEvent;
    FName: string;
    procedure SetOnShortCut(const Value: TMouseShortCutEvent);
    procedure SetName(const Value: string);
  public
    function GetNamePath:string;override;
    procedure Execute(X,Y:Integer;Shift:TShiftState);override;
  published
    property ShiftState;

    property OnShortCut:TMouseShortCutEvent read FOnShortCut write SetOnShortCut;

    property Name:string read FName write SetName;
  end;

  TKeyShortCutCollection=class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TKeyShortCut;
    procedure SetItem(Index: Integer; const Value: TKeyShortCut);
  public
    function Add:TKeyShortCut;

    property Item[Index:Integer]:TKeyShortCut read GetItem write SetItem;default;
  end;

  TMouseShortCutCollection=class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TMouseShortCut;
    procedure SetItem(Index: Integer; const Value: TMouseShortCut);
  public
    function Add:TMouseShortCut;

    property Item[Index:Integer]:TMouseShortCut read GetItem write SetItem;
  end;

procedure ShortCutToKey(ShortCut: TShortCut; var Key: Word; var Shift: TShiftState);  //a little "fix"
function ShortCut(Key: Word; Shift: TShiftState): TShortCut;

implementation

const
  scLeft = $100;
  scRight = $200;
  scMiddle = $400;
  scDouble = $800;

function ShortCut(Key: Word; Shift: TShiftState): TShortCut;
begin
  Result := 0;
  if WordRec(Key).Hi <> 0 then Exit;
  Result := Key;
  if ssShift in Shift then Inc(Result, scShift);
  if ssCtrl in Shift then Inc(Result, scCtrl);
  if ssAlt in Shift then Inc(Result, scAlt);
  if ssLeft in Shift then Inc(Result, scLeft);
  if ssRight in Shift then Inc(Result, scRight);
  if ssMiddle in Shift then Inc(Result, scMiddle);
  if ssDouble in Shift then Inc(Result, scDouble);
end;

procedure ShortCutToKey(ShortCut: TShortCut; var Key: Word; var Shift: TShiftState);
begin
  Key := Byte(ShortCut);
  Shift := [];
  if ShortCut and scShift <> 0 then Include(Shift, ssShift);
  if ShortCut and scCtrl <> 0 then Include(Shift, ssCtrl);
  if ShortCut and scAlt <> 0 then Include(Shift, ssAlt);
  if ShortCut and scLeft <> 0 then Include(Shift, ssLeft);
  if ShortCut and scMiddle <> 0 then Include(Shift, ssRight);
  if ShortCut and scRight <> 0 then Include(Shift, ssMiddle);
  if ShortCut and scDouble <> 0 then Include(Shift, ssDouble);
end;

{ TKeyShortCut }

procedure TKeyShortCut.SetKey(const Value: Word);
begin
  FKey := Value;
end;

procedure TKeyShortCut.SetShiftState(
  const Value: TShiftState);
begin
  FShiftState := Value;
end;

{ TPublicKeyShortCut }

procedure TPublicKeyShortCut.Execute(var Key: Word; Shift: TShiftState);
begin
  if Assigned(FOnShortCut) then
    FOnShortCut(Self,Key,Shift);
end;

function TPublicKeyShortCut.GetNamePath: string;
begin
  if FName<>'' then
    Result:=Collection.GetNamePath+'_'+FName
  else
    Result:=inherited GetNamePath;
end;

function TPublicKeyShortCut.GetShortCut: TShortCut;
begin
  Result:=ShortCutCollection.ShortCut(FKey,FShiftState);
end;

procedure TPublicKeyShortCut.SetName(const Value: string);
begin
  FName := Value;
end;

procedure TPublicKeyShortCut.SetOnShortCut(
  const Value: TKeyShortCutEvent);
begin
  FOnShortCut := Value;
end;

procedure TPublicKeyShortCut.SetShortCut(const Value: TShortCut);
begin
  ShortCutToKey(Value,FKey,FShiftState);
end;

{ TMouseShortCut }

procedure TMouseShortCut.SetShiftState(const Value: TShiftState);
begin
  FShiftState := Value;
end;

{ TPublicMouseShortCut }

procedure TPublicMouseShortCut.Execute(X, Y: Integer; Shift: TShiftState);
begin
  if Assigned(FOnShortCut) then
    FOnShortCut(Self,X,Y,Shift);
end;

function TPublicMouseShortCut.GetNamePath: string;
begin
  if FName<>'' then
    Result:=Collection.GetNamePath+'_'+FName
  else
    Result:=inherited GetNamePath;
end;

procedure TPublicMouseShortCut.SetName(const Value: string);
begin
  FName := Value;
end;

procedure TPublicMouseShortCut.SetOnShortCut(
  const Value: TMouseShortCutEvent);
begin
  FOnShortCut := Value;
end;

{ TKeyShortCutCollection }

function TKeyShortCutCollection.Add: TKeyShortCut;
begin
  Result:=(inherited Add) as TKeyShortCut;
end;

function TKeyShortCutCollection.GetItem(Index: Integer): TKeyShortCut;
begin
  Result:=(inherited GetItem(Index)) as TKeyShortCut;
end;

procedure TKeyShortCutCollection.SetItem(Index: Integer;
  const Value: TKeyShortCut);
begin
  inherited SetItem(Index,Value);
end;

{ TMouseShortCutCollection }

function TMouseShortCutCollection.Add: TMouseShortCut;
begin
  Result:=(inherited Add) as TMouseShortCut;
end;

function TMouseShortCutCollection.GetItem(Index: Integer): TMouseShortCut;
begin
  Result:=(inherited GetItem(Index)) as TMouseShortCut;
end;

procedure TMouseShortCutCollection.SetItem(Index: Integer;
  const Value: TMouseShortCut);
begin
  inherited SetItem(Index,Value);
end;

end.


