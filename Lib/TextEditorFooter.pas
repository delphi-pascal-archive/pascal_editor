unit TextEditorFooter;

interface

uses
  SysUtils,Windows,Classes,CompileErrorManager,ComCtrls;

type
  TEditMode=(emInsert,emOverwrite);

  IEditStateListener=interface(IInterfaceComponentReference)
    ['{D5CCEF7F-6E6C-4D92-831A-D69C6A9C1D58}']
    procedure EditStateChanged;
  end;

  ITextEditor=interface
    ['{773F843B-7CAF-4BE9-9764-FB50607CD227}']
    procedure RegisterEditStateListener(Listener:IEditStateListener);
    procedure UnregisterEditStateListener(Listener:IEditStateListener);

    function GetCaretPos:TPoint;
    property CaretPos:TPoint read GetCaretPos;

    function GetEditMode:TEditMode;
    property EditMode:TEditMode read GetEditMode;

    function GetModified:Boolean;
    property Modified:Boolean read GetModified;
  end;

  TTextEditorFooter=class(TCustomStatusBar,IEditStateListener)
  private
    FTextEditor: ITextEditor;
    procedure SetTextEditor(const Value: ITextEditor);
  protected
    procedure Notification(AComponent:TComponent;Operation:TOperation);override;

    procedure EditStateChanged;
    function GetComponent:TComponent;
  public
    constructor Create(AOwner:TComponent);override;

    destructor Destroy;override;
  published
    property TextEditor:ITextEditor read FTextEditor write SetTextEditor;
  end;

implementation

{ TTextEditorFooter }

constructor TTextEditorFooter.Create(AOwner: TCOmponent);
begin
  inherited;
  with Panels do begin
    with Add do begin
      Alignment:=taCenter;
      Width:=85;
    end;
    with Add do begin
      Width:=70;
    end;
    with Add do begin
      Width:=70;
    end;
    with Add do begin
      Alignment:=taCenter;
      Width:=40;
    end;
    with Add do begin
      Alignment:=taCenter;
      Width:=40;
    end;
    Add;
  end;
end;

destructor TTextEditorFooter.Destroy;
begin
  TextEditor:=nil;
  inherited;
end;

procedure TTextEditorFooter.EditStateChanged;
const
  T1:array[False..True] of string=('','Modified');
  T2:array[emInsert..emOverwrite] of string=('Insert','Overwrite');
  T3:array[False..True] of string=('NUM','');
  T4:array[False..True] of string=('CAPS','');
var
  a:Integer;
begin
  if Assigned(FTextEditor) then begin
    with FTextEditor.CaretPos do
      Panels[0].Text:=IntToStr(X+1)+' : '+IntToStr(Y+1);
    Panels[1].Text:=T1[FTextEditor.Modified];
    Panels[2].Text:=T2[FTextEditor.EditMode];
    Panels[3].Text:=T3[GetKeyState(VK_NUMLOCK) and $1=0];
    Panels[4].Text:=T4[GetKeyState(VK_CAPITAL) and $1=0];
  end else
    for a:=0 to Panels.Count-1 do
      Panels[a].Text:='';
end;

function TTextEditorFooter.GetComponent: TComponent;
begin
  Result:=Self;
end;

procedure TTextEditorFooter.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if Assigned(FTextEditor) and
     Supports(FTextEditor,IInterfaceComponentReference) and
     ((FTextEditor as IInterfaceComponentReference).GetComponent=AComponent) then begin
    FTextEditor:=nil;
    EditStateChanged;
  end;
  inherited;
end;

procedure TTextEditorFooter.SetTextEditor(const Value: ITextEditor);
begin
  if Assigned(FTextEditor) then
    FTextEditor.UnregisterEditStateListener(Self);
  FTextEditor := Value;
  if Assigned(FTextEditor) then begin
    FTextEditor.RegisterEditStateListener(Self);
    if Supports(FTextEditor,IInterfaceComponentReference) then
      (FTextEditor as IInterfaceComponentReference).GetComponent.FreeNotification(Self);
  end;
  EditStateChanged;
end;

end.
