unit VirtualScrollingWinControl;

interface

uses
  SysUtils,Windows,Messages,Classes,Controls,Graphics;

type
  TScrollBarUpdateStateItem=(sbuPage,sbuPosition,sbuRange);
  TScrollBarUpdateState=set of TScrollBarUpdateStateItem;

  TVirtualScrollBar=class
  private
    FWND:HWND;
    FVertical:Boolean;
    FPageSize,FPosition,FRange,FUpdateCount:Integer;
    FUpdateState:TScrollBarUpdateState;

    function GetPageSize: Integer;
    function GetPosition: Integer;
    function GetRange: Integer;
    procedure SetPageSize(const Value: Integer);
    procedure SetPosition(const Value: Integer);
    procedure SetRange(const Value: Integer);
    procedure SetWNDHandle(const Value: HWND);
  protected
    function GetBarFlag:Cardinal;
    procedure DoUpdate;

    property WNDHandle:HWND read FWND write SetWNDHandle;
  public
    constructor Create(AVertical:Boolean);

    procedure BeginUpdate;
    procedure EndUpdate;

    procedure Repaint;

    property Vertical:Boolean read FVertical;
    property Position:Integer read GetPosition write SetPosition;
    property Range:Integer read GetRange write SetRange;
    property PageSize:Integer read GetPageSize write SetPageSize;
  end;

  TVirtualScrollingWinControl=class(TWinControl)
  private
    FHorzScrollBar: TVirtualScrollBar;
    FVertScrollBar: TVirtualScrollBar;

    procedure DoUpdateScrollBars;

    procedure WMPaint(var Message:TWMPaint);message WM_PAINT;
    procedure WMDestroy(var Message:TMessage);message WM_DESTROY;
    procedure WMVScroll(var Message:TWMVScroll);message WM_VSCROLL;
    procedure WMHScroll(var Message:TWMHScroll);message WM_HSCROLL;
    procedure WMSize(var Message:TWMSize);message WM_SIZE;
    procedure WMNCPaint(var Message:TWMNCPaint);message WM_NCPAINT;
  protected
    property HorzScrollBar:TVirtualScrollBar read FHorzScrollBar;
    property VertScrollBar:TVirtualScrollBar read FVertScrollBar;

    procedure CreateHandle;override;

    procedure ScrollPosChanged;virtual;
    procedure UpdateScrollBars;virtual;
  public
    constructor Create(AOwner:TComponent);override;

    procedure UpdateScrollState;

    destructor Destroy;override;
  end;

implementation

uses
  Themes;

{ TVirtualScrollBar }

procedure TVirtualScrollBar.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

constructor TVirtualScrollBar.Create(AVertical: Boolean);
begin
  inherited Create;
  FVertical:=AVertical;
end;

procedure TVirtualScrollBar.DoUpdate;
var
  SI:TScrollInfo;
  i:TScrollBarUpdateStateItem;
const
  T:array[sbuPage..sbuRange] of Cardinal=(SIF_PAGE,SIF_POS,SIF_RANGE);
begin
  if (FUpdateState=[]) or (FWND=0) then
    Exit;
  ZeroMemory(@SI,SizeOf(SI));
  with SI do begin
    cbSize:=SizeOf(SI);
    if FPageSize<0 then
      FPageSize:=0;
    nPage:=FPageSize;
    nPos:=FPosition;
    nMax:=FRange;
    for i:=sbuPage to sbuRange do
      if i in FUpdateState then
        fMask:=fMask or T[i];
  end;
  SetScrollInfo(FWND,GetBarFlag,SI,True);
  SI.fMask:=SIF_ALL;
  GetScrollInfo(FWND,GetBarFlag,SI);
  with SI do begin
    FPageSize:=nPage;
    FPosition:=nPos;
    FRange:=nMax;
  end;
  FUpdateState:=[];
end;

procedure TVirtualScrollBar.EndUpdate;
begin
  Assert(FUpdateCount>0,'Negative update count');
  Dec(FUpdateCount);
  if FUpdateCount=0 then
    DoUpdate;
end;

function TVirtualScrollBar.GetBarFlag: Cardinal;
begin
  if FVertical then
    Result:=SB_VERT
  else
    Result:=SB_HORZ;
end;

function TVirtualScrollBar.GetPageSize: Integer;
begin
  DoUpdate;
  Result:=FPageSize;
end;

function TVirtualScrollBar.GetPosition: Integer;
begin
  DoUpdate;
  Result:=FPosition;
end;

function TVirtualScrollBar.GetRange: Integer;
begin
  DoUpdate;
  Result:=FRange;
end;

procedure TVirtualScrollBar.Repaint;
var
  SI:TScrollInfo;
begin
  ZeroMemory(@SI,SizeOf(SI));
  SI.cbSize:=Sizeof(SI);
  SetScrollInfo(FWND,GetBarFlag,SI,True);
end;

procedure TVirtualScrollBar.SetPageSize(const Value: Integer);
begin
  BeginUpdate;
  if FPageSize<>Value then begin
    Include(FUpdateState,sbuPage);
    FPageSize:=Value;
  end;
  EndUpdate;
end;

procedure TVirtualScrollBar.SetPosition(const Value: Integer);
begin
  BeginUpdate;
  if FPosition<>Value then begin
    Include(FUpdateState,sbuPosition);
    FPosition:=Value;
  end;
  EndUpdate;
end;

procedure TVirtualScrollBar.SetRange(const Value: Integer);
begin
  BeginUpdate;
  if FRange<>Value then begin
    Include(FUpdateState,sbuRange);
    FRange:=Value;
  end;
  EndUpdate;
end;

procedure TVirtualScrollBar.SetWNDHandle(const Value: HWND);
begin
  FWND := Value;
  if Value<>0 then begin
    FUpdateState:=[sbuPage,sbuPosition,sbuRange];
    DoUpdate;
  end;
end;

{ TVirtualScrollingWinControl }

constructor TVirtualScrollingWinControl.Create(AOwner: TComponent);
begin
  FHorzScrollBar:=TVirtualScrollBar.Create(False);
  FVertScrollBar:=TVirtualScrollBar.Create(True);
  inherited;
end;

procedure TVirtualScrollingWinControl.CreateHandle;
begin
  inherited;
  FHorzScrollBar.WNDHandle:=Handle;
  FVertScrollBar.WNDHandle:=Handle;
  DoUpdateScrollBars;
end;

destructor TVirtualScrollingWinControl.Destroy;
begin
  inherited Destroy;
  FHorzScrollBar.Destroy;
  FVertScrollBar.Destroy;
end;

procedure TVirtualScrollingWinControl.DoUpdateScrollBars;
begin
  FVertScrollBar.BeginUpdate;
  FHorzScrollBar.BeginUpdate;
  try
    UpdateScrollBars;
  finally
    FVertScrollBar.EndUpdate;
    FHorzScrollBar.EndUpdate;
  end;
end;

procedure TVirtualScrollingWinControl.ScrollPosChanged;
begin
  Invalidate;
end;

procedure TVirtualScrollingWinControl.UpdateScrollBars;
begin
  ScrollPosChanged;
end;

procedure TVirtualScrollingWinControl.UpdateScrollState;
begin
  if HandleAllocated then
    DoUpdateScrollBars;
end;

procedure TVirtualScrollingWinControl.WMDestroy(var Message: TMessage);
begin
  FVertScrollBar.WNDHandle:=0;
  FHorzScrollBar.WNDHandle:=0;
  inherited;
end;

procedure TVirtualScrollingWinControl.WMHScroll(var Message: TWMHScroll);
var
  SI:TScrollInfo;
begin
  with HorzScrollBar do
    case Message.ScrollCode of
      SB_LINEDOWN:Position:=Position+1;
      SB_LINEUP:Position:=Position-1;
      SB_PAGEDOWN:Position:=Position+PageSize;
      SB_PAGEUP:Position:=Position-PageSize;
      SB_THUMBTRACK:begin
        ZeroMemory(@Si,SizeOf(SI));
        SI.cbSize:=SizeOf(SI);
        SI.fMask:=SIF_TRACKPOS;
        if GetScrollInfo(Handle,SB_HORZ,SI) then
          Position:=SI.nTrackPos
        else
          Position:=Message.Pos;
      end;
    else
      inherited;
      Exit;
    end;
  Message.Result:=0;
  ScrollPosChanged;
end;

procedure TVirtualScrollingWinControl.WMNCPaint(var Message: TWMNCPaint);  //another fix of Delphi VCL...
const
  InnerStyles: array[TBevelCut] of Integer = (0, BDR_SUNKENINNER, BDR_RAISEDINNER, 0);
  OuterStyles: array[TBevelCut] of Integer = (0, BDR_SUNKENOUTER, BDR_RAISEDOUTER, 0);
  EdgeStyles: array[TBevelKind] of Integer = (0, 0, BF_SOFT, BF_FLAT);
  Ctl3DStyles: array[Boolean] of Integer = (BF_MONO, 0);
var
  DC: HDC;
  RC, RW, RWH, RWV, SaveRW: TRect;
  EdgeSize: Integer;
  WinStyle: Longint;
begin
  { Get window DC that is clipped to the non-client area }
  inherited;
  if (BevelKind <> bkNone) or (BorderWidth > 0) then
  begin                      
    DC := GetWindowDC(Handle);
    try
      Windows.GetClientRect(Handle, RC);
      GetWindowRect(Handle, RW);
      MapWindowPoints(0, Handle, RW, 2);
      OffsetRect(RC, -RW.Left, -RW.Top);
      ExcludeClipRect(DC, RC.Left, RC.Top, RC.Right, RC.Bottom);
      { Draw borders in non-client area }
      SaveRW := RW;
      InflateRect(RC, BorderWidth, BorderWidth);
      RW := RC;
      WinStyle := GetWindowLong(Handle, GWL_STYLE);
      if BevelKind <> bkNone then
      begin
        EdgeSize := 0;
        if BevelInner <> bvNone then Inc(EdgeSize, BevelWidth);
        if BevelOuter <> bvNone then Inc(EdgeSize, BevelWidth);
        with RW do
        begin
          if beLeft in BevelEdges then Dec(Left, EdgeSize);
          if beTop in BevelEdges then Dec(Top, EdgeSize);
          if beRight in BevelEdges then Inc(Right, EdgeSize);
          if (WinStyle and WS_VSCROLL) <> 0 then Inc(Right, GetSystemMetrics(SM_CYVSCROLL));
          if beBottom in BevelEdges then Inc(Bottom, EdgeSize);
          if (WinStyle and WS_HSCROLL) <> 0 then Inc(Bottom, GetSystemMetrics(SM_CXHSCROLL));
          DrawEdge(DC, RW, InnerStyles[BevelInner] or OuterStyles[BevelOuter],
            Byte(BevelEdges) or EdgeStyles[BevelKind] or Ctl3DStyles[Ctl3D] or BF_ADJUST);
        end;
        IntersectClipRect(DC, RW.Left, RW.Top, RW.Right, RW.Bottom);
        RW := SaveRW;
      end;
      RWH:=Rect(2*BevelWidth+BorderWidth,RW.Bottom,RW.Right,RW.Bottom);
      RWV:=Rect(RW.Right,2*BevelWidth+BorderWidth,RW.Right,RW.Bottom);
      if (WinStyle and WS_VSCROLL) <> 0 then begin
        Dec(RWH.Right, GetSystemMetrics(SM_CXVSCROLL));
        Dec(RWV.Left, GetSystemMetrics(SM_CXVSCROLL));
      end;
      if (WinStyle and WS_HSCROLL) <> 0 then begin
        Dec(RWH.Top, GetSystemMetrics(SM_CYHSCROLL));
        Dec(RWV.Bottom, GetSystemMetrics(SM_CYHSCROLL));
      end;
      with RWH do
        ExcludeClipRect(DC,Left,Top,Right,Bottom);
      with RWV do
        ExcludeClipRect(DC,Left,Top,Right,Bottom);
      { Erase parts not drawn }
      OffsetRect(RW, -RW.Left, -RW.Top);
      Windows.FillRect(DC, RW, Brush.Handle);
    finally
      ReleaseDC(Handle, DC);
    end;
  end;

  if ThemeServices.ThemesEnabled and (csNeedsBorderPaint in ControlStyle) then
    ThemeServices.PaintBorder(Self, False);
end;

procedure TVirtualScrollingWinControl.WMPaint(var Message: TWMPaint);
begin
  inherited;
end;

procedure TVirtualScrollingWinControl.WMSize(var Message: TWMSize);
begin
  inherited;
  DoUpdateScrollBars;
end;

procedure TVirtualScrollingWinControl.WMVScroll(var Message: TWMVScroll);
var
  SI:TScrollInfo;
begin
  with VertScrollBar do
    case Message.ScrollCode of
      SB_LINEDOWN:Position:=Position+1;
      SB_LINEUP:Position:=Position-1;
      SB_PAGEDOWN:Position:=Position+PageSize;
      SB_PAGEUP:Position:=Position-PageSize;
      SB_THUMBTRACK:begin
        ZeroMemory(@Si,SizeOf(SI));
        SI.cbSize:=SizeOf(SI);
        SI.fMask:=SIF_TRACKPOS;
        if GetScrollInfo(Handle,SB_VERT,SI) then
          Position:=SI.nTrackPos
        else
          Position:=Message.Pos;
      end;
    else
      inherited;
//      Exit;
    end;
  Message.Result:=0;
  ScrollPosChanged;
end;

end.
