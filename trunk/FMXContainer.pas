unit FMXContainer;

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TFireMonkeyContainer.
 *
 * The Initial Developer of the Original Code is David Millington.
 *
 * Portions created by the Initial Developer are Copyright (C) 2013
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s): David Millington
 *                 Edgar Reis
 *                 Ilya S
 *                 Paul Thornton
 *
 * Originally based on code found here:
 *  - http://delphisorcery.blogspot.com/2011/09/delphi-xe2-heating-up-hype-playing.html
 *  - http://stackoverflow.com/questions/7315050/delphi-xe2-possible-to-instantiate-a-firemonkey-form-in-vcl-application?rq=1
 * but with substantial modifications.
 *
 * ***** END LICENSE BLOCK ***** *)

interface

uses
  Vcl.Controls, Vcl.Forms, FMX.Forms, Winapi.Messages, System.Classes, Winapi.Windows,
  System.Generics.Collections;

const
  WM_FMX_FORM_ACTIVATED = WM_USER + 1;

type
  TOnCreateFMXFormEvent = procedure(var Form : FMX.Forms.TCommonCustomForm) of object;
  TCloseHostedFMXFormAction = (fcaNone, fcaFree);
  TOnDestroyFMXFormEvent = procedure(var Form : FMX.Forms.TCommonCustomForm; var Action : TCloseHostedFMXFormAction) of object;

  [ComponentPlatformsAttribute(pidWin32 or pidWin64)] // Thanks Edgar Reis
  TFireMonkeyContainer = class(TWinControl)
  private
    FFMXForm : FMX.Forms.TCommonCustomForm;
    FOldVCLWndProc : System.Classes.TWndMethod;
    FSubclassedForm : Vcl.Forms.TCustomForm;
    FOldFMXWndProc : Winapi.Windows.TFNWndProc;
    FNewFMXWndProc : Pointer;
    FOnCreateForm : TOnCreateFMXFormEvent;
    FOnDestroyForm : TOnDestroyFMXFormEvent;
    FCreateFormCalled : Boolean;
    FHandlingFMXActivation : Boolean;

    procedure DoOnCreate;
    procedure DoOnDestroy;

    procedure SetFMXForm(Form : FMX.Forms.TCommonCustomForm);
    procedure HandleResize;
    procedure HostTheFMXForm;
    procedure HideFMAppClassWindow;
    function GetHostedFMXFormWindowHandle : HWND;
    function GetFMXFormWindowHandle(const Form: FMX.Forms.TCommonCustomForm) : HWND;

    procedure SubClassVCLForm;
    procedure UnSubClassVCLForm;

    procedure SubClassFMXForm;
    procedure UnSubClassFMXForm;
    procedure FMXFormWndProc(var Msg: TMessage);
    procedure HandleFMXFormActivate(var Msg: TMessage);

    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMFmxFormActivated(var Message: TMessage); message WM_FMX_FORM_ACTIVATED;
  protected
    procedure Resize; override;
    procedure CreateHandle; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetParent(AParent: TWinControl); override;
  public
    constructor Create(Owner : TComponent); override;
    destructor Destroy; override;
    procedure BeforeDestruction; override;
  published
    property FireMonkeyForm : FMX.Forms.TCommonCustomForm read FFMXForm write SetFMXForm;
    property FireMonkeyFormHandle : HWND read GetHostedFMXFormWindowHandle;
    property OnCreateFMXForm : TOnCreateFMXFormEvent read FOnCreateForm write FOnCreateForm;
    property OnDestroyFMXForm : TOnDestroyFMXFormEvent read FOnDestroyForm write FOnDestroyForm;
    property Align;
    property Anchors;
    property Constraints;
    property AlignWithMargins;
    property Left;
    property Top;
    property Width;
    property Height;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
  end;

  TVCLFormHook = class
  strict private
    FOriginalWndProc : System.Classes.TWndMethod;
    FForm : Vcl.Forms.TCustomForm;
    FContainersOnThisForm : TList<TFireMonkeyContainer>;

    class function IncrementFormUsed(const Form : Vcl.Forms.TCustomForm) : Boolean;
    class function DecrementFormUsed(const Form : Vcl.Forms.TCustomForm) : Boolean;

    procedure AddContainerUsed(const Container : TFireMonkeyContainer);
    procedure RemoveContainerUsed(const Container : TFireMonkeyContainer);
    procedure VCLFormWndProc(var Msg: TMessage);
    procedure HandleVCLFormNcActivate(var Msg: TMessage);
    function IsWindowInVCLFormTree(const Wnd : HWND) : Boolean;
    function IsHostedFMXForm(const Wnd : HWND) : Boolean;
  private
    class var FFormHooks : TDictionary<Vcl.Forms.TCustomForm, TVCLFormHook>;
    class var FFormContainerCount : TDictionary<Vcl.Forms.TCustomForm, Integer>;
  public
    constructor Create(const Form : Vcl.Forms.TCustomForm);
    destructor Destroy; override;

    class procedure HookVCLForm(const Form : Vcl.Forms.TCustomForm;
      const Container : TFireMonkeyContainer);
    class procedure UnHookVCLForm(const Form : Vcl.Forms.TCustomForm;
      const Container : TFireMonkeyContainer);
  end;

implementation

uses
  FMX.Platform, FMX.Platform.Win, System.Types, SysUtils, Graphics, Vcl.Dialogs;

const
  PW_CLIENTONLY = $1;

var
  PFPrintWindow : function(Hnd: HWND; HdcBlt: HDC; nFlags: UINT): BOOL; stdcall; // Not declared in Windows.pas

function EnumWindowCallback(hWnd: HWND; lParam: LPARAM): BOOL; stdcall;
const
  FMXClassName = 'TFMAppClass';
var
  ProcessID : DWORD;
  ClassName : string;
  ClassNameLength : NativeInt;
begin
  // XE4 (possibly others) show a phantom TFMAppClass window on the taskbar.  Hide it.
  // Ensure the one we hide belongs to this thread / process - don't damage other FMX apps
  if (GetWindowThreadProcessId(hWnd, ProcessID) = GetCurrentThreadId) and (ProcessID = GetCurrentProcessId) then begin
    // Thanks to the ubiquitous David Heffernan... http://stackoverflow.com/questions/7096542/collect-all-active-window-class-names
    SetLength(ClassName, 256);
    ClassNameLength := GetClassName(hWnd, PChar(ClassName), Length(ClassName));
    if ClassNameLength = 0 then RaiseLastOSError;
    SetLength(ClassName, ClassNameLength);
    if ClassName = FMXClassName then begin
      // Found.  Hide it, and return false to stop enumerating
      ShowWindow(hWnd, SW_HIDE);
      Exit(False);
    end;
  end;
  Result := True; // Fallthrough, keep iterating
end;

{ TFiremonkeyContainer }

constructor TFireMonkeyContainer.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  FFMXForm := nil;
  FOldVCLWndProc := nil;
  FSubclassedForm := nil;
  FOldFMXWndProc := nil;
  FNewFMXWndProc := nil;
  FCreateFormCalled := false;
  FHandlingFMXActivation := false;
end;

destructor TFireMonkeyContainer.Destroy;
begin
  UnSubClassVCLForm;
  if Assigned(FFMXForm) then UnSubClassFMXForm;
  inherited;
end;

procedure TFireMonkeyContainer.BeforeDestruction;
begin
  DoOnDestroy;
  inherited;
end;

procedure TFireMonkeyContainer.DoOnCreate;
var
  OldForm, Form : FMX.Forms.TCommonCustomForm;
  Action : TCloseHostedFMXFormAction;
begin
  if (not FCreateFormCalled) and Assigned(FOnCreateForm) and not (csDesigning in ComponentState) then begin
    FCreateFormCalled := true;
    Form := FFMXForm;
    FOnCreateForm(Form);
    if (Form <> FFMXForm) and Assigned(FOnDestroyForm) then begin
      // Changed: want a new form, not the one it was set to. Call OnDestroy for the existing one,
      // otherwise free
      Action := fcaNone;
      FOnDestroyForm(FFMXForm, Action);
      case Action of
        fcaNone: ;
        fcaFree: begin
          OldForm := FFMXForm;
          SetFMXForm(nil);
          OldForm.Free;
        end;
      end;
    end;
    SetFMXForm(Form);
  end;
end;

procedure TFireMonkeyContainer.DoOnDestroy;
var
  Action : TCloseHostedFMXFormAction;
  OldForm : FMX.Forms.TCommonCustomForm;
begin
  if Assigned(FOnDestroyForm) and not (csDesigning in ComponentState) then begin
    Action := fcaNone;
    FOnDestroyForm(FFMXForm, Action);
    case Action of
      fcaNone: ;
      fcaFree: begin
        OldForm := FFMXForm;
        SetFMXForm(nil);
        OldForm.Free;
      end;
    end;
  end;
end;

procedure TFireMonkeyContainer.CreateHandle;
begin
  UnSubClassVCLForm;
  inherited;
  SubClassVCLForm;

  // Call OnCreateForm now.  Loaded() is too early - a linked autocreated FMX form won't be created yet.
  // When this component's handle is first created, it and the parent form and the FMX form are
  // guaranteed to exist
  // I'd like an earlier time since this is quite late, but I can't find a reliable one...
  DoOnCreate; // Checks it is only called once (handle can be recreated)

  // When this form's handle changes, update the hosted FMX form (setting parent, position, etc)
  if Assigned(FFMXForm) then begin
    HostTheFMXForm;
  end;
end;

procedure TFireMonkeyContainer.SetParent(AParent: TWinControl);
begin
  // If the parent changes, it might be changing forms.  Unhook, process the parent change, and
  // re-hook

  UnSubClassVCLForm;
  inherited;
  if Assigned(Parent) then
    SubClassVCLForm;
end;

procedure TFireMonkeyContainer.SubClassVCLForm;
begin
  if csDesigning in ComponentState then Exit;
  if not Assigned(FFMXForm) then Exit; // No point if not doing anything yet

  if Assigned(GetParentForm(Self)) then begin
    FSubclassedForm := GetParentForm(Self);
    TVCLFormHook.HookVCLForm(FSubclassedForm, Self);
  end;
end;

procedure TFireMonkeyContainer.UnSubClassVCLForm;
begin
  if csDesigning in ComponentState then Exit;

  // May not have subclassed yet, eg if no FMX form assigned
  if Assigned(FSubclassedForm) then begin
    TVCLFormHook.UnHookVCLForm(FSubclassedForm, Self);
    FSubclassedForm := nil;
  end;
end;

procedure TFireMonkeyContainer.SubClassFMXForm;
var
  FMXHandle : HWND;
begin
  if csDesigning in ComponentState then Exit;

  FMXHandle := GetHostedFMXFormWindowHandle;
  if (FMXHandle <> 0) and not Assigned(FOldFMXWndProc) then begin // Not already subclassed
    // Subclass FMX windows the old-fashioned way - no WindowProc property to assign
    FOldFMXWndProc := TFNWndProc(Winapi.Windows.GetWindowLong(FMXHandle, GWL_WNDPROC));
    FNewFMXWndProc := MakeObjectInstance(FMXFormWndProc);
    Winapi.Windows.SetWindowLong(FMXHandle, GWL_WNDPROC, NativeInt(FNewFMXWndProc));
  end;
end;

procedure TFireMonkeyContainer.UnSubClassFMXForm;
var
  FMXHandle : HWND;
begin
  if csDesigning in ComponentState then Exit;

  FMXHandle := GetHostedFMXFormWindowHandle;
  assert(FMXHandle <> 0);
  if Assigned(FOldFMXWndProc) then begin
    Winapi.Windows.SetWindowLong(FMXHandle, GWL_WNDPROC, NativeInt(FOldFMXWndProc));
    FreeObjectInstance(FNewFMXWndProc);
    FNewFMXWndProc := nil;
    FOldFMXWndProc := nil;
  end;
end;

procedure TFireMonkeyContainer.FMXFormWndProc(var Msg: TMessage);
begin
  if (Msg.Msg = WM_ACTIVATE) or (Msg.Msg = WM_MOUSEACTIVATE) then begin
    HandleFMXFormActivate(Msg);
  end;
  Msg.Result := CallWindowProc(FOldFMXWndProc, GetHostedFMXFormWindowHandle, Msg.Msg, Msg.WParam, Msg.LParam);
end;

procedure TFireMonkeyContainer.HandleFMXFormActivate(var Msg: TMessage);
begin
  assert((Msg.Msg = WM_ACTIVATE) or (Msg.Msg = WM_MOUSEACTIVATE));
  // So many brackets! But: "if this isn't recursively being sent, and it's an activation message"
  if (not FHandlingFMXActivation) and HandleAllocated then begin
    if ((Msg.Msg = WM_ACTIVATE) and (Msg.WParam <> WA_INACTIVE)) or
        ((Msg.Msg = WM_MOUSEACTIVATE) and ((Msg.WParam = MA_ACTIVATE) or ((Msg.WParam = MA_ACTIVATEANDEAT)))) then
    begin
      Winapi.Windows.PostMessage(Handle, WM_FMX_FORM_ACTIVATED, WPARAM(GetHostedFMXFormWindowHandle), 0);
    end;
  end;
end;

procedure TFireMonkeyContainer.WMFmxFormActivated(var Message: TMessage);
var
  VCLForm : Vcl.Forms.TCustomForm;
  FMXForm : FMX.Forms.TCommonCustomForm;
  Loop : Integer;
begin
  // When the FMX form is clicked on, it activates (and gets focus etc) but the host VCL form doesn't
  // so the previous window stays on top and draws as active. Solve this by setting the active window
  // first to the host VCL form, then the hosted FMX form, so end up with an active FMX form in a
  // on-top, drawing-as-active VCL form.
  // The title bar still messes up occasionally: fix it by telling other forms they are not active.
  FHandlingFMXActivation := true;
  try
    SetActiveWindow(GetParentForm(Self).Handle);
    SetActiveWindow(GetHostedFMXFormWindowHandle);

    for Loop := 0 to Vcl.Forms.Screen.CustomFormCount-1 do begin
      VCLForm := Vcl.Forms.Screen.CustomForms[Loop];
      if VCLForm <> GetParentForm(Self) then
        Winapi.Windows.PostMessage(VCLForm.Handle, WM_NCACTIVATE, WPARAM(False), 0);
    end;
    for Loop := 0 to FMX.Forms.Screen.FormCount-1 do begin
      FMXForm := FMX.Forms.Screen.Forms[Loop];
      if FMXForm <> FFMXForm then
        Winapi.Windows.PostMessage(GetFMXFormWindowHandle(FMXForm), WM_NCACTIVATE, WPARAM(False), 0);
    end;

    Winapi.Windows.SetFocus(GetHostedFMXFormWindowHandle);
  finally
    FHandlingFMXActivation := false;
  end;
end;

procedure TFireMonkeyContainer.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  // Prevent flicker when resizing
  if Assigned(FFMXForm) then
    Message.Result := 1
  else
    inherited;
end;

procedure TFireMonkeyContainer.Resize;
begin
  inherited;
  HandleResize;
end;

procedure TFireMonkeyContainer.HandleResize;
var
  WindowService : IFMXWindowService;
begin
  if csDesigning in ComponentState then Exit; // Do not actually change the form when designing

  if Assigned(FFMXForm) and HandleAllocated then begin
    WindowService := TPlatformServices.Current.GetPlatformService(IFMXWindowService) as IFMXWindowService;
    WindowService.SetWindowRect(FFMXForm, RectF(0, 0, Width, Height));
    FFMXForm.Invalidate;
  end;
end;

procedure TFireMonkeyContainer.SetFMXForm(Form: FMX.Forms.TCommonCustomForm);
begin
  UnSubClassVCLForm;
  if Assigned(FFMXForm) then begin
    UnSubClassFMXForm;
    FFMXForm.RemoveFreeNotification(Self);
  end;

  FFMXForm := Form;

  if Assigned(FFMXForm) then begin
    FFMXForm.FreeNotification(Self);
    HideFMAppClassWindow;
    if HandleAllocated then begin // Will otherwise occur in CreateHandle
      HostTheFMXForm;
      SubClassVCLForm;
    end;
  end;
end;

procedure TFireMonkeyContainer.Notification(AComponent: TComponent; Operation: TOperation);
const
  strLostReference = 'The form %s has been closed, and so the %s.FireMonkeyForm property has been set to nil.'
    + #10#13 + #10#13
    + 'Either keep the FireMonkey form open in the IDE while the VCL form hosting it (%s) is open,'
    + ' or use the TFireMonkeyContainer events OnCreateFMXForm and OnDestroyFMXForm to define the hosted'
    + ' FireMonkey form.';
begin
  if (Operation = opRemove) and (AComponent = FFMXForm) then begin
    if csDesigning in ComponentState then begin
      Vcl.Dialogs.MessageDlg(Format(strLostReference, [FFMXForm.Name, GetParentForm(Self).Name + '.' + Name, GetParentForm(Self).Name]),
        mtWarning, [mbOk], 0, mbOk);
    end;
    SetFMXForm(nil);
    Invalidate; // Repaint to show missing form, not 'unable to draw form'
  end;
  inherited;
end;

procedure TFireMonkeyContainer.HostTheFMXForm;
var
  ParentHandle : HWND;
  CurrentParent : TWinControl;
  FormName : string;
begin
  // Don't change the FMX form etc when in design mode - changes the actual, designing form in the IDE tab
  if not (csDesigning in ComponentState) then begin
    ParentHandle := Winapi.Windows.GetAncestor(GetHostedFMXFormWindowHandle, GA_PARENT);
    CurrentParent := Vcl.Controls.FindControl(ParentHandle);
    if (CurrentParent = nil) then begin
      FFMXForm.BorderIcons := [];
      {$WARN SYMBOL_DEPRECATED OFF} // None is deprecated in favour of bsNone; keep this for compatibility
      FFMXForm.BorderStyle := TFmxFormBorderStyle.bsNone;
      HandleResize;
      FFMXForm.Visible := True;

      // To set the parent, remove the WS_CHILD and WS_POPUP states - otherwise, the owner remains
      // the FMX app class window. (That means GetParent returns the FMX app window not the VCL host
      // window, which breaks some things including drag-drop.) Then set the parent, set ws_child
      // again.
      Winapi.Windows.SetWindowLong(GetHostedFMXFormWindowHandle, GWL_STYLE,
        Winapi.Windows.GetWindowLong(GetHostedFMXFormWindowHandle, GWL_STYLE) and not (WS_POPUP OR WS_CHILD));
      Winapi.Windows.SetParent(GetHostedFMXFormWindowHandle, Handle);
      Winapi.Windows.SetWindowLong(GetHostedFMXFormWindowHandle, GWL_STYLE,
        Winapi.Windows.GetWindowLong(GetHostedFMXFormWindowHandle, GWL_STYLE) or WS_CHILD);
      Winapi.Windows.SetParent(GetHostedFMXFormWindowHandle, Handle);

      SubclassFMXForm;
      HandleResize; // Now it's reparented ensure it's in the right position
    end else if CurrentParent <> Self then begin
      // The FMX form is already hosted by a VCL control. This can happen when a form is set at
      // designtime, and then two instances of the host VCL form are created and both try to host
      // the one FMX form.
      FormName := FFMXForm.Name;
      SetFMXForm(nil);
      raise Exception.Create('The FireMonkey form ''' + FormName + ''' is already hosted by another'
        + ' container,  ''' + CurrentParent.Name + '''.');
    end;
  end;
end;

procedure TFireMonkeyContainer.HideFMAppClassWindow;
begin
  // XE4 (possibly others) show a phantom TFMAppClass window on the taskbar. Hide it.
  EnumWindows(@EnumWindowCallback, 0);
end;

function TFireMonkeyContainer.GetHostedFMXFormWindowHandle: HWND;
begin
  assert(Assigned(FFMXForm));
  Result := GetFMXFormWindowHandle(FFMXForm);
end;

function TFireMonkeyContainer.GetFMXFormWindowHandle(const Form: FMX.Forms.TCommonCustomForm): HWND;
var
{$IF CompilerVersion >= 25.0} // XE4+
  WinHandle : TWinWindowHandle;
{$ELSE} // XE3 and XE2
  WinHandle : HWND;
{$IFEND}
begin
  assert(Assigned(Form));
  Result := 0;
  {$IF CompilerVersion >= 25.0} // XE4+
  if Assigned(Form) and Assigned(Form.Handle) then begin
    WinHandle := WindowHandleToPlatform(Form.Handle);
    if Assigned(WinHandle) then Exit(WinHandle.Wnd);
  end;
{$ELSE} // XE3 and XE2
  if Assigned(Form) and (Form.Handle <> 0) then begin
    WinHandle := FmxHandleToHWND(Form.Handle);
    if (WinHandle <> 0) then Exit(WinHandle);
  end;
{$IFEND}
end;

procedure TFireMonkeyContainer.WMPaint(var Message: TWMPaint);
const
  strDefaultText = 'TFireMonkeyContainer' + #10#13#10#13 + 'Set the FireMonkeyForm property to ' +
    ' an autocreated FireMonkey form at designtime, or in code at runtime using the  OnCreateFMXForm' +
    ' and OnDestroyFMXForm events (recommended.) You can host both 2D (HD) and 3D FireMonkey forms.';
var
  Canvas : TControlCanvas;
  Rect : TRect;
  strText : string;
begin
  inherited;

  if csDesigning in ComponentState then begin
    Canvas := TControlCanvas.Create;
    try
      Canvas.Control := Self;
      // Fill background
      Rect := ClientRect;
      Canvas.Brush.Style := bsDiagCross;
      Canvas.Brush.Color := clSkyBlue;
      SetBkColor(Canvas.Handle, ColorToRGB(Parent.Brush.Color));
      Canvas.FillRect(Rect);
      Canvas.Brush.Style := bsClear;
      // If hosting a form, paint an image of it
      if Assigned(FFMXForm) then begin
        if not Assigned(PFPrintWindow) or (not PFPrintWindow(GetHostedFMXFormWindowHandle, Canvas.Handle, PW_CLIENTONLY)) then begin
          // Paint a message that was unable to show a preview image
          Rect.Inflate(-16, -16);
          strText := FFMXForm.Name + ' : Unable to draw preview image';
          Winapi.Windows.DrawTextEx(Canvas.Handle, PChar(strText), Length(strText), Rect,
            DT_CENTER or DT_WORDBREAK or DT_END_ELLIPSIS, nil);
        end;
      end else begin
        // Otherwise, paint a message that you can host a form
        Rect.Inflate(-16, -16);
        if Name <> '' then strText := Name + ' : ' + strDefaultText
          else strText := strDefaultText;
        Winapi.Windows.DrawTextEx(Canvas.Handle, PChar(strText), Length(strText), Rect,
          DT_CENTER or DT_WORDBREAK or DT_END_ELLIPSIS, nil);
      end;
    finally
      Canvas.Free;
    end;
  end;
end;

{ TVCLFormHook }

{
  FMX forms are embedded by parenting them to a TWinControl, essentially.  However, there are focus
  issues where the form on which the parent TWinControl lives draws its title bar as unfocused when
  the FMX control has focus / is active, plus others situations, eg switching to the app via the
  Windows start bar etc. To solve this, subclass the VCL form and change the behaviour of the focus
  messages in some situations.
    This is easy for one FMX container <-> one form - the new WindowProc can be a method of the
  container. But for several FMX containers, each trying to hook the form, it gets messy: it can be
  hooked several times and if the first container is removed before the others, it resets (unhooks)
  the window proc back to the original despite there being other containers. The solution is a
  single TVCLFormHook instance per form: a count of containers per form is kept and a TVCLFormHook
  is created when the first container is parented, and it is removed when the last container is
  unparented/freed/etc. It keeps a list of containers so it can regard focus as belonging to the VCL
  form or not appropriately depending on if the window handle is a hosted FMX form or not.
    TFireMonkeyContainer calls TVCLFormHook.HookVCLForm / TVCLFormHook.UnHookVCLForm based on
  whether it itself needs a hook installed or not at the time (eg, it won't hook if it is not
  hosting a FMX form and is just sitting there empty.) HookVCLForm or UnHookVCLForm only actually
  hook or unhook if the container is the first/last, as above.
}

class procedure TVCLFormHook.HookVCLForm(const Form: Vcl.Forms.TCustomForm;
  const Container: TFireMonkeyContainer);
var
  Hook : TVCLFormHook;
begin
  assert(FFormContainerCount.ContainsKey(Form) = FFormHooks.ContainsKey(Form)); // Otherwise mismatched

  // If the form doesn't already have a hook, install one
  if IncrementFormUsed(Form) then begin // This is the first container on the form
    Hook := TVCLFormHook.Create(Form);
    FFormHooks.Add(Form, Hook);
  end;

  // Whether the above installed a new hook or not, one now exists.  Tell it about this
  // container
  Hook := FFormHooks[Form];
  Hook.AddContainerUsed(Container);
end;

class procedure TVCLFormHook.UnHookVCLForm(const Form: Vcl.Forms.TCustomForm;
  const Container: TFireMonkeyContainer);
begin
  // Assuming a hook was already installed on the form (otherwise why is this being called?)
  // tell it this container is no longer being used
  assert(FFormHooks.ContainsKey(Form));
  FFormHooks[Form].RemoveContainerUsed(Container);

  if DecrementFormUsed(Form) then begin // This was the last container on the form
    FFormHooks[Form].Free;
    FFormHooks.Remove(Form);
  end;
end;

class function TVCLFormHook.IncrementFormUsed(const Form: Vcl.Forms.TCustomForm) : Boolean;
var
  Value : Integer;
begin
  if FFormContainerCount.TryGetValue(Form, Value) then begin
    Result := false;
    FFormContainerCount.AddOrSetValue(Form, Value + 1);
  end else begin
    Result := true; // The first added
    FFormContainerCount.AddOrSetValue(Form, 1);
  end;
end;

class function TVCLFormHook.DecrementFormUsed(const Form: Vcl.Forms.TCustomForm) : Boolean;
var
  Value : Integer;
begin
  Result := false;
  if FFormContainerCount.TryGetValue(Form, Value) then begin
    Dec(Value);
    assert(Value >= 0, 'Container count decremented below 0');
    if Value = 0 then begin
      Result := true; // This was the last container on the form
      FFormContainerCount.Remove(Form)
    end else begin
      FFormContainerCount.AddOrSetValue(Form, Value);
    end;
  end else
    assert(false, 'Container count decremented but count did not exist');
end;

constructor TVCLFormHook.Create(const Form : Vcl.Forms.TCustomForm);
begin
  FForm := Form;
  FOriginalWndProc := Form.WindowProc;
  Form.WindowProc := VCLFormWndProc;
  FContainersOnThisForm := TList<TFireMonkeyContainer>.Create;

  inherited Create();
end;

destructor TVCLFormHook.Destroy;
begin
  FForm.WindowProc := FOriginalWndProc;
  assert(FContainersOnThisForm.Count = 0); // Unhooking form when a container hasn't unregistered itself?
  FContainersOnThisForm.Free;

  inherited;
end;

procedure TVCLFormHook.AddContainerUsed(const Container: TFireMonkeyContainer);
begin
  assert(not FContainersOnThisForm.Contains(Container), 'FMX container added to form twice');
  FContainersOnThisForm.Add(Container);
end;

procedure TVCLFormHook.RemoveContainerUsed(const Container: TFireMonkeyContainer);
begin
  assert(FContainersOnThisForm.Contains(Container), 'FMX container not registered with form');
  FContainersOnThisForm.Remove(Container);
end;

function TVCLFormHook.IsWindowInVCLFormTree(const Wnd: HWND): Boolean;
begin
  // This method is the reason for registering the containers on a form etc - need to know if
  // Wnd represents a FMX control embedded somewhere in this form

  Result := (Wnd = FForm.Handle) or
    Winapi.Windows.IsChild(FForm.Handle, Wnd) or
      IsHostedFMXForm(Wnd);
    //(Wnd = GetHostedFMXFormWindowHandle);
end;

function TVCLFormHook.IsHostedFMXForm(const Wnd: HWND): Boolean;
var
  Container : TFireMonkeyContainer;
begin
  for Container in FContainersOnThisForm do
    if Container.GetHostedFMXFormWindowHandle = Wnd then
      Exit(true);

  // Fallthrough: not the handle of a FMX form hosted in a container on this form
  Exit(false);
end;

procedure TVCLFormHook.VCLFormWndProc(var Msg: TMessage);
begin
  assert(Assigned(FOriginalWndProc));

  if (Msg.Msg = WM_NCACTIVATE) then begin
    HandleVCLFormNcActivate(Msg);
  end else
    FOriginalWndProc(Msg);
end;

procedure TVCLFormHook.HandleVCLFormNcActivate(var Msg: TMessage);
var
  Active : Boolean;
  HandleBeingActivated : HWND;
begin
  // When the FMX form is clicked, the VCL forms draws with an inactive title bar, despite the
  // window parenting.  Fix this by changing the active value the VCL form is told to draw
  assert(Msg.Msg = WM_NCACTIVATE);

  // If wants to draw as active, fine, pass through
  // If wants to draw as inactive, check if the FMX form is focused.  If so, draw
  // as active too.
  if not Boolean(Msg.WParam) then begin // if not active
    HandleBeingActivated := HWND(Msg.LParam); // Doesn't follow MSDN, but see http://www.catch22.net/tuts/docking-toolbars-part-1
    if HandleBeingActivated = 0 then begin
      Active := false // Window being activated belongs to another thread
    end else begin
      Active := IsWindowInVCLFormTree(HandleBeingActivated);
    end;
    Msg.WParam := WPARAM(Active);
  end;

  FOriginalWndProc(Msg);
end;

initialization
  PFPrintWindow := GetProcAddress(GetModuleHandle(Winapi.Windows.user32), 'PrintWindow'); // XP+ only
  // TVCLFormHook class constructor replacement (not a supported language feature in C++)
  TVCLFormHook.FFormHooks := TDictionary<Vcl.Forms.TCustomForm, TVCLFormHook>.Create;
  TVCLFormHook.FFormContainerCount := TDictionary<Vcl.Forms.TCustomForm, Integer>.Create;


finalization
  PFPrintWindow := nil;
  // TVCLFormHook class destructor replacement (not a supported language feature in C++)
  TVCLFormHook.FFormContainerCount.Free;
  assert(TVCLFormHook.FFormHooks.Count = 0);
  TVCLFormHook.FFormHooks.Free;

end.

