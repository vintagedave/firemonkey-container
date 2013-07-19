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
 *
 * Originally based on code found here:
 *  - http://delphisorcery.blogspot.com/2011/09/delphi-xe2-heating-up-hype-playing.html
 *  - http://stackoverflow.com/questions/7315050/delphi-xe2-possible-to-instantiate-a-firemonkey-form-in-vcl-application?rq=1
 * but with substantial modifications.
 *
 * ***** END LICENSE BLOCK ***** *)

interface

uses
  Vcl.Controls, FMX.Forms, Winapi.Messages, System.Classes, Winapi.Windows;

type
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)] // Thanks Edgar Reis
  TFireMonkeyContainer = class(TWinControl)
  private
    FFMXForm : FMX.Forms.TCommonCustomForm;
    FOldWndProc : System.Classes.TWndMethod;

    procedure SetFMXForm(Form : FMX.Forms.TCommonCustomForm);
    procedure HandleResize;
    procedure HostTheFMXForm;
    procedure HideFMAppClassWindow;
    function GetFMXFormWindowHandle : HWND;

    procedure FormWndProc(var Msg: TMessage);
    procedure SubClassForm;
    procedure UnsubClassForm;
    procedure HandleFormNcActivate(var Msg: TMessage);

    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
  protected
    procedure Resize; override;
    procedure CreateHandle; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(Owner : TComponent); override;
    destructor Destroy; override;
  published
    property FireMonkeyForm : FMX.Forms.TCommonCustomForm read FFMXForm write SetFMXForm;
    property Align;
    property Anchors;
    property Constraints;
    property AlignWithMargins;
    property Left;
    property Top;
    property Width;
    property Height;
  end;

implementation

uses
  FMX.Platform, FMX.Platform.Win, System.Types, SysUtils, Graphics, Vcl.Forms, Vcl.Dialogs;

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

{ TDMFiremonkeyContainer }

constructor TFireMonkeyContainer.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  FFMXForm := nil;
  FOldWndProc := nil;
end;

destructor TFireMonkeyContainer.Destroy;
begin
  UnsubclassForm;
  inherited;
end;

procedure TFireMonkeyContainer.CreateHandle;
begin
  inherited;
  // When this form's handle changes, update the hosted FMX form (setting parent, position, etc)
  UnSubclassForm;
  if Assigned(FFMXForm) then begin
    HostTheFMXForm;
  end;
end;

procedure TFireMonkeyContainer.SubClassForm;
begin
  FOldWndProc := GetParentForm(Self).WindowProc;
  GetParentForm(Self).WindowProc := FormWndProc;
end;

procedure TFireMonkeyContainer.UnsubClassForm;
begin
  if Assigned(FOldWndProc) and Assigned(GetParentForm(Self)) then begin
    GetParentForm(Self).WindowProc := FOldWndProc;
    FOldWndProc := nil;
  end;
end;

procedure TFireMonkeyContainer.FormWndProc(var Msg: TMessage);
begin
  assert(Assigned(FOldWndProc));
  if Msg.Msg = WM_NCACTIVATE then begin
    HandleFormNcActivate(Msg);
  end else begin
    FOldWndProc(Msg);
  end;
end;

procedure TFireMonkeyContainer.HandleFormNcActivate(var Msg: TMessage);
var
  FMXHandle : Winapi.Windows.HWND;
  Active : Boolean;
  HandleBeingActivated : HWND;
  ParentForm : TCustomForm;
begin
  // When the FMX form is clicked, the VCL forms draws with an inactive title bar, despite the
  // window parenting.  Fix this by changing the active value the VCL form is told to draw
  assert(Msg.Msg = WM_NCACTIVATE);

  FMXHandle := GetFMXFormWindowHandle;
  ParentForm := GetParentForm(Self);

  // If wants to draw as active, fine, pass through
  // If wants to draw as inactive, check if the FMX form is focused.  If so, draw
  // as active too.
  if not Boolean(Msg.WParam) then begin // if not active
    HandleBeingActivated := HWND(Msg.LParam); // Doesn't follow MSDN, but see http://www.catch22.net/tuts/docking-toolbars-part-1
    if HandleBeingActivated = 0 then begin
      Active := false // Window being activated belongs to another thread
    end else begin
      Active := (HandleBeingActivated = ParentForm.Handle) or
        Winapi.Windows.IsChild(ParentForm.Handle, HandleBeingActivated) or
        (HandleBeingActivated = FMXHandle);
    end;
    Msg.WParam := WPARAM(Active);
  end;

  FOldWndProc(Msg);
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
  if Assigned(FFMXForm) then begin
    UnSubclassForm;
    FFMXForm.RemoveFreeNotification(Self);
  end;

  FFMXForm := Form;
  if Assigned(FFMXForm) then begin
    FFMXForm.FreeNotification(Self);
    HideFMAppClassWindow;
    if HandleAllocated then HostTheFMXForm; // Will otherwise occur in CreateHandle
  end else begin
    UnSubclassForm;
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
begin
  // Don't change the FMX form etc when in design mode - changes the actual, designing form in the IDE tab
  if not (csDesigning in ComponentState) then begin
    FFMXForm.BorderIcons := [];
    FFMXForm.BorderStyle := TFmxFormBorderStyle.bsNone;
    HandleResize;
    FFMXForm.Visible := True;
    Winapi.Windows.SetParent(GetFMXFormWindowHandle, Handle);
    SubClassForm;
  end;
end;

procedure TFireMonkeyContainer.HideFMAppClassWindow;
begin
  // XE4 (possibly others) show a phantom TFMAppClass window on the taskbar. Hide it.
  EnumWindows(@EnumWindowCallback, 0);
end;

function TFireMonkeyContainer.GetFMXFormWindowHandle: HWND;
var
  WinHandle : TWinWindowHandle;
begin
  assert(Assigned(FFMXForm));
  Result := 0;
  if Assigned(FFMXForm) and Assigned(FFMXForm.Handle) then begin
    WinHandle := WindowHandleToPlatform(FFMXForm.Handle);
    if Assigned(WinHandle) then Exit(WinHandle.Wnd);
  end;
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
        if not Assigned(PFPrintWindow) or (not PFPrintWindow(GetFMXFormWindowHandle, Canvas.Handle, PW_CLIENTONLY)) then begin
          // Paint a message that was unable to show a preview image
          Rect.Inflate(-16, -16);
          strText := FFMXForm.Name + ' : Unable to draw preview image';
          Winapi.Windows.DrawTextEx(Canvas.Handle, PChar(strText), Length(strText), Rect,
            DT_CENTER or DT_WORDBREAK or DT_END_ELLIPSIS, nil);
        end;
      end else begin
        // Otherwise, paint a message that you can host a form
        Rect.Inflate(-16, -16);
        strText := strDefaultText;
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

initialization
  PFPrintWindow := GetProcAddress(GetModuleHandle(Winapi.Windows.user32), 'PrintWindow'); // XP+ only

finalization
  PFPrintWindow := nil;

end.
