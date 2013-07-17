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
  Vcl.Controls, FMX.Forms, Winapi.Messages, System.Classes;

type
  TFireMonkeyContainer = class(TWinControl)
  private
    FFMXForm : FMX.Forms.TCommonCustomForm;
    procedure SetFMXForm(Form : FMX.Forms.TCommonCustomForm);
    procedure HandleResize;
    procedure HostTheFMXForm;
    procedure HideFMAppClassWindow;

    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
  protected
    procedure Resize; override;
    procedure CreateHandle; override;
  public
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
  FMX.Platform, FMX.Platform.Win, System.Types, Windows, SysUtils, Graphics;

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

procedure TFireMonkeyContainer.CreateHandle;
begin
  inherited;
  // When this form's handle changes, update the hosted FMX form (setting parent, position, etc)
  if Assigned(FFMXForm) then begin
    HostTheFMXForm;
  end;
end;

procedure TFireMonkeyContainer.HandleResize;
var
  WindowService : IFMXWindowService;
  DesignRect : TRectF;
begin
  if Assigned(FFMXForm) and HandleAllocated then begin
    WindowService := TPlatformServices.Current.GetPlatformService(IFMXWindowService) as IFMXWindowService;
    WindowService.SetWindowRect(FFMXForm, RectF(0, 0, Width, Height));
    FFMXForm.Invalidate;
    if csDesigning in ComponentState then Invalidate;
  end;
end;

procedure TFireMonkeyContainer.Resize;
begin
  inherited;
  HandleResize;
end;

procedure TFireMonkeyContainer.SetFMXForm(Form: FMX.Forms.TCommonCustomForm);
begin
  FFMXForm := Form;
  if Assigned(FFMXForm) then begin
    HideFMAppClassWindow;
    if HandleAllocated then HostTheFMXForm; // Will otherwise occur in CreateHandle
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

procedure TFireMonkeyContainer.HideFMAppClassWindow;
begin
  // XE4 (possibly others) show a phantom TFMAppClass window on the taskbar. Hide it.
  EnumWindows(@EnumWindowCallback, 0);
end;

procedure TFireMonkeyContainer.HostTheFMXForm;
var
  WinHandle : TWinWindowHandle;
begin
  WinHandle := WindowHandleToPlatform(FFMXForm.Handle);
  FFMXForm.BorderIcons := [];
  FFMXForm.BorderStyle := TFmxFormBorderStyle.bsNone;
  HandleResize;
  FFMXForm.Visible := True;
  Windows.SetParent(WinHandle.Wnd, Handle);
end;

procedure TFireMonkeyContainer.WMPaint(var Message: TWMPaint);
const
  strDefaultText = 'TFireMonkeyContainer' + #10#13#10#13 + 'Set the FireMonkeyForm property to ' +
    ' an autocreated FireMonkey form at designtime (with some limitation), or in code at runtime. You can' +
    ' host both 2D (HD) and 3D FireMonkey forms.';
var
  Canvas : TControlCanvas;
  Rect : TRect;
  strText : string;
begin
  inherited;
  if (csDesigning in ComponentState) and (not Assigned(FFMXForm)) then begin
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
      // And paint a message with a small border
      Rect.Inflate(-8, -8);
      strText := strDefaultText;
      if Name <> '' then strText := Name + ' : ' + strDefaultText
        else strText := strDefaultText;
      Windows.DrawTextEx(Canvas.Handle, PChar(strText), Length(strText), Rect,
        DT_CENTER or DT_WORDBREAK or DT_END_ELLIPSIS, nil);
    finally
      Canvas.Free;
    end;
  end;
end;

end.
