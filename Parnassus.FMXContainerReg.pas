unit Parnassus.FMXContainerReg;

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
  System.Classes, Parnassus.FMXContainer;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Parnassus', [TFireMonkeyContainer]);
end;

end.
