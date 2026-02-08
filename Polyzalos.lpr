{
  |
  |  Polyzalos, FKA OrgaMon
  |  https://wiki.orgamon.org/
  |  SPDX-License-Identifier: MIT
  |
  |               ____       _                _
  |              |  _ \ ___ | |_   _ ______ _| | ___  ___
  |              | |_) / _ \| | | | |_  / _` | |/ _ \/ __|
  |              |  __/ (_) | | |_| |/ / (_| | | (_) \__ \
  |              |_|   \___/|_|\__, /___\__,_|_|\___/|___/
  |                            |___/
  |
  |  Copyright (C) 2026  Andreas Filsinger
  |
  |  Permission is hereby granted, free of charge, to any person obtaining a copy
  |  of this software and associated documentation files (the "Software"), to deal
  |  in the Software without restriction, including without limitation the rights
  |  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  |  copies of the Software, and to permit persons to whom the Software is
  |  furnished to do so, subject to the following conditions:
  |
  |  The above copyright notice and this permission notice shall be included in all
  |  copies or substantial portions of the Software.
  |
  |  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  |  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  |  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  |  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  |  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  |  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  |  SOFTWARE.
  |
}
program Polyzalos;

{$codepage cp1252}
{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp,
  { you can add units after this }
  Charset,
  fpchelper,
  html,
  CareTakerClient,
  SimplePassword,
  Geld,
  SolidFTP,
  srvXMLRPC,
  OrientationConvert,
  globals,
  Funktionen_Basis,
  Funktionen_Auftrag,
  Identitaet,
  tgputtylib,
  zcomponent,
  zcore;

begin
 setIdentitaetAndRun;
end.

