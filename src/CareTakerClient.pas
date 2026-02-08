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
  |  Copyright (C) 2007 - 2026  Andreas Filsinger
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
unit CareTakerClient;

interface

uses
  Classes;

type
  // Für die "Funktions Sicherstellung"
  tTestProc = procedure(Path: string) of object;
  tSelfTestProc = function: TStringList of object;

const
  cERRORText = 'ERROR:'; // never change!
  cWARNINGText = 'WARNING:'; // never change!
  cINFOText = 'INFO:'; // never change!
  cEXCEPTIONText = 'EXCEPTION:'; // never change!
  // Log-Files
  cLogExtension = '.log.txt';
  cOKText = 'OK!'; // never change!
  iWikiServer: string = '';
  ReportedErrorCount: integer = 0;
  reinInformativ = true;

function Int64asKeyStr(i: int64): AnsiString;
function KeyStrasInt64(s: AnsiString): int64;
function MachineID: string;
procedure MachineIDChanged;
function vhost(url: string): string;
function cHelpURL: string;
function ResolveServer(s: string): string;
function e_r_Kontext: string;
function ErrorFName(Namespace: string; only4yi: boolean = false):string;

implementation

uses
  sysutils, fpchelper,

  // tools
  unix, anfix, SimplePassword,

  // Indy
  IdHTTP;

const
  _TrueServerName: string = '';
  _MachineID: string = '';

function MachineID: string;
begin
  if (_MachineID = '') then
    _MachineID :=
     {} GetUserName + '@' +
     {} 'de_DE' + '.' +
     {} GetHostName + '.' +
     {} MandantName;
  result := _MachineID;
end;

procedure MachineIDChanged;
begin
  _MachineID := '';
end;

function ResolveServer(s: string): string;
var
  hPROXY: TIdHTTP;
begin
  try
    hPROXY := TIdHTTP.create(nil);
    _TrueServerName :=
      StrFilter(ExtractSegmentBetween(hPROXY.get(s + '?p=' +
      FindANewPassword('', 15)), '<BODY>', '</BODY>', true), '.0123456789');
    result := _TrueServerName;
    hPROXY.free;
  except
    result := '';
  end;
end;

function vhost(url: string): string;
var
  hCARETAKER: TIdHTTP;
  RawResult: string;
begin
  try
    hCARETAKER := TIdHTTP.create(nil);
    RawResult := ExtractSegmentBetween(hCARETAKER.get(url), '<BODY>',
      '</BODY>', true);
    hCARETAKER.free;
    result := StrFilter(RawResult, '0123456789.', false);
  except
    result := '';
  end;
end;

function cHelpURL: string;
begin
  if (iWikiServer <> '') then
  begin
    result := iWikiServer + '?title='
  end
  else
  begin
    result := 'https://wiki.orgamon.org/?title=';
  end;
end;

function Int64asKeyStr(i: int64): AnsiString;
var
  L, n: integer;
begin
  SetLength(result, 8);
  L := sizeof(i);
  for n := 1 to L do
  begin
    result[n] := AnsiChar(i AND 255);
    i := i shr 8;
  end;
end;

function KeyStrasInt64(s: AnsiString): int64;
var
  L: integer;
begin
  result := 0;
  if (length(s) <> 8) then
    raise Exception.create
      ('KeyStr as a base of int64 must have the length of 8 Bytes');
  for L := 8 downto 1 do
  begin
    result := result shl 8;
    result := result + ord(s[L]);
  end;
end;

function getQuestion(Path: string): TStringList;
begin
  result := TStringList.create;
end;

procedure setAnswer(sAnswer: TStringList);
begin

end;

const
  _Kontext: string = '';

function e_r_Kontext: string;
begin
  if (_Kontext = '') then
    _Kontext := FindANewPassword;
  result := _Kontext;
end;

function ErrorFName(Namespace: string; only4yi: boolean = false):string;
begin
  result :=
   { } DebugLogPath +
   { } NameSpace + '-' +
   { } DatumLog + '-' +
   { } e_r_Kontext + '-' +
   { } 'ERROR'+
   {} cLogExtension;
  if not(only4yi) then
   inc(ReportedErrorCount);
end;


end.

