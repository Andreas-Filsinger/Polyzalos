unit systemd;

{$mode objfpc}{$H+}

// #
// # OS - Wrapper
// #
// # win32: Aufruf externer Anwendungen
// # linux: rudimentäres libsystemd API um "ordentliche" Services schreiben zu können
// #

interface

uses
 Classes;

// Tools
function CallExternalApp(Cmd: string; const CmdShow: Integer): Cardinal;
function RunExternalApp(Cmd: string; const CmdShow: Integer): boolean;
procedure WakeOnLan(MAC: string); // sample: WakeOnLan('00-07-95-1C-64-7E');

// Dokumente konvertieren
function html2pdf(Dokument: string; OnlyIfOutDated: boolean = true): TStringList;

const
   SD_LISTEN_FDS_START = 3;

// systemd
function RunningAsService: Boolean;
procedure Wuff;
procedure Ready;

implementation

uses
 sd_daemon, anfix, CareTakerClient,
 fpchelper,
 IdUDPClient,
 SysUtils;

function CallExternalApp(Cmd: string; const CmdShow: Integer): Cardinal;
begin
 if DebugMode then
   AppendStringsToFile(Cmd, DebugLogPath + 'SYSTEMD-' + DatumLog + cLogExtension, Uhr8);
 {$ifdef FPC}
 // imp pend
 {$else}
 result := JclMiscel.WinExec32AndWait(Cmd,CmdShow);
 {$endif}
 if DebugMode then
   AppendStringsToFile(IntToStr(result), DebugLogPath + 'SYSTEMD-' + DatumLog + cLogExtension, Uhr8);
end;

function RunExternalApp(Cmd: string; const CmdShow: Integer): boolean;
begin
 if DebugMode then
   AppendStringsToFile(Cmd, DebugLogPath + 'SYSTEMD-' + DatumLog + cLogExtension, Uhr8);
 {$ifdef FPC}
 // imp pend
 {$else}
 result := JclMiscel.WinExec32(Cmd,CmdShow);
 {$endif}
 if DebugMode then
   AppendStringsToFile(BoolToStr(result), DebugLogPath + 'SYSTEMD-' + DatumLog + cLogExtension, Uhr8);
end;

function RunningAsService: Boolean;
begin
 result := GetEnvironmentVariable('INVOCATION_ID')<>'';
end;

procedure Wuff;
begin
 sd_notify(0, 'WATCHDOG=1')
end;

procedure Ready;
begin
 // sample before:     sd_notify( 0, "RELOADING=1\nSTATUS=50% done\n" );
 sd_notify(0, 'READY=1')
end;

const
 wkhtmltopdf_Installation: string = '';

function html2pdf(Dokument: string; OnlyIfOutDated: boolean = true): TStringList;
const
 cHTMLextension = '.html';
 cPDFextension = '.pdf';
var
 Dokument_pdf : string;
 k : integer;
 ErrorMsg: string;
begin
  repeat

     result := TStringList.Create;
     ErrorMsg := '';
     if not(FileExists(Dokument)) then
     begin
       ErrorMsg := 'Quell-HTML nicht gefunden!';
       break;
     end;

     if (wkhtmltopdf_Installation='') then
     begin

      repeat
       wkhtmltopdf_Installation := 'C:\Program Files\wkhtmltopdf\bin\wkhtmltopdf.exe';
       if FileExists(wkhtmltopdf_Installation) then
        break;
       // imp pend
       //wkhtmltopdf_Installation := ProgramFilesDir + 'wkhtmltopdf\bin\wkhtmltopdf.exe';
       if FileExists(wkhtmltopdf_Installation) then
        break;
       wkhtmltopdf_Installation := 'C:\Program Files (x86)\wkhtmltopdf\bin\wkhtmltopdf.exe';
       if FileExists(wkhtmltopdf_Installation) then
        break;
       wkhtmltopdf_Installation := '';
      until yet;

     end;

    if (wkhtmltopdf_Installation='') then
    begin
      ErrorMsg := 'wkhtmltopdf Installation nicht gefunden!';
      break;
    end;

    Dokument_pdf := Dokument;

    k := revpos(cHTMLextension, Dokument_pdf);
    if (k > 0) then
    begin
      Dokument_pdf := copy(Dokument_pdf, 1, pred(k)) + cPDFExtension
    end
    else
    begin
      ErrorMsg := 'Anteil "' + cHTMLextension + '" in "' + Dokument + '" nicht gefunden!';
      break;
    end;

    //
    if
     {} not(OnlyIfOutDated) or
     {} (FileDateTime(Dokument) > FileDateTime(Dokument_pdf)) then
    begin
      if (pos('--',iPDFZoom)=0) then
        CallExternalApp(
          { } '"' + wkhtmltopdf_Installation + '"' + ' ' +
          { } '--quiet ' +
          { } '--print-media-type ' +
          { } '--page-width 2480px ' + // DIN A4 Format
          { } '--page-height 3508px ' +
          { } '--margin-top 100px ' +
          { } '--margin-bottom 9px ' +
          { } '--margin-left 9px ' +
          { } '--margin-right 9px ' +
          { } '--dpi 150 ' +
          { } '--zoom '+iPDFZoom+' ' +
          { } '"' + Dokument + '"' + ' ' +
          { } '"' + Dokument_pdf + '"',
          { } SW_HIDE)
      else
        CallExternalApp(
          { } '"' + wkhtmltopdf_Installation + '"' + ' ' +
          { } iPDFZoom + ' ' +
          { } '"' + Dokument + '"' + ' ' +
          { } '"' + Dokument_pdf + '"',
          { } SW_HIDE);
    end;

    if (FSize(Dokument_pdf)<739) then
    begin
      ErrorMsg := 'PDF-Erstellung ist nicht erfolgt. Ev. keine wkhtmltopdf Installation gefunden!';
      break;
    end;

  until true;
  if (ErrorMsg<>'') then
   result.values['ERROR'] := ErrorMsg
  else
   result.values['ConversionOutFName'] := Dokument_pdf;
end;

procedure WakeOnLan(MAC: string); // sample: WakeOnLan('00-07-95-1C-64-7E');

  function nextp(var s: string; Delimiter: string): string;
  var
    k: integer;
  begin
    k := pos(Delimiter, s);
    if (k > 0) then
    begin
      result := copy(s, 1, pred(k));
      Delete(s, 1, pred(k + length(Delimiter)));
    end
    else
    begin
      result := s;
      s := '';
    end;
  end;

var
  OutStr: AnsiString;
  OneLine: AnsiString;
  n: integer;
  BroadCaster: TIdUDPClient;
  Delimiter: char;
begin
  // check Delimiter
  if (pos('-', MAC) > 0) then
    Delimiter := '-'
  else
    Delimiter := ':';

  // assemble one MAC
  OneLine := '';
  for n := 0 to pred(6) do
    OneLine := OneLine + chr(strtoint('$' + nextp(MAC, Delimiter)));

  // assemble magic Paket
  OutStr := fill(#$FF, 6);
  for n := 0 to pred(16) do
    OutStr := OutStr + OneLine;

  // broadcast magic Paket
  BroadCaster := TIdUDPClient.Create(nil);
{$IFDEF FPC}
  BroadCaster.broadcast(OutStr, 9, '');
{$ELSE}
{$IFDEF VER310}
  BroadCaster.broadcast(OutStr, 9, '');
{$ELSE}
  BroadCaster.broadcast(OutStr, 9, '', TEncoding.ANSI);
{$ENDIF}
{$ENDIF}
  BroadCaster.free;
end;

end.
