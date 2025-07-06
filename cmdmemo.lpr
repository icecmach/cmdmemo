program cmdmemo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp,
  StrUtils;

type

  { TCmdMemo }

  TCmdMemo = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
    procedure WriteUsage; virtual;
    procedure AddEntry; virtual;
    procedure ListEntry(paramL: string); virtual;
    function GetConfFile: string; virtual;
  end;

  TRow = record
    cmd: string;
    cat: string;
    desc: string;
  end;

{ TCmdMemo }

procedure TCmdMemo.DoRun;
var
  ErrorMsg: String;
  paramL: String;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('hal', 'help add list');
  if ErrorMsg<>'' then begin
    //ShowException(Exception.Create(ErrorMsg));
    WriteLn(ErrorMsg + ''#13#10);
    WriteUsage;
    Terminate;
    Exit;
  end;

  // check if parameters are provided
  if not HasOption('a', 'add')  and
     not HasOption('h', 'help') and
     not HasOption('l', 'list') then
  begin
    WriteUsage;
    Terminate;
    Exit;
  end;

  // parse parameters -h --help
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  // parse parameters -a --add
  if HasOption('a', 'add') then
  begin
    AddEntry;
    Terminate;
    Exit;
  end;

    // parse parameters -l --list
  if HasOption('l', 'list') then
  begin
    paramL := GetOptionValue('l', 'list');

    ListEntry(paramL);
    Terminate;
    Exit;
  end;

  // stop program loop
  Terminate;
end;

constructor TCmdMemo.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TCmdMemo.Destroy;
begin
  inherited Destroy;
end;

procedure TCmdMemo.WriteHelp;
begin
  WriteUsage;
  WriteLn('Program Description:');
  WriteLn('  This is a simple program that stores snippets of frequently used commands'#13#10);
  WriteLn('Examples:');
  WriteLn('  1) Add the command used to list services to the systemd category');
  WriteLn('  cmdmemo -a');
  WriteLn('  Enter a command: systemctl --type=service');
  WriteLn('  Enter a category: systemd');
  WriteLn('  Enter a description: List services'#13#10);
  WriteLn('  2) List command snippets from systemd category');
  WriteLn('  cmdmemo -l systemd'#13#10);
  WriteLn('  ┌──────────────────────────────────────────────┐');
  WriteLn('  │Command|Category                |Description  |');
  WriteLn('  │───────|────────────────────────|─────────────|');
  WriteLn('  │systemd|systemctl --type=service|List services|');
  WriteLn('  └──────────────────────────────────────────────┘');
end;

procedure TCmdMemo.WriteUsage;
begin
  writeln('Usage: ', Title, ' [args]'#13#10);
  WriteLn('Options:');
  WriteLn('  -h, --help show this help message and exit');
  WriteLn('  -a, --add  add a command snippet');
  WriteLn('  -l, --list list all entries or for a specific category'#13#10);
end;

function TCmdMemo.GetConfFile: string;
var
  configFile: String;
begin
  configFile := GetAppConfigDir(false) + 'cmdmemo.conf';
  Result := configFile;

  if not FileExists(configFile) then
  begin
    Writeln(Format('The file "%s" doesn''t exist', [configFile]));
    Terminate;
    Exit;
  end;
end;

procedure TCmdMemo.AddEntry;
var
  configFile: String;
  F: TextFile;
  command, category, description: String;
begin
  // check for .conf file
  configFile := GetConfFile;

  if Terminated then Exit;

  Write('Enter command: ');
  ReadLn(command);

  if Length(command) = 0 then
  begin
    writeln('The input string is empty.');
    Terminate;
    Exit;
  end;

  Write('Enter category: (default empty) ');
  ReadLn(category);

  Write('Enter description: (default empty) ');
  ReadLn(description);

  try
    AssignFile(F,configFile);
    Append(F);
    WriteLn(F, Format('%s,%s,%s', [command, category, description]));
    if category = '' then
      category:= 'empty';
    if description = '' then
      description:= 'empty';
    WriteLn(Format('Snippet added for command: %s, category: %s, description: %s', [command, category, description]));
  finally
    CloseFile(F);
  end;
end;

procedure TCmdMemo.ListEntry(paramL: string);
var
  configFile: String;
  F: TextFile;
  Line: String;
  Column1: String;
  delimiter: TSysCharSet = [','];
  data: array of TRow;
  i: integer;
  maxCmdLen, maxCatLen, maxDescLen: Integer;
begin
  // check for .conf file
  configFile := GetConfFile;

  if Terminated then Exit;

  try
    AssignFile(F,configFile);
    Reset(F);
    SetLength(data, 0);

    while not eof(F) do
    begin
      // load array
      ReadLn(F,Line);
      Column1 := ExtractDelimited(2, Line, delimiter);

      // If param is specified list only its entries
      if (paramL <> '') and (Pos(paramL, Column1) = 0) then
        Continue;

      SetLength(data, Length(data) + 1);
      data[High(data)].cmd  := ExtractDelimited(1, Line, delimiter);
      data[High(data)].cat  := ExtractDelimited(2, Line, delimiter);
      data[High(data)].desc := ExtractDelimited(3, Line, delimiter);
    end;
  finally
    CloseFile(F);
  end;

  // Terminate if no entries found
  if Length(data) = 0 then
  begin
    WriteLn('No entry found for ', paramL);
    Terminate;
    Exit;
  end;

  maxCmdLen := 7;
  maxCatLen := 8;
  maxDescLen := 11;

  for i := Low(data) to High(data) do
  begin
    if Length(data[i].cmd) > maxCmdLen then
      maxCmdLen := Length(data[i].cmd);

    if Length(data[i].cat) > maxCatLen then
      maxCatLen := Length(data[i].cat);

    if Length(data[i].desc) > maxDescLen then
      maxDescLen := Length(data[i].desc);
  end;

  // print header
  WriteLn('┌' +
          DupeString('─', maxCmdLen + maxCatLen + maxDescLen + 2) +
          '┐');

  WriteLn('│Command'    + PadRight('', maxCmdLen -7)   + '│' +
          'Category'    + PadRight('', maxCatLen  -8)  + '│' +
          'Description' + PadRight('', maxDescLen -11) + '│');

  WriteLn('│' +
          DupeString('─', maxCmdLen + maxCatLen + maxDescLen + 2) +
          '│');

  // print commands
  for i := Low(data) to High(data) do
  begin
    WriteLn('│' +
            data[i].cmd  + PadRight('', maxCmdLen  - Length(data[i].cmd))  + '│' +
            data[i].cat  + PadRight('', maxCatLen  - Length(data[i].cat))  + '│' +
            data[i].desc + PadRight('', maxDescLen - Length(data[i].desc)) + '│');
  end;

  // print footer
  WriteLn('└' +
          DupeString('─', maxCmdLen + maxCatLen + maxDescLen + 2) +
          '┘');
end;

var
  Application: TCmdMemo;
begin
  Application:=TCmdMemo.Create(nil);
  Application.Title:='cmdmemo';
  Application.Run;
  Application.Free;
end.
