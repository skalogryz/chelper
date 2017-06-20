{ The unit is part of Lazarus Chelper package

  Copyright (C) 2010 Dmitry Boyarintsev skalogryz dot lists at gmail.com

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}
unit TextParsingUtils;

{$ifdef fpc}{$mode delphi}{$h+}{$endif}

interface

uses
  Types;

type
  TCharSet = set of Char;

const
  EoLnChars      = [#10,#13];
  SpaceChars     = [#32,#9];
  InvsChars      = [#0..#32];
  WhiteSpaceChars = SpaceChars;
  SpaceEolnChars = EoLnChars+SpaceChars;
  NumericChars   = ['0'..'9'];
  AlphabetChars  = ['a'..'z','A'..'Z'];
  AlphaNumChars  = AlphabetChars+NumericChars;

function ScanWhile(const s: AnsiString; var index: Integer; const ch: TCharSet): AnsiString; overload;
function ScanBackWhile(const s: AnsiString; var index: Integer; const ch: TCharSet): AnsiString;
function ScanTo(const s: AnsiString; var index: Integer; const ch: TCharSet): AnsiString;
function ScanBackTo(const s: AnsiString; var index: Integer; const ch: TCharSet): AnsiString;
procedure SkipTo(const s: AnsiString; var index: Integer; const ch: TCharSet);
procedure SkipWhile(const s: AnsiString; var index: Integer; const ch: TCharSet);
procedure SkipToEoln(const s: AnsiString; var index: Integer);

// returns #10, #13, #10#13 or #13#10, if s[index] is end-of-line sequence
// otherwise returns empty string
function EolnStr(const s: AnsiString; index: Integer): String;

function IsSubStr(const sbs, s: AnsiString; index: Integer): Boolean;

// todo: not used?
function SkipCommentBlock(const s: AnsiString; var index: Integer; const closecmt: AnsiString): AnsiString;

function SkipLine(const s: AnsiString; var index: Integer): AnsiString;

procedure OffsetToLinePos(const t: AnsiString; Offset: Integer; var P: TPoint);


type
  TRange = record stofs, endofs : Integer; end;

  { TSubBuffer }

  TSubBuffer = class(TObject)
    Ranges      : array of TRange;
    RangesCount : Integer;
    Name        : string;
    Tag         : TObject;
    constructor Create(const AName: string; ATag: TObject);
  end;

  { TTextBuffer }

  TTextBuffer = class(TObject)
  private
    function GetSubBuffer(i: Integer): TSubBuffer;
  protected
    function GetCount: Integer;
  public
    buffer: String;
    constructor Create(const Abuffer: String=''; const aname: string = ''; aobj: TObject = nil);
    procedure InsertSubBuffer(pos: Integer; const ABuffer: string; const AName: string = ''; ATag: TObject = nil);
    property SubBuffer[i: Integer]: TSubBuffer read GetSubBuffer;
    property Count: Integer read GetCount;
  end;

type
  TFileOfsInfo = record
    origOfs : Integer; // original 1-based index in the file
    delta   : Integer; // the new delta that should be used starting this file
  end;

  { TFileOffsets }

  TFileOffsets = class(TObject)
  public
    Ofs   : array of TFileOfsInfo;
    Count : Integer;
    procedure AddOffset(origOfs, delta: integer);
    function OrigOffset(tempOfs: integer): Integer;
  end;

implementation

function ScanWhile(const s: AnsiString; var index: Integer; const ch: TCharSet
  ): string;
var
  i : Integer;
begin
  Result := '';
  if (index <= 0) or (index > length(s)) then Exit;
  for i := index to length(s) do
    if not (s[i] in ch) then begin
      if i = index then Result := ''
      else Result := Copy(s, index, i - index);
      index := i;
      Exit;
    end;
  Result := Copy(s, index, length(s) - index + 1);
  index := length(s) + 1;
end;

function ScanBackWhile(const s: AnsiString; var index: Integer; const ch: TCharSet): AnsiString;
var
  j : integer;
begin
  Result:='';
  if (index <= 0) or (index > length(s)) then Exit;
  j:=index;
  while (index>0) and (s[index] in ch) do dec(index);
  Result:=Copy(s, index+1, j-index);
end;

procedure SkipTo(const s: AnsiString; var index: Integer; const ch: TCharSet);
begin
  if (index <= 0) or (index > length(s)) then Exit;
  while (index<=length(s)) and not (s[index] in ch) do inc(index);
end;

procedure SkipWhile(const s: AnsiString; var index: Integer; const ch: TCharSet);
begin
  if (index <= 0) or (index > length(s)) then Exit;
  while (index<=length(s)) and (s[index] in ch) do inc(index);
end;

function ScanBackTo(const s: AnsiString; var index: Integer; const ch: TCharSet): AnsiString;
var
  j : integer;
begin
  Result:='';
  if (index <= 0) or (index > length(s)) then Exit;
  j:=index;
  while (index>0) and not (s[index] in ch) do dec(index);
  Result:=Copy(s, index+1, j-index);
end;

function ScanTo(const s: AnsiString; var index: Integer; const ch: TCharSet): AnsiString;
var
  i : Integer;
begin
  Result := '';
  if (index <= 0) or (index > length(s)) then Exit;
  for i := index to length(s) do
    if (s[i] in ch) then begin
      if i = index then Result := ''
      else Result := Copy(s, index, i - index);
      index := i;
      Exit;
    end;
  Result := Copy(s, index, length(s) - index + 1);
  index := length(s) + 1;
end;

function EolnStr(const s: AnsiString; index: Integer): String;
begin
  if (index<=0) or (index>length(s)) or (not (s[index] in EoLnChars)) then
    Result:=''
  else begin
    if (index<length(s)) and (s[index+1] in EolnChars) and (s[index]<>s[index+1]) then
      Result:=Copy(s, index, 2)
    else
      Result:=s[index];
  end;
end;

procedure SkipToEoln(const s: AnsiString; var index: Integer);
begin
  SkipTo(s, index, EoLnChars);
end;

function IsSubStr(const sbs, s: AnsiString; index: Integer): Boolean;
var
  i : Integer;
  j : Integer;
begin
  Result := false;
  if (sbs = '') or (length(sbs) > length(s) - index) then Exit;
  j := index;
  for i := 1 to length(sbs) do begin
    if sbs[i] <> s[j] then Exit;
    inc(j);
  end;
  Result := true;
end;

function SkipCommentBlock(const s: AnsiString; var index: Integer; const closecmt: AnsiString): AnsiString;
begin
  Result := '';
  if closecmt = '' then begin
    index := length(s) + 1;
    Exit;
  end;
  while index <= length(s) do begin
    Result := Result + ScanTo(s, index, [closecmt[1]]+EoLnChars);
    //if (index<=length(s)) and (s in EoLnChars(

    if IsSubStr(closecmt, s, index) then begin
      inc(index, length(closecmt));
      Exit;
    end else begin
      Result := Result + s[index];
      inc(index);
    end;
  end;
end;

function SkipLine(const s: AnsiString; var index: Integer): AnsiString;
begin
  Result:=ScanTo(s, index, EoLnChars);
  if (index<length(s)) and (s[index+1] in EoLnChars) and (s[index]<>s[index+1]) then
    inc(index);
  inc(index);
end;

procedure OffsetToLinePos(const t: AnsiString; Offset: Integer; var P: TPoint);
var
  i,  le  : Integer;
begin
  i := 1;
  le := 0;
  P.X := 0;
  P.Y := 0;
  while i < Offset do begin
    Inc(P.Y);
    le := i;
    SkipLine(t, i);
  end;
  P.X := Offset - le + 1;
end;

{ TTextBuffer }

function TTextBuffer.GetSubBuffer(i: Integer): TSubBuffer;
begin
  Result:=nil;
end;

function TTextBuffer.GetCount: Integer;
begin
  Result:=0;
end;

constructor TTextBuffer.Create(const Abuffer: String; const aname: string;
  aobj: TObject);
begin
  if abuffer<>'' then
    InsertSubBuffer(1, abuffer, aname, aobj);
end;

procedure TTextBuffer.InsertSubBuffer(pos: Integer; const ABuffer: string; const AName: string; ATag: TObject);
begin

end;

{ TSubBuffer }

constructor TSubBuffer.Create(const AName: string; ATag: TObject);
begin
  inherited Create;
  Name:=AName;
  Tag:=ATag;
end;

procedure TFileOffsets.AddOffset(origOfs, delta: integer);
begin
  if Count=length(Ofs) then begin
    if Count=0 then SetLength(Ofs, 4)
    else SetLength(Ofs, Count*2);
  end;
  Ofs[Count].origOfs:=origOfs;
  Ofs[Count].delta:=delta;
  inc(Count);
end;

function TFileOffsets.OrigOffset(tempOfs: integer): Integer;
var
  i : Integer;
begin
  Result:=tempOfs;
  for i:=0 to Count-1 do begin
    if (Ofs[i].origOfs <= tempOfs) then
      inc(Result, Ofs[i].delta);
  end;
end;



end.

