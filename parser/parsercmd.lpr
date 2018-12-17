program parsercmd;

// this is project is just to have a stand-alone parser

{$mode delphi}{$H+}

uses
  Classes, SysUtils, cparserutils, cparsertypes, ctopasconvert, cparserexp;

function GetTextFromfile(const fn: string): string;
var
  fs :TFileStream;
begin
  fs := TFileStream.Create(fn, fmOpenRead or fmShareDenyNone);
  try
    SetLength(Result, fs.Size);
    if (fs.Size>0) then
      fs.Read(Result[1], fs.Size);
  finally
    fs.Free;
  end;
end;

procedure DumpExp(exp: TExp; const prefix: string = '');
begin
  if not Assigned(exp) then Exit;
  if exp.dir = edTernary then begin
    DumpExp(exp.main,prefix+'  ');
    writeln(prefix,exp.op);
  end;
  if Assigned(exp.Left) then DumpExp(exp.left,prefix+'  ');
  if exp.dir = edValue then writeln(prefix, exp.val)
  else begin
    if exp.dir = edTernary
      then writeln(prefix, ':')
      else writeln(prefix, exp.op);
  end;
  DumpExp(exp.right,prefix+'  ');
end;

procedure DumpEnt(ent: TList);
var
  i : integer;
  e : TEntity;
  nm : TNamePart;
begin
  for i:=0 to ent.Count-1 do begin
    e := TEntity(ent[i]);
    writeln(TObject(ent[i]).Classname);
    if e is TVarFuncEntity then begin
      nm := TVarFuncEntity(e).FirstName;
      if Assigned(nm) and Assigned(nm.valexp) and Assigned(nm.valexp.exp) then
        DumpExp(nm.valexp.exp);
    end;
    //if TObject
  end;
end;

procedure Run(const fn : string);
var
  inp : TParseInput;
  txt : string;
  res : TParseOutput;
  ent : TList;
  b   : Boolean;
begin
  InitCParserInput(inp);
  ent := TList.Create;
  try
    txt := GetTextFromfile(fn);
    ResetText(inp, txt);
    b := ParseCEntities(inp, ent, res);

    writeln('result:  ', b);
    writeln('stop at: ',res.endPoint.x,':',res.endPoint.y);
    writeln('error:   ',res.error.isError);
    writeln('errmsg:  ',res.error.ErrorMsg);
    writeln('errpos:  ',res.error.ErrorPos.x,':',res.error.ErrorPos.y);

    DumpEnt(ent);
  finally
    FreeCParserInput(inp);
    ent.Free;
  end;
end;

begin
  if ParamCount=0 then begin
    writeln('please provide the c source file name');
    exit;
  end;
  try
    Run(ParamStr(1));
  except
    on e: exception do
      writeln(e.message);
  end;


end.

