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
unit ctopasconvert;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils,
  cparsertypes, TextParsingUtils, codewriter, cparserutils
  ,objcparsing, cconvlog, cparserexp;

var
  DoDebugEntities : Boolean = False; // write parsed entities names if no converter found!?

type

  { TConvertSettings }

  TConvertSettings = class
    RecordsArePacked  : Boolean;
    UseBitPacked      : Boolean;
    FuncsAreExternal  : Boolean;
    EnumsAsConst      : Boolean;
    UsedNames         : TStringList;
    CtoPasTypes       : TStringList;

    DefaultCType      : AnsiString;

    // for unkown types ONLY! (not available at CtoPasTypes);
    TypeNamePrefix    : AnsiString;
    RefTypeNamePrefix : AnsiString;
    FuncConv          : AnsiString;
    ExtLibName        : AnsiString;
    FuncDeclPostfix   : AnsiString;
    ParamPrefix       : AnsiString;

    CustomDefines     : AnsiString;

    // obj-c
    RemoveLastUnderscores : Boolean;
    PropsAsMethods        : Boolean;

    constructor Create;
    destructor Destroy; override;
    function GetUniqueName(const n: ansistring): Ansistring;
    function GetTypeName(const CTypeName: AnsiString): Ansistring;
  end;

  TErrorInfo = record
    isError   : Boolean;
    ErrorMsg  : AnsiString; // error message
    ErrorPos  : TPoint;     // position in ORIGINAL (not-macrosed) text
  end;

// endPoint contains
//  Y - line number (starting from 1),
//  X - column (starting from 1);
function ConvertCode(const t: AnsiString; var endPoint: TPoint; AllText: Boolean; var ParseError: TErrorInfo; cfg: TConvertSettings = nil): AnsiString;

// converts C-expression to Pascal expression, replace symbols with pascal equvialents.
// WARN: * the function doesn't handle macroses (treats them as identifiers)
//       * it doesn't recognizes typecasting
//       * it doesn't recognize the correct order of operations.
function PasExp(x: TExpression): AnsiString;

// returns true, if x is single number expression. V is the value of the number
function isNumberExp(x: TExpression; var v: Int64): Boolean;

// returns array limit base on x expression.
// if expression is a single number (N), then evaluates the N-1 number and returns it as string
// if expression is complex, returns pascal expression exp-1.
// i.e.   int a[10] ->       a: array [0..9] of Integer;
//        int a[10*2] ->     a: array [0..10*2-1] of Integer;
//        int a[MAXCONST] -> a: array [0..MAXCONST-1] of Integer;
function PasArrayLimit(x: TExpression): AnsiString;

type

  { TStopComment }

  TStopComment = class(TObject)
  public
    FirstComment  : boolean;
    CommentFound  : boolean;
    CommentEnd    : Integer;
    Precomp       : TEntity;
    PrecompEnd    : Integer;

    procedure OnComment(Sender: TObject; const Str: ansistring);
    procedure OnPrecompiler(Sender: TTextParser; PrecompEntity: TEntity);
    procedure Clear;
  end;


  { TMacrosMaker }

  TMacrosMaker = class(TObject)
  public
    hnd          : TCMacroHandler;
    allowRedfine : Boolean;   // default true
    ifCondProc   : Boolean;   // default false
    constructor Create(AHandler: TCMacroHandler);
    procedure Precompiler(AParser: TTextParser; PrecompEntity: TEntity);
    procedure HandleIfCond(AParser: TTextParser; IfEntity: TEntity);
  end;

type
  TConvertCheck = function (ent: TEntity): Boolean;


type
  TParseInput = record
    parser  : TTextParser;
    mmaker  : TMacrosMaker;
    //stopcmt : TStopComment;
    alltext : Boolean;
  end;

  TParseOutput = record
    endPoint : TPoint;
    error    : TErrorInfo;
  end;

  { THeaderFile }

  THeaderFile = class(TObject)
    ents      : TList; // list of lang entities
    cmts      : TList; // list of comments
    pres      : TList; // list of preprocess entities
    fn        : string;
    inclOrder : Integer;
    useCount  : Integer;
    isCore    : Boolean;
    usedBy    : Integer;
    text      : string;
    fileOfs   : TFileOffsets;
    constructor Create;
    destructor Destroy; override;
  end;

{ Initiialized the TParseInput structure
  inp - the structure that holds all the objects used for parsing (parser and Macro's handler)
  parseAll - should the entire text be parsed (only the first entity is parsed overwise) }
procedure InitCParserInput(var inp: TParseInput; parseAll: Boolean = true);

{ Frees all classes located at TParseInput structure}
procedure FreeCParserInput(var inp: TParseInput);

procedure LoadDefines(inp: TParseInput; const definesCode: string);
procedure ResetText(const inp: TParseInput; const txt: string);
function ParseCEntities(const inp: TParseInput; entList: TList; var outputInfo: TParseOutput): Boolean;
function CEntitiesToPas(const originText: string; entList: TList; cfg: TConvertSettings): AnsiString;
procedure ReleaseList(enlist: TList);

procedure AssignIntComments(SortedEnlist: TList);
procedure DebugHeaders(files: TStrings);

implementation

type
  TFuncWriterProc = procedure (wr: TCodeWriter; const FunctName, FuncRetName: AnsiString;
    const Params, ParamTypes: array of AnsiString) of object;

  TVarListItem = record
    VarName : AnsiString;
    VarType : AnsiString;
    Comment : AnsiString;
  end;

  { TVarList }

  TVarList = class(TObject)
  public
    Items       : array of TVarListItem;
    ItemsCount  : Integer;
    procedure Add(const VarName, VarType, Comment: AnsiString); overload;
    procedure Add(const Comment: AnsiString); overload;
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure WriteList(wr: TCodeWriter);
  end;

  { TCodeConvertor }

  TCodeConvertor = class(TObject)
  protected
    CmtList         : TList;
    Breaker         : TLineBreaker;
    LastOffset      : Integer;
  protected
    fWriters        : TList;
    AuxTypeCounter  : Integer;

    procedure DefFuncWrite(wr: TCodeWriter; const FuncName, FuncRetType: AnsiString;
      const Params, ParamTypes: array of AnsiString);

    function NextAuxTypeName(const Prefix: AnsiString): AnsiString;

    function GetPasTypeName(RetType: TEntity; TypePart: TNamePart): AnsiString;

    procedure DeclarePasType(TypeEntity: TEntity; const PasTypeName: AnsiString);
    procedure DeclareFuncType(const PasTypeName, RetTypeName: AnsiString; const params: array of TFuncParam);

    procedure WriteLnCommentForOffset(AOffset: Integer; NeedOffset: Boolean=True);
    function NextCommentBefore(AOffset: Integer): Integer;
    procedure WriteLnCommentsBeforeOffset(AOffset: Integer);

    procedure WriteFuncDecl(const FnName, PasRetType: AnsiString; const params : array of TFuncParam);
    procedure WriteFuncOrVar(cent: TVarFuncEntity; StartVar, WriteComment: Boolean); // todo: deprecate!
    procedure WriteTypeDef(tp: TTypeDef);
    procedure WriteEnum(en: TEnumType);
    procedure WriteEnumAsConst(en: TEnumType; FinishWithInteger: Boolean=True);
    procedure WriteUnion(st: TUnionType);
    procedure WriteStruct(st: TStructType);
    procedure WriteCommentToPas(cent: TComment; NeedLineBreak: Boolean);
    procedure WriteExp(x: TExpression);
    procedure WritePreprocessor(cent: TCPrepDefine);

    function GetPasObjCMethodName(names: TStrings): AnsiString;
    procedure WriteObjCMethod(m: TObjCMethod);
    procedure WriteObjCProperty(p: TObjCProperty);
    procedure WriteObjCMethods(list: TList);
    procedure WriteObjCInterface(cent: TObjCInterface);
    procedure WriteObjCProtocol(cent: TObjCProtocol);
    procedure WriteObjCClasses(cent: TObjCClasses);

    function CanConvert(ent: TEntity): Boolean;

    procedure PushWriter;
    procedure PopWriter;
  public
    wr        : TCodeWriter;
    cfg       : TConvertSettings;
    WriteFunc : TFuncWriterProc;
    DebugEntities : Boolean;
    constructor Create(ASettings: TConvertSettings);
    destructor Destroy; override;
    procedure WriteCtoPas(cent: TEntity; comments: TList; const ParsedText: AnsiString);
  end;

procedure TVarList.Add(const VarName,VarType,Comment:AnsiString);
begin
  if ItemsCount=length(Items) then begin
    if ItemsCount=0 then SetLength(Items, 4)
    else SetLength(Items, ItemsCount*2);
  end;
  Items[ItemsCount].VarName:=VarName;
  Items[ItemsCount].VarType:=VarType;
  Items[ItemsCount].Comment:=Comment;
  inc(ItemsCount);
end;

procedure TVarList.Add(const Comment:AnsiString);
begin
  Add('', '', Comment);
end;

constructor TVarList.Create;
begin

end;

destructor TVarList.Destroy;
begin
  inherited Destroy;
end;

procedure TVarList.Clear;
begin
  ItemsCount:=0;
end;


function MaxStrLen(const s: AnsiString; Max: Integer): Integer; inline;
begin
  if Max>length(s) then Result:=Max
  else Result:=length(s);
end;

function StrToLen(const s: AnsiString; Len: Integer; const SpChar: AnsiChar = ' '): AnsiString;
begin
  if length(s)<len then begin
    SetLength(Result, len);
    FillChar(Result[1], Len, SpChar);
    if length(s)>0 then Move(s[1], Result[1], length(s));
  end else
    Result:=s;
end;

procedure TVarList.WriteList(wr:TCodeWriter);
var
  MaxNameLen  : Integer;
  MaxTypeLen  : Integer;
  i           : Integer;
begin
  if ItemsCount=0 then Exit;

  MaxNameLen:=0;
  MaxTypeLen:=0;

  for i:=0 to ItemsCount-1 do begin
    MaxNameLen:=MaxStrLen(Items[i].VarName, MaxNameLen);
    MaxTypeLen:=MaxStrLen(Items[i].VarType, MaxTypeLen);
  end;
  inc(MaxNameLen);
  inc(MaxTypeLen, 2); // ';' + ' ' after type name

  for i:=0 to ItemsCount-1 do
    with Items[i] do
      if Comment<>'' then
        wr.Wln( StrToLen(VarName, MaxNameLen)+': '+StrToLen(VarType+';', MaxTypeLen) + ' '+Comment)
      else
        wr.Wln( StrToLen(VarName, MaxNameLen)+': '+VarType+';');
end;


{ TStopComment }

procedure TStopComment.OnComment(Sender: TObject; const Str: ansistring);
var
  parser: TTextParser;
begin
  parser := TTextParser(Sender);
  if not FirstComment then
  begin
    FirstComment := parser.Stack.Count = 0;
    CommentEnd := parser.Index;
  end;
  CommentFound := True;
end;

procedure TStopComment.OnPrecompiler(Sender: TTextParser; PrecompEntity: TEntity);
begin
  if not FirstComment and (PrecompEntity is TEntity) then
  begin
    FirstComment:=True;
    Precomp:=PrecompEntity as TEntity;
    PrecompEnd:=Sender.Index;
  end;
end;

procedure TStopComment.Clear;
begin
  FirstComment:=False;
  CommentFound:=False;
  Precomp:=nil;
  CommentEnd:=-1;
  PrecompEnd:=-1;
end;

function ParseNextEntityOrComment(AParser: TTextParser; cmt: TStopComment; var ParseError: TErrorInfo): TEntity;
var
  ent     : TEntity;
  entidx  : Integer;
begin
  Result:=nil;
  ent := ParseNextEntity(AParser);
  entidx := AParser.Index;

  if cmt.FirstComment then begin
    if Assigned(cmt.Precomp) then begin
      Result:=cmt.Precomp;
      AParser.Index:=cmt.PrecompEnd;
    end else if (AParser.Comments.Count > 0) then
    begin
      Result := TComment(AParser.Comments[0]);
      AParser.Index := cmt.CommentEnd;
    end;
  end;

  if (not Assigned(Result)) or (Assigned(ent) and (ent.Offset<Result.Offset)) then begin
    if AParser.Errors.Count>0 then begin
      ParseError.ErrorPos.X:=AParser.TokenPos;
      ParseError.ErrorMsg:=AParser.Errors[0];
      ParseError.isError:=True;
    end;
    Result:=ent;
    AParser.Index:=entidx;
  end else begin
    ent.Free;
  end;
end;

function GetRefAsterix(const AstCount: integer): ansistring;
begin
  if Astcount = 0 then
    Result := '';
  SetLength(Result, Astcount);
  FillChar(Result[1], AstCount, '*');
end;


function isNumberExp(x: TExpression; var v: Int64): Boolean;
var
  err : Integer;
begin
  Result:=Assigned(x) and (x.count=1);
  if Result then begin
    Val(x.Tokens[0].Token, v, err);
    Result:=err=0;
  end;
end;

function PasArrayLimit(x: TExpression): AnsiString;
var
  i   : Int64;
begin
  if isNumberExp(x, i) then
    Result:=IntToStr(i-1)
  else
    Result:=PasExp(x) + '-1';
end;

procedure WriteArray(arr: TNamePart; wr: TCodeWriter);
var
  i : Integer;
begin
  wr.W('array ');
  for i := 0 to length(arr.arrayexp) - 1 do wr.W('[0..' + PasArrayLimit(arr.arrayexp[i])+']');
  wr.W(' of ');
end;

constructor TMacrosMaker.Create(AHandler: TCMacroHandler);
begin
  inherited Create;
  allowRedfine:=true;
  hnd:=AHandler;
end;

procedure TMacrosMaker.Precompiler(AParser: TTextParser; PrecompEntity: TEntity);
var
  d : TCPrepDefine;
begin
  //writelN('precompiler: ', PrecompEntity.ClassName);
  if (ifCondProc) and (PrecompEntity is TCPrepIf) then begin
    HandleIfCond(AParser, PrecompEntity);
    Exit;
  end else if not (PrecompEntity is TCPrepDefine) then
    Exit;

  d:=TCPrepDefine(PrecompEntity);

  if hnd.isMacroDefined(d._Name) and not allowRedfine then Exit;

  CPrepDefineToMacrosHandler(d, hnd);
end;

procedure SkipPreproc(AParser: TTextParser);
var
  cnd : integer;
  i   : Integer;
begin
  // skipping  until the end of line
  i:=AParser.Index;
  ScanTo(AParser.Buf, i, EoLnChars);
  ScanWhile(AParser.Buf, i, EoLnChars);
  // scan until preproc, comment line or end of line
  ScanWhile(AParser.Buf, i, WhiteSpaceChars);
  if i>length(AParser.Buf) then Exit;

  if AParser.Buf[i] = '#' then begin
    // precompiler!

  end else begin
    if (AParser.Buf[i]='/') and (AParser.Buf[i+1]='/') then begin
      // skipping until the end of line
      ScanTo(AParser.Buf, i, EoLnChars);
    end else if (AParser.Buf[i]='/') and (AParser.Buf[i+1]='*') then begin
      // skip until then close of '*
    end;
  end;
end;

procedure TMacrosMaker.HandleIfCond(AParser: TTextParser; IfEntity: TEntity);
var
  op   : string;
  cond : string;
  isCondMet : Boolean;
  cnt       : integer;
begin
  writeln('if cond! ', IfEntity.ClassName);
  op:='';
  cond:='';
  if IfEntity is TCPrepIf then begin
    op := trim(TCPrepIf(IfEntity).IfOp);
    cond := trim(TCPrepIf(IfEntity)._Cond);
  end;

  if ((op='ifndef') or (op = 'ifdef')) then begin
    isCondMet := hnd.isMacroDefined(cond);
    if (op='ifndef') then isCondMet:=not isCondMet;
  end else begin
    isCondMet := false;
  end;

  writeln('if op = "',op,'"');
  writeln('cond  = "',cond,'"');
  writeln('result = ', isCondMet);
  writeln('processing macro: ', Aparser.ProcessingMacro);
  exit;

  cnt:=0;
  if not isCondMet then begin
    // let's skip! until the "end" or "else" or "elif"

    AParser.OnPrecompile:=nil; //hack: this must not be HERE!
    while AParser.Token<>'' do begin
      AParser.NextToken;
    end;

    AParser.OnPrecompile:=Self.Precompiler; //hack: this must not be HERE!
  end;

  AParser.NextToken;
end;

procedure PrepareMacros(const t: AnsiString; hnd: TCMacroHandler);
var
  p : TTextParser;
  m : TMacrosMaker;
begin
  if t='' then Exit;
  if not Assigned(hnd) then Exit;

  m := TMacrosMaker.Create(hnd);
  p:=CreateCParser(t, false);
  p.OnPrecompile:=m.Precompiler;

  while p.NextToken do ; // parse through

  p.Free;
  m.Free;
end;

function GetEmptyLinesCount(const t: AnsiString; StartOfs, EndOfs: Integer): Integer;
var
  i : Integer;
begin
  i:=StartOfs;
  if i<=0 then Exit;

  Result:=0;
  while (i<EndOfs) and (i<=length(t)) do begin
    if t[i] in [#13,#10] then begin
      inc(Result); inc(i);
      if (i<=length(t)) and (t[i] in [#13,#10]) and (t[i]<>t[i-1]) then inc(i);
    end;
    inc(i);
  end;
end;

function GetEmptyLines(const t: AnsiString; StartOfs, EndOfs: Integer): AnsiString;
var
  i : Integer;
  c : Integer;
begin
  c:=GetEmptyLinesCount(t, StartOfs, EndOfs);
  Result:='';
  for i:=1 to c do
    Result:=Result+LineEnding;
end;

procedure InitCParserInput(var inp: TParseInput; parseAll: Boolean );
var
  p   : TTextParser;
begin
  FillChar(inp, sizeof(inp), 0);

  p := CreateCParser('', true);
  p.UseCommentEntities := True;

  //inp.stopcmt:=TStopComment.Create;

  inp.mmaker := TMacrosMaker.Create(p.MacroHandler);
  inp.mmaker.ifCondProc:=true;
  inp.mmaker.allowRedfine:=false; // todo: it should be true!
  p.OnPrecompile:=inp.mmaker.Precompiler;
  p.ExpParseProc := cparserexp.ParseCExprEx;

  inp.parser:=p;
  inp.alltext:=parseAll;
end;

procedure LoadDefines(inp: TParseInput; const definesCode: string);
begin
  if not Assigned(inp.parser) or not Assigned(inp.parser.MacroHandler) or (definesCode='') then Exit;
  PrepareMacros(definesCode, inp.parser.MacroHandler);
end;

procedure ResetText(const inp: TParseInput; const txt: string);
begin
  inp.parser.Buf:=txt;
  inp.parser.Index:=1;
  inp.parser.Line:=1;
  inp.parser.MacrosDelta:=0;
  inp.parser.TokenPos:=1;
  inp.parser.Errors.Clear;
  inp.parser.Comments.Clear;
end;

function SortByOffset(p1, p2: Pointer): integer;
var
  e1, e2: TEntity;
begin
  e1:=TEntity(p1);
  e2:=TEntity(p2);
  if e1.Offset=e2.Offset then Result:=0
  else if e1.Offset<e2.Offset then Result:=-1
  else Result:=1
end;

procedure AssignIntComments(SortedEnlist: TList);
var
  i   : integer;
  ent : TEntity;
begin
  i:=0;
  while i<SortedEnlist.Count do begin
    ent:=TEntity(SortedEnlist[i]);
    inc(i);
    if not Assigned(ent) or (ent is TComment) then Continue;

    while (i<SortedEnlist.Count)
      and (TObject(SortedEnList[i]) is TComment)
      and (TComment(SortedEnList[i]).Offset >= ent.Offset)
      and (TComment(SortedEnList[i]).Offset <= ent.EndOffset) do
    begin
      if not assigned(ent.intComment) then ent.intComment:=TList.Create;
      ent.intComment.Add( TComment(SortedEnList[i]) );
      SortedEnList[i]:=nil;
      inc(i);
    end;
  end;
  SortedEnList.Pack;
end;

procedure DebugHeaders(files: TStrings);
var
  hdr : THeaderFile;
  i   : Integer;
begin
  log('hist,used,u-by, idx, name');
  for i:=0 to files.Count-1 do begin
    hdr:=THeaderFile(files.Objects[i]);
    writeln(hdr.inclOrder:4,hdr.useCount:5,hdr.usedBy:5,i:5,'  ',files[i]);
  end;
end;

function ParseCEntities(const inp: TParseInput; entList: TList;
  var outputInfo: TParseOutput): Boolean;
var
  p   : TTextParser;
  //cmt : TStopComment;
  ent : TEntity;
  //i   : Integer;
begin
  p:=inp.parser;
  //cmt:=inp.stopcmt;

  outputInfo.error.ErrorMsg:='';
  outputInfo.error.ErrorPos.X:=0;
  outputInfo.error.ErrorPos.Y:=0;
  outputInfo.error.isError:=false;

  ent:=nil;
  repeat
    try
      if not (ent is TCPPSection) then
        p.NextToken;
      ent := ParseNextEntity(p);

    except
      ent:=nil;
    end;

    if p.Errors.Count>0 then begin
      outputInfo.error.isError:=true;
      outputInfo.error.ErrorMsg:=p.Errors.Text;
      outputInfo.error.ErrorPos.x:=p.Index;
    end;

    Result:=not outputInfo.error.isError;
    if not Result then begin
      OffsetToLinePos(p.Buf, outputinfo.error.ErrorPos.X + p.MacrosDelta, outputinfo.error.ErrorPos);
      Break;
    end;

    if Assigned(ent) then entList.Add(ent);
  until (ent=nil) or not inp.AllText;

  entList.AddList( p.Comments );
  p.Comments.Clear;
  entList.Sort( SortByOffset );
end;

procedure FreeCParserInput(var inp: TParseInput);
begin
  inp.mmaker.Free;
  inp.mmaker:=nil;
  inp.parser.MacroHandler.Free;
  inp.parser.Free;
  inp.parser:=nil;
end;

function CEntitiesToPas(const originText: string;  entList: TList; cfg: TConvertSettings): AnsiString;
var
  i       : integer;
  lastsec : string;
  ent     : TEntity;
  cmtlist : TList;
  cnv     : TCodeConvertor;
  ofs     : Integer;
begin
  Result:='';

  cnv := TCodeConvertor.Create(cfg);
  cmtlist:=TList.Create;
  try
    lastsec:='';
    cnv.wr.Section:=lastsec;
    ofs:=1;
    for i:=0 to entlist.Count-1 do begin
      if not (TObject(entlist[i]) is TEntity) then Continue;
      ent:=TEntity(entlist[i]);

      //hack, based on knowledge of how enums writting works
      if (ent is TEnumType) or ((ent is TTypeDef) and (TTypeDef(ent).origintype is TEnumType)) then
      begin
        if cfg.EnumsAsConst and (cnv.wr.Section='type') then begin
          cnv.wr.DecIdent;
          cnv.wr.Section:='';
        end;
      end;

      cmtlist.Clear;

      try
        cnv.WriteCtoPas(ent, cmtlist, originText);
        lastsec:=cnv.wr.Section;
      except
        on e: Exception do Result:=Result+LineEnding+ 'error while converting C code: ' + e.Message;
      end;
      Result := Result+GetEmptyLines(originText, ofs, ent.Offset);
      ofs:=ent.Offset;
    end;

    Result:=cnv.wr.Text;
  finally
    cnv.Free;
    cmtlist.Free;
  end;

end;

procedure ReleaseList(enlist: TList);
var
  i : integer;
begin
  if not Assigned(enlist) then Exit;
  for i:=0 to enlist.Count-1 do
    TObject(enlist[i]).Free;
  enlist.Clear;
end;

function ConvertCode(const t: AnsiString; var endPoint: TPoint; AllText: Boolean; var ParseError: TErrorInfo; cfg: TConvertSettings): AnsiString;
var
  p         : TTextParser;
  ent       : TEntity;
  cnv       : TCodeConvertor;
  macros    : TCMacroHandler;
  owncfg    : Boolean;
  lastsec   : AnsiString; // last code section
  ofs       : Integer;
  cmt       : TStopComment;
  i         : Integer;
  succidx   : Integer;
  cmtlist   : TList;

begin
  FillChar(ParseError, sizeof(ParseError), 0);
  Result:='';
  ent:=nil;
  owncfg:=not Assigned(cfg);
  lastsec:='';
  if owncfg then cfg := TConvertSettings.Create;
  try
    macros:=TCMacroHandler.Create;

    if cfg.CustomDefines<>'' then
      PrepareMacros(cfg.CustomDefines, macros);

    cmt := TStopComment.Create;
    p := CreateCParser(t);
    p.MacroHandler:=macros;
    p.UseCommentEntities := True;
    p.OnComment:=cmt.OnComment;
    p.OnPrecompile:=cmt.OnPrecompiler;
    cmtlist:=TList.Create;

    try
      repeat
        try
          ofs := p.Index;
          p.NextToken;
          ent := ParseNextEntityOrComment(p, cmt, ParseError);
        except
          ent:=nil;
        end;

        if ParseError.isError then
          Break
        else
          succidx:=p.Index + p.MacrosDelta;

        if Assigned(ent) then begin
          cnv := TCodeConvertor.Create(cfg);
          try
            cnv.wr.Section:=lastsec;
            if lastsec<>'' then cnv.wr.IncIdent;

            //hack, based on knowledge of how enums writting works
            if (ent is TEnumType) or ((ent is TTypeDef) and (TTypeDef(ent).origintype is TEnumType)) then
            begin
              if cfg.EnumsAsConst and (cnv.wr.Section='type') then begin
                cnv.wr.DecIdent;
                cnv.wr.Section:='';
              end;
            end;

            cmtlist.Clear;
            for i:=0 to p.Comments.Count-1 do
              if TObject(p.Comments[i]) is TComment then cmtlist.Add(TObject(p.Comments[i]));
            cnv.WriteCtoPas(ent, cmtlist, p.Buf);

            lastsec:=cnv.wr.Section;
          except
            on e: Exception do Result:=Result+LineEnding+ 'error while converting C code: ' + e.Message;
          end;
          Result := Result+GetEmptyLines(p.Buf, ofs, ent.Offset)+cnv.wr.Text;
          cnv.Free;
        end;

        if Assigned(ent) and (p.Comments.IndexOf(ent)<0) then ent.Free;
        for i:=0 to p.Comments.Count-1 do TComment(p.Comments[i]).Free;
        p.Comments.Clear;
        cmt.Clear;
      until (ent=nil) or not AllText;


      OffsetToLinePos(t, succidx, endPoint);

      if ParseError.isError then
        OffsetToLinePos(t, ParseError.ErrorPos.X + p.MacrosDelta, ParseError.ErrorPos);

    finally
      p.Free;
      macros.Free;
      cmt.Free;
      cmtlist.Free;
    end;
  except
    on e: Exception do Result:=Result+LineEnding+' internal error: '+ e.Message;
  end;
  if owncfg then cfg.Free;
end;

{ TCodeConvertor }

constructor TCodeConvertor.Create(ASettings:TConvertSettings);
begin
  cfg:=ASettings;
  wr:=TCodeWriter.Create;
  WriteFunc:=DefFuncWrite;
  DebugEntities := DoDebugEntities;
end;

destructor TCodeConvertor.Destroy;
var
  i : Integer;
begin
  if Assigned(fWriters) then begin
    for i:=0 to fWriters.Count-1 do TObject(fWriters[i]).Free;
    fWriters.Free;
  end;
  wr.Free;
  inherited Destroy;
end;

procedure TCodeConvertor.WriteCommentToPas(cent: TComment; NeedLineBreak: Boolean);
var
  u: ansistring;
begin
  u := cent._Comment;
  if cent.CommenType = ctBlock then
  begin
    u := StringReplace(u, '*)', '* )', [rfReplaceAll]);
    wr.W('(*' + u + ' *)');
  end
  else
  begin
    wr.W('//' + u);
  end;
  if NeedLineBreak then wr.Wln;
end;

procedure TCodeConvertor.WriteExp(x:TExpression);
begin
  wr.W(PasExp(x));
end;

function CtoPasSymbol(const t: AnsiString): AnsiString;
begin
  if (t='>>') then Result:='shr'
  else if (t='<<') then Result:='shl'
  else if (t='%') then Result:='mod'
  else if (t='|') or (t='||') then Result:='or'
  else if (t='&') or (t='&&') then Result:='and'
  else if (t='^') then Result:='xor'
  else if (t='!') or (t='~') then Result:='not'
  else if (t='!=') then Result:='<>'
  else Result:=t;
end;

function CtoPasString(const t: AnsiString; cfg: TConvertSettings): AnsiString;
begin
  Result:=#39+Copy(t, 2, length(t)-2)+#39;
end;

procedure TCodeConvertor.WritePreprocessor(cent:TCPrepDefine);
var
  p   : TTextParser;
  s   : AnsiString;
begin
  if cent.SubsText<>'' then begin
    SetPasSection(wr, 'const');
    p:=CreateCParser(cent.SubsText, false);
    s:='';
    while p.NextToken do begin
      case p.TokenType of
        tt_String:  s:=s+' '+CtoPasString(p.Token, cfg);
        tt_Symbol:  s:=s+' '+CtoPasSymbol(p.Token);
      else
        s:=s+' '+p.Token;
      end;
    end;
    p.Free;
    wr.W(cfg.GetUniqueName(cent._Name) + ' =' + s+';');

    WriteLnCommentForOffset(cent.Offset);
  end;
end;

function TCodeConvertor.GetPasObjCMethodName(names:TStrings):AnsiString;
var
  i : Integer;
begin
  Result:='';
  for i:=0 to names.Count-1 do Result:=Result+names[i];
  for i:=1 to length(Result) do if Result[i]=':' then Result[i]:='_';
  if cfg.RemoveLastUnderscores then begin
    i:=length(Result);
    while (i>0) and (Result[i]='_') do dec(i);
    Result:=Copy(Result, 1, i);
  end;
end;

procedure TCodeConvertor.WriteObjCMethod(m: TObjCMethod);
var
  ret     : AnsiString;
  i       : Integer;
  PNames  : array of AnsiString;
  PTypes  : array of AnsiString;
begin
  if not CanConvert(m) then Exit;


  if m.RetType=nil then ret:='id' else ret:=GetPasTypeName(m.RetType, m.RetName);
  SetLength(PNames, length(m.Args));
  SetLength(PTypes, length(m.Args));
  if length(m.Args)>0 then
    for i:=0 to length(m.Args)-1 do begin
      if m.Args[i].Name=''
        then PNames[i]:=cfg.ParamPrefix+IntToStr(i)
        else PNames[i]:=m.Args[i].Name;
      if not Assigned(m.Args[i].RetType) then
        PTypes[i]:='id'
      else
        PTypes[i]:=GetPasTypeName(m.Args[i].RetType, m.Args[i].TypeName);
    end;

  DefFuncWrite(wr, GetPasObjCMethodName(m.Name), ret, PNames, PTypes);
  wr.W(';');
  wr.W(' message ''');
  for i:=0 to m.Name.Count-1 do wr.W(m.Name[i]);
  wr.W(''';');
end;

procedure TCodeConvertor.WriteObjCProperty(p:TObjCProperty);
var
  tp  : AnsiString;
  mtd : AnsiString;
  nmp : TNamePart;
  nm  : AnsiString;
begin
  //if not Assigned(p.Name) or (p.Name.Kind<>nk_Ident) then Exit;
  nmp:=GetIdPart(p.Name);
  if not Assigned(nmp) or (nmp.Id='') then Exit;
  tp:=GetPasTypeName(p.RetType, nmp.owner);
  if tp='' then Exit;

  nm:=nmp.Id;
  if (cfg.PropsAsMethods) then begin

    if p.GetterName='' then mtd:=nmp.Id else mtd:=p.GetterName;
    wr.W('function '+nm+': '+tp+'; message '''+mtd+''';');

    if p.Props.IndexOf('readonly')<0 then begin
      wr.Wln;

      nm:='set'+UpperCase(nm[1])+Copy(nm, 2, length(nm)-1);
      if p.SetterName='' then mtd:=nm+':' else mtd:=p.SetterName;

      wr.W('procedure '+nm+'(AValue: '+tp+'); message '''+mtd+''';');
    end;
  end;
end;

procedure TCodeConvertor.WriteObjCMethods(list:TList);
var
  ent : TEntity;
  i   : Integer;
begin
  if not Assigned(list) or (list.Count=0) then Exit;
  for i:=0 to list.Count-1 do begin
    ent:=TEntity(list[i]);
    if not Assigned(ent) then Continue;

    WriteLnCommentsBeforeOffset(ent.Offset);
    if ent is TObjCMethod then
      WriteObjCMethod(TObjCMethod(ent))
    else if ent is TObjCProperty then begin
      WriteObjCProperty(TObjCProperty(ent));
    end;

    WriteLnCommentForOffset(ent.Offset);
  end;

end;

procedure TCodeConvertor.WriteObjCInterface(cent:TObjCInterface);
var
  i     : Integer;
  sc    : TObjCScope;
  ivar  : TObjCInstVar;
  sect  : AnsiString;
const
  sectname : array [TObjCScope] of AnsiString = ('private', 'protected', 'public', 'protected');
begin
  SetPasSection(wr, 'type');
  if cent.isCategory then begin
    wr.W(cent.Name + ' = objccategory external ');
    if cent.SuperClass<>'' then wr.W('('+cent.SuperClass+')');
    wr.Wln;
  end else begin
    wr.W(cent.Name + ' = objcclass external ');
    if cent.SuperClass<>'' then wr.W('('+cent.SuperClass);
    if cent.Protocols.Count>0 then begin
      if cent.SuperClass='' then wr.W('(id, ')
      else wr.W(', ');
      for i:=0 to cent.Protocols.Count-2 do wr.W(cent.Protocols[i]+'Protocol, ');
      wr.W(cent.Protocols[cent.Protocols.Count-1]+'Protocol');
    end;
    if (cent.SuperClass<>'') or (cent.Protocols.Count>0) then wr.Wln(')')
    else wr.Wln;

    sect:='';
    sc:=os_Public;
    for i:=0 to cent.Vars.Count-1 do begin
      ivar:=TObjCInstVar(cent.Vars[i]);
      if (sect='') or (ivar.scope<>sc) then begin
        if sect<>'' then wr.DecIdent;
        sc:=ivar.scope;
        sect:=sectname[sc];
        wr.Wln(sect);
        wr.IncIdent;
      end;
      WriteLnCommentsBeforeOffset(ivar.v.RetType.Offset);
      WriteFuncOrVar(ivar.v, false, true);
    end;
    if sect<>'' then wr.DecIdent;
  end;

  if cent.Methods.Count>0 then begin
    wr.Wln('public');
    wr.IncIdent;
    WriteObjCMethods(cent.Methods);
    wr.DecIdent;
  end;
  wr.Wln('end;');
end;

procedure TCodeConvertor.WriteObjCProtocol(cent:TObjCProtocol);
var
  i : Integer;
begin
  SetPasSection(wr, 'type');

  if cent.isForward then begin
    for i:=0 to cent.Names.Count-1 do
      wr.Wln(cent.Names[i]+'Protocol = objcprotocol; external name '''+cent.Names[i]+''';');
  end else begin
    wr.W(cent.Names[0]+'Protocol = objcprotocol external');

    if cent.Protocols.Count>0 then begin
      wr.W('(');
      for i:=0 to cent.Protocols.Count-2 do wr.W(cent.Protocols[i]+'Protocol, ');
      wr.WLn(cent.Protocols[cent.Protocols.Count-1]+'Protocol)');
    end else
      wr.WLn;

    if cent.Methods.Count>0 then begin
      wr.IncIdent;
      WriteObjCMethods(cent.Methods);
      wr.DecIdent;
    end;
    wr.W('end; ');
    wr.Wln(' name '''+cent.Names[0]+''';');
  end;
end;

procedure TCodeConvertor.WriteObjCClasses(cent:TObjCClasses);
var
  i : Integer;
begin
  SetPasSection(wr, 'type');
  for i:=0 to cent.ClassList.Count-1 do
    wr.WLn(cent.ClassList[i] +' = objcclass external;');
end;


function CanConvertObjCMethod(ent: TObjCMethod): Boolean;
var
  i : Integer;
begin
  Result:=True;
  if not Assigned(ent) then Exit;
  for i:=0 to length(ent.Args)-1 do
    if Assigned(ent.Args[i].TypeName) and isAnyBlock(ent.Args[i].TypeName) then begin
      Result:=False;
      Exit;
    end;
end;

function TCodeConvertor.CanConvert(ent: TEntity): Boolean;
begin
  Result:=Assigned(ent);
  if not Result then Exit;


  if ent is TVarFuncEntity then
  begin
    Result:=(not isAnyBlock(TVarFuncEntity(ent).FirstName)) and CanConvert(TVarFuncEntity(ent).RetType)
  end else if ent is TObjCMethod then
    Result:=CanConvertObjCMethod(TObjCMethod(ent));

end;

procedure TCodeConvertor.PushWriter;
begin
  if not Assigned(fWriters) then fWriters:=TList.Create;
  fWriters.Add(wr);
  wr:=TCodeWriter.Create;
end;

procedure TCodeConvertor.PopWriter;
var
  t : TCodeWriter;
  s4 : AnsiString;
  s5 : AnsiString;
  i : Integer;
begin
  if not Assigned(fWriters) or (fWriters.Count=0) then Exit;
  t:=wr;
  i:=fWriters.Count-1;
  if i<0 then wr:=nil else wr:=TCodeWriter(fWriters[i]);

  fWriters.Delete(i);
  if t.Text<>'' then begin
    // HACK: Push/Pop writing takes place for new type declarations only
    //  if there're multiple pop/push operations, the resulting code might look like:
    //  type
    //    A1 = something
    //  type
    //    A2 = something
    //  It's possible to merge them into:
    //  type
    //    A1 = something
    //    A2 = something
    s4:=Copy(t.Text, 1, 4);
    s5:=Copy(t.text, 1, 5);
    if Assigned(wr) then begin
      if (s4='type') and (Copy(wr.Text, 1, 4)=s4) then
        wr.Text:=t.Text+Copy(wr.Text, 4+sizeof(LineEnding)+1, length(wr.Text))
      else if (s5='const') and (Copy(wr.Text, 1, 5)=s5) then
        wr.Text:=t.Text+Copy(wr.Text, 5+sizeof(LineEnding)+1, length(wr.Text))
      else
        wr.Text:=t.Text+wr.Text;
    end;
  end;
  t.Free;
end;

procedure TCodeConvertor.DeclareFuncType(const PasTypeName, RetTypeName: AnsiString; const params: array of TFuncParam);
begin
  SetPasSection(wr, 'type');
  wr.W(PasTypeName + ' = ');
  WriteFuncDecl('', RetTypeName, params);
end;

procedure TCodeConvertor.WriteLnCommentForOffset(AOffset:Integer; NeedOffset: Boolean);
var
  cmt : TComment;
  ln  : Integer;
  c   : Integer;
  i   : Integer;
begin
  ln:= Breaker.LineNumber(AOffset);
  c:=0;
  for i:=0 to CmtList.Count-1 do begin
    cmt:=TComment(CmtList[i]);
    if Breaker.LineNumber(TComment(CmtList[i]).Offset)=ln then begin
      if NeedOffset then begin
        wr.W('  ');
        NeedOffset:=False;
      end;
      WriteCommentToPas(cmt, false);
      if cmt.Offset>LastOffset then LastOffset:=cmt.Offset;
      inc(c);
    end;
  end;
  wr.Wln;
end;

function TCodeConvertor.NextCommentBefore(AOffset:Integer):Integer;
var
  i : Integer;
  c : TComment;
begin
  Result:=-1;
  for i:=0 to CmtList.Count-1 do begin
    c:=TComment(CmtList[i]);
    if (c.Offset>LastOffset) and (c.Offset<AOffset) then begin
      Result:=c.Offset;
      Exit;
    end else if c.Offset>AOffset then
      Exit;
  end;
end;

procedure TCodeConvertor.WriteLnCommentsBeforeOffset(AOffset:Integer);
var
  i : Integer;
begin
  i:=NextCommentBefore(AOffset);
  while i>=0 do begin
    WriteLnCommentForOffset(i, False);
    i:=NextCommentBefore(AOffset);
  end;
end;

// returns the name for simple types, or empty structs:
//   struct num n; - returns 'num' (name of the struct),
// but
//   struct num {int f;} n; returns '', because struct is NOT simple named type
function GetSimpleName(ent: TEntity): AnsiString;
begin
  if ent is TSimpleType then
    Result:=TSimpleType(ent).Name
  else if (ent is TStructType) and ( length(TStructType(ent).fields)=0) then
    Result:=TStructType(ent).Name
  else if (ent is TEnumType) and (length(TEnumType(ent).items)=0) then
    Result:=TEnumType(ent).Name
  else
    Result:='';
end;

// returns the declared typename
// for
//   struct num n;
//   struct num {int f;} n;
// returns 'num' (name of the struct),
function GetComplexTypeName(ent: TEntity): AnsiString;
begin
  if ent is TStructType then
    Result:=TStructType(ent).Name
  else if ent is TUnionType then
    Result:=TUnionType(ent).Name
  else if ent is TEnumType then
    Result:=TEnumType(ent).Name
  else
    Result:='';
end;

function TCodeConvertor.GetPasTypeName(RetType: TEntity; TypePart: TNamePart): AnsiString;
var
  CtypeName : AnsiString;
  pasRef    : AnsiString;
  pasType   : AnsiString;
  rt        : AnsiString;
  i         : Integer;
begin
  if isNamePartPtrToFunc(TypePart) then begin
    PushWriter;
    rt := GetPasTypeName(RetType, TypePart.owner.owner);
    PopWriter;

    Result:=NextAuxTypeName('TAuxCallback');
    DeclareFuncType(Result, rt, TypePart.owner.params);
    wr.Wln(';');

  end else begin

    CtypeName:=GetSimpleName(RetType);
    if CtypeName<>'' then begin
      pasRef:=cfg.RefTypeNamePrefix+cfg.GetTypeName(CtypeName);
    end else begin
      CtypeName:=GetComplexTypeName(RetType);
      if CTypeName='' then CtypeName:=NextAuxTypeName('TAuxType');
      DeclarePasType(RetType, CtypeName);
      cfg.CtoPasTypes.Values[CtypeName]:=CTypeName;
      pasRef:=cfg.RefTypeNamePrefix+Copy(CtypeName, 2, length(CTypeName));
      wr.Wln(';');
    end;

    if Assigned(TypePart) and (TypePart.Kind=nk_Ref) then begin
      pasType:=cfg.GetTypeName(CtypeName);
      for i:=1 to TypePart.RefCount do begin
        CTypeName:=CTypeName+'*';
        rt:=cfg.CtoPasTypes.Values[CTypeName];
        if rt='' then begin
          PushWriter;
          SetPasSection(wr, 'type');
          wr.Wln(pasRef+' = ^'+pasType+';');
          pasType:=pasRef;
          PopWriter;

          // filling required reference type
          cfg.CtoPasTypes.Values[CTypeName]:=pasType;

        end else
          pasType:=rt;
        pasRef:=cfg.RefTypeNamePrefix+pasType;
      end;
      Result:=pasType;
    end else begin
      Result:=cfg.GetTypeName(CtypeName);
    end;
  end;
end;

function isVoidParams(const params : array of TFuncParam): Boolean;
begin
  Result:=length(params)=0;
  if Result then Exit;
  Result:=length(params)=1;
  if Result then
    Result:=(params[0].prmtype is TSimpleType) and
            (TSimpleType(params[0].prmtype).Name='void') and
            (params[0].name=nil);
end;

procedure TCodeConvertor.WriteFuncDecl(const FnName, PasRetType: AnsiString; const params : array of TFuncParam);
var
  i        : Integer;
  ptypes   : array of String;
  pnames   : array of String;
  tp       : TNamePart;
begin
  PushWriter;
  if not isVoidParams(params) then begin
    SetLength(ptypes, length(params));
    SetLength(pnames, length(params));
    for i:=0 to length(params)-1 do begin
      tp:=params[i].name;
      if Assigned(tp) then begin
        while Assigned(tp.child) do tp:=tp.child;
        if tp.Kind=nk_Ident then begin
          pnames[i]:=cfg.GetUniqueName(tp.Id);
          tp:=tp.owner;
        end;
      end;
      if pnames[i]='' then pnames[i] := cfg.ParamPrefix+IntToStr(i);
      ptypes[i]:=GetPasTypeName(params[i].prmtype, tp);
    end;
  end else begin
    ptypes:=nil;
    pnames:=nil;
  end;
  PopWriter;

  wr.CheckLineLen:=True;
  WriteFunc(wr, FnName, PasRetType, pnames, ptypes);
  wr.CheckLineLen:=False;

  if cfg.FuncConv<>'' then wr.W('; '+cfg.FuncConv);
  if cfg.FuncDeclPostfix<>'' then wr.W('; '+cfg.FuncDeclPostfix);
end;

function isDeclExternal(cfg: TConvertSettings; DeclType: TEntity; isFunc: Boolean): Boolean;
begin
  Result:=(isfunc and cfg.FuncsAreExternal) or
          (Assigned(DeclType) and (DeclType.Specifiers.IndexOf('extern')>=0));
end;

procedure TCodeConvertor.WriteFuncOrVar(cent: TVarFuncEntity; StartVar, WriteComment: Boolean);
var
  i, j  : integer;
  Name  : TNamePart;
  n     : TNamePart;
  id    : AnsiString;
  ref   : TNamePart;
  rt    : AnsiString;
  isfunc  : Boolean;
begin
  for j := 0 to cent.Names.Count - 1 do
  begin
    Name:=GetIdPart(TNamePart(cent.Names[j]));
    if not Assigned(name) then begin
      wr.Wln(' bad declaration synax!');
      Exit;
    end;
    isfunc:=False;
    id:=cfg.GetUniqueName(name.Id);
    n:=name.owner;
    if not Assigned(n) then begin
      PushWriter;
      rt:=GetPasTypeName(cent.RetType, Name);
      PopWriter;
      if StartVar then SetPasSection(wr, 'var');
      wr.W(id + ' : ' + rt);
    end else if (n.Kind=nk_Func) then begin
      SetPasSection(wr, '');
      rt:=GetPasTypeName(cent.RetType, n.owner);
      WriteFuncDecl(id, rt, n.params);
      isfunc:=True;
    end else if (n.Kind=nk_Ref) then begin
      if StartVar then SetPasSection(wr, 'var');
      wr.W(id + ' : ');
      ref:=n;
      n:=n.owner;
      if not Assigned(n) then begin
        wr.W( GetPasTypeName(cent.RetType, ref) );
      end else
        case n.Kind of
          nk_Array: begin
            for i:=1 to ref.RefCount do wr.W('^');
            WriteArray(n, wr);
            wr.W(GetPasTypeName(cent.RetType, n.owner))
          end;
          nk_Func: begin
            PushWriter;
            rt:=GetPasTypeName(cent.RetType, n.owner);
            PopWriter;
            WriteFuncDecl('', rt, n.params);
          end;
        end;

    end else if (n.Kind=nk_Array) then begin
      if StartVar then SetPasSection(wr, 'var');
      wr.W(id + ' : ');
      WriteArray(n, wr);
      wr.W(GetPasTypeName(cent.RetType, n.owner));
    end;
    wr.W(';');
    if isDeclExternal(cfg, cent.RetType, isfunc) then begin
      wr.W(' external');
      if isfunc and (cfg.ExtLibName<>'') then wr.W(' '+cfg.ExtLibName);
      wr.W(';');
    end;
    if WriteComment then WriteLnCommentForOffset(cent.Offset);
  end;
end;

procedure TCodeConvertor.WriteCtoPas(cent: TEntity; comments: TList; const ParsedText: AnsiString);
var
  tp  : AnsiString;
begin
  if not CanConvert(cent) then Exit;

  CmtList:=comments;
  Breaker:=TLineBreaker.Create;
  Breaker.SetText(ParsedText);

  if cent is TVarFuncEntity then begin
    WriteFuncOrVar(cent as TVarFuncEntity, True, True)
  end else if cent is TTypeDef then
    WriteTypeDef(cent as TTypeDef)
  else if (cent is TStructType) or (cent is TUnionType) then begin
    DeclarePasType(cent, GetComplexTypeName(cent));
    wr.Wln(';');
  end else if (cent is TEnumType) then begin
    tp:=GetComplexTypeName(cent);
    if cfg.EnumsAsConst and (tp='') then
      WriteEnumAsConst(TEnumType(cent), false)
    else begin
      DeclarePasType(TEnumType(cent), GetComplexTypeName(cent));
      wr.Wln(';');
    end;
  end else if cent is TComment then
    WriteCommentToPas(cent as TComment, True)
  else if cent is TCPrepDefine then
    WritePreprocessor(cent as TCPrepDefine)
  else if cent is TObjCInterface then
    WriteObjCInterface(cent as TObjCInterface)
  else if cent is TObjCProtocol then
    WriteObjCProtocol(cent as TObjCProtocol)
  else if cent is TObjCClasses then
    WriteObjCClasses(cent as TObjCClasses)
  else begin
    if DebugEntities then
      wr.Wln(cent.ClassName);
  end;

  Breaker.Free;
end;

// typedef allows to declare multiple type alias names, with the same base type.   //
// i.e.:                                                                           //
// typedef struct mystruct_{int a,b;}                                              //
//                  t[10], *t_ptr, (*f)(int i);                                    //
//                                                                                 //
// typedef writting alogrithm:                                                     //
// 1. find the base type name.                                                     //
// 2. if no name is found, generate AuxType name                                   //
// 3. declare all types using the base name (treating the base name as simpletype) //
//                                                                                 //
// found simple declaration, that can be used as base name:                        //
// typedef struct {int a;} t, *t_ptr;  - t is base name                            //
procedure TCodeConvertor.WriteTypeDef(tp: TTypeDef);
var
  nm        : TNamePart;
  n         : TNamePart;
  fn        : TNamePart;
  rt        : AnsiString;
  tpart     : TNamePart;
  i         : Integer;

  name        : AnsiString;
  basetype    : TSimpleType;
  basetypeown : Boolean;

begin
  if tp.names.Count=0 then Exit; // no names specified!

  // 1. selecting base name (and TSimpleType) type
  if tp.origintype is TSimpleType then begin
    basetype:=TSimpleType(tp.origintype);
    basetypeown:=False;
  end else begin
    name:=GetComplexTypeName(tp.origintype);
    if name='' then
      for i:=0 to tp.names.Count-1 do begin
        if TNamePart(tp.names[i]).Kind=nk_Ident then begin
          name:=TNamePart(tp.names[i]).Id;
          Break;
        end;
      end;
    // 2. no suitable name found in typedef, generating auxtype
    if name='' then begin
      PushWriter;
      name:=GetPasTypeName(tp.origintype, nil);
      PopWriter;
    end else begin
      DeclarePasType(tp.origintype, name);
      wr.Wln(';');
    end;
    basetype:=TSimpleType.Create;
    basetype.Name:=name;
    basetypeown:=True;
  end;

  // 3. writting down all types
  for i:=0 to tp.names.Count-1 do begin
    nm:=GetIdPart(TNamePart(tp.names[i]));
    if not Assigned(nm) then Exit;
    SetPasSection(wr,'type');

    n:=nm.owner;

    if not Assigned(n) then begin
      if nm.Id<>basetype.Name then wr.W(nm.Id+' = '+GetPasTypeName( basetype, nil))
      else Continue;
    end else begin
      fn:=n.owner;
      if n.Kind=nk_Array then begin
        PushWriter;
        name:=GetPasTypeName(basetype, n.owner);
        PopWriter;
        wr.W(nm.Id+' = ');
        WriteArray(n, wr);
        wr.W(name);;
      //typedef functions and typedef function pointers are converted the same way.
      end else if (n.Kind=nk_Ref) and (not Assigned(fn) or (fn.Kind<>nk_Func)) then begin
        PushWriter;
        name:=GetPasTypeName(basetype, n);
        PopWriter;
        wr.W(nm.Id+' = '+name);
        fn:=n.owner;
      end else if isNamePartPtrToFunc(n) or (Assigned(n) and (n.kind=nk_Func) ) then begin

        if isNamePartPtrToFunc(n) then begin
          tpart:=n.owner.owner // rettype of function pointer
        end else begin
          tpart:=n.owner;
          cfg.CtoPasTypes.Values[nm.id+'*']:=nm.id;
        end;

        PushWriter;
        rt := GetPasTypeName(basetype, tpart);
        PopWriter;

        if n.Kind=nk_Func then fn:=n;
        DeclareFuncType(nm.id, rt, fn.params);
      end;
    end;
    wr.Wln(';');
  end;
  if basetypeown then basetype.Free;
end;

procedure TCodeConvertor.WriteStruct(st:TStructType);
var
  i       : Integer;
  anybit  : Boolean;

  x       : TExpression;
  bval    : Int64;
  xp      : AnsiString;
begin
  anybit:=False;
  for i:=0 to length(st.fields)-1 do
    if st.fields[i].isbitted then begin
      anybit:=True;
      Break;
    end;

  if cfg.UseBitPacked and anybit then
    wr.W('bitpacked ')
  else if cfg.RecordsArePacked then
    wr.W('packed ');

  wr.Wln('record');
  wr.IncIdent;

  for i:=0 to length(st.fields)-1 do begin
    WriteLnCommentsBeforeOffset(st.fields[i].v.Offset);

    if cfg.UseBitPacked and st.fields[i].isbitted then begin
      x:=st.fields[i].bits;
      if isNumberExp(x, bval) then begin
        bval:=(1 shl bval) - 1;
        xp:=IntToStr(bval);
      end else
        xp:='((1 shl ('+PasExp(x)+'))-1)';
      // returns true, if x is single number expression. V is the value of the number
      wr.W( GetIdFromPart(st.fields[i].v.FirstName) + ' : 0..'+xp+';');
      WriteLnCommentForOffset(st.fields[i].v.Offset);
    end else
      WriteFuncOrVar(st.fields[i].v, False, True);
  end;
  wr.DecIdent;
  wr.W('end');
end;

procedure TCodeConvertor.WriteEnum(en:TEnumType);
var
  b : Boolean;
  i : Integer;
begin
  if cfg.EnumsAsConst then
    WriteEnumAsConst(en)
  else begin
    WriteLnCommentsBeforeOffset(en.Offset);
    wr.W('(');
    wr.IncIdent;
    b:=wr.CheckLineLen;
    wr.CheckLineLen:=True;
    for i:=0 to length(en.items)-2 do begin
      WriteLnCommentsBeforeOffset(en.Items[i].Offset);
      wr.W(en.items[i].Name);
      if Assigned(en.items[i].Value) then begin
        wr.W(' = ');
        WriteExp(en.items[i].Value);
      end;
      wr.W(',');
      WriteLnCommentForOffset(en.Items[i].Offset);
    end;
    i:=length(en.items)-1;
    WriteLnCommentsBeforeOffset(en.Items[i].Offset);
    wr.W(en.items[i].Name);
    if Assigned(en.items[i].Value) then begin
      wr.Wln(' = ');
      WriteExp(en.Items[i].Value);
    end else
      wr.Wln;
    WriteLnCommentForOffset(en.Items[i].Offset);
    wr.DecIdent;
    wr.W(')');
    wr.CheckLineLen:=b;
  end;
end;

procedure TCodeConvertor.WriteEnumAsConst(en:TEnumType; FinishWithInteger: Boolean);
var
  i       : Integer;
  v       : Int64;
  last    : AnsiString;
  useval  : Boolean;
begin
  if length(en.items)>0 then begin
    PushWriter;
    WriteLnCommentsBeforeOffset(en.Offset);
    SetPasSection(wr, 'const');
    v:=0;
    last:='';
    useval:=True;

    for i:=0 to length(en.items)-1 do begin
      WriteLnCommentsBeforeOffset(en.items[i].Offset);

      wr.W(en.items[i].Name + ' = ');
      if Assigned(en.items[i].Value) then begin
        WriteExp(en.items[i].Value);
        useval:=isNumberExp(en.items[i].Value, v);
      end else begin
        if useval
        then wr.W(IntToStr(v))
        else wr.W(last+' + 1');
      end;
      wr.W(';');
      WriteLnCommentForOffset(en.items[i].Offset);
      inc(v);
      last:=en.Items[i].Name;
    end;

    PopWriter;
  end;
  if FinishWithInteger then wr.W('Integer');
end;

procedure TCodeConvertor.WriteUnion(st:TUnionType);
var
  i : Integer;
begin
  if cfg.RecordsArePacked then wr.W('packed ');
  wr.WLn('record');
  wr.Wln('case Integer of');
  wr.IncIdent;
  for i:=0 to length(st.fields)-1 do begin
    WriteLnCommentsBeforeOffset(st.fields[i].v.Offset);
    wr.w(IntToStr(i)+':(');
    WriteFuncOrVar(st.fields[i].v, False, False);
    wr.W(');');
    WriteLnCommentForOffset(st.fields[i].v.Offset);
  end;
  wr.DecIdent;
  wr.w('end');
end;

function TCodeConvertor.NextAuxTypeName(const Prefix:AnsiString):AnsiString;
begin
  if Prefix='' then Result:='AuxType'+IntToStr(AuxTypeCounter)
  else Result:=Prefix+IntToStr(AuxTypeCounter);
  inc(AuxTypeCounter);
end;

procedure TCodeConvertor.DeclarePasType(TypeEntity: TEntity; const PasTypeName: AnsiString);
begin
  SetPasSection(wr, 'type');
  wr.W(PasTypeName + ' = ');
  if TypeEntity is TStructType then
    WriteStruct(TStructType(TypeEntity))
  else if TypeEntity is TEnumType then begin
    WriteEnum(TEnumType(TypeEntity))
  end else if TypeEntity is TUnionType then begin
    WriteUnion(TUnionType(TypeEntity))
  end else if TypeEntity is TSimpleType then
    wr.W( cfg.GetTypeName(TSimpleType(TypeEntity).Name))
  else begin
    {SetPasSection(wr, 'type');
    wr.W(PasTypeName + ' = ');}
    wr.W('todo: '+TypeEntity.ClassName);
  end;
  //todo: ...parse any Entity
end;

procedure TCodeConvertor.DefFuncWrite(wr:TCodeWriter;const FuncName,FuncRetType:AnsiString;
  const Params,ParamTypes: array of AnsiString);
var
  isProc  : Boolean;
  tp      : AnsiString;
  p       : AnsiString;
  i       : Integer;

const
  FnKind : array [Boolean] of AnsiString = ('procedure','function');
begin
  isProc:=FuncRetType<>'';

  wr.W ( FnKind[isProc]  );
  if FuncName<>'' then wr.W(' '+FuncName);
  if length(Params)>0 then begin
    tp:=ParamTypes[0];
    p:='';
    wr.W('(');
    for i:=0 to length(Params)-1 do begin
      if ParamTypes[i]=tp then begin
        if p='' then p:=Params[i] else p:=p+', '+Params[i];
      end else begin
        wr.W(p+': '+tp+'; ');
        p:=Params[i];
        tp:=ParamTypes[i];
      end;
    end;
    wr.W(p+': '+tp+')');
  end;
  if FuncRetType<>'' then wr.W(': '+FuncRetType);
end;


{ TConvertSettings }

procedure FillPasReserved(st: TStrings);
begin
  with st do
  begin
    // turbo pascal reserved
    Add('absolute');
    Add('and');
    Add('array');
    Add('asm');
    Add('begin');
    Add('case');
    Add('const');
    Add('constructor');
    Add('destructor');
    Add('div');
    Add('do');
    Add('downto');
    Add('else');
    Add('end');
    Add('file');
    Add('for');
    Add('function');
    Add('goto');
    Add('if');
    Add('implementation');
    Add('in');
    Add('inherited');
    Add('inline');
    Add('interface');
    Add('label');
    Add('mod');
    Add('nil');
    Add('not');
    Add('object');
    Add('of');
    Add('on');
    Add('operator');
    Add('or');
    Add('packed');
    Add('procedure');
    Add('program');
    Add('record');
    Add('reintroduce');
    Add('repeat');
    Add('self');
    Add('set');
    Add('shl');
    Add('shr');
    Add('string');
    Add('then');
    Add('to');
    Add('type');
    Add('unit');
    Add('until');
    Add('uses');
    Add('var');
    Add('while');
    Add('with');
    Add('xor');
    // object pascal reserved
    Add('as');
    Add('class');
    Add('dispinterface');
    Add('except');
    Add('exports');
    Add('finalization');
    Add('finally');
    Add('initialization');
    Add('inline');
    Add('is');
    Add('library');
    Add('on');
    Add('out');
    Add('packed');
    Add('property');
    Add('raise');
    Add('resourcestring');
    Add('threadvar');
    Add('try');
    // free pascal reserved
    Add('dispose');
    Add('exit');
    Add('false');
    Add('new');
    Add('true');
    // modifiers
    Add('absolute');
    Add('abstract');
    Add('alias');
    Add('assembler');
    Add('cdecl');
    Add('cppdecl');
    Add('default');
    Add('export');
    Add('external');
    Add('far');
    Add('far16');
    Add('forward');
    Add('index');
    Add('local');
    Add('name');
    Add('near');
    Add('nostackframe');
    Add('oldfpccall');
    Add('override');
    Add('pascal');
    Add('private');
    Add('protected');
    Add('public');
    Add('published');
    Add('read');
    Add('register');
    Add('reintroduce');
    Add('safecall');
    Add('softfloat');
    Add('stdcall');
    Add('virtual');
    Add('write');
    // common types
    Add('integer');
    Add('char');
    Add('longword');
    Add('word');
    Add('qword');
    Add('int64');
    Add('byte');
  end;
end;

constructor TConvertSettings.Create;
begin
  UsedNames := TStringList.Create;
  UsedNames.CaseSensitive := False;
  FillPasReserved(UsedNames);
  EnumsAsConst := True;
  FuncsAreExternal := True;
  RecordsArePacked := True;
  UseBitPacked := True;

  DefaultCType := 'int';
  FuncConv := 'cdecl';
  FuncDeclPostfix:='';
  TypeNamePrefix := '';
  RefTypeNamePrefix := 'P';
  ParamPrefix:='par';

  CtoPasTypes := TStringList.Create;
  CtoPasTypes.Values['bool'] := 'LongBool';
  CtoPasTypes.Values['double'] := 'Double';
  CtoPasTypes.Values['float'] := 'Single';
  CtoPasTypes.Values['float*'] := 'PSingle';
  CtoPasTypes.Values['int'] := 'Integer';
  CtoPasTypes.Values['int*'] := 'PInteger';
  CtoPasTypes.Values['void'] := '';
  CtoPasTypes.Values['void*'] := 'Pointer';
  CtoPasTypes.Values['void**'] := 'PPointer';
  CtoPasTypes.Values['char'] := 'Char';
  CtoPasTypes.Values['char*'] := 'PChar';
  CtoPasTypes.Values['char**'] := 'PPChar';
  CtoPasTypes.Values['signed char'] := 'SmallInt';
  CtoPasTypes.Values['long'] := 'Longword';
  CtoPasTypes.Values['long*'] := 'PLongword';
  CtoPasTypes.Values['long long'] := 'Int64';
  CtoPasTypes.Values['long long*'] := 'PInt64';
  CtoPasTypes.Values['unsigned long long'] := 'QWord';
  CtoPasTypes.Values['unsigned long long*'] := 'PQWord';
  CtoPasTypes.Values['short'] := 'SmallInt';
  CtoPasTypes.Values['short*'] := 'PSmallInt';
  CtoPasTypes.Values['unsigned'] := 'LongWord';
  CtoPasTypes.Values['unsigned short'] := 'Word';
  CtoPasTypes.Values['unsigned short*'] := 'PWord';
  CtoPasTypes.Values['unsigned char'] := 'Byte';
  CtoPasTypes.Values['unsigned char*'] := 'PByte';
  CtoPasTypes.Values['unsigned long'] := 'LongWord';
  CtoPasTypes.Values['unsigned int'] := 'LongWord';
  CtoPasTypes.Values['unsigned long int'] := 'LongWord';
  CtoPasTypes.Values['signed long'] := 'Integer';
  CtoPasTypes.Values['...'] := 'array of const';
  CtoPasTypes.Values['va_list'] := 'array of const';

  // obj-c
  PropsAsMethods:=True;
end;

destructor TConvertSettings.Destroy;
begin
  CtoPasTypes.Free;
  UsedNames.Free;
  inherited Destroy;
end;

function TConvertSettings.GetUniqueName(const n: ansistring): ansistring;
begin
  Result := n;
  while UsedNames.IndexOf(Result) >= 0 do
    Result := Result + '_';
end;

function TConvertSettings.GetTypeName(const CTypeName: ansistring): ansistring;
begin
  Result := CtoPasTypes.Values[CTypeName];
  if (Result = '') and (CTypeName<>'void') then
  begin
    Result := TypeNamePrefix + CTypeName;
    Result := GetUniqueName(Result);
  end;
end;

function PasExp(x: TExpression): AnsiString;
var
  i : Integer;
begin
  Result:='';
  for i:=0 to x.Count-1 do begin
    if x.Tokens[i].TokenType=tt_Symbol then
      Result:=Result+CtoPasSymbol(x.Tokens[i].Token)+' '
    else
      Result:=Result+x.Tokens[i].Token+' ';
  end;
  Result:=Copy(Result, 1, length(Result)-1);
end;

{ THeaderFile }

constructor THeaderFile.Create;
begin
  inherited Create;
  ents := TList.Create;
  cmts := TList.Create;
  pres := TList.Create;
  fileOfs := TFileOffsets.Create;
end;

destructor THeaderFile.Destroy;
begin
  ReleaseList(ents);
  ReleaseList(cmts);
  ReleaseList(pres);
  cmts.Free;
  pres.Free;
  ents.Free;
  fileOfs.Free;
  inherited Destroy;
end;


end.

