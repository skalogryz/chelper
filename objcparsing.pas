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
unit
  objcparsing;

interface

{$ifdef fpc}{$mode delphi}{$h+}{$endif}

uses
  Classes, SysUtils, cparsertypes;

const
  objcend = '@end';

type

  { TObjCClasses }

  TObjCClasses = class(TEntity)
    ClassList : TStringList;
    constructor Create(AOffset: Integer=-1); override;
    destructor Destroy; override;
  end;

  { TObjCMethod }

  TObjCMethodArg = record
    RetType  : TEntity;
    TypeName : TNamePart;
    Name     : AnsiString;
  end;

  TObjCMethodOpt = (mo_Required, mo_Optional);

  TObjCMethod = class(TEntity)
  public
    isClassMethod : Boolean;
    Name      : TStringList;
    RetType   : TEntity;
    RetName   : TNamePart;
    Args      : array of TObjCMethodArg;
    Option    : TObjCMethodOpt;
    VarParams : Boolean;
    constructor Create(AOffset: Integer=-1); override;
    destructor Destroy; override;
    procedure AddArg(const ArgType: TEntity; ArgTypeName: TNamePart; const Name: AnsiString);
  end;

  TObjCScope = (os_Private, os_Protected, os_Public, os_Package);

  { TObjCInstVar }

  TObjCInstVar = class(TEntity)
  public
    scope  : TObjCScope;
    v      : TVarFuncEntity;
    bits   : TExpression;
    isbitted : Boolean;
    destructor Destroy; override;
  end;

  { TObjCInterface }

  TObjCInterface = class(TEntity)
  public
    Name        : AnsiString;
    SuperClass  : AnsiString;
    isCategory  : Boolean;
    Protocols   : TStringList;
    Vars        : TList;
    Methods     : TList;
    Entities    : TList;
    constructor Create(AOffset: Integer=-1); override;
    destructor Destroy; override;
  end;

  { TObjCProtocol }

  TObjCProtocol = class(TEntity)
  public
    Names       : TStringList;
    isForward   : Boolean;
    Protocols   : TStringList;
    Methods     : TList;
    Entities    : TList;
    constructor Create(AOffset: Integer=-1); override;
    destructor Destroy; override;
  end;

  { TObjCProperty }

  TObjCProperty = class(TEntity)
  public
    Name        : TNamePart;
    RetType     : TEntity;
    SetterName  : AnsiString;
    GetterName  : AnsiString;
    Props       : TStringList;
    constructor Create(AOffset: Integer=-1); override;
    destructor Destroy; override;
  end;

function ParseClassList(AParser: TTextParser): TObjCClasses;
function ParseInterface(AParser: TTextParser): TObjCInterface;
function ParseMethod(AParser: TTextParser): TObjCMethod;
function ParseProperty(AParser: TTextParser): TObjCProperty;
function ParseMethods(AParser: TTextParser; MethodsList, OthersList: TList; const EndToken: AnsiString = objcend): Boolean;
function ParseProtocol(AParser: TTextParser): TEntity;

function ParseNextObjCEntity(AParser: TTextParser): TEntity;

function isObjCKeyword(const token: AnsiString): Boolean; inline;
function GetObjCKeyword(const token: AnsiString): AnsiString;

const
  nk_Protocol = $1000;

implementation

function isObjCKeyword(const token: AnsiString): Boolean; inline;
begin
  Result:=(token<>'') and (token[1]='@');
end;

function GetObjCKeyword(const token: AnsiString): AnsiString;
begin
  if isObjCKeyword(token) then Result:=Copy(token, 2, length(token)-1)
  else Result:=token;
end;

function ParseClassList(AParser: TTextParser): TObjCClasses;
var
  cl  : TObjCClasses;
begin
  Result:=nil;
  if AParser.Token<>'@class' then Exit;
  try
    cl:=TObjCClasses.Create(AParser.TokenPos);
    AParser.NextToken;
    while AParser.Token<>';' do begin
      if AParser.TokenType<>tt_Ident then begin
        ErrorExpect(AParser,'identifier');
        cl.Free;
        Exit;
      end;
      cl.ClassList.Add(AParser.Token);
      AParser.NextToken;
      if AParser.Token=',' then
        AParser.NextToken
      else if AParser.Token<>';' then begin
        ErrorExpect(AParser,';');
        cl.Free;
        Exit;
      end;
    end;
    if AParser.Token<>';' then ErrorExpect(AParser, ';');
    Result:=cl;
  finally
    if not Assigned(Result) then cl.Free;
  end;
end;

function ParseInstVars(AParser: TTextParser; Vars: TList): Boolean;
var
  v     : TVarFuncEntity;
  iv    : TObjCInstVar;
  s     : AnsiString;
  scope : TObjCScope;
  isbitted : Boolean;
  bits  : TExpression;
begin
  Result:=True;
  if AParser.Token<>'{' then Exit;

  Result:=False;
  AParser.NextToken;

  scope:=os_Protected;
  while AParser.Token<>'}' do begin
    if isObjCKeyword(AParser.Token) then begin
      s:=GetObjCKeyword(APArser.Token);
      if s='protected' then scope:=os_Protected
      else if s='private' then scope:=os_Private
      else if s='public' then scope:=os_Public
      else if s='package' then scope:=os_Package
      else begin
        ErrorExpect(AParser,'}');
        Exit;
      end;
      AParser.NextToken;
    end else begin
      v:=TVarFuncEntity.Create(AParser.TokenPos);
      if not ParseNames(AParser, v.RetType, v.Names, [';',':']) then Exit;

      if AParser.Token=':' then begin
        AParser.NextToken;
        isbitted:=True;
        bits:=ParseCExpr(AParser);
      end else begin
        isbitted:=false;
        bits:=nil;
      end;
      iv:=TObjCInstVar.Create(v.Offset);
      iv.v:=v;
      iv.scope:=scope;
      iv.isbitted:=isbitted;
      iv.bits:=bits;
      Vars.Add(iv);
      if AParser.Token=';' then AParser.NextToken;
    end;
  end;
  AParser.NextToken;

  Result:=True;
end;

function ParseInterface(AParser: TTextParser): TObjCInterface;
var
  itf : TObjCInterface;
  i   : Integer;
  nm  : AnsiString;
  ent : TEntity;
begin
  Result:=nil;
  if AParser.Token<>'@interface' then Exit;
  i:=AParser.TokenPos;
  AParser.NextToken;

  if not ConsumeIdentifier(AParser, nm) then Exit;

  itf:=TObjCInterface.Create(i);
  try
    itf.isCategory:=AParser.Token='(';
    if itf.isCategory then begin
      itf.SuperClass:=nm;
      AParser.NextToken;
      if not (ConsumeIdentifier(AParser, itf.Name) and ConsumeToken(AParser, ')')) then
        Exit;
    end else begin
      itf.Name:=nm;
      // super-class
      if AParser.Token=':' then begin
        AParser.NextToken;
        if not ConsumeIdentifier(AParser, itf.SuperClass) then Exit;
      end;
    end;

    // protocols, also can be specified for categories
    if AParser.Token='<' then begin
      AParser.NextToken;
      while AParser.Token<>'>' do begin
        if not ConsumeIdentifier(AParser, nm) then Exit;
        itf.Protocols.Add(nm);
        if AParser.Token=',' then AParser.NextToken
        else if AParser.Token<>'>' then begin
          ErrorExpect(AParser, '>');
          Exit;
        end;
      end;
      AParser.NextToken;
    end;


    // categories cannot have instance variables
    if not itf.isCategory then ParseInstVars(AParser, itf.Vars);

    if not ParseMethods(AParser, itf.Methods, itf.Entities, objcend) then Exit;
    if AParser.Token<>objcend then ErrorExpect(AParser, objcend);

    Result:=itf;
  finally
    if not Assigned(Result) then itf.Free;
  end;
end;

function ParseProtocol(AParser: TTextParser): TEntity;
var
  p   : TObjCProtocol;
  nm  : AnsiString;
begin
  Result:=nil;
  if AParser.Token<>'@protocol' then Exit;
  p := TObjCProtocol.Create(AParser.TokenPos);
  try
    AParser.NextToken;
    if not ConsumeIdentifier(AParser, nm) then Exit;
    p.Names.Add(nm);
    p.isForward:= (AParser.Token=';') or (AParser.Token=',');
    if p.isForward then begin
      while AParser.Token<>';' do begin
        AParser.NextToken;
        ConsumeIdentifier(AParser, nm);
        p.Names.Add(nm);
      end;
      Result:=p;
      if AParser.Token<>';' then ErrorExpect(AParser, ';');
      Exit;
    end;

    if AParser.Token='<' then begin
      AParser.NextToken;
      while AParser.Token<>'>' do begin
        if not ConsumeIdentifier(AParser, nm) then Exit;
        p.Protocols.Add(nm);
        if AParser.Token=',' then AParser.NextToken;
      end;
      if AParser.Token='>' then AParser.NextToken;
    end;

    if ParseMethods(AParser, p.Methods, p.Entities, objcend) then
      Result:=p;
    if AParser.Token<>objcend then ErrorExpect(AParser, objcend);
  finally
    if not Assigned(Result) then p.Free;
  end;
end;

var
  PrevParseNextEntity : function (AParser: TTextParser): TEntity = nil;
  PrevNamePart : function (AParser: TTextParser): TNamePart = nil;

function ParseNextObjCEntity(AParser: TTextParser): TEntity;
var
  t   : AnsiString;
begin
  Result:=nil;
  if (AParser.Token<>'') and (AParser.Token[1]='@') then begin
    t:=GetObjCKeyword(AParser.Token);
    if t='class' then Result:=ParseClassList(AParser)
    else if t='interface' then Result:=ParseInterface(AParser)
    else if t='protocol' then Result:=ParseProtocol(AParser);
  end else begin
    if Assigned(PrevParseNextEntity) then
      Result:=PrevParseNextEntity(AParser)
    else
      Result:=nil;
  end;
end;

{ TObjCClasses }

constructor TObjCClasses.Create(AOffset:Integer);
begin
  inherited Create(AOffset);
  ClassList := TStringList.Create;
end;

destructor TObjCClasses.Destroy;
begin
  ClassList.Free;
  inherited Destroy;
end;

{ TObjCInterface }

constructor TObjCInterface.Create(AOffset:Integer);
begin
  Vars := TList.Create;
  Methods := TList.Create;
  Entities := TList.Create;
  Protocols := TStringList.Create;
  inherited Create(AOffset);
end;

destructor TObjCInterface.Destroy;
var
  i : Integer;
begin
  for i:=0 to Vars.Count-1 do TObject(Vars[i]).Free;
  Vars.Free;
  for i:=0 to Methods.Count-1 do TObject(Methods[i]).Free;
  Methods.Free;
  for i:=0 to Entities.Count-1 do TObject(Entities[i]).Free;
  Entities.Free;
  Protocols.Free;
  inherited Destroy;
end;

function isObjCParamMod(const t: string): Boolean;
begin
  if t ='' then Result:=false
  else begin
    Result:=false;
    case t[1] of
      'c': if (t = 'const') then Result:=true;
      'i': if (t = 'in') or (t = 'inout') then Result:=true;
      'o': if (t = 'out') or (t='oneway') then Result:=true;
      'b': if (t = 'bycopy') or (t='byref') then Result:=true;
    end;
  end;
end;

function ParseMethod(AParser: TTextParser): TObjCMethod;
var
  m       : TObjCMethod;
  nm      : AnsiString;
  atype   : TEntity;
  atname  : TNamePart;
  aname   : Ansistring;
  prm     : Boolean;
begin
  Result:=nil;
  if (AParser.Token<>'+') and (AParser.Token<>'-') then Exit;

  m:=TObjCMethod.Create(AParser.TokenPos);
  try
    m.isClassMethod:=AParser.Token='+';
    AParser.NextToken;

    if AParser.Token='(' then begin
      AParser.NextToken;
      if not ParseName(AParser,  m.RetType, m.RetName,[')']) then Exit;
      if not ConsumeToken(AParser, ')') then Exit;
    end;

    if not ConsumeIdentifier(AParser, nm) then Exit;

    if (AParser.Token=':') then begin
      m.Name.Add(nm+':');
      AParser.NextToken;

      while (AParser.Token<>';') and (AParser.Token<>',') do begin
        if AParser.Token='(' then begin
          prm:=ConsumeToken(AParser, '(');
          if prm then begin
            // todo: need to store them somewhere!
            while isObjCParamMod(AParser.Token) do
              AParser.NextToken;

            prm:=ParseName(APArser, atype, atname,[')']) and
            ConsumeToken(AParser, ')','parsing objc method paramter type');
          end;
        end else begin
          prm:=True;
          atype:=nil;
          atname:=nil;
        end;
        if not prm then Exit;
        ConsumeIdentifier(AParser, aname);
        m.AddArg(atype, atname, aname);

        // the next name starts
        if AParser.TokenType=tt_Ident then ConsumeIdentifier(AParser, nm) else nm:='';
        if (AParser.Token<>';') and (AParser.Token<>',') then begin
          if not ConsumeToken(AParser,':','parsing the objc method " '+Copy(Aparser.Buf, AParser.Index-30, 40)+'"') then Exit;
          m.Name.Add(nm+':');
        end;
      end;
    end else
      m.Name.Add(nm);

    if AParser.Token=',' then begin
      AParser.NextToken;
      if ConsumeToken(AParser,'...') then m.VarParams:=True
      else ErrorExpect(AParser, '...');
    end;

    if not ConsumeToken(AParser, ';','parsing the end of objc method') then Exit;

    Result:=m;
  finally
    if not Assigned(Result) then m.Free;
  end;
end;

function ParseMethods(AParser: TTextParser; MethodsList, OthersList: TList; const EndToken: AnsiString): Boolean;
var
  m   : TObjCMethod;
  p   : TObjCProperty;
  opt : TObjCMethodOpt;
  s   : AnsiString;
  ent : TEntity;
begin
  Result:=False;
  if not Assigned(MethodsList) or not Assigned(AParser) then Exit;
  opt:=mo_Required;
  while (AParser.Token<>EndToken) and (AParser.Token<>'') do begin

    if (AParser.Token[1] in ['+','-','@']) then begin
      if isObjCKeyword(AParser.Token) then begin
        s:=GetObjCKeyword(AParser.Token);
        if s='property' then begin
          p:=ParseProperty(AParser);
          MethodsList.Add(p);
        end else begin
          if s='optional' then opt:=mo_Optional
          else opt:=mo_Required;
          AParser.NextToken;
        end;
      end else begin
        m:=ParseMethod(AParser);
        if not Assigned(m) then Exit;
        m.Option:=opt;
        MethodsList.Add(m);
      end;
    end else begin
      ent:=ParseNextCEntity(AParser, false);
      AParser.NextToken;
    end;
  end;
  Result:=True;
end;

{ TObjCMethod }

constructor TObjCMethod.Create(AOffset:Integer);
begin
  inherited Create(AOffset);
  Name      := TStringList.Create;
end;

destructor TObjCMethod.Destroy;
var
  i : Integer;
begin
  Name.Free;
  RetType.Free;
  RetName.Free;
  for i:=0 to length(Args)-1 do begin
    Args[i].RetType.Free;
    Args[i].TypeName.Free;
  end;
  inherited Destroy;
end;

procedure TObjCMethod.AddArg(const ArgType:TEntity;ArgTypeName:TNamePart;const Name:AnsiString);
var
  i : Integer;
begin
  i:=length(Args);
  SetLength(Args, i+1);
  Args[i].Name:=Name;
  Args[i].RetType:=ArgType;
  Args[i].TypeName:=ArgTypeName;
end;

{ TObjCInstVar }


destructor TObjCInstVar.Destroy;
begin
  v.Free;
  bits.Free;
  inherited Destroy;
end;

{ TObjCProtocol }

constructor TObjCProtocol.Create(AOffset:Integer);
begin
  inherited Create(AOffset);
  Protocols := TStringList.Create;
  Methods   := TList.Create;
  Entities  := TList.Create;
  Names     := TStringList.Create;
end;

destructor TObjCProtocol.Destroy;
var
  i : Integer;
begin
  for i:=0 to Methods.Count-1 do TObject(Methods[i]).Free;
  Methods.Free;
  for i:=0 to Entities.Count-1 do TObject(Entities[i]).Free;
  Entities.Free;
  Protocols.Free;
  Names.Free;
  inherited Destroy;
end;

{ TObjCProperty }

constructor TObjCProperty.Create(AOffset:Integer);
begin
  inherited Create(AOffset);
  Props:=TStringList.Create;
end;

destructor TObjCProperty.Destroy;
begin
  RetType.Free;
  Name.Free;
  Props.Free;
  inherited Destroy;
end;

function ParseProperty(AParser: TTextParser): TObjCProperty;
var
  p   : TObjCProperty;
  s   : AnsiString;
  nm  : AnsiString;
begin
  Result:=nil;
  if AParser.Token<>'@property' then Exit;
  AParser.NextToken;
  p := TObjCProperty.Create(AParser.TokenPos);
  try
    if AParser.Token='(' then begin
      AParser.NextToken;
      while AParser.Token<>')' do begin
        s:=AParser.Token;
        if (s='setter') or (s='getter') then begin
          AParser.NextToken;
          if not ConsumeToken(AParser, '=') then Exit;
          if not ConsumeIdentifier(AParser, nm) then Exit;
          while (AParser.TokenType=tt_Ident) or (APArser.Token=':') do begin
            nm:=nm+AParser.Token;
            AParser.NextToken;
          end;
          if s='setter' then p.SetterName:=nm
          else p.GetterName:=nm;
        end else begin
          if APArser.TokenType=tt_Ident then p.Props.Add(AParser.Token);
          AParser.NextToken;
        end;
        if AParser.Token=',' then AParser.NextToken;
      end;
      if AParser.Token=')' then
        AParser.NextToken
      else begin
        ErrorExpect(AParser,')');
        Exit;
      end;
      if ParseName(AParser, p.RetType, p.Name,[';']) then begin
        Result:=p;
        if AParser.Token=';' then AParser.NextToken;
      end;

    end;

  finally
    if not Assigned(Result) then p.Free;
  end;
end;

function ParseObjCNamePart(AParser: TTextParser): TNamePart;
var
  p   : AnsiString;
begin
  // skipping protocol adopted type definition
  if AParser.Token='<' then begin
    Result:=nil;
    AParser.NextToken;
    repeat
      if not ConsumeIdentifier(AParser, p) then Exit;
      if AParser.Token=',' then AParser.NextToken;
    until AParser.Token='>';

    if not ConsumeToken(AParser,'>') then Exit;
  end;
  Result:=PrevNamePart(AParser);
end;

initialization
  PrevParseNextEntity:=_ParseNextEntity;
  _ParseNextEntity:=@ParseNextObjCEntity;

  PrevNamePart:=ParseNamePart;
  ParseNamePart:=ParseObjCNamePart;

end.

