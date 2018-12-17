unit cparserexp;

interface

uses
  SysUtils, cparsertypes;



const
  CIdentClose : array [TIdentType] of string = ('', ']',')', '', '');
  CIdentOpen  : array [TIdentType] of string = ('', '[','(', '.', '->');

function ParseCExprEx(p: TTextParser): TExp;

function isCTypeCast(exp: TExp; tinfo: TCTypeInfo): Boolean;

implementation

function isCTypeCast(exp: TExp; tinfo: TCTypeInfo): Boolean;
var
  hasType: Boolean;
begin
  Result:=false;
  while Assigned(exp) do begin
    if exp.dir = edPostfix then begin
      exp:=exp.left;
    end else if exp.dir = edPrefix then begin
      exp:=exp.right;
    end else if (exp.dir = edValue) then begin
      if isStdCType(exp.val) then
        hastype:=true
      else begin
        hasType:=Assigned(tinfo) and (tinfo.isType(exp.val));
        if not hasType then Exit // an identify that's not a type
      end;
      exp:=nil;
    end else begin
      // nothing else os allowed in typecast
      Exit;
    end;
  end;
  Result:=hasType;
end;

function _Rotate(core: TExp): TExp;
begin
  if Assigned(core.right) and (core.right.dir<>edValue) and (core.right.pr>=core.pr) then
  begin
    Result:=core.right;
    core.right:=Result.left;
    Result.left:=core;

    // additional rotate
    Result.left:=_Rotate(core);
  end else
    Result:=core;
end;

function isIdentSep(const t: string; var it: TIdentType): Boolean;
begin
  Result:=true;
  if t='(' then it:=itFuncCall
  else if t = '[' then it:=itIndex
  else if t = '.' then it:=itField
  else if t = '->' then it:=itSubSel
  else Result:=false;
end;

function level2(p: TTextParser): TExp;
var
  exp : TExp;
  res : Boolean;
  it  : TIdentType;
  e   : TExp;
begin
  Result:=nil;
  if p.TokenType=tt_Numeric then begin
    exp := TExp.Create(2, '', edValue);
    exp.val:=p.Token;
    p.NextToken;
    Result:=exp;
  end else if p.TokenType=tt_Ident then begin
    exp := TExp.Create(2, '', edValue);
    exp.val:=p.Token;
    p.NextToken;
    res:=isIdentSep(p.Token, it);
    if res then begin
      e:=TExp.Create(2, p.Token, edPostfix);
      e.left:=exp;
      e.identtype:=it;
      exp:=e;
      p.NextToken;
      if it in [itField, itSubSel] then
        exp.right:=level2(p)
      else if it in [itFuncCall, itIndex] then begin
        exp.inner:=ParseCExprEx(p);
        if p.Token = CIdentClose[it] then
          p.NextToken
        else begin
          // error!
        end;
      end;
    end else if (p.Token='++') or (p.Token='--') then begin
      e:=TExp.Create(2, p.Token, edPostfix);
      e.left:=exp;
      exp:=e;
      p.NextToken;
    end;
    Result:=exp;
  end;
end;

function level3(p: TTextParser): TExp;
var
  exp : TExp;
  ct  : TExp;
begin
  exp:=level2(p);
  if not Assigned(exp) then begin
    Result:=nil;
    // typecast
    if (p.Tokentype=tt_Symbol) and (p.Token='(') then begin
      p.NextToken;
      ct:=ParseCExprEx(p);
      if (p.TokenType=tt_Symbol) and (p.Token = ')') then
        p.NextToken;
      if not isCTypeCast(ct, p.CTypeInfo) then begin
        // not a typecast!
        ct.pr:=1;
        Result:=ct;
      end else begin
        Result:=TExp.Create(3, 'typecast', edInfix);
        Result.inner:=ct;
        Result.right:=ParseCExprEx(p);
        //Result:=Rotate(REsult);
      end;
    end else if (p.Token='sizeof') or (p.Token='++') or (p.Token='--')
      or ((length(p.Token) = 1) and (p.Token[1] in ['&','*','~','!','-','+']))
    then begin
      Result:=TExp.Create(3, p.Token, edPrefix);
      p.NextToken;
      Result.right:=ParseCExprEx(p);
      //Result:=Rotate(Result);
    end
  end else
    Result:=exp;
end;

function level5(p: TTextParser): TExp;
var
  e : TExp;
begin
  Result :=level3(p);
  while Assigned(Result) and (p.TokenType = tt_Symbol) and ( (length(p.Token)=1) and (p.Token[1] in ['*','/','%'])) do begin
    e:=Result;
    Result:=TExp.Create(5, p.Token, edInfix);
    p.NextToken;
    Result.left:=e;
    Result.right:=level3(p);
  end;
end;

function level6(p: TTextParser): TExp;
var
  e : TExp;
begin
  Result:=level5(p);
  while Assigned(Result) and  (p.TokenType = tt_Symbol) and ( (length(p.Token)=1) and (p.Token[1] in ['+','-'])) do begin
    e:=Result;
    Result:=TExp.Create(6, p.Token, edInfix);
    p.NextToken;
    Result.left:=e;         // a * b + c
    Result.right:=level5(p);
  end;
end;

function level7(p: TTextParser): TExp;
var
  e : TExp;
begin
  Result:=level6(p);
  while Assigned(Result) and (p.TokenType = tt_Symbol) and ( (p.Token = '<<') or (p.Token='>>')) do begin
    e:=Result;
    Result:=TExp.Create(7, p.Token, edInfix);
    p.NextToken;
    Result.left:=e;
    Result.right:=level6(p);
  end;
end;

function level8(p: TTextParser): TExp;
var
  e : TExp;
begin
  Result:=level7(p);
  while Assigned(Result) and (p.TokenType = tt_Symbol) and ((p.Token='<') or (p.Token='<=') or (p.Token='>=') or (p.Token='>')) do begin
    e:=Result;
    Result:=TExp.Create(8, p.Token, edInfix);
    p.NextToken;
    Result.left:=e;
    Result.right:=level7(p);
  end;
end;

function level9(p: TTextParser): TExp;
var
  e : TExp;
begin
  Result:=level8(p);
  while Assigned(Result) and (p.TokenType = tt_Symbol) and ((p.Token='==') or (p.Token='!=')) do begin
    e := Result;
    Result:=TExp.Create(9, p.Token, edInfix);
    Result.left:=e;
    p.NextToken;
    Result.right:=level8(p);
  end;
end;

function level10(p: TTextParser): TExp;
var
  e : TExp;
begin
  Result:=level9(p);
  while Assigned(Result) and (p.TokenType = tt_Symbol) and (p.Token='&') do begin
    e:=Result;
    Result:=TExp.Create(10, p.Token, edInfix);
    Result.left:=e;
    p.NextToken;
    Result.right:=level9(p);
  end;
end;

function level11(p: TTextParser): TExp;
var
  e : TExp;
begin
  Result:=level10(p);
  while Assigned(Result) and (p.TokenType = tt_Symbol) and (p.Token='^') do begin
    e := Result;
    Result:=TExp.Create(11, p.Token, edInfix);
    Result.left:=e;
    p.NextToken;
    Result.right:=level10(p);
  end;
end;

function level12(p: TTextParser): TExp;
var
  e : TExp;
begin
  Result:=level11(p);
  while Assigned(Result) and (p.TokenType = tt_Symbol) and (p.Token='|') do begin
    e := Result;
    Result:=TExp.Create(12, p.Token, edInfix);
    Result.left:=e;
    p.NextToken;
    Result.right:=level11(p);
  end;
end;

function level13(p: TTextParser): TExp;
var
  e  : TExp;
begin
  Result:=level12(p);
  while Assigned(Result) and (p.TokenType = tt_Symbol) and (p.Token='&&') do begin
    e := Result;
    Result:=TExp.Create(13, p.Token, edInfix);
    Result.left:=e;
    p.NextToken;
    Result.right:=level12(p);
  end;
end;

function level14(p: TTextParser): TExp;
var
  e : TExp;
begin
  Result:=level13(p);
  while Assigned(Result) and (p.TokenType = tt_Symbol) and (p.Token='||') do begin
    e := Result;
    Result:=TExp.Create(14, p.Token, edInfix);
    Result.left:=e;
    p.NextToken;
    Result.right:=level13(p);
    //Result:=Rotate(Result);
  end;
end;

function level15(p: TTextParser): TExp;
var
  e   : TExp;
begin
  Result:=level14(p);
  while Assigned(result) and (p.Token='?') do
  begin
    p.NextToken;
    e:=Result;
    Result:=TExp.Create(15, '?', edTernary);
    Result.main:=e;
    Result.left:=level14(p);
    if p.Token = ':' then p.NextToken;
    Result.right:=level14(p);
  end;
end;

function level16(p: TTextParser): TExp;
var
  tk  : string;
  e   : TExp;
begin
  Result:=level15(p);
  while Assigned(Result) and ((tk='=')
       or (tk='+=') or (tk='-=') or (tk='*=') or (tk='/=')
       or (tk='%=') or (tk='<<=') or (tk='>>=') or (tk='&=')
       or (tk='^=') or (tk='|=')) do
  begin
    e:=Result;
    Result:=TExp.Create(16, tk, edInfix);
    Result.right:=e;
    p.NextToken;
    Result.left:=level15(p);
  end;
end;

function level17(p: TTextParser): TExp;
var
  tk  : string;
  e   : TExp;
begin
  Result:=level16(p);
  while Assigned(Result) and (p.TokenType=tt_Symbol) and (p.Token=',') do begin
    e:=Result;
    p.NextToken;
    Result:=TExp.Create(17, ',', edSequence);
    Result.left:=e;
    Result.right:=level16(p);
  end;
end;

function ParseCExprEx(p: TTextParser): TExp;
begin
  Result:=level17(p);
end;

end.
