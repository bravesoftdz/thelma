{ This code for this unit was originally written by Luis Enrique Silvestre as
  part of his fplot library. It has some subsequent modifications by Stefanos
  Kozanis.

  Copyright (C) 2000 Luis Enrique Silvestre
  Copyright (C) 2007-2009 National Technical University of Athens

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:

  1. Redistributions of source code must retain the above copyright notice,
     this list of conditions and the following disclaimer.
  2. Redistributions in binary form must reproduce the above copyright notice,
     this list of conditions and the following disclaimer in the documentation
     and/or other materials provided with the distribution.
  3. The names of the authors may not be used to endorse or promote products
     derived from this software without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS OR IMPLIED
  WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
  MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO
  EVENT SHALL THE AUTHORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
  OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
  WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
  OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
  ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
}

unit calculus;

interface

type

    TFuncDef = class
          public
                Function name:string; virtual; abstract;
                Function Eval(x:double):double; virtual; abstract;
          end;

    TVarDef = class
          public
                name:String[12];
                value:double;
          end;

    TCalculus = class
              public
              Function eval:double; virtual; abstract;
              end;

    TOperator = class (TCalculus)
          public
                constructor create(c1,c2:TCalculus);
                destructor destroy; override;
          protected
                 e1,e2:TCalculus;
          end;

    TSum = class (TOperator)
          public
              Function eval:double; override;
          end;

    TMinus = class (TOperator)
          public
              Function eval:double; override;
          end;

    TProduct = class (TOperator)
          public
              Function eval:double; override;
          end;

    TDivision = class (TOperator)
          public
              Function eval:double; override;
          end;

    TPower = class (TOperator)
          public
              Function eval:double; override;
          end;

    TFunc = class (TCalculus)
          public
              constructor create(v:TCalculus; f:TFuncDef);
              destructor destroy; override;
              Function eval:double; override;
          protected
              variable:TCalculus;
              def:TFuncDef;
          end;

    TVar = class (TCalculus)
          public
              constructor create(v:TVarDef);
              Function eval:double; override;
          protected
              def:TVarDef;
          end;

    TConst = class (TCalculus)
           public
              constructor create(c:double);
              Function eval:double; override;
           private
              val:double;
           end;

implementation

uses Math;

constructor TOperator.create(c1,c2:TCalculus);
begin
     e1:=c1;
     e2:=c2;
end;

destructor TOperator.destroy;
begin
     e1.Free;
     e2.Free;
end;

Function TSum.eval:double;
begin
     eval:=e1.eval+e2.eval;
end;

Function TMinus.eval:double;
begin
     eval:=e1.eval-e2.eval;
end;

Function TProduct.eval:double;
begin
     eval:=e1.eval*e2.eval;
end;

Function TDivision.eval:double;
begin
     eval:=e1.eval/e2.eval;
end;

Function TPower.eval:double;
begin
     eval:=Power(e1.eval,e2.eval);
end;

constructor TFunc.create(v:TCalculus; f:TFuncDef);
begin
     variable:=v;
     def:=f;
end;

destructor TFunc.destroy;
begin
     variable.Free;
end;

function TFunc.eval:double;
begin
     eval:=def.eval(variable.eval);
end;

constructor TVar.create(v:TVarDef);
begin
     def:=v;
end;

Function TVar.eval:double;
begin
     eval:=def.value;
end;

constructor TConst.create(c:double);
begin
     val:=c;
end;

Function TConst.eval:double;
begin
     eval:=val;
end;

end.
