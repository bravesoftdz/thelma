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


unit standardFuncs;

interface
uses Calculus, SysUtils;

type

    TSqr = class(TFuncDef)
         public
                Function name:string; override;
                Function Eval(x:double):double; override;
          end;

    TSqrt = class(TFuncDef)
         public
                Function name:string; override;
                Function Eval(x:double):double; override;
          end;

    TAbs = class(TFuncDef)
         public
                Function name:string; override;
                Function Eval(x:double):double; override;
          end;

    TSin = class(TFuncDef)
         public
                Function name:string; override;
                Function Eval(x:double):double; override;
          end;

    TCos = class(TFuncDef)
         public
                Function name:string; override;
                Function Eval(x:double):double; override;
          end;

    TTan = class(TFuncDef)
         public
                Function name:string; override;
                Function Eval(x:double):double; override;
          end;

    TArcTan = class(TFuncDef)
         public
                Function name:string; override;
                Function Eval(x:double):double; override;
          end;

    TArcSin = class(TFuncDef)
         public
                Function name:string; override;
                Function Eval(x:double):double; override;
          end;

    TArcCos = class(TFuncDef)
         public
                Function name:string; override;
                Function Eval(x:double):double; override;
          end;

    TLog = class(TFuncDef)
         public
                Function name:string; override;
                Function Eval(x:double):double; override;
          end;

    TLog10 = class(TFuncDef)
         public
                Function name:string; override;
                Function Eval(x:double):double; override;
          end;

    TExp = class(TFuncDef)
         public
                Function name:string; override;
                Function Eval(x:double):double; override;
          end;

    TRandom = class(TFuncDef)
         public
                Function name:string; override;
                Function Eval(x:double):double; override;
          end;

    TNormalRandom = class(TFuncDef)
          public
                Function name:string; override;
                Function Eval(x:double):double; override;
          end;

    TTrunc = class(TFuncDef)
          public
                Function name:string; override;
                Function Eval(x:double):double; override;
          end;

    TInt = class(TFuncDef)
          public
                Function name:string; override;
                Function Eval(x:double):double; override;
          end;

    TRound = class(TFuncDef)
          public
                Function name:string; override;
                Function Eval(x:double):double; override;
          end;

implementation

uses Math, rnd;

Function TSqr.name:string;
begin
     name:='sqr';
end;

Function TSqr.Eval(x:double):double;
begin
     eval:=Sqr(x);
end;

Function TSqrt.name:string;
begin
     name:='sqrt';
end;

Function TSqrt.Eval(x:double):double;
begin
     eval:=Sqrt(x);
end;

Function TAbs.name:string;
begin
     name:='abs';
end;

Function TAbs.Eval(x:double):double;
begin
     eval:=Abs(x);
end;

Function TSin.name:string;
begin
     name:='sin';
end;

Function TSin.Eval(x:double):double;
begin
     eval:=Sin(x);
end;

Function TCos.name:string;
begin
     name:='cos';
end;

Function TCos.Eval(x:double):double;
begin
     eval:=cos(x);
end;

Function TTan.name:string;
begin
     name:='tan';
end;

Function TTan.Eval(x:double):double;
begin
     eval:=Tan(x);
end;

Function TArcTan.name:string;
begin
     name:='arctan';
end;

Function TArcTan.Eval(x:double):double;
begin
     eval:=ArcTan(x);
end;

Function TArcSin.name:string;
begin
     name:='arcsin';
end;

Function TArcSin.Eval(x:double):double;
begin
     eval:=ArcSin(x);
end;

Function TArcCos.name:string;
begin
     name:='arccos';
end;

Function TArcCos.Eval(x:double):double;
begin
     eval:=ArcCos(x);
end;

Function TLog.name:string;
begin
     name:='log';
end;

Function TLog.Eval(x:double):double;
begin
     eval:=Ln(x);
end;

Function TLog10.name:string;
begin
     name:='log10';
end;

Function TLog10.Eval(x:double):double;
begin
     eval:=Log10(x);
end;

Function TExp.name:string;
begin
     name:='exp';
end;

Function TExp.Eval(x:double):double;
begin
     eval:=Exp(x);
end;

Function TRandom.name:string;
begin
     name:='random';
end;

Function TRandom.Eval(x:double):double;
begin
     eval:=Random*x;
end;

Function TNormalRandom.name:string;
begin
     name:='nrnd';
end;

Function TNormalRandom.Eval(x:double):double;
begin
     eval:=nrnd(0,x);
end;

Function TTrunc.name:string;
begin
     name:='trunc';
end;

Function TTrunc.Eval(x:double):double;
begin
     eval:=Max(x, 0);
end;

Function TInt.name:string;
begin
     name:='int';
end;

Function TInt.Eval(x:double):double;
begin
     eval:=Int(x)
end;

Function TRound.name:string;
begin
     name:='round';
end;

Function TRound.Eval(x:double):double;
begin
     eval:=Round(x);
end;

end.
