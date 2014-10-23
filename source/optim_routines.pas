{******************************************************************}
{                                                                  }
{  Thelma library                                                  }
{                                                                  }
{  Copyright (c) 2006   National Technical University of Athens    }
{                                                                  }
{******************************************************************}

{** Optimization algorithms.
}

unit optim_routines;

interface

uses Classes, SysUtils, Math, Matrix;

type
  TMathFunc = function(X: Real): Real;
  TMathFuncMult = function(X: TVector): Real;
  TMathFuncMultDeriv = function(X: TVector): TVector;

  TArrayOfReal = array of Real;
  T2DArrayOfReal = array of array of Real;
  TIntArray = array of Integer;

  TMathFuncMult1 = function(var X: TArrayOfReal): Real;


function SIGN(a,b:Real):Real;
function Bisection(f:TMathFunc; x1, x2, xacc: Real): Real;
procedure GoldenSection(f:TMathFunc; ax,bx,cx,tol: Real; var xmin,fmin:Real);
procedure MinimumBracket(f:TMathFunc; var ax,bx,cx,fa,fb,fc: Real);
procedure Brent(f:TMathFunc; ax,bx,cx,tol:Real; var xmin,fmin:Real);
procedure DBrent(f,df:TMathFunc; ax,bx,cx,tol:Real; var xmin,fmin:Real);
procedure LinMin(f:TMathFuncMult; var Xi:TVector; var p:TVector; var fret:Real);
function FDim(x:Real):Real;
procedure dLinMin(f:TMathFuncMult; df:TMathFuncMultDeriv; var xi:TVector; var p:TVector; var fret:Real);
function dFDim(x:Real):Real;
procedure FletcherReevesPolakRibiere(f:TMathFuncMult; df:TMathFuncMultDeriv; ftol:Real; var p:TVector; var iter:Integer; var fret:Real);
procedure NelderMead(f:TMathFuncMult; ftol:Real; var p:TMatrix; var y:TVector; var nfunk:Integer);

{** The evolutionary annealing-simplex method.
    Multidimensional minimization of the function F(x), where x[1..n] is a
    vector in n dimensions, by simulated annealing combined with the downhill
    simplex method of Nelder and Mead. Input arguments are:<p>
    -------------------------------------------------------<p>
    n           = problem dimension<p>
    m           = population count (m > n + 1)<p>
    xmin[n]     = lower parameter bounds<p>
    xmax[n]     = upper parameter bounds<p>
    xopt[n]     = vector containing the optimal values of control variables<p>
    F           = objective function<p>
    fopt        = the minimal value of the objective function<p>
    eval        = number of function evaluations<p>
    ftol        = the fractional convergence tolerance to be achieved in the<p>
                  function value for an early return
    maxeval     = maximum number of function evaluations<p>
    ratio       = fraction of temperature reduction, when a local minimum is
                  found<p>
    beta        = annealing schedule parameter<p>
    pmut        = probability of accepting an offspring generated via
                  mutation<p>
    maxclimbs   = maximum number of uphill steps<p>
    iniflg      = if true then xopt is one of the starting points<p>
    restart     = if true then the search procedure is repeated<p>
    @author Andreas Efstratiadis
}
procedure AnnealSimplex(n, m: Integer; xmin, xmax: TArrayOfReal;
                        var xopt: TArrayOfReal; F: TMathFuncMult1;
                        var fopt: Real; var eval: Integer;  ftol: Real; maxeval: Integer;
                        ratio, pmut, beta: Real; maxclimbs: Integer; iniflg, restart: Boolean;
                        var StopOptim: Boolean);

procedure NewPoint(num_var: Integer; var eval: Integer; a1, a2: Real; F: TMathFuncMult1;
                   var x, x1, x2, xmin, xmax: TArrayOfReal; var fx: Real; var boundary: Boolean);


var
  Pcom, Xicom: TVector;
  TVectorFunction: TMathFuncMult;
  TVectorFunctionDeriv: TMathFuncMultDeriv;

implementation


function SIGN(a,b:Real):Real;
begin
  If b>=0 then Result:=ABS(a) else Result:=-ABS(a);
end;


function Bisection(f:TMathFunc; x1, x2, xacc: Real): Real;
{Root finding using the bisection method}
const
  MaxIter = 100;
  small = 10e-6;
var
  j: Integer;
  dx, fx, fmid, xmid, rtb: Real;
begin
  fx:=f(x1);
  fmid:=f(x2);

  If fx*fmid>=0 then
  begin
    Bisection:=small;
    Exit;
  end;

  If (fx<0) then
  begin
    rtb := x1;
    dx := x2-x1
  end
  else
  begin
    rtb := x2;
    dx := x1-x2
  end;

  j:=0;
  Repeat
    j:=j+1;
    dx:=dx*0.5;
    xmid:=rtb+dx;
    fmid:=f(xmid);
    If (fmid<=0) then rtb:=xmid;
  until ((ABS(dx)<xacc) or (fmid=0) or (j=MaxIter));

  Bisection:=rtb;

end; {function Bisection}


procedure GoldenSection(f:TMathFunc; ax,bx,cx,tol: Real; var xmin,fmin: Real);
{Given a function f and given a bracketing triplet of abscissas ax, bx, cx (such
as bx is between ax and cx, and f(bx) is less than both f(ax) and f(cx)),this
routine performs a golden section search for the minimum, isolating it to a
fractional precision of about tol. The abscissa of the minimum is returned as
xmin and the minimum function value as fmin}
const
  R = 0.61803399;
  C = 1-R;
var
  f1, f2, x0, x1, x2, x3 : Real;

  procedure Change2(var k,l:Real; m:Real);
  begin
    k:=l; l:=m;
  end;

  procedure Change3(var k,l,m:Real; n:Real);
  begin
    k:=l; l:=m; m:=n;
  end;

begin
  x0:=ax;
  x3:=cx;

  If ABS(cx-bx)>ABS(bx-ax) then
  begin
    x1:=bx;
    x2:=bx+C*(cx-bx);
  end
  else
  begin
    x2:=bx;
    x1:=bx-C*(bx-ax);
  end;

  f1:=f(x1);
  f2:=f(x2);

  While ABS(x3-x0)>tol*(ABS(x1)+ABS(x2)) do
  begin
    If f2<f1 then
    begin
      Change3(x0,x1,x2,R*x2+C*x3);
      Change2(f1,f2,f(x2));
    end
    else
    begin
      Change3(x3,x2,x1,R*x1+C*x0);
      Change2(f2,f1,f(x1));
    end;
  end;

  If f1<f2 then
  begin
    xmin:=x1;
    fmin:=f(x1);
  end
  else
  begin
    xmin:=x2;
    fmin:=f(x2);
  end;
end;{procedure GoldenSection}


procedure MinimumBracket(f:TMathFunc; var ax,bx,cx,fa,fb,fc:Real);
{Given a function f and initial points ax and bx, the routine searches in the
downhill direction and returns new points ax, bx, cx that bracket a minimum of the
function. Also returned are the function values at the three points fa, fb, fc}
const
  gold = 1.618034; {ratio by witch successive intervals are magnified}
  limit = 100; {maximum magnification allowed for a parabolic-fit step}
  tiny = 10e-20;
var
  ulim, u, r, q, fu, temp : Real;

  function MAX(a,b:Real):Real;
  begin
    If a>b then Result:=a else Result:=b;
  end;

begin
  fa:=f(ax);
  fb:=f(bx);

  If fb>fa then {switch roles of a and b so that we can go downhill in the direction from a to b}
  begin
    temp:=ax;
    ax:=bx;
    bx:=temp;
    temp:=fb;
    fb:=fa;
    fa:=temp;
  end;

  cx:=bx+gold*(bx-ax); {first guess of c}
  fc:=f(cx);

  While (fb>fc) do
  begin
    r:=(bx-ax)*(fb-fc);
    q:=(bx-cx)*(fb-fa);
    u:=bx-((bx-cx)*q-(bx-ax)*r)/(2*SIGN(MAX(ABS(q-r),tiny), q-r)); {parabolic extrapolation from a,b,c}
    ulim:=bx+limit*(cx-bx);

    If (bx-u)*(u-cx)>0.0 then {parabolic u is between b and c}
    begin
      fu:=f(u);
      If fu<fc then {got a minimum between b and c}
      begin
        ax:=bx;
        bx:=u;
        fa:=fb;
        fb:=fu;
      end
      else if fu>fb then {got a minimum between a and u}
      begin
        cx:=u;
        fc:=fu;
      end;

      u:=cx+gold*(cx-bx); {parabolic fit was no use, use default magnification}
      fu:=f(u);
    end
    else if (cx-u)*(u-ulim)>0.0 then {parabolic fit is between c and its allowed limit}
    begin
      fu:=f(u);
      If fu<fc then
      begin
        bx:=cx;
        cx:=u;
        u:=cx+gold*(cx-bx);
        fb:=fc;
        fc:=fu;
        fu:=f(u)
      end;
    end
    else if (u-ulim)*(ulim-cx)>=0.0 then {limit parabolic u to a maximum allowed value}
    begin
      u:=ulim;
      fu:=f(u);
    end
    else {reject parabolic u, use default magnification}
    begin
      u:=cx+gold*(cx-bx);
      fu:=f(u);
    end;

    {eliminate oldest point and continue}
    ax:=bx;
    bx:=cx;
    cx:=u;
    fa:=fb;
    fb:=fc;
    fc:=fu;
  end;

end; {procedure MinimumBracket}


procedure Brent(f:TMathFunc; ax,bx,cx,tol:Real; var xmin,fmin:Real);
{Given a function f, and given a bracketing triplet of abscissas ax, bx, cx
[such that bx is between ax and cx, and f(bx) is less than both f(ax) and f(cx)]
, this routine isolates the minimum to a fractional precision of about tol using
 Brent's method. The abscissa of the minimum is returned as xmin, and the
minimum function value is returned as fmin}
const
  ItMax = 100; {Maximum allowed number of iterations}
  CGold = 0.3819660; {Golden ratio}
  ZEPS = 1e-10; {Small number that protects against trying to achieve fractional
                 accuracy for a minimum that happens to be exactly zero}

var
  a, b, d, etemp, fu, fv, fw, fx, p, q, r, tol1, tol2, u, v, w, x, xm: Real;
  e: Real; {This will be the distance moved on the step before last}
  iter: Integer;
begin
  e:=0; d:=0;
  If ax<cx then a:=ax else a:=cx; {a and b must be in ascending order, but input abscissas need not be}
  If ax>cx then b:=ax else b:=cx;

  v:=bx; {Initializations}
  w:=v;
  x:=w;
  fx:=f(x);
  fv:=fx;
  fw:=fv;

  For iter:=1 to ItMax do {Main program loop}
  begin
    xm:=0.5*(a+b);
    tol1:=tol*ABS(x)+ZEPS;
    tol2:=2*tol1;

    If ABS(x-xm)<=(tol2-0.5*(b-a)) then {Test for done here}
    begin
      xmin:=x;
      fmin:=fx;
      exit;
    end;

    If ABS(e)>tol1 then {Construct a trial parabolic fit}
    begin
      r:=(x-w)*(fx-fv);
      q:=(x-v)*(fx-fw);
      p:=(x-v)*q-(x-w)*r;
      q:=2*(q-r);
      If q>0 then p:=-p;
      q:=ABS(q);

      etemp:=e;
      e:=d;

      If (ABS(p)>=ABS(0.5*q*etemp)) or (p<=q*(a-x)) or (p>=q*(b-x)) then
      begin
       {The above conditions determine the acceptability of the parabolic fit.
       Here we take the golden section step into the larger of the two segments}
        If x>=xm then e:=a-x else e:=b-x;
        d:=CGold*e;
      end
      else {Take the parabolic step}
      begin
        d:=p/q;
        u:=x+d;
        If (u-a<tol2) or (b-u<tol2) then d:=SIGN(tol1,xm-x);
      end;
    end
    else
    begin
      If x>=xm then e:=a-x else e:=b-x;
      d:=CGold*e;
    end;

    If ABS(d)>=tol1 then u:=x+d else u:=x+SIGN(tol1,d);
    fu:=f(u); {This is the one function evaluation per iteration}
    If fu<=fx then {Now decide what to do with our function evaluation}
    begin
      If u>=x then a:=x else b:=x;
      v:=w; w:=x; x:=u; {Housekeeping}
      fv:=fw; fw:=fx; fx:=fu;
    end
    else
    begin
      If u<x then a:=u else b:=u;
      If (fu<=fw) or (w=x) then
      begin
        v:=w;
        w:=u;
        fv:=fw;
        fw:=fu;
      end
      else if (fu<=fv) or (v=x) or (v=w) then
      begin
        v:=u;
        fv:=fu;
      end;
    end;
  end;

  xmin:=x;
  fmin:=fx;
  
end; {procedure Brent}


procedure DBrent(f,df:TMathFunc; ax,bx,cx,tol:Real; var xmin,fmin:Real);
{Given a function f and its derivative function df, and given a bracketing
triplet of abscissas ax, bx, cx [such that bx is between ax and cx, f(bx) is
less than both f(ax) and f(cx)], this routine isolates the minimum to a
fractional precision of about tol using a modification of Brent's method that
uses derivatives. The abscissa of the minimum is returned as xmin, and the
minimum function value is returned as fmin}
const
  ItMax = 100;
  ZEPS = 1e-10;

  procedure Mov3(var a, b, c:Real; d, e, f:Real);
  begin
    a:=d;
    b:=e;
    c:=f;
  end;

var
  a, b, d, d1, d2, du, dv, dw, dx: Real;
  e: Real; {This will be the distance moved on the step before last}
  fu, fv, fw, fx, olde, tol1, tol2, u, u1, u2, v, w, x, xm: Real;
  ok1, ok2: Boolean; {Will be used as flags for whether proposed steps are acceptable or not}
  iter: Integer;
begin
  e:=0; d:=0;

  If ax<cx then a:=ax else a:=cx; {a and b must be in ascending order, but input abscissas need not be}
  If ax>cx then b:=ax else b:=cx;

  v:=bx; {Initializations}
  w:=v;
  x:=w;
  fx:=f(x);
  fv:=fx;
  fw:=fv;
  dx:=df(x);
  dv:=dx;
  dw:=dv;

  For iter:=1 to ItMax do {Main program loop}
  begin
    xm:=0.5*(a+b);
    tol1:=tol*ABS(x)+ZEPS;
    tol2:=2*tol1;
    If ABS(x-xm)<=(tol2-0.5*(b-a)) then {Test for done here}
    begin
      xmin:=x;
      fmin:=fx;
      exit;
    end;

    If ABS(e)>tol1 then {Construct a trial parabolic fit}
    begin
      d1:=2*(b-a); {Initialize these d's to an out-of-bracket value}
      d2:=d1;
      If dw<>dx then d1:=(w-x)*dx/(dx-dw); {Secant method with one point}
      If dv<>dx then d2:=(v-x)*dx/(dx-dv); {Second point}

      {Which of these two estimates of d shall we take? We will insist that they
      be within the bracket, and on the side pointed to by the derivative at x}
      u1:=x+d1;
      u2:=x+d2;
      If ((a-u1)*(u1-b)>0) and (dx*d1<=0) then ok1:=True else ok1:=False;
      If ((a-u2)*(u2-b)>0) and (dx*d2<=0) then ok2:=True else ok2:=False;
      olde:=e; {Movement on the step before last}
      e:=d;

      If ok1 or ok2 then {Take only an acceptable d, and if both are acceptable, then take the smallest one}
      begin
        If ok1 and ok2 then
          If ABS(d1)<ABS(d2) then d:=d1 else d:=d2
        else if ok1 then d:=d1
        else             d:=d2;

        If ABS(d)<=ABS(0.5*olde) then
        begin
          u:=x+d;
          If (u-a<tol2) or (b-u<tol2) then d:=SIGN(tol1,xm-x);
        end
        else {Bisect, not golden section}
        begin
          If dx>=0 then e:=a-x else e:=b-x; {Decide which segment by the sign of the derivative}
          d:=0.5*e;
        end;
      end
      else
      begin
        If dx>=0 then e:=a-x else e:=b-x;
        d:=0.5*e;
      end;
    end
    else
    begin
      If dx>=0 then e:=a-x else e:=b-x;
      d:=0.5*e;
    end;

    If ABS(d)>=tol1 then
    begin
      u:=x+d;
      fu:=f(u);
    end
    else
    begin
      u:=x+SIGN(tol1,d);
      fu:=f(u);
      If fu>fx then {If the minimum step in the downhill direction takes us uphill, then we are done}
      begin
        xmin:=x;
        fmin:=fx;
        exit;
      end;
    end;

    du:=df(u); {Now all the housekeeping}
    If fu<=fx then
    begin
      If u>=x then a:=x else b:=x;
      Mov3(v, fv, dv, w, fw, dw);
      Mov3(w, fw, dw, x, fx, dx);
      Mov3(x, fx, dx, u, fu, du);
    end
    else
    begin
      If u<x then a:=u else b:=u;
      If (fu<=fw) or (w=x) then
      begin
        Mov3(v, fv, dv, w, fw, dw);
        Mov3(w, fw, dw, u, fu, du);
      end
      else if (fu<fv) or (v=x) or (v=w) then Mov3(v, fv, dv, u, fu, du);
    end;
  end;

end; {procedure DBrent}


procedure LinMin(f:TMathFuncMult; var Xi:TVector; var p:TVector; var fret:Real);
{Given an n-dimensional point p[1..n] and an n-dimensional direction xi[1..n],
moves and resets p to where the function f(p) takes on a minimum along the
direction xi from p, and replaces xi by the actual TVector displacement that p
was moved. Also returns as fret the value of f at the returned location p. This
is actually all accomplished by calling the routines MinimumBracket and Brent}
const
  tol = 2*10e-4; {tolerence passed to Brent}
var
  FuncDim: TMathFunc;
  xx, xmin, fx, fb, fa, bx, ax: Real;
  n,j: Integer;
begin
  n:=P.rowcount;
  Pcom:=TVector.Create(n);
  Xicom:=TVector.Create(n);

  Pcom.Assign(P);
  Xicom.Assign(Xi);
  TVectorFunction:=f;

  ax:=0.0; {initial guess for brackets}
  xx:=0.000001;

  FuncDim:=FDim;
  MinimumBracket(FuncDim,ax,xx,bx,fa,fx,fb);
  Brent(FuncDim,ax,xx,bx,tol,xmin,fret);
  For j:=1 to n do
  begin
    Xi.e[j]:= xmin* Xi.e[j];
    P.e[j]:= P.e[j] + Xi.e[j];
  end;

  pcom.Free;
  xicom.Free;

end; {procedure LinMin}


function FDim(x:Real):Real;
var
  Xt: TVector;
  j,n: Integer;
begin
  n:=Pcom.Rowcount;
  Xt:=TVector.Create(n);
  For j:=1 to n do Xt.e[j]:=Pcom.e[j] + x*Xicom.e[j];
  Result:=TVectorFunction(Xt);
  Xt.Free;
end; {function FDim}


procedure dLinMin(f:TMathFuncMult; df:TMathFuncMultDeriv; var xi:TVector; var p:TVector; var fret:Real);
{Given an n-dimensional point p[1..n] and an n-dimensional direction xi[1..n],
moves and resets p to where the function f(p) takes on a minimum along the
direction xi from p, and replaces xi by the actual TVector displacement that p
was mooved. Also returns as fret the value of f at the returned location p. This
 is actually all accomplished by calling the routines mnbrak and dbrent}
const
  TOL = 2e-4;
var
  FuncDim: TMathFunc;
  dFuncDim: TMathFunc;
  xx, xmin, fx, fb, fa, bx, ax: Real;
  n, j: Integer;
begin
  n:=p.rowcount; {Define the global variables}
  pcom:=TVector.Create(n);
  xicom:=TVector.Create(n);
  TVectorFunction:=f;
  TVectorFunctionDeriv:=df;

  Pcom.Assign(P);
  Xicom.Assign(Xi);

  ax:=0.0; {Initial guess for brackets}
  xx:=1.0;

  FuncDim:=FDim;
  dFuncDim:=dFDim;
  MinimumBracket(FuncDim,ax,xx,bx,fa,fx,fb);
  DBrent(FuncDim,dFuncDim,ax,xx,bx,TOL,xmin,fret);

  For j:=1 to n do
  begin
    Xi.e[j]:=xmin*Xi.e[j];
    P.e[j]:=P.e[j]+ Xi.e[j];
  end;

  pcom.Free;
  xicom.Free;
end; {procedure dLinMin}


function dFDim(x:Real):Real;
var
  Xt,dX: TVector;
  j,n: Integer;
  VectorFunctionDeriv_handler: TVector;
begin
  Result:=0;
  n:=Pcom.Rowcount;
  Xt:=TVector.Create(n);
  dX:=TVector.Create(n);
  For j:=1 to n do Xt.e[j]:=Pcom.e[j] + x*Xicom.e[j];
  VectorFunctionDeriv_handler := TVectorFunctionDeriv(Xt);
  dx.Assign(VectorFunctionDeriv_handler);
  FreeAndNil(VectorFunctionDeriv_handler);
  For j:=1 to n do Result:=Result + dX.e[j]*Xicom.e[j];
  Xt.Free;
  dX.Free;
end; {function FDim}


procedure FletcherReevesPolakRibiere(f:TMathFuncMult; df:TMathFuncMultDeriv; ftol:Real; var p:TVector; var iter:Integer; var fret:Real);
{Given a starting point p[1..n], Fletcher-Reeves-Polak-Ribiere minimization is
performed on a funtion f, using its gradient as calculated by a routine df.
The convergence tolerance on the function value is input as ftol. Returned
quantities are p(the location of the minimum), iter(the number of iterations that
were performed) and fret(the minimum value of the function). The routine dlinmin
is called to perform line minimizations}
const
  ItMax = 200; {maximum allowed number of iterations}
  EPS   = 1e-10; {small number to rectify the special case of converging
                  to exactly zero function value}
var
  g, h, xi, df_handler: TVector;
  gg, gam, fp, dgg: Real;
  j, its, dim: Integer;
begin
  dim:=p.rowcount;
  g:=TVector.Create(dim);
  h:=TVector.Create(dim);
  xi:=TVector.Create(dim);

  fp:=f(p); {Initializations}
  df_handler := df(p);
  xi.Assign(df_handler);
  FreeAndNil(df_handler);

  For j:=1 to dim do
  begin
    g.e[j]:=-xi.e[j];
    h.e[j]:=g.e[j];
    xi.e[j]:=h.e[j];
  end;

  For its:=1 to ItMax do {Loop over iterations}
  begin
    iter:=its;
    dlinmin(f,df,xi,p,fret); {Next statement is the normal termination}
    If (2*ABS(fret-fp)<=ftol*(ABS(fret)+ABS(fp)+EPS)) then break;

    fp:=f(p);
    df_handler := df(p);
    xi.Assign(df_handler);
    FreeAndNil(df_handler);
    gg:=0;
    dgg:=0;
    For j:=1 to dim do
    begin
      gg:=gg+SQR(g.e[j]);
//      dgg:=dgg+SQR(xi.e[j]); {This statement for Fletcher-Reeves}
      dgg:=dgg+(xi.e[j]+g.e[j])*xi.e[j]; {This statement for Polak-Ribiere}
    end;

    If gg=0 then break; {Unlikely. If gradient is exactly zero then we are already done}

    gam:=dgg/gg;

    For j:=1 to dim do
    begin
      g.e[j]:=-xi.e[j];
      h.e[j]:=g.e[j]+gam*h.e[j];
      xi.e[j]:=h.e[j];
    end;

  end;

  g.Free;  h.Free;  xi.Free;

end; {procedure FletcherReevesPolakRibiere}


procedure NelderMead(f:TMathFuncMult; ftol:Real; var p:TMatrix; var y:TVector; var nfunk:Integer);
{Multidimensional minimization of the function f(x) where x[1..ndim] is a TVector
in ndim dimensions, by the downhill simplex method of Nelder and Mead. The
matrix p[1..ndim+1][1..ndim] is input. Its ndim+1 rows are ndim-dimensional
TVectors which are the vertices of the starting simplex. Also input is the TVector
y[1..ndim+1], whose components must be preinitialized to the values of f
evaluated at the ndim+1 vertices (rows) of p, and ftol the fractional
convergence tolerance to be achieved in the function value. On output, p and y
will have been reset to ndim+1 new points all within ftol of a minimum function
value, and nfunk gives the number of function evaluations taken}
const
  NMax = 500; {Maximum allowed number of function evaluations}
var
  psum: TVector;
  rtol, ysave, ytry: Real;
  i, ihi, ilo, inhi, j, mpts, ndim: Integer;
  temp: Real;

  procedure Get_PSum;
  var
    Sum: Real;
    k, l: Integer;
  begin
    For k:=1 to ndim do
    begin
      Sum:=0;
      For l:=1 to mpts do Sum:=Sum+p.e[l,k];
      psum.e[k]:=Sum;
    end;
  end;

  function amotry(fac:Real):Real;
  {Extrapolates by a factor fac through the face of the simplex across from the
  high point, tries it, and replaces the high point if the new point is better}
  var
    ptry: TVector;
    fac1, fac2: Real;
    k: Integer;
  begin
    ptry:=TVector.Create(ndim);
    fac1:=(1-fac)/ndim;
    fac2:=fac1-fac;
    For k:=1 to ndim do ptry.e[k]:=psum.e[k]*fac1-p.e[ihi,k]*fac2;
    Result:=f(ptry); {Evaluate the function at the trial point}
    If Result<y.e[ihi] then {If it's better than the highest, then replace the highest}
    begin
      y.e[ihi]:=Result;
      For k:=1 to ndim do
      begin
        psum.e[k]:=psum.e[k]+ptry.e[k]-p.e[ihi,k];
        p.e[ihi,k]:=ptry.e[k];
      end;
    end;

    ptry.Free;
  end;

begin
  ndim:=p.rowcount-1;
  mpts:=ndim+1;

  psum:=TVector.Create(ndim);
  nfunk:=0;
  Get_PSum;

  While True do
  begin
    ilo:=1;

    {First we must determine which point is the highest (worst), next-highest,
    and lowest (best), by looping over the points in the simplex}
    If y.e[1]>y.e[2] then
    begin
      inhi:=2;
      ihi:=1;
    end
    else
    begin
      inhi:=1;
      ihi:=2;
    end;
    For i:=1 to mpts do
    begin
      If y.e[i]<=y.e[ilo] then ilo:=i;
      If y.e[i]>y.e[ihi] then
      begin
        inhi:=ihi;
        ihi:=i;
      end
      else if (y.e[i]>y.e[inhi]) and (i<>ihi) then inhi:=i;
    end;

    rtol:=2*ABS(y.e[ihi]-y.e[ilo])/(ABS(y.e[ihi])+ABS(y.e[ilo]));
    {Compute the fractional range from highest to lowest and return if satisfactory}
    If rtol<ftol then
    begin
      temp:=y.e[ilo];
      y.e[ilo]:=y.e[1];
      y.e[1]:=temp;
      For i:=1 to ndim do
      begin
        temp:=p.e[ilo,i];
        p.e[ilo,i]:=p.e[1,i];
        p.e[1,i]:=temp;
      end;
      break;
    end;

//    If nfunk>=NMax then ShowMessage('NMax exceeded');
    nfunk:=nfunk+2;
    {Begin a new iteration. First extrapolate by a factor -1 through the face of
     the simplex across from the high point, i.e. reflect the simplex from the
     high point}
    ytry:=amotry(-1);

    (* Note: instead of the line "if (...) and (...) then" below, formerly the
    code had this:
      If ytry<=y.e[ilo] then ytry:=amotry(2) {Gives a result better than the best point, so try an additional extrapolation by a factor 2}
      else if ytry>=y.e[inhi] then
    The compiler was correctly hinting that ytry is assigned but never used.
    I fixed the code like it is now so that the compiler shows no hint, however
    obviously something is wrong in the code.
    A.X., 2014-10-23
    *)

    if (ytry > y.E[ilo]) and (ytry >= y.e[inhi]) then
    begin
      {The reflected point is worse than the second-highest, so look for an
      intermediate lower point, i.e. do a one-dimensional contraction}
      ysave:=y.e[ihi];
      ytry:=amotry(0.5);
      If ytry>=ysave then {Can't seem to get rid of that high point. Better contract around the lowest (best) point}
      begin
        For i:=1 to mpts do
          If i<>ilo then
          begin
            For j:=1 to ndim do
            begin
              psum.e[j]:=0.5*(p.e[i,j]+p.e[ilo,j]);
              p.e[i,j]:=psum.e[j];
            end;
            y.e[i]:=f(psum);
          end;

        nfunk:=nfunk+ndim; {Keep track of function evaluations}
        Get_PSum; {Recompute psum}
      end;
    end
    else nfunk:=nfunk-1; {Correct the evaluation count}
  end;

  psum.Free;

end; {procedure NelderMead}




{*******************************************************************************
 *******************************************************************************
                   =========================================
                   THE EVOLUTIONARY ANNEALING-SIMPLEX METHOD
                   =========================================
********************************************************************************
********************************************************************************}
procedure AnnealSimplex(n, m: Integer; xmin, xmax: TArrayOfReal;
                        var xopt: TArrayOfReal; F: TMathFuncMult1;
                        var fopt: Real; var eval: Integer;  ftol: Real; maxeval: Integer;
                        ratio, pmut, beta: Real; maxclimbs: Integer; iniflg, restart: Boolean;
                        var StopOptim: Boolean);

{*******************************************************************************
Multidimensional minimization of the function F(x), where x[1..n] is a vector in
n dimensions, by simulated annealing combined with the downhill simplex method
of Nelder and Mead. Input arguments are:

n           = problem dimension
m           = population count (m > n + 1)
xmin[n]     = lower parameter bounds
xmax[n]     = upper parameter bounds
xopt[n]     = vector containing the optimal values of control variables
F           = objective function
fopt        = the minimal value of the objective function
eval        = number of function evaluations
ftol        = the fractional convergence tolerance to be achieved in the
              function value for an early return
maxeval     = maximum number of function evaluations
ratio       = fraction of temperature reduction, when a local minimum is found
beta        = annealing schedule parameter
pmut        = probability of accepting an offspring generated via mutation
maxclimbs   = maximum number of uphill steps
iniflg      = if true then xopt is one of the starting points
restart     = if true then the search procedure is repeated
*******************************************************************************}

var
  pop: T2DArrayOfReal; {population of m points}
  fpop: TArrayOfReal; {population fitness}
  simplex: T2DArrayOfReal; {sub-population of n+1 points - simplex vertices}
  fsimplex: TArrayOfReal; {simplex fitness}
  pstart, pref, ptry, pcent: TArrayOfReal; {temp arrays}
  xmean, xstdev: TArrayOfReal; {arrays of population statistics}
  ipos: TIntArray; {index array}

  fmin, fmax: Real; {minimum and maximum criteria values}
  fmean, fstdev: Real; {statistics}

  temperature: Real; {control parameter}

  imin, imax, lpos: Integer;
  i, ihi, ilo, istart, j, step: Integer;
  rtol, sum, yhi, ylo, ystart, ytry, yref, ylast, fac, maxdist: Real;

  BoundReached: Boolean;
  BestStep: Integer;
  RejectPoint: Boolean;

  label 100, 200, 999;

begin
  {Check for consistency of input arguments}
  If (m<n+1) then m:=n+1;

  {Create arrays}
  SetLength(pop, m, n);
  SetLength(fpop, m);
  SetLength(simplex, n+1, n);
  SetLength(fsimplex, n+1);
  SetLength(xmean, n);
  SetLength(xstdev, n);
  SetLength(ptry, n);
  SetLength(pcent, n);
  SetLength(pstart, n);
  SetLength(pref, n);
  SetLength(ipos, n+1);

  eval:=0;
  fopt:=1e100;

  { The value assigned to imin here is probably never used, but the assignment
  was added because otherwise the compiler shows a warning that it might be
  used without having been initialized. The warning now does not show, but
  the code is really in need of overhaul. A.X., 2014-10-23. }
  imin := 0;

100:
  fmin:=1e100;
  fmax:=-fmin;

  {Initialization}
  If iniflg then
  begin
    For j:=0 to n-1 do pop[0,j]:=xopt[j];
    fpop[0]:=F(xopt);
    fmin:=fpop[0]; fmax:=fpop[0];
    i:=1;
  end
  else i:=0;

  Repeat
    For j:=0 to n-1 do ptry[j]:=xmin[j]+random*(xmax[j]-xmin[j]);
    ytry:=F(ptry);
    For j:=0 to n-1 do pop[i,j]:=ptry[j];
    fpop[i]:=ytry;
    If ytry<fmin then fmin:=ytry;
    If ytry>fmax then fmax:=ytry;
    i:=i+1;
  until (i=m);

  temperature:=fmax-fmin;
  eval:=eval+m;

  {*****************}
  {*** Main loop ***}
  {*****************}

  Repeat

    //If StopOptim then Break;
    If StopOptim then goto 999;

    {Compute the statistics of the population}
    For j:=0 to n-1 do
    begin
      sum:=0;
      For i:=0 to m-1 do sum:=sum+pop[i,j];
      xmean[j]:=sum/m;
      sum:=0;
      For i:=0 to m-1 do sum:=sum+(pop[i,j]-xmean[j])*(pop[i,j]-xmean[j]);
      xstdev[j]:=sqrt(sum/m);
    end;
    sum:=0;
    For i:=0 to m-1 do sum:=sum+fpop[i];
    fmean:=sum/m;
    sum:=0;
    For i:=0 to m-1 do sum:=sum+(fpop[i]-fmean)*(fpop[i]-fmean);
    fstdev:=sqrt(sum/m);

    {Generate a simplex, selecting its vertices randomly from the actual
     population}
    If (m=n+1) then for i:=0 to m-1 do ipos[i]:=i
    else
    begin
      ipos[0]:=Round(random*(m-1));
      For i:=1 to n do
      begin
        200: lpos:=Round(random*(m-1));
        For j:=0 to i do if lpos=ipos[j] then goto 200;
        ipos[i]:=lpos;
      end;
    end;

    {Assign the coordinates and the function value to each vertex}
    For i:=0 to n do
    begin
      lpos:=ipos[i];
      For j:=0 to n-1 do simplex[i,j]:=pop[lpos,j];
      fsimplex[i]:=fpop[lpos];
    end;

    {Determine the highest (worst) and the lowest (best) point, as well as the
     randomized worst point, according to the criterio xw = max f(x) + rnd*T}
    ilo:=0; ihi:=0; istart:=0;
    For i:=1 to n do if fsimplex[i]<fsimplex[ilo] then ilo:=i else
                     if fsimplex[i]>fsimplex[ihi] then ihi:=i;
    yhi:=fsimplex[ihi]; ylo:=fsimplex[ilo];

    If temperature>(beta*(yhi-ylo)) then temperature:=beta*(yhi-ylo);

    ystart:=ylo;
    For i:=0 to n do
    begin
      ytry:=fsimplex[i]+random*temperature;
      If (ytry>ystart) and (i<>ilo) then
      begin
        istart:=i;
        ystart:=ytry;
      end;
    end;
    ystart:=fsimplex[istart];

    {Store the randomized worst vertex}
    For i:=0 to n-1 do pstart[i]:=simplex[istart,i];

    {Compute the centroid of the simplex}
    For i:=0 to n-1 do
    begin
      sum:=0;
      For j:=0 to n do sum:=sum+simplex[j,i];
      pcent[i]:=(sum-pstart[i])/n;
    end;

    {Make a reflection step}
    fac:=0.5+random;
    NewPoint(n, eval, 1+fac, -fac, F, pref, pcent, pstart, xmin, xmax, yref, BoundReached);
    If StopOptim then goto 999;

    {If the reflection point is either not accepted (no move) or fr<fw (downhill
     move) the method follows the modified (quasi-stochastic) Nelder-Mead
     procedure, making contraction and expansion moves respectively}

    If yref<ystart then
    begin

      {Accept the reflection point}
      For i:=0 to n-1 do simplex[istart,i]:=pref[i];
      fsimplex[istart]:=yref;
      lpos:=ipos[istart];
      For i:=0 to n-1 do pop[lpos,i]:=pref[i];
      fpop[lpos]:=yref;

      {If the reflected point is better than the lowest, try a line minimization
       employing subsequent random expansion steps, else try an outside contraction
       step between xc and xr}
      If (yref<ylo) and (not BoundReached) then
      {Multiple expansion}
      Repeat
        fac:=fac+0.5+random;
        NewPoint(n, eval, 1+fac, -fac, F, ptry, pcent, pstart, xmin, xmax, ytry, BoundReached);
        If StopOptim then goto 999;
        If ytry<fsimplex[istart] then
        begin
          fsimplex[istart]:=ytry;
          For j:=0 to n-1 do simplex[istart,j]:=ptry[j];
          lpos:=ipos[istart];
          For j:=0 to n-1 do pop[lpos,j]:=ptry[j];
          fpop[lpos]:=ytry;
        end;
      until (ytry>yref) or BoundReached
      else
      {Outside contraction}
      begin
        fac:=random*fac;
        NewPoint(n, eval, 1+fac, -fac, F, ptry, pcent, pstart, xmin, xmax, ytry, BoundReached);
        If StopOptim then goto 999;
        If ytry<fsimplex[istart] then
        begin
          fsimplex[istart]:=ytry;
          For j:=0 to n-1 do simplex[istart,j]:=ptry[j];
          lpos:=ipos[istart];
          For j:=0 to n-1 do pop[lpos,j]:=ptry[j];
          fpop[lpos]:=ytry;
        end;
      end; {outside contraction step}

    end
    else if (yref-random*temperature)>(ystart+random*temperature)
    then
    begin

      {Don't accept the reflection step and try an inside contraction step}
      fac:=random;
      NewPoint(n, eval, fac, 1-fac, F, ptry, pcent, pstart, xmin, xmax, ytry, BoundReached);
      If StopOptim then goto 999;
      If ytry<fsimplex[istart] then
      begin
        fsimplex[istart]:=ytry;
        For j:=0 to n-1 do simplex[istart,j]:=ptry[j];
        lpos:=ipos[istart];
        For j:=0 to n-1 do pop[lpos,j]:=ptry[j];
        fpop[lpos]:=ytry;
      end;

      {Reduce the temperature}
      temperature:=ratio*temperature;

      {Multiple contraction step}
      If ytry>ystart then
      begin
        For i:=0 to n-1 do pstart[i]:=simplex[ilo,i]; {store the coordinates of the best vertex}
        For i:=0 to n do if i<>ilo then
        begin
          For j:=0 to n-1 do ptry[j]:=simplex[i,j]; {store the coordinates of the ith vertex}
          fac:=0.5;
          NewPoint(n, eval, fac, fac, F, ptry, ptry, pstart, xmin, xmax, ytry, BoundReached);
          If StopOptim then goto 999;
          lpos:=ipos[i];
          For j:=0 to n-1 do pop[lpos,j]:=ptry[j];
          fpop[lpos]:=ytry;
        end;
      end;
    end
    else
    begin

      {Accept the reflection point}
      For j:=0 to n-1 do simplex[istart,j]:=pref[j];
      fsimplex[istart]:=yref;
      lpos:=ipos[istart];
      For j:=0 to n-1 do pop[lpos,j]:=pref[j];
      fpop[lpos]:=yref;

      {Store the reflection point to the temporary vector pstart to use it for
       the computation of the direction xr-xc}
      For i:=0 to n-1 do pstart[i]:=pref[i];

      {Try some random uphill steps along the reflection direction and store the
       best of them; if you observe a hill climbing, replace the reflection point.
       Whenever you reach the bounds, exit from the search procedure}
      yref:=1e20;
      BoundReached:=False;
      step:=0; fac:=1;
      ylast:=yref;
      Repeat
        step:=step+1;
        fac:=fac+0.5+random;
        NewPoint(n, eval, 1+fac, -fac, F, ptry, pcent, pstart, xmin, xmax, ytry, BoundReached);
        If StopOptim then goto 999;
        If ytry<ylast then
        begin
          BestStep:=step;
          yref:=ytry;
          For j:=0 to n-1 do pref[j]:=ptry[j];
        end
        else ylast:=ytry;
      until (step=maxclimbs) or BoundReached;

      {If any hill climbing occurs, try a mutation step by generating a random
       point out of the range (xmean-xstdev, xmean+xstdev)}
      If (BestStep>1) or (yref<fsimplex[istart]) then
      begin
        For j:=0 to n-1 do simplex[istart,j]:=pref[j];
        fsimplex[istart]:=yref;
        For j:=0 to n-1 do pop[lpos,j]:=pref[j];
        fpop[lpos]:=yref;
      end
      else
      begin
        {Mutation}
        For i:=0 to n-1 do
        begin
          If random>0.5 then
          begin
            maxdist:=xmax[i]-xmean[i]-xstdev[i];
            If maxdist<0 then maxdist:=0;
            ptry[i]:=xmean[i]+xstdev[i]+random*maxdist;
          end
          else
          begin
            maxdist:=xmean[i]-xstdev[i]-xmin[i];
            If maxdist<0 then maxdist:=0;
            ptry[i]:=xmean[i]-xstdev[i]-random*maxdist;
          end;
          if ptry[i]>xmax[i] then ptry[i] := xmax[i];
          if ptry[i]<xmin[i] then ptry[i] := xmin[i];
        end;
        ytry:=F(ptry);
        eval:=eval+1;
        If (ytry<fsimplex[istart]) or (random<pmut) then
        begin
          fsimplex[istart]:=ytry;
          For j:=0 to n-1 do simplex[istart,j]:=ptry[j];
          For j:=0 to n-1 do pop[lpos,j]:=ptry[j];
          fpop[lpos]:=ytry;
        end;
      end;

    end;

    {Determine the best and worst point into the population}
    imin:=0; imax:=0;
    For i:=1 to m-1 do
    begin
      If fpop[i]<fpop[imin] then imin:=i else
      If fpop[i]>fpop[imax] then imax:=i;
    end;

    fmin:=fpop[imin];
    fmax:=fpop[imax];

    {Check convergence criteria}
    If restart then
    begin
      try rtol:=2*ABS(fmax-fmin)/(ABS(fmax)+ABS(fmin))
      except rtol:=2*ABS(fmax-fmin)
      end;
      If rtol<ftol then Break;
    end;

  until (eval>=maxeval);

999:
  {Save the optimal solution}
  For i:=0 to n-1 do xopt[i]:=pop[imin,i];
  fopt:=F(xopt);

  {Destoy all arrays}
  pop:=nil; fpop:=nil; simplex:=nil; fsimplex:=nil; pstart:=nil; pref:=nil;
  ptry:=nil; pcent:=nil; xmean:=nil; xstdev:=nil; ipos:=nil;

end; {procedure AnnealSimplex}


procedure NewPoint(num_var: Integer; var eval: Integer; a1, a2: Real; F: TMathFuncMult1;
                   var x, x1, x2, xmin, xmax: TArrayOfReal; var fx: Real; var boundary: Boolean);
{Generates a feasible point according to the formula x[i] = a1*x1[i] + a2*x2[i]}
var
  i: Integer;
begin
  boundary:=False;
  For i:=0 to num_var-1 do
  begin
    x[i]:=a1*x1[i]+a2*x2[i];
    If x[i]<xmin[i] then
    begin
      x[i]:=xmin[i]; boundary:=True;
    end
    else if x[i]>xmax[i] then
    begin
      x[i]:=xmax[i]; boundary:=True;
    end;
  end;
  fx:=F(x);
  eval:=eval+1;
end; {procedure NewPoint}


end.
