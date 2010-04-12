{******************************************************************}
{                                                                  }
{  Thelma library                                                    }
{                                                                  }
{  Copyright (c) 2000-04 National Technical University of Athens   }
{                                                                  }
{******************************************************************}

{** random numbers generators }
unit rnd;

interface

uses math;

{** Set the RandSeed system variable with value
}
procedure SetRandSeed (value : longint);

{** Returns a random number described by the uniform distribution.
    Set the desired bounds: low and high.
}
function urnd (low, high : real) : real;

{** Triangular distribution random generator.<p>
    Returns a random number described by the triangular distribution.
    Triangular distribution probability density function:<p>
    f(x) = 2 * x / xmax^2 where xmax is the upper limit of the
    random variable x. xmax should be greater than 0 (xmax > 0) or
    else an exception is raised.
}
function trnd(xmax : real) : real;

{** Power distribution random generator.<p>
    Returns a random number described by the power distrubtion.
    Power distribution probability density function:<p>
    f(x) =  x^a, 0 < = x < = 1 where a is the exponent and
    it should be greater tha 0 or else an exception is raised.
}
function rrnd(exponent : real) : real;

{** Exponential distribution random generator.<p>
    Returns a random number described by the exponential distrubtion
    of one parameter.
    Exponential distribution probability density function is:<p>
    f(x) = lambda * EXP(-lambda*x)<p>
    where lambda is the scale parameter and it should be greater
    than zero (lambda > 0 ) or else an exception is raised. 
}
function ernd(lamda : real) : real;

{** Normal distribution random generator.<p>
    Function is based on a method introduced by Box and Muller (1958).
    See: Kottegoda, N.T., "Stochastic Water Resources Technology".
    Normal distribution probability density function is:<p>
    f(x) = [1 / (s*sqrt(2pi))] * EXP[- (x - m) ^ 2 / (2s ^ 2)]<p>
    where, m is the mean value and s the standard deviation.
}
function nrnd(mi, sigma : real) : real;

{** LogNormal distribution random generator.<p>
    @SeeAlso <See Routine=nrnd>
}
function lnrnd(mi, sigma : real) : real;

{** Gamma distribution random generator.<p>
    grnd is a branch function with the cases bellow:<p>
    -If kapa > = GaussLimit, use normal distribution random generator
     with mi=kapa/lamda, s= sqrt(kapa)/lamda<p>
    -If kapa < IntLimit and kapa is integer, use algorithm for integer
     kapa<p>
    -In the other cases, use Cheng Feast algorithm for kapa > 1 and
     Ahrens Dieter algorithm for k < 1.<p>
     GaussLimit and IntLimit are set within the code as:<p>
     GaussLimit = 100;<p>
     IntLimit = 25;<p>
     Gamma random number generator due to Ahrens and Dieter, 1974
     for pdf:  f(x) = lamda^kapa * x^(kapa-1) * EXP(-lamda*x) / Gamma(kapa)
     good for k < 1<p>
     see Ripley, 1987, p. 88<p>
     Gamma random number generator due to Cheng and Feast, 1979<p>
     for pdf:  f(x) = lamda^kapa * x^(kapa-1) * EXP(-lamda*x) / Gamma(kapa)
     good for k > 1<p>
     see Ripley, 1987, p. 90<p>
    @SeeAlso <See Routine=gmsrnd>
}
function grnd(kapa,lamda : real) : real;

{** Gamma distribution random generator (obsolete).<p>
    Based on a method introduced by Whittaker (1973).
    See: Haan C.T., "Statistical Methods in Hydrology".<p>
    Function is obsolete, use grnd instead.
    @SeeAlso <See Routine=grnd>
}
function grndOld(kapa,lamda : real) : real;

{** gmsrnd := grnd(sqr(mi/sigma), mi/sqr(sigma))<p>
    This version of grnd is using mean value (m) and standard deviation (s)
    instead of kapa and lamda parameters.
    @SeeAlso <See Routine=grnd>
}
function gmsrnd(mi,sigma : real) : real;

{** 3p Gamma distribution (Pearson III) random generator.<p>
    This function is using mean value (mi), standard deviation (sigma)
    and skewness (skew) as parameters.
    @SeeAlso <See Routine=grnd>
}
function gmssrnd(mi,sigma, skew : real) : real;

{** Beta distribution random generator<p>
    Probability density function is:
    f(x) = x^(mi-1) * (1-x)^(ni-1) * Gamma(mi+ni) / [Gamma(mi)*Gamma(ni)]  , 0 <= x <= 1 <p>
    Where mi and in parameters are greater than zero.
}
function brnd (mi, ni : real) : real;

{** Poisson distribution random generator.<p>
    Probability density function is:<p>
    p(x) = lamda^x * EXP(-lamda) / x!<p>
    where, lamda parameter greater than zero.
}
function prnd(lamda : real) : integer;

{** Weibull distribution random generator.<p>
    cumulative distribution function is:<p>
    F(x) = 1 - exp (x/b) ^ c<p>
    where b, c = parameters ( > 0) <p>
}
function wrnd (b, c : real) : real;

{** chi2rnd := grnd(n/2, 2)
    @SeeAlso <See Routine=grnd>
}
function chi2rnd( n : integer) : real;

{** Gumbel max distribution random generator.<p>
    gumbelrnd := x0 - ln(-ln (1 - random)) /a
}
function gumbelrnd (a, x0 : real) : real;

{** Gumbel max distribution random generator by mean value and
    standard deviation.<p>
    gumbelmsrnd := gumbelrnd (1.2825/sigma, mi - 0.4501 * sigma);
    @SeeAlso <See Routine=gumbelrnd>
}
function gumbelmsrnd (mi, sigma : real) : real;

{** Pareto distrubtion random generator.
}
function Paretomsrnd (mi, sigma, kapa : real) : real;

{** GEV (General Extreme Values) distribution random generator.
}
function GEVrnd (kapa, lamda, psi: real): real;

implementation

uses sysutils;

{$ifopt D+}
{$define debug}
{$endif}

{Συνάρτηση παραγωγής τυχαίων αριθμών ομοιόμορφης κατανομής
  (uniform distribution random generator) }
{Α. Στο διάστημα [0, 1)}
{function random : real; external;}
{Β. Στο διάστημα [0, range)}
var
  IsReady : boolean;
  ReadyNrnd : real;

{function random [ ( range : word) ] : word; external;}

{function random : real;
  begin
  random := rand.rnd;
  end;}

function urnd (low, high : real) : real;
  begin
  urnd := low + random * (high - low);
  end;

procedure SetRandSeed (value : longint);
  begin
  RandSeed := value;
  end;

resourcestring
  rsInvalidTriangularParameter = 'Triangular distribution parameter <=0';

function trnd(xmax : real) : real;
  begin
  if (xmax <= 0) then
    raise Exception.Create(rsInvalidTriangularParameter);
  trnd := sqrt(random) * xmax;
  end;

resourcestring
  rsInvalidPowerParameter = 'Power distribution parameter <=0';

function rrnd(exponent : real) : real;
  begin
  if (exponent <= 0) then
    raise Exception.Create(rsInvalidPowerParameter);
  rrnd := power(random, 1 / exponent);
  end;

resourcestring
  rsInvalidExponentialParameter = 'Exponential distribution parameter <= 0';

function ernd(lamda : real) : real;
  begin
  if (lamda <= 0) then
    Exception.Create(rsInvalidExponentialParameter);
  ernd := -ln(random) / lamda;
  end;

function nrnd(mi, sigma : real) : real;
  var
    z, f, nrnd1 : real;
  begin
  if IsReady then nrnd := sigma * ReadyNrnd + mi
  else
    begin
    z := sqrt(-2*ln(random));
    f := 2 * pi * random;
    nrnd1 := z * cos(f);
    ReadyNrnd := z * sin(f);
    nrnd := sigma * nrnd1 + mi;
    end;
  IsReady := not IsReady;
  end;

function lnrnd(mi, sigma : real) : real;
  var bdiv2 : real;
  begin
  bdiv2 := ln (1 + sqr (sigma / mi));
  lnrnd := exp (nrnd (ln (mi) - bdiv2/2.0, sqrt(bdiv2)));
  end;

{ Συνάρτηση παραγωγής τυχαίων αριθμών κατανομής Γάμα
  (gamma distribution random generator) }
{ Η συνάρτηση βασίζεται σε μια μέθοδο που προτάθηκε από τον Whittaker (1973).
  Βλέπε : Haan C.T.,"Statistical Methods in Hydrology".

  Η συνάρτηση πυκνότητας πιθανότητας της κατανομής Γάμα είναι:
  f(x) = λ^κ * x^(κ-1) * EXP(-λ*x) / Γ(κ)
  όπου λ = παράμετρος κλίμακας και κ = παράμετρος σχήματος (κ > 0) }

resourcestring
  rsInvalidGammaParameters = 'Gamma distribution parameters <= 0';

function grndOld(kapa,lamda : real) : real;
  const
    acc = 1e-5;
    GaussLimit = 100;
  var
    k, i :integer;
    n, rnd1, rnd2, s1, s2, s, product : real;

  begin
  if (kapa <= 0) then
    raise Exception.Create(rsInvalidGammaParameters);
  if kapa > GaussLimit then result := nrnd(kapa/lamda, sqrt(kapa)/lamda)
  else
    begin
    k := trunc(kapa); n := kapa-k;
    if n >= 1 - acc then
      begin
      k := k + 1;
      rnd1 := 0;
      end
    else
      if  n <= acc then rnd1 := 0
      else
        begin
        repeat
          s1 := power(random, 1/n);
          s2 := power(random, 1/(1-n));
          s := s1+s2;
          until (s<=1) and (s > 0);
        rnd1 := -(s1/s) * ln(random);
        end;
    if k = 0 then
      rnd2 := 0
    else
      begin
      product := 1;
      for i := 1 to k do  product := product * random;
      rnd2 := -ln (product);
      end;
    result := (rnd1 + rnd2) / lamda;
    end;
  end;
{Gamma random number generator due to Ahrens and Dieter, 1974
 for pdf:  f(x) = lamda^kapa * x^(kapa-1) * EXP(-lamda*x) / Γ(kapa)
 good for k < 1
 see Ripley, 1987, p. 88}

function grndAhrensDieter(kapa, lamda: double): double;
  const e =2.718281828;
  var u0, u1, x, ul: double;
  begin
  assert ((kapa > 0) and (kapa <= 1));
  repeat
    u0 := random; u1 := random;
    if u0 > e / (e + kapa) then
      begin
      x := -ln((kapa + e) * (1 - u0)/ (kapa * e));
      ul := power(x, kapa - 1)
      end
    else
      begin
      x := power((kapa + e) * u0 / e, 1 / kapa);
      ul := exp(-x)
      end
    until u1 <= ul;
  result := x / lamda;
  end;

{Gamma random number generator due to Cheng and Feast, 1979
 for pdf:  f(x) = lamda^kapa * x^(kapa-1) * EXP(-lamda*x) / Γ(kapa)
 good for k > 1
 see Ripley, 1987, p. 90}
function grndChengFeast(kapa, lamda: double): double;
  var
    c1, c2, c3, c4, c5, u1, u2, w: double;
  begin
  assert(kapa > 1);
  c1 := kapa - 1; c2 := (kapa - 1/6/kapa)/c1; c3 := 2/c1;
  c4 := c3 + 2; c5 := 1 / sqrt(kapa);
  repeat
    repeat
      u1 := random; u2 := random;
      if kapa > 2.5 then u1 := u2 + c5 * (1 - 1.86*u1);
      until (u1 > 0) and (u1 < 1);
    w := c2 * u2 / u1;
    until (c3 * u1 + w + 1 / w <= c4) or (c3 * ln (u1) - ln (w) + w <= 1);
  result := c1 * w / lamda;
  end;

function IntGrnd (kapa: integer; lamda: double): double;
  var product: double; i: integer;
  begin
  product := 1;
  for i := 1 to kapa do  product := product * random;
  result := -ln (product) / lamda;
  end;

function grnd;
  const
    Tol = 1e-5;
    GaussLimit = 100;
    IntLimit = 25;
  begin
  assert(kapa > 0);
  if kapa > GaussLimit then result := nrnd(kapa/lamda, sqrt(kapa)/lamda)
  else if (kapa < IntLimit) and (abs(kapa - round(kapa)) < 1e-5) then
    result := IntGrnd(round(kapa), lamda)
  else if kapa > 1 then result := grndChengFeast(kapa, lamda)
  else result := grndAhrensDieter(kapa, lamda);
  end;

function gmsrnd(mi, sigma : real) : real;
  begin
  gmsrnd := grnd(sqr(mi/sigma), mi/sqr(sigma));
  end;

function gmssrnd(mi, sigma, skew : real) : real;
  var
    kapa, lamda, c: double;
  begin
  if skew < 0 then
    result := -gmssrnd(-mi, sigma, -skew)
  else
    begin
    kapa := 4/sqr(skew); lamda := sqrt(kapa)/sigma; c := mi - kapa / lamda;
{  if skew < 0 then c := -c;}
    result := grnd(kapa, lamda) + c;
{  if skew < 0 then result := - result;}
    end;
  end;

{ Συνάρτηση παραγωγής τυχαίων αριθμών κατανομής Βήτα
  (Beta distribution random generator) }
{ Η συνάρτηση πυκνότητας πιθανότητας της κατανομής είναι :
  f(x) = x^(μ-1) * (1-x)^(ν-1) * Γ(μ+ν) / [Γ(μ)*Γ(ν)]  , 0 <= χ <= 1
  όπου μ και ν = παράμετροι (και οι δύο > 0) }
function brnd (mi, ni : real) : real;
  var
    rnd1, rnd2 : real;
  begin
  rnd1 := grnd (mi, 1.0);
  rnd2 := grnd (ni, 1.0);
  if rnd1 + rnd2 = 0.0 then brnd := brnd (mi, ni)
  else brnd := rnd1 / (rnd1 + rnd2);
  end;

{ Συνάρτηση παραγωγής τυχαίων αριθμών κατανομής Poisson
  (Poisson distribution random generator) }

{ Η συνάρτηση πιθανότητας της κατανομής  είναι:
  p(x) = λ^x * EXP(-λ) / x!
  όπου λ = παράμετρος ( > 0) }

resourcestring
  rsInvalidPoissonParameter = 'Poisson distribution parameter <= 0';

function prnd(lamda : real) : integer;
  var
    k   : integer;
    product {, explamda} : extended;
  begin
  if (lamda <= 0) then
    raise Exception.Create(rsInvalidPoissonParameter);
  k:= 0;
{  product := 1.0;
  explamda := exp (-lamda);
  repeat
    product := product * random;
    k := k + 1;
    until product < explamda;}
  product := 0;
//  explamda := exp (-lamda);
  repeat
    product := product - ln(random);
    k := k + 1;
    until product > lamda;
  prnd := k - 1;
  end;

{ Συνάρτηση παραγωγής τυχαίων αριθμών κατανομής Weibull
  (Weibull distribution random generator) }

{ Η συνάρτηση κατανομής  είναι:
  F(x) = 1 - exp (x/b)^c
  όπου b, c = παράμετροι ( > 0) }

function wrnd (b, c : real) : real;
  begin
  wrnd := b * power(-ln(random), 1/c)
  end;

function chi2rnd( n : integer) : real;
  begin
  chi2rnd := grnd(n/2, 2);
  end;

function gumbelrnd (a, x0 : real) : real;
  begin
  gumbelrnd := x0 - ln(-ln (1 - random)) /a;
  end;

function gumbelmsrnd (mi, sigma : real) : real;
  begin
  gumbelmsrnd := gumbelrnd (1.2825/sigma, mi - 0.4501 * sigma);
  end;

resourcestring
  rsInvalidParetoParameter = 'Pareto distribution with kapa > 0.5';

function Paretomsrnd (mi, sigma, kapa : real) : real;
  var lamda, psi : real;
  begin
  if kapa > 0.5 then
    raise Exception.Create(rsInvalidParetoParameter);
  lamda := sigma * sqrt (1 - 2 * kapa) * (1 - kapa);
  psi := (mi/lamda) - 1 /(1 - kapa);
  Paretomsrnd := (lamda/kapa)
    * (power(1 - random, -kapa) + kapa * psi - 1);
  end;

function GEVrnd;
  const
    small = 1e-6;
  begin
  if abs(kapa) < small then result := Gumbelrnd(1/lamda, psi * lamda)
  else result := lamda *(psi + ((power(-ln(random), -kapa) - 1) / kapa));
  end;


begin
IsReady := false;
end.

