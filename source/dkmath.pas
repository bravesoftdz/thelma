{******************************************************************}
{                                                                  }
{  Thelma library                                                  }
{                                                                  }
{  Copyright (c) 2000-04 National Technical University of Athens   }
{                                                                  }
{******************************************************************

{** Some math functions not included in the standard
    math library.
}
unit DKMath;

interface

uses sysutils;
{** Raises a Real base to an interger exponential.
}
function IntRaise (base: real; power: integer): real;
{** This function is obsolete, use standard Power function instead.
}
function RealRaise (base, power: real): real;
{** Sign returns -1 if Value<0, 1 if Value>0 and 0 if Value=0
}
function Sign(Value: Real): Real;
{** Gamma returns the gamma function of a real number and its defined by
    the integral of Integral(y^(a-1)*e^(-y)dy,0,+inf). Some properties of
    Gamma function: Gamma(a+1)=aGamma(a), Gamma(1/2)=sqrt(pi),
    Gamma(n+1)=n! (n is an integer) etc.
}
function Gamma(arg : real) : real;
{** Beta(x,y) returns the Beta function defined by:
    Beta(x,y) = Gamma(x)Gamma(y)/Gamma(x+y).
}
function Beta (x, y: real) : real;
{** Erf(x) returns the Error function of x, defined by the integral
    of (1/sqrt(pi))*Exp(-t^2) from 0 to +inf. Erf is used to calculate the
    cumulative distribution function of the normal distribution.
    @SeeAlso <See Routine=Erfc>
}
function Erf (x : real) : real;
{** Erfc(x) = 1 - Erf(x)
    @SeeAlso <See Routine=Erf>
}
function Erfc (x : real) : real;

implementation

uses math;

function Sign(Value: Real):Real;
begin
  if Value <0 then
    Result := -1 else
    Result := 1;
  if Value = 0 then
    Result := 0;
end;

{'Υψωση σε δύναμη με ακέραιο εκθέτη}
function IntRaise (base: real; power: integer): real;
  begin
  if power = 0 then IntRaise := 1.0
  else if (base = 0) and (power > 0) then IntRaise := 0.0
  else if power < 0 then IntRaise := 1 / IntRaise (base, -power)
  else if odd(power) then IntRaise := base * IntRaise (base, power - 1)
  else IntRaise := sqr(IntRaise (base, power div 2))
  end;

{'Υψωση σε δύναμη με πραγματικό εκθέτη}
function RealRaise (base, power: real): real;
const
  low = -1400;

 var t: real;
  begin
  if power = 0 then RealRaise := 1.0
  else if base = 0 then RealRaise := 0.0
  else if power < 0 then RealRaise := 1 / RealRaise (base, -power)
  else
    begin
    t := power * ln(base);
    if t < low then RealRaise := 0.0
    else RealRaise := exp(t)
    end
  end;

resourcestring
  rsInvalidGammaArgument = 'Invalid argument (Gamma function)';

{Συνάρτηση Γάμα}
function Gamma(arg : real) : real;
  const
    limit      = 1.0E-10;  {πρακτικό κάτω όριο ορίσματος}
    minval     = 10;       {ελάχιστη τιμή αναγωγής ορίσματος- εξ. 1}

  var
    y       : real;     {ανηγμένο όρισμα}
    gy      : real;     {= Γ(y)}
    product : real;
    m       : integer;  {διαφορά y - arg}
    j       : integer;  {δείκτης}
  begin
  if (abs(arg) < limit) or ((arg < 0) and (abs(arg-trunc(arg))<limit))
    then begin
    raise EInvalidArgument.Create(rsInvalidGammaArgument);
    end;
  try
    m := minval - trunc(arg);
  except
    on E: Exception do
      raise EMathError.Create(E.Message);
  end;
  if m < 0 then m := 0;
  y := arg + m;
  gy := sqrt(2*Pi/y) * exp( y*ln(y) + (1-1/(30*y*y))/(12*y)-y ); {1}
  product := 1.0;
  for j := 0 to m-1 do product := product * (arg + j);
  gamma := gy / product;
  end;

{Συνάρτηση Βήτα}
function Beta (x, y: real) : real;
  begin
  beta := gamma(x) * gamma(y) / gamma(x+y)
  end;

{ Συναρτήσεις erf(x) και erfc(x)}
{ όπου
              2       χ
  erfc(x) = ------- ολοκλ. exp (-t^2) dt
            π^(1/2)   0
  και

  erfc(x) = 1 - erf(x)}

{ Υπολογισμός με βάση τις σχέσεις

                2             άπειρο  2^n x^(2n+1)
    erf(x) = ------- exp(-x^2)  Σ    -------------- (κατάλληλη για χ <= 1.5)
             π^(1/2)           n=0   1 3 ... (2n+1)

              1/[1+v/[1+2v/[1+3v/(1+...)]]]
    erfc(x) = -----------------------------         (κατάλληλη για χ > 1.5)
                 π^(1/2) x exp(x^2)

  όπου v = 1 / (2x^2)
  Βλέπε Miller : Basic programs for scientists and engineers, σελ.288
}

resourcestring
  rsInvalidErfArgument = 'Invalid argument (Error function - Erf)';

function Erf (x : real) : real;
  const
    accuracy = 1e-10;
  var
    product, sum : real;
    x2           : real;
    n            : integer;
  begin
  if x < 0 then
    begin
    raise EInvalidArgument.Create(rsInvalidErfArgument);
    end;
  if x < accuracy then erf := 0
  else if x > 1.5 then erf := 1 - erfc(x)
  else
    begin
    x2 := x * x;
    n := 0;
    product := x;
    sum := x;
    repeat
      n := n + 1;
      product := product * 2 * x2 / (1 + 2 * n);
      sum := sum + product;
      until product < sum * accuracy;
    erf := 2 * sum * exp (-x2) / sqrt(Pi);
    end;
  end;

function Erfc (x : real) : real;
  const
    limit = 12;
  var
    v              : real;
    n              : integer;
    term, compound : real;
  begin
  if (x <= 1.5) then erfc := 1 - erf(x)
  else
    begin
    v := 0.5 / x / x;
    term := 1 + v * (limit +1);
    for n := limit downto 1 do
      begin
      compound := 1 + n * v / term;
      term := compound;
      end;
    erfc := exp(-x*x)/ (x*compound*sqrt(Pi));
    end;
  end;

end.
