{******************************************************************}
{                                                                  }
{  Thelma library                                                  }
{                                                                  }
{  Copyright (c) 2000-2010 National Technical University of Athens }
{                                                                  }
{******************************************************************}

{** statistical calculations }
unit stat2;

interface

uses math, DKmath;

{** Counts array elements with values greater than threshold value.
}
function CountLarge (threshold: Real; const l: array of real; n: integer): LongInt;

{** Calculates mean value of array l elements. l should be 0 based. n specifies
    array count. l range is 0..n-1
}
function mean (const l: array of real; n: integer): real;

{** Calculates the unbiased variance of array l elements.
    @SeeAlso <See Routine=mean>
}
function variance (const l: array of real; n: integer): real;

{** Calculates the central moment of array l elements. Specify moment
    order by the order parameter.
    @SeeAlso <See Routine=mean>
}
function CentralMoment (order: integer; const l: array of real;
  n: integer): real;

{** Calculates the central cross moment between two arrays l1, l2
    of order1, order2.
    @SeeAlso <See Routine=mean>
    @SeeAlso <See Routine=CentralMoment>
}
function CentralCrossMoment (order1, order2: integer;
  const l1, l2: array of real; n: integer): real;

{** Calculates the variation of array l elements. Variation is calculated
    by dividing square root of variance with mean value.
    @SeeAlso <See Routine=variance>
}
function Variation (const l: array of real; n: integer): real;

{** Calculates the Skewness (asymetry factor) of array l elements.
    Skweness is calculated by the third central moment and
    mean value of the sample. Unbiased evaluation by Yevjevich,
    1978, pp. 110.
    @SeeAlso <See Routine=mean>
    @SeeAlso <See Routine=CentralMoment>
}
function Skewness (const l: array of real; n: integer): real;

{** Calculates the biased value of Kyrtosis of array l elements.
    Kyrtosis is calculated by the fourth and second central moments.
    @SeeAlso <See Routine=CentralMoment>
}
function Kurtosis (const l: array of real; n: integer): real;

{** Calculates the covariance between two array (l1, l2) elements, representing
    two samples with n elements.
    @SeeAlso <See Routine=correlation>
}
function covariance (const l1, l2: array of real; n: integer): real;

{** Calculates the correlation between two array (l1, l2) elements, representing
    two samples with n elements.
    @SeeAlso <See Routine=covariance>
}
function correlation (const l1, l2: array of real; n: integer): real;

{** Calculate the autocovariance of the array l elements with order
    represented by parameter order.
    @SeeAlso <See Routine=autocorrelation>
}
function autocovariance (order: integer; const l: array of real; n: integer):
  real;

{** Calculate the autocorrelation of the array l elements with order
    represented by parameter order.
    @SeeAlso <See Routine=autocovariance>
}
function autocorrelation (order: integer; const l: array of real; n: integer):
  real;

{** Calculates mean value of array l elements between from and tto
    indeces.
    @SeeAlso <See Routine=mean>
}
function mean1 (const l: array of real; n : integer; from, tto: integer): real;

{** Calculates central moment of order order of array l elements between
    from and tto indeces.
    @SeeAlso <See Routine=CentralMoments>
}
function CentralMoment1 (order: integer; const l: array of real; n : integer;
  from, tto: integer): real;

{** Calculates the Probability Weighted Moments of array l elements.
    Set biased parameter to true for biased estimation. Moments are
    returned by reference to b array. Four moments are estimated and
    returned by order to b[0], b[1], b[2] and b[3].
    @SeeAlso <See Routine=LMoments>
}
procedure PWM (const l: array of real;  n: integer; biased: boolean;
  var b: array of real);

{** Calculates the LMoments of array l elements representing a sample.
    Set biased parameter to true for biased estimation. L-Moments are
    returned by reference to lamda1, lamda2, lamda3 and lamda4.
    @SeeAlso <See Routine=PWM>
}
procedure LMoments (const l: array of real;  n: integer; biased: boolean;
  var lamda1, lamda2, lamda3, lamda4: Real);

{** Sorts a array elements by the QuickSort algorithm by ascension.
    @SeeAlso <See Routine=QuickOrderAsc>
}
procedure QuickSortAsc (var a: array of real; n: integer);

{** Returns an array o with the order of each element of a array.
    @SeeAlso <See Routine=QuickSortAsc>
}
procedure QuickOrderAsc(const a: array of real; var o: array of integer; n: integer);

//procedure QuickOrderAsc(const a: array of real; o: array of integer; n: integer);

{** Test Delta Smirnov - Kolmogorov.<p>
  ������ � Smirnov - Kolmogorov (Test � Smirnov - Kolmogorov)
  �����������  ��� �������� �������������� ��� ��� �������  (�� ��������) ����
  ���������� ��������� ��� ��� ������.
  � ���������  SignLevel ���� �������� ��  �������  ���  ���������  N  ��� ���
  ��������� � (delta) ��� ������� Smirnov-Kolmogorov, ��� ���������� ��� ����
  ��� �������� �������������� a (0 < = a < = 1);
  �� ������� ������������ ������������ ��� ��� ������ ����: a = 1 - L(z),
  ���� L(z) = [(2�)^(1/2) / z] * � < k = 1 ����� ������ > [exp(-(2k-1)^2*�^2/�^2/8]
  ���   z = N^(1/2)*�.
  (��. Kottegoda: Stochastic Resources Technology, ���. 90)
  � ��� ���� ����� ������ ��� ������� ����� ��� � ( > 35).��� ���������� �����
  ������� ��� �������� ��� ����� ��� z, ���� �� ������������ �� ����������� ��
  �� ������� ���������� ������.
  M�� �������������  ���� ��� a ������ �� ����������� ��� �� ��� �������� ����
  ��� �������� �� ����������� ���������� ���  �����  ���  �������� �����������
  ������ ��� ������ ��� -0.12 < = y < = 0.79 (� 0.5 > = a > = 0.0001).
  a := exp(3.323 - 4.794 y - 8.366 y^3) / 100, ���� y = ln(z).
  (� ����� ����� ��� ��������������� ������ ��� �� ���������)
  ������ ��� ������� ������������ �. ������������� (��������� 1986).
}
function KolmogSigLev (N: integer; delta: real): real;

{** Find the order of the array l element with the maximum value.
    @SeeAlso <See Routine=QuickSortAsc>
}
function OrderOfMax (var l: array of real; n: integer): integer;

{** Find the order of the array l element with the minimum value.
    @SeeAlso <See Routine=QuickSortAsc>
}
function OrderOfMin (var l: array of real; n: integer): integer;

{������� �� ����� ��������� ������ - �������: � ����� WList �������������}

{** Returns OrderList. Caution!! l array is destroyed after
    execution of the procedure!
}
procedure OrderList (var l, OrderList: array of real; n: integer);

implementation

uses Generics.Collections, Generics.Defaults;

function CountLarge;
  var
    i: integer;
    sum: integer;
  begin
  sum := 0;
  for i := 0 to n - 1 do
    if l[i] > threshold then sum := sum + 1;
  result := sum;
  end;

function mean;
  const
    SafeLimit = 100;
  var
    i, ti    : integer;
    sum, tsum: real;
  begin
  sum := 0; tsum := 0; ti := 0;
  for i := 0 to n - 1 do
    begin
    tsum := tsum + l[i];
    ti := ti + 1;
    if ti = SafeLimit then
      begin
      sum := sum + tsum;
      ti := 0; tsum := 0;
      end;
    end;
  sum := sum + tsum;
  mean := sum / n;
  end;

{����������� ��� ���������}

function variance;
  const
    SafeLimit = 100;
  var
    i, ti    : integer;
    m        : real;
    sum, tsum: real;
  begin
  m := mean (l, n);
  sum := 0; tsum := 0; ti := 0;
  for i := 0 to n - 1 do
    begin
    tsum := tsum + sqr((l[i] - m));
    ti := ti + 1;
    if ti = SafeLimit then
      begin
      sum := sum + tsum;
      ti := 0; tsum := 0;
      end;
    end;
  sum := sum + tsum;
  variance := sum / (n - 1)
  end;

function CentralMoment;
  const
    SafeLimit = 100;
  var
    i, ti    : integer;
    m        : real;
    sum, tsum: real;
  begin
  m := mean (l,n);
  sum := 0; tsum := 0; ti := 0;
  for i := 0 to n - 1 do
    begin
    tsum := tsum + IntRaise((l[i] - m), order);
    ti := ti + 1;
    if ti = SafeLimit then
      begin
      sum := sum + tsum;
      ti := 0; tsum := 0;
      end;
    end;
  sum := sum + tsum;
  CentralMoment := sum / n;
  end;

function CentralCrossMoment;
  const
    SafeLimit = 100;
  var
    i, ti    : integer;
    m1, m2   : real;
    sum, tsum: real;
  begin
  m1 := mean (l1,n);
  m2 := mean (l2,n);
  sum := 0; tsum := 0; ti := 0;
  for i := 0 to n - 1 do
    begin
    tsum := tsum + IntRaise ((l1[i] - m1), order1) *
                   IntRaise ((l2[i] - m2), order2);
    ti := ti + 1;
    if ti = SafeLimit then
      begin
      sum := sum + tsum;
      ti := 0; tsum := 0;
      end;
    end;
  sum := sum + tsum;
  CentralCrossMoment := sum / n;
  end;


function Variation;
  begin
  Variation := sqrt(variance(l, n))/mean (l, n);
  end;

{������� - ����� ������ ��� �� Yevjevic, ��� 110, ���� ����� �����
 ��� ����������, ��. ���. 186}
function Skewness;
  var
    m3, m2: extended;
  begin
  m2 := CentralMoment (2, l, n);
  m3 := CentralMoment (3, l, n);
  if m2 <> 0.0 then
    Skewness := (m3 / RealRaise (m2, 1.5)) * (N / (N-1)) * (N / (N-2))
  else if m3 = 0.0 then Skewness := 0
  else skewness := 1e100;
  end;

function Kurtosis;
  var
    m4, m2: real;
  begin
  m2 := CentralMoment (2, l, n);
  m4 := CentralMoment (4, l, n);
  if m2 <> 0.0 then
    Kurtosis := (m4 / sqr (m2)) {* (N / (N-1)) * (N / (N-2)) * (N / (N-3))} - 3
  else if m4 = 0.0 then Kurtosis := 0.0
  else Kurtosis := 1e100;
  end;

function covariance;
  const
    SafeLimit = 100;
  var
    i, ti    : integer;
    m1, m2   : real;
    sum, tsum: real;
  begin
  m1 := mean (l1, n);
  m2 := mean (l2, n);
  sum := 0; tsum := 0; ti := 0;
  for i := 0 to n - 1 do
    begin
    tsum := tsum + (l1[i] - m1) * (l2[i] - m2);
    ti := ti + 1;
    if ti = SafeLimit then
      begin
      sum := sum + tsum;
      ti := 0; tsum := 0;
      end;
    end;
  sum := sum + tsum;
  covariance := sum / n;
  end;

function correlation;
  begin
  correlation := covariance (l1, l2, n) / sqrt(CentralMoment (2, l1, n)) /
                                       sqrt(CentralMoment (2, l2, n));
  end;

{���� ���� ������������ ������� ��������� ������}
function mean1;
  const
    SafeLimit = 100;
  var
    i, ti    : integer;
    sum, tsum: real;
  begin
  if (from < 1) or (tto < from) or (tto > n) then
     raise EInvalidArgument.Create('Mean1: Error in Array position');
  sum := 0; tsum := 0; ti := 0;
  for i := from - 1 to tto - 1 do
    begin
    tsum := tsum + l[i];
    ti := ti + 1;
    if ti = SafeLimit then
      begin
      sum := sum + tsum;
      ti := 0; tsum := 0;
      end;
    end;
  sum := sum + tsum;
  mean1 := sum / (tto - from + 1);
  end;

{�������� ���� ������������ ������� ��������� ������}
function CentralMoment1;
  const
    SafeLimit = 100;
  var
    i, ti    : integer;
    m        : real;
    sum, tsum: real;
  begin
  if (from < 1) or (tto < from) or (tto > n) then
       raise EInvalidArgument.Create('CentralMoment1: Error in Array position');
  m := mean1 (l, n, from, tto);
  sum := 0; tsum := 0; ti := 0;
  for i := from - 1 to tto - 1 do
    begin
    tsum := tsum + IntRaise((l[i] - m), order);
    ti := ti + 1;
    if ti = SafeLimit then
      begin
      sum := sum + tsum;
      ti := 0; tsum := 0;
      end;
    end;
  sum := sum + tsum;
  CentralMoment1 := sum / (tto - from + 1);
  end;

{��. V. Yevevich: Stchastic Processes in Hydrology}
function autocovariance;
  const
    SafeLimit = 100;
  var
    i, ti    : integer;
    m1, m2   : real;
    sum, tsum: real;
  begin
  m1 := mean1 (l, n, 1, n - order);
  m2 := mean1 (l, n, order + 1, n);
  sum := 0; tsum := 0; ti := 0;
  for i := 0 to n - order - 1 do
    begin
    tsum := tsum + (l[i] - m1) * (l[i + order] - m2);
    ti := ti + 1;
    if ti = SafeLimit then
      begin
      sum := sum + tsum;
      ti := 0; tsum := 0;
      end;
    end;
  sum := sum + tsum;
  autocovariance := sum / (n - order);
  end;

{��. V. Yevevich: Stochastic Processes in Hydrology}
function autocorrelation;
  var
    s1, s2: real;
  begin
  s1 := sqrt (CentralMoment1 (2, l, n, 1, n - order));
  s2 := sqrt (CentralMoment1 (2, l, n, order + 1, n));
  try
    autocorrelation := autocovariance (order, l, n) / s1 / s2;
  except autocorrelation := 0;
  {Showmessage ('Autocorrelation not defined');}
  end;
  end;


procedure PWM;
  var
    i, j: integer;
    NumEstimators: integer;
    {temp, }temp1: extended;
    order: array of Integer;
  begin
  try
    SetLength(order,n);
    QuickOrderAsc(l,order, n);
    NumEstimators := High(b);
    for j := 0 to NumEstimators do b[j] := 0;
    for i := 0 to n - 1 do
      begin
    //temp := 1 - (n + 1 - order^[i] - 0.35) / n;
      temp1 := 1;
      b[0] := b[0] + l[i];
      for j := 1 to NumEstimators do
        begin
        if biased then temp1 := temp1 {* temp} * (order[i] - 0.65) / n
        else temp1 := temp1 * (order[i] - j) / (n - j);
        b[j] := b[j] + l[i] * temp1;
        end;
      end;
    for j := 0 to NumEstimators do b[j] := b[j] / n;
  finally
    SetLength(order,0);
  end;
  end;

procedure LMoments;
  var b: array[0..3] of Real;
  begin
  PWM (l, n, Biased, b);
  lamda1 := b[0];
  lamda2 := 2*b[1] - b[0];
  lamda3 := 6*b[2] - 6*b[1] + b[0];
  lamda4 := 20*b[3] - 30*b[2] + 12*b[1] - b[0];
  end;

procedure QuickSortAsc(var a: array of real; n: integer);
begin
  TArray.Sort<Real>(a, TComparer<Real>.Default, 0, n);
end;

type TRealInt = record
  r: Real;
  i: Integer;
end;

procedure QuickOrderAsc(const a: array of real; var o: array of Integer; n: Integer);
var
  a1: array of TRealInt;
  i: Integer;
  Comparer: IComparer<TRealInt>;
begin
  SetLength(a1, n);
  for i := 0 to n-1 do
  begin
    a1[i].r := a[i];
    a1[i].i := i+1;
  end;
  Comparer := TDelegatedComparer<TRealInt>.Create(
    function(const Left, Right: TRealInt): Integer
    begin
      if Left.r>Right.r then Result := 1
      else if Left.r<Right.r then Result := -1
      else Result := 0;
    end);
  TArray.Sort<TRealInt>(a1, Comparer, 0, n);
  for i := 0 to n - 1 do
    o[a1[i].i-1] := i+1;
end;


{��������� KOLMOGSIGLEV
  ������ � Smirnov - Kolmogorov (Test � Smirnov - Kolmogorov)
  �����������  ��� �������� �������������� ��� ��� �������  (�� ��������) ����
  ���������� ��������� ��� ��� ������.
  � ���������  SignLevel ���� �������� ��  �������  ���  ���������  N  ��� ���
  ��������� � (delta) ��� ������� Smirnov-Kolmogorov, ��� ���������� ��� ����
  ��� �������� �������������� a (0 <= a <= 1);
  �� ������� ������������ ������������ ��� ��� ������ ����: a = 1 - L(z),
  ���� L(z) = [(2�)^(1/2) / z] * �<k=1 ����� ������>[exp(-(2k-1)^2*�^2/�^2/8]
  ���   z = N^(1/2)*�.
  (��. Kottegoda: Stochastic Resources Technology, ���. 90)
  � ��� ���� ����� ������ ��� ������� ����� ��� � ( > 35).��� ���������� �����
  ������� ��� �������� ��� ����� ��� z, ���� �� ������������ �� ����������� ��
  �� ������� ���������� ������.
  M�� �������������  ���� ��� a ������ �� ����������� ��� �� ��� �������� ����
  ��� �������� �� ����������� ���������� ���  �����  ���  �������� �����������
  ������ ��� ������ ��� -0.12 <= y <= 0.79 (� 0.5 >= a >= 0.0001).
  a := exp(3.323 - 4.794 y - 8.366 y^3) / 100, ���� y = ln(z).
  (� ����� ����� ��� ��������������� ������ ��� �� ���������)
  ������ ��� ������� ������������ �. ������������� (��������� 1986).
}
function KolmogSigLev (N: integer; delta: real): real;
  const
    pi = 3.141592654;
    accur = 1e-12;
  var
    e, z, a  : real;
    k        : integer;
    term, sum: real;
  begin
  if N >= 50 then e := 0 else e := (50 - N) / 500;
  z := delta * sqrt(n) + e;
  if z < 0.3 then a := 1
  else if z > 8.5 then a := 0
  else
    begin
    k := 1;
    sum := 0;
    repeat
      term := exp(-sqr((2*k-1)* pi / z) / 8);
      sum := sum + term;
      k := k + 1;
      until term < accur;
    a := 1 - sqrt(2 * pi) * sum / z;
    end;
  KolmogSigLev := a;
  end;


function OrderOfMax;
  var
    i, j: integer;
    temp: real;
  begin
  j := 0; temp := l[0];
  for i := 1 to n - 1 do
    if temp < l[i] then
      begin
      j := i;
      temp := l[i];
      end;
   Result  := j + 1;
   end;

function OrderOfMin;
  var
    i, j: integer;
    temp: real;
  begin
  j := 0; temp := l[0];
  for i := 1 to n - 1 do
    if temp > l[i] then
      begin
      j := i;
      temp := l[i];
      end;
   Result  := j + 1;
   end;

{������� �� ����� ��������� ������ - �������: � ����� WList �������������}
procedure OrderList;

  var
    i, j   : integer;
    VeryLow: real;

  begin
  i := OrderOfMin(l, n); VeryLow := l[i] - 1;
  for i := 0 to n - 1 do
    begin
    j := OrderofMax(l, n);
    OrderList[i] := j;
    l[j] := VeryLow;
    end;
  end;

end.
