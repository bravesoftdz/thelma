{******************************************************************}
{                                                                  }
{  Thelma library                                                  }
{                                                                  }
{  Copyright (c) 2000-04 National Technical University of Athens   }
{                                                                  }
{******************************************************************}

{** statistical processes }
unit statprocesses;

interface

uses
  classes, ts, contnrs, sysutils, optim_routines, Clipbrd;

type
  TProbabilityPaperType = (pdptLinear, pdptNormal, pdptGumbelMax,
    pdptGumbelMin, pdptWeibull, pdptGEVMax, pdptGEVMin);
  TProbabilityPaperTypes = set of TProbabilityPaperType;
const pmz_PaperMinZ: array [TProbabilityPaperType] of Real =
  (0.01,-3.5,-2.0,-5.0,-5.0,-2.0,-5.0);
const pmz_PaperMaxZ: array [TProbabilityPaperType] of Real =
  (0.99,3.5,8.0,2.0,2.0,8.0,2.0);
type
  TStatisticalDistributionType = (sdtNormal, sdtLogNormal,
    sdtGalton, sdtExponential, sdtGamma, sdtPearsonIII, sdtLogPearsonIII,
    sdtEV1Max, sdtEV2Max, sdtEV1Min, sdtEV3Min, sdtGEVMax, sdtGEVMin,
    sdtPareto, sdtLNormal, sdtLExponential, sdtLEV1Max, sdtLEV2Max,
    sdtLEV1Min, sdtLEV3Min, sdtLGEVMax, sdtLGEVMin, sdtLPareto,
    sdtGEVMaxK, sdtGEVMinK, sdtLGEVMaxK, sdtLGEVMinK);
  TStatisticalDistributionTypes = set of TStatisticalDistributionType;
type
  EImpossibleXSquareTest = class(EMathError);

type
{** ProbabilityRecord Class, holding a value, the Weibull
      probability and the Date.
}
  TProbabilityRecord = class(TObject)
  private

  public
{** Holds the sample value for the record.
}
    Value: Real;
{** Holds the Weibull Probability of each point. Write a function to
    sort values ascending, then calculate Weibull Probability by
    i / (n+1), where, n the sample size, i the order of value.
}
    WeibullProbability: Real;
{** Blom probability function.
}
    BlomProbability: Real;
{** Cunnane probability function.
}
    CunnaneProbability: Real;
{** Gringorten probability function.
}
    GringortenProbability: Real;
{** Holds date of the record. Date is used to extract monthly
    sub-samples.
}
    Date, ActualDate: TDateTime;
  end;

{** DataList Class. DataList is the parent for the FullDataList
      and the type for monthly data stored. Data list class owns
      methods for Weibull Probability calculation, and calculation
      of typical statistical values, such as mean value etc.
}
  TDataList = class(TPersistent)
  private
    FUnbiased: Boolean;
    FBiasedStandardDeviation: Real;
    FBiasedThirdCentralMoment: Real;
    FBiasedFourthCentralMoment: Real;
    FBiasedLogStandardDeviation: Real;
    FBiasedLogThirdCentralMoment: Real;
    FBiasedLMoment: array[1..4] of Real;
    FUnbiasedLMoment: array[1..4] of Real;
    FTruncate: Boolean;
    FItemsList: TObjectList;
    function GetStandardDeviation: Real;
    function GetVariance: Real;
    function GetThirdCentralMoment: Real;
    function GetLogStandardDeviation: Real;
    function GetLogThirdCentralMoment: Real;
    function GetSkewness: Real;
    function GetKurtosis: Real;
    function GetLogSkewness: Real;
    function GetLMoment1: Real;
    function GetLMoment2: Real;
    function GetLMoment3: Real;
    function GetLMoment4: Real;
    function GetLSkewness: Real;
    function GetLKurtosis: Real;
{** Calc weibull probability (empirical distribution) for each
    record, after a data sorting}
    procedure CalcStats(Truncate: Boolean);
    function GetCount: Integer;
    function GetCountZeros: Integer;
    function GetCountNonZeros: Integer;
  protected
{** Returns a TProbabilityRecord from the items array of index Index.
}
    function Get(Index: Integer): TProbabilityRecord;
{** Returns first TProbabilityRecord from items array.
}
    function GetFirst: TProbabilityRecord;
{** Returns last TStatisticalDistribution from items array.
}
    function GetLast: TProbabilityRecord;
  public
{** The mean value of the sample xi}
    MeanValue: Real;
{** The mean value of the sample yi = log xi}
    LogMeanValue: Real;
{** First xi>0}
    FirstPositiveValue: Real;
{** Is true when L-Moments may be calculated, i.e. when
    at least three not null values exist}
    LMomentExist: Boolean;
{** Standard constructor. Creates the items array. Set AOwnsObjects
    to True if DataList onws TProbabilityRecord objects.
}
    constructor Create(AOwnsObjects: Boolean); overload;
{** Standard constructor. By creating a TDataList with this method,
    then data list owns objects.
}
    constructor Create; overload;
{** Standard destructor.
}
    destructor Destroy; override;
{** Returns the number of the items.
}
    property Count: Integer read GetCount;
{** Returns the number of zero (<1e-9) items
}
    property CountZeros: Integer read GetCountZeros;
{** Returns the number of non zero (>1e-9) items as count-countzeros
}
    property CountNonZeros: Integer read GetCountNonZeros;
{** Items array. Items property is default, you don't have to use Items
    property, write: DataList[i].
}
    property Items[Index: Integer]: TProbabilityRecord read Get; default;
{** Returns first Item.
}
    property First: TProbabilityRecord read GetFirst;
{** Returns last Item.
}
    property Last: TProbabilityRecord read GetLast;
{** Use Add to create new Items. Set Distribution type and Data list with
    sample values.
}
    procedure Add(AProbabilityRecord: TProbabilityRecord);
{** Standard deviation for the sample xi. Set Unbiased property
    to true for unbiased estimation.
    @SeeAlso <See Property=Unbiased>
}
    property StandardDeviation: Real read GetStandardDeviation;
{** Variance of the sample xi = Sqr(StandardDeviation).
}
    property Variance: Real read GetVariance;
{** Third Central Moment of the sample xi. Set Unbiased property
    to true for unbiased estimation.
    @SeeAlso <See Property=Unbiased>
}
    property ThirdCentralMoment: Real read GetThirdCentralMoment;
{** Skewness is the estimator of the asymetry of the sample.
    Skewness = ThirdCentralMoment / Power(StandardDeviation,3)
}
    property Skewness: Real read GetSkewness;
{**
}
    property Kurtosis: Real read GetKurtosis;
{** Standard deviation for the yi = log xi.
}
    property LogStandardDeviation: Real read GetLogStandardDeviation;
{** Third central moment for the yi = log xi.
}
    property LogThirdCentralMoment: Real read GetLogThirdCentralMoment;
{** Skewness for the yi = log xi.
}
    property LogSkewness: Real read GetLogSkewness;
{** First L-Moment. Set Unbiased property
    to true for unbiased estimation.
}
    property LMoment1: Real read GetLMoment1;
{** Second L-Moment. Set Unbiased property
    to true for unbiased estimation.
}
    property LMoment2: Real read GetLMoment2;
{** Third L-Moment. Set Unbiased property
    to true for unbiased estimation.
}
    property LMoment3: Real read GetLMoment3;
{** Fourth L-Moment. Set Unbiased property
    to true for unbiased estimation.
}
    property LMoment4: Real read GetLMoment4;
{**
}
    property LSkewness: Real read GetLSkewness;
{**
}
    property LKurtosis: Real read GetLKurtosis;
{** Set Unbiased property to True for unbiased estimators or set to
    False for biased estimators. Default value is True (unbiased
    estimators).
}
    property Unbiased: Boolean read FUnbiased write FUnbiased default True;
{** Read truncate to examine if data are truncated to zero.
}
    property Truncate: Boolean read FTruncate;
{** Call CalcWeibull in order to calculate weibull probability function
    for each of the sample values.
}
    procedure CalcWeibull;
{** Call prepare data in order to calculate all statistical parameters.
    Set ToSort variable to True in order to sort sample values. Set
    Truncate to True in order to truncate negative sample values to
    zero (0). When truncate, stored values are not altered, only
    derived statistical parameters consider this feature.
}
    procedure PrepareData(ToSort, Truncate: Boolean);
{** Use clear to empty items array.
}
    procedure Clear;
{** AddValue creates a TProbabilityRecord holding AValue
}
    procedure AddValue(AValue: Real);
  end;

{** FullDataList is a child of DataList class, to hold all the values,
      yearly or monthly
}
  TFullDataList = class(TDataList)
  private
{}
  public
{** MUnit is the string holdig the unit of measurement.
}
    MUnit: string;
{** SetTS read Timeseries records in order to fill sample values of
    the DataList. Set Truncate to True in order to truncate negative
    values to zero.
}
    procedure SetTS(Timeseries: TTimeseries;Truncate: Boolean);
  end;

{** MonthlyDataList is a class derived from TPersistent, holding
      12 TDataList, each for a month.
      FillData gets values from a FullDataList to fill month
      values, then it prepares the data of each month.
}
  TMonthlyDataList = class (TPersistent)
  private
{}
  public
{** Months is an array from 1 to 12, representing 12 TDataList
    one for each month. TDataList holds pointers, so TMonthlyDataList
    shares the same records as the FullDataList filling the class.
}
    Months: array[1..12] of TDataList;
{** The standard constructor. Create gets memory for
    12 TDataList objects.
}
    constructor Create;
{** The standard destructor. Frees also all TDataList owned.
}
    destructor Destroy; override;
{** Call FillData to get the records pointers from TFullDataList,
    and the classify them as months.
}
    procedure FillData(FullDataList: TFullDataList; Truncate: Boolean);
  end;

{** An object to hold the properties of a statistical distribution.
}
  TStatisticalDistribution = class(TPersistent)
  private
    FDataList: TDataList;
    FDistributionType: TStatisticalDistributionType;
    FParam1, FParam2, FParam3: Real;
    FLMoment1, FLMoment2, FLMoment3, FLMoment4: Real;
    FMeanValue, FStandardDeviation, FVariance, FSkewness, FKurtosis: Real;
    FLogMeanValue, FLogStandardDeviation, FLogSkewness: Real;
    FGEVShape: Real;
    FAproxParameters: Boolean;
    foptMaxEval: Real;
    function GetParamsCount: Integer;
    function GetMinX: Real;
    function GetMaxX: Real;
    function GetName: string;
    function GetParam1Name: string;
    function GetParam2Name: string;
    function GetParam3Name: string;
    function GetIsLMomentMethod: Boolean;
    function GetIsLowerBounded: Boolean;
    function GetIsUpperBounded: Boolean;
    function GetStatParam1: Real;
    function GetStatParam2: Real;
    function GetStatParam3: Real;
    function GetStatParam4: Real;
    function GetNumberOfClasses: Integer;
    function MLEObjFunc(X: TArrayOfReal): Real;
    function pdfValueAtP(p1, p2, p3: Real; AXValue: Real): Real;
  public
    function GetMinXAtP(p1, p2, p3: Real): Real;
    function GetMaxXAtP(p1, p2, p3: Real): Real;
{** The standard constructor. Set the distribution type and load sample
    values from ADataList. GEVShape is used by GEVMax and GEVMin
    distributions if the k parameter is specified.
}
    constructor Create(DistributionType: TStatisticalDistributionType;
      ADataList: TDataList; GEVShape: Real); overload;
{** Use this version of Create in order to specify if Aproximate Parameters
    are considered.
}
    constructor Create(DistributionType: TStatisticalDistributionType;
      ADataList: TDataList; GEVShape: Real; AproxParameters: Boolean); overload;
{** StatParameter1 is the Mean Value for distributions evaluated with
    momement method, LogMeanValue for Log-PearsonIII and LMoment1 for
    distributions evaluated with L-Momements method.
}
    property StatParameter1: Real read GetStatParam1;
{** StatParameter2 is the StdDev for distributions evaluated with
    momement method, LogStdDev for Log-PearsonIII and LMoment2 for
    distributions evaluated with L-Momements method.
}
    property StatParameter2: Real read GetStatParam2;
{** StatParameter3 is the Skewness for distributions evaluated with
    momement method, LogSkew for Log-PearsonIII and LMoment3 for
    distributions evaluated with L-Momements method.
}
    property StatParameter3: Real read GetStatParam3;
{** StatParameter4 is the Kurtosis for distributions evaluated with
    momement method and LMoment4 for
    distributions evaluated with L-Momements method.
}
    property StatParameter4: Real read GetStatParam4;
{** First parameter of the statistical distribution.
}
    property Parameter1: Real read FParam1;
{** Second parameter of the statistical distribution.
}
    property Parameter2: Real read FParam2;
{** Third parameter of the statistical distribution.
}
    property Parameter3: Real read FParam3;
{** Number of parameter, specific to the distribution type.<p>
    When ParameterCount value is 1, only Parameter1 is used, if it is
    2, Parameter1 and Parameter2 are used etc.<p>
    2 Parameters distributions: Normal, LogNormal, Exponential, Gamma,
    EV1-Max and Min, EV2Max, EV3Min (with Moments or L-Moments method),
    GEV-Max and Min with k is specified.<p>
    3 Parameters distributions: PearsonIII, LogPearsonIII, GEVMax and
    GEVMin (k is unkwonwn, with Moments or L-Moments method), Pareto.
}
    property ParameterCount: Integer read GetParamsCount;
{** True if distribution is based on L-Moment estimators.
}
    property IsLMomentMethod: Boolean read GetIsLMomentMethod;
{** Distribution type as passed from the Create method.
}
    property DistributionType: TStatisticalDistributionType read
      FDistributionType;
{** Minimum value (Lower bound) of the random variable. Read MinX
    only when IsLowerBounded is true. If IsLowerBounded is false,
    a value of -inf is returned where inf = 1e34.
    @SeeAlso <See Property=IsLowerBounded>
}
    property MinX: Real read GetMinX;
{** Maximum value (Upper bound) of the random variable. Read MaxX
    only when IsUpperBounded is true. If IsUpperBounded is false,
    a value of +inf is returned where inf = 1e34.
    @SeeAlso <See Property=IsUpperBounded>
}
    property MaxX: Real read GetMaxX;
{** Returns True if distribution is lower bounded (e.g. Gamma
    distribution). If distribution is lower bounded, read
    MinX property.
    @SeeAlso <See Property=MinX>
}
    property IsLowerBounded: Boolean read GetIsLowerBounded;
{** Returns True if distribution is upper bounded (e.g. some cases of Pareto
    distribution). If distribution is upper bounded, read
    MaxX property.
    @SeeAlso <See Property=MaxX>
}
    property IsUpperBounded: Boolean read GetIsUpperBounded;
{** Returns a string containing the name of the distribution.
}
    property Name: string read GetName;
{** A string containing the name of the first parameter of the distribution.
}
    property Param1Name: string read GetParam1Name;
{** A string containing the name of the second parameter of the distribution.
}
    property Param2Name: string read GetParam2Name;
{** A string containing the name of the third parameter of the distribution.
    (should be empty string for a 2 parameter distribution).
}
    property Param3Name: string read GetParam3Name;
{** Returns DataList as assigned from Create method.
}
    property DataList: TDataList read FDataList;
{** Returns the probability density function Value f(x) for a specific value x
    of the random variable = AXValue.
    @SeeAlso <See Method=InvcdfValue>
}
    function pdfValue(AXValue: Real): Real;
{** Returns the probability function Value F(x) for a specific value x of the
    random variable = AXValue.
    @SeeAlso <See Method=InvcdfValue>
}
    function cdfValue(AXValue: Real): Real; overload;
    function cdfValue(p1, p2, p3, AXValue: Real): Real; overload;
{** Returns the inverted probability function, namely the value of the
    value x for a specific probability function F = AFValue.
    @SeeAlso <See Method=cdfValue>
}
    function InvcdfValue(AFValue: Real): Real;
{** Returns a random value following the statistical distribution.
}
    function Rand: Real;
{** Calculates distribution parameters. You don't have normaly to
    call CalcParams, use Refresh.
}
    procedure CalcParams;
{** Calculates distribution parameters by aproximate method. (for EV2Max,
    EV3Min, GEVMin, GEVMax). You don't have normaly to
    call CalcAproxParams, use Refresh.
}
    procedure CalcAproxParams;
{** Use Refresh to recalculate Distribution parameters. Recalculate calls
    either CalcParams or CalcAproxParams according to Create.
    Refresh first reads parameters from DataList then recalculates
    distribution parameters.
}
    procedure Refresh;
{** MultiplyStatParam, multiplies StatParam1-3 by Multiplier1-3. This
    method is used by Monte Carlo simulation algorithm in order to
    calculate derivatives. Use this method with CAUTION.
    Call Refresh to restore original parameters. Refresh reads
    DataList parameters.
}
    procedure MultiplyStatParam(Multiplier1, Multiplier2,
      Multiplier3: Real);
{** SetGEVShape procedure sets the GEV parameter when Shape factor
    is specified.
    Use SetGEVShape to alter default value. AGEVShape should be within
    a range of 0.001-0.5 or else an exception is raised.
    If DistributionType is one of sdtGEVMaxK, sdtGEVMinK, sdtLGEVMaxK,
    sdtLGEVMinK, then Refresh is called automatically after SetGEVShape
    in order to recalculate parameters.
}
    procedure SetGEVShape(AGEVShape: Real);
{** Read GEVShape parameter.
    @SeeAlso <See Method = SetGEVShape>
}
    property GEVShape: Real read FGEVShape;
{**
}
    property NumberOfClasses: Integer read GetNumberOfClasses;
{** Make A X-Square test for the distribution.
    Returns True if the distribution pass the test for the specified
    Level of importance. Returns also the required level of importance to
    fail the test
    as well as the Pearson Statistical Parameter for the test.
}
    function XSquareTest(ALevel: Real; ANumberOfClasses: Integer;
      var LevelToFail, PearsonParam: Real): Boolean;
{** Make a Kolmogoov-Smirnov test for the distribution.
    Returns True if the distribution pass the test for the specified
    Level of importance. Returns also the required level of importance
    to fail the test as well as the Maximum diference in probabilities
    (theritical from empirical).
}
    function KolmogorovSmirnovTest(ALevel: Real; var LevelToFail, Dmax: Real):
      Boolean;
{** Distribution parameters estimation with the maximum likelihood estimation
    method (MLE).
}
    procedure CalculateMaxLikelihoodParams(
      min1, min2, min3, max1, max2, max3:Real;
      var param1, param2, param3: Real);
  end;

{** TStatisticalDistributionList is a class holding an object list
    of TStatisticalDistributionItems.
    @SeeAlso <See Class=TStatisticalDistribution>
}
  TStatisticalDistributionsList = class(TPersistent)
  private
    FDistributionList: TObjectList;
    function GetCount: Integer;
  protected
{** Returns a TStatisticalDistribution from the items array of index Index.
}
    function Get(Index: Integer): TStatisticalDistribution;
{** Returns first TStatisticalDistribution from items array.
}
    function GetFirst: TStatisticalDistribution;
{** Returns last TStatisticalDistribution from items array.
}
    function GetLast: TStatisticalDistribution;
  public
{** Standard constructor. Creates the items array.
}
    constructor Create;
{** Standard destructor.
}
    destructor Destroy; override;
{** Returns the number of the items.
}
    property Count: Integer read GetCount;
{** Items array. Items property is default, you don't have to use Items
    property, write: DistributionList[i].
}
    property Items[Index: Integer]: TStatisticalDistribution read Get; default;
{** Returns first Item.
}
    property First: TStatisticalDistribution read GetFirst;
{** Returns last Item.
}
    property Last: TStatisticalDistribution read GetLast;
{** Returns first instance of TStatisticalDistribution of type
    ADistributionType.
}
    function FindFirstDistributionOf(ADistributionType: TStatisticalDistributionType):
      TStatisticalDistribution;
{** Use Add to create new Items. Set Distribution type and Data list with
    sample values.
}
    procedure Add(ADistributionType: TStatisticalDistributionType;
      ADataList: TDataList; GEVShape: Real);
{** Use clear to empty items array.
}
    procedure Clear;
  end;

{** Use ProbabilityPaper in order to transorm exceedance probability (F1x) to
    a z value in order to linerize typical distributions such as
    normal, gumbel, etc. Set GEVParameter to a positive value with
    default value GEVParameter=0.15 (kappa - shape parameter).
    @SeeAlso <See Routine=InvProbabilityPaper>
}
function ProbabilityPaper(Probability: Real; PType: TProbabilityPaperType;
  GEVParameter: Real): Real;

{** Use InvProbabilityPaper to transform a z value to probability function
    (F).
    @SeeAlso <See Routine=ProbabilityPaper>
}
function InvProbabilityPaper(ZValue: Real; PType: TProbabilityPaperType;
    GEVParameter: Real): Real;

{** Interpolates a Probability function value on a set of points. Point
    coordinates are XValues, YValues. XValues are transformed points
    e.g. by ProbabilityPaper function, holding standarized Z value.
    Result is the interpolated value of variable X.
    @SeeAlso <See Routine=ProbabilityPaper>
    @SeeAlso <See Routine=ValueLinearInterpolate>
}
function ProbabilityLinearInterpolate(XValues, YValues: array of Real;
  AProbability: Real; PType: TProbabilityPaperType; GEVParameter: Real): Real;

{** Interpolates a Value to a probability function value. Point coordinates
    are XValues, YValues. XValues are transformed poitns e.g. by
    ProbabilityPaper function, holding standarized Z value.
    AValue is a value of the random variable X.
    Result is the probability function F.
    @SeeAlso <See Routine=ProbabilityPaper>
    @SeeAlso <See Routine=ProbabilityLinearInterpolate>    
}
function ValueLinearInterpolate(XValues, YValues: array of Real;
  AValue: Real; PType: TProbabilityPaperType; GEVParameter: Real): Real;

implementation

uses
  math, dates, prob, stat2, rnd, dateutils;

{ type TListSortCompare = function (Item1, Item2: Pointer): Integer;}

{$WARN UNSAFE_CAST OFF}
{$WARN UNSAFE_TYPE OFF}

function RecordsCompare(Value1, Value2: Pointer): Integer;
begin
 if (TProbabilityRecord(Value1).Value - TProbabilityRecord(Value2).Value)
   < 0 then
   Result := 1
 else if (TProbabilityRecord(Value1).Value - TProbabilityRecord(Value2).Value)
   > 0 then
   Result := -1
 else
   Result := 0;
end;

{$WARN UNSAFE_TYPE ON}
{$WARN UNSAFE_CAST ON}

{*********}
{TDataList}
{*********}

constructor TDataList.Create;
begin
  Create(True);
end;

constructor TDataList.Create(AOwnsObjects: Boolean);
begin
  inherited Create;
  FUnbiased := True;
  FItemsList := TObjectList.Create(AOwnsObjects);
end;

destructor TDataList.Destroy;
begin
  FItemsList.Free;
  inherited Destroy;
end;

function TDataList.Get(Index: Integer): TProbabilityRecord;
begin
  Result := TProbabilityRecord(FItemsList[Index]);
end;

function TDataList.GetFirst: TProbabilityRecord;
begin
  Result := TProbabilityRecord(FItemsList.First);
end;

function TDataList.GetLast: TProbabilityRecord;
begin
  Result := TProbabilityRecord(FItemsList.Last);
end;

function TDataList.GetCount: Integer;
begin
  Result := FItemsList.Count;
end;

function TDataList.GetCountZeros: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to FItemsList.Count-1 do
    if Abs(Items[i].Value)<1e-9 then
      Inc(Result);
end;

function TDataList.GetCountNonZeros: Integer;
begin
  Result := GetCount-GetCountZeros;
end;

procedure TDataList.Clear;
begin
  FItemsList.Clear;
end;

procedure TDataList.Add(AProbabilityRecord: TProbabilityRecord);
begin
  FItemsList.Add(AProbabilityRecord);
end;

procedure TDataList.CalcWeibull;
var
  i: Integer;
  AProbabilityRecord: TProbabilityRecord;
begin
  for i := 0 to Count -1 do
  begin
    AProbabilityRecord := Items[i];
    AProbabilityRecord.WeibullProbability :=
      WeibullEmpirical(i+1,Count);
    AProbabilityRecord.BlomProbability :=
      BlomEmpirical(i+1,Count);
    AProbabilityRecord.CunnaneProbability :=
      CunnaneEmpirical(i+1,Count);
    AProbabilityRecord.GringortenProbability :=
      GringortenEmpirical(i+1,Count);
  end;
end;

procedure TDataList.CalcStats(Truncate: Boolean);
var
  i, ACount: Integer;
  ATempValue: Real;
begin
  MeanValue := 0;
  FBiasedStandardDeviation := 0;
  FBiasedThirdCentralMoment := 0;
  FBiasedFourthCentralMoment := 0;
  if Count < 1 then
  begin
    FirstPositiveValue := 0.1;
    FBiasedStandardDeviation := 0;
  end else begin
    FirstPositiveValue := First.Value;
    try
      for i := Count -1 downto 0 do
        if Items[i].Value>0 then
        begin
          FirstPositiveValue := Items[i].Value;
          Break;
        end;
      for i := 0 to Count - 1 do
      begin
        ATempValue := Items[i].Value;
        if (ATempValue<0) and Truncate then
          ATempValue := 0;
        MeanValue := MeanValue + ATempValue;
      end;
      MeanValue := MeanValue / Count;
      for i := 0 to Count -1 do
      begin
        ATempValue := Items[i].Value;
        if (ATempValue<0) and Truncate then
          ATempValue := 0;
        FBiasedStandardDeviation := FBiasedStandardDeviation +
          Sqr(ATempValue - MeanValue);
        FBiasedThirdCentralMoment := FBiasedThirdCentralMoment +
          Power(ATempValue - MeanValue, 3);
        FBiasedFourthCentralMoment := FBiasedFourthCentralMoment +
          Power(ATempValue - MeanValue, 4);
      end;
      FBiasedStandardDeviation := Sqrt(FBiasedStandardDeviation / Count);
      FBiasedThirdCentralMoment := FBiasedThirdCentralMoment / Count;
      FBiasedFourthCentralMoment := FBiasedFourthCentralMoment / Count;
    except
      on EMathError do
      begin
        MeanValue := 0;
        FBiasedStandardDeviation := -1;
        FBiasedThirdCentralMoment := 0;
        FBiasedFourthCentralMoment := 0;
      end else raise;
    end;
  end;
{Calculate Log Statistics Values for Log Pearson III and Weibull}
  LogMeanValue := 0;
  FBiasedLogStandardDeviation := 0;
  FBiasedLogThirdCentralMoment := 0;
  ACount := 0;
  try
    for i := 0 to Count-1 do
      if Items[i].Value>0 then
      begin
        Inc(ACount);
        LogMeanValue := LogMeanValue + Ln(Items[i].Value);
      end;
    LogMeanValue := LogMeanValue / ACount;
    for i := 0 to Count-1 do
      if Items[i].Value>0 then
      begin
        FBiasedLogStandardDeviation := FBiasedLogStandardDeviation +
          Sqr( Ln(Items[i].Value) - LogMeanValue);
        FBiasedLogThirdCentralMoment := FBiasedLogThirdCentralMoment +
          Power( Ln(Items[i].Value) - LogMeanValue, 3);
      end;
    FBiasedLogStandardDeviation := Sqrt(FBiasedLogStandardDeviation / ACount);
    FBiasedLogThirdCentralMoment := FBiasedLogThirdCentralMoment / ACount;
  except
    on EMathError do
      begin
        LogMeanValue := 0;
        FBiasedLogStandardDeviation := 0;
        FBiasedLogThirdCentralMoment := 0;
      end else raise;
  end;
{L-Moments}
  FUnbiasedLMoment[1] := MeanValue;
  FUnbiasedLMoment[2] := 0;
  FUnbiasedLMoment[3] := 0;
  FUnbiasedLMoment[4] := 0;
  FBiasedLMoment[1] := MeanValue;
  FBiasedLMoment[2] := 0;
  FBiasedLMoment[3] := 0;
  FBiasedLMoment[4] := 0;
  if Count<3 then
  begin
    LMomentExist := False
  end else begin
    LMomentExist := True;
    for i := 0 to Count-1 do
    begin
{Keep original value if gonna truncated}
      ATempValue := Items[Count-i-1].Value;
{Set negative values to 0 if truncated}
      if ATempValue < 0 then
        if Truncate then
          Items[Count-i-1].Value := 0;
      FBiasedLMoment[2] := FBiasedLMoment[2] +
        (1-(Count-i-0.35)/Count) * Items[Count-i-1].Value;
      FBiasedLMoment[3] := FBiasedLMoment[3] +
        Sqr(1-(Count-i-0.35)/Count) *
          Items[Count-i-1].Value;
      FBiasedLMoment[4] := FBiasedLMoment[3] +
        Power(1-(Count-i-0.35)/Count, 3) *
          Items[Count-i-1].Value;
      if i>0 then
        FUnbiasedLMoment[2] := FUnbiasedLMoment[2] +
          i* Items[Count-i-1].Value/(Count-1);
      if i>1 then
        FUnbiasedLMoment[3] := FUnbiasedLMoment[3] + (i*((i-1)*
          Items[Count-i-1].Value)/(Count-1))/(Count-2);
      if i>2 then
        FUnbiasedLMoment[4] := FUnbiasedLMoment[4] + (i*((i-1)*(i-2)*
          Items[Count-i-1].Value)/(Count-1))/((Count-2)*(Count-3));
{Restore original value}
      Items[Count-i-1].Value := ATempValue;
    end;
    FUnbiasedLMoment[2] := 2*FUnbiasedLMoment[2]/Count - FUnbiasedLMoment[1];
    FUnbiasedLMoment[3] := 6*FUnbiasedLMoment[3]/Count -
      3*FUnbiasedLMoment[2] - 2*FUnbiasedLMoment[1];
    FUnbiasedLMoment[4] := 20*FUnbiasedLMoment[4]/Count -
      - 5*FUnbiasedLMoment[3] - 9*FUnbiasedLMoment[2] - 5*FUnbiasedLMoment[1];
    FBiasedLMoment[2] := 2*FBiasedLMoment[2]/Count - FBiasedLMoment[1];
    FBiasedLMoment[3] := 6*FBiasedLMoment[3]/Count -
      3*FBiasedLMoment[2] - 2*FBiasedLMoment[1];
    FBiasedLMoment[4] := 20*FBiasedLMoment[4]/Count -
      - 5*FBiasedLMoment[3] - 9*FBiasedLMoment[2] - 5*FBiasedLMoment[1];
  end;
end;

procedure TDataList.PrepareData(ToSort, Truncate: Boolean);
begin
  FTruncate := Truncate;
  if ToSort then
    FItemsList.Sort(RecordsCompare);
  CalcWeibull;
  CalcStats(FTruncate);
end;

function TDataList.GetStandardDeviation: Real;
begin
  Result := FBiasedStandardDeviation;
  if Unbiased then
    if Count > 1 then
      Result := Result * sqrt(Count/(Count-1));
end;

function TDataList.GetVariance: Real;
begin
  Result := Sqr(StandardDeviation);
end;

function TDataList.GetLogStandardDeviation: Real;
begin
  Result := FBiasedLogStandardDeviation;
  if Unbiased then
    if Count > 1 then
      Result := Result * sqrt(Count/(Count-1));
end;

function TDataList.GetThirdCentralMoment: Real;
begin
  Result := FBiasedThirdCentralMoment;
  if Unbiased then
    if Count >2 then
      Result := (((Result * Count)*Count)/(Count-1))/(Count-2);
end;

function TDataList.GetKurtosis: Real;
var n: Integer;
begin
  Result := 0;
  n := Count;
  if Unbiased then
  begin
    if (StandardDeviation>0 ) and (n>3) then
    begin
      Result := (n+1)*n*n*FBiasedFourthCentralMoment /
        ((n-1)*Sqr(Variance)) - 3*Sqr(n-1);
      Result := Result / ((n-2)*(n-3));
    end;
  end else begin
    if StandardDeviation>0 then
      Result := FBiasedFourthCentralMoment/Sqr(Variance) - 3;
  end;
end;

function TDataList.GetLogThirdCentralMoment: Real;
begin
  Result := FBiasedLogThirdCentralMoment;
  if Unbiased then
    if Count >2 then
      Result := (((Result * Count)*Count)/(Count-1))/(Count-2);
end;

function TDataList.GetSkewness: Real;
begin
  Result := 0;
  if StandardDeviation > 0 then
    Result := ThirdCentralMoment / Power(StandardDeviation,3);
end;

function TDataList.GetLogSkewness: Real;
begin
  Result := 0;
  if LogStandardDeviation > 0 then
    Result := LogThirdCentralMoment / Power(LogStandardDeviation,3);
end;

function TDataList.GetLMoment1: Real;
begin
  if Unbiased then
    Result := FUnbiasedLMoment[1]
  else
    Result := FBiasedLMoment[1];
end;

function TDataList.GetLMoment2: Real;
begin
  if Unbiased then
    Result := FUnbiasedLMoment[2]
  else
    Result := FBiasedLMoment[2];
end;

function TDataList.GetLMoment3: Real;
begin
  if Unbiased then
    Result := FUnbiasedLMoment[3]
  else
    Result := FBiasedLMoment[3];
end;

function TDataList.GetLMoment4: Real;
begin
  if Unbiased then
    Result := FUnbiasedLMoment[4]
  else
    Result := FBiasedLMoment[4];
end;

function TDataList.GetLSkewness: Real;
begin
  Result := 0;
  if LMoment2<>0 then
    Result := LMoment3 / LMoment2;
end;

function TDataList.GetLKurtosis: Real;
begin
  Result := 0;
  if LMoment2<>0 then
    Result := LMoment4 / LMoment2;
end;

resourcestring
  rsCannotAddValuesInDataList =
    'Cannot add values in TDataList object since it does not own objects';

procedure TDataList.AddValue(AValue: Real);
var
  AProbabilityRecord: TProbabilityRecord;
begin
  if not FItemsList.OwnsObjects then
    raise Exception.Create(rsCannotAddValuesInDataList);
  AProbabilityRecord := nil;
  try
    AProbabilityRecord := TProbabilityRecord.Create;
    AProbabilityRecord.Value := AValue;
    Add(AProbabilityRecord);
    AProbabilityRecord := nil;
  except
    AProbabilityRecord.Free;
    raise;
  end;
end;

{*************}
{TFullDataList}
{*************}

procedure TFullDataList.SetTS(Timeseries: TTimeseries; Truncate: Boolean);
var
  i: Integer;
  AProbabilityRecord: TProbabilityRecord;
begin
  AProbabilityRecord := nil;
  try
{Get MUnit}
  MUnit := Timeseries.MUnit;
{Clear the full records list}
    Clear;
    for i := 0 to Timeseries.Count -1 do
    begin
      if not Timeseries.Items[i].IsNull then
        with AProbabilityRecord do
        begin
          AProbabilityRecord := TProbabilityRecord.Create;
          Value := Timeseries.Items[i].AsFloat;
          Date := Timeseries.Items[i].Date;
          if Timeseries.TimeStep>=tstMonthly then
            ActualDate := AProbabilityRecord.Date
          else
            ActualDate := Timeseries.IntervalMidPoint(AProbabilityRecord.Date);
          Add(AProbabilityRecord);
          AProbabilityRecord := nil;
        end;
    end;
  except
    AProbabilityRecord.Free;
    raise;
  end;
{Now, prepare data}
{Sort on to the full data list}
  PrepareData(True, Truncate);
end;

{****************}
{TMonthlyDataList}
{****************}

constructor TMonthlyDataList.Create;
var
  i: Integer;
begin
  inherited Create;
  for i := 1 to 12 do
  begin
    Months[i] := TDataList.Create(False);
  end;
end;

destructor TMonthlyDataList.Destroy;
var
  i: Integer;
begin
  for i := 1 to 12 do
  begin
    Months[i].Free;
  end;
  inherited Destroy;
end;

procedure TMonthlyDataList.FillData(FullDataList: TFullDataList;
   Truncate: Boolean);
var
  i, AMonth: Integer;
  AProbabilityRecord: TProbabilityRecord;
begin
  for i := 1 to 12 do
    Months[i].Clear;
  for i := 0 to FullDataList.Count-1 do
  begin
    AProbabilityRecord := FullDataList[i];
    AMonth := MonthOf(AProbabilityRecord.ActualDate);
    Months[AMonth].Add(AProbabilityRecord);
  end;
{The data is already sorted, do not sort}
  for i := 1 to 12 do
    Months[i].PrepareData(False, Truncate);
end;

{Linear transforms for probability function}

function ProbabilityPaper(Probability: Real;
  PType: TProbabilityPaperType; GEVParameter: Real): Real;
begin
  Result := 0;
  case PType of
{Use the F_1x not the distribution function F_x}
    pdptLinear:
      Result := 1-Probability;
    pdptNormal:
      Result := InvNormalexpr(Probability,0,1);
    pdptGumbelMax:
      Result := -1*Ln(-1*Ln(1-Probability));
    pdptGumbelMin:
      Result := Ln(-1*Ln(Probability));
    pdptWeibull:
      Result := Ln(-1*Ln(Probability));
    pdptGEVMax:
      Result := ( Power( -Ln(1-Probability), -GEVParameter ) - 1 )/
        GEVParameter;
    pdptGEVMin:
      Result := ( Power( -Ln(Probability), GEVParameter ) - 1 )/
        GEVParameter;
  end;
end;

{Inverted linear transforms}

function InvProbabilityPaper(ZValue: Real;
  PType: TProbabilityPaperType; GEVParameter: Real): Real;
begin
  Result := 0;
  case PType of
{Use the F_1x not the distribution function F_x}
    pdptLinear:
      Result := ZValue;
    pdptNormal:
      Result := Normalcdf(ZValue,0,1);
    pdptGumbelMax:
      Result := Exp(-1*Exp(-1*ZValue));
    pdptGumbelMin:
      Result := 1-Exp(-1*Exp(ZValue));
    pdptWeibull:
      Result := 1-Exp(-1*Exp(ZValue));
    pdptGEVMax:
      Result := Exp(-1*Power(1+ZValue*GEVParameter,-1/GEVParameter));
    pdptGEVMin:
      Result := 1-Exp(-Power(1+GEVParameter*ZValue,1/GEVParameter));
  end;
end;

function ProbabilityLinearInterpolate(XValues, YValues: array of Real;
  AProbability: Real; PType: TProbabilityPaperType; GEVParameter: Real): Real;
var
  i: Integer;
  AZValue: Real;
begin
  Result := -1;
  try
    AZValue := ProbabilityPaper(AProbability, PType, GEVParameter);
  except
    on EMathError do
      Exit;
    else
      raise;
  end;
  for i := 0 to Length(XValues)-1 do
  begin
    if XValues[i]>= AZValue then
    begin
      if i < 1 then
      begin
        Exit;
      end else begin
        try
          Result := YValues[i-1]+(YValues[i]-YValues[i-1])/
            (XValues[i]-XValues[i-1])*(AZValue-XValues[i-1]);
          Break;
        except
          on EMathError do
          begin
            Result := -1;
            Exit;
          end;
          else
            raise;
        end;
      end;
    end;
  end;
end;

function ValueLinearInterpolate(XValues, YValues: array of Real;
  AValue: Real; PType: TProbabilityPaperType; GEVParameter: Real): Real;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Length(XValues)-1 do
  begin
    if YValues[i]>= AValue then
    begin
      if i < 1 then
      begin
        Exit;
      end else begin
        try
          Result := XValues[i-1]+(XValues[i]-XValues[i-1])/
            (YValues[i]-YValues[i-1])*(AValue-YValues[i-1]);
          Result := InvProbabilityPaper(Result, PType, GEVParameter);
          Break;
        except
          on EMathError do
          begin
            Result := -1;
            Exit;
          end;
          else
            raise;
        end;
      end;
    end;
  end;
end;

constructor TStatisticalDistribution.Create(DistributionType: TStatisticalDistributionType;
      ADataList: TDataList; GEVShape: Real);
begin
  Create(DistributionType, ADataList, GEVShape, False);
end;

constructor TStatisticalDistribution.Create(DistributionType: TStatisticalDistributionType;
      ADataList: TDataList; GEVShape: Real; AproxParameters: Boolean);
begin
  inherited Create;
  FDistributionType := DistributionType;
  FDataList := ADataList;
  FGEVShape := GEVShape;
  Refresh;
end;

function TStatisticalDistribution.GetParamsCount;
begin
  if FDistributionType in [sdtNormal, sdtLogNormal, sdtExponential, sdtGamma,
    sdtEV1Max, sdtEV2Max, sdtEV1Min, sdtEV3Min, sdtLNormal, sdtLExponential,
    sdtLEV1Max, sdtLEV2Max, sdtLEV1Min, sdtLEV3Min, sdtGEVMaxK, sdtGEVMinK,
    sdtLGEVMaxK, sdtLGEVMinK] then
    Result := 2 else
    Result := 3;
end;

function TStatisticalDistribution.GetMinXAtP(p1, p2, p3: Real): Real;
const
  inf=1e34;
begin
  Result := -inf;
  case FDistributionType of
    sdtNormal: Result := -inf;
    sdtLogNormal: Result := 0;
    sdtGalton: Result := p1;
    sdtExponential, sdtLExponential: Result := p1;
    sdtGamma: Result := 0;
    sdtPearsonIII: Result := p3;
    sdtLogPearsonIII: Result := Exp(p3);
    sdtEV1Max, sdtEV1Min, sdtLEV1Max, sdtLEV1Min: Result := -inf;
    sdtEV2Max, sdtLEV2Max: Result := 0;
    sdtEV3Min, sdtLEV3Min: Result := 0;
    sdtPareto, sdtLPareto: Result := p2*p3;
    sdtGEVMax, sdtLGEVMax, sdtGEVMin, sdtLGEVMin,
    sdtGEVMaxK, sdtLGEVMaxK, sdtGEVMinK, sdtLGEVMinK:
      begin
        if p1>0 then
          Result := p2*(p3-1/p1)
        else
          Result := -inf;
      end;
    else
      Assert(False);
  end;
end;

function TStatisticalDistribution.GetMinX: Real;
begin
  Result := GetMinXAtP(FParam1, FParam2, FParam3);
end;

function TStatisticalDistribution.GetMaxXAtP(p1, p2, p3: Real): Real;
const
  inf=1e34;
begin
  Result := inf;
  case FDistributionType of
    sdtNormal: Result := inf;
    sdtLogNormal: Result := inf;
    sdtGalton: Result := inf;
    sdtExponential, sdtLExponential: Result := inf;
    sdtGamma: Result := inf;
    sdtPearsonIII: Result := inf;
    sdtLogPearsonIII: Result := inf;
    sdtEV1Max, sdtEV1Min, sdtLEV1Max, sdtLEV1Min: Result := inf;
    sdtEV2Max, sdtLEV2Max: Result := inf;
    sdtEV3Min, sdtLEV3Min: Result := inf;
    sdtPareto, sdtLPareto:
      begin
        if p1>0 then
          Result := p2*p3+p2/p1
        else
          Result := inf;
      end;
    sdtGEVMax, sdtLGEVMax, sdtGEVMin, sdtLGEVMin,
    sdtGEVMaxK, sdtLGEVMaxK, sdtGEVMinK, sdtLGEVMinK:
      begin
        if p1<0 then
          Result := p2*(p3-1/p1)
        else
          Result := inf;
      end;
    else
      Assert(False);
  end;
end;

function TStatisticalDistribution.GetMaxX: Real;
begin
  Result := GetMaxXAtP(FParam1, FParam2, FParam3);
end;

function TStatisticalDistribution.GetStatParam1: Real;
begin
  if IsLMomentMethod then
    Result := FLMoment1
  else
  begin
    if FDistributionType = sdtLogPearsonIII then
      Result := FLogMeanValue else
      Result := FMeanValue;
  end;
end;

function TStatisticalDistribution.GetStatParam2: Real;
begin
  if IsLMomentMethod then
    Result := FLMoment2
  else
  begin
    if FDistributionType = sdtLogPearsonIII then
      Result := FLogStandardDeviation else
      Result := FStandardDeviation;
  end;
end;

function TStatisticalDistribution.GetStatParam3: Real;
begin
  if IsLMomentMethod then
    Result := FLMoment3
  else
  begin
    if FDistributionType = sdtLogPearsonIII then
      Result := FLogSkewness else
      Result := FSkewness;
  end;
end;

function TStatisticalDistribution.GetStatParam4: Real;
begin
  if IsLMomentMethod then
    Result := FLMoment4
  else
    Result := FKurtosis;
end;

resourcestring
  rsNormalName = 'Normal';
  rsLNormalName = 'Normal (L-Moments)';
  rsLogNormalName = 'LogNormal';
  rsGaltonName = 'Galton';
  rsExponentialName = 'Exponential';
  rsLExponentialName = 'Exponential (L-Moments)';
  rsGammaName = 'Gamma';
  rsPearsonIIIName = 'Pearson III';
  rsLogPearsonIIIName = 'Log Pearson III';
  rsEV1MaxName = 'EV1-Max (Gumbel)';
  rsEV2MaxName = 'EV2-Max';
  rsLEV1MaxName = 'EV1-Max (Gumbel, L-Moments)';
  rsLEV2MaxName = 'EV2-Max (L-Momments)';
  rsEV1MinName = 'EV1-Min (Gumbel)';
  rsEV3MinName = 'EV3-Min (Weibull)';
  rsLEV1MinName = 'EV1-Min (Gumbel, L-Moments)';
  rsLEV3MinName = 'EV3-Min (Weibull, L-Moments)';
  rsGEVMaxName = 'GEV-Max';
  rsGEVMinName = 'GEV-Min';
  rsLGEVMaxName = 'GEV-Max (L-Moments)';
  rsLGEVMinName = 'GEV-Min (L-Moments)';
  rsGEVMaxKName = 'GEV-Max (kappa specified)';
  rsGEVMinKName = 'GEV-Min (kappa specified)';
  rsLGEVMaxKName = 'GEV-Max (kappa specified, L-Moments)';
  rsLGEVMinKName = 'GEV-Min (kappa specified, L-Moments)';
  rsParetoName = 'Pareto';
  rsLParetoName = 'Pareto (L-Moments)';

function TStatisticalDistribution.GetName;
begin
  case FDistributionType of
    sdtNormal: Result := rsNormalName;
    sdtLogNormal:  Result := rsLogNormalName;
    sdtGalton:  Result := rsGaltonName;
    sdtExponential: Result := rsExponentialName;
    sdtGamma: Result := rsGammaName;
    sdtPearsonIII: Result := rsPearsonIIIName;
    sdtLogPearsonIII: Result := rsLogPearsonIIIName;
    sdtEV1Max: Result := rsEV1MaxName;
    sdtEV2Max: Result := rsEV2MaxName;
    sdtEV1Min: Result := rsEV1MinName;
    sdtEV3Min: Result := rsEV3MinName;
    sdtGEVMax: Result := rsGEVMaxName;
    sdtGEVMin: Result := rsGEVMinName;
    sdtPareto: Result := rsParetoName;
    sdtLNormal: Result := rsLNormalName;
    sdtLExponential: Result := rsLExponentialName;
    sdtLEV1Max: Result := rsLEV1MaxName;
    sdtLEV2Max: Result := rsLEV2MaxName;
    sdtLEV1Min: Result := rsLEV1MinName;
    sdtLEV3Min: Result := rsLEV3MinName;
    sdtLGEVMax: Result := rsLGEVMaxName;
    sdtLGEVMin: Result := rsLGEVMinName;
    sdtLPareto: Result := rsLParetoName;
    sdtGEVMaxK: Result := rsGEVMaxKName;
    sdtGEVMinK: Result := rsGEVMinKName;
    sdtLGEVMaxK: Result := rsLGEVMaxKName;
    sdtLGEVMinK: Result := rsLGEVMinKName;
    else
      Assert(False);
  end;
end;

resourcestring
  rsKappa = 'kappa';
  rsLambda = 'lambda';
  rsPsi = 'psi';
  rsMi = 'mi';
  rsSigma = 'sigma';
  rsMiy = 'miy';
  rsSigmay = 'sigmay';
  rsC = 'c';

function TStatisticalDistribution.GetParam1Name: string;
begin
  case FDistributionType of
    sdtNormal, sdtLNormal: Result := rsMi;
    sdtLogNormal: Result := rsMiy;
    sdtGalton, sdtExponential, sdtLExponential:  Result := rsC;
    sdtEV1Max, sdtLEV1Max, sdtEV1Min, sdtLEV1Min: Result := rsLambda;
    sdtEV3Min, sdtLEV3Min, sdtGEVMax, sdtLGEVMax, sdtGEVMin, sdtLGEVMin,
      sdtPareto, sdtLPareto, sdtEV2Max, sdtLEV2Max, sdtGamma,
      sdtPearsonIII, sdtLogPearsonIII,
      sdtGEVMaxK, sdtGEVMinK, sdtLGEVMaxK, sdtLGEVMinK: Result := rsKappa;
    else
      Assert(False);
  end;
end;

function TStatisticalDistribution.GetParam2Name: string;
begin
  case FDistributionType of
    sdtNormal, sdtLNormal: Result := rsSigma;
    sdtLogNormal: Result := rsMiy;
    sdtGalton, sdtExponential, sdtLExponential:  Result := rsMiy;
    sdtEV1Max, sdtLEV1Max, sdtEV1Min, sdtLEV1Min: Result := rsPsi;
    sdtEV3Min, sdtLEV3Min, sdtGEVMax, sdtLGEVMax, sdtGEVMin, sdtLGEVMin,
      sdtPareto, sdtLPareto, sdtEV2Max, sdtLEV2Max, sdtGamma,
      sdtPearsonIII, sdtLogPearsonIII,
      sdtGEVMaxK, sdtGEVMinK, sdtLGEVMaxK, sdtLGEVMinK: Result := rsLambda;
    else
      Assert(False);
  end;
end;

function TStatisticalDistribution.GetParam3Name: string;
begin
  case FDistributionType of
    sdtNormal, sdtLNormal, sdtGamma: Result := '';
    sdtLogNormal, sdtExponential, sdtLExponential: Result := '';
    sdtGalton:  Result := rsSigmay;
    sdtPearsonIII, sdtLogPearsonIII: Result := rsC;
    sdtEV1Max, sdtLEV1Max, sdtEV1Min, sdtLEV1Min: Result := '';
    sdtEV2Max, sdtLEV2Max, sdtEV3Min, sdtLEV3Min: Result := '';
    sdtGEVMax, sdtLGEVMax, sdtGEVMin, sdtLGEVMin,
      sdtPareto, sdtLPareto,
      sdtGEVMaxK, sdtGEVMinK, sdtLGEVMaxK, sdtLGEVMinK: Result := rsPsi;
    else
      Assert(False);
  end;
end;

function TStatisticalDistribution.GetIsLMomentMethod;
begin
  if FDistributionType in [sdtLNormal, sdtLExponential, sdtLEV1Max, sdtLEV2Max,
    sdtLEV1Min, sdtLEV3Min, sdtLGEVMax, sdtLGEVMin, sdtLPareto,
    sdtLGEVMaxK, sdtLGEVMinK] then
    Result := True else
    Result := False;
end;

function TStatisticalDistribution.GetIsLowerBounded;
begin
  Result := True;
  if FDistributionType in [sdtNormal, sdtEV1Max, sdtEV1Min, sdtLNormal,
    sdtLEV1Max, sdtLEV1Min] then Result := False;
  if FDistributionType in [sdtGEVMin, sdtGEVMax, sdtLGEVMin, sdtLGEVMax,
    sdtGEVMinK, sdtGEVMaxK, sdtLGEVMinK, sdtLGEVMaxK] then
    if FParam1<= 0 then
      Result := False;
end;

function TStatisticalDistribution.GetIsUpperBounded;
begin
  Result := False;
  if FDistributionType in [sdtPareto, sdtLPareto] then
    if FParam1>0 then
      Result := True;
  if FDistributionType in [sdtGEVMin, sdtGEVMax, sdtLGEVMin, sdtLGEVMax,
    sdtGEVMinK, sdtGEVMaxK, sdtLGEVMinK, sdtLGEVMaxK] then
    if FParam1< 0 then
      Result := True;
end;

procedure TStatisticalDistribution.CalcParams;
begin
  FParam1 := 0;
  FParam2 := 0;
  FParam3 := 0;
  case FDistributionType of
    sdtNormal:
      begin
        FParam1 := FMeanValue;
        FParam2 := FStandardDeviation;
      end;
    sdtLogNormal:  LogNormalParam(FMeanValue, FStandardDeviation,
      FParam1, FParam2);
    sdtGalton:  GaltonParam(FMeanValue, FStandardDeviation,
      FSkewness, FParam1, FParam2, FParam3);
    sdtExponential: ExponentialParam(FMeanValue,
      FStandardDeviation, FParam1, FParam2);
    sdtGamma:
      begin
        FParam1 := GammaShape(FMeanValue, FVariance);
        FParam2 := GammaScale(FMeanValue, FVariance);
      end;
    sdtPearsonIII:
      PearsonIIIParam(FMeanValue, FStandardDeviation, FSkewness,
        FParam1, FParam2, FParam3);
    sdtLogPearsonIII:
      PearsonIIIParam(FLogMeanValue, FLogStandardDeviation,
        FLogSkewness, FParam1, FParam2, FParam3);
    sdtEV1Max:
      EV1MaxParam(FMeanValue, FStandardDeviation, FParam1, FParam2);
    sdtEV2Max:
      EV2MaxParam(FMeanValue, FVariance, FParam1, FParam2);
    sdtEV1Min:
      EV1MinParam(FMeanValue, FStandardDeviation, FParam1, FParam2);
    sdtEV3Min:
      begin
        WeibAproxParam(FMeanValue, FVariance, FParam1, FParam2);
        WeibParam(FMeanValue, FVariance, FParam1, FParam2);
      end;
    sdtGEVMax:
      GEVMaxParam(FMeanValue, FStandardDeviation,
        FSkewness, FParam1, FParam2, FParam3);
    sdtGEVMin:
      GEVMinParam(FMeanValue, FStandardDeviation,
        FSkewness, FParam1, FParam2, FParam3);
    sdtPareto:
      ParetoParam(FMeanValue, FStandardDeviation, FSkewness,
        FParam1, FParam2, FParam3);
    sdtLNormal:
      NormalLParam(FLMoment1, FLMoment2, FLMoment3, FParam1, FParam2);
    sdtLExponential:
      ExponentialLParam(FLMoment1, FLMoment2, FLMoment3, FParam1, FParam2);
    sdtLEV1Max:
      EV1MaxLParam(FLMoment1, FLMoment2, FLMoment3, FParam1, FParam2);
    sdtLEV2Max:
      EV2MaxLParam(FLMoment1, FLMoment2, FLMoment3, FParam1, FParam2);
    sdtLEV1Min:
      EV1MinLParam(FLMoment1, FLMoment2, FLMoment3, FParam1, FParam2);
    sdtLEV3Min:
      WeibLParam(FLMoment1, FLMoment2, FLMoment3, FParam1, FParam2);
    sdtLGEVMax:
      GEVMaxLParam(FLMoment1, FLMoment2, FLMoment3, FParam1, FParam2, FParam3);
    sdtLGEVMin:
      GEVMinLParam(FLMoment1, FLMoment2, FLMoment3, FParam1, FParam2, FParam3);
    sdtLPareto:
      ParetoLParam(FLMoment1, FLMoment2, FLMoment3, FParam1, FParam2, FParam3);
    sdtGEVMaxK:
    begin
       FParam1 := FGEVShape;
       GEVMaxParamKS(FMeanValue, FStandardDeviation, FParam1, FParam2, FParam3);
    end;
    sdtGEVMinK:
     begin
       FParam1 := FGEVShape;
       GEVMinParamKS(FMeanValue, FStandardDeviation, FParam1, FParam2, FParam3);
     end;
    sdtLGEVMaxK:
    begin
      FParam1 := FGEVShape;
      GEVMaxLParamKS(FLMoment1, FLMoment2, FLMoment3, FParam1,
        FParam2, FParam3);
     end;
    sdtLGEVMinK:
    begin
      FParam1 := FGEVShape;
      GEVMinLParamKS(FLMoment1, FLMoment2, FLMoment3, FParam1,
         FParam2, FParam3);
    end;
    else
      Assert(False);
  end;
end;

procedure TStatisticalDistribution.CalcAproxParams;
begin
  case FDistributionType of
    sdtEV2Max:
      EV2MaxAproxParam(FMeanValue, FVariance, Fparam1, FParam2);
    sdtEV3Min:
      WeibAproxParam(FMeanValue, FVariance, FParam1, FParam2);
    sdtGEVMax:
      GEVMaxAproxParam(FMeanValue, FStandardDeviation, FSkewness,
        FParam1, FParam2, FParam3);
    sdtGEVMin:
      GEVMinAproxParam(FMeanValue, FStandardDeviation, FSkewness,
        FParam1, FParam2, FParam3);
    else
      CalcParams;
  end;
end;

procedure TStatisticalDistribution.Refresh;
begin
  FMeanValue := FDataList.MeanValue;
  FStandardDeviation := FDataList.StandardDeviation;
  FSkewness := FDataList.Skewness;
  FLogMeanValue := FDataList.LogMeanValue;
  FLogStandardDeviation := FDataList.LogStandardDeviation;
  FLogSkewness := FDataList.LogSkewness;
  FKurtosis := FDataList.Kurtosis;
  FVariance := FDataList.Variance;
  FLMoment1 := FDataList.LMoment1;
  FLMoment2 := FDataList.LMoment2;
  FLMoment3 := FDataList.LMoment3;
  FLMoment4 := FDataList.LMoment4;
  if not FAproxParameters then
    CalcParams
  else
    CalcAproxParams;
end;

function TStatisticalDistribution.pdfValueAtP(p1, p2, p3: Real;
         AXValue: Real): Real;
begin
  Result := 0;
  case FDistributionType of
    sdtNormal: Result := Normalpdf(AXValue, p1, p2);
    sdtLogNormal: Result := Normalpdf(Ln(AXValue), p1, p2)/AXValue;
    sdtGalton: Result := Normalpdf(Ln(AXValue-p3), p1, p2)/(AXValue-p3);
    sdtExponential: Result := Exponentialpdf(AXValue, p2, p1);
    sdtGamma: Result := Gammapdf(AXValue, p1, p2);
    sdtPearsonIII: Result := Gammapdf(AXValue-p3, p1, p2);
    sdtLogPearsonIII: Result := Gammapdf(Ln(AXValue)-p3, p1, p2)/AXValue;
    sdtPareto: Result := Paretopdf(AXValue, p1, p2, p3);
    sdtEV1Max: Result := EV1Maxpdf(AXValue, p1, p2);
    sdtEV2Max: Result := EV2Maxpdf(AXValue, p1, p2);
    sdtEV1Min: Result := EV1Minpdf(AXValue, p1, p2);
    sdtEV3Min: Result := Weibpdf(AXValue, p1, p2);
    sdtGEVMax: Result := GEVMaxpdf(AXValue, p1, p2, p3);
    sdtGEVMin: Result := GEVMinpdf(AXValue, p1, p2, p3);
  else
    Assert(False);
  end;
end;

function TStatisticalDistribution.pdfValue(AXValue: Real): Real;
begin
  Result := pdfValueAtP(FParam1, FParam2, FParam3, AXValue);
end;

function TStatisticalDistribution.cdfValue(AXValue: Real): Real;
begin
  Result := cdfValue(FParam1, FParam2, FParam3, AXValue);
end;

function TStatisticalDistribution.cdfValue(p1, p2, p3, AXValue: Real): Real;
begin
  Result := 0;
  case FDistributionType of
    sdtNormal, sdtLNormal: Result := Normalcdf(AXValue, p1, p2);
    sdtLogNormal: Result := LogNormalcdf(AXValue, p1, p2);
    sdtGalton: Result := Galtoncdf(AXValue, p2, p3, p1);
    sdtExponential,sdtLExponential: Result :=
      Exponentialcdf(AXValue, p2, p1);
    sdtGamma: Result := Gammacdf(AXValue, p1, p2);
    sdtPearsonIII: Result := PearsonIIIcdf(AXValue, p1, p2, p3);
    sdtLogPearsonIII: Result := LogPearsonIIIcdf(AXValue, p1, p2,
      p3);
    sdtEV1Max, sdtLEV1Max: Result :=
      EV1Maxcdf(AXValue, p1, p2);
    sdtEV2Max, sdtLEV2Max: Result := EV2Maxcdf(AXValue, p1, p2);
    sdtEV1Min, sdtLEV1Min: Result := EV1Mincdf(AXValue, p1, p2);
    sdtEV3Min, sdtLEV3Min: Result := Weibcdf(AXValue, p1, p2);
    sdtGEVMax, sdtLGEVMax, sdtGEVMaxK, sdtLGEVMaxK: Result :=
      GEVMaxcdf(AXValue, p1, p2, p3);
    sdtGEVMin, sdtLGEVMin, sdtGEVMinK, sdtLGEVMinK: Result :=
      GEVMincdf(AXValue, p1, p2, p3);
    sdtPareto, sdtLPareto: Result :=
      Paretocdf(AXValue, p1, p2, p3);
    else
      Assert(False);
  end;
end;

function TStatisticalDistribution.InvcdfValue(AFValue: Real): Real;
begin
  Result := 0;
  case FDistributionType of
    sdtNormal, sdtLNormal: Result := InvNormalCdf(AFValue, FParam1, FParam2);
    sdtLogNormal: Result := InvLogNormalcdf(AFValue, FParam1, FParam2);
    sdtGalton: Result := InvGaltoncdf(AFValue, FParam2, FParam3, FParam1);
    sdtExponential,sdtLExponential: Result :=
      InvExponentialcdf(AFValue, FParam2, FParam1);
    sdtGamma: Result := InvGammacdf(AFValue, FParam1, FParam2);
    sdtPearsonIII: Result := InvPearsonIIIcdf(AFValue, FParam1, FParam2, FParam3);
    sdtLogPearsonIII: Result := InvLogPearsonIIIcdf(AFValue, FParam1, FParam2,
      FParam3);
    sdtEV1Max, sdtLEV1Max: Result :=
      InvEV1Maxcdf(AFValue, FParam1, FParam2);
    sdtEV2Max, sdtLEV2Max: Result := InvEV2Maxcdf(AFValue, FParam1, FParam2);
    sdtEV1Min, sdtLEV1Min: Result := InvEV1Mincdf(AFValue, FParam1, FParam2);
    sdtEV3Min, sdtLEV3Min: Result := InvWeibcdf(AFValue, FParam1, FParam2);
    sdtGEVMax, sdtLGEVMax, sdtGEVMaxK, sdtLGEVMaxK: Result :=
      InvGEVMaxcdf(AFValue, FParam1, FParam2, FParam3);
    sdtGEVMin, sdtLGEVMin, sdtGEVMinK, sdtLGEVMinK: Result :=
      InvGEVMincdf(AFValue, FParam1, FParam2, FParam3);
    sdtPareto, sdtLPareto: Result :=
      InvParetocdf(AFValue, FParam1, FParam2, FParam3);
    else
      Assert(False);
  end;
end;

function TStatisticalDistribution.Rand;
begin
  Result := 0;
  case FDistributionType of
    sdtNormal, sdtLNormal: Result :=nrnd(FParam1, FParam2);
    sdtLogNormal: Result := Exp(nrnd(FParam1, FParam2));
    sdtGalton:  Result := FParam1+Exp(nrnd(FParam2, FParam3));
    sdtExponential, sdtLExponential:
      Result := InvExponentialcdf(Random, FParam2, FParam1);
    sdtGamma: Result := grnd(FParam1, FParam2);
    sdtPearsonIII: Result := grnd(FParam1, FParam2)+FParam3;
    sdtLogPearsonIII: Result := Exp(grnd(FParam1, FParam2)+FParam3);
    sdtEV1Max, sdtLEV1Max:  Result := InvEV1Maxcdf(Random, FParam1, FParam2);
    sdtEV2Max, sdtLEV2Max:  Result := InvEV2Maxcdf(Random, FParam1, FParam2);
    sdtEV1Min, sdtLEV1Min: Result := InvEV1Mincdf(Random, FParam1, FParam2);
    sdtEV3Min, sdtLEV3Min: Result := InvWeibcdf(Random, FParam1, FParam2);
    sdtGEVMax, sdtLGEVMax, sdtGEVMaxK, sdtLGEVMaxK:
      Result := InvGEVMaxcdf(Random, FParam1, FParam2, FParam3);
    sdtGEVMin, sdtLGEVMin, sdtGEVMinK, sdtLGEVMinK:
      Result := InvGEVMincdf(Random, FParam1, FParam2, FParam3);
    sdtPareto, sdtLPareto:
      Result := InvParetocdf(Random, FParam1, FParam2, FParam3);
    else
      Assert(False);
  end;
end;

procedure TStatisticalDistribution.MultiplyStatParam(Multiplier1, Multiplier2,
      Multiplier3: Real);
begin
  FMeanValue := FMeanValue * Multiplier1;
  FLogMeanValue := FLogMeanValue * Multiplier1;
  FLMoment1 := FLMoment1 * Multiplier1;
  FStandardDeviation := FStandardDeviation * Multiplier2;
  FVariance := FVariance * Sqr(Multiplier2);
  FLogStandardDeviation := FLogStandardDeviation * Multiplier2;
  FLMoment2 := FLMoment2 * Multiplier2;
  FSkewness := FSkewness * Multiplier3;
  FLogSkewness := FLogSkewness * Multiplier3;
  FLMoment3 := FLMoment3 * Multiplier3;
  if not FAproxParameters then
    CalcParams
  else
    CalcAproxParams;
end;

resourcestring
  rsSpecifiedShapeShouldBeWithin =
    'Specified shape factor for GEV should be within a range of 0.001-0.5';

procedure TStatisticalDistribution.SetGEVShape(AGEVShape: Real);
begin
  if (AGEVShape<0.001) or (AGEVShape>0.5) then
    raise EInvalidArgument.Create(rsSpecifiedShapeShouldBeWithin);
  FGEVShape := AGEVShape;
  if FDistributionType in [sdtGEVMaxK, sdtGEVMinK, sdtLGEVMaxK, sdtLGEVMinK] then
    Refresh;
end;

resourcestring
  rsTooFewForXSq = 'Too few values for implementing the X-Square test';

function TStatisticalDistribution.GetNumberOfClasses;
begin
  Result := ParameterCount+2;
  Result := Max( Result,
    ((FDataList.Count div 5) + Result) div 2);
end;

function TStatisticalDistribution.XSquareTest(ALevel: Real;
  ANumberOfClasses: Integer; var LevelToFail, PearsonParam: Real): Boolean;
var
  AStatParam, AThreshold, ALowClass, AHighClass: Real;
  i, j, CountPoints: Integer;
begin
  if FDataList.Count < ANumberOfClasses then
    raise EImpossibleXSquareTest.Create(rsTooFewForXSq);
  Result := True;
  LevelToFail := 1.0;
  AThreshold := InvXSquareCdf(1-ALevel, ANumberOfClasses- ParameterCount- 1);
  AStatParam := 0;
  for i := 0 to ANumberOfClasses-1 do
  begin
    CountPoints := 0;
    if i >0 then
      ALowClass :=  InvcdfValue(i/ANumberOfClasses)
    else
      ALowClass := -1e35;
    if i <(AnumberOfClasses-1) then
      AHighClass :=  InvcdfValue((i+1)/ANumberOfClasses)
    else
      AHighClass := 1e35;
    for j := 0 to FDataList.Count-1 do
    begin
      if (FDataList[j].Value >= ALowClass) and
        (FDataList[j].Value < AHighClass) then
          Inc(CountPoints);
    end;
    AStatParam := AStatParam + Sqr(CountPoints);
  end;
  AStatParam := AStatParam* (ANumberOfClasses/ FDataList.Count) -
    FDataList.Count;
  PearsonParam := AStatParam;
  LevelToFail := 1- XSquareCdf(AStatParam, ANumberOfClasses- ParameterCount- 1);
  if AStatParam > AThreshold then
    Result := False;
end;

function TStatisticalDistribution.KolmogorovSmirnovTest(ALevel: Real;
  var LevelToFail, Dmax: Real): Boolean;
var
  i: Integer;
  ADistributionF: Real;
begin
  Result := True;
  DMax := 0;
  for i := 0 to FDataList.Count-1 do
  begin
    try
      ADistributionF := cdfValue(FDataList[i].Value);
      DMax := Max(DMax, Abs(ADistributionF-1+FDataList[i].WeibullProbability));
    except
      on EMathError do
        Continue;
      else
        raise;
    end;
  end;
  LevelToFail := KolmogSigLev(FDataList.Count, DMax);
  if LevelToFail < ALevel then
    Result := False;
end;

var AStatisticalDistribution: TStatisticalDistribution;

function ObjectiveFunction(var X: TArrayOfReal): Real;
begin
  Result := AStatisticalDistribution.MLEObjFunc(X);
end;

var
  XMin, XMax, XOpt: TArrayOfReal;
  fopt: Real;
  eval, eval2: Integer;
  optext: string;

procedure TStatisticalDistribution.CalculateMaxLikelihoodParams(min1, min2,
      min3, max1, max2, max3:Real;
      var param1, param2, param3: Real);
var
//  XMin, XMax, XOpt: TArrayOfReal;
//  fopt: Real;
//  eval: Integer;
  Dummy: Boolean;
begin
  AStatisticalDistribution := Self;
  try
      SetLength(XMin, 3);
      SetLength(XMax, 3);
      SetLength(XOpt, 3);
      XOpt[0] := param1;
      XOpt[1] := param2;
      XOpt[2] := param3;
      XMin[0] := min1;
      XMin[1] := min2;
      XMin[2] := min3;
      XMax[0] := max1;
      XMax[1] := max2;
      XMax[2] := max3;
      optext := '';
      eval2 := 0;
      foptMaxEval := MLEObjFunc(XOpt);
      AnnealSimplex(ParameterCount, ParameterCount*4, XMin, XMax, Xopt,
        ObjectiveFunction, fopt, eval, 0.005, 1000*ParameterCount, 0.99, 0.10,
        2, 5, True, False, Dummy);
      param1 := XOpt[0];
      param2 := XOpt[1];
      param3 := XOpt[2];
      Clipboard.AsText := optext;

  finally
    {Dereference}
    AStatisticalDistribution := nil;
  end;
end;

function TStatisticalDistribution.MLEObjFunc(X: TArrayOfReal): Real;
var
  i, valid_count: Integer;
  apdf, ax: Real;
begin
  Result := 0;
  valid_count := 0;
  for i := 0 to FDataList.Count-1 do
  begin
    ax := FDataList[i].Value;
    if ax>=GetMaxXAtP(X[0], X[1], X[2]) then
    begin
      Continue;
    end
    else if ax<=GetMinXAtP(X[0], X[1], X[2]) then
    begin
      Continue;
    end;
    try
      apdf := pdfValueAtP(X[0], X[1], X[2], ax);
      Inc(valid_count);
    except
      on EMathError do
      begin
        Result := foptMaxEval*1.01;
        Break;
      end;
    else
      raise;
    end;
    if (apdf>0) and (apdf<1) then
    begin
      Result := Result - Ln( apdf );
    end else begin
      Result := foptMaxEval*1.01;
      Break;
    end;
  end;
  if valid_count>0 then
    Result := Result * FDataList.Count / valid_count
  else
    Result := foptMaxEval*1.01;
  if foptMaxEval<Result then
    foptMaxEval := Result;
  if True {eval > eval2} then
  begin
    optext := optext+
        IntToStr(eval)+#9+FloatToStr(X[0])+#9+
        FloatToStr(X[1])+#9+FloatToStr(X[2])+#9+
        FloatToStr(Result)+#13#10;
    eval2 := eval;
  end;
end;

{ TStatisticalDistributionsList }

constructor TStatisticalDistributionsList.Create;
begin
  inherited Create;
  FDistributionList := TObjectList.Create;
end;

destructor TStatisticalDistributionsList.Destroy;
begin
  FDistributionList.Free;
  inherited Destroy;
end;

function TStatisticalDistributionsList.Get(Index: Integer):
  TStatisticalDistribution;
begin
  Result := TStatisticalDistribution(FDistributionList[Index]);
end;

function TStatisticalDistributionsList.GetCount: Integer;
begin
  Result := FDistributionList.Count;
end;

function TStatisticalDistributionsList.FindFirstDistributionOf(
  ADistributionType: TStatisticalDistributionType): TStatisticalDistribution;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count-1 do
  begin
    if Items[i].DistributionType = ADistributionType then
    begin
      Result := Items[i];
      Exit;
    end;
  end;
end;

function TStatisticalDistributionsList.GetFirst: TStatisticalDistribution;
begin
  Result := TStatisticalDistribution(FDistributionList.First);
end;

function TStatisticalDistributionsList.GetLast: TStatisticalDistribution;
begin
  Result := TStatisticalDistribution(FDistributionList.Last);
end;

procedure TStatisticalDistributionsList.Add(ADistributionType: TStatisticalDistributionType;
      ADataList: TDataList; GEVShape: Real);
var
  AStatisticalDistribution: TStatisticalDistribution;
begin
  AStatisticalDistribution := nil;
  try
    AStatisticalDistribution := TStatisticalDistribution.Create(
      ADistributionType, ADataList, GEVShape);
    FDistributionList.Add(AStatisticalDistribution);
    AStatisticalDistribution := nil;
  except
    on EMathError do
      raise;
    else
    begin
      AStatisticalDistribution.Free;
      raise;
    end;
  end;
end;

procedure TStatisticalDistributionsList.Clear;
begin
  FDistributionList.Clear;
end;

end.
