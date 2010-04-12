{******************************************************************}
{                                                                  }
{  Thelma library                                                  }
{                                                                  }
{  Copyright (c) 2005    National Technical University of Athens   }
{                                                                  }
{******************************************************************}

{** A Simple Hydrological Model.
}
unit hydromodel;

interface

uses ts, dates, optim_routines;

{** One step simulation of a basin's hydrological processes.
    Description of parameters:<p>
    Model inputs / outputs (all given in equivalent depths (mm)):<p>
    -------------------------------------------------------------<p>
    rainfall       = areal rainfall<p>
    rotevap        = potential evapostranspiration<p>
    pumping        = demand for groundwater abstractions<p>
    K              = soil moistrure storage capacity<p>
    H1             = threshold for interflow generation (H1 < K)<p>
    H2             = threshold for baseflow generation<p>
    epsilon        = maximum fraction of precipitation that can be directly
                     evaporated<p>
    kapa           = coefficient of direct runoff generation<p>
    lamda          = recession rate for interflow (= time-lagged component
                     of surface flow)<p>
    mi             = recession rate for peroclation<p>
    ksi            = recession rate for baseflow (= flow of springs)<p>
    phi            = recession rate for outflow to neigboring aquifers
                     or the sea<p>
    soil_storage   = actual value of soil moisture storage
                     (input St, output St+1)<p>
    ground_storage = actual value of groundwater storage
                     (input Gt, output Gt+1)<p>
    evaporation    = real evapotranspiration (= direct evaporation +
                     soil evaporation)<p>
    percolation    = percolation to the aquifer<p>
    runoff         = runoff (= direct runoff + surface saturated flow +
                     interflow + baseflow)<p>
    outflow        = water losses to neigboring aquifers or the sea<p>
    Some conditions should be fullfilled or else exceptions (EInvalidArgument)
    are raised that is: All scalar parameters should be between 0 and 1,
    storage should be between 0 and storage capacity and thresholds should
    be between 0 and storage capacity.<p>
    @author Andreas Efstratiadis
    @SeeAlso <See Routine=CalcBasinSim>
}
procedure BasinSim(rainfall, potevap, pumping: Real;
                     K, H1, H2, epsilon, kapa, lamda, mi, ksi, phi: Real;
                     var soil_storage, ground_storage, evaporation, percolation,
                     runoff, outflow: Real);
type
  TBasinSimParametersType = (bsptSoilStorage, bsptH1Ratio, bsptH2, bsptEpsilon,
    bsptKappa, bsptLambda, bsptMi, bsptKsi, bsptPhi,
    bsptInitialSoilStorageRatio, bsptInitialGroundStorage);
  TBasinSimParametersTypes = set of TBasinSimParametersType;
const bspcParams: array [0..10] of  TBasinSimParametersType =
(bsptSoilStorage, bsptH1Ratio, bsptH2, bsptEpsilon,
    bsptKappa, bsptLambda, bsptMi, bsptKsi, bsptPhi,
    bsptInitialSoilStorageRatio, bsptInitialGroundStorage);
type
//  TArrayOfReal = array of Real;
//  T2DArrayOfReal = array of array of Real;
//  TIntArray = array of Integer;
  TMathFuncMult = function(var X: TArrayOfReal): Real of object;
{** DetermCoeff is an objective function.
    DetermCoeff is used in order to callibrate the simple hydro model
    by the annealing-simplex method.
    @author Andreas Efstratiadis
    @SeeAlso <See Routine=AnnealSimplex>
    @SeeAlso <See Routine=CalcBasinOpt>
}
function DetermCoeff(x, y: TTimeseries; CommonPeriod: TDateTimeList;
  istart, iend: Integer): Real;
type
{** A structure to store the parameters for Basin Simulation Process.
    OptParams, fopt and eval are used for the callibration process.
    @author Stefanos
    @SeeAlso <See Routine=CalcBasinOpt>
    @SeeAlso <See Routine=CalcBasinSim>
    @SeeAlso <See Class=TObjFunction>
}
  TBasinSimParams = record
    Params: array[TBasinSimParametersType] of Real;
    MinParams: array[TBasinSimParametersType] of Real;
    MaxParams: array[TBasinSimParametersType] of Real;
    OptParams: array[TBasinSimParametersType] of Boolean;
    fopt: Real;
    eval: Integer;
  end;
type
  TBSProgressIndicator = procedure(Params: TBasinSimParams;
    index, total: Integer) of object;
type
{** A structure to store the timeseries for Basin Simulation Process.
    @author Stefanos
    @SeeAlso <See Routine=CalcBasinOpt>
    @SeeAlso <See Routine=CalcBasinSim>
    @SeeAlso <See Class=TObjFunction>
}
  TBasinSimTimeseries = record
  Rainfall: TTimeseries;
  Potevap: TTimeseries;
  Pumping: TTimeseries;
  MeasuredRunoff: TTimeseries;
  SoilStorage: TTimeseries;
  GroundStorage: TTimeseries;
  Evaporation: TTimeseries;
  Percolation: TTimeseries;
  Runoff: TTimeseries;
  Outflow: TTimeseries;
  Error: TTimeseries;
end;
type
{** This class is used to represent the objective function.
    @author Stefanos
    @SeeAlso <See Routine=CalcBasinOpt>
}
  TObjFunction = class(TObject)
  private
    FIndex: Integer;
    FFopt: Real;
    FTimeseries: TBasinSimTimeseries;
    FParams: TBasinSimParams;
    FCommonPeriod: TDateTimeList;
    FMeasuredPeriod: TDateTimeList;
    FProgress: TBSProgressIndicator;
    function GetXMin: TArrayOfReal;
    function GetXMax: TArrayOfReal;
    function GetParamCount: Integer;
    procedure SetParams(var X: TArrayOfReal);
  public
{** Create initialize the class object.
    Set BasinSimParams to min/max values. ATimeseries contains the
    timeseries used for the simulation, namely, measured runoff,
    rainfall, pumping and potential evapotranspiration.
    You should set also the ProgressIndicator Property.
}
    constructor Create(ATimeseries: TBasinSimTimeseries;
      AParams: TBasinSimParams; ADateTimeList, AMeasuredPeriod: TDateTimeList;
      ProgressIndicator: TBSProgressIndicator);
{** The Objective function itself. FObf returns the DetermCoeff function
    after a simulation process.
    @SeeAlso <See Routine=CalcBasinSim>
    @SeeAlso <See Routine=DetermCoeff>
}
    function FObf(var X: TArrayOfReal): Real;
{** The Timeseries used by the simulation process.
}
    property Timeseries: TBasinSimTimeseries read FTimeseries write FTimeseries;
{** The parameters record used by the simulation process.
}
    property Params: TBasinSimParams read FParams write FParams;
{** An array containg the Minimum values for the parameters.
    Used by the AnnealSimplex algorithm.
    @SeeAlso <See Routine=AnnealSimplex>
}
    property XMin: TArrayOfReal read GetXMin;
{** An array containg the Maximum values for the parameters.
    Used by the AnnealSimplex algorithm.
    @SeeAlso <See Routine=AnnealSimplex>
}
    property XMax: TArrayOfReal read GetXMax;
{** The number of parameters to optimize.
}
    property ParamCount: Integer read GetParamCount;
  end;
{** Run an optimization process on the parameters of Basin Simulation.
    Normally you don't have to call ths version; use the other overloaded
    version of CalcBasinOpt, witch will determine the common period and
    call this one.<p>
    @author Stefanos
    @SeeAlso <See Routine=AnnealSimplex>
    @SeeAlso <See Routine=DetermCoeff>
    @SeeAlso <See Routine=CalcBasinOpt>
    @SeeAlso <See Routine=CalcBasinSim>
}
procedure CalcBasinOpt(Timeseries: TBasinSimTimeseries;
  ADateTimeList, AMeasuredPeriod: TDateTimeList; var Params: TBasinSimParams;
  ProgressIndicator: TBSProgressIndicator; var StopOptim: Boolean); overload;
{** Run an optimization process in order to calibrate the Hydrological model.
    CalcBasinOpt use the AnnealSimplex algorithm by minimizing the
    DetermCoeff objective function.
    @author Stefanos
    @SeeAlso <See Routine=AnnealSimplex>
    @SeeAlso <See Routine=DetermCoeff>
    @SeeAlso <See Routine=CalcBasinOpt>
    @SeeAlso <See Routine=CalcBasinSim>
}
procedure CalcBasinOpt(Timeseries: TBasinSimTimeseries;
  AMeasuredPeriod: TDateTimeList; var Params: TBasinSimParams;
  ProgressIndicator: TBSProgressIndicator; var StopOptim: Boolean); overload;
{** Run a Simulation of Basin's hydrological processed, based on timeseries.
    Normally you don't have to call this version; use the other overloaded
    version of CalcBasinSim, which will determine the common period and
    call this one.<p>
    @author Stefanos
    @SeeAlso <See Routine=BasinSim>
}
procedure CalcBasinSim(Timeseries: TBasinSimTimeseries; Params: TBasinSimParams;
                         ADateTimeList: TDateTimeList); overload;
{** Run a Simulation of Basin's hydrological processed, based on timeseries.
    A rainfall timeseries should be supplied at least. The assumed input
    timeseries are rainfall, potential evapotranspiration (PotevapTS),
    pumping and measured runoff. Calculations are considered for the common
    period of (Rainfall, Evapotranspiration and Pumping) if all three
    timeseries are supplied.<p>
    You may provide a measured runoff time series in order to calculate the
    "error timeseries" between measured and actual (calculated) runoff.<p>
    @author Stefanos
    @SeeAlso <See Routine=BasinSim>
}
procedure CalcBasinSim(Timeseries: TBasinSimTimeseries;
  Params: TBasinSimParams); overload;

implementation

uses math, contnrs, tsprocess, sysutils;

resourcestring
  rsInvalidParameter = 'Parameters should be between 0 and 1';
  rsHGreaterThanZero = 'Threshold levels should be greater than 0';
  rsHLessThanStorage =
    'Threshold for interflow generation should be smaller'+
      ' than Soil Storage Capacity';
  rsInvalidValueForStorage =
    'Invalid Value for Soil or Ground Storage (Should be within 0 and'+
      ' Storage Capacity';

procedure BasinSim(rainfall, potevap, pumping: Real; K, H1, H2, epsilon, kapa,
                     lamda, mi, ksi, phi: Real;
                     var soil_storage, ground_storage, evaporation, percolation,
                     runoff, outflow: Real);
const
  n = 10;
var
  DRain, DPotEvap, DPump: Real;
  DQd, DQs, DEd, DEs, DQi, DPerc, DQb, DOut: Real;
  i: Integer;
begin
  if (epsilon<0) or (epsilon>1) or (kapa<0) or (kapa>1) or (mi<0) or (mi>1) or
    (lamda<0) or (lamda>1) or (ksi<0) or (ksi>1) or (phi<0) or (phi>1) then
    raise EInvalidArgument.Create(rsInvalidParameter);
  if (H1<0) or (H2<0) then
    raise EInvalidArgument.Create(rsHGreaterThanZero);
  if (H1>K) then
    raise EInvalidArgument.Create(rsHLessThanStorage);
  if (soil_storage<0) or (soil_storage>K) or (ground_storage<0) then
    raise EInvalidArgument.Create(rsInvalidValueForStorage);
  evaporation:=0;
  percolation:=0;
  runoff:=0;
  outflow:=0;
  DRain:=rainfall/n;
  DPotEvap:=potevap/n;
  DPump:=pumping/n;
  for i:=1 to n do
  begin
    DEd:=Min(epsilon*DRain, DPotEvap); {direct evapotranspiration}
    DQd:= kapa*(DRain-DEd);            {direct runoff}
{Soil moistrure reservoir processes}
    soil_storage:=soil_storage+DRain-DEd-DQd;
{quick flow, due to saturation of soil moisture zone}
    DQs:=max(0, soil_storage-K);
    soil_storage:=soil_storage-DQs;
{soil evaporation, assumed proportional to the soil storage percentage}
    if Abs(K)>0.001 then
      DEs := max(0, soil_storage*(1-exp((DEd-DPotEvap)/K)))
    else
      DEs := 0;
    soil_storage:=soil_storage-DEs;
{lagged-time component of surface flow}
    DQi:=lamda*Max(0, soil_storage-H1);
    soil_storage:=soil_storage-DQi;
{percolation = inflow to groundwater reservoir}
    DPerc:=mi*soil_storage;
    soil_storage:=Max(0,soil_storage-DPerc);
{Groundwater reservoir processes}
    ground_storage:=Max(0,ground_storage+DPerc-DPump);
{baseflow}
    DQb:=ksi*max(0, ground_storage-H2);
    ground_storage:=ground_storage-DQb;
{outflow}
    DOut:=phi*ground_storage;
    ground_storage:=Max(0,ground_storage-DOut);
    evaporation := evaporation + DEd + DEs;
    percolation := percolation + DPerc;
    runoff      := runoff + DQd + DQs + DQi + DQb;
    outflow     := outflow + DOut;
  end;
end;

resourcestring
  rsMeasuredValuesShouldBeMore =
    'Measured values should be at least m+1 (m the number of parameters, m=';

constructor TObjFunction.Create(ATimeseries: TBasinSimTimeseries;
  AParams: TBasinSimParams; ADateTimeList, AMeasuredPeriod: TDateTimeList;
  ProgressIndicator: TBSProgressIndicator);
begin
  inherited Create;
  FIndex := 0;
  FFopt := 1e30;
  Timeseries := ATimeseries;
  FProgress := ProgressIndicator;
  Params := AParams;
  FCommonPeriod := ADateTimeList;
  FMeasuredPeriod := AMeasuredPeriod;
  if FMeasuredPeriod.Count<ParamCount+1 then
    raise Exception.Create(rsMeasuredValuesShouldBeMore+
      IntToStr(ParamCount)+')');
end;

function TObjFunction.GetXMin: TArrayOfReal;
var
  i,j : Integer;
begin
  SetLength(Result, ParamCount);
  j := 0;
  for i := 0 to 10 do
  begin
    if FParams.OptParams[bspcParams[i]] then
    begin
      Result[j] := Fparams.MinParams[bspcParams[i]];
      Inc(j);
    end;
  end;
end;

function TObjFunction.GetXMax: TArrayOfReal;
var
  i,j : Integer;
begin
  SetLength(Result, ParamCount);
  j := 0;
  for i := 0 to 10 do
  begin
    if FParams.OptParams[bspcParams[i]] then
    begin
      Result[j] := Fparams.MaxParams[bspcParams[i]];
      Inc(j);
    end;
  end;
end;

function TObjFunction.GetParamCount: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to 10 do
  begin
    if FParams.OptParams[bspcParams[i]] then
      Inc(Result);
  end;
end;

procedure TObjFunction.SetParams(var X: TArrayOfReal);
var
  i,j : Integer;
begin
  j := 0;
  for i := 0 to 10 do
  begin
    if FParams.OptParams[bspcParams[i]] then
    begin
      FParams.Params[bspcParams[i]] := X[j];
      Inc(j);
    end;
  end;
end;

function TObjFunction.FObf(var X: TArrayOfReal): Real;
var
  AList: TObjectList;
  CommonPeriod: TDateTimeList;
  i: Integer;
begin
  SetParams(X);
  CalcBasinSim(Timeseries, Params, FCommonPeriod);
  AList := nil;
  CommonPeriod := nil;
  try
    AList := TObjectList.Create(False);
    AList.Add(Timeseries.Runoff);
    AList.Add(Timeseries.MeasuredRunoff);
    CommonPeriod := GetCommonPeriod(AList, 0);
    for i := CommonPeriod.Count-1 downto 0 do
    begin
      if FMeasuredPeriod.IndexOf(CommonPeriod[i])<0 then
        CommonPeriod.Remove(CommonPeriod[i]);
    end;
    Result := -DetermCoeff( Timeseries.Runoff, Timeseries.MeasuredRunoff,
        CommonPeriod, 0, CommonPeriod.Count-1);
    Inc(FIndex);
    if Result < FFopt then
    begin
      FFopt := Result;
      FParams.fopt := FFopt;
      FParams.eval := FIndex;
      FProgress(FParams, FIndex, 4*ParamCount);
    end;
  finally
    AList.Free;
    CommonPeriod.Free;
  end;
end;

procedure CalcBasinSim(Timeseries: TBasinSimTimeseries;
                         Params: TBasinSimParams); overload;
var
  AList: TObjectList;
  CommonPeriod: TDateTimeList;
begin
  AList := nil;
  CommonPeriod := nil;
  try
    AList := TObjectList.Create(False);
    with Timeseries do
    begin
      if Rainfall<>nil then AList.Add(Rainfall);
      if Potevap<>nil then AList.Add(Potevap);
      if Pumping<>nil then AList.Add(Pumping);
    end;
    CommonPeriod := GetCommonPeriod(AList, 0);
    CalcBasinSim(Timeseries, Params, CommonPeriod);
  finally
    Alist.Free;
    CommonPeriod.Free;
  end;
end;

resourcestring
  rsRainfallMustBeSupplied = 'Rainfall time series must be supplied';
  rsIncompatibleTimesteps = 'All time series should be of the same time step';
  rsRecordsShouldBeConsecutive =
    'Records for common period of time series should be consecutive (';
  rsShouldHaveAtLeastTwoRecords =
    'Common period of time series should have at least 2 records';

procedure CalcBasinSim(Timeseries: TBasinSimTimeseries; Params: TBasinSimParams;
                       ADateTimeList: TDateTimeList); overload;
  procedure CheckCommonPeriodConsistency;
  var
    i: Integer;
    Difference: Integer;
  begin
    if ADateTimeList.Count < 2 then
      raise Exception.Create(rsShouldHaveAtLeastTwoRecords);
    if Timeseries.Rainfall.TimeStep>=tstMonthly then
      Difference := Timeseries.Rainfall.TimeStep.LengthMonths else
      Difference := Timeseries.Rainfall.TimeStep.LengthMinutes*60;
{    case Timeseries.Rainfall.TimeStep of
      tstMonthly: Difference := 1;
      tstAnnual: Difference := 12
      else
      Difference := DiffInSecs(ADateTimeList[1], ADateTimeList[0]);
    end;}
    for i := 1 to ADateTimeList.Count-1 do
    begin
//      case Timeseries.Rainfall.TimeStep of
//        tstMonthly, tstAnnual:
        if Timeseries.Rainfall.TimeStep>=tstMonthly then
        begin
          if Abs(DiffInSecs(ADateTimeList[i],
            IncMonth(ADateTimeList[i-1],Difference)))>1 then
              raise Exception.Create(rsRecordsShouldBeConsecutive +
                DateTimeToStr(ADateTimeList[i-1])+', '+
                  DateTimeToStr(ADateTimeList[i])+')');
        end
        else begin
          if Abs(DiffInSecs(ADateTimeList[i],
            ADateTimeList[i-1])-Difference)>1 then
            raise Exception.Create(rsRecordsShouldBeConsecutive +
              DateTimeToStr(ADateTimeList[i-1])+', '+
                DateTimeToStr(ADateTimeList[i])+')');
        end;
    end;
  end;

  procedure AssertTsTS(FirstTimeseries, SecondTimeseries: TTimeseries);
  begin
    Assert(FirstTimeseries.TimeStep=SecondTimeseries.TimeStep,
      rsIncompatibleTimesteps+': '+#13#10+
      FirstTimeseries.Title+' and '+SecondTimeseries.Title);
  end;

var
  i: Integer;
  ADate: TDateTime;
  ARainfall, APotevap, APumping: Real;
  ASoilStorage, AGroundStorage, AEvaporation, APercolation, ARunoff, AOutFlow:
    Real;
begin
  with Timeseries do
  begin
    Assert(Rainfall<>nil, rsRainfallMustBeSupplied);
    if Potevap<>nil then AssertTsTS(Rainfall, Potevap);
    if Pumping<>nil then AssertTsTS(Rainfall, Pumping);
    if SoilStorage<>nil then AssertTsTS(Rainfall, SoilStorage);
    if GroundStorage<>nil then AssertTsTS(Rainfall, GroundStorage);
    if Evaporation<>nil then AssertTsTS(Rainfall, Evaporation);
    if Percolation<>nil then AssertTsTS(Rainfall ,Percolation);
    if Runoff<>nil then AsserttsTS(Rainfall, Runoff);
    if MeasuredRunoff<>nil then AssertTsTS(Rainfall, MeasuredRunoff);
    if Error<>nil then AssertTsTS(Rainfall, Error);
    if Outflow<>nil then AssertTsTS(Rainfall, Outflow);
    if SoilStorage<>nil then SoilStorage.Clear;
    if GroundStorage<>nil then GroundStorage.Clear;
    if Evaporation<>nil then Evaporation.Clear;
    if Percolation<>nil then Percolation.Clear;
    if Runoff<>nil then Runoff.Clear;
    if Outflow<>nil then Outflow.Clear;
    if Error<>nil then Error.Clear;
  end;
  ASoilStorage := Params.Params[bsptInitialSoilStorageRatio]*
    Params.Params[bsptSoilStorage];
  AGroundStorage := Params.Params[bsptInitialGroundStorage];
  CheckCommonPeriodConsistency;  
  for i := 0 to ADateTimeList.Count-1 do
  begin
    with Timeseries do
    begin
      ADate := ADateTimeList.Items[i];
      ARainfall := Rainfall[Rainfall.IndexOf(ADate)].AsFloat;
      if Potevap<>nil then
        APotevap := Potevap[Potevap.IndexOf(ADate)].AsFloat else
        APotevap := 0;
      if Pumping<>nil then
        APumping := Pumping[Pumping.IndexOf(ADate)].AsFloat else
        APumping := 0;
      with Params do
        BasinSim(ARainfall, APotevap, APumping, Params[bsptSoilStorage],
         Params[bsptH1Ratio]*Params[bsptSoilStorage], Params[bsptH2],
         Params[bsptEpsilon], Params[bsptKappa], Params[bsptLambda],
         Params[bsptMi], Params[bsptKsi], Params[bsptPhi], ASoilStorage,
         AGroundStorage, AEvaporation, APercolation, ARunoff, AOutflow);
      if SoilStorage<>nil then
        SoilStorage.Add(ADate, False, ASoilStorage, '' , msNew);
      if GroundStorage<>nil then
        GroundStorage.Add(ADate, False, AGroundStorage, '', msNew);
      if Evaporation<>nil then
        Evaporation.Add(ADate, False, AEvaporation, '', msNew);
      if Percolation<>nil then
        Percolation.Add(ADate, False, APercolation, '', msNew);
      if Runoff<>nil then
        Runoff.Add(ADate, False, ARunoff, '', msNew);
      if Outflow<>nil then
        Outflow.Add(ADate, False, AOutflow, '', msNew);
      if (MeasuredRunoff<>nil) and (Error<>nil) then
        if MeasuredRunoff.IndexOf(ADate)>-1 then
          if MeasuredRunoff[MeasuredRunoff.IndexOf(ADate)].IsNull then
            Error.Add(ADate, True, 0, '', msNew)
          else
            Error.Add(ADate, False,
              MeasuredRunoff[MeasuredRunoff.IndexOf(ADate)].AsFloat-ARunoff,
                '', msNew);
    end;
  end;
end;

resourcestring
  rsDateIndexOutOfBounds = ' Date index out of bounds';
  rsCouldNotCalculateDet =
    'Cound not calculate determination coefficient for empty time series';

function DetermCoeff(x, y: TTimeseries; CommonPeriod: TDateTimeList;
  istart, iend: Integer): Real;
var
  i: Integer;
  count_num: Integer;
  sum1, sum2, mx: Real;
begin
  Assert(istart>-1, rsDateIndexOutOfBounds);
  Assert(iend<CommonPeriod.Count, rsDateIndexOutOfBounds);
  if CommonPeriod.Count<=0 then
    raise EInvalidArgument.Create(rsCouldNotCalculateDet);
  count_num:=0;
  mx:=0;
  for i:= istart to iend do
    if (not x[x.IndexOf(CommonPeriod.Items[i])].IsNull) and
      (not y[y.IndexOf(CommonPeriod.Items[i])].IsNull) then
    begin
      Inc(count_num);
      mx:=mx+x[x.IndexOf(CommonPeriod.Items[i])].AsFloat;
    end;
  mx:=mx/count_num;

  sum1:=0; sum2:=0;
  for i:=istart to iend do
  begin
    If (not x[x.IndexOf(CommonPeriod.Items[i])].IsNull) and
      (not y[y.IndexOf(CommonPeriod.Items[i])].IsNull) then
    begin
      sum1:=sum1+Sqr( x[x.IndexOf(CommonPeriod.Items[i])].AsFloat-
        y[y.IndexOf(CommonPeriod.Items[i])].AsFloat );
      sum2:=sum2+Sqr(x[x.IndexOf(CommonPeriod.Items[i])].AsFloat-mx);
    end;
  end;
  try
    Result:=1 - sum1/sum2;
  except
    on EMathError do
      Result := -1e30;
    else
      raise;
  end;
end; {function DetermCoeff}

procedure CalcBasinOpt(Timeseries: TBasinSimTimeseries;
  AMeasuredPeriod: TDateTimeList; var Params: TBasinSimParams;
    ProgressIndicator: TBSProgressIndicator; var StopOptim: Boolean);
var
  AList: TObjectList;
  CommonPeriod: TDateTimeList;
begin
  AList := nil;
  CommonPeriod := nil;
  try
    AList := TObjectList.Create(False);
    with Timeseries do
    begin
      if Rainfall<>nil then AList.Add(Rainfall);
      if Potevap<>nil then AList.Add(Potevap);
      if Pumping<>nil then AList.Add(Pumping);
    end;
    CommonPeriod := GetCommonPeriod(AList, 0);
    CalcBasinOpt(Timeseries, AMeasuredPeriod, CommonPeriod, Params,
      ProgressIndicator, StopOptim);
  finally
    AList.Free;
    CommonPeriod.Free;
  end;
end;

var
  Obf: TObjFunction;

function ObjectiveFunction(var X: TArrayOfReal): Real;
begin
  Result := Obf.FObf(X);
end;

resourcestring
  rsShouldAtLeastOneParameter =
    'You should set at least one parameter to optimize';

procedure CalcBasinOpt(Timeseries: TBasinSimTimeseries;
  ADateTimeList, AMeasuredPeriod: TDateTimeList; var Params: TBasinSimParams;
  ProgressIndicator: TBSProgressIndicator; var StopOptim: Boolean);
var
  Xopt: TArrayOfReal;
  fopt: Real;
  eval: Integer;

begin
  RandSeed := 1973;
  Obf := nil;
  try try
    Obf := TObjFunction.Create(Timeseries, Params, ADateTimeList,
      AMeasuredPeriod, ProgressIndicator);
    if Obf.ParamCount<=0 then
      raise Exception.Create(rsShouldAtLeastOneParameter);
    SetLength(Xopt, Obf.ParamCount);    
    with Obf do
      AnnealSimplex(ParamCount, ParamCount*4, XMin, XMax, Xopt,
        ObjectiveFunction, fopt, eval, 0.005, 1000*ParamCount, 0.99, 0.10, 2, 5,
          False, False, StopOptim);
    Obf.SetParams(Xopt);
    Params := Obf.Params;
    Params.fopt := fopt;
    Params.eval := eval;
  finally
    Obf.Free;
  end;
  except
    SetLength(Xopt,0);
    raise;
  end;
end;

end.
