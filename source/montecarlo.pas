{******************************************************************}
{                                                                  }
{  Thelma library                                                  }
{                                                                  }
{  Copyright (c) 2004   National Technical University of Athens    }
{                                                                  }
{******************************************************************}

{** Monte carlo statistical simulation functions. }
unit montecarlo;

interface

uses statprocesses, SysUtils;

type
  TMCProgress = procedure(var Stop: Boolean) of object;

  {** Exception for stop by user
  }
  EStopByUser = class(Exception);

{** MonteCarloLimits calculates upper and lower confidence limits
    by Monte Carlo method. Results are returned by reference to
    UpperSampleLimit and LowerSampleLimit for the sample limits
    and to UpperMCLimit and LowerMCLimit for the actual confidence
    intervals. Specify the non-excedence probability value by the
    AFValue parameter (taking values from 0 to 1).
    MCCount is the size of the
    simulation points. MCConfidenceLevel is the level of confidence
    between 0 and 1.
    ADistribution should be a TStatisticalDistribution holding
    the original TDataList. Sample count is TDataList.Count.
    Other parameters are representing statistical properties.
}
procedure MonteCarloLimits(AFValue: Real;
  ADistribution: TStatisticalDistribution; MCCount: Integer;
  MCConfidenceLevel: Real; var UpperSampleLimit, LowerSampleLimit,
  UpperMCLimit, LowerMCLimit: Real; AProgressIndicator: TMCProgress); overload;

{** In this overloaded version of MonteCarloLimits you must
    specify the sample size (SampleCount).
}
procedure MonteCarloLimits(AFValue: Real;
  ADistribution: TStatisticalDistribution; MCCount, SampleCount: Integer;
  MCConfidenceLevel: Real; var UpperSampleLimit, LowerSampleLimit,
  UpperMCLimit, LowerMCLimit: Real; AProgressIndicator: TMCProgress); overload;

implementation

uses matrix;

resourcestring
  rsProcessTerminatedByUser = 'Process terminated by user';

{A function to interpolate between the list of values
derived from simulations}
function InterpolListItem (const l: TVector; alpha: Real): Real;
var
  k: Integer;
  m: Real;
  n: Integer;
begin
  n := l.Size;
  m := alpha*n;
  k := trunc (m);
  m := m - k;
  if k <= 0 then
    Result := l[1]
  else if k >= n - 1 then
    Result := l[n]
  else
    Result := l[k + 1] + (l[k + 2] - l[k + 1])* m;
end;

function GetRandom(ADistribution: TStatisticalDistribution): Real;
begin
  try
    Result := ADistribution.Rand;
  except
    on EMathError do
      Result := GetRandom(ADistribution);
    else
      raise;
  end;
end;

const
  ErrorValue = -999999;

{Function to fill a list of values derived from MC simulation
 process
}
procedure FillList (MCSample: TVector; SampleCount: Integer;
  F: Real; ADistribution: TStatisticalDistribution; var VarParam1, VarParam2,
  VarParam3: Real; CalcVarParam:Boolean; AProgressIndicator: TMCProgress);
var
  i, j, ACount: Integer;
  AStop: Boolean;
  SumParam1, SumParam2, SumParam3: Real;
  SimulDataList: TDataList;
  SimulDistribution: TStatisticalDistribution;
begin
  SumParam1 := 0;
  SumParam2 := 0;
  SumParam3 := 0;
  VarParam1 := 0;
  VarParam2 := 0;
  VarParam3 := 0;
  SimulDataList := nil;
  SimulDistribution := nil;
  try
    SimulDataList := TDataList.Create(True);
    SimulDataList.Unbiased :=
      ADistribution.DataList.Unbiased;
    ACount := 0;
    for i := 1 to MCSample.Size do
    begin
      MCSample[i] := ErrorValue;
      try
        SimulDataList.Clear;
        for j := 1 to SampleCount do
          SimulDataList.AddValue(GetRandom(ADistribution));
        SimulDataList.PrepareData(ADistribution.IsLMomentMethod,False);
        if SimulDistribution = nil then
          SimulDistribution :=
            TStatisticalDistribution.Create(ADistribution.DistributionType,
              SimulDataList,ADistribution.GEVShape,True) else
          SimulDistribution.Refresh;
{Invert... to get a simulation value}
        MCSample[i] := SimulDistribution.InvcdfValue(F);
{Calculate variation of parameters only if CalcVarParam is true}
        if CalcVarParam then
        begin
          VarParam1 := VarParam1 + Sqr(SimulDistribution.StatParameter1);
          VarParam2 := VarParam2 + Sqr(SimulDistribution.StatParameter2);
          VarParam3 := VarParam3 + Sqr(SimulDistribution.StatParameter3);
          SumParam1 := SumParam1 + SimulDistribution.StatParameter1;
          SumParam2 := SumParam2 + SimulDistribution.StatParameter2;
          SumParam3 := SumParam3 + SimulDistribution.StatParameter3;
        end;
      except
        on EMathError do
          Continue;
        else
          raise;
      end;
      if (i mod 100)=0 then
      begin
        AProgressIndicator(AStop);
        if AStop then raise EStopByUser.Create(rsProcessTerminatedByUser);
      end;
      Inc(ACount);
    end;
    if CalcVarParam then
    begin
      VarParam1 := ( VarParam1 - Sqr(SumParam1)/ACount )/(ACount-1);
      VarParam2 := ( VarParam2 - Sqr(SumParam2)/ACount )/(ACount-1);
      VarParam3 := ( VarParam3 - Sqr(SumParam3)/ACount )/(ACount-1);
    end;
{Finally sort the simulation values}
    MCSample.Sort;
    i := 1;
    while True do
    begin
      if MCSample[i]<>ErrorValue then Break;
      Inc(i);
    end;
    Assert((i-1) = (MCSample.Size-ACount));
    MCSample.Shift(MCSample.Size-ACount+1, MCSample.Size, ACount-MCSample.Size);
    MCSample.Resize(ACount);
  finally
    SimulDistribution.Free;
    SimulDataList.Free;
  end;
end;

procedure MonteCarloLimits(AFValue: Real;
  ADistribution: TStatisticalDistribution; MCCount: Integer;
  MCConfidenceLevel: Real; var UpperSampleLimit, LowerSampleLimit,
  UpperMCLimit, LowerMCLimit: Real; AProgressIndicator: TMCProgress);
begin
  MonteCarloLimits(AFValue, ADistribution, MCCount,
    ADistribution.DataList.Count, MCConfidenceLevel, UpperSampleLimit,
      LowerSampleLimit, UpperMCLimit, LowerMCLimit, AProgressIndicator);
end;

procedure MonteCarloLimits(AFValue: Real;
  ADistribution: TStatisticalDistribution; MCCount, SampleCount: Integer;
  MCConfidenceLevel: Real; var UpperSampleLimit, LowerSampleLimit,
  UpperMCLimit, LowerMCLimit: Real; AProgressIndicator: TMCProgress);
var
  MCSample: TVector;
  Central,CentralDL1,CentralDL2,CentralDL3: Real;
  Upper,UpperDL1,UpperDL2,UpperDL3: Real;
  Lower,LowerDL1,LowerDL2,LowerDL3: Real;
  TrueUpper, TrueLower: Real;
  VarParam1, VarParam2, VarParam3: Real;
  PreVarParam1, PreVarParam2, PreVarParam3: Real;
  DeltaFactor: Real;
begin
{Delta factor is a multiply factor for numerical estimations of
 the derivates}
  DeltaFactor := 0.15;
  RandSeed := 0;
  MCSample := nil;
  try
    MCSample := TVector.Create(MCCount);
{Stage I: Calculate central values, Upper and Lower sample values}
    FillList(MCSample, SampleCount, AFValue, ADistribution,
      PreVarParam1, PreVarParam2, PreVarParam3, True, AProgressIndicator);
    VarParam1 := PreVarParam1;
    VarParam2 := PreVarParam2;
    VarParam3 := PreVarParam3;
    Lower := InterpolListItem(MCSample,(1-MCConfidenceLevel)*0.5);
    Upper := InterpolListItem(MCSample,(MCConfidenceLevel+1)*0.5);
    Central := InterpolListItem(MCSample,0.500);
{Stage II: Calculate DL1}
    ADistribution.MultiplyStatParam(1+DeltaFactor,1,1);
    FillList(MCSample, SampleCount, AFValue, ADistribution,
      PreVarParam1, PreVarParam2, PreVarParam3, False, AProgressIndicator);
    ADistribution.Refresh;
    CentralDL1 := InterpolListItem(MCSample,0.500);
    LowerDL1 := InterpolListItem(MCSample,(1-MCConfidenceLevel)*0.5);
    UpperDL1 := InterpolListItem(MCSample,(MCConfidenceLevel+1)*0.5);
    CentralDL1 := (CentralDL1-Central)/
      (DeltaFactor*ADistribution.StatParameter1);
    UpperDL1 := (UpperDL1-Upper)/(DeltaFactor*ADistribution.StatParameter1);
    LowerDL1 := (LowerDL1-Lower)/(DeltaFactor*ADistribution.StatParameter1);
{Stage III: Calculate DL2}
    ADistribution.MultiplyStatParam(1,1+DeltaFactor,1);
    FillList(MCSample, SampleCount, AFValue, ADistribution,
      PreVarParam1, PreVarParam2, PreVarParam3, False, AProgressIndicator);
    ADistribution.Refresh;
    CentralDL2 := InterpolListItem(MCSample,0.500);
    LowerDL2 := InterpolListItem(MCSample,(1-MCConfidenceLevel)*0.5);
    UpperDL2 := InterpolListItem(MCSample,(MCConfidenceLevel+1)*0.5);
    CentralDL2 := (CentralDL2-Central)/
      (DeltaFactor*ADistribution.StatParameter2);
    UpperDL2 := (UpperDL2-Upper)/(DeltaFactor*ADistribution.StatParameter2);
    LowerDL2 := (LowerDL2-Lower)/(DeltaFactor*ADistribution.StatParameter2);
{Stage IV: Calculate DL3}
    if ADistribution.ParameterCount = 3 then
    begin
      ADistribution.MultiplyStatParam(1,1,1+DeltaFactor);
      FillList(MCSample, SampleCount, AFValue, ADistribution,
        PreVarParam1, PreVarParam2, PreVarParam3, False, AProgressIndicator);
      ADistribution.Refresh;
      CentralDL3 := InterpolListItem(MCSample,0.500);
      UpperDL3 := InterpolListItem(MCSample,(MCConfidenceLevel+1)*0.5);
      LowerDL3 := InterpolListItem(MCSample,(1-MCConfidenceLevel)*0.5);
      CentralDL3 := (CentralDL3-Central)/
        (DeltaFactor*ADistribution.StatParameter3);
      UpperDL3 := (UpperDL3-Upper)/(DeltaFactor*ADistribution.StatParameter3);
      LowerDL3 := (LowerDL3-Lower)/(DeltaFactor*ADistribution.StatParameter3);
    end else begin
      CentralDL3 := 0;
      UpperDL3 := 0;
      LowerDL3 := 0;
      VarParam3 := 0;
    end;
{Calculate derivatives}
    TrueUpper :=
      (CentralDL1*(CentralDL1+UpperDL1)*VarParam1+
       CentralDL2*(CentralDL2+UpperDL2)*VarParam2+
       CentralDL3*(CentralDL3+UpperDL3)*VarParam3)/
      (LowerDL1*(CentralDL1+UpperDL1)*VarParam1+
       LowerDL2*(CentralDL2+UpperDL2)*VarParam2+
       LowerDL3*(CentralDL3+UpperDL3)*VarParam3);
    TrueLower :=
      (CentralDL1*(CentralDL1+LowerDL1)*VarParam1+
       CentralDL2*(CentralDL2+LowerDL2)*VarParam2+
       CentralDL3*(CentralDL3+LowerDL3)*VarParam3)/
      (UpperDL1*(CentralDL1+LowerDL1)*VarParam1+
       UpperDL2*(CentralDL2+LowerDL2)*VarParam2+
       UpperDL3*(CentralDL3+LowerDL3)*VarParam3);
    TrueUpper := Central + (Central-Lower)*TrueUpper;
    TrueLower := Central + (Central-Upper)*TrueLower;
    UpperSampleLimit := Upper;
    LowerSampleLimit := Lower;
    UpperMCLimit := TrueUpper;
    LowerMCLimit := TrueLower;
  finally
    MCSample.Free;
  end;
end;

end.
