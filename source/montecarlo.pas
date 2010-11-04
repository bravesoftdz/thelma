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

procedure MonteCarloLimits(AFValue: Real;
  ADistribution: TStatisticalDistribution; MCCount, SampleCount: Integer;
  MCConfidenceLevel: Real; var UpperSampleLimit, LowerSampleLimit,
  UpperMCLimit, LowerMCLimit,
  p1usl, p1lsl, p2usl, p2lsl, p3usl, p3lsl,
  p1umc, p1lmc, p2umc, p2lmc, p3umc, p3lmc: Real;
  AProgressIndicator: TMCProgress); overload;

implementation

uses matrix;

type
  TVector2D = array of TVector;
  TArrayOfReal = array[0..3] of Real;

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
procedure FillList (MCSample: TVector2D; SampleCount: Integer;
  F: Real; ADistribution: TStatisticalDistribution; var VarParam1, VarParam2,
  VarParam3: Real; CalcVarParam:Boolean; AProgressIndicator: TMCProgress);
var
  i, j, k, ACount, CountP: Integer;
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
  CountP := ADistribution.ParameterCount;
  SimulDataList := nil;
  SimulDistribution := nil;
  try
    SimulDataList := TDataList.Create(True);
    SimulDataList.Unbiased :=
      ADistribution.DataList.Unbiased;
    ACount := 0;
    for i := 1 to MCSample[0].Size do
    begin
      for k := 0 to CountP do
        MCSample[k][i] := ErrorValue;
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
        MCSample[0][i] := SimulDistribution.InvcdfValue(F);
        MCSample[1][i] := SimulDistribution.Parameter1;
        if CountP=2 then MCSample[2][i] := SimulDistribution.Parameter2;
        if CountP=3 then MCSample[3][i] := SimulDistribution.Parameter3;
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
    for k := 0 to CountP do
      MCSample[k].Sort;
    i := 1;
    while True do
    begin
      if MCSample[0][i]<>ErrorValue then Break;
      Inc(i);
    end;
    Assert((i-1) = (MCSample[0].Size-ACount));
    for k := 0 to CountP do
    begin
      MCSample[k].Shift(MCSample[k].Size-ACount+1, MCSample[k].Size,
        ACount-MCSample[k].Size);
      MCSample[k].Resize(ACount);
    end;
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
  d: Real;
begin
  MonteCarloLimits(AFValue, ADistribution, MCCount,
    SampleCount, MCConfidenceLevel, UpperSampleLimit,
      LowerSampleLimit, UpperMCLimit, LowerMCLimit,
      d,d,d,d,d,d,d,d,d,d,d,d, AProgressIndicator);
end;

procedure MonteCarloLimits(AFValue: Real;
  ADistribution: TStatisticalDistribution; MCCount, SampleCount: Integer;
  MCConfidenceLevel: Real; var UpperSampleLimit, LowerSampleLimit,
  UpperMCLimit, LowerMCLimit,
  p1usl, p1lsl, p2usl, p2lsl, p3usl, p3lsl,
  p1umc, p1lmc, p2umc, p2lmc, p3umc, p3lmc: Real;
  AProgressIndicator: TMCProgress);
var
  MCSample: TVector2D;
  Central,CentralDL1,CentralDL2,CentralDL3: TArrayOfReal;
  Upper,UpperDL1,UpperDL2,UpperDL3: TArrayOfReal;
  Lower,LowerDL1,LowerDL2,LowerDL3: TArrayOfReal;
  TrueUpper, TrueLower: TArrayOfReal;
  VarParam1, VarParam2, VarParam3: Real;
  PreVarParam1, PreVarParam2, PreVarParam3: Real;
  DeltaFactor: Real;
  i, CountP: Integer;
begin
{Delta factor is a multiply factor for numerical estimations of
 the derivates}
  DeltaFactor := 0.15;
  RandSeed := 0;
  CountP := ADistribution.ParameterCount;
  SetLength(MCSample, CountP+1);
  for i := 0 to CountP do
    MCSample[i] := nil;
  try
    for i := 0 to CountP do
      MCSample[i] := TVector.Create(MCCount);
{Stage I: Calculate central values, Upper and Lower sample values}
    FillList(MCSample, SampleCount, AFValue, ADistribution,
      PreVarParam1, PreVarParam2, PreVarParam3, True, AProgressIndicator);
    VarParam1 := PreVarParam1;
    VarParam2 := PreVarParam2;
    VarParam3 := PreVarParam3;
    for i := 0 to CountP do
    begin
      Lower[i] := InterpolListItem(MCSample[i],(1-MCConfidenceLevel)*0.5);
      Upper[i] := InterpolListItem(MCSample[i],(MCConfidenceLevel+1)*0.5);
      Central[i] := InterpolListItem(MCSample[i],0.500);
    end;
{Stage II: Calculate DL1}
    ADistribution.MultiplyStatParam(1+DeltaFactor,1,1);
    FillList(MCSample, SampleCount, AFValue, ADistribution,
      PreVarParam1, PreVarParam2, PreVarParam3, False, AProgressIndicator);
    ADistribution.Refresh;
    for i := 0 to CountP do
    begin
      CentralDL1[i] := InterpolListItem(MCSample[i],0.500);
      LowerDL1[i] := InterpolListItem(MCSample[i],(1-MCConfidenceLevel)*0.5);
      UpperDL1[i] := InterpolListItem(MCSample[i],(MCConfidenceLevel+1)*0.5);
      CentralDL1[i] := (CentralDL1[i]-Central[i])/
        (DeltaFactor*ADistribution.StatParameter1);
      UpperDL1[i] := (UpperDL1[i]-Upper[i])/
        (DeltaFactor*ADistribution.StatParameter1);
      LowerDL1[i] := (LowerDL1[i]-Lower[i])/
        (DeltaFactor*ADistribution.StatParameter1);
    end;
{Stage III: Calculate DL2}
    ADistribution.MultiplyStatParam(1,1+DeltaFactor,1);
    FillList(MCSample, SampleCount, AFValue, ADistribution,
      PreVarParam1, PreVarParam2, PreVarParam3, False, AProgressIndicator);
    ADistribution.Refresh;
    for i := 0 to CountP do
    begin
      CentralDL2[i] := InterpolListItem(MCSample[i],0.500);
      LowerDL2[i] := InterpolListItem(MCSample[i],(1-MCConfidenceLevel)*0.5);
      UpperDL2[i] := InterpolListItem(MCSample[i],(MCConfidenceLevel+1)*0.5);
      CentralDL2[i] := (CentralDL2[i]-Central[i])/
        (DeltaFactor*ADistribution.StatParameter2);
      UpperDL2[i] := (UpperDL2[i]-Upper[i])/
        (DeltaFactor*ADistribution.StatParameter2);
      LowerDL2[i] := (LowerDL2[i]-Lower[i])/
        (DeltaFactor*ADistribution.StatParameter2);
    end;
{Stage IV: Calculate DL3}
    for i := 0 to CountP do
    begin
      if ADistribution.ParameterCount = 3 then
      begin
        ADistribution.MultiplyStatParam(1,1,1+DeltaFactor);
        FillList(MCSample, SampleCount, AFValue, ADistribution,
          PreVarParam1, PreVarParam2, PreVarParam3, False, AProgressIndicator);
        ADistribution.Refresh;
        CentralDL3[i] := InterpolListItem(MCSample[i],0.500);
        UpperDL3[i] := InterpolListItem(MCSample[i],(MCConfidenceLevel+1)*0.5);
        LowerDL3[i] := InterpolListItem(MCSample[i],(1-MCConfidenceLevel)*0.5);
        CentralDL3[i] := (CentralDL3[i]-Central[i])/
          (DeltaFactor*ADistribution.StatParameter3);
        UpperDL3[i] := (UpperDL3[i]-Upper[i])/
          (DeltaFactor*ADistribution.StatParameter3);
        LowerDL3[i] := (LowerDL3[i]-Lower[i])/
          (DeltaFactor*ADistribution.StatParameter3);
      end else begin
        CentralDL3[i] := 0;
        UpperDL3[i] := 0;
        LowerDL3[i] := 0;
        VarParam3 := 0;
      end;
    end;
{Calculate derivatives}
    for i := 0 to CountP do
    begin
      TrueUpper[i] :=
        (CentralDL1[i]*(CentralDL1[i]+UpperDL1[i])*VarParam1+
         CentralDL2[i]*(CentralDL2[i]+UpperDL2[i])*VarParam2+
         CentralDL3[i]*(CentralDL3[i]+UpperDL3[i])*VarParam3)/
        (LowerDL1[i]*(CentralDL1[i]+UpperDL1[i])*VarParam1+
         LowerDL2[i]*(CentralDL2[i]+UpperDL2[i])*VarParam2+
         LowerDL3[i]*(CentralDL3[i]+UpperDL3[i])*VarParam3);
      TrueLower[i] :=
        (CentralDL1[i]*(CentralDL1[i]+LowerDL1[i])*VarParam1+
         CentralDL2[i]*(CentralDL2[i]+LowerDL2[i])*VarParam2+
         CentralDL3[i]*(CentralDL3[i]+LowerDL3[i])*VarParam3)/
        (UpperDL1[i]*(CentralDL1[i]+LowerDL1[i])*VarParam1+
         UpperDL2[i]*(CentralDL2[i]+LowerDL2[i])*VarParam2+
         UpperDL3[i]*(CentralDL3[i]+LowerDL3[i])*VarParam3);
      TrueUpper[i] := Central[i] + (Central[i]-Lower[i])*TrueUpper[i];
      TrueLower[i] := Central[i] + (Central[i]-Upper[i])*TrueLower[i];
    end;
    UpperSampleLimit := Upper[0];
    LowerSampleLimit := Lower[0];
    UpperMCLimit := TrueUpper[0];
    LowerMCLimit := TrueLower[0];
    p1usl := Upper[1];
    p1lsl := Lower[1];
    p1umc := TrueUpper[1];
    p1lmc := TrueLower[1];
    p2usl := Upper[2];
    p2lsl := Lower[2];
    p2umc := TrueUpper[2];
    p2lmc := TrueLower[2];
    p3usl := Upper[3];
    p3lsl := Lower[3];
    p3umc := TrueUpper[3];
    p3lmc := TrueLower[3];
  finally
    for i := 0 to CountP do
      MCSample[i].Free;
    MCSample := nil;
  end;
end;

end.
