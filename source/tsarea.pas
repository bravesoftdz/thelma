{******************************************************************}
{                                                                  }
{  Thelma library                                                  }
{                                                                  }
{  Copyright (c) 2000-09 National Technical University of Athens   }
{                                                                  }
{******************************************************************}

{** Unit with rainfall area integration functions.
}
unit tsarea;

interface

uses ts, tsprocess, Classes, Contnrs, SysUtils, Dates, Genutils, Math;

type
  TPointMeanRain = record
    Altitude: Real;
    Value: Real;
    Weight: Real;
    CoordX, CoordY: Real;
  end;

  TArrayOfPointMeanRains = array of TPointMeanRain;

function RainfallGradient(TimeseriesList: TObjectList; MeanAltitude: Real;
  var PointMeanRains: TArrayOfPointMeanRains;
  var ConstantTerm, CorrelationCoef, ReductionFactor: Real): Real;

procedure CalcArealRainfall(TimeseriesList: TObjectList; Dest: TTimeseries;
  PointMeanRains: TArrayOfPointMeanRains; ReductionFactor: Real;
  MakeAltitudeCorrection: Boolean);

function CalcStationsMeanAltitude(PointMeanRains: TArrayOfPointMeanRains): Real;

procedure CalcWeightsFromCoords(MeanX, MeanY: Real; BFactor: Real;
  var PointMeanRains: TArrayOfPointMeanRains);

procedure FindStationCenter(PointMeanRains: TArrayOfPointMeanRains;
  var MeanX, MeanY: Real);

implementation

procedure LSLinearFit(Measures: TArrayOfPointMeanRains; var a, b, r: Real);
var
  i, n: Integer;
  SumX, SumY, SumXsq, SumYsq, SumXY: Real;
  x, y: Real;
begin
  n := Length(Measures);
  SumX := 0;
  SumY := 0;
  SumXsq := 0;
  SumYsq := 0;
  SumXY := 0;
  for i := 0 to n-1 do
  begin
    x := Measures[i].Altitude;
    y := Measures[i].Value;
    SumX := SumX + x;
    SumXsq := SumXsq + Sqr(x);
    SumY := SumY + y;
    SumYsq := SumYsq + Sqr(y);
    SumXY := SumXY + x*y;
  end;
  r := (n*SumXY-SumX*SumY)/Sqrt( (n*SumXsq-Sqr(SumX)) * (n*SumYsq-Sqr(SumY)) );
  b := r * Sqrt( (n*SumYsq-Sqr(SumY)) / (n*SumXsq-Sqr(SumX)) );
  a := (SumY-b*SumX)/n;
end;

function WeightCorrectionFactor(Measures: TArrayOfPointMeanRains): Real;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Length(Measures)-1 do
    Result := Result + Measures[i].Weight;
  Result := 1 / Result;
end;

resourcestring
  rsTimeseiesShouldHaveSameTimestep =
    'Timeseries should have same time step';
  rsAtleastTwoTimeseries =
    'Two at least time series shoud be specified in order to calculate '+
    'rainfall gradient';
  rsCouldNotFindMeasurements =
    'Could not find concurent rainfall measurements';

function CalcStationsMeanAltitude(PointMeanRains: TArrayOfPointMeanRains): Real;
var
  i: Integer;
  WeightCorrection: Real;
begin
  WeightCorrection := WeightCorrectionFactor(PointMeanRains);
  Result := 0;
  for i := 0 to Length(PointMeanRains)-1 do
    with PointMeanRains[i] do
      Result := Result + WeightCorrection * Weight* Altitude;
end;

function RainfallGradient(TimeseriesList: TObjectList; MeanAltitude: Real;
  var PointMeanRains: TArrayOfPointMeanRains;
  var ConstantTerm, CorrelationCoef, ReductionFactor: Real): Real;
var
  i, j: Integer;
  ACommonPeriod: TDateTimeList;
  AStationsMeanAltitude: Real;
  WeightCorrection: Real;
  UnreducedMeanAreaRain: Real;
begin
  Assert(TimeseriesList<>nil);
  Assert(Length(PointMeanRains)=TimeseriesList.Count);
  if TimeseriesList.Count<2 then
    raise Exception.Create(rsAtleastTwoTimeseries);
  for i := 1 to TimeseriesList.Count-1 do
    if TTimeseries(TimeseriesList[i]).TimeStep<>
      TTimeseries(TimeseriesList[0]).TimeStep then
      raise Exception.Create(rsTimeseiesShouldHaveSameTimestep);
  ACommonPeriod := nil;
  try
    ACommonPeriod := GetCommonPeriod(TimeseriesList, 0);
    if ACommonPeriod.Count<1 then
      raise Exception.Create(rsCouldNotFindMeasurements);
    for i := 0 to Length(PointMeanRains)-1 do
      PointMeanRains[i].Value := 0;
    for i := 0 to ACommonPeriod.Count-1 do
      for j := 0 to TimeseriesList.Count -1 do
        with TTimeseries (TimeseriesList[j]) do
          PointMeanRains[j].Value := PointMeanRains[j].Value +
            Items[IndexOf(ACommonPeriod[i])].AsFloat;
    for i := 0 to Length(PointMeanRains)-1 do
      PointMeanRains[i].Value := PointMeanRains[i].Value / ACommonPeriod.Count;
  finally
    ACommonPeriod.Free;
  end;
  LSLinearFit(PointMeanRains, ConstantTerm, Result, CorrelationCoef);
  WeightCorrection := WeightCorrectionFactor(PointMeanRains);
  AStationsMeanAltitude := CalcStationsMeanAltitude(PointMeanRains);
  UnreducedMeanAreaRain := 0;
  for i := 0 to Length(PointMeanRains)-1 do
    with PointMeanRains[i] do
      UnreducedMeanAreaRain := UnreducedMeanAreaRain +
        WeightCorrection * Weight * Value;
  ReductionFactor := 1 + Result * (MeanAltitude - AStationsMeanAltitude) /
    UnreducedMeanAreaRain;
end;

procedure CalcArealRainfall(TimeseriesList: TObjectList; Dest: TTimeseries;
  PointMeanRains: TArrayOfPointMeanRains; ReductionFactor: Real;
  MakeAltitudeCorrection: Boolean);
var
  i, j: Integer;
  ACommonPeriod: TDateTimeList;
  AAllRecords: TDateTimeList;
  AValue: Real;
  WeightCorrection: Real;
begin
  Assert(TimeseriesList<>nil);
  Assert(Dest<>nil);
  Assert(Length(PointMeanRains)=TimeseriesList.Count);
  for i := 1 to TimeseriesList.Count-1 do
    if TTimeseries(TimeseriesList[i]).TimeStep<>
      TTimeseries(TimeseriesList[0]).TimeStep then
      raise Exception.Create(rsTimeseiesShouldHaveSameTimestep);
  ACommonPeriod := nil;
  AAllRecords := nil;
  WeightCorrection := WeightCorrectionFactor(PointMeanRains);
  try
    ACommonPeriod := GetCommonPeriod(TimeseriesList, 0);
    AAllRecords := GetAllRecords(TimeseriesList, 0);
    for i := 0 to AAllRecords.Count-1 do
    begin
      AValue := 0;
      Dest.Add(AAllRecords[i], True, 0, '', msNew);
      if ACommonPeriod.IndexOf(Dest.Last.Date)<0 then
        Continue;
      for j := 0 to Length(PointMeanRains)-1 do
        with PointMeanRains[j] do
          with TTimeseries(TimeseriesList[j]) do
            with Items[IndexOf(AAllRecords[i])] do
              AValue := AValue + Weight * AsFloat * WeightCorrection;
      Dest.Last.AsFloat := AValue;
    end;
  finally
    ACommonPeriod.Free;
    AAllRecords.Free;
  end;
  if MakeAltitudeCorrection then
    for i := 0 to Dest.Count-1 do
      if not Dest[i].IsNull then
        Dest[i].AsFloat := Dest[i].AsFloat * ReductionFactor;
end;

procedure CalcWeightsFromCoords(MeanX, MeanY: Real; BFactor: Real;
  var PointMeanRains: TArrayOfPointMeanRains);

  function GetDistance(x1, y1, x2, y2: Real):Real;
  begin
    Result := Sqrt(Sqr(x2-x1) + Sqr(y2-y1));
  end;

var
  i: Integer;
  ADistance, ASum: Real;
begin
  ASum := 0;
  for i := 0 to Length(PointMeanRains)-1 do
    with PointMeanRains[i] do
    begin
      ADistance := Max(GetDistance(MeanX, MeanY, CoordX, CoordY), 0.01);
      Weight := Power(ADistance, -BFactor);
      ASum := ASum + Weight;
    end;
  for i := 0 to Length(PointMeanRains)-1 do
    with PointMeanRains[i] do
      Weight := Weight / ASum;
end;

procedure FindStationCenter(PointMeanRains: TArrayOfPointMeanRains;
  var MeanX, MeanY: Real);
var
  i: Integer;
begin
  MeanX := 0;
  MeanY := 0;
  for i := 0 to Length(PointMeanRains)-1 do
  begin
    MeanX := MeanX + PointMeanRains[i].CoordX;
    MeanY := MeanY + PointMeanRains[i].CoordY;
  end;
  MeanX := MeanX / Length(PointMeanRains);
  MeanY := MeanY / Length(PointMeanRains);
end;

end.
