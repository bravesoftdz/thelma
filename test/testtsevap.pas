unit testtsevap;

interface

uses
  TestFramework, ts;

type

  TTestEvaporation = class(TTestCase)
  private
    TimeseriesList: array[0..3] of TTimeseries;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestPenman;
    procedure TestPenmanMonteith;
    procedure TestThornthwaite;
  end;

implementation

uses Classes, tsevap, sysutils, dates;

const testfiles: array[0..3] of string = ('PenmanTemp', 'PenmanRH',
  'PenmanSD', 'PenmanWS');

const
  tsTemp = 0;
  tsRH = 1;
  tsSD = 2;
  tsWS = 3;

procedure TTestEvaporation.SetUp;
var
  i: Integer;
begin
  for i := 0 to 3 do
  begin
    TimeseriesList[i] := TTimeseries.Create;
    TimeseriesList[i].LoadFromFile('..\test\data\'+testfiles[i]);
  end;
end;

procedure TTestEvaporation.TearDown;
var i: Integer;
begin
  for i := 0 to 3 do
    TimeseriesList[i].Free;
end;

procedure TTestEvaporation.TestPenman;
var
  ATimeseries: TTimeseries;
  APenmanOptions: TPenmanOptions;
  AFloat: Real;
  i: Integer;
begin
  ATimeseries := nil;
  try
    APenmanOptions.EvapMode := pevmPenman;
    APenmanOptions.SunDurMode := psdmDuration;
    APenmanOptions.MonthlyDay := pmodRepresentative;
    ATimeseries := TTimeseries.Create;
    CalcPenmanEvap(ATimeseries,
      TimeseriesList[tsTemp], TimeseriesList[tsRH], TimeseriesList[tsWS],
        TimeseriesList[tsSD], 38.75, 145, 0.08, 0.56, 0.08, 0.10, 0.90, 0.25,
          0.50, APenmanOptions);
    AFloat := 0;
    for i := 0 to 11 do
      AFloat := AFloat + ATimeseries[i].AsFloat;
  finally
    ATimeseries.Free;
  end;
  Check(Abs((AFloat-1326.3)/1326.3) <= 0.005);
end;

procedure TTestEvaporation.TestPenmanMonteith;
var
  ATimeseries: TTimeseries;
  APenmanOptions: TPenmanOptions;
  AFloat: Real;
  i: Integer;
begin
  ATimeseries := nil;
  try
    APenmanOptions.EvapMode := pevmPenmanMonteith;
    APenmanOptions.SunDurMode := psdmDuration;
    APenmanOptions.MonthlyDay := pmodRepresentative;
    ATimeseries := TTimeseries.Create;
    CalcPenmanEvap(ATimeseries, TimeseriesList[tsTemp], TimeseriesList[tsRH],
      TimeseriesList[tsWS], TimeseriesList[tsSD], 38.75, 145, 0.25, 0.34, 0.044,
      0.10, 0.90, 0.25, 0.50, APenmanOptions);
    AFloat := 0;
    for i := 0 to 11 do
      AFloat := AFloat + ATimeseries[i].AsFloat;
  finally
    ATimeseries.Free;
  end;
  Check(Abs((AFloat-1162)/1162) <= 0.005);
end;

procedure TTestEvaporation.TestThornthwaite;
var
  ATimeseries: TTimeseries;
  AFloat: Real;
  i: Integer;
begin
  ATimeseries := nil;
  try
    ATimeseries := TTimeseries.Create;
    CalcThornthwaiteEvap(ATimeseries, TimeseriesList[tsTemp],
      38.75, CalcThornthwaiteIota(TimeseriesList[tsTemp]));
    AFloat := 0;
    for i := 0 to 11 do
      AFloat := AFloat + ATimeseries[i].AsFloat;
  finally
    ATimeseries.Free;
  end;
  Check(Abs((AFloat-875.1)/875.1) <= 0.005);
end;

initialization
  RegisterTest(TTestEvaporation.Suite);
end.
