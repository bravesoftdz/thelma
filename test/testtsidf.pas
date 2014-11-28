unit testtsidf;

interface

uses
  TestFramework, tsidf;

type

  TTestIDF = class(TTestCase)
  private
    IDFTimeseriesCollection: TIDFTimeseriesCollection;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestKruskalWallis;
    procedure TestEtaTheta;
    procedure TestSingleIDF;
  end;

implementation

uses Classes, ts, sysutils, statprocesses, dates;

const testfiles: array[0..7] of string =
  ('tsidf.5min', 'tsidf.10min', 'tsidf.30min', 'tsidf.1h',
    'tsidf.2h', 'tsidf.6h', 'tsidf.12h', 'tsidf.24h');

const testdurations: array[0..7] of Real =
  (1/12, 1/6, 1/2, 1, 2, 6, 12, 24);

procedure TTestIDF.Setup;
var
  ATimeseries: TTimeseries;
  i: Integer;
begin
  IDFTimeseriesCollection := nil;
  IDFTimeseriesCollection := TIDFTimeseriesCollection.Create(1/3);
  for i := 0 to 7 do
  begin
    ATimeseries := nil;
    try
      ATimeseries := TTimeseries.Create;
      ATimeseries.LoadFromFile('..\test\data\'+testfiles[i]);
      IDFTimeseriesCollection.Add(ATimeseries,idftsIntensity,
        testdurations[i], 1/12);
    finally
      ATimeseries.Free;
    end;
  end;
end;

procedure TTestIDF.Teardown;
begin
  IDFTimeseriesCollection.Free;
end;

procedure TTestIDF.TestKruskalWallis;
var
  AIDFVectorCollection: TIDFVectorCollection;
  AFloat: Real;
begin
  AIDFVectorCollection := nil;
  try
    AIDFVectorCollection := TIDFVectorCollection.Create(
      IDFTimeseriesCollection, False, 0.796, 0.189,
        IDFTimeseriesCollection.Amount);
    AFloat := AIDFVectorCollection.KruskalWallisParameter;
  finally
    AIDFVectorCollection.Free;
  end;
  Check(Abs(3.34-AFloat) <= 0.01);
end;

procedure TTestIDF.TestEtaTheta;
var
  Eta, Theta: Real;
begin
  IDFEtaThetaEvaluation(IDFTimeseriesCollection, False,
    Eta, Theta);
  Check(Abs(Eta-0.796) <= 0.01);
end;

procedure TTestIDF.TestSingleIDF;
var
  Omega, Eta, r: Real;
begin
  IDFTimeseriesCollection.SingleIDFEvaluation(sdtEV1Max, 5, False,
    0.15, Omega, Eta, r);
  Check(Abs(Omega - 24.09) / Omega <= 0.01);
  Check(Abs(Eta - 0.649) <= 0.01);

  IDFTimeseriesCollection.SingleIDFEvaluation(sdtEV1Max, 50, False,
    0.15, Omega, Eta, r);
  Check(Abs(Omega - 37.91) / Omega <= 0.01);
  Check(Abs(Eta - 0.644) <= 0.01);
end;

initialization
  RegisterTest(TTestIDF.Suite);
end.
