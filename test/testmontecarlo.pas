unit testmontecarlo;

interface

uses TestFramework;

type
  TTestMonteCarlo= class(TTestCase)
  published
    procedure TestLimits;
  end;

  TDummyClass = class
    public
      procedure DummyProgress(var Stop: Boolean);
  end;

implementation

uses montecarlo, ts, statprocesses, sysutils, prob, Classes, dates;

procedure TDummyClass.DummyProgress(var Stop: Boolean);
begin
  Stop := False;
end;

procedure TTestMonteCarlo.TestLimits;
var
  ATimeseries: TTimeseries;
  ADataList: TFullDataList;
  ADistribution: TStatisticalDistribution;
  sUpperLimit, sLowerLimit, sUpperSample, sLowerSample: Real;
  rUpperLimit, rLowerLimit, rUpperSample, rLowerSample: Real;
  mi, sigma: Real;
  ADummyClass: TDummyClass;
begin
  ATimeseries := nil;
  ADataList := nil;
  ADistribution := nil;
  ADummyClass := nil;
  try
    ATimeseries := TTimeseries.Create;
    ADataList := TFullDataList.Create;
    ADummyClass := TDummyClass.Create;
    ATimeseries.LoadFromFile('..\test\data\tsidf.30min');
    ADataList.Unbiased := True;
    ADataList.SetTS(ATimeseries,True);
    ADistribution :=
      TStatisticalDistribution.Create(sdtNormal,ADataList,0.15);
    MonteCarloLimits(0.500,ADistribution,60000,0.95,
      sUpperSample, sLowerSample, sUpperLimit, sLowerLimit,
        ADummyClass.DummyProgress);
    mi := ADistribution.StatParameter1;
    sigma := ADistribution.StatParameter2;
    rUpperSample := mi + sigma*InvNormalCdf(0.975,0,1)/Sqrt(ATimeseries.CountNotNull);
    rLowerSample := mi - sigma*InvNormalCdf(0.975,0,1)/Sqrt(ATimeseries.CountNotNull);
    rUpperLimit := mi + sigma*2.230/Sqrt(ATimeseries.CountNotNull);
    rLowerLimit := mi - sigma*2.230/Sqrt(ATimeseries.CountNotNull);
    Check(Abs((sUpperSample+sLowerSample)-(rUpperLimit+rLowerLimit)) <=
      0.01*(rUpperSample+rLowerSample));
    Check(Abs((sUpperSample-sLowerSample)-(rUpperSample-rLowerSample)) <=
      0.01*(rUpperSample-rLowerSample));
    Check(Abs((sUpperLimit-sLowerLimit)-(rUpperSample-rLowerLimit)) <=
      0.1*(rUpperLimit-rLowerLimit));
  finally
    ADistribution.Free;
    ADataList.Free;
    ATimeseries.Free;
    ADummyClass.Free;
  end;
end;

initialization
  RegisterTest(TTestMonteCarlo.Suite);

end.
