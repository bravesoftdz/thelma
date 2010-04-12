unit testmontecarlo;

interface

type
  TDummyClass = class
    public
      procedure DummyProgress(var Stop: Boolean);
  end;

function test(Verbose: Boolean): string;

implementation

uses montecarlo, ts, statprocesses, sysutils, prob, Classes, dates;

var TestsPassed: Integer;

procedure TDummyClass.DummyProgress(var Stop: Boolean);
begin
  {}
end;

function testlimits(Verbose: Boolean): string;
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
    ATimeseries.LoadFromFile('..\data\tsidf.30min');
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
    if Abs( (sUpperSample+sLowerSample)-(rUpperLimit+rLowerLimit) )>
      0.01*(rUpperSample+rLowerSample) then
      raise Exception.Create('Failed test of testmontecarlo:'+
        #13#10+'Sample and Confidence central values diverge more than 1%');
    Inc(TestsPassed);
    if Abs( (sUpperSample-sLowerSample)-(rUpperSample-rLowerSample) )>
      0.01*(rUpperSample-rLowerSample) then
      raise Exception.Create('Failed test of testmontecarlo:'+
        #13#10+'Theoritical and simulated sample limits diverge more than 1%');
    Inc(TestsPassed);
    if Abs( (sUpperLimit-sLowerLimit)-(rUpperSample-rLowerLimit) )>
      0.1*(rUpperLimit-rLowerLimit) then
      raise Exception.Create('Failed test of testmontecarlo:'+
        #13#10+'Theoritical and simulated confidence limits diverge more than 10%');
    Inc(TestsPassed);        
  finally
    ADistribution.Free;
    ADataList.Free;
    ATimeseries.Free;
    ADummyClass.Free;
  end;
  Result := '';
  if Verbose then
  begin
    Result := Result + #13#10+
    '  Monte Carlo confidence limits tested. A simulation of 60000 experiments'+
    #13#10+
    '  were run to determine the sample and the confidence limits of the'+#13#10+
    '  mean value of a variable that follows the Normal Distribution.'+#13#10+
    '  Three sub-test were considered:'+#13#10+
    '  1. Central values of confidence limits and sample limits should not'+#13#10+
    '     differ more than 1%. Central value of Sample limits is: '+
    FormatFloat('#.###',0.5*(sUpperSample+sLowerSample))+#13#10+
    '     Central value of Confidence limits is: '+
    FormatFloat('#.###',0.5*(rUpperLimit+rLowerLimit))+#13#10+
    '  2. Theoritical and simulated sample limits shoud not differ more'+#13#10+
    '     than 1%. Theoritical sample limits interval is: '+
    FormatFloat('#.###', (rUpperSample-rLowerSample))+#13#10+
    '     Simulated sample limits interval is: '+
    FormatFloat('#.###', (sUpperSample-sLowerSample))+#13#10+
    '  3. Theoritical and simulated confidence interval should not'+#13#10+
    '     differ more than 10%. Theoritical confidence interval is: '+
    FormatFloat('#.###', (rUpperLimit-rLowerLimit))+#13#10+
    '     Simulated confidence interval is: '+
    FormatFloat('#.###', (sUpperLimit-sLowerLimit))+#13#10+
    '  So all three test were succesfull';
  end;
end;

function test(Verbose: Boolean): string;
begin
  TestsPassed := 0;
  Result := '';
  Result := Result + testlimits(Verbose);
  Result := Result + IntToStr(TestsPassed) + ' tests passed';
end;

end.
