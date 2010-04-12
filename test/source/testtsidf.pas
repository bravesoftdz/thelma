unit testtsidf;

interface

function test(Verbose: Boolean): string;

implementation

uses Classes, ts, tsidf, sysutils, statprocesses, dates;

var AIDFTimeseriesCollection: TIDFTimeseriesCollection;

var TestsPassed: Integer;

const testfiles: array[0..7] of string =
  ('tsidf.5min', 'tsidf.10min', 'tsidf.30min', 'tsidf.1h',
    'tsidf.2h', 'tsidf.6h', 'tsidf.12h', 'tsidf.24h');

const testdurations: array[0..7] of Real =
  (1/12, 1/6, 1/2, 1, 2, 6, 12, 24);

procedure LoadTimeseries;
var
  ATimeseries: TTimeseries;
  i: Integer;
begin
  for i := 0 to 7 do
  begin
    ATimeseries := nil;
    try
      ATimeseries := TTimeseries.Create;
      ATimeseries.LoadFromFile('..\data\'+testfiles[i]);
      AIDFTimeseriesCollection.Add(ATimeseries,idftsIntensity,
        testdurations[i], 1/12);
    finally
      ATimeseries.Free;
    end;
  end;
end;

function testKruskalWallis(Verbose: Boolean): string;
var
  AIDFVectorCollection: TIDFVectorCollection;
  AFloat: Real;
begin
  AIDFVectorCollection := nil;
  try
    AIDFVectorCollection := TIDFVectorCollection.Create(
      AIDFTimeseriesCollection, False, 0.796, 0.189,
        AIDFTimeseriesCollection.Amount);
    AFloat := AIDFVectorCollection.KruskalWallisParameter;
  finally
    AIDFVectorCollection.Free;
  end;
  if Abs(3.34-AFloat)>0.01 then
    raise Exception.Create('Failed test of testKruskalWallis:'+
      #13#10+'Estimated Kruskal Wallis parameter should be 3.34'+#13#10+
        'but it diverge more than 0.01. Estimated value ='+
          FloatToStr(AFloat));
  Result := '';
  if Verbose then
  begin
    Result := Result+'.'+#13#10+
    '  Kruskal-Wallis estimation function tested. A (Eta, Theta) set of:'+
    #13#10+
    '  (0.796, 0.189) was considered, then statistical parametre estimated as:'
    +#13#10+
    '  '+FormatFloat('#.###',AFloat)+' mm, reference value is 3.34 mm'+#13#10+
    '  Estimated and reference value differ less than 0.01'+#13#10+
    '  so the test was succesfull.'+#13#10+'.';
  end;
  Inc(TestsPassed);
end;

function testEtaTheta(Verbose: Boolean): string;
var
  Eta, Theta: Real;
begin
  IDFEtaThetaEvaluation(AIDFTimeseriesCollection, False,
    Eta, Theta);
  if (Abs(Eta-0.796)>0.01) or (Abs(Theta-0.189)>0.01) then
    raise Exception.Create('Failed test of testEtaTheta:'+
        #13#10+ 'Eta, Theta estimated parameters diverge more than 0.01 than'+
        'Reference values of Eta=0.189, Eta=0.796'+#13#10+
        'Estimated Vales: Eta='+FloatToStr(Eta)+' Theta='+FloatToStr(Theta));
  Result := '';
  if Verbose then
  begin
    Result := Result+#13#10+
    '  Eta, Theta estimation function tested.'+
    '  Estimated value of Eta='+FormatFloat('#.###',Eta)+' , Theta='
    +FormatFloat('#.###',Theta)+#13#10+
    '  Reference values of (Eta, Theta)=(0.796, 0.189) differ from estimated'+
    #13#10+'less than 0.01 so the test was succesfull.'+#13#10+'.';
  end;
  Inc(TestsPassed);
end;

function testSingleIDF(Verbose: Boolean): string;
var
  Omega, Eta, r: Real;
begin
  AIDFTimeseriesCollection.SingleIDFEvaluation(sdtEV1Max, 5, False,
    0.15, Omega, Eta, r);
  if (Abs(Omega-24.09)/Omega>0.01) or (Abs(Eta-0.649)>0.01) then
    raise Exception.Create('Failed test of testSingleIDF:'+
        #13#10+ 'Eta, Omega estimated parameters diverge more than 1% than'+
        'Reference values of Omega=24.09, Eta=0.649'+#13#10+
        'Estimated Vales: Eta='+FloatToStr(Eta)+' Omega='+FloatToStr(Omega));
  Result := '';
  if Verbose then
  begin
    Result := Result+#13#10+'  Single IDF estimation test I passed.'+#13#10+
    '  (Omega, Eta) estimated as ('+FormatFloat('#.###',Omega)+', '+
    FormatFloat('#.###',Eta)+' ) and differ less than 1% from'+#13#10+
    '  reference values of (24.09, 0.649)'+#13#10+'.';
  end;
  Inc(TestsPassed);
  AIDFTimeseriesCollection.SingleIDFEvaluation(sdtEV1Max, 50, False,
    0.15, Omega, Eta, r);
  if (Abs(Omega-37.91)/Omega>0.01) or (Abs(Eta-0.644)>0.01) then
    raise Exception.Create('Failed test of testSingleIDF:'+
        #13#10+ 'Eta, Omega estimated parameters diverge more than 1% than'+
        'Reference values of Omega=37.91, Eta=0.644'+#13#10+
        'Estimated Vales: Eta='+FloatToStr(Eta)+' Omega='+FloatToStr(Omega));
  if Verbose then
  begin
    Result := Result+#13#10+'  Single IDF estimation test II passed.'+#13#10+
    '  (Omega, Eta) estimated as ('+FormatFloat('#.###',Omega)+', '+
    FormatFloat('#.###',Eta)+' ) and differ less than 1% from'+#13#10+
    '  reference values of (37.91, 0.644)'+#13#10;
  end;
  Inc(TestsPassed);
end;

function test(Verbose: Boolean): string;
begin
  Result := '';
  TestsPassed := 0;
  LoadTimeseries;
  Result := Result+ testKruskalWallis(Verbose);
  Result := Result+ testEtaTheta(Verbose);
  Result := Result+ testSingleIDF(Verbose);
  AIDFTimeseriesCollection.Clear;
  Result := Result + IntToStr(TestsPassed) + ' tests passed';
end;

initialization
  AIDFTimeseriesCollection := TIDFTimeseriesCollection.Create(1/3);

finalization
  AIDFTimeseriesCollection.Free;

end.
