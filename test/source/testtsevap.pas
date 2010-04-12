unit testtsevap;

interface

function test(Verbose: Boolean): string;

implementation

uses Classes, ts, tsevap, sysutils, dates;

var TestsPassed: Integer;

var TimeseriesList: array[0..3] of TTimeseries;

const testfiles: array[0..3] of string = ('PenmanTemp', 'PenmanRH',
  'PenmanSD', 'PenmanWS');

const
  tsTemp = 0;
  tsRH = 1;
  tsSD = 2;
  tsWS = 3;

procedure LoadTimeseries;
var
  i: Integer;
begin
  for i := 0 to 3 do
  begin
    TimeseriesList[i].LoadFromFile('..\data\'+testfiles[i]);
  end;
end;

function testPenman(Verbose: Boolean): string;
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
          0.50,APenmanOptions);
    AFloat := 0;
    for i := 0 to 11 do
      AFloat := AFloat + ATimeseries[i].AsFloat;
  finally
    ATimeseries.Free;
  end;
  if Abs((AFloat-1326.3)/1326.3)>0.005 then
    raise Exception.Create('  Failed test of testPenman:'+#13#10+
      '  Estimated evaporation differs more than 0.5% from reference value.'+
      #13#10+'  Evaporation should be: 1326.3 mm but it is estimated as: '
      +FormatFloat('#.##',AFloat)+' mm');
  Result := '';
  if Verbose then
  begin
    Result := Result+'.'+#13#10+
    '  Penman function tested. 12 monthly values of evaporation have been'+
    #13#10+
    '  calculated and aggregated. The estimated value for one year is:'+#13#10+
    '  '+FormatFloat('#.##',AFloat)+' mm, reference value is 1326.3 mm'+#13#10+
    '  Estimated and reference value differ less than 0.5%'+#13#10+
    '  so the test was succesfull.'+#13#10+'.';
  end;
  Inc(TestsPassed);
end;

function testPenmanMonteith(Verbose: Boolean): string;
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
  if Abs((AFloat-1162)/1162)>0.005 then
    raise Exception.Create('  Failed test of testPenmanMonteith:'+#13#10+
      '  Estimated evaporation differs more than 0.5% from reference value.'+
      #13#10+'  Evaporation should be: 1162 mm but it is estimated as: '
      +FormatFloat('#.##',AFloat)+' mm');
  Result := '';
  if Verbose then
  begin
    Result := Result+#13#10+
    '  Penman-Monteith function tested. 12 monthly values of evaporation have'+
    ' been'+#13#10+
    '  calculated and aggregated. The estimated value for one year is:'+#13#10+
    '  '+FormatFloat('#.##',AFloat)+' mm, reference value is 1162.0 mm'+#13#10+
    '  Estimated and reference value differ less than 0.5%'+#13#10+
    '  so the test was succesfull.'+#13#10+'.';
  end;
  Inc(TestsPassed);
end;

function testThornthwaite(Verbose: Boolean): string;
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
  if Abs((AFloat-875.1)/875.1)>0.005 then
    raise Exception.Create(' Failed test of testThornthwaite:'+#13#10+
      ' Estimated evaporation differs more than 0.5% from reference value.'+
      #13#10+' Evaporation should be: 875.1 mm but it is estimated as: '
      +FormatFloat('#.##',AFloat)+' mm');
  Result := '';
  if Verbose then
  begin
    Result := Result+#13#10+
    '  Thornthwaite function tested. 12 monthly values of evaporation have'+
    ' been'+#13#10+
    '  calculated and aggregated. The estimated value for one year is:'+#13#10+
    '  '+FormatFloat('#.##',AFloat)+' mm, reference value is 875.1 mm'+#13#10+
    '  Estimated and reference value differ less than 0.5%'+#13#10+
    '  so the test was succesfull.'+#13#10;
  end;
  Inc(TestsPassed);
end;

function test(Verbose: Boolean): string;
begin
  Result := '';
  TestsPassed := 0;
  LoadTimeseries;
  Result := Result + testPenman(Verbose);
  Result := Result + testPenmanMonteith(Verbose);
  Result := Result + testThornthwaite(Verbose);
  Result := Result + IntToStr(TestsPassed) + ' tests passed';
end;

procedure Init;
var
  i: Integer;
begin
  for i:= 0 to 3 do
    TimeseriesList[i] := TTimeseries.Create;
end;

procedure Final;
var
  i: Integer;
begin
  for i := 0 to 3 do
    TimeseriesList[i].Free;
end;

initialization
  Init;

finalization
  Final;
  
end.
