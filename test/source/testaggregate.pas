unit testaggregate;

interface

function test(Verbose: Boolean): string;

implementation

uses SysUtils, ts;

var TestsPassed: Integer;

function CompareTimeSeries(RefSeries, TestSeries: TTimeseries): string;
var
  i, AIndex: Integer;
begin
  Result := '';
  for i := 0 to RefSeries.Count-1 do
  begin
    AIndex := TestSeries.IndexOf(RefSeries[i].Date);
    if AIndex<0 then
      Result := 'Reference record: '+RefSeries.Items[i].DateAsString +
        ' does not exist on tested time series';
    if Result<> '' then Break;
    if RefSeries[i].IsNull <> TestSeries[AIndex].IsNull then
      Result := 'Reference and test time series Null state differ at record: '
        +RefSeries[i].DateAsString + ' Expected state: '+
        BoolToStr(RefSeries[i].IsNull)+
        ' Obtained state: '+BoolToStr(TestSeries[i].IsNull);
    if Abs(RefSeries[i].AsFloat-TestSeries[AIndex].AsFloat)>0.05 then
      Result := 'Reference and test time series differ at record: '
        +RefSeries[i].DateAsString + ' Expected value: '+RefSeries[i].AsString+
        ' Obtained value: '+TestSeries[AIndex].AsString;
    if Result<> '' then Break;
    if RefSeries[i].GetFlag('MISSING') <> TestSeries[AIndex].GetFlag('MISSING')
      then
      Result := 'Reference and test time series flag MISSING differ at record: '
        +RefSeries[i].DateAsString + ' Expected state: '+
        BoolToStr(RefSeries[i].GetFlag('MISSING'))+
        ' Obtained state: '+BoolToStr(TestSeries[i].GetFlag('MISSING'));
    if Result<> '' then Break;
  end;
  if Result<>'' then
  Result := Result + ' when testing with '+ RefSeries.FileName;
end;

type
  TestSet = record
    InputFile, ResultsFile: string;
    VariableType: TVariableType;
    MissingAllowed: Integer;
  end;

const
  TestSets: array [0..3] of TestSet =
    (
      ( InputFile: '..\data\AggregateTenMin.hts';
        ResultsFile: '..\data\AggregateHourlySum.hts';
        VariableType: vtCumulative;
        MissingAllowed: 3),
      ( InputFile: '..\data\AggregateTenMin.hts';
        ResultsFile: '..\data\AggregateHourlyAv.hts';
        VariableType: vtAverage;
        MissingAllowed: 3),
      ( InputFile: '..\data\AggregateTenMin.hts';
        ResultsFile: '..\data\AggregateHourlyMax.hts';
        VariableType: vtMaximum;
        MissingAllowed: 3),
      ( InputFile: '..\data\AggregateTenMin.hts';
        ResultsFile: '..\data\AggregateHourlyMin.hts';
        VariableType: vtMinimum;
        MissingAllowed: 3)
    );

function TestAggregation(Verbose: Boolean): string;
var
  Source, Dest, Ref: TTimeseries;
  i: Integer;
  s: string;
  Options: TAggregationOptionsRec;
begin
  Result := '';
  Source := nil;
  Dest := nil;
  Ref := nil;
  for i := 0 to Length(TestSets)-1 do
  begin
    try
      Options.MissingAllowed := TestSets[i].MissingAllowed;
      Options.MissingFlag := 'MISSING';
      Source := TTimeseries.Create;
      Dest := TTimeseries.Create;
      Ref := TTimeseries.Create;
      Source.LoadFromFile(TestSets[i].InputFile);
      Ref.LoadFromFile(TestSets[i].ResultsFile);
      Dest.TimeStep := Ref.TimeStep;
      Dest.VariableType := TestSets[i].VariableType;
      if Verbose then
        Result := Result+'Aggregating '+ TestSets[i].InputFile +
        ' and comparing with '+ TestSets[i].ResultsFile +'...'#13#10;
      Source.Aggregate(Dest, nil, Options);
      s := CompareTimeSeries(Ref, Dest);
      if s<>'' then
        raise Exception.Create(s);
      Inc(TestsPassed);
    finally
      Source.Free;
      Dest.Free;
      Ref.Free;
      Source := nil;
      Dest := nil;
      Ref := nil;
    end;
  end;
end;

function test(Verbose: Boolean): string;
begin
  TestsPassed := 0;
  Result := Result + TestAggregation(Verbose);
  Result := Result + IntToStr(TestsPassed) + ' tests passed';
end;

end.
