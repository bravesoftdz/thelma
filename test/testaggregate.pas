unit testaggregate;

interface

uses TestFramework;

type
  TTestAggregate = class(TTestCase)
  published
    procedure TestAggregate;
  end;

implementation

uses SysUtils, ts;

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
      ( InputFile: '..\test\data\AggregateTenMin.hts';
        ResultsFile: '..\test\data\AggregateHourlySum.hts';
        VariableType: vtCumulative;
        MissingAllowed: 3),
      ( InputFile: '..\test\data\AggregateTenMin.hts';
        ResultsFile: '..\test\data\AggregateHourlyAv.hts';
        VariableType: vtAverage;
        MissingAllowed: 3),
      ( InputFile: '..\test\data\AggregateTenMin.hts';
        ResultsFile: '..\test\data\AggregateHourlyMax.hts';
        VariableType: vtMaximum;
        MissingAllowed: 3),
      ( InputFile: '..\test\data\AggregateTenMin.hts';
        ResultsFile: '..\test\data\AggregateHourlyMin.hts';
        VariableType: vtMinimum;
        MissingAllowed: 3)
    );

procedure TTestAggregate.TestAggregate;
var
  Source, Dest, Ref: TTimeseries;
  i: Integer;
  s: string;
  Options: TAggregationOptionsRec;
begin
  Source := nil;
  Dest := nil;
  Ref := nil;
  for i := 0 to Length(TestSets)-1 do
  begin
    try
      Options.MissingAllowed := TestSets[i].MissingAllowed;
      Options.MissingFlag := 'MISSING';
      Options.SeasonalAggregation := False;
      Source := TTimeseries.Create;
      Dest := TTimeseries.Create;
      Ref := TTimeseries.Create;
      Source.LoadFromFile(TestSets[i].InputFile);
      Ref.LoadFromFile(TestSets[i].ResultsFile);
      Dest.TimeStep := Ref.TimeStep;
      Dest.VariableType := TestSets[i].VariableType;
      Source.Aggregate(Dest, nil, Options);
      s := CompareTimeSeries(Ref, Dest);
      CheckEquals(s, '', s);
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

initialization
  RegisterTest(TTestAggregate.Suite);

end.
