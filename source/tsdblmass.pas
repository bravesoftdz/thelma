{******************************************************************}
{                                                                  }
{  Thelma library                                                  }
{                                                                  }
{  Copyright (c) 2000-04 National Technical University of Athens   }
{                                                                  }
{******************************************************************}

{Calculations on double mass curve}
unit tsdblmass;

interface

uses classes, ts, Contnrs;

type
{** TDoubleMassCurvePoint is a record holding the properties of a point
    of a double mass curves. Independent and dependent variables are
    cumulative. Active indicates if the point will be used for the
    final disaggregation process. Usually an inactive point represents
    inconsistency.
    @author Stefanos
}
TDoubleMassCurvePoint = record
  Date: TDateTime;
  Active: Boolean;
  Independent, Dependent: Real;
end;

type
{** TDoubleMassCurve is an array of TDoubleMassCurvePoint(s) and
    it represents a Double Mass Curve.
    @author Stefanos
}
TDoubleMassCurve = array of TDoubleMassCurvePoint;

{** Use TimeseriesToDoubleMassCurve to aggregate two timeseries to a
    unique double mass curve of type TDoubleMassCurve. Independent timeseries
    represents the measurements of the base stations. Dependent timeseries are
    the measurements to reduce. TimeseriesToDoubleMassCurve does not make
    any process on DoubleMassCurve data, use the appropriate component
    to do the job.
    @author Stefanos
    @SeeAlso <See Routine=DoubleMassCurveToTimeseries>
}
procedure TimeseriesToDoubleMassCurve(Independent, Dependent: TTimeseries;
  var DoubleMassCurve: TDoubleMassCurve);

{** DoubleMassCurveToTimeseries disaggregates the Dependent variable
    of the DoubleMassCurve to the Destination timeseries. Procedure reads
    Source Timeseries, copies it to the Dest and change values of Dest
    as appropriate based on DoubleMassCurve. Changed values are marked with
    flag HOMOGEN.
    @author Stefanos
    @SeeAlso <See Routine=TimeseriesToDoubleMassCurve>
}
procedure DoubleMassCurveToTimeseries(Source, Dest: TTimeseries;
  var DoubleMassCurve: TDoubleMassCurve);

{** Reduces several timeseries based on a double mass curve.
    Independent, dependent is the timeseries before and after restoration of
    homogeinity.<p>
    TimesereriesList is an object list of timeseries objects. Timestep of
    TimeseriesList should be of greater resolution of the Independent an
    dependent timeseries.
}
procedure ReduceDblMassSeveralTimeseries(Independent, Dependent: TTimeseries;
  TimeseriesList: TObjectList);

type
{** Action types for TDblMassAction property ActionType.
    dmatMove concerns to outliers removal, dmatRotate concerns to
    homogeneity restoration and dmatRevese to date order reverse.
    @SeeAlso <See Class=TDblMassAction>
    @author Stefanos
}
  TDblMassActionType = (dmatMove, dmatRotate, dmatReverse);

type
{** An object class to store the actions requested from double mass
    curve analysis interface. Use TDblMassAction in Action lists
    to make possible undoing or redoing user requested actions.
    You may use a TObjectList to Add TDblMassAction objects.
    @author Stefanos
}
  TDblMassAction = class(TPersistent)
  public
{** Action type defines the action category.
}
    ActionType: TDblMassActionType;
{** Specify the point of actions. Point is an index in a
    double mass curve point list.
}
    Point: Integer;
{** Labmda is used on dmatRotate actions for homogeneity restoration
    by curve rotation.
}
    Lambda: Real;
{** Compares to TDblMassActions and returns True if actions are
    indentical. Use this feature for advanced undo - redo preocessing.
}
    function Compare(AnotherAction: TDblMassAction): Boolean;
  end;

{** Fits with least square a linear relationship of independent variable (x)
    and dependent variable (y) with an equation form of y=a+bx on
    (x,y) points using points from  StartPoint to EndPoint.
    The result is return by reference to a (constant term), b (multiplier)
    and rxy (coefficient of determination).
}
procedure FitLineOnChartSeries(XValues, YValues: array of Real;
  StartPoint, EndPoint: Integer; var a, b, rxy: Real);

{** Fits with least square a linear relationship y=bx (homogeneous line with
    constant term "a" equal to zero a=0) on Series data
    using points from StartPoint to EndPoint.
    The result is return by reference to a (constant term, equal to zero) and
    b (multiplier).
    @author Stefanos
}
procedure FitHomogeneousLineOnChartSeries(XValues, YValues: array of Real;
  StartPoint, EndPoint: Integer; var a, b: Real);

{** Fits least square line on chart data with the initial constraint of the
    first point (startPoint) to be the last point of an homogeneous line
    with equation y=alpha*x.
    The result is return by reference to a (constant term) and b (multiplier).
    @author Stefanos
}
procedure FitLineOnChartSeriesWithConstraint(XValues, YValues: array of Real;
  StartPoint, EndPoint: Integer; alpha: Real; var a, b: Real);

{** Reduce a double mass curve by Action. Use reduce for direct user
    requested actions or for redoing undone actions.
    @SeeAlso <See Routine=UnreduceDblMassCurve>
    @SeeAlso <See Class=TDblMassAction>
    @author Stefanos
}
procedure ReduceDblMassCurve(var DoubleMassCurve: TDoubleMassCurve;
  Action: TDblMassAction);

{** Unreduce a double mass curve by Action. This is the reverse action
    of ReduceDblMassCurve and it is used for the "udno" purposes.
    @SeeAlso <See Routine=ReduceDblMassCurve>
    @SeeAlso <See Class=TDblMassAction>
    @author Stefanos
}
procedure UnreduceDblMassCurve(var DoubleMassCurve: TDoubleMassCurve;
  Action: TDblMassAction);

{** Calculates the determination factor for a bilinear curve:
    y=a1+b1x, y=a2+b2x, where the chart data on XValues, YValues and
    ChangePoint the section of the lines. Use the factor for
    automatically retrieving the point of rotation.
    @author Stefanos
}
function BiLDeterminationFactor(XValues, YValues: array of Real; ChangePoint: Integer;
  a1, b1, a2, b2: Real): Real;

{** Reverse the chronological order of a double mass curve aggregating
    from the end to the start of the initial double mass curve.
    @author Stefanos
}
procedure ReverseDoubleMass( var DoubleMassCurve: TDoubleMassCurve);

{** Check for superior outliers on series points (y values)
    n is the order of the outlier. if n < Series.Count then
    more outliers may exist. j is specific point of series
    corresponding to the outlier retrived. Dsup is a the
    statistic parameter: Dsup = (Xn-Xn-1)/(Xn/X1).
    If Dsup>D0.05=0.182+2.3/n then point j may be
    outlier.
    @author Stefanos
}
procedure FindSupOutlier(XValues, YValues: array of Real;
  var n, j: Integer; var Dsup: Real);

{** Check if Dsup is outlier. For superior outliers: Dsup = (Xn-Xn-1)/(Xn/X1).
    If Dsup>D0.05=0.182+2.3/n then point j may be outlier, and
    IsOutlier returns True.
    @author Stefanos    
}
function IsOutlier(n: Integer; Dvalue: Real): Boolean;

implementation

uses stat2, Dates, tsprocess, sysutils, DateUtils;

resourcestring
  rsInsufficientPointsForDoubleMass =
    'Insufficient number of points for double mass curve';

procedure TimeseriesToDoubleMassCurve(Independent, Dependent: TTimeseries;
  var DoubleMassCurve: TDoubleMassCurve);
var
  TimeseriesList: TObjectList;
  DateTimeList: TDateTimeList;
  i: Integer;
begin
  TimeseriesList := nil;
  DateTimeList := nil;
  try
    TimeseriesList := TObjectList.Create(False);
    TimeseriesList.Add(Independent);
    TimeseriesList.Add(Dependent);
    DateTimeList := GetCommonPeriod(TimeseriesList,0);
    if DateTimeList.Count<3 then
      raise Exception.Create(rsInsufficientPointsForDoubleMass);
    SetLength(DoubleMassCurve, DateTimeList.Count);
    for i := 0 to DateTimeList.Count - 1 do
    begin
      DoubleMassCurve[i].Active := True;
      DoubleMassCurve[i].Date := DateTimeList[DateTimeList.Count - i -1];
      DoubleMassCurve[i].Independent :=
        Independent[Independent.IndexOf(DateTimeList[DateTimeList.Count - i - 1])].AsFloat;
      DoubleMassCurve[i].Dependent :=
        Dependent[Dependent.IndexOf(DateTimeList[DateTimeList.Count - i - 1])].AsFloat;
    end;
    for i := 1 to DateTimeList.count - 1 do
    begin
      DoubleMassCurve[i].Independent := DoubleMassCurve[i].Independent +
        DoubleMassCurve[i-1].Independent;
      DoubleMassCurve[i].Dependent := DoubleMassCurve[i].Dependent +
        DoubleMassCurve[i-1].Dependent;
    end;
  finally
    DateTimeList.Free;
    TimeseriesList.Free;
  end;
end;

procedure DoubleMassCurveToTimeseries(Source, Dest: TTimeseries;
  var DoubleMassCurve: TDoubleMassCurve);
var
  i,j: Integer;
begin
  Dest.Assign(Source);
  for i := Length(DoubleMassCurve)-1 downto 1 do
  begin
    for j := i-1 downto 0 do
    begin
      if DoubleMassCurve[j].Active then
      begin
        DoubleMassCurve[i].Dependent := DoubleMassCurve[i].Dependent -
          DoubleMassCurve[j].Dependent;
        Break;
      end;
    end;
  end;
  for i := 0 to Length(DoubleMassCurve)-1 do
  begin
    j := Dest.IndexOf(DoubleMassCurve[i].Date);
    if j<0 then Continue;
    if not DoubleMassCurve[i].Active then
    begin
      Dest[j].SetFlag('INCONSISTENT',True);
    end else begin
      if (Abs(Dest[j].AsFloat - DoubleMassCurve[i].Dependent) > 0.01) then
      begin
        Dest[j].AsFloat := DoubleMassCurve[i].Dependent;
        Dest[j].SetFlag('HOMOGEN', True);
      end
    end;
  end;
end;

resourcestring
  rsReducingTimeseriesShouldBe =
    'Reducing time series should have time step less than monthly';

procedure ReduceDblMassSeveralTimeseries(Independent, Dependent: TTimeseries;
  TimeseriesList: TObjectList);
var
  i, j, k, FromIndex, ToIndex: Integer;
  AFactor: Real;
  ADate1, ADate2, ADate3, ADate4: TDateTime;
begin
  Assert(Independent.TimeStep=Dependent.TimeStep);
  Assert(Independent.Count=Dependent.Count);
  Assert(Independent.First.Date = Dependent.First.Date);
  for i := 0 to TimeseriesList.Count-1 do
  begin
    if Independent.Timestep = tstAnnual then
      if TTimeseries(TimeseriesList[i]).Timestep = tstAnnual then
        raise Exception.Create(rsReducingTimeseriesShouldBe);
    if Independent.TimeStep=tstMonthly then
      if TTimeseries(TimeseriesList[i]).TimeStep>= tstMonthly then
        raise Exception.Create(rsReducingTimeseriesShouldBe);
  end;
  for i := 0 to Dependent.Count-1 do
  begin
    AFactor := 1;
    if (not Independent[i].IsNull) and (not Dependent[i].IsNull) then
      if Abs(Independent[i].AsFloat)>0.0001 then
        AFactor := Dependent[i].AsFloat / Independent[i].AsFloat;
    ADate1 := Independent.IntervalStartPoint(Independent[i].Date);
    ADate2 := Independent.IntervalEndPoint(Independent[i].Date);
    for j := 0 to TimeseriesList.Count-1 do
      with TimeseriesList[j] as TTimeseries do
      begin
        ADate3 := IntervalStartPoint(ContainingInterval(ADate1));
        ADate4 := IntervalEndPoint(ContainingInterval(ADate2));
        FromIndex := IndexOf(ADate3);
        ToIndex := IndexOf(ADate4);
        if FromIndex<0 then FromIndex := 0;
        if ToIndex<0 then ToIndex := Count-1;
        for k := FromIndex to ToIndex do
         if (DiffInSecs(IntervalMidPoint(Items[k].Date), ADate1)>=0) and
           (DiffInSecs(IntervalMidPoint(Items[k].Date), ADate2)<0) then
         begin
           if not Items[k].IsNull then
             if Abs(AFactor-1)>0.0001 then
             begin
               Items[k].AsFloat := Items[k].AsFloat * AFactor;
               Items[k].SetFlag('HOMOGEN',True);
             end;
         end;
      end;
  end;
end;

function TDblMassAction.Compare(AnotherAction: TDblMassAction): Boolean;
begin
  Result := False;
  if (AnotherAction.ActionType = ActionType) and
    (AnotherAction.Point = Point) and
    (AnotherAction.Lambda = Lambda) then
    Result := True;
end;

procedure FitLineOnChartSeries(XValues, YValues: array of Real;
  StartPoint, EndPoint: Integer; var a, b, rxy: Real);
var
  SumX, SumY, SumX2, SumY2, SumXY: Real;
  i, Count: Integer;
begin
  a := 0;
  b := 0;
  rxy := 0;
  if (StartPoint >= EndPoint) or (StartPoint <0) or
    (EndPoint > Length(XValues)-1) then
    Exit;
  Count := 0;
  SumX := 0;
  SumY := 0;
  SumX2 := 0;
  SumY2 := 0;
  SumXY := 0;
  for i := StartPoint to EndPoint do
  begin
    Inc(Count);
    SumX := SumX + XValues[i];
    SumY := SumY + YValues[i];
    SumX2 := SumX2 + Sqr(XValues[i]);
    SumY2 := SumY2 + Sqr(YValues[i]);
    SumXY := SumXY + XValues[i]*YValues[i];
  end;
  rxy := (Count*SumXY - SumX*SumY) / Sqrt( (Count*SumX2-Sqr(SumX))*
    (Count*SumY2-Sqr(SumY)) );
  b := rxy * Sqrt( (Count*SumY2-Sqr(SumY))/(Count*SumX2-Sqr(SumX)) );
  a := SumY/Count - b*SumX/Count;
end;

procedure ReduceDblMassCurve(var DoubleMassCurve: TDoubleMassCurve;
  Action: TDblMassAction);
var
  i, LegalPoint: Integer;
  AValue: Real;
begin
  LegalPoint := Action.Point;
  case Action.ActionType of
    dmatRotate:
      begin
        for i := Action.Point + 1 to Length(DoubleMassCurve) - 1 do
        begin
          if not DoubleMassCurve[i].Active then Continue;
          DoubleMassCurve[i].Dependent :=
            DoubleMassCurve[Action.Point].Dependent + Action.Lambda *
            (DoubleMassCurve[i].Dependent -
            DoubleMassCurve[Action.Point].Dependent);
        end;
      end;
    dmatMove:
      begin
        AValue := 0;
        for i := Action.Point -1 downto 0 do
        begin
          LegalPoint := i;
          if DoubleMassCurve[i].Active then
          begin
            AValue := DoubleMassCurve[i].Dependent;
            break;
          end;
        end;
        DoubleMassCurve[Action.Point].Active := False;
        for i := Action.Point + 1 to Length(DoubleMassCurve) -1 do
        begin
          DoubleMassCurve[i].Dependent :=
            DoubleMassCurve[i].Dependent + AValue -
            DoubleMassCurve[Action.Point].Dependent;
          DoubleMassCurve[i].Independent :=
            DoubleMassCurve[i].Independent +
            DoubleMassCurve[LegalPoint].Independent -
            DoubleMassCurve[Action.Point].Independent;
        end;
      end;
    dmatReverse:
      ReverseDoubleMass( DoubleMassCurve );
  end;
end;

procedure UnreduceDblMassCurve(var DoubleMassCurve: TDoubleMassCurve;
  Action: TDblMassAction);
var
  i, LegalPoint: Integer;
  AValue: Real;
begin
  LegalPoint := Action.Point;
  case Action.ActionType of
    dmatRotate:
      begin
        for i := Action.Point + 1 to Length(DoubleMassCurve) - 1 do
        begin
          if not DoubleMassCurve[i].Active then Continue;
          DoubleMassCurve[i].Dependent :=
            DoubleMassCurve[Action.Point].Dependent +
            (DoubleMassCurve[i].Dependent -
            DoubleMassCurve[Action.Point].Dependent) / Action.Lambda;
        end;
      end;
    dmatMove:
      begin
        AValue := 0;
        for i := Action.Point -1 downto 0 do
        begin
          LegalPoint := i;
          if DoubleMassCurve[i].Active then
          begin
            AValue := DoubleMassCurve[i].Dependent;
            break;
          end;
        end;
        DoubleMassCurve[Action.Point].Active := True;
        for i := Action.Point + 1 to Length(DoubleMassCurve) -1 do
        begin
          DoubleMassCurve[i].Dependent :=
            DoubleMassCurve[i].Dependent - AValue +
            DoubleMassCurve[Action.Point].Dependent;
          DoubleMassCurve[i].Independent :=
            DoubleMassCurve[i].Independent -
            DoubleMassCurve[LegalPoint].Independent +
            DoubleMassCurve[Action.Point].Independent;
        end;
      end;
    dmatReverse:
      ReverseDoubleMass( DoubleMassCurve );
  end;
end;

function BiLDeterminationFactor( XValues, YValues: array of Real;
  ChangePoint: Integer;
  a1, b1, a2, b2: Real): Real;
var
  i, ACount: Integer;
  SumY, SumY2, SumW2: Real;
begin
  ACount := 0;
  SumY := 0;
  SumY2 := 0;
  SumW2 := 0;
  for i := 0 to ChangePoint-1 do
  begin
    SumW2 := SumW2 + Sqr( YValues[i] - a1 - b1 * XValues[i] );
    SumY := SumY + YValues[i];
    SumY2 := SumY2 + Sqr( YValues[i] );
    Inc(ACount);
  end;
  for i := ChangePoint to Length(XValues) -1 do
  begin
    SumW2 := SumW2 + Sqr( YValues[i] - a2 - b2 * XValues[i] );
    SumY := SumY + YValues[i];
    SumY2 := SumY2 + Sqr( YValues[i] );
    Inc(ACount);
  end;
  Result := 1 - SumW2 / (SumY2 - Sqr(SumY)/ACount);
end;

procedure FitHomogeneousLineOnChartSeries(XValues, YValues: array of Real;
  StartPoint, EndPoint: Integer; var a, b: Real);
var
  i: Integer;
  SumXY, SumX2: Real;
begin
  a := 0;
  b := 0;
  if (StartPoint >= EndPoint) or (StartPoint <0) or
    (EndPoint > Length(XValues)-1) then
    Exit;
  SumXY := 0;
  SumX2 := 0;
  for i := StartPoint to EndPoint do
  begin
    SumX2 := SumX2 + Sqr(XValues[i]);
    SumXY := SumXY + XValues[i]*YValues[i];
  end;
  b := SumXY / SumX2;
end;

procedure FitLineOnChartSeriesWithConstraint(XValues, YValues: array of Real;
  StartPoint, EndPoint: Integer; alpha: Real; var a, b: Real);
var
  x0, SumX, SumY, SumX2, SumXY: Real;
  i, ACount: Integer;
begin
  ACount := 0;
  a := 0;
  b := 0;
  if (StartPoint >= EndPoint) or (StartPoint <0) or
    (EndPoint > Length(XValues)-1) then
    Exit;
  SumX := 0;
  SumY := 0;
  sumX2 := 0;
  sumXY := 0;
  x0 := XValues[StartPoint];
  for i := StartPoint to EndPoint do
  begin
    Inc(ACount);
    SumX := SumX + XValues[i];
    SumY := SumY + YValues[i];
    SumX2 := SumX2 + Sqr(XValues[i]);
    SumXY := SumXY + XValues[i]*YValues[i];
  end;
  b := ACount*alpha*Sqr(x0)+ SumXY - x0*(SumY+alpha*SumX);
  b := b / (ACount*Sqr(x0)+SumX2-2*x0*SumX);
  a := (alpha - b)*x0;
end;

procedure ReverseDoubleMass( var DoubleMassCurve: TDoubleMassCurve);
var
  i,j: Integer;
  ADoubleMassCurvePoint: TDoubleMassCurvePoint;
begin
  for i := Length(DoubleMassCurve)- 1 downto 1 do
  begin
    DoubleMassCurve[i].Independent := DoubleMassCurve[i].Independent -
      DoubleMassCurve[i-1].Independent;
    for j := i-1 downto 0 do
    begin
      if DoubleMassCurve[j].Active then
      begin
        DoubleMassCurve[i].Dependent := DoubleMassCurve[i].Dependent -
          DoubleMassCurve[j].Dependent;
        Break;
      end;
    end;
  end;
  for i := 0 to Length(DoubleMassCurve) div 2 - 1 do
  begin
    ADoubleMassCurvePoint := DoubleMassCurve[i];
    DoubleMassCurve[i] := DoubleMassCurve[Length(DoubleMassCurve)-1-i];
    DoubleMassCurve[Length(DoubleMassCurve)-1-i] := ADoubleMassCurvePoint;
  end;
  for i := 1 to Length(DoubleMassCurve) - 1 do
  begin
    DoubleMassCurve[i].Independent := DoubleMassCurve[i].Independent +
      DoubleMassCurve[i-1].Independent;
    for j := i-1 downto 0 do
    begin
      if DoubleMassCurve[j].Active then
      begin
        DoubleMassCurve[i].Dependent := DoubleMassCurve[i].Dependent +
          DoubleMassCurve[j].Dependent;
        Break;
      end;
    end;
  end;
end;

procedure FindSupOutlier(XValues, YValues: array of Real;
  var n, j: Integer; var Dsup: Real);
var
  i: Integer;
  l: array of real;
begin
  try
    SetLength(l,Length(XValues));
    for i := 0 to Length(XValues)-1 do
      l[i] := YValues[i];
    for i := Length(XValues)-1 downto 1 do
      l[i] := l[i]-l[i-1];
    QuickSortAsc(l,Length(XValues));
    n := Length(XValues)-1;
    Dsup := (l[n]-l[n-1])/(l[n]-l[0]);
    for i := Length(XValues)-1 downto Length(XValues)*9 div 10 do
    begin
      n := i;
      Dsup := (l[n]-l[n-1])/(l[n]-l[0]);
      if Dsup > 0.182+2.3/n then Break;
    end;
    j := 0;
    for i := 1 to Length(XValues)-1 do
    begin
      if Abs(YValues[i]-YValues[i-1]-l[n])<=0.01 then
      begin
        j := i;
        Break;
      end;
    end;
  finally
    SetLength(l,0);
  end;
end;

function IsOutlier(n: Integer; Dvalue: Real): Boolean;
begin
  Result := False;
  if DValue>0.182+2.3/n then
    Result := True;
end;

end.
