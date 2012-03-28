{******************************************************************}
{                                                                  }
{  Thelma library                                                  }
{                                                                  }
{  Copyright (c) 2000-12 National Technical University of Athens   }
{                                                                  }
{******************************************************************}

{** Procedures for Climacograms. }

unit climacgr;

interface

uses ts;

{** Standard deviation for a series ATimeseries, aggregated to scale SScale.
    Original code by d.k. in VBA:
    Returns -1 if there are to few groups to calculate standard
    deviation.
    https://openmeteo.org/code/ticket/256
}
function AggrStDev(ATimeseries: TTimeseries; SScale: Integer): Real;

{** Standard deviation (all) for a series ATimeseries, aggregated to scale SScale.
    This is the second version and it considers all possible sums for a given
    scale.
    Returns -1 if there are to few data to calculate.
    Original code by d.k. in VBA:
    https://openmeteo.org/code/ticket/256
}
function AggrStDevAll(ATimeseries: TTimeseries; SScale: Integer): Real;

implementation

uses math, stat2, dates, tsprocess;

function GetPeriod(ATimeseries: TTimeseries): TDateTimeList;
var
  Begining, Ending, ADateTime: TDateTime;
begin
  Result := nil;
  try
    Result := TDateTimeList.Create;
    if ATimeseries.Count<1 then
      Exit;
    Begining := ATimeseries.DownTimestamp(ATimeseries.First.Date);
    Ending := ATimeseries.UpTimestamp(ATimeseries.Last.Date);
    ADateTime := Begining;
    while DiffInSecs(ADateTime, Ending)<=1 do
    begin
      Result.Add(ADateTime);
      ADateTime := ATimeseries.NextTimestamp(ADateTime);
    end;
  except
    Result.Free;
    raise
  end;
end;

type
  TArrayOfReal = array of real;

function AggrStDev(ATimeseries: TTimeseries; SScale: Integer): Real;
var
  i, j, t, Index, Count, NotNullCount: Integer;
  IgnoreFlag: Boolean;
  Sum: Real;
  aa: TArrayOfReal;
  Period: TDateTimeList;
begin
  Assert(ATimeseries.TimeStepStrict);
  aa := nil;
  Period := nil;
  try
    Period := GetPeriod(ATimeseries);
    Count := Period.Count div SScale;
    NotNullCount := Count;
    SetLength(aa, Count);
    t := 0;
    for i := 0 to Count - 1 do
    begin
      IgnoreFlag := False;
      Sum := 0;
      for j := i*SScale to (i+1)*SScale - 1 do
      begin
        Index := ATimeseries.IndexOf(Period[j]);
        if (Index<0) or ATimeseries[Index].IsNull then
        begin
          IgnoreFlag := True;
          Break;
        end else
          Sum := Sum + ATimeseries[Index].AsFloat;
      end;
      if IgnoreFlag then
      begin
        Dec(NotNullCount);
        Continue;
      end;
      aa[t] := Sum;
      Inc(t);
    end;
    if NotNullCount>1 then
      Result := sqrt(variance(aa, NotNullCount))/SScale
    else
      Result := -1;
  finally
    Period.Free;
    aa := nil;
  end;
end;

function AggrStDevAll(ATimeseries: TTimeseries; SScale: Integer): Real;
var
  i, j, k, t, c, Index, Count, NotNullCount, NotNullScale: Integer;
  IgnoreFlag: Boolean;
  Sum: Real;
  b, aa: TArrayOfReal;
  Period: TDateTimeList;
begin
  Assert(ATimeseries.TimeStepStrict);
  b := nil;
  Period := nil;
  try
    SetLength(b, SScale);
    c := 0;
    NotNullScale := SScale;
    Period := GetPeriod(ATimeseries);
    for k := 0 to SScale - 1 do
    begin
      Count := (Period.Count - k) div SScale;
      NotNullCount := Count;
      aa := nil;
      try
        SetLength(aa, Count);
        t := 0;
        for i := 0 to Count - 1 do
        begin
          IgnoreFlag := False;
          Sum := 0;
          for j := i*SScale+k to (i+1)*SScale+k - 1 do
          begin
            Index := ATimeseries.IndexOf(Period[j]);
            if (Index<0) or ATimeseries[Index].IsNull then
            begin
              IgnoreFlag := True;
              Break;
            end else
              Sum := Sum + ATimeseries[Index].AsFloat;
          end;
          if IgnoreFlag then
          begin
            Dec(NotNullCount);
            Continue;
          end;
          aa[t] := Sum;
          Inc(t);
        end;
        if NotNullCount>1 then
          b[c] := Sqrt(variance(aa, NotNullCount))
        else begin
          Dec(NotNullScale);
          Continue;
        end;
        Inc(c);
      finally
        aa := nil;
      end;
    end;
    if NotNullScale>0 then
      Result := mean(b, NotNullScale) / SScale
    else
      Result := -1;
  finally
    Period.Free;
    b := nil;
  end;
end;

end.
