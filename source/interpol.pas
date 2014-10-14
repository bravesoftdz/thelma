{******************************************************************}
{                                                                  }
{  Thelma library                                                  }
{                                                                  }
{  Copyright (c) 2000-05 National Technical University of Athens   }
{                                                                  }
{******************************************************************}

{** Classes for interpolation in curves. }
unit interpol;

interface

uses Classes, SysUtils, Contnrs, Ts, Math, Dates;

type
  {** A structure for the storage of hytrometric points.
  }
  THydrometricPoint = record
    Stage, Discharge: Real;
    Date: TDateTime;
    Outlier: Boolean;
  end;

  {** Represents a point of a TInterpolatingCurve.
      A point of a curve is defined by its two co-ordinates, which are named
      Independent and Dependent, rather than X and Y, because in
      stage-discharge curves the independent variable (stage) is plotted
      vertically, and X and Y would be confusing.
      @SeeAlso <See Class=TInterpolatingCurve>
      @SeeAlso <See Property=TInterpolatingCurve.Points>
  }
  TCurvePoint = record
    Independent, Dependent: Real;
  end;

  {** Compares two TCurvePoint records.
      CurvePointsEqual returns True if its two arguments have identical
      co-ordinates. Equality tests are performed, which might not be
      appropriate for floating point numbers.
  }
  function CurvePointsEqual(Point1, Point2: TCurvePoint): Boolean;

type

  {** Stores a curve representing the relationship between an independent and a
      dependent variable.
      Use TInterpolatingCurve to store curves for stage-discharge,
      discharge-sediment discharge, reservoir stage-surface area, reservoir
      stage-volume, and so on. TInterpolatingCurve has properties for storing
      these curves and methods for interpolating values in the curves.<p>
      A curve is defined by a set of points, an offset point, and a boolean
      specifying whether to interpolate with linear or with power functions.
      @author A.X.
  }
  TInterpolatingCurve = class(TPersistent)
  private
    FPoints: array of TCurvePoint;
    FCount: Integer;
    FOffset: TCurvePoint;
    FLog: Boolean;
    function GetOffset: TCurvePoint;
    procedure SetOffset(Value: TCurvePoint);
  protected
    function Get(Index: Integer): TCurvePoint;
    procedure Put(Index: Integer; Item: TCurvePoint);
  public
    {** The number of points in the curve.
        Count is the number of entries in the Points array.
	@SeeAlso <See Property=Points>
    }
    property Count: Integer read FCount;
    {** Stores the points.
        Points is an array of TCurvePoint records, each of which holds a point
	of the curve. These points may be either in direct or reverse order.<p>
    }
    property Points[Index: Integer]: TCurvePoint read Get write Put;
    {** Specifies whether the interpolation should be done with power function.
	Set Log to True to specify that the interpolation between points is to
	be done with power functions, and to False to specify linear
	interpolation. If Log is true, the curve's segments appear as straight
	lines on log-log charts; otherwise, they appear as straight lines on
	linear charts.
    }
    property Log: Boolean read FLog write FLog;
    {** Specifies the offsets for log curves.
        Offset is used only if Log is True, and specifies the points at which
	the curve crosses the zero axes. For example, if X is the independent
	variable and Y is the dependent, then the curve crosses the X axis at
	point (X0, 0), where X0=Offset.Independent.<p>
    }
    property Offset: TCurvePoint read GetOffset write SetOffset;
    {** Creates a TInterpolatingCurve object.
    }
    constructor Create(Log: Boolean); virtual;
    {** Destroyes a TInterpolatingCurve object.
    }
    destructor Destroy; override;
    {** Copies a TInterpolatingCurve object.
        @SeeAlso <Jump File=Delphi5.hlp K="TPersistent,Assign" Text=Assign>
    }
    procedure Assign(Source: TPersistent); override;
    {** Adds a point to the end of the Points array.
        Add adds the specified point to the end of the Points array and
        returns its index.
    }
    function Add(ACurvePoint: TCurvePoint): Integer; overload;
    {** An overloaded method in order to add arithmetic values instead directly,
        instead of TCurvePoint).
    }
    function Add(IndependentValue, DependentValue: Real): Integer; overload;
    {** Deletes all points.
        Call Clear to empty the Points array and set the Count to 0.
    }
    procedure Clear; virtual;
    {** Removes the point at the position given by the Index parameter.
        Call Delete to remove from the Points array the item with the specified
	Index.
	@SeeAlso <See Method=Remove>
    }
    procedure Delete(Index: Integer);
    {** Returns Points[0].
    }
    function First: TCurvePoint;
    {** Returns the index of the first entry in the Points array with a specified value.
        Call IndexOf to get the index of a point in the Points array.<p>
	IndexOf makes simple equality tests, which may be inappropriate for
	floating point values. However, IndexOf is only provided for
	completeness and compatibility to classes such as TList. At the time of
	this writing, it is not clear if and how IndexOf will be used, so
	specifying tolerances is left for later.
    }
    function IndexOf(Item: TCurvePoint): Integer;
    {** Inserts a point at the position specified by Index.
        Call Insert to add a point to the middle of the Points array.
    }
    procedure Insert(Index: Integer; Item: TCurvePoint);
    {** Returns Points[Count-1].
    }
    function Last: TCurvePoint;
    {** Deletes the first matching item from the Points array.
        Call Remove to remove a specific point from the Points array when its
	index is unknown. The value returned is the index of the point in the
	Points array before it was removed.<p>
	If the Points array contains more than one copy of the point, only the
	first copy is deleted.<p>
	Remove uses IndexOf to locate the point, so the tolerance problem
	described in the help for IndexOf also applies here.
	@SeeAlso <See Method=Delete>
	@SeeAlso <See Method=IndexOf>
    }
    function Remove(Item: TCurvePoint): Integer;
    {** Interpolates a value in the curve.
        Call Interpolate to interpolate a value for the independent variable
	into the curve and determine the corresponding value for the dependent
	variable.
        If the specified value lies outside the segments of the curve,
        extrapolation is performed.<p>
        If Log is True, one of the points can be Offset. Although this is a
        valid, using it for interpolation would mean taking the logarithm of
        zero. Thus, the point is ignored; if the value must be interpolated in
        a segment having Offset as one of its ends, it is, instead, extrapolated
        in an adjacent segment. The result should be mathematically correct.
	@SeeAlso <See Method=ReverseInterpolate>
    }
    function Interpolate(IndependentValue: Real): Real;
    {** Determines the value of the independent variable given the value of the dependent variable.
        ReverseInterpolate is the opposite of Interpolate; it interpolates a
	value for the dependent variable into the curve and returns the
	corresponding value for the independent variable.<p>
        ReverseInterpolate will only work correctly if the curve represents
        a reversible function, i.e., if one and only one value of the
        independent variable corresponds to any given value of the dependent
        variable. If this condition does not hold, the behavior of
        ReverseInterpolate will be undefined (it will probably return one
        of the possible answers).<p>
        The remarks in Interpolate about extrapolation are also true for
        ReverseInterpolate.
	@SeeAlso <See Method=Interpolate>
    }
    function ReverseInterpolate(DependentValue: Real): Real;
  end;

  {** Stores a TInterpolatingCurve with a period of validity.
      TTransientCurve is a TInterpolatingCurve with the additional properties
      StartDate and EndDate, which specify the period of validity of this
      curve. TTransientCurve is useful for representing stage-discharge curves.
      @author A.X.
      @SeeAlso <See Class=TTransientCurveList>
  }
  TTransientCurve = class(TInterpolatingCurve)
  private
    FStartDate, FEndDate: TDateTime;
    FStartMonth, FEndMonth: Integer;
    FLinkedTo: TPersistent;
    FPrimaryCurve: Boolean;
    procedure SetLinkedTo(ATransientCurve: TPersistent);
  public
    Extension: Boolean;
{** Use this index for the curves contruction interface.
}
    GraphIndex: Integer;
    AssignedTo: TPersistent;
{** Creates a TTransientCurve object.
}
    constructor Create(Log: Boolean); override;
{** Use Linked to to link to another curve.
    Use Linked to to link to another curve, when adding several curves for
    the same period but for different seasons.
}
    property LinkedTo: TPersistent read FLinkedTo write SetLinkedTo;
    property StartDate: TDateTime read FStartDate write FStartDate;
    property EndDate: TDateTime read FEndDate write FEndDate;
    property StartMonth: Integer read FStartMonth write FStartMonth;
    property EndMonth: Integer read FEndMonth write FEndMonth;
{** Used when two curves are linked. The first is the primary, the
    second linked should copy date intervals, etc.
}
    property PrimaryCurve: Boolean read FPrimaryCurve write FPrimaryCurve;
    procedure Assign(Source: TPersistent); override;
  end;

  {** The exception class for invalid interpolation in TTransientCurveList.
      @SeeAlso <See Method=TTransientCurveList.Interpolate>
  }
  ENoInterpolCurve = class(Exception);

  {** Stores a set of curves.
      TTransientCurveList stores a set of curves and provides interpolating
      functions that interpolate a value to the chronologically appropriate
      TTransientCurve. TTransientCurveList is mostly useful for sets of
      stage-discharge curves.
      @SeeAlso <See Class=TTransientCurve>
      @author A.X.
  }
  TTransientCurveList = class(TPersistent)
  private
    FCurveList: TObjectList;
    FHydrometricPointCollection: array of THydrometricPoint;
    FHydrometricPointStartDate, FHydrometricPointEndDate: TDateTime;
    function GetCount: Integer;
    function GetCountNotExtension: Integer;
    function GetHydrometricPointsCount: Integer;
  protected
{** Returns a TTransientCurve object from the items array.
}
    function Get(Index: Integer): TTransientCurve;
{** Returns first TTransientCurve object.
}
    function GetFirst: TTransientCurve;
{** Returns last TTransientCurve object.
}
    function GetLast: TTransientCurve;
{** Returns a TTransientCurve object, but not extension line.
}
    function GetNotExtension(Index: Integer): TTransientCurve;
{** Returns first TTransientCurve, but not extension line.
}
    function GetFirstNotExtension: TTransientCurve;
{** Returns last TTransientCurve, but not extension line.
}
    function GetLastNotExtension: TTransientCurve;
{** Returns a THydrometricPoint object;
}
    function GetHydrometricPoint(Index: Integer): THydrometricPoint;
{** Write a hydrometric point;
}
    procedure SetHydrometricPoint(Index: Integer; Item: THydrometricPoint);
  public
{** Use create method to initialize a TTransientCurveList object.
}
    constructor Create;
{** Use Destroy method in order to free memory. Do not call Destroy
    directely, call Free method instead.
}
    destructor Destroy; override;
{** Copies a TTransientCurveList object.
        @SeeAlso <Jump File=Delphi5.hlp K="TPersistent,Assign" Text=Assign>
}
    procedure Assign(Source: TPersistent); override;
{** Count property returns the number of TTransientCurve object contained
    in the Items list.
}
    property Count: Integer read GetCount;
{** CountNotExtension returns the number of objects contained in the
    ItemsNotExtenstion list.
}
    property CountNotExtension: Integer read GetCountNotExtension;
{** The actual TTransientCurve Items array. Items property is default,
    you may read items by AIDFTimeseriesCollection[i].
}
    property Items[Index: Integer]: TTransientCurve read Get; default;
{** Like Items but only for not extension curves.
}
    property ItemsNotExtension[Index: Integer]: TTransientCurve read
      GetNotExtension;
{** The HydrometricPoints array.
}
    property HydrometricPoints[Index: Integer]: THydrometricPoint read
      GetHydrometricPoint write SetHydrometricPoint;
{** Returns the number of Hydrometric points.
}
    property HydrometricPointsCount: Integer read GetHydrometricPointsCount;
{** The first TTransientCurve object.
}
    property First: TTransientCurve read GetFirst;
{** The last TTransientCurve object.
}
    property Last: TTransientCurve read GetLast;
{** The first TTransientCurve object, but not extension.
}
    property FirstNotExtension: TTransientCurve read GetFirstNotExtension;
{** The last TTransientCurve object, but not extension.
}
    property LastNotExtension: TTransientCurve read GetLastNotExtension;
{** The hydrometric points start date.
    The value is assigned after the calling of SetHydrometricPoints
    method.
}
    property HydrometricPointStartDate: TDateTime
      read FHydrometricPointStartDate write FHydrometricPointStartDate;
{** The hydrometric points end date.
    The value is assigned after the calling of SetHydrometricPoints
    method.
}
    property HydrometricPointEndDate: TDateTime
      read FHydrometricPointEndDate write FHydrometricPointEndDate;
{** Transforms the Index to an Index conserning ItemsNotExtension array.
}
    function IndexNotExtension(Index: Integer): Integer;
    {** Interpolates a value to the appropriate curve.
        Interpolate searches the Items array in order to find a TTransientCurve
	such that the date of ATsRecord is within the curve's period. It then
	calls that TTransientCurve's Interpolate function to interpolate the
	value of ATsRecord, and returns the result. If no suitable curve is
	found, Interpolate raises ENoInterpolCurve.
	@SeeAlso <See Class=TTransientCurve>
	@SeeAlso <See Method=TInterpolatingCurve.Interpolate>
	@SeeAlso <See Method=ReverseInterpolate>
    }
    function Interpolate(ATsRecord: TTsRecord): Real; overload;
    {** This is an overloaded version of Interpolate, mostly, for internal use.
        You may you in most times the other version of Interpolate.
    }
    function Interpolate(AValue: Real; ADate: TDateTime): Real; overload;
    {** Performs reverse interpolation to the appropriate curve.
        ReverseInterpolate is the same as Interpolate, except that it calls the
	appropriate curve's ReverseInterpolate function rather than
	Interpolate.
	@SeeAlso <See Method=TInterpolatingCurve.ReverseInterpolate>
	@SeeAlso <See Method=Interpolate>
    }
    function ReverseInterpolate(ATsRecord: TTsRecord): Real;
    {** Read Curves data from a text file
    }
    procedure LoadFromFile(FileName: string);
    {** Write Curves data in a text file, except descriptive fields
        such as remarks and so
    }
    procedure WriteToFile(FileName, DateFormat: string; Delimiter,
                DecimalSeparator: Char);
{** Use Add method to add TTransientCurve objects in the Items array.<p>
    Add calls TTransientCurve.Create to initialize objects. Items list
    owns objects an destroy then on Clear or Free method call.
    @SeeAlso <See Method=TTransientCurve.Create>
}
    procedure Add(ATransientCurve: TTransientCurve);
{** Call Insert to add an object at a specified position in the list,
    shifting the item that previously occupied that position
    (and all subsequent items) up. Insert increments Count and,
    if necessary, allocates memory by increasing the value of Capacity.
}
    procedure Insert(Index: Integer; ATransientCurve: TTransientCurve);
{** Use Clear method to empty the items array.
}
    procedure Clear;
{** Use Delete to remove a record from the items array. Specify
    the appropriate Index.
}
    procedure Delete(Index: Integer);
{** Set the hydrometric points array from timeseries of stage, discharge.
}
    procedure SetHydrometricPoints(StageTS, DischargeTS: TTimeseries);
{** Returns the determiniation coefficient based on curves and s-d points.
    You must set first some stage - discharge measurments as well as
    stage-discarhge curves in order to compute the coefficient, or else
    exceptions may raise.
}
    function GetDeterminationCoefficient: Real; overload;
{** This is an overloaded method of GetDeterminationCoefficient.
    In this version you may set a desired period for calculations in
    order to get the coefficient e.g. for a single curve.
}
    function GetDeterminationCoefficient(AStartDate, AEndDate: TDateTime): Real;
      overload;
{** With this overloaded method you get the Determination Coefficient for a
    specific TTransientCurve Items.
    ACurve should be between zero (0) and Cound-1 or else a raise is raised.
}
    function GetDeterminationCoefficient(ACurve: Integer): Real; overload;
{** Fits a polyline for a specific TTransientCurve Item.
    Hydrometric points should be set in order to fit a polyline curve or
    else an exception is raised.
    FitPolyline clears all data before fiting the curve. Segments should
    be at least 1. Finally the determination factor is returned.
}
    function FitPolyline(ACurve: Integer; Segments: Integer): Real;
{** Extract the Timeseries from the Stage measurements.
    Outliers values are considered as null values.
    The functions Creates the Timeseries object.
}
    function ExtractStageTS: TTimeseries;
{** Extract the Timeseries from the Discharge measurements.
    Outliers values are considered as null values.
    The functions Creates the Timeseries object.
}
    function ExtractDischargeTS: TTimeseries;
  end;

  {** Store Stage - Leakage coeficients and
      interpolates leakage by stage values
      @author Stefanos
  }
  TLeakageInterpolation = class(TPersistent)
  public
    FHmin, FHmax: Real;
    FACoef: array[1..12] of Real;
    FBCoef: array[1..12] of Real;
    FCCoef: array[1..12] of Real;
    FECoef: array[1..12] of Real;
    {** The maximum stage of a reservoir
    }
    property Hmin: Real read FHmin;
    {** The minimum stage of a reservoir
    }
    property Hmax: Real read FHmax;
    {** Interpolate a value of stage for the curves corresponding
      to the specified Month
      @SeeAlso <See Method=InterpolateTS>
    }
    function Interpolate(Month: Integer; Stage: Real): Real;
    {** Interpolate values of a Stage Timeseries (Source). The
      result (leakage values) is written to the Destination
      Timeseries
      @SeeAlso <See Method=Interpolate>
    }
    procedure InterpolateTS(Source, Destination: TTimeseries);

end;

{** Accounts for differences between a dense and a sparse time series.
    Sometimes there are two sets of measurements of a quantity, with different
    time step. For example, in rivers, there may be a time series coming from a
    recorder, with an hourly time step, and a time series coming from a gauge,
    with a daily time step. These two time series should, in theory, have
    coinciding values at the points of coinciding times, but in practice there
    are differences.<p>
    InterpolateDiffs reads ReferenceSeries and CorrectableSeries.
    ReferenceSeries is a relatively sparse time series, which is considered to
    be correct; CorrectableSeries is a relatively dense time series, which is
    considered to be less accurate. InterpolateDiffs checks whether for
    coinciding dates the two time series have identical values. If not, it
    estimates the corrections that must be added to the values of
    CorrectableSeries to make them compatible to ReferenceSeries. The results
    are returned in ResultingDelta (whose original contents are deleted). On
    return ResultingDelta contains as many records as CorrectableSeries, with
    the same dates. ResultingDelta is the time series that must be added to
    CorrectableSeries to make it compatible to ReferenceSeries.<p>
    After calling InterpolateDiffs, you may want to add ResultingDelta to
    CorrectableSeries using AddTimeseries, and merge the result with
    ReferenceSeries using TTimeseries.Merge.<p>
    InterpolateDiffs was created in order to correct stage series.<p>
    The implementation contains a comment that describes the algorithm exactly.
    @author A.X.
    @SeeAlso <See Routine=AddTimeseries>
    @SeeAlso <See Method=TTimeseries.Merge>
    @SeeAlso <See Routine=StageCorrect>
}
procedure InterpolateDiffs(ReferenceSeries, CorrectableSeries, ResultingDelta:
  TTimeseries);

{** Creates artificial stage alterations to account for the fact that discharge measurements don't fall exactly on the rating curves.
    Use StageCorrect to make stage corrections (also known as "Stout
    corrections"). StageSeries is a river stage series. SDStage and SDDischarge
    must have an equal number of records with identical dates, and represent
    the outcome of discharge measurements. The results are returned in
    ResultingDelta (whose original contents are deleted). On return
    ResultingDelta contains as many records as StageSeries, with
    the same dates. ResultingDelta is the time series that must be added to
    StageSeries to correct it.<p>
    After calling StageCorrect, you may want to add ResultingDelta to
    StageSeries using AddTimeseries.<p>
    @author A.X.
    @SeeAlso <See Routine=AddTimeseries>
    @SeeAlso <See Routine=InterpolateDiffs>
    @SeeAlso <See Routine=TsInterpolate>
}
procedure StageCorrect(StageSeries, SDStage, SDDischarge: TTimeseries;
    Curves: TTransientCurveList; ResultingDelta: TTimeseries);

{** Interpolates a time series to a curve or set of curves.
    Use TsInterpolate to interpolate the entire Source time series to the
    specified Curve. The result is returned in Dest, whose previous contents
    are deleted.
    @author A.X.
}
procedure TsInterpolate(Source, Dest: TTimeseries; Curve: TInterpolatingCurve);
  overload;
{** Interpolates a time series to a curve or set of curves.
    Use TsInterpolate to interpolate the entire Source time series to the
    specified curve set. The result is returned in Dest, whose previous
    contents are deleted.
    @author A.X.
}
procedure TsInterpolate(Source, Dest: TTimeseries;
  Curves: TTransientCurveList); overload;

{** Decrease Stage Correct if extensions lines are considered
    in Curves.
    @author Stefanos.
}
procedure DecreaseStageCorrect(Curves: TTransientCurveList;
  StageSeries, ResultingDelta: TTimeseries);

implementation

{$C+}

uses tsprocess, matrix, GenUtils, istrutils;

function CurvePointsEqual(Point1, Point2: TCurvePoint): Boolean;
begin
  Result := ( Abs(Point1.Dependent-Point2.Dependent)<0.001 ) and
            ( Abs(Point1.Independent-Point2.Independent)<0.001 );
end;

{ TInterpolatingCurve }

constructor TInterpolatingCurve.Create(Log: Boolean);
begin
  inherited Create;
  SetLength(FPoints, 5);
  FCount := 0;
  FLog := Log;
  FOffset.Dependent := 0;
  FOffset.Independent := 0;
end;

destructor TInterpolatingCurve.Destroy;
begin
  SetLength(FPoints, 0);
  inherited Destroy;
end;

procedure TInterpolatingCurve.Assign(Source: TPersistent);
var i: Integer;
begin
  with Source as TInterpolatingCurve do
  begin
    Self.FOffset := FOffset;
    Self.FLog := FLog;
    Self.Clear;
    for i := 0 to Count-1 do
      Self.Add(Points[i]);
  end;
end;

resourcestring
  rsOffsetUnavailableForNonLog =
    'TInterpolatingCurve.Offset can only be used when Log is True.';

function TInterpolatingCurve.GetOffset: TCurvePoint;
begin
  if not FLog then raise Exception.Create(rsOffsetUnavailableForNonLog);
  Result := FOffset;
end;

procedure TInterpolatingCurve.SetOffset(Value: TCurvePoint);
begin
  if not FLog then raise Exception.Create(rsOffsetUnavailableForNonLog);
  FOffset := Value;
end;

resourcestring
  rsListIndexOutOfBounds = 'List index out of bounds';
  
function TInterpolatingCurve.Get(Index: Integer): TCurvePoint;
begin
  if (Index<0) or (Index>=FCount) then
    raise EListError.Create(rsListIndexOutOfBounds + '('+IntToStr(Index)+')');
  Result := FPoints[Index];
end;

procedure TInterpolatingCurve.Put(Index: Integer; Item: TCurvePoint);
begin
  if (Index<0) or (Index>=FCount) then
    raise EListError.Create(rsListIndexOutOfBounds + '('+IntToStr(Index)+')');
  FPoints[Index] := Item;
end;

function TInterpolatingCurve.Add(ACurvePoint: TCurvePoint): Integer;
begin
  if FCount = Length(FPoints) then
    SetLength(FPoints, Length(FPoints)+5);
  Result := FCount;
  FPoints[Result] := ACurvePoint;
  Inc(FCount);
end;

function TInterpolatingCurve.Add(IndependentValue,
  DependentValue: Real): Integer;
var
  ACurvePoint: TCurvePoint;
begin
  ACurvePoint.Independent := IndependentValue;
  ACurvePoint.Dependent := DependentValue;
  Result := Add(ACurvePoint);
end;

procedure TInterpolatingCurve.Clear;
begin
  FCount := 0;
  SetLength(FPoints, 5);
end;

procedure TInterpolatingCurve.Delete(Index: Integer);
begin
  if (Index<0) or (Index>=FCount) then
    raise EListError.Create(rsListIndexOutOfBounds + '('+IntToStr(Index)+')');
  while Index<FCount-1 do
  begin
    FPoints[Index] := FPoints[Index+1];
    Inc(Index);
  end;
  Dec(FCount);
end;

function TInterpolatingCurve.First: TCurvePoint;
begin
  Result := FPoints[0];
end;

function TInterpolatingCurve.IndexOf(Item: TCurvePoint): Integer;
begin
  Result := FCount-1;
  while Result>=0 do
  begin
    if CurvePointsEqual(Item, FPoints[Result]) then Exit;
    Dec(Result);
  end;
end;

procedure TInterpolatingCurve.Insert(Index: Integer; Item: TCurvePoint);
var i: Integer;
begin
  if FCount = Length(FPoints) then
    SetLength(FPoints, Length(FPoints)+5);
  Inc(FCount);
  for i := FCount-1 downto Index+1 do
    FPoints[i] := FPoints[i-1];
  FPoints[Index] := Item;
end;

function TInterpolatingCurve.Last;
begin
  Result := FPoints[FCount-1];
end;

function TInterpolatingCurve.Remove(Item: TCurvePoint): Integer;
begin
  Result := IndexOf(Item);
  if Result>=0 then
    Delete(Result);
end;

function TInterpolatingCurve.Interpolate(IndependentValue: Real): Real;
var
  BaseIndex: Integer; { Will interpolate between points BaseIndex & BaseIndex+1
                      }
  x0, y0, x1, x2, y1, y2, x: Real;
begin
  { Determine BaseIndex }
  if Last.Independent>First.Independent then
  begin
    { Points are in normal sorting order }
    BaseIndex := Count - 2;
    while BaseIndex>=0 do
    begin
      if Points[BaseIndex].Independent < IndependentValue then Break;
      Dec(BaseIndex);
    end;
    if BaseIndex<0 then BaseIndex := 0; { Extrapolate downwards }
  end else
  begin
    { Points are in reverse sorting order }
    BaseIndex := Count - 2;
    while BaseIndex>=0 do
    begin
      if Points[BaseIndex].Independent > IndependentValue then Break;
      Dec(BaseIndex);
    end;
    if BaseIndex<0 then BaseIndex := 0; { Extrapolate downwards }
  end;
  { Avoid points equal to offset. }
  if Log and CurvePointsEqual(Points[BaseIndex], Offset) then Inc(BaseIndex);
  if Log and CurvePointsEqual(Points[BaseIndex+1], Offset) then Dec(BaseIndex);

  { Determine offsets }
  if Log then
  begin
    x0 := Offset.Independent;
    y0 := Offset.Dependent;
  end else
  begin
    x0 := 0;
    y0 := 0;
  end;

  {I've change x1 := .... - x0 to + x0 to all
   variables, since the offset has additive character.
   Stefanos 01/07/2003
  }
  x1 := Points[BaseIndex].Independent + x0;
  x2 := Points[BaseIndex+1].Independent + x0;
  x  := IndependentValue + x0;
  y1 := Points[BaseIndex].Dependent + y0;
  y2 := Points[BaseIndex+1].Dependent + y0;
  if Log then
{old: Result := Exp((Ln(x)-Ln(x1))/(Ln(x2)-Ln(x1))*(Ln(y2)-Ln(y1)) + Ln(y1)) + x0
 changed by stefanos 03/07/2003}
    Result := Exp((Ln(x)-Ln(x1))/(Ln(x2)-Ln(x1))*(Ln(y2)-Ln(y1)) + Ln(y1)) - y0
  else
    Result := (x-x1)/(x2-x1)*(y2-y1) + y1;
end;

function TInterpolatingCurve.ReverseInterpolate(DependentValue: Real): Real;
var
  BaseIndex: Integer; { Will interpolate between points BaseIndex & BaseIndex+1
                      }
  x0, y0, x1, x2, y1, y2, y: Real;
begin
  { Determine BaseIndex }
  if Last.Dependent>First.Dependent then
  begin
    { Points are in normal sorting order }
    BaseIndex := Count - 2;
    while BaseIndex>=0 do
    begin
      if Points[BaseIndex].Dependent < DependentValue then Break;
      Dec(BaseIndex);
    end;
    if BaseIndex<0 then BaseIndex := 0; { Extrapolate downwards }
  end else
  begin
    { Points are in reverse sorting order }
    BaseIndex := Count - 2;
    while BaseIndex>=0 do
    begin
      if Points[BaseIndex].Dependent > DependentValue then Break;
      Dec(BaseIndex);
    end;
    if BaseIndex<0 then BaseIndex := 0; { Extrapolate downwards }
  end;
  { Avoid points equal to offset. }
  if Log and CurvePointsEqual(Points[BaseIndex], Offset) then Inc(BaseIndex);
  if Log and CurvePointsEqual(Points[BaseIndex+1], Offset) then Dec(BaseIndex);

  { Determine offsets }
  if Log then
  begin
    x0 := Offset.Independent;
    y0 := Offset.Dependent;
  end else
  begin
    x0 := 0;
    y0 := 0;
  end;

  x1 := Points[BaseIndex].Independent + x0;
  x2 := Points[BaseIndex+1].Independent + x0;
  y  := DependentValue + y0;
  y1 := Points[BaseIndex].Dependent + y0;
  y2 := Points[BaseIndex+1].Dependent + y0;
  if Log then
{ old: Result := Exp((Ln(y)-Ln(y1))/(Ln(y2)-Ln(y1))*(Ln(x2)-Ln(x1)) + Ln(x1)) + y0
 changed by Stefanos 03/07/2003}
    Result := Exp((Ln(y)-Ln(y1))/(Ln(y2)-Ln(y1))*(Ln(x2)-Ln(x1)) + Ln(x1)) - x0
  else
    Result := (y-y1)/(y2-y1)*(x2-x1) + x1;
end;

{ TTransientCurve }

constructor TTransientCurve.Create(Log: Boolean);
begin
  inherited Create(Log);
  GraphIndex := -1;
  FStartMonth := 0;
  FEndMonth := 0;
  FPrimaryCurve := True;
  FLinkedTo := nil;
  AssignedTo := nil;
end;

procedure TTransientCurve.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TTransientCurve then
  begin
    with Source as TTransientCurve do
    begin
      Self.StartDate := StartDate;
      Self.EndDate := EndDate;
      Self.Extension := Extension;
      Self.GraphIndex := GraphIndex;
      Self.StartMonth := StartMonth;
      Self.EndMonth := EndMonth;
      Self.PrimaryCurve := PrimaryCurve;
      Self.FLinkedTo := LinkedTo;
    end;
    (Source as TTransientCurve).AssignedTo := Self;
  end;
end;

procedure TTransientCurve.SetLinkedTo(ATransientCurve: TPersistent);
begin
  if (FLinkedTo = ATransientCurve) then Exit;
  FLinkedTo := ATransientCurve;
  if ATransientCurve<>nil then
    TTransientCurve(ATransientCurve).LinkedTo := Self;
end;

{ TTransientCurveList }

constructor TTransientCurveList.Create;
begin
  inherited Create;
  FCurveList := nil;
  try
    FCurveList := TObjectList.Create(True);
  except
    FCurveList.Free;
  end;
  SetLength(FHydrometricPointCollection,0);
  FHydrometricPointStartDate := 0;
  FHydrometricPointEndDate := 0;
end;

destructor TTransientCurveList.Destroy;
begin
  FCurveList.Free;
  SetLength(FHydrometricPointCollection,0);
  inherited Destroy;
end;

procedure TTransientCurveList.Assign(Source: TPersistent);
var
  i: Integer;
  ATransientCurve: TTransientCurve;
begin
  Clear;
  with Source as TTransientCurveList do
  begin
    ATransientCurve := nil;
    for i := 0 to Count-1 do
    begin
      try
        ATransientCurve := TTransientCurve.Create(True);
        ATransientCurve.Assign(Items[i]);
        Self.Add(ATransientCurve);
        ATransientCurve := nil;
      except
        ATransientCurve.Free;
        raise;
      end;
    end;
    SetLength(Self.FHydrometricPointCollection, HydrometricPointsCount);
    Self.FHydrometricPointStartDate := HydrometricPointStartDate;
    Self.FHydrometricPointEndDate := HydrometricPointEndDate;
    for i := 0 to HydrometricPointsCount-1 do
    begin
      Self.FHydrometricPointCollection[i].Date := HydrometricPoints[i].Date;
      Self.FHydrometricPointCollection[i].Stage := HydrometricPoints[i].Stage;
      Self.FHydrometricPointCollection[i].Discharge :=
        HydrometricPoints[i].Discharge;
      Self.FHydrometricPointCollection[i].Outlier :=
        HydrometricPoints[i].Outlier;
    end;
  end;
  for i := 0 to Count-1 do
    if Items[i].LinkedTo<>nil then
      if (Items[i].LinkedTo as TTransientCurve).AssignedTo<>nil then
        Items[i].LinkedTo :=
          (Items[i].LinkedTo as TTransientCurve).AssignedTo;
end;

function TTransientCurveList.GetCount: Integer;
begin
  Result := FCurveList.Count;
end;

function TTransientCurveList.GetCountNotExtension: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Count-1 do
    if not Items[i].Extension then
      Inc(Result);
end;

function TTransientCurveList.GetHydrometricPointsCount: Integer;
begin
  Result := Length(FHydrometricPointCollection);
end;

function TTransientCurveList.Get(Index: Integer): TTransientCurve;
begin
  Result := TTransientCurve(FCurveList.Items[Index]);
end;

resourcestring
  rsInterpolatingCurveOutOfBounds = 'Interpolating curve: out of bounds';

function TTransientCurveList.GetNotExtension(Index: Integer): TTransientCurve;
var
  i, j: Integer;
begin
  Result := nil;
  if (Index<0) or (Index>CountNotExtension-1) then
    raise EListError.Create(rsInterpolatingCurveOutOfBounds);
  j := -1;
  for i := 0 to Count-1 do
  begin
    if not Items[i].Extension then
      Inc(j);
    if j = Index then
    begin
      Result := Items[i];
      Exit;
    end;
  end;
end;

function TTransientCurveList.IndexNotExtension(Index: Integer): Integer;
var
  i: Integer;
begin
  if (Index<0) or (Index>Count-1) then
    raise Exception.Create(rsListIndexOutOfBounds);
  Result := -1;
  for i := 0 to Count -1 do
  begin
    if not Items[i].Extension then
      Inc(Result);
    if Index=i then
      Exit;
  end;
end;

function TTransientCurveList.GetFirst: TTransientCurve;
begin
  Result := TTransientCurve(FCurveList.First);
end;

function TTransientCurveList.GetLast: TTransientCurve;
begin
  Result := TTransientCurve(FCurveList.Last);
end;

function TTransientCurveList.GetFirstNotExtension: TTransientCurve;
begin
  if CountNotExtension<1 then
    raise EListError.Create(rsInterpolatingCurveOutOfBounds);
  Result := GetNotExtension(0);
end;

function TTransientCurveList.GetLastNotExtension: TTransientCurve;
begin
  if CountNotExtension<1 then
    raise EListError.Create(rsInterpolatingCurveOutOfBounds);
  Result := GetNotExtension(CountNotExtension-1);
end;

resourcestring
  rsNoHydrometricPointsDefined = 'No hydrometric points defined';

function TTransientCurveList.GetHydrometricPoint(Index: Integer):
  THydrometricPoint;
begin
  if Length(FHydrometricPointCollection)<1 then
    raise EListError.Create(rsNoHydrometricPointsDefined);
  Result := FHydrometricPointCollection[Index];
end;

procedure TTransientCurveList.SetHydrometricPoint(Index: Integer;
  Item: THydrometricPoint);
begin
  if (Index<0) or (Index>HydrometricPointsCount) then
    raise EListError.Create(rsListIndexOutOfBounds);
  FHydrometricPointCollection[Index] := Item;
end;

procedure TTransientCurveList.Add(ATransientCurve: TTransientCurve);
begin
  FCurveList.Add(ATransientCurve);
end;

procedure TTransientCurveList.Insert(Index: Integer;
  ATransientCurve: TTransientCurve);
begin
  FCurveList.Insert(Index, ATransientCurve);
end;

procedure TTransientCurveList.Clear;
begin
  FCurveList.Clear;
end;

procedure TTransientCurveList.Delete(Index: Integer);
begin
  FCurveList.Delete(Index);
end;

resourcestring
  rsNoInterpolCurve =
    'There is no interpolating curve in the list suitable for this date: ';

function TTransientCurveList.Interpolate(AValue: Real; ADate: TDateTime): Real;
var
  i, j: Integer;
  x0, y0: Real;
  x0_ext, y0_ext, x0mean: Real;
  ATransientCurve,AExtensionCurve: TTransientCurve;
  Year, Month, Day: Word;
  FromMonth, ToMonth: Integer;
  InMonth: Boolean;
begin
  for i := 0 to Count-1 do
  begin
    ATransientCurve := TTransientCurve(Items[i]);
    DecodeDate(ADate, Year, Month, Day);
    FromMonth := ATransientCurve.StartMonth;
    ToMonth := ATransientCurve.EndMonth;
    if ToMonth>FromMonth then
      InMonth := (Month<=ToMonth) and (Month>=FromMonth)
    else
      InMonth := (Month<=ToMonth) or (Month>=FromMonth);
    if (DiffInSecs(ATransientCurve.StartDate, ADate)<=0)
    and (DiffInSecs(ATransientCurve.EndDate, ADate)>=0)
    and (InMonth) and (not ATransientCurve.Extension) then
    begin
      Result := ATransientCurve.Interpolate(AValue);

{Check if h>hmax, then search for extension curve}
      if AValue>ATransientCurve.Last.Independent then
      begin
{First consider all curves}
        for j := 0 to Count-1 do
        begin
          AExtensionCurve := TTransientCurve(Items[j]);
{And if it is an extension curve...}
          if AExtensionCurve.Extension then
          begin
            if ATransientCurve.Log then
            begin
              x0 := ATransientCurve.Offset.Independent;
              y0 := ATransientCurve.Offset.Dependent;
            end else begin
              x0 := 0;
              y0 := 0;
            end;
            if AExtensionCurve.Log then
            begin
              x0_ext := AExtensionCurve.Offset.Independent;
              y0_ext := AExtensionCurve.Offset.Dependent;
            end else begin
              x0_ext := 0;
              y0_ext := 0;
            end;
{Check if the time period for extension curve is valid...}
            if (DiffInSecs(AExtensionCurve.StartDate, ADate)<=0)
            and (DiffInSecs(AExtensionCurve.EndDate, ADate)>=0) then
            begin
{First case, If h> first h of the extension Interpolate as usual}
              if (AValue+x0>=AExtensionCurve.First.Independent+
                x0_ext) then
              begin
                Result := AExtensionCurve.Interpolate(AValue);
                Exit;
              end else
{Last case, if h <first of the extension, keeping in mind that h>hmax,
 interpolate between hmax and hlast, in logarithmic means}
              begin
                x0mean := (AValue-ATransientCurve.Last.Independent)*
                          (x0_ext-x0)/
                          (AExtensionCurve.First.Independent-ATransientCurve.Last.Independent)+
                          x0;
                Result := Ln(ATransientCurve.Last.Dependent+y0) +
                         (Ln(AValue+x0mean)-Ln(ATransientCurve.Last.Independent+x0))*
                         (Ln(AExtensionCurve.First.Dependent+y0_ext)-Ln(ATransientCurve.Last.Dependent+y0))/
                         (Ln(AExtensionCurve.First.Independent+x0_ext)-Ln(ATransientCurve.Last.Independent+x0));
                Result := Exp(Result)-y0;
                Exit;
              end;
            end;
          end;
        end;
      end;
      Exit;
    end;
  end;
  raise ENoInterpolCurve.Create(rsNoInterpolCurve+DateToStr(ADate));
end;

function TTransientCurveList.Interpolate(ATsRecord: TTsRecord): Real;
begin
  Result := Interpolate(ATsRecord.AsFloat, ATsRecord.Date);
end;

resourcestring
  rsFileIsMixedUp = 'Records in file are incorrectly ordered.';
  rsPropertyNotFound = 'Property not found.';
  rsShouldHaveOneCharacter = 'should have exactly one character.';

procedure TTransientCurveList.LoadFromFile(FileName: string);

  function GetValue(AStringList: TStringList; Name: string): string;
  var i: Integer;
  begin
    Result := AStringList.Values[LowerCase(Name)];
    if Result = '' then
    begin
      { Is it really empty, or is it just absent? }
      if AStringList.IndexOfName(LowerCase(Name)) = -1 then
        raise Exception.Create(Name + ': ' + rsPropertyNotFound);
    end;
    if Result='' then Exit;
    i := Length(Result);
    if (Result[1]='"') and (Result[i]='"') then
      Result := Copy(Result, 2, i-2);
  end;

  function StrToChar(AString: string): Char;
  begin
    if Length(AString)<>1 then
      raise EConvertError.Create(AString+' '+rsShouldHaveOneCharacter);
    Result := AString[1];
  end;

var
  F: TextFile;
  s: string;
  LineNo, i, j, ACount, PointsCount: Integer;
  Preamble: TStringList;
  SavedDecimalSeparator, Delimiter: Char;
  DateFormat: string;
  ATransientCurve: TTransientCurve;
  StartDate: TDateTime;
  EndDate: TDateTime;
  SavedFileMode, StartMonth, EndMonth: Integer;
  Logarithmic, Extension: Boolean;
  Offset: Real;
  ACurvePoint: TCurvePoint;
  isFirstLine: Boolean;

begin
  SavedFileMode := FileMode;
  AssignFile(F, FileName);
  LineNo := 0;
  Preamble := nil;
  SavedDecimalSeparator := SysUtils.FormatSettings.DecimalSeparator;
  try try
    FileMode := fmOpenRead or fmShareDenyWrite;
    Preamble := TStringList.Create;
    Reset(F);
    Clear;
    while True do
    begin
      ReadLn(F, s);
      Inc(LineNo);
      if s='' then Break;
      Preamble.Add(s);
    end;
    Delimiter := StrToChar(GetValue(Preamble, 'Delimiter'));
    SysUtils.FormatSettings.DecimalSeparator :=
      StrToChar(GetValue(Preamble, 'DecimalSeparator'));
    DateFormat := GetValue(Preamble, 'DateFormat');
    ACount := StrToInt(GetValue(Preamble, 'Count'));

    for i := 0 to ACount-1 do
    begin
      isFirstLine := True;
      Preamble.Clear;
      while True do
      begin
        ReadLn(F, s);
        Inc(LineNo);
        if s='' then
        begin
          if isFirstLine then
          begin
            isFirstLine := False;
            Continue;
          end else
          Break;
        end;
        Preamble.Add(s);
        isFirstLine := False;
      end;

      StartDate := FormatStrToDateTime(DateFormat,
        GetValue(Preamble, 'StartDate'));
      EndDate := FormatStrToDateTime(DateFormat,
        GetValue(Preamble, 'EndDate'));
{Using exception handling to keep compatibility with previous
 versions of files.. Stefanos, 2009-07-20}
      try
        StartMonth := StrToInt(GetValue(Preamble, 'StartMonth'));
      except
        StartMonth := 0;
      end;
      try
        EndMonth := StrToInt(GetValue(Preamble, 'EndMonth'));
      except
        EndMonth := 0;
      end;
      Extension := StrToBool(GetValue(Preamble, 'Extension'));
      Logarithmic := StrToBool(GetValue(Preamble, 'Logarithmic'));
      PointsCount := StrToInt(GetValue(Preamble, 'PointsCount'));
      ATransientCurve := TTransientCurve.Create(Logarithmic);
      ATransientCurve.StartDate := StartDate;
      ATransientCurve.EndDate := EndDate;
      ATransientCurve.StartMonth := StartMonth;
      ATransientCurve.EndMonth := EndMonth;
      ATransientCurve.Extension := Extension;
      if Logarithmic then
      begin
        Offset := StrToFloat(GetValue(Preamble, 'Offset'));
        ACurvePoint.Independent := Offset;
        ACurvePoint.Dependent := 0.000;
        ATransientCurve.Offset := ACurvePoint;
      end;

      for j := 0 to PointsCount-1 do
      begin
        Readln(F, s);
        ACurvePoint.Independent := StrToFloat(DelimitedStringItem(s, 1, Delimiter));
        ACurvePoint.Dependent := StrToFloat(DelimitedStringItem(s, 2, Delimiter));
        ATransientCurve.Add(ACurvePoint);
        Inc(LineNo);
        if (j>0)
         and (ATransientCurve.Points[j].Independent<
           ATransientCurve.Points[j-1].Independent) then
          raise Exception.Create(rsFileIsMixedUp);
      end;

      Add(ATransientCurve);

    end;
  finally
    SysUtils.FormatSettings.DecimalSeparator := SavedDecimalSeparator;
    Preamble.Free;
    CloseFile(F);
    FileMode := SavedFileMode;
  end
  except
    on E:Exception do
    begin
      E.Message := Filename+'('+IntToStr(LineNo)+'): '+E.Message;
      Clear;
      raise;
    end;
  end;
end;


procedure TTransientCurveList.WriteToFile(FileName, DateFormat: string; Delimiter,
   DecimalSeparator: Char);
var
  F: TextFile;
  i, j: Integer;
  SavedDecimalSeparator: Char;
begin
  SavedDecimalSeparator := SysUtils.FormatSettings.DecimalSeparator;
  AssignFile(F, FileName);
  try
    Rewrite(F);
    SysUtils.FormatSettings.DecimalSeparator := DecimalSeparator;
    WriteLn(F, 'Delimiter="', Delimiter, '"');
    WriteLn(F, 'DecimalSeparator="', DecimalSeparator, '"');
    WriteLn(F, 'DateFormat="', DateFormat, '"');
    WriteLn(F, 'Count=', IntToStr(Count));
    WriteLn(F, '');
    for i := 0 to Count-1 do
    begin
      WriteLn(F, 'StartDate=',FormatDateTime(DateFormat, TTransientCurve(Items[i]).StartDate));
      WriteLn(F, 'EndDate=',FormatDateTime(DateFormat, TTransientCurve(Items[i]).EndDate));
      WriteLn(F, 'StartMonth=',TTransientCurve(Items[i]).StartMonth);
      WriteLn(F, 'EndMonth=',TTransientCurve(Items[i]).EndMonth);
      WriteLn(F, 'Extension=',BoolToStr(TTransientCurve(Items[i]).Extension));
      WriteLn(F, 'Logarithmic=',BoolToStr(TTransientCurve(Items[i]).Log));
      if TTransientCurve(Items[i]).Log then
        WriteLn(F, 'Offset=',TTransientCurve(Items[i]).Offset.Independent);
      WriteLn(F, 'PointsCount=',TTransientCurve(Items[i]).Count);
      WriteLn(F,'');
      for j := 0 to TTransientCurve(Items[i]).Count-1 do
      begin
        WriteLn(F, FloatToStr(TTransientCurve(Items[i]).Points[j].Independent),
          Delimiter,FloatToStr(TTransientCurve(Items[i]).Points[j].Dependent));
      end;
      if i<Count-1 then WriteLn(F,'');
    end;
  finally
    SysUtils.FormatSettings.DecimalSeparator := SavedDecimalSeparator;
    CloseFile(F);
  end;
end;

function TTransientCurveList.ReverseInterpolate(ATsRecord: TTsRecord): Real;
var
  i: Integer;
  ATransientCurve: TTransientCurve;
begin
  for i := 0 to Count-1 do
  begin
    ATransientCurve := TTransientCurve(Items[i]);
    if (DiffInSecs(ATransientCurve.StartDate, ATsRecord.Date)<=0)
    and (DiffInSecs(ATransientCurve.EndDate, ATsRecord.Date)>=0) then
    begin
      Result := ATransientCurve.ReverseInterpolate(ATsRecord.AsFloat);
      Exit;
    end;
  end;
  raise ENoInterpolCurve.Create(rsNoInterpolCurve+DateToStr(ATsRecord.Date));
end;

resourcestring
  rsStageAndDischargeMustBeSet =
    'Stage and discharge time series should be set in order to set '+
      'hydrometric points';

procedure TTransientCurveList.SetHydrometricPoints(StageTS,
  DischargeTS: TTimeseries);
var
  i: Integer;
  ACommonPeriod: TDateTimeList;
  ATSList: TObjectList;
begin
  if (StageTS=nil) or (DischargeTS=nil) then
    raise Exception.Create(rsStageAndDischargeMustBeSet);
  ACommonPeriod := nil;
  ATSList := nil;
  try
    ATSList := TObjectList.Create(False);
    ATSList.Add(StageTS);
    ATSList.Add(DischargeTS);
    ACommonPeriod := GetCommonPeriod(ATSList,0);
    ACommonPeriod.Sort;
    FHydrometricPointStartDate := ACommonPeriod.First;
    FHydrometricPointEndDate := ACommonPeriod.Last;
    SetLength(FHydrometricPointCollection, ACommonPeriod.Count);
    for i := 0 to ACommonPeriod.Count-1 do
    begin
      with FHydrometricPointCollection[i] do
      begin
        Date := ACommonPeriod[i];
        Stage := StageTS[StageTS.IndexOf(Date)].AsFloat;
        Discharge := DischargeTS[DischargeTS.IndexOf(Date)].AsFloat;        
        Outlier := False;
      end;
    end;
  finally
    ATSList.Free;
    ACommonPeriod.Free;
  end;
end;

resourcestring
  rsNoSufficientPointsForDet =
    'Insufficient points for determination coefficient calculation';

function TTransientCurveList.GetDeterminationCoefficient(AStartDate, AEndDate:
  TDateTime): Real;
var
  i, ACount: Integer;
  SumY, SumY2, SumW2: Real;
begin
  ACount := 0;
  SumY := 0;
  SumY2 := 0;
  SumW2 := 0;
  for i := 0 to HydrometricPointsCount-1 do
  begin
    with HydrometricPoints[i] do
    begin
      if DiffInSecs(Date, AStartDate) < -1 then Continue;
      if DiffInSecs(Date, AEndDate) > 1 then Continue;
      if not Outlier then
      begin
        Inc(ACount);
        SumY := SumY + Discharge;
        SumY2 := SumY2 + Sqr(Discharge);
        SumW2 := SumW2 + Sqr(Discharge - Interpolate(Stage, Date) );
      end;
    end;
  end;
  if ACount < 1 then
    raise Exception.Create(rsNoSufficientPointsForDet);
  Result := 1 - SumW2 / (SumY2 - Sqr(SumY)/ACount);  
end;

function TTransientCurveList.GetDeterminationCoefficient: Real;
begin
  Result := GetDeterminationCoefficient(First.StartDate, Last.EndDate);
end;

resourcestring
  rsCurveNumShouldBeBetweenZeroAndCount = 'Curve number should be between '+
    'zero (0) and Count-1';
  rsCannotGetForExtensionLine =
    'Cannot calculate determination coefficient for an extension line';

function TTransientCurveList.GetDeterminationCoefficient(ACurve: Integer): Real;
begin
  if (ACurve<0) or (ACurve>Count-1) then
    raise EListError.Create(rsCurveNumShouldBeBetweenZeroAndCount);
  if Items[ACurve].Extension then
    raise Exception.Create(rsCannotGetForExtensionLine);
  with Items[ACurve] do
    Result := GetDeterminationCoefficient(StartDate, EndDate);
end;

resourcestring
  rsTsStage = 'Stage';
  rsTsStageComments = 'Stage, outlier values considered as null';

function TTransientCurveList.ExtractStageTS: TTimeseries;
var
  i: Integer;
begin
  Result := nil;
  if HydrometricPointsCount<1 then
    Exit;
  try
    Result := TTimeseries.Create;
    Result.TimeStep := tstVariable;
    Result.VariableType := vtInstantaneous;
    Result.Title := rsTsStage;
    Result.Comment := rsTsStageComments;
    for i := 0 to HydrometricPointsCount-1 do
      with HydrometricPoints[i] do
        if Outlier then
          Result.Insert(Date,True,0,'',msNew)
        else
          Result.Insert(Date,False,Stage,'',msNew);
  except
    Result.Free;
    raise;
  end;
end;

resourcestring
  rsTsDischarge = 'Discharge';
  rsTsDischargeComments = 'Discharge, outlier values considered as null';

function TTransientCurveList.ExtractDischargeTS: TTimeseries;
var
  i: Integer;
begin
  Result := nil;
  if HydrometricPointsCount<1 then
    Exit;
  try
    Result := TTimeseries.Create;
    Result.TimeStep := tstVariable;
    Result.VariableType := vtInstantaneous;
    Result.Title := rsTsDischarge;
    Result.Comment := rsTsDischargeComments;
    for i := 0 to HydrometricPointsCount-1 do
      with HydrometricPoints[i] do
        if Outlier then
          Result.Insert(Date,True,0,'',msNew)
        else
          Result.Insert(Date,False,Discharge,'',msNew);
  except
    Result.Free;
    raise;
  end;
end;

{ }

function FitSinglePolyline(Logarithmic: Boolean; Offset: Real;
  StartDate, EndDate: TDateTime; StartMonth, EndMonth: Integer;
    HydrometricPoints: array of THydrometricPoint;
      NumOfHydrometricPoints: Integer; IndependentValues: TVector;
        DependentValues: TVector): Real;
var
  i, j, ip1: Integer;
  A: TMatrix;
  B: TVector;
  xi1,xi2,yi1,yi2,xp,yp: Real;
  SumW2, SumY, SumY2: Real;
  ACount: Integer;
  ADate: TDateTime;
  InMonth: Boolean;
  Year, Month, Day: Word;
begin
  Assert(DependentValues<>nil);
  A := nil;
  B := nil;
  try
    A := TMatrix.Create(IndependentValues.Size, IndependentValues.Size );
    B := TVector.Create(IndependentValues.Size );
    B.Assign(DependentValues);
    for i := 1 to IndependentValues.Size do
      for j := 1 to IndependentValues.Size do
         A[i,j] := 0;
    for i := 1 to IndependentValues.Size do
    begin
      if i < IndependentValues.Size then
        ip1 := i+1 else
        ip1 := i-1;
      B[i] := 0;
      if Logarithmic then
      begin
        xi1 := ln(IndependentValues[i]+Offset);
        xi2 := ln(IndependentValues[ip1]+Offset);
      end else
      begin
        xi1 := IndependentValues[i];
        xi2 := IndependentValues[ip1];
      end;
      for j := 0 to NumOfHydrometricPoints -1 do
      begin
        ADate := HydrometricPoints[j].Date;
        DecodeDate(ADate, Year, Month, Day);
        if HydrometricPoints[j].Outlier then Continue;
        if DiffInSecs(ADate,StartDate)<-1 then Continue;
        if DiffInSecs(ADate,EndDate)>1 then Continue;
        if EndMonth>StartMonth then
          InMonth := (Month<=EndMonth) and (Month>=StartMonth)
        else
          InMonth := (Month<=EndMonth) or (Month>=StartMonth);
        if not InMonth then Continue;
        if HydrometricPoints[j].Stage < Min(IndependentValues[i],
          IndependentValues[ip1]) then Continue;
        if HydrometricPoints[j].Stage >= Max(IndependentValues[i],
          IndependentValues[ip1]) then
            if ip1>i then Continue;
        if Logarithmic then
        begin
          xp := ln(HydrometricPoints[j].Stage+Offset);
          yp := ln(HydrometricPoints[j].Discharge);
        end else
        begin
          xp := HydrometricPoints[j].Stage;
          yp := HydrometricPoints[j].Discharge;
        end;
        B[i] := B[i] + yp * ( 1 - (xp-xi1)/(xi2-xi1) );
        A[i, i] := A[i, i] + Sqr((xp-xi1)/(xi2-xi1) - 1);
        A[i, ip1] := A[i, ip1] - (xp-xi1)/(xi2-xi1)*((xp-xi1)/(xi2-xi1) - 1);
      end;
    end;
    A.Invert;
    B.Multiply(A, maLeft);
    DependentValues.Assign(B);
    {De-Logarithm Calculated Dependent Values}
    If Logarithmic then
    begin
      for i := 1 to DependentValues.Size do
      begin
        DependentValues[i] := Exp(DependentValues[i]);
      end;
    end;
    {Calculate Determination Factor}
    SumW2 := 0;
    SumY := 0;
    SumY2 := 0;
    ACount := 0;
    for i := 1 to IndependentValues.Size-1 do
    begin
      ip1 := i+1;
      if Logarithmic then
      begin
        xi1 := ln(IndependentValues[i]+Offset);
        xi2 := ln(IndependentValues[ip1]+Offset);
        yi1 := ln(DependentValues[i]);
        yi2 := ln(DependentValues[ip1]);
      end else
      begin
        xi1 := IndependentValues[i];
        xi2 := IndependentValues[ip1];
        yi1 := DependentValues[i];
        yi2 := DependentValues[ip1];
      end;
      for j := 0 to NumOfHydrometricPoints -1 do
      begin
        ADate := HydrometricPoints[j].Date;
        DecodeDate(ADate, Year, Month, Day);
        if HydrometricPoints[j].Outlier then Continue;
        if DiffInSecs(ADate,StartDate)<-1 then Continue;
        if DiffInSecs(ADate,EndDate)>1 then Continue;
        if EndMonth>StartMonth then
          InMonth := (Month<=EndMonth) and (Month>=StartMonth)
        else
          InMonth := (Month<=EndMonth) or (Month>=StartMonth);
        if not InMonth then Continue;
        if HydrometricPoints[j].Stage < Min(IndependentValues[i],
          IndependentValues[ip1]) then Continue;
        if HydrometricPoints[j].Stage >= Max(IndependentValues[i],
          IndependentValues[ip1]) then
            if ip1>i then Continue;
        if Logarithmic then
        begin
          xp := ln(HydrometricPoints[j].Stage+Offset);
          yp := ln(HydrometricPoints[j].Discharge);
        end else
        begin
          xp := HydrometricPoints[j].Stage;
          yp := HydrometricPoints[j].Discharge;
        end;
        Inc(ACount);
        SumY := SumY + yp;
        SumY2 := SumY2 + Sqr(yp);
        SumW2 := SumW2 + Sqr(yp-yi1-(yi2-yi1)*(xp-xi1)/(xi2-xi1));
      end;
    end;
    Result := 1 - SumW2 / (SumY2 - Sqr(SumY)/ACount);
  finally
    A.Free;
    B.Free;
  end;
end;

resourcestring
  rsInsufficientHydrometricPoints =
    'Insufficient number of hydrometric points to fit so much segments';
  rsCannotForExtensionLine = 'Cannot fit polyline for extension line';
  rsSegmensGreaterThanZero = 'Segments should be at least one';
  rsNegativeStagesDetected =
    'Negative stage values, considering offset value. Stage+offset should '+
      'be greater than zero';

function TTransientCurveList.FitPolyline(ACurve: Integer;
  Segments: Integer): Real;
var
  i, j, k, ACount: Integer;
  Indexes: array of Integer;
  AFlag: Boolean;
  DependentValues, IndependentValues: TVector;
  OptDependentValues, OptIndependentValues: TVector;
  OptCoefficient, DetCoefficient: Real;
  HydPointsStage: TFloatList;
  FromMonth, ToMonth: Integer;
  InMonth: Boolean;
  ADate: TDateTime;
  Year, Month, Day: Word;
begin
  if (ACurve<0) or (ACurve>Count-1) then
    raise EListError.Create(rsCurveNumShouldBeBetweenZeroAndCount);
  Assert(Segments>0, rsSegmensGreaterThanZero);
  Assert(not Items[ACurve].Extension, rsCannotForExtensionLine);
  DependentValues := nil;
  IndependentValues := nil;
  OptDependentValues := nil;
  OptIndependentValues := nil;
  HydPointsStage := nil;
  Result := 0;
  try
    HydPointsStage := TFloatList.Create;
    HydPointsStage.Clear;
    ACount := 0;
    for i := 0 to Length(FHydrometricPointCollection)-1 do
    begin
      ADate := HydrometricPoints[i].Date;
      FromMonth := Items[ACurve].StartMonth;
      ToMonth := Items[ACurve].EndMonth;
      DecodeDate(ADate, Year, Month, Day);
      If HydrometricPoints[i].Outlier then
        Continue;
      if DiffInSecs(ADate, Items[ACurve].StartDate) < -1 then
        Continue;
      if DiffInSecs(ADate, Items[ACurve].EndDate) > 1 then
        Continue;
      if ToMonth>FromMonth then
        InMonth := (Month<=ToMonth) and (Month>=FromMonth)
      else
        InMonth := (Month<=ToMonth) or (Month>=FromMonth);
      if not InMonth then Continue;
      Inc(ACount);
      if HydrometricPoints[i].Stage + Items[ACurve].Offset.Independent <= 0 then
        raise Exception.Create(rsNegativeStagesDetected);
      HydPointsStage.Add(HydrometricPoints[i].Stage);
    end;
    HydPointsStage.Sort;
    if ACount<=Segments then
      raise Exception.Create(rsInsufficientHydrometricPoints);
    SetLength(Indexes, Segments+1);
    DependentValues := TVector.Create(Segments+1);
    IndependentValues := TVector.Create(Segments+1);
    OptDependentValues := TVector.Create(Segments+1);
    OptIndependentValues := TVector.Create(Segments+1);
    Indexes[0] := 0;
    Indexes[Segments] := ACount-1;
    for i := 1 to Segments-1 do
      Indexes[i] := i;
    OptCoefficient := 0;
    repeat
      for i := Segments-1 downto 1 do
      begin
        if i = Segments-1 then
          Indexes[i] := Indexes[i] + 1;
        if Indexes[i+1] = ACount + i - Segments + 1 then
        begin
          Indexes[i] := Indexes[i] + 1;
          for j := i+1 to Segments-1 do
            Indexes[j] := Indexes[j-1]+1;
        end;
      end;
      for k := 0 to Segments do
        IndependentValues[k+1] := HydPointsStage.Items[Indexes[k]];
      AFlag := True;
{ Check for indentical independent values, in that case do not proceed with
  calculations to avoid numerical errors to matrices inversion.}
      for k := 1 to Segments-1 do
        if IndependentValues[k+1]-IndependentValues[k]<
          0.005*(IndependentValues[Segments+1]-IndependentValues[1]) then
            AFlag := False;
      if not AFlag then Continue;
      with Items[ACurve] do
        DetCoefficient := FitSinglePolyline(True, Offset.Independent, StartDate,
          EndDate, StartMonth, EndMonth, FHydrometricPointCollection,
          HydrometricPointsCount, IndependentValues, DependentValues);
{ Check if Dependent and independent values are increasing with stage}
      for k := 1 to Segments do
        if DependentValues[k+1]<DependentValues[k] then
          DetCoefficient := 0.0;
      if DetCoefficient > OptCoefficient then
      begin
        OptCoefficient := DetCoefficient;
        for k := 1 to Segments +1 do
        begin
          OptDependentValues[k] := DependentValues[k];
          OptIndependentValues[k] := IndependentValues[k];
        end;
      end;
    until Indexes[1] = ACount - Segments;
    if OptCoefficient > 0 then
    begin
      Items[ACurve].Clear;
      for i := 1 to Segments+1 do
        Items[ACurve].Add(OptIndependentValues[i], OptDependentValues[i]);
      Result := OptCoefficient;
    end;
  finally
    SetLength(Indexes, 0);
    DependentValues.Free;
    IndependentValues.Free;
    OptDependentValues.Free;
    OptIndependentValues.Free;
    HydPointsStage.Free;
  end;
end;

{TLeakageInterpolation}

function TLeakageInterpolation.Interpolate(Month: Integer; Stage: Real): Real;
begin
  result := FACoef[Month]*Power(Stage,3)+FBCoef[Month]*Power(Stage,2)+
              FCCoef[Month]*Stage+FECoef[Month];
end;

procedure TLeakageInterpolation.InterpolateTS(Source, Destination: TTimeseries);
var
  i,AMonth: Integer;
  Value: Real;
begin
  Destination.Clear;
  for i := 0 to Source.Count-1 do
    with Source.Items[i] do
    begin
      AMonth := StrToInt(FormatDateTime('m',Date));
      if not IsNull then Value := Interpolate(AMonth,AsFloat) else Value := 0;
      Destination.Add(Date, IsNull, Value, '', msNew);
    end;
end;

{ InterpolateDiffs }

{ Algorithm description.
  sm denotes a measurement from the ReferenceSeries, sg one from the
  CorrectableSeries.  sm's that coincide in date (with a tolerance of 1 hour)
  with some sg are considered, the rest are ignored. The sm's considered will
  be termed "coinciding sm's".

  Algorithm:
  Beginning:
  1) Find the first coinciding sm, and its coincidence, sg, named sm1 and sg1.
  2) Set all deltas preceding and including sg1 to sm1-sg1.
  Middle:
  3) Find next coinciding sm, and its coincidence sg, namely sm2 and sg2.
  4) Set all deltas between sg1 and sg2 to
       (sm1-sg1)*(tm2-t)/(tm2-tm1) + (sm2-sg2)*(t-tm1)/(tm2-tm1)
     where tm1, tm2 the times of sm1, sm2, and t the time of each delta.
  5) Set im1=im2 and ig1=ig2 and go to (3) until no more coinciding sm's.
  End (sm1 and sg1 are already the last coinciding sm and its coincidence):
  6) Set all deltas from sg1 to end of sg to sm1-sg1.

  We use variables im1, im2 for the indices into ReferenceSeries corresponding
  to sm1 and sm2; ig1, ig2 for the indices into CorrectableSeries
  corresponding to sg1 and sg2; tm1, tm2, tg1, tg2 for the respective times.
}

procedure InterpolateDiffs(ReferenceSeries, CorrectableSeries, ResultingDelta:
  TTimeseries);
var
  tm1, tg1, tm2, tg2: TDateTime;
  im1, im2, ig1, ig2: Integer;
  t, dt: TDateTime;
  i: Integer;
  a, b: Real;
begin
  { Initialize corrections to zero. }
  ResultingDelta.Clear;
  if CorrectableSeries.Count = 0 then Exit;
  for i := 0 to CorrectableSeries.Count-1 do
    ResultingDelta.Add(CorrectableSeries[i].Date, False, 0, '', msNew);

  { Beginning }
  tm1 := 0; ig1 := 0;
  im1 := 0;
  while im1 < ReferenceSeries.Count do
  begin
    tm1 := ReferenceSeries[im1].Date;
    ig1 := CorrectableSeries.NearestTo(tm1);
    tg1 := CorrectableSeries[ig1].Date;
    if Abs(DiffInSecs(tm1, tg1))<=1800 then Break;
    Inc(im1);
  end;
  if im1=ReferenceSeries.Count then Exit;
  a := ReferenceSeries[im1].AsFloat - CorrectableSeries[ig1].AsFloat;
  for i := 0 to ig1 do
    ResultingDelta[i].AsFloat := a;

  { Middle }
  im2 := im1 + 1;
  while im2<ReferenceSeries.Count do
  begin
    tm2 := ReferenceSeries[im2].Date;
    ig2 := CorrectableSeries.NearestTo(tm2);
    tg2 := CorrectableSeries[ig2].Date;
    if Abs(DiffInSecs(tm2, tg2))>1800 then
    begin
      Inc(im2);
      Continue;
    end;
    a := ReferenceSeries[im1].AsFloat - CorrectableSeries[ig1].AsFloat;
    b := ReferenceSeries[im2].AsFloat - CorrectableSeries[ig2].AsFloat;
    dt := SubtractDateTime(tm2, tm1);
    for i := ig1 to ig2 do
    begin
      t := ResultingDelta[i].Date;
      ResultingDelta[i].AsFloat :=
              a*SubtractDateTime(tm2, t)/dt + b*SubtractDateTime(t, tm1)/dt;
    end;
    im1 := im2; tm1 := tm2; ig1 := ig2;
    { We don't need tg1 := tg2; this is not used further }
    Inc(im2);
  end;

  { End }
  a := ReferenceSeries[im1].AsFloat - CorrectableSeries[ig1].AsFloat;
  for i := ig1 to CorrectableSeries.Count-1 do
    ResultingDelta[i].AsFloat := a;
end;

procedure TsInterpolate(Source, Dest: TTimeseries; Curve: TInterpolatingCurve);
var
  i: Integer;
  Value: Real;
begin
  Dest.Clear;
  for i := 0 to Source.Count-1 do
    with Source.Items[i] do
    begin
      if not IsNull then Value := Curve.Interpolate(AsFloat) else Value := 0;
      Dest.Add(Date, IsNull, Value, '', msNew);
    end;
end;

procedure TsInterpolate(Source, Dest: TTimeseries;
  Curves: TTransientCurveList);
var
  i: Integer;
  Value: Real;
begin
  Dest.Clear;
  for i := 0 to Source.Count-1 do
    with Source.Items[i] do
    begin
      if not IsNull then Value := Curves.Interpolate(Source.Items[i]) else Value := 0;
      Dest.Add(Date, IsNull, Value, '', msNew);
    end;
end;

{ StageCorrect }

{ The same algorithm as for InterpolateDiffs is used, with a few differences:
  1) It's done separately for each period (associated with a curve).
  2) There are no coincidences required here. Correction is done anyway.
}

procedure StageCorrectForCurve(StageSeries, SDStage, SDDischarge: TTimeseries;
  Curve: TTransientCurve; ResultingDelta: TTimeseries;
  isl, ish, iml, imh: Integer); forward;
  { isl, ish, iml, imh are the low and high limits of the indices into
    StageSeries (isl, ish) and SDStage & SDDischarge (iml, imh) to be
    considered. }

procedure StageCorrect(StageSeries, SDStage, SDDischarge: TTimeseries;
  Curves: TTransientCurveList; ResultingDelta: TTimeseries);
var
  i, isl, ish, iml, imh, CurrentCurve: Integer;
  tb, te: TDateTime;
begin
  { Initialize corrections to zero. }
  ResultingDelta.Clear;
  if SDStage.Count=0 then Exit;
  for i := 0 to StageSeries.Count-1 do
    ResultingDelta.Add(StageSeries[i].Date, False, 0, '', msNew);

  for CurrentCurve := 0 to Curves.Count-1 do
  begin
    if Curves[CurrentCurve].Extension then Continue;
    tb := Curves[CurrentCurve].StartDate;
    te := Curves[CurrentCurve].EndDate;
    isl := StageSeries.NearestTo(tb);
    ish := StageSeries.NearestTo(te);
    iml := SDStage.NearestTo(tb);
    imh := SDStage.NearestTo(te);
    if DiffInSecs(StageSeries[isl].Date, tb) < 0 then Inc(isl);
    if DiffInSecs(StageSeries[ish].Date, te) > 0 then Dec(ish);
    if DiffInSecs(SDStage[iml].Date, tb) < 0 then Inc(iml);
    if DiffInSecs(SDStage[imh].Date, te) > 0 then Dec(imh);
    if (isl>ish) or (iml>imh) then Continue;
    StageCorrectForCurve(StageSeries, SDStage, SDDischarge,
      Curves[CurrentCurve], ResultingDelta,
      isl, ish, iml, imh);
  end;
end;

procedure StageCorrectForCurve(StageSeries, SDStage, SDDischarge: TTimeseries;
  Curve: TTransientCurve; ResultingDelta: TTimeseries;
  isL, isH, imL, imH: Integer);
var
  tm1, tm2, ts1, ts2: TDateTime;
  im1, im2, is1, is2: Integer;
  t, dt: TDateTime;
  i: Integer;
  a, b: Double;
begin
  { Beginning }
  im1 := imL; tm1 := SDStage[im1].Date;
  is1 := StageSeries.NearestTo(tm1); ts1 := StageSeries[is1].Date;
  if DiffInSecs(tm1, ts1)<3600 then Dec(is1);
  if (is1>=isL) and (is1<=isH) then
  begin
    a := Curve.ReverseInterpolate(SDDischarge[im1].AsFloat)
         - SDStage[im1].AsFloat;
    for i := isL to is1 do
    begin
      ResultingDelta[i].AsFloat := a;
    end;
  end;
  Inc(is1);
  if is1>isH then Exit;

  { Middle }
  im2 := im1 + 1;
  while im2<=imH do
  begin
    tm2 := SDStage.Items[im2].Date;
    is2 := StageSeries.NearestTo(tm2); ts2 := StageSeries[is2].Date;
    if DiffInSecs(tm2, ts2)<3600 then Dec(is2);
    { ts2 is not used beyond this point, thus not updated }
    a := Curve.ReverseInterpolate(SDDischarge[im1].AsFloat)
         - SDStage[im1].AsFloat;
    b := Curve.ReverseInterpolate(SDDischarge[im2].AsFloat)
         - SDStage[im2].AsFloat;
    dt := SubtractDateTime(tm2, tm1);
    for i := is1 to is2 do
    begin
      t := ResultingDelta[i].Date;
      ResultingDelta[i].AsFloat :=
              a*SubtractDateTime(tm2, t)/dt + b*SubtractDateTime(t, tm1)/dt;
    end;
    im1 := im2; tm1 := tm2; is1 := is2+1; { ts1 is not used further }
    Inc(im2);
  end;

  { End }
  a := Curve.ReverseInterpolate(SDDischarge[im1].AsFloat)
       - SDStage[im1].AsFloat;
  for i := is1 to isH do ResultingDelta[i].AsFloat := a;
end;

resourcestring
  rsTSDiffCount =
    'Time series of different count';

procedure DecreaseStageCorrect(Curves: TTransientCurveList; StageSeries, ResultingDelta: TTimeseries);
var
  i,j,k: Integer;
  value,x0,dh,x0_ext: Real;
  Date: TDateTime;
  ATransientCurve, AExtensionCurve: TTransientCurve;
begin
  if ResultingDelta.Count<>StageSeries.Count then raise Exception.Create(rsTSDiffCount);
  for i := 0 to ResultingDelta.Count-1 do
  begin
    Date := ResultingDelta.Items[i].Date;
    dh := ResultingDelta.Items[i].AsFloat;
    value := StageSeries.Items[i].AsFloat;

    for k :=0 to Curves.Count-1 do
    begin
      ATransientCurve := Curves[k];
      if (DiffInSecs(ATransientCurve.StartDate, Date)<=0)
          and (DiffInSecs(ATransientCurve.EndDate, Date)>=0)
          and (not ATransientCurve.Extension) then
      begin
        if value > ATransientCurve.Last.Independent then
        begin
          for j := 0 to Curves.Count-1 do
          begin
            AExtensionCurve := Curves[j];
            if AExtensionCurve.Extension then
            begin
              if ATransientCurve.Log then
              begin
                x0 := ATransientCurve.Offset.Independent;
              end else begin
                x0 := 0;
              end;
              if AExtensionCurve.Log then
              begin
                x0_ext := AExtensionCurve.Offset.Independent;
              end else begin
                x0_ext := 0;
              end;
              if (DiffInSecs(AExtensionCurve.StartDate, Date)<=0)
              and (DiffInSecs(AExtensionCurve.EndDate, Date)>=0) then
              begin
                if (value+x0>=AExtensionCurve.First.Independent+x0_ext) then
                begin
                  ResultingDelta.Items[i].AsFloat := 0;
                end else
                begin
                   if AExtensionCurve.First.Independent+x0_ext>
                   ATransientCurve.Last.Independent+x0 then
                   begin
                     ResultingDelta.Items[i].AsFloat := dh*
                     (value-ATransientCurve.Last.Independent)/
                     (AExtensionCurve.First.Independent-ATransientCurve.Last.Independent+x0_ext-x0)
                   end;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

end.
