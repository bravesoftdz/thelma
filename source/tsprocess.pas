{******************************************************************}
{                                                                  }
{  Thelma library                                                  }
{                                                                  }
{  Copyright (c) 2000-04 National Technical University of Athens   }
{                                                                  }
{******************************************************************}

{** Procedures for essential time series processing. }
unit tsprocess;

interface

uses ts, Dates, Matrix, Classes, Contnrs;

{** Performs range check on a time series.
    RangeCheck searches the entire time series in order to find values which
    are outside the specified limits. For those values, it sets the specified
    Flag; for the values within limits, it resets the Flag.<p>
    So the flag is set to RangeBit when exceed the range or to RangeBit-1
    when within the range<p>
    Set LowLimit or HighLimit to true in order to mark out of range
    in respect to the sample statistical properties and by
    considering the Normal distribution.<p>
    The TRangeCheckDialog can be used as an interface with the user.<p>
    @author A.X.
    @SeeAlso <See Class=TRangeCheckDialog>
    @SeeAlso <See Routine=TimeConsistencyCheck>
}
procedure RangeCheck(Timeseries: TTimeseries; var LowLimit, HighLimit: Real;
  const Flag: string; AutoLow, AutoHigh: Boolean; ProbabilityLevel: Real);

{** Performs a time consistency check.
    TimeConsistency checks if [i-1, i](i=0...count-1) difference are
    within the Triger value. If the difference exceeds the Triger
    value<p>
    @author Stefanos
    @SeeAlso <See Routine=RangeCheck>
}
procedure TimeConsistencyCheck(Timeseries: TTimeseries; Triger: Real;
  const Flag: string);

{** Performs time series addition.
    AddTimeseries adds the time series specified by Source and Dest and puts the
    result in Dest. Specifically, to those records of Dest that have
    coinciding dates with not null values in Source, the corresponding records
    of Source are added.  The rest of the records are unchanged.
}
procedure AddTimeseries(Source, Dest: TTimeseries);

type
  TRegularizeStepMethod = (rsmInstantaneous, rsmVector, rsmCumulativeCStep,
    rsmCumulativeVStep, rsmChangeTime);

{** Produces a time series with strict step from one with not strict step.
    The purpose of RegularizeStep is to smooth out the time
    irregularities that may exist in a time series of essentially constant time
    step. RegularizeStep processes the time series specified by Source and
    produces a new time series of strict constant step, which can be ten-minute,
    hourly, or daily. The new time series is put in Dest. Source.TimeStep
    specifies the constant step. The time step of Dest must be the same, except
    if Dest does not have any data, in which case it is ignored and set to
    Source.TimeStep. Dest.TimeStepStrict is also set to True.<p>

    All timestamps in the resulting time series are multiples of the time step
    with an offset of TimeOffset. For example, if Source.TimeStep is
    tstTenMinute and TimeOffset is 2, all timestamps in the resulting time
    series end in 2, i.e.  :02, :12, :22, :32, :42, :52. TimeOffset is given in
    minutes.  The resulting time series begins with the timestamp A which is
    nearest to the timestamp of the first record of Source, and ends at the
    timestamp B which is nearest to the last record of Source.  Between A and B,
    the resulting time series contains records with all possible timestamps,
    although some may be null. Any already existing records in Dest with
    timestamps between A and B are deleted. For the remainder of this text,
    "resulting time series" means the part of Dest between A and B, not the
    entire Dest.<p>

    The algorithm differs depending on Method, which can be one of
    rsmInstantaneous, rsmVector, rsmCumulativeVStep, rsmCumulativeCStep, and
    rsmChangeTime. The first four are respectively suitable for normal
    instantaneous, vector instantaneous, cumulative variables with variable
    step, and cumulative variables with constant step. rsmChangeTime merely
    changes the time of the record without altering its value, so it's suitable
    for any type of variable.<p>

    For rsmInstantaneous, the value and flags for each record with timestamp t
    are determined as follows:
    <ul>
      <li>If a record exists in Source and has timestamp t, that record's value
      and flags are used.
      <li>Otherwise, if two successive not null records exist in Source such
      that t is between their timestamps, and the time difference between the
      resulting record and the two source records is no more than 0.50*TimeStep
      for the one and 1.50*TimeStep for the other, then the value is found by
      interpolating between the two records, and the flags of the source record
      closest to the resulting record are used.
      <li>Likewise for the first record of the result, but with extrapolation,
      provided that the t is less than the first Source record, but by no more
      than 0.50 times the time step, and no more than 1.50 times the time step
      away from the second record. Likewise for the last record.
      <li>Otherwise, the value is null and no flags are set.
    </ul>
    <p>

    For rsmVector, each value must be a direction in degrees, between 0 and 360;
    the measurement is considered instantaneous. The same algorithm as for
    rsmInstantaneous applies, except that for interpolating and extrapolating
    the values are analyzed into their x and y constituents;
    interpolation/extrapolation is performed on the constituents, and the
    direction of the resulting vector is considered to be the result of the
    interpolation/extrapolation.<p>

    For rsmCumulativeCStep, it is assumed that each record in Source contains a
    measurement which lasted for the specified time step, regardless of the time
    elapsed from the previous measurement. The value and flags for a record with
    timestamp t are determined as follows:
    <ul>
      <li>If a record exists in Source and has the same timestamp, that record's
      value and flags are used.
      <li>Otherwise, if not null records exist in Source such that their
      timestamps is between t-TimeStep and t+TimeStep (non-inclusive),
      then the corresponding portion of each of these records is used, and all
      such portions are added, to form the value. The flags are the union of
      the flags of the source records.
      <li>Otherwise, the value is null and no flags are set.
    </ul>
    <p>

    For rsmCumulativeVStep, it is assumed that each record in Source contains a
    measurement which began on the timestamp of the previous record; for the
    first record, the time step is assumed. The value and flags for a record
    with timestamp t are determined as follows:
    <ul>
      <li>If a record exists in Source with timestamp t, and either is the first
      record of Source or is preceded by a record with timestamp
      t-TimeStep, that record's value and flags is used.
      <li>Otherwise, if all existing records in Source with timestamps between
      t-TimeStep and the first timestamp greater than or equal to t are not
      null, and the one closest to t is no more than 0.50 times the time step,
      and for all of them the preceding record is no more than 1.50 times the
      time step behind, then the corresponding portion of each of these records
      is used, and all such portions are added, to form the value. The flags are
      those of the record with the nearest timestamp.
      <li>Otherwise, the value is null and no flags are set.
    </ul>
    <p>

    For rsmChangeTime, each source record is treated as follows:
    <ul>
      <li>If its timestamp t is valid as the destination timestamp (that is, it
      has the correct offset), it is left unaltered.
      <li>Otherwise, t1&lt;t&lt;t2, where t1 and t2 are the valid destination
      timestamps closest to t. If t is closer to t2 than to t1, the timestamp
      in the destination record is set to t2.
      <li>Otherwise, if t is closer to t1 than to t2, the timestamp in the
      destination record is set to t1.
      <li>Otherwise, if t differs from t1 and t2 by an equal amount, the
      timestamp in the destination record is set to t1, unless that timestamp
      is already occupied in the destination timeseries, in which case it is
      set to t2.
    </ul>
    If the above algorithm results in a timestamp that is already occupied in
    the destination time series (obviously by the previously processed source
    record), then the source record is ignored; a warning is logged in Warnings
    in that case.
    <p>

    Whenever the algorithm results in creating a record in Dest whose date does
    not have an exact match in Source, the flag specified by NewDateFlag is
    raised in the destination record, unless NewDateFlag is the empty string.<p>

    Any important warnings during the procedure are logged in Warnings (which
    is overwritten), which should be showed to the user. At the time of this
    writing the only warning is that of ignoring a source record in the
    rsmChangeTime case (see the algorithm above).<p>

    The TRegularizeStepDialog may be used as user interface.
  @author A.X.
  @SeeAlso <See Class=TRegularizeStepDialog>
  @SeeAlso <See Property=TTimeseries.TimeStep>
  @SeeAlso <See Property=TTimeseries.DateOffset>
  @SeeAlso <See Property=TTimeseries.TimeStepStrict>
}
procedure RegularizeStep(Source, Dest: TTimeseries; TimeOffset: Integer;
  StepMinutes: Integer; const NewDateFlag: string;
  Method: TRegularizeStepMethod; var Warnings: string);

{** Calculates covariance of two time series.
    Use TimeseriesCovariance to calculate the covariance between Timeseries1 and
    Timeseries2. Only the time stamps that are common in both time series are
    considered. The number of those time stamps is returned in
    CommonPeriodCardinality.
    @author A.X.
}
function TimeseriesCovariance(Timeseries1, Timeseries2: TTimeseries;
  var CommonPeriodCardinality: Integer): Real; overload;

{** (Overloaded function - this line does not appear in help).
    This version of TimeseriesCovariance calculates the covariance for the
    specified dates only. Both time series must have not null values for all
    dates in ADateTimeList, or an exception is raised.
    @author A.X.
}
function TimeseriesCovariance(Timeseries1, Timeseries2: TTimeseries;
  ADateTimeList: TDateTimeList): Real; overload;

{** Calculates sum of products of two time series.
    Use TimeseriesSumOfProducts to calculate the sum of products of Timeseries1
    and Timeseries2. Only the time stamps that are common in both time series
    are considered. The number of those time stamps is returned in
    CommonPeriodCardinality.
    @author A.X.
}
function TimeseriesSumOfProducts(Timeseries1, Timeseries2: TTimeseries;
  var CommonPeriodCardinality: Integer): Real; overload;

{** (Overloaded function - this line does not appear in help).
    This version of TimeseriesSumOfProducts calculates the sum of products for
    the specified dates only. Both time series must have not null values for all
    dates in ADateTimeList, or an exception is raised.
    @author A.X.
}
function TimeseriesSumOfProducts(Timeseries1, Timeseries2: TTimeseries;
  ADateTimeList: TDateTimeList): Real; overload;

type
  TAggregateMethod = (agmDefault, agmAverage, agmSum, agmMaximum, agmMinimum,
    agmVector, agmInstant);
  TAggregateOption = (agoHydrologicalYear, agoSeasonal);
  TAggregateOptions = set of TAggregateOption;

{** Aggregates a time series, producing a time series of a greater time step.
    Aggregate processes the time series specified by Source and produces a new
    time series of a greater time step. The new time series is put in Dest. Any
    already existing records in Dest with time stamps between the first and the
    last aggregated records are deleted.  Dest.TimeStep is ignored and
    overwritten, and so is Dest.TimeStepStrict, which is set to True; the time
    step of the destination time series is determined automatically as follows:
    <ul>
      <li>Ten-minute time series are aggregated to hourly time series.
      <li>Hourly time series are aggregated to daily time series.
      <li>Daily time series are aggregated to monthly time series.
      <li>Monthly time series are aggregated to yearly time series.
    </ul><p>

    TimeOffset, which is given in minutes, is normally used when producing daily
    time series which must be aggregated from 08:00 to 08:00; in that case,
    TimeOffset must be 480. It can also be used when producing hourly, daily,
    or monthly time series. When producing hourly or daily time series,
    TimeOffset must be zero or positive; for monthly, it can be zero, positive
    or negative.<p>

    If agoSeasonal is included in options, aggregations is done for a season
    starting from FromMonth and ending to ToMonth. If ToMonth < FromMonth the
    aggregation period last next year of each FromMonth year.<p>

    The source time series must be of constant, strict step. Except when
    producing annual time series, its DateOffset  must be in accordance to
    TimeOffset; for example, if daily time series are produced and TimeOffset is
    a multiple of 60, then Source.DateOffset must be zero. This does not
    matter when producing annual time series.<p>

    Each record produced in the destination time series is the sum, average,
    maximum, minimum, or vector average of a number of records in the source
    time series. (The sum, average, maximum, minimum, or vector average will be
    called "aggregation".)
    <ul>
      <li>For hourly time series, the produced record is the
      aggregation of the source record whose time stamp equals that of
      the destination record, and of the five records preceeding it.
      <li>For daily time series, the produced record is the
      aggregation of the source record whose time step equals that of
      the destination record, and of the 23 records preceeding it.
      <li>For monthly time series, each produced record is given a time stamp of
      01/MM/YYYY 00:00, and is the aggregation of N source records, where N is
      the number of days in month MM/YYYY. The source records used in the
      aggregation are the record whose time stamp equals the beginning of the
      month following MM/YYYY plus TimeOffset, and the N-1 records following.
      <li>For yearly time series, each produced record is given a time stamp of
      01/01/YYYY 00:00 if the calendar year is used, and 01/10/YYYY 00:00 if the
      hydrological year is used; in both cases all fields except for the year
      are to be ignored. If the calendar year is used, the produced record is
      the aggregation of the 12 source records whose time stamps belong to year
      YYYY; if the hydrological year is used, the produced record is the
      aggregation of the 12 source records whose time stamps are in the interval
      (10/YYYY - 09/YYYY+1). The hydrological year is used if
      agoHydrologicalYear is specified in Options; otherwise, the calendar year
      is used.
    </ul><p>

    If some of the source records corresponding to a destination record are
    missing, the MissingAllowed parameter specifies what will be done. If more
    than MissingAllowed source records are missing or null, the resulting
    destination record is null; otherwise, the destination record is derived
    even though some records are missing. In that case, the flag specified by
    MissingFlag is raised in the destination record.<p>

    If the Missing parameter is not nil, then any already existing records in
    Missing are deleted, and additional results are stored there. Specifically,
    for each destination record a record with the same date is also created in
    Missing, containing the number of missing source values for that destination
    record. If Missing is nil, no such output is produced.<p>

    Method specifies if the produced records are sums (agmSum), averages
    (agmAverage), minimums (agmMinimum), maximums (agmMaximum), or vector
    averages (agmVector) of the source records. In the case of agmVector, the
    source records are considered to be directions in degrees (as in a wind
    direction time series); each produced record is the direction in degrees of
    the sum of the unit vectors whose direction is specified by the source
    records.<p>
    
    If Method is agmDefault, the method is determined from Source.VariableType:
    <ul>
      <li>If Source.VariableType is ivtInstantaneous or ivtAverage, agmAverage
      is used.
      <li>If Source.VariableType is ivtCumulative, agmSum is used.
      <li>If Source.VariableType is ivtMaximum, agmMaximum is used.
      <li>If Source.VariableType is ivtMinimum, agmMinimum is used.
    </ul>
    <p>
    TAggregationDialog may be used as user interface.<p>
    @author A.X.
    @SeeAlso <See Property=TTimeseries.VariableType>
    @SeeAlso <See Property=TTimeseries.TimeStep>
    @SeeAlso <See Property=TTimeseries.DateOffset>
    @SeeAlso <See Property=TTimeseries.TimeStepStrict>
    @SeeAlso <See Routine=RegularizeStep>
    @SeeAlso <See Class=TAggregationDialog>
}
procedure Aggregate(Source, Dest: TTimeseries; TimeOffset: TDateOffset;
  Method: TAggregateMethod; MissingAllowed: Integer; MissingFlag: string;
  Missing: TTimeseries; FromMonth, ToMonth: Integer;
  Options: TAggregateOptions);

type

  {** Stores regression results.
      Use TRegressionResults to hold together all co-efficents resulting from
      timeseries regression.<p>
      TRegressionResults descends from TVector. The elements of that vector are
      the co-efficients of regression - the first element corresponds to the
      constant term and the next to the co-efficients of the independent time
      series. TRegressionResults defines also a couple of additional properties
      holding statistical values.
      @author A.X.
      @SeeAlso <See Routine=RegressTimeseries>
  }
  TRegressionResults = class(TVector)
  private
    FCoefficientOfDetermination: Double;
    FCriticalCoefficientOfDetermination: Real;
    FCommonPeriodCardinality: Integer;
    FTimeseriesErrorMeanValue: Real;
    FTimeseriesErrorStandardDeviation: Real;
    function GetCoefficientOfCorrelation: Double;
  public
    {** Stores the co-efficient of determination.
    }
    property CoefficientOfDetermination: Double read FCoefficientOfDetermination
      write FCoefficientOfDetermination;
    {** Returns the co-efficient of correlation.
        The co-efficient of correlation is the square root of the co-efficient
	of determination with the sign of the constant term E[0].
	Attempting to read the co-efficient of correlation when the coefficient
	of determination is negative raises an exception.
	@SeeAlso <See Property=CoefficientOfDetermination>
	@SeeAlso <See Property=Coefficients>
    }
    property CoefficientOfCorrelation: Double
      read GetCoefficientOfCorrelation;
    {** Stores the cardinality of the common period.
        When performing regression between two or more time series, only the
	common period is used. The common period is defined as the set of all
	time stamps for which all time series have a value. Use
	CommonPeriodCardinality for the number of time stamps in the common
	period.
    }
    property CommonPeriodCardinality: Integer read FCommonPeriodCardinality
      write FCommonPeriodCardinality;
    {** Returns the mean value of the determination errors. Used to add
        error terms to interpolated values.
    }
    property TimeseriesErrorMeanValue: Real read FTimeseriesErrorMeanValue
      write FTimeseriesErrorMeanValue;
    {** Returns the standard deviation of the determination errors. Used to add
        error terms to interpolated values.
    }
    property TimeseriesErrorStandardDeviation: Real
      read FTimeseriesErrorStandardDeviation
      write FTimeseriesErrorStandardDeviation;
    {** The critical (minimum) value for the coefficient of determination.
    }
    property CriticalCoefficientOfDetermination: Real read
      FCriticalCoefficientOfDetermination write
        FCriticalCoefficientOfDetermination;      
  end;

  TRegressTimeseriesOption = (rtOrganic, rtSeasonal, rtCrossesZero,
    rtDoFilling, rtRandomTerm, rtTruncToZero, rtRandomSeed,
    rtDoExtendBefore, rtDoExtendAfter, rtDonotFillInnerValues, rtMeanValue,
    rtOptimize);
  TRegressTimeseriesOptions = set of TRegressTimeseriesOption;

  TOptimizeRegressionOption = (oroDoNotUse=0, oroUse=1, oroAuto=2);
  TArrayOfOptimizedRegressTimeseries = array of TOptimizeRegressionOption;
  TOptimizeRegressionOptionsForMonth = record
    TimeseriesSettings: TArrayOfOptimizedRegressTimeseries;
    MaxTimeseries: Integer;
    OnlyPositiveCoefficients: Boolean;
  end;
  TSeasonalOptimizeRegressionOptions =
    array of TOptimizeRegressionOptionsForMonth;

{** Performs time series regression.
    Use RegressTimeseries to perform regression of the dependent timeseries to
    the independent timeseries.<p>
    Lag specifies the order of autocorrelation. If Lag is zero, there is no
    autocorrelation. If rtOrganic is specified in Options, regression will be
    organic. In that case, IndependentTimeseries must hold exactly one time
    series. Otherwise, it must hold at least one time series.  If rtCrossesZero
    is specified in Options, then the constant term of the regression will be
    zero.<p>
    If rtSeasonal is specified in Options, then 12 different regressions are
    performed, one for each month.<p>
    RegressTimeseries raises an exception in case of errors in the arguments. It
    raises EZeroDivide if the matrix is not invertible.<p>
    The return value depends on whether rtSeasonal has been specified. If it has
    not been specified, RegressTimeseries creates and returns a
    TRegressionResults object. Otherwise, it creates and returns a TObjectList
    object which contains (and owns) 12 TRegressionResults objects, one for each
    month, the first one (Items[0]) for January.
    @author A.X.
}
function RegressTimeseries (DependentTimeseries: TTimeseries;
  IndependentTimeseries: TObjectList; Lag: Integer;
  Options: TRegressTimeseriesOptions;
  OptimizationArray: TSeasonalOptimizeRegressionOptions): TObject; overload;

{** WRITE HELP
}
function RegressTimeseries(DependentTimeseries: TTimeseries;
  IndependentTimeseries: TObjectList; ADateTimeList: TDateTimeList;
  Options: TRegressTimeseriesOptions; Optimize: Boolean;
  OptimizationOptions: TOptimizeRegressionOptionsForMonth): TObject; overload;

{** (Overloaded function - this line is not shown in the help).
    This version of RegressTimeseries performs regression for a specific list of
    dates. For all dates in ADateTimeList not null values must exist in all time
    series, or an exception is raised.<p>
    Normally you don't have to call this version; use the other overloaded
    version of RegressTimeseries, which will determine the common period and
    call this one.<p>
    rtSeasonal is ignored in Options for this version.<p>
    @author A.X.
}
function RegressTimeseries(DependentTimeseries: TTimeseries;
  IndependentTimeseries: TObjectList; ADateTimeList: TDateTimeList;
  Options: TRegressTimeseriesOptions): TObject; overload;

{** Organic regression on one independent and one dependent timeseries.
    If IndependentTimeseries number is different than 1, an exception
    is raised. Coefficient of determination is calculated from
    2*|rxy|-1. Constant coefficient a = my-b*mx where
    b = sing(rxy)*sy/sx<p>
    OrganicRegressTimeseries is called from RegressTimeseries function
    when rtOrganic is in RegressTimeseries Options.
    @author Stefanos
    @SeeAlso <See Routine=RegressTimeseries>
}
function OrganicRegressTimeseries(DependentTimeseries: TTimeseries;
  IndependentTimeseries: TObjectList; ADateTimeList: TDateTimeList): TObject;

{** Determines the common period of a list of time series.
    Use GetCommonPeriod to find the list of dates for which a not null value
    exists for all time series in the TimeseriesList. If Month is not zero,
    GetCommonPeriod returns the common period for the specified month only.
}
function GetCommonPeriod(TimeseriesList: TObjectList; Month: Integer):
  TDateTimeList;

{** GetAllRecords returns a list w all possible records from time series list.
    Resulting DateTimeList contains date records for every time series record
    from time series list, null and not-null values.
}
function GetAllRecords(TimeseriesList: TObjectList; Month: Integer):
  TDateTimeList;

{** Creates the matrix of covariances of a list of timeseries.
    Use CreateCovarianceMatrix to create the matrix each element E[i, j] of
    which is the covariance of time series i-1 with the time series j-1 of the
    list, for the specified list of dates. For all dates of ADateTimeList all
    time series must have not null values, or an exception is raised.
    @author A.X.
    @SeeAlso <See Routine=GetCommonPeriod>
    @SeeAlso <See Routine=CreateSumOfProductsMatrix>
}
function CreateCovarianceMatrix(TimeseriesList: TObjectList;
  ADateTimeList: TDateTimeList): TMatrix;

{** Creates the matrix of the sum of products of a list of timeseries.
    Use CreateSumOfProductsMatrix to create the matrix each element E[i, j] of
    which is the sum of products of time series i-1 with the time series j-1 of
    the list, for the specified list of dates. For all dates of ADateTimeList
    all time series must have not null values, or an exception is raised.
    @author A.X.
    @SeeAlso <See Routine=GetCommonPeriod>
    @SeeAlso <See Routine=CreateCovarianceMatrix>
}
function CreateSumOfProductsMatrix(TimeseriesList: TObjectList;
  ADateTimeList: TDateTimeList): TMatrix;

{** Calculates the mean square error of a time series.
    Use TimeseriesMSE to calculate the mean square error after
    performing timeseries regression. It is mostly used internally by
    RegressTimeseries to calculate the co-efficient of determination.
    @author A.X.
    @SeeAlso <See Routine=RegressTimeseries>
    @SeeAlso <See Routine=GetCommonPeriod>
}
function TimeseriesMSE(DependentTimeseries: TTimeseries;
  IndependentTimeseries: TObjectList; CommonPeriod: TDateTimeList;
  Coefficients: TVector): Double;

{** Calculates the mean value of the errors after performing
    timeseries regression. It is mostly used internally to
    add a random error term when filling missing values.
    @author Stefanos
    @SeeAlso <See Routine=RegressTimeseries>
    @SeeAlso <See Routine=GetCommonPeriod>
    @SeeAlso <See Routine=TimeseriesMSE>
}
function TimeseriesErrorMeanValue(DependentTimeseries: TTimeseries;
  IndependentTimeseries: TObjectList; CommonPeriod: TDateTimeList;
  Coefficients: TVector): Double;

{** Calculates the linear combination of time series.
    Use TimeseriesLinComb to calculate the linear combination of some
    independent time series.<p>
    Coefficients[1] is the constant term; Coefficients[2] is the coefficient of
    IndependentTimeseries[0]; and so on.<p>
    Any already existing data in DependentTimeseries are deleted before
    performing the combination.
    @author A.X.
}
procedure TimeseriesLinComb(DependentTimeseries: TTimeseries;
  IndependentTimeseries: TObjectList; Coefficients: TVector); overload;

{** Calculates the linear combination of time series.
    This version of TimeseriesLinComb does the linear combination for a specific
    list of dates. For all dates in ADateTimeList not null values must exist in
    all time series, or an exception is raised.<p>
    Normally you don't have to call this version; use the other overloaded
    version of TimeseriesLinComb, which will determine the common period and
    call this one.<p>
    @author A.X.
}
procedure TimeseriesLinComb(DependentTimeseries: TTimeseries;
  IndependentTimeseries: TObjectList; ADateTimeList: TDateTimeList;
  Coefficients: TVector); overload;

{** Fills the missing records of the Destination Timeseries.
    TimeseriesFillMissing use a set of IndependentTimeseries
    stored in a TObjectList as well a set of coefficients
    stored in a Tvector and by application of the TimeseriesLinComb routine,
    calculates a new timeseries. Missing values of Destination
    are being filled by the new timeseries.<p>
    TimeseriesLinComb may be used in conjunction with the
    RegressTimeseries.<p>
    @author Stefanos
    @SeeAlso <See Routine=RegressTimeseries>
    @SeeAlso <See Routine=TimeseriesLinComb>
}
procedure TimeseriesFillMissing(IndependentTimeseries: TObjectList;
  Destination:TTimeseries; RegressionResults: TObject;
  Options: TRegressTimeseriesOptions);

{** TimeseriesShiftDates creates a copy of Source timeseries to
    destination timeseries, but dates shifted by Timestep*ShiftBy.<p>
    First, source timeseries is assigned to destination, then
    destination is being cleared and new records are being
    added that the are copies of source records with dates
    shifted.
    @author Stefanos
}
procedure TimeseriesShiftDates(Source, Destination: TTimeseries;
  ShiftBy: Integer);

{** Integrates time series by time using the method of trapezoid.
    Time unit is expressed in seconds and takes values such as 1 for
    unit per second, 3600 for unit per hour, etc.<p>
    The limits of integration is Source.StartDate and Source.EndDate
    if First and Last records are not null. TimeseriesIntegrate
    doesn't consider null records.
    TimeInterval may be from tstYearly or tstMonthly, an attempth
    to set a different TimeInterval raises exception.<p>
    Destination timeseries is cleared prior to write the integrated
    monthly/yearly values.<p>
    Setting AverageValue to True, calculates monthly/yearly average
    values.
    @author Stefanos
}
procedure TimeseriesIntegrate(Source, Destination: TTimeseries;TimeUnit: Real;
  TimeInterval: TTimeStep; Denominator: Real;AverageValue: Boolean);

type
  TIDFOption = (idfoIntensity, idfoAllowMissing, idfoHydrologicalYear);
  TIDFOptions = set of TIDFOption;

{** Accumulates data from ten-minute, hourly, daily timeseries to
    Extreme (max) values time-series of monthly or yearly time step.
    Multiplier specifies the accumulation units e.g. a multiplier of 6
    to a ten-minutes time series, accumulates to 1 hour intervals and
    procedure returns maximum yearly or monthly values for 1 hour maximum
    values.<p>
    Specify the desired accumulated time series timestep with
    AnalysisTimestep. AnalysisTimestep should be of tstYearly or
    tstMonthly or else an Assertion Exception is raised.<p>
    Source time series should be of ten minute, hourly or daily strict
    time step or else an exception is raised.<p>
    Specify MissingFlag and MarginalFlag so that flags are raised when
    missing values are considered. Missing flag is raised when
    idfoAllowMissing option is set and missing values are inclued in
    the accumulation interval. Marginal flag is raised when accumulation
    interval bounds with missing(s) value(s).
    @author Stefanos
}
procedure TimeseriesToSingleIDF(Source, Dest, MissingPercent, DayTS: TTimeseries;
  Multiplier: Integer; AnalysisTimestep: TTimestep; Options: TIDFOptions;
  MissingFlag, MarginalFlag: string);

type
{** Disaggregation method type.
    Choose between average or cumulative variable type, resulting to
    constant or random disaggregated values within the original time step.
    @SeeAlso <See Routine=TimeseriesDisaggregate>
}
  TDisaggregateMethod = (tdoAverageConstant, tdoAverageRandom,
    tdoInstantaneousLinear, tdoCumulativeConstant, tdoCumulativeRandom);
  TDisaggregateMethods = set of TDisaggregateMethod;
{** Random model when the disaggregation mode includes random term.
}
  TDisaggregateRandomModel = (tdrmUniform, tdrmExponential, tdrmLogarithmic,
    tdrmQuadric, tdrmHighOrder);
  TDisaggregateRandomModels = set of TDisaggregateRandomModel;

{** Disaggregate time series using the next smaller time step.
    Disaggregation order:<p>
    Annual -> monthly -> daily -> hourly -> ten minutes<p>
    Disaggregated time series could not reprocedure the statistical properties
    of the smaller time step since no information is supplied for this.
    Disaggration is based on the assumption of constant values within the
    original time step or random values from a uniform statistical distribution.
    <p>
    Say, for a monthly value of rainfall of 30 mm for the April of 2007,
    thirty disaggregated values of 1 mm will be produced for each day of
    April. If random values method is selected, the sum of the 30 daily values
    will be 30 mm. For a average value variable type, e.g. a mean temperature of
    25oC for the Month of May 2007, will produce 31 values of 25oC for each day
    of May. If random values method is selected, the mean value of the 31
    daily values will be also 25oC..
    @author Stefanos
}
procedure TimeseriesDisaggregate(Source, Dest: TTimeseries; Method:
  TDisaggregateMethod); overload;

procedure TimeseriesDisaggregate(Source, Dest: TTimeseries; Method:
  TDisaggregateMethod; RandomModel: TDisaggregateRandomModel); overload;

implementation

uses SysUtils, Math, rnd, dkmath, prob, dateutils, isets;

resourcestring
  rsRangeCheckHighGreaterLow = 'Range check: High Limit should be greater'+
    ' than Low Limit';

procedure RangeCheck(Timeseries: TTimeseries; var LowLimit, HighLimit: Real;
  const Flag: string; AutoLow, AutoHigh: Boolean; ProbabilityLevel: Real);
var i: Integer;
begin
  if AutoLow then
    LowLimit := InvNormalcdf(0.5*(1-ProbabilityLevel),
      Timeseries.Mean,Sqrt(Timeseries.Variance));
  if AutoHigh then
    HighLimit := InvNormalcdf(0.5*(1+ProbabilityLevel),
      Timeseries.Mean,Sqrt(Timeseries.Variance));
  if LowLimit >= HighLimit then
    raise Exception.Create(rsRangeCheckHighGreaterLow);
  for i := 0 to Timeseries.Count-1 do
    with Timeseries[i] do
      if not IsNull then
        if (AsFloat>HighLimit) or (AsFloat<LowLimit) then
          SetFlag(Flag, True)
      else
        SetFlag(Flag, False);
end;

procedure TimeConsistencyCheck(Timeseries: TTimeseries; Triger: Real;
  const Flag: string);
var i: Integer;
begin
  for i := 1 to Timeseries.Count-1 do
  begin
    if (not Timeseries[i].IsNull) and (not Timeseries[i-1].IsNull) then
    begin
      if ( abs(Timeseries[i].AsFloat - Timeseries[i-1].AsFloat) >= Triger ) then
        Timeseries[i].SetFlag(Flag, True)
      else
        Timeseries[i].SetFlag(Flag, False);
    end;
  end;
end;

procedure AddTimeseries(Source, Dest: TTimeseries);
var i, k: Integer;
begin
  Assert((Dest<>nil) and (Source<>nil));
  for i := 0 to Source.Count-1 do
  begin
    k := Dest.IndexOf(Source.Items[i].Date);
    if (k<>-1) and not Dest.Items[k].IsNull and not Source.Items[i].IsNull then
      Dest.Items[k].AsFloat := Dest.Items[k].AsFloat + Source.Items[i].AsFloat;
  end;
end;

resourcestring
  rsInvalidRegularizedStep = 'Step regularization may only be done for '+
    'time steps less than monthly';
  rsDestHasDifferentTimeStep = 'The destination time series is in a '+
    'different time step from the source time series';

{ Used by RegularizeStep to delete whichever part of Dest needs to be deleted
  and create null records. StartIndex and EndIndex are set to the beginning and
  end of these null records. }
procedure PrepareResult(Source, Dest: TTimeseries; TimeOffset: Integer;
  var StartIndex, EndIndex: Integer; StepSeconds: Integer);
var
  SourceStartCSecs, SourceEndCSecs: TCDateTime;
    { Start and end date of source time series in seconds since 1900 }
  DestStartCSecs, DestEndCSecs: TCDateTime;
    { Start and end date of resulting interval in seconds since 1900 }
  d: TCDateTime;
begin
  { Determine start and end dates }
  SourceStartCSecs := DateTimeToC(Source.First.Date)*86400;
  SourceEndCSecs := DateTimeToC(Source.Last.Date)*86400;
  DestStartCSecs := (Int(SourceStartCSecs/StepSeconds) + 1)*StepSeconds +
    TimeOffset*60;
  while DestStartCSecs>SourceStartCSecs do
    DestStartCSecs := DestStartCSecs - StepSeconds;
  DestEndCSecs := (Int(SourceEndCSecs/StepSeconds) - 2)*StepSeconds +
    TimeOffset*60; { -2 rather than -1 in case it's negative }
  while DestEndCSecs<=SourceEndCSecs-StepSeconds/2 do
    DestEndCSecs := DestEndCSecs + StepSeconds;

  { Delete relevant part of Dest }
  StartIndex := Dest.PositionOfNext(DestStartCSecs/86400);
  if StartIndex = -1 then StartIndex := 0;
  while (StartIndex<Dest.Count)
  and (DateTimeToC(Dest[StartIndex].Date)*86400<=DestEndCSecs) do
    Dest.Delete(StartIndex);

  { Now create relevant part of Dest }
  d := DestStartCSecs;
  EndIndex := StartIndex-1;
  while d<=DestEndCSecs do
  begin
    Inc(EndIndex);
    Dest.Insert(CToDateTime(d/86400), True, 0, '', msUnmodified);
    d := d + StepSeconds;
  end;
end;

{ RegularizeStep for the case of instantaneous and vector variable }
procedure RegularizeStepInstantaneous(Source, Dest: TTimeseries; TimeOffset:
  Integer; const NewDateFlag: string; Method: TRegularizeStepMethod;
  StartIndex, EndIndex: Integer; StepSeconds: Integer; var Warnings: string);
var
  i, j, k: Integer;
  SmallDiff, LargeDiff, tem: Int64;
  x, y: Real;
begin
  for i := StartIndex to EndIndex do
  begin
    j := Source.IndexOf(Dest[i].Date);
    if j<>-1 then
    begin
      if not Source[j].IsNull then Dest[i].AsFloat := Source[j].AsFloat;
      Dest[i].SetAllFlags(Source[j].GetAllFlags);
      Continue;
    end;
    if NewDateFlag<>'' then Dest[i].SetFlag(NewDateFlag, True);
    k := Source.PositionOfNext(Dest[i].Date);
    j := Source.PositionOfPrevious(Dest[i].Date);
    if (k = 0) and (Source.Count>1) then
    begin
      Assert(j = -1);
      j := 1;
    end;
    if (j = Source.Count-1) and (Source.Count>1) then
    begin
      Assert(k = -1);
      Dec(j);
      k := j+1;
    end;
    if (j<>-1) and (k<>-1) then
    begin
      SmallDiff := DiffInSecs(Source[k].Date, Dest[i].Date);
      LargeDiff := DiffInSecs(Dest[i].Date, Source[j].Date);
      if SmallDiff>LargeDiff then
      begin
        tem := SmallDiff;
        SmallDiff := LargeDiff;
        LargeDiff := tem;
      end;
      if (not Source[j].IsNull) and (not Source[k].IsNull)
        and (SmallDiff<=0.50*StepSeconds) and (LargeDiff<=1.50*StepSeconds) then
      begin
        if Method = rsmInstantaneous then
        begin
          Dest[i].AsFloat :=
            (DiffInSecs(Source[k].Date, Dest[i].Date)*Source[j].AsFloat +
            DiffInSecs(Dest[i].Date, Source[j].Date)*Source[k].AsFloat)
            /DiffInSecs(Source[k].Date, Source[j].Date);
        end else if Method = rsmVector then
        begin
          x := (DiffInSecs(Source[k].Date, Dest[i].Date)*
            Cos(Source[j].AsFloat/180*Pi) +
            DiffInSecs(Dest[i].Date, Source[j].Date)*
            Cos(Source[k].AsFloat/180*Pi))
            /DiffInSecs(Source[k].Date, Source[j].Date);
          y := (DiffInSecs(Source[k].Date, Dest[i].Date)*
            Sin(Source[j].AsFloat/180*Pi) +
            DiffInSecs(Dest[i].Date, Source[j].Date)*
            Sin(Source[k].AsFloat/180*Pi))
            /DiffInSecs(Source[k].Date, Source[j].Date);
            Dest[i].AsFloat := ArcTan(Abs(y/x))/Pi*180;
          if (x<0) and (y>0) then Dest[i].AsFloat := 180-Dest[i].AsFloat
          else if (x<0) and (y<0) then Dest[i].AsFloat := 180+Dest[i].AsFloat
          else if (x>0) and (y<0) then Dest[i].AsFloat := 360-Dest[i].AsFloat;
        end else Assert(False);
        Dest[i].SetAllFlags(Source[Source.NearestTo(Dest[i].Date)].GetAllFlags);
        if NewDateFlag<>'' then
          Dest[i].SetFlag(NewDateFlag, True);
        Continue;
      end;
    end;
  end;
end;

{ RegularizeStep for rsmCumulativeVStep }
procedure RegularizeStepCumulativeVStep(Source, Dest: TTimeseries; TimeOffset:
  Integer; const NewDateFlag: string; Method: TRegularizeStepMethod;
  StartIndex, EndIndex: Integer; StepSeconds: Integer; var Warnings: string);
var
  i, j, k, n: Integer;
  t1, t2, ta1, ta2, dt: TCDateTime;
begin
  for i := StartIndex to Endindex do
  begin
    j := Source.IndexOf(Dest[i].Date);
    if j<>-1 then
      if (j=0) or (DiffInSecs(Source[j].Date,Source[j-1].Date)<=StepSeconds) then
      begin
        if not Source[j].IsNull then Dest[i].AsFloat := Source[j].AsFloat;
        Dest[i].SetAllFlags(Source[j].GetAllFlags);
        Continue;
      end;
    if NewDateFlag<>'' then Dest[i].SetFlag(NewDateFlag, True);
    { Find all records from t-TimeStep to the first >= t }
    j := Source.PositionOfNext(AddDateTime(Dest[i].Date, -StepSeconds/86400));
    k := Source.PositionOfNext(Dest[i].Date);

    n := Source.NearestTo(Dest[i].Date);
    if (n=-1) or (Abs(DiffInSecs(Source[n].Date,Dest[i].Date))>0.50*StepSeconds)
    then Continue;
    for n := j to k do
    begin
      if Source[n].IsNull or
      ((n<>0) and (DiffInSecs(Source[n].Date, Source[n-1].Date)>
      1.50*StepSeconds)) then
      begin
        Dest[i].SetNull;
        Continue;
      end;
      if NewDateFlag<>'' then Dest[i].SetFlag(NewDateFlag, True);
      { Dest[i] holds rainfall in interval (t1, t2).
        Source[n] holds rainfall in interval (ta1, ta2).
        dt is the length of the intersection of these two intervals.
        Source[n].AsFloat * dt / (ta2-ta1) is the portion of Source[n].AsFloat
        that corresponds to Dest[i]. }
      t2 := DateTimeToC(Dest[i].Date);
      t1 := t2 - StepSeconds/86400;
      ta2 := DateTimeToC(Source[n].Date);
      if n>0 then ta1 := DateTimeToC(Source[n-1].Date)
      else ta1 := ta2 - StepSeconds/86400;
      dt := t2 - t1;
      if ta1>t1 then dt := dt - (ta1-t1);
      if ta2<t2 then dt := dt - (t2-ta2);
      if Dest[i].IsNull then Dest[i].AsFloat := 0;
      Dest[i].AsFloat := Dest[i].AsFloat + Source[n].AsFloat*dt/(ta2-ta1);
    end;
    Dest[i].SetAllFlags(Source[Source.NearestTo(Dest[i].Date)].GetAllFlags);
    if NewDateFlag<>'' then Dest[i].SetFlag(NewDateFlag, True);
  end;
end;

{ RegularizeStep for rsmCumulativeCStep }
procedure RegularizeStepCumulativeCStep(Source, Dest: TTimeseries; TimeOffset:
  Integer; const NewDateFlag: string; Method: TRegularizeStepMethod;
  StartIndex, EndIndex: Integer; StepSeconds: Integer; var Warnings: string);
var i, j, k: Integer;
  ANullFlag: Boolean;
begin
  Warnings := '';
  for i := StartIndex to EndIndex do
  begin
    j := Source.IndexOf(Dest[i].Date);
    if (j=-1) and (NewDateFlag<>'') then Dest[i].SetFlag(NewDateFlag, True);
    j := Source.PositionOfNext(AddDateTime(Dest[i].Date,-(StepSeconds-1)/86400));
    k := Source.PositionOfPrevious(AddDateTime(Dest[i].Date,
                                               (StepSeconds-1)/86400));
                                               { The minus one is to make it
                                                 non-inclusive }
    ANullFlag := False;
    if j<>-1 then
    begin
      while j<=k do
      begin
        if Source[j].IsNull then
        begin
          ANullFlag := True;
          Inc(j);
          Continue;
        end;
        if Dest[i].IsNull then
          Dest[i].AsFloat := 0;
        Dest[i].AsFloat := Dest[i].AsFloat + Source[j].AsFloat *
          (StepSeconds-Abs(DiffInSecs(Dest[i].Date,Source[j].Date)))/
          StepSeconds;
        if Source[j].GetAllFlags<>'' then
          Dest[i].SetAllFlags(Dest[i].GetAllFlags+ ' '+ Source[j].GetAllFlags);
        Inc(j);
      end;
      if (ANullFlag) and (not Dest[i].IsNull) then
      begin
        for j := i-1 downto 0 do
          if not Dest[j].IsNull then
          begin
            Dest[j].AsFloat := Dest[j].AsFloat + Dest[i].AsFloat;
            Break;
          end;
        Dest[i].SetNull;
      end;
    end;
  end;
end;

resourcestring
  rsSourceRecordIgnored = 'Source record ignored';

{ RegularizeStep for rsmChangeTime }
procedure RegularizeStepChangeTime(Source, Dest: TTimeseries; TimeOffset:
  Integer; const NewDateFlag: string; Method: TRegularizeStepMethod;
  StartIndex, EndIndex: Integer; StepSeconds: Integer; var Warnings: string);
var
  j, i1, i2, i, d1, d2: Integer;
begin
  for j := 0 to Source.Count-1 do
  begin
    i1 := Dest.PositionOfPrevious(Source[j].Date);
    i2 := Dest.PositionOfNext(Source[j].Date);
    if i1 = i2 then
      i := i1
    else if i1 < 0 then
      i := i2
    else if i2 < 0 then
      i := i1
    else
    begin
      d1 := DiffInSecs(Source[j].Date, Dest[i1].Date);
      d2 := DiffInSecs(Dest[i2].Date, Source[j].Date);
      if d1 < d2 then
        i := i1
      else if d1 > d2 then
        i := i2
      else if Dest[i1].IsNull then
        i := i1
      else
        i := i2;
    end;
    if not Dest[i].IsNull then
    begin
      Warnings := Warnings + rsSourceRecordIgnored + ': ' +
        Source[j].DateAsString + #13#10;
      Continue;
    end;
    Dest[i].AsString := Source[j].AsString;
    Dest[i].SetAllFlags(Source[j].GetAllFlags);
    if (NewDateFlag<>'') and (DiffInSecs(Dest[i].Date, Source[j].Date)<>0) then
      Dest[i].SetFlag(NewDateFlag, True);
  end;

end;


procedure RegularizeStep(Source, Dest: TTimeseries; TimeOffset: Integer;
  StepMinutes: Integer; const NewDateFlag: string;
  Method: TRegularizeStepMethod; var Warnings: string);
var
  StartIndex, EndIndex: Integer; // start and end index of result in Dest
  StepSeconds: Integer; // time step in seconds
  AOrdinalValue: Integer;
begin
  Warnings := '';
  if Source.TimeStep>=tstMonthly then
    raise Exception.Create(rsInvalidRegularizedStep);
  StepSeconds := StepMinutes*60;
  if (Dest.TimeStep<>Source.TimeStep) and (Dest.Count>0) then
    raise Exception.Create(rsDestHasDifferentTimeStep);
  Dest.AssignMeta(Source);
  {set ordinal value for backwards compatibility}
  case StepMinutes of
    5: AOrdinalValue := tstFiveMinute.OrdinalValue;
    10: AOrdinalValue := tstTenMinute.OrdinalValue;
    60: AOrdinalValue := tstHourly.OrdinalValue;
    1440: AOrdinalValue := tstDaily.OrdinalValue;
  else
    AOrdinalValue := 0;
  end;
  if Source.TimeStep.LengthMinutes<>StepMinutes then
    Dest.TimeStep := TTimeStep.Create(StepMinutes, 0, AOrdinalValue);
  Dest.TimeStepStrict := True;
  PrepareResult(Source, Dest, TimeOffset, StartIndex, EndIndex, StepSeconds);
  case Method of
    rsmInstantaneous:
      RegularizeStepInstantaneous(Source, Dest, TimeOffset, NewDateFlag,
        Method, StartIndex, EndIndex, StepSeconds, Warnings);
    rsmCumulativeCStep:
      RegularizeStepCumulativeCStep(Source, Dest, TimeOffset, NewDateFlag,
	      Method, StartIndex, EndIndex, StepSeconds, Warnings);
    rsmCumulativeVStep:
      RegularizeStepCumulativeVStep(Source, Dest, TimeOffset, NewDateFlag,
	      Method, StartIndex, EndIndex, StepSeconds, Warnings);
    rsmVector:
      RegularizeStepInstantaneous(Source, Dest, TimeOffset, NewDateFlag,
	      Method, StartIndex, EndIndex, StepSeconds, Warnings);
    rsmChangeTime:
      RegularizeStepChangeTime(Source, Dest, TimeOffset, NewDateFlag,
	      Method, StartIndex, EndIndex, StepSeconds, Warnings);
  else
    Assert(False);
  end;
end;

{ Aggregate }

type

  { Aggregate uses state-transition. Here are the states. }
  TAggregateState =
    (aesEnd,           { Finished processing. }
     aesStartCollect,  { About to determine first of Source records to be
                         aggregated for a single Dest record. }
     aesMiddleCollect, { In the middle of determining Source records to be
                         aggregated for a single Dest record. }
     aesEndCollect     { Determined last of Source records to be aggregated
                         for a single Dest record. }
    );

  { The following record holds together all variables to be passed from one
    state to another. }
  TAggregateData = record
    Source, Dest, Missing: TTimeseries;
    MissingAllowed: Integer;
    MissingFlag: string;
    SourceStepMinutes, DestStepMinutes: Integer;
                          { Not used when aggregating monthly. }
    Time: TDateTime;      { Time being examined in Source. }
    SourceCount: Integer; { Number of not null Source records having been
                            collected for the Dest record being considered. }
    x, y: Real; { Cartesian co-ordinates of sum of vectors, used if agmVector. }
    AggregatedValue: Real;
    TimeOffset: TDateOffset;
    FromMonth, ToMonth: Integer;
    Method: TAggregateMethod;
    Options: TAggregateOptions;
    function ActualTime: TDateTime; {Returns actual time based on
                                     Source.ActualOffset}
  end;

  function TAggregateData.ActualTime: TDateTime;
  begin
    if Source.TimeStep<tstMonthly then
      Result := AddDateTime(Time, Source.ActualOffset.Minutes/1440)
    else
      Result := IncMonth(Time, Source.ActualOffset.Months);
  end;

{ The following three functions handle the various states. }

function AggregateStartCollect(var AggregateData: TAggregateData):
  TAggregateState;
begin
  with AggregateData do
  begin
    SourceCount := 0;
    case Method of
      agmMaximum: AggregatedValue := -1e38;
      agmMinimum: AggregatedValue := 1e38;
      agmAverage, agmSum, agmInstant: AggregatedValue := 0;
      agmVector:
      begin
        x := 0;
        y := 0;
      end;
    else
      Assert(False);
    end;
  end;
  Result := aesMiddleCollect;
end;

function AggregateMiddleCollect(var AggregateData: TAggregateData):
  TAggregateState;
var
  i: Integer;
  OnLastRecord, ExcludeRecord: Boolean;
  Year, Month, Day, Hour, Min, Sec, MSec: Word;
begin
  ExcludeRecord := False;
  OnLastRecord := False;  { Eliminates compiler warning. }
  with AggregateData do
  begin
    if (agoSeasonal in Options) and (Source.TimeStep = tstMonthly) then
    begin
      if ToMonth >= FromMonth then
      begin
        if (MonthOf(Time)<FromMonth) or (MonthOf(Time)>ToMonth) then
          ExcludeRecord := True
      end else begin
        if (MonthOf(Time)>ToMonth) and (MonthOf(Time)<FromMonth) then
          ExcludeRecord := True;
      end;
    end;
    i := Source.IndexOf(Time);
    if (i>=0) and (not Source[i].IsNull) and (not ExcludeRecord) then
    begin
      Inc(SourceCount);
      case Method of
        agmMaximum: if Source[i].AsFloat > AggregatedValue then
                      AggregatedValue := Source[i].AsFloat;
        agmMinimum: if Source[i].AsFloat < AggregatedValue then
                      AggregatedValue := Source[i].AsFloat;
        agmAverage, agmSum: AggregatedValue := AggregatedValue+
                                               Source[i].AsFloat;
        agmInstant:
          AggregatedValue := 0;
        agmVector:
          begin
            x := x + Cos(Source[i].AsFloat/180*Pi);
            y := y + Sin(Source[i].AsFloat/180*Pi);
          end;
      else
        Assert(False);
      end;
    end;
    if Dest.TimeStep<tstMonthly then
      OnLastRecord :=
        (Round(DateTimeToC(ActualTime)*1440) mod DestStepMinutes =
          TimeOffset.Minutes)
    else if Dest.TimeStep = tstMonthly then
    begin
      DecodeDate(AddDateTime(ActualTime, -TimeOffset.Minutes/1440), Year,
        Month, Day);
      DecodeTime(AddDateTime(ActualTime, -TimeOffset.Minutes/1440),
        Hour, Min, Sec, MSec);
      OnLastRecord := (Day=1) and (Hour=0) and (Min=0);
    end
    else if Source.TimeStep = tstMonthly then
        begin
          if agoSeasonal in Options then
          begin
            if not (agoHydrologicalYear in Options) then
            begin
              if ToMonth<FromMonth then
                OnLastRecord := MonthOf(ActualTime)=ToMonth+1 else
                OnLastRecord := MonthOf(ActualTime)=1;
            end else
            begin
              if ToMonth<10 then
                OnLastRecord := MonthOf(ActualTime)=10 else
                OnLastRecord := MonthOf(ActualTime)=ToMonth+1;
            end;
          end else begin
            OnLastRecord :=
              (((agoHydrologicalYear in Options) and (MonthOf(ActualTime)=10)) or
              (not (agoHydrologicalYear in Options) and (MonthOf(ActualTime)=1)));
          end;
        end
    else
      Assert(False);
    if OnLastRecord then
      Result := aesEndCollect
    else begin
      Result := aesMiddleCollect;
      Time := Source.TimeStep.IncStep(Time)
    end;
  end;
end;

function AggregateEndCollect(var AggregateData: TAggregateData):
  TAggregateState;
var
  SourceCountShouldBe: Integer;
  MissingValues: Integer;
  Year, Month, Day: Word;
  Date: TDateTime;
  AIndex: Integer;
begin
  { Eliminate compiler warnings }
  SourceCountShouldBe := 0;
  Date := 0;

  with AggregateData do
  begin
    if Dest.TimeStep<tstMonthly then Date := ActualTime
    else if Dest.TimeStep=tstMonthly then
    begin
      DecodeDate(IncMonth(ActualTime-TimeOffset.Minutes/1440, -1), Year, Month,
        Day);
      Date := EncodeDateTime(Year, Month, 1, 0, 0, 0, 0);
    end
    else if Dest.TimeStep=tstYearly then
    begin
      DecodeDate(ActualTime, Year, Month, Day);
      if agoHydrologicalYear in Options then
      begin
        Month := 10;
        Dec(Year);
      end else
        Month := 1;
      if agoSeasonal in Options then
      begin
        if agoHydrologicalYear in Options then
        begin
          if ToMonth>=10 then
            Inc(Year);
        end else
        begin
         if ToMonth<FromMonth then
            Dec(Year);
        end;
      end;
      Date := EncodeDateTime(Year, Month, 1, 0, 0, 0, 0);
    end else
       Assert(False);
    if Dest.TimeStep < tstMonthly then
      SourceCountShouldBe := Dest.TimeStep.LengthMinutes div
        Source.TimeStep.LengthMinutes
    else if Source.TimeStep = tstMonthly then
      begin
        if not (agoSeasonal in Options) then
          SourceCountShouldBe := Dest.TimeStep.LengthMonths div
        Source.TimeStep.LengthMonths
        else begin
          SourceCountShouldBe := ToMonth-FromMonth;
          if SourceCountShouldBe >= 0 then
            SourceCountShouldBe := SourceCountShouldBe +1 else
            SourceCountShouldBe := 13+SourceCountShouldBe;
        end;
      end
    else if Dest.TimeStep = tstMonthly then
        SourceCountShouldBe := DaysInMonth(IncMonth(ActualTime-
          TimeOffset.Minutes/1440, -1))
    else
      Assert(False);

//???????????????Ti einai ayto katv????? Stef 2010-03-08T13:52
//    Dest.Remove(ActualTime);

    MissingValues := SourceCountShouldBe-SourceCount;
    if Method = agmInstant then
    begin
        if Dest.TimeStep=tstMonthly then
          Date := IncMonth(Date,1);
        if Dest.TimeStep=tstYearly then
          Time := IncMonth(Time,-11);
      if (Source.IndexOf(Time) < 0) or
        (Source.Items[Source.IndexOf(Time)].IsNull) then
      begin
        MissingAllowed := 0;
        MissingValues := 1;
      end else
      begin
        MissingValues := 0;
      end;
    end;
    if Missing<>nil then Missing.Insert(Date, False, MissingValues, '', msNew);
    if MissingValues>MissingAllowed then
      Dest.Insert(Date, True, 0, '', msNew)
    else
    begin
      case Method of
        agmAverage:
        begin
          if SourceCount>0 then
            AIndex := Dest.Insert(Date, False, AggregatedValue/SourceCount,
              '', msNew)
          else
            AIndex := Dest.Insert(Date, True, 0, '', msNew)
        end;
        agmVector:
        begin
          if Abs(x)>1e-6 then
            AggregatedValue := ArcTan(Abs(y/x))/Pi*180
          else
            AggregatedValue := 90;
          if (x<0) and (y>0) then AggregatedValue := 180-AggregatedValue
          else if (x<0) and (y<0) then AggregatedValue := 180+AggregatedValue
          else if (x>0) and (y<0) then AggregatedValue := 360-AggregatedValue;
          AIndex := Dest.Insert(Date, False, AggregatedValue, '', msNew);
        end;
        agmInstant:
        begin
          AggregatedValue := Source.Items[Source.IndexOf(Time)].AsFloat;
          AIndex := Dest.Insert(Date, False, AggregatedValue, '', msNew);
        end;
      else
        AIndex := Dest.Insert(Date, False, AggregatedValue, '', msNew);
      end;
      if MissingValues>0 then Dest[AIndex].SetFlag(MissingFlag, True);
    end;
    if (Method=agmInstant) and (Dest.TimeStep = tstYearly) then
      Time := IncMonth(Time,11);
    Time := Source.TimeStep.IncStep(Time);
    if DiffInSecs(Time, Source.Last.Date)>0 then
      Result := aesEnd
    else
      Result := aesStartCollect;
  end;
end;

resourcestring
  rsInvalidAggregationStep = 'Can only aggregate ten-minute, hourly, daily, '+
    'or monthly time series.';
  rsAggrDestAlreadyContainsInvalidTimeStep = 'The destination time series '+
    'already contains data of a different than the target time step.';
  rsOffsetsDontMatch = 'The specified time offset is not in accordance with '+
    'the source time series'' offset.';
  rsSourceMustHaveStrictTimeStep = 'When aggregating, source time series must '+
    'have strict time step.';

procedure Aggregate(Source, Dest: TTimeseries; TimeOffset: TDateOffset;
  Method: TAggregateMethod; MissingAllowed: Integer; MissingFlag: string;
  Missing: TTimeseries; FromMonth, ToMonth: Integer;
  Options: TAggregateOptions);
var
  DestTimeStep: TTimeStep;
  State: TAggregateState;
  AggregateData: TAggregateData;
begin
  AggregateData.Source := Source;
  AggregateData.Dest := Dest;
  AggregateData.Missing := Missing;
  AggregateData.MissingAllowed := MissingAllowed;
  AggregateData.MissingFlag := MissingFlag;
  AggregateData.TimeOffset := TimeOffset;
  AggregateData.Method := Method;
  AggregateData.FromMonth := FromMonth;
  AggregateData.ToMonth := ToMonth;
  AggregateData.Options := Options;

  { Determine destination time step and source and dest steps in minutes. }
    if Source.TimeStep=tstFiveMinute then
      DestTimeStep := tstTenMinute
    else if Source.TimeStep=tstTenMinute then
      DestTimeStep := tstHourly
    else if Source.TimeStep=tstHourly then
      DestTimeStep := tstDaily
    else if Source.TimeStep=tstDaily then
      DestTimeStep := tstMonthly
    else if Source.TimeStep=tstMonthly then DestTimeStep := tstAnnual
  else
    raise Exception.Create(rsInvalidAggregationStep);

  if Source.TimeStep<tstMonthly then
    AggregateData.SourceStepMinutes := Source.TimeStep.LengthMinutes;
  if DestTimeStep<tstMonthly then
    AggregateData.DestStepMinutes := DestTimeStep.LengthMinutes;

  { Adjust destination time step and similar properties. }
  if (Dest.Count>0) and (Dest.TimeStep<>DestTimeStep) then
    raise Exception.Create(rsAggrDestAlreadyContainsInvalidTimeStep);
  Dest.TimeStep := DestTimeStep;
  Dest.TimeStepStrict := True;
  if (DestTimeStep=tstAnnual) then
    Dest.SetHydrologicalYear(agoHydrologicalYear in Options);
  if (DestTimeStep>=tstMonthly) then
    Dest.DateOffset := TimeOffset.Minutes/1440;

  { Prepare the Missing time series.  }
  if Missing<>nil then
  begin
    Missing.Clear;
    Missing.Precision := 0;
    Missing.TimeStep := DestTimeStep;
    Missing.TimeStepStrict := True;
    if (DestTimeStep=tstAnnual) then
      Missing.SetHydrologicalYear(agoHydrologicalYear in Options);
    if (DestTimeStep>=tstMonthly) then
      Missing.DateOffset := TimeOffset.Minutes/1440;
  end;

  { Check that necessary conditions are met in Source time step and similar
    properties. }
  if (Source.TimeStep<tstMonthly) then
  begin
    if (not Source.TimeStepStrict) then
      raise Exception.Create(rsSourceMustHaveStrictTimeStep);
    if (TimeOffset.Minutes>=0) and (Abs((TimeOffset.Minutes mod
    AggregateData.SourceStepMinutes)/1440-Source.DateOffset)>1/86400) then
      raise Exception.Create(rsOffsetsDontMatch);
    if (TimeOffset.Minutes<0) and
    (Abs((1440+(TimeOffset.Minutes mod AggregateData.SourceStepMinutes))/1440 -
    Source.DateOffset)  >  1/86400) then
      raise Exception.Create(rsOffsetsDontMatch);
    if (TimeOffset.Minutes<0) and (Source.TimeStep<>tstDaily) then
      raise Exception.Create(rsOffsetsDontMatch);
  end;

  { Determine method if default was specified. }
  if Method=agmDefault then
    case Source.VariableType of
      vtInstantaneous, vtAverage: AggregateData.Method := agmAverage;
      vtCumulative:               AggregateData.Method := agmSum;
      vtMaximum:                  AggregateData.Method := agmMaximum;
      vtMinimum:                  AggregateData.Method := agmMinimum;
    else
      Assert(False);
    end;

  { Use state-transition to do the job. }
  AggregateData.Time := Source.First.Date;
  State := aesStartCollect;
  while State<>aesEnd do
    case State of
      aesStartCollect: State := AggregateStartCollect(AggregateData);
      aesMiddleCollect: State := AggregateMiddleCollect(AggregateData);
      aesEndCollect: State := AggregateEndCollect(AggregateData);
    else
      Assert(False);
    end;
end;

function TRegressionResults.GetCoefficientOfCorrelation: Double;
var
  AValue: Real;
  i: Integer;
  AMaxValue: Real;
begin
  AValue := CoefficientOfDetermination;
  if AValue < 0 then AValue := 0;
  Result := Sqrt(AValue);
  AMaxValue := 0;
  for i := 2 to Size do
    if Abs(E[i])>Abs(AMaxValue) then
      AMaxValue := E[i];
  Result := Result * Sign(AMaxValue);
end;

resourcestring
  rsNoTimeseries = 'No one time series specified in order to seek records';

function GetCommonPeriod(TimeseriesList: TObjectList; Month: Integer):
  TDateTimeList;
var
  i, j, k: Integer;
  Dummy1, m, Dummy2: Word;
  FirstTimeseries, ATimeseries: TTimeseries;
  ValueRejected: Boolean;
  ADateTime: TDateTime;
begin
  Assert(TimeseriesList<>nil);
  if TimeseriesList.Count<1 then
    raise Exception.Create(rsNoTimeseries);
  Result := nil;
  try
    Result := TDateTimeList.Create;
    FirstTimeseries := TTimeseries(TimeseriesList[0]);
    for i := 0 to FirstTimeseries.Count-1 do
    begin
      ValueRejected := False;
      if FirstTimeseries[i].IsNull then Continue;
      if Month<>0 then
      begin
        if FirstTimeseries.TimeStep< tstMonthly then
          ADateTime := FirstTimeseries.IntervalMidPoint(FirstTimeseries[i].Date)
        else
          ADateTime := FirstTimeseries[i].Date;
        DecodeDate(ADateTime, Dummy1, m, Dummy2);
        if m<>Month then Continue;
      end;
      for j := 1 to TimeseriesList.Count-1 do
      begin
        ATimeseries := TTimeseries(TimeseriesList[j]);
        k := ATimeseries.IndexOf(FirstTimeseries[i].Date);
        if (k=-1) or ATimeseries[k].IsNull then
        begin
          ValueRejected := True;
          Break;
        end;
      end;
      if not ValueRejected then
        Result.Add(FirstTimeseries[i].Date);
    end;
  except
    Result.Free;
    raise;
  end;
end;

function GetAllRecords(TimeseriesList: TObjectList; Month: Integer):
  TDateTimeList;
var
  i, j: Integer;
  Dummy1, m, Dummy2: Word;
begin
  Assert(TimeseriesList<>nil);
  if TimeseriesList.Count<1 then
    raise Exception.Create(rsNoTimeseries);
  Result := nil;
  try
    Result := TDateTimeList.Create;
    for i := 0 to TimeseriesList.Count-1 do
      with TimeseriesList[i] as TTimeseries do
        for j := 0 to Count-1 do
        begin
          if Month<>0 then
          begin
            DecodeDate(Items[j].Date, Dummy1, m, Dummy2);
            if m<>Month then Continue;
          end;
          if Result.IndexOf(Items[j].Date)>-1 then
            Continue
          else
          Result.InsertSorted(Items[j].Date);
        end;
  except
    Result.Free;
    raise;
  end;
end;

function TimeseriesCovariance(Timeseries1, Timeseries2: TTimeseries;
  var CommonPeriodCardinality: Integer): Real; overload;
var
  ADateTimeList: TDateTimeList;
  BothTimeseries: TObjectList;
begin
  ADateTimeList := nil;
  BothTimeseries :=nil;
  try
    BothTimeseries := TObjectList.Create(False);
    BothTimeseries.Add(Timeseries1);
    BothTimeseries.Add(Timeseries2);
    ADateTimeList := GetCommonPeriod(BothTimeseries, 0);
    Result := TimeseriesCovariance(Timeseries1, Timeseries2, ADateTimeList);
    CommonPeriodCardinality := ADateTimeList.Count;
  finally
    BothTimeseries.Free;
    ADateTimeList.Free;
  end;
end;

function TimeseriesCovariance(Timeseries1, Timeseries2: TTimeseries;
  ADateTimeList: TDateTimeList): Real; overload;
var
  i, j, k: Integer;
  m1, m2: Double;
begin
  Result := 0;
  m1 := Timeseries1.Mean(ADateTimeList);
  m2 := Timeseries2.Mean(ADateTimeList);
  for i := 0 to ADateTimeList.Count-1 do
  begin
    j := Timeseries1.IndexOf(ADateTimeList[i]);
    k := Timeseries2.IndexOf(ADateTimeList[i]);
    Result := Result + ((Timeseries1[j].AsFloat-m1) *
                        (Timeseries2[k].AsFloat-m2));
  end;
  Result := Result/(ADateTimeList.Count-1);
end;

function TimeseriesSumOfProducts(Timeseries1, Timeseries2: TTimeseries;
  var CommonPeriodCardinality: Integer): Real; overload;
var i, k: Integer;
begin
  CommonPeriodCardinality := 0;
  Result := 0;
  for i := 0 to Timeseries1.Count-1 do
    if not Timeseries1[i].IsNull then
    begin
      k := Timeseries2.IndexOf(Timeseries1[i].Date);
      if (k<>-1) and (not Timeseries2[k].IsNull) then
      begin
        Result := Result + (Timeseries1[i].AsFloat * Timeseries2[k].AsFloat);
	      Inc(CommonPeriodCardinality);
      end;
    end;
end;

function TimeseriesSumOfProducts(Timeseries1, Timeseries2: TTimeseries;
  ADateTimeList: TDateTimeList): Real; overload;
var i, j, k: Integer;
begin
  Result := 0;
  for i := 0 to ADateTimeList.Count-1 do
  begin
    j := Timeseries1.IndexOf(ADateTimeList[i]);
    k := Timeseries2.IndexOf(ADateTimeList[i]);
    Result := Result + (Timeseries1[j].AsFloat * Timeseries2[k].AsFloat);
  end;
end;

{ Return a null regression result, with rxy set to 0.
  Function visible only to this unit implementation.
}

function NullRegressionResult(IndependentTimeseries: TObjectList;
  ADateTimeList: TDateTimeList): TRegressionResults;
var
  Cxy: TVector;
  i, m: Integer;
begin
  m := IndependentTimeseries.Count;
  Cxy := nil;
  Result := nil;
  try try
    Cxy := TVector.Create(m+1);
    for i := 1 to m+1 do
      Cxy[i] := 0;
    Result := TRegressionResults.Create;
    TRegressionResults(Result).Assign(Cxy);
    TRegressionResults(Result).CoefficientOfDetermination := 0;
    TRegressionResults(Result).CommonPeriodCardinality := ADateTimeList.Count;
    TRegressionResults(Result).TimeseriesErrorMeanValue := 0;
    TRegressionResults(Result).TimeseriesErrorStandardDeviation := 0;
  except
    Result.Free;
    raise;
  end;
  finally
    Cxy.Free;
  end;
end;

resourcestring
  rsTooFewIndependentTimeseries =
    'Too few independent time series (must be at least 1)';
  rsOrganicAllowedOnlyForOneIndependent =
    'Organic regression allowed only for one independent variable';
  rsTimeSeriesNotOfTheSameTimestep =
    'Time series must be of the same time step';
  rsSeasonalRegressionNotForAnnual =
    'Seasonal regression not allowed for annual time series';
  rsMeanValueOnlyFor =
    'Mean value cannot be used in conjugation with organic, '+
    'zero constant term or autoregression';
  rsOptimizationNotFor =
    'Optimization doesn''t work for Mean value function';

function RegressTimeseries(DependentTimeseries: TTimeseries;
  IndependentTimeseries: TObjectList; Lag: Integer;
  Options: TRegressTimeseriesOptions;
  OptimizationArray: TSeasonalOptimizeRegressionOptions): TObject; overload;
var
  i, b, e, oi: Integer;
  CommonPeriod: TDateTimeList;
  AllTimeseries: TObjectList;
  RegressionResults: TRegressionResults;
  RegressionResultsList: TObjectList;
  ATimeseries: TTimeseries;
begin
  if (rtMeanValue in Options) and
    ((Abs(Lag)>0) or (rtOrganic in Options) or (rtCrossesZero in Options)) then
      raise Exception.Create(rsMeanValueOnlyFor);
  if not (rtOrganic in Options) and
    (IndependentTimeseries.Count+Abs(Lag)<1) and
      not (rtMeanValue in Options) then
    raise Exception.Create(rsTooFewIndependentTimeseries);
  if (rtSeasonal in Options) and not (rtMeanValue in Options) and
    (IndependentTimeseries.Count>0) then
    if TTimeseries(IndependentTimeseries.Items[0]).Timestep=tstAnnual then
      raise Exception.Create(rsSeasonalRegressionNotForAnnual);
  if (rtSeasonal in Options) and (DependentTimeseries.TimeStep=tstAnnual) then
    raise Exception.Create(rsSeasonalRegressionNotForAnnual);
  if (rtOptimize in Options) and (rtMeanValue in Options) then
    raise Exception.Create(rsOptimizationNotFor);
  CommonPeriod := nil;
  ATimeseries := nil;
  AllTimeseries := nil;
  RegressionResults := nil;
  RegressionResultsList := nil;
  Result := nil;
  try
{Add autocorrelations timeseries}
    for i := 0 to Abs(Lag)-1 do
    begin
      ATimeSeries := TTimeseries.Create;
      TimeseriesShiftDates(DependentTimeseries,ATimeseries, i+1);
      ATimeseries.Title := 'Lagged -'+IntToStr(i+1)+' dependent time series';
      IndependentTimeseries.Add(ATimeSeries);
      ATimeSeries := nil;
    end;
    AllTimeseries := TObjectList.Create(False);
    AllTimeseries.Add(DependentTimeseries);
    for i := 0 to IndependentTimeseries.Count-1 do
      AllTimeseries.Add(IndependentTimeseries[i]);
    for i := 0 to AllTimeseries.Count-1 do
      if TTimeseries(AllTimeseries.Items[0]).TimeStep <>
        TTimeseries(AllTimeseries.Items[i]).TimeStep then
        raise Exception.Create(rsTimeSeriesNotOfTheSameTimestep);
    b := 0;
    e := 0;
    if rtSeasonal in Options then
    begin
      RegressionResultsList := TObjectList.Create(True);
      b := 1;
      e := 12;
    end;
    for i := b to e do
    begin
      if e=12 then oi := i-1 else oi := 0;
      CommonPeriod := GetCommonPeriod(AllTimeseries, i);
      try
        RegressionResults :=
          TRegressionResults(RegressTimeseries(DependentTimeseries,
            IndependentTimeseries, CommonPeriod, Options,
            (rtOptimize in Options), OptimizationArray[oi]));
      except
        on EMathError do
          RegressionResults := NullRegressionResult(IndependentTimeseries,
            CommonPeriod);
        else
        raise;
      end;
      CommonPeriod.Free;
      CommonPeriod := nil;
      if rtSeasonal in Options then
        RegressionResultsList.Add(RegressionResults)
      else
        Result := RegressionResults;
      RegressionResults := nil;
    end;
    if rtSeasonal in Options then
      Result := RegressionResultsList;
    RegressionResultsList := nil;
  finally
    ATimeseries.Free;
    AllTimeseries.Free;
    CommonPeriod.Free;
    RegressionResults.Free;
    RegressionResultsList.Free;
  end;
end;

function RegressTimeseries(DependentTimeseries: TTimeseries;
  IndependentTimeseries: TObjectList; ADateTimeList: TDateTimeList;
  Options: TRegressTimeseriesOptions; Optimize: Boolean;
  OptimizationOptions: TOptimizeRegressionOptionsForMonth): TObject;
var
  AInitialSet: TSetOfIntegers;
  ASetOfSets: TSetOfSetsOfIntegers;
  i, j: Integer;
  ASolution, AOptimalSolution: TObject;
  OptimalSetIndex: Integer;
  AOptimalCoef: Real;
  SubIndependentTimeseries: TObjectList;
  ContinueNextSet: Boolean;
  ContainsNegatives: Boolean;
begin
  if not Optimize then
  begin
    Result := RegressTimeseries(DependentTimeseries, IndependentTimeseries,
      ADateTimeList, Options);
    Exit;
  end;
  SetLength(AInitialSet, IndependentTimeseries.Count);
  for i := 0 to Length(AInitialSet)-1 do
    AInitialSet[i] := i;
  ASetOfSets := GetAllSets(AInitialSet);
  SubIndependentTimeseries := nil;
  AOptimalSolution := nil;
  AOptimalCoef := -1e6;
  OptimalSetIndex := -1;
  try
    SubIndependentTimeseries := TObjectList.Create(False);
    for i := 0 to Length(ASetOfSets)-1 do
    begin
      ContinueNextSet :=
        Length(ASetOfSets[i])>OptimizationOptions.MaxTimeseries;
      for j := 0 to Length(OptimizationOptions.TimeseriesSettings)-1 do
        if OptimizationOptions.TimeseriesSettings[j] = oroDoNotUse then
          ContinueNextSet := ContinueNextSet or ElementInSet(j, ASetOfSets[i]);
      for j := 0 to Length(OptimizationOptions.TimeseriesSettings)-1 do
        if OptimizationOptions.TimeseriesSettings[j] = oroUse then
          ContinueNextSet := ContinueNextSet or not (ElementInSet(j,
          ASetOfSets[i]));
      if ContinueNextSet then Continue;
      SubIndependentTimeseries.Clear;
      for j := 0 to Length(ASetOfSets[i])-1 do
        SubIndependentTimeseries.Add(
          IndependentTimeseries.Items[AsetOfSets[i][j]]);
      ASolution := nil;
      try
        ASolution :=
          RegressTimeseries(DependentTimeseries, SubIndependentTimeseries,
          ADateTimeList, Options);
        ContainsNegatives := False;
        if OptimizationOptions.OnlyPositiveCoefficients then
          for j := 2 to TRegressionResults(ASolution).Size do
            if TRegressionResults(ASolution)[j] < 0 then
              ContainsNegatives := ContainsNegatives or True;
        if (TRegressionResults(ASolution).CoefficientOfDetermination>
          AOptimalCoef) and not ContainsNegatives then
        begin
          AOptimalCoef :=
            TRegressionResults(ASolution).CoefficientOfDetermination;
          AOptimalSolution.Free;
          AOptimalSolution := ASolution;
          ASolution := nil;
          OptimalSetIndex := i;
        end;
      finally
        ASolution.Free;
      end;
    end;
    TRegressionResults(Result) := NullRegressionResult(IndependentTimeseries,
      ADateTimeList);
    if AOptimalSolution<>nil then
    begin
      TRegressionResults(Result).CoefficientOfDetermination :=
        TRegressionResults(AOptimalSolution).CoefficientOfDetermination;
      TRegressionResults(Result).CommonPeriodCardinality :=
        TRegressionResults(AOptimalSolution).CommonPeriodCardinality;
      TRegressionResults(Result).TimeseriesErrorMeanValue :=
        TRegressionResults(AOptimalSolution).TimeseriesErrorMeanValue;
      TRegressionResults(Result).TimeseriesErrorStandardDeviation :=
        TRegressionResults(AOptimalSolution).TimeseriesErrorStandardDeviation;
      TRegressionResults(Result).CriticalCoefficientOfDetermination :=
        TRegressionResults(AOptimalSolution).CriticalCoefficientOfDetermination;
      TRegressionResults(Result)[1] := TRegressionResults(AOptimalSolution)[1];
      for i := 0 to Length(ASetOfSets[OptimalSetIndex])-1 do
        TRegressionResults(Result)[ASetOfSets[OptimalSetIndex][i]+2] :=
          TRegressionResults(AOptimalSolution)[i+2];
    end;
  finally
    SubIndependentTimeseries.Free;
    AOptimalSolution.Free;
    ASetOfSets := nil;
    AInitialSet := nil;
  end;
end;

{ The method used by RegressTimeseries below is described in D. Koutsoyiannis,
  Statistiki Ydrologia, Ekdosh 4, Auhna 1997, pp. 207-209. Essentially it says
  that if A = [a0, a1, ..., am] is the resulting vector, where a0 the constant
  term and a1 to am the co-efficients of the dependent time series, then
           A = Inverse(Cxx)*Cxy
  where Cxx is the following (m+1)*(m+1) matrix:
         1     E(x1)    ...  E(xm)
	 E(x1) E(x1*x1)      E(x1*xm)
	 .
	 .
	 .
	 E(xm) E(xm*x1) ...  E(xm*xm)
  where E(x) denotes the mean value of x and E(x*y) denotes the mean sum of
  products of x and y, and x1 to xm denotes the independent timeseries.
  Cxy is the vector [E(y) E(x1*y) ... E(xm*y)] where y the dependent time
  series.
  If CrossesZero is used, that is, the constant term is zero, then the same
  method applies, with the first row and column of Cxx missing and the first
  element of Cxy and A missing.
}
function RegressTimeseries(DependentTimeseries: TTimeseries;
  IndependentTimeseries: TObjectList; ADateTimeList: TDateTimeList;
  Options: TRegressTimeseriesOptions): TObject; overload;
var
  Cxx: TMatrix;
  Cxy: TVector;
  i, m: Integer;
  AMSE, AEMV: Real;
begin
{ check moved here from above in order to optimize for 1 variable...
}
  if (rtOrganic in Options) and (IndependentTimeseries.Count<>1) then
    raise Exception.Create(rsOrganicAllowedOnlyForOneIndependent);
  m := IndependentTimeseries.Count;
{First, consider case with too few common records}
  if ADateTimeList.Count < 2 then
  begin
    Result := NullRegressionResult(IndependentTimeseries, ADateTimeList);
    Exit;
  end;
{Then consider case of organic regression}
  if rtOrganic in Options then
  begin
    Result := OrganicRegressTimeseries(DependentTimeseries,
      IndependentTimeseries,ADateTimeList);
    Exit;
  end;
{Mean value}
  if rtMeanValue in Options then
  begin
    Cxy := nil;
    Result := nil;
    try try
      Cxy := TVector.Create(2);
      Cxy[1] := DependentTimeseries.Mean(ADateTimeList);
      Cxy[2] := 0;
      Result := TRegressionResults.Create;
      TRegressionResults(Result).Assign(Cxy);
      TRegressionResults(Result).CoefficientOfDetermination := 0;
      TRegressionResults(Result).CommonPeriodCardinality := ADateTimeList.Count;
      TRegressionResults(Result).TimeseriesErrorMeanValue := 0;
        TRegressionResults(Result).TimeseriesErrorStandardDeviation := 0;
      TRegressionResults(Result).CriticalCoefficientOfDetermination := 0;
      for i := 0 to ADateTimeList.Count-1 do
      begin
        TRegressionResults(Result).TimeseriesErrorMeanValue :=
          TRegressionResults(Result).TimeseriesErrorMeanValue + Abs( Cxy[1] -
            DependentTimeseries[DependentTimeseries.IndexOf(ADateTimeList[i])].AsFloat);
        TRegressionResults(Result).TimeseriesErrorStandardDeviation :=
          TRegressionResults(Result).TimeseriesErrorStandardDeviation + Sqr( Cxy[1] -
            DependentTimeseries[DependentTimeseries.IndexOf(ADateTimeList[i])].AsFloat);
      end;
      if ADateTimeList.Count>0 then
        TRegressionResults(Result).TimeseriesErrorMeanValue :=
          TRegressionResults(Result).TimeseriesErrorMeanValue /
             ADateTimeList.Count;
      if ADateTimeList.Count>1 then
        TRegressionResults(Result).TimeseriesErrorStandardDeviation :=
          Sqrt(TRegressionResults(Result).TimeseriesErrorStandardDeviation /
             (ADateTimeList.Count-1 ));
      Exit;
    finally
      Cxy.Free;
    end;
    except
      Result.Free;
      raise;
    end;
  end;
{Multiregression}
  Cxx := nil;
  Cxy := nil;
  Result := nil;
  try try
    { Prepare Cxx }
    Cxx := CreateSumOfProductsMatrix(IndependentTimeseries, ADateTimeList);
    Cxx.ScalarMultiply(1/ADateTimeList.Count);
    if not (rtCrossesZero in Options) then
    begin
      Cxx.Resize(m+1, m+1);
      Cxx.Shift(1, 1, m, m, 1, 1);
      Cxx[1, 1] := 1.0;
      for i := 1 to m do
      begin
        Cxx[1, i+1] :=
          TTimeseries(IndependentTimeseries[i-1]).Mean(ADateTimeList);
	Cxx[i+1, 1] := Cxx[1, i+1];
      end;
    end;

    { Prepare Cxy }
    Cxy := TVector.Create(m);
    for i := 1 to m do
      Cxy[i] := TimeseriesSumOfProducts(DependentTimeseries,
        TTimeseries(IndependentTimeseries[i-1]), ADateTimeList);
    Cxy.ScalarMultiply(1/ADateTimeList.Count);
    if not (rtCrossesZero in Options) then
    begin
      Cxy.Resize(m+1);
      Cxy.Shift(1, m, 1);
      Cxy[1] := DependentTimeseries.Mean(ADateTimeList);
    end;

    { Make the calculations }
    Cxx.Invert;
    Cxx.Multiply(Cxy, maRight);
    Result := TRegressionResults.Create;
    TRegressionResults(Result).Assign(Cxx);

    { Add zero constant term if CrossesZero }
    if rtCrossesZero in Options then
      with TRegressionResults(Result) do
      begin
        Resize(m+1);
        Shift(1, m, 1);
        E[1] := 0;
      end;

    { Get statistical indexes. }
    AMSE := TimeseriesMSE(DependentTimeseries, IndependentTimeseries,
      ADateTimeList, TRegressionResults(Result));
    AEMV := TimeseriesErrorMeanValue(DependentTimeseries, IndependentTimeseries,
     ADateTimeList, TRegressionResults(Result));
    TRegressionResults(Result).CoefficientOfDetermination := 1 -
      AMSE /DependentTimeseries.Variance(ADateTimeList)*
        ADateTimeList.Count/(ADateTimeList.Count-1);
    TRegressionResults(Result).CommonPeriodCardinality := ADateTimeList.Count;
    if ADateTimeList.Count > 4 then
      TRegressionResults(Result).CriticalCoefficientOfDetermination :=
        4 / ADateTimeList.Count
    else
      TRegressionResults(Result).CriticalCoefficientOfDetermination := 1;
    TRegressionResults(Result).TimeseriesErrorMeanValue := AEMV;
    TRegressionResults(Result).TimeseriesErrorStandardDeviation :=
      Sqrt(AMSE - SQR(AEMV))*Sqrt(ADateTimeList.Count/(ADateTimeList.Count-1));
  finally
    Cxx.Free;
    Cxy.Free;
  end;
  except
    Result.Free;
    raise;
  end;
end;

function OrganicRegressTimeseries(DependentTimeseries: TTimeseries;
  IndependentTimeseries: TObjectList;ADateTimeList: TDateTimeList): TObject;
var
  rxy,mx,my,sx,sy,sxy: Real;
  AVector: TVector;
  AMSE, AEMV: Real;
begin
  AVector := nil;
  Result := nil;
  if IndependentTimeseries.Count<>1 then
    raise Exception.Create(rsOrganicAllowedOnlyForOneIndependent);
  try try
    AVector := TVector.Create(2);
    mx := TTimeseries(IndependentTimeseries.Items[0]).Mean(ADateTimeList);
    my := DependentTimeseries.Mean(ADateTimeList);
    sx := sqrt(TTimeseries(
      IndependentTimeseries.Items[0]).Variance(ADateTimeList));
    sy := sqrt(DependentTimeseries.Variance(ADateTimeList));
    sxy := TimeseriesCovariance(TTimeseries(IndependentTimeseries.Items[0]),
      DependentTimeseries, ADateTimeList);
    rxy := sxy/(sx*sy);
    AVector[2] := Sign(rxy)*sy/sx;
    AVector[1] := my - AVector[2]*mx;
    Result := TRegressionResults.Create;
    TRegressionResults(Result).Assign(AVector);
    TRegressionResults(Result).CommonPeriodCardinality := ADateTimeList.Count;
    if ADateTimeList.Count > 2 then
      TRegressionResults(Result).CriticalCoefficientOfDetermination :=
        Max(4 / sqrt(ADateTimeList.Count)-1, 0)
    else
      TRegressionResults(Result).CriticalCoefficientOfDetermination := 1;
    AMSE := TimeseriesMSE(DependentTimeseries, IndependentTimeseries,
      ADateTimeList, TRegressionResults(Result));
    AEMV := TimeseriesErrorMeanValue(DependentTimeseries, IndependentTimeseries,
     ADateTimeList, TRegressionResults(Result));
    TRegressionResults(Result).CoefficientOfDetermination := 2*Abs(rxy)-1;
    TRegressionResults(Result).TimeseriesErrorMeanValue := AEMV;
    TRegressionResults(Result).TimeseriesErrorStandardDeviation :=
      Sqrt(AMSE - SQR(AEMV))*Sqrt(ADateTimeList.Count/(ADateTimeList.Count-1));
  finally
    AVector.Free;
  end;
  except
    Result.Free;
    raise;
  end;
end;

function CreateCovarianceMatrix(TimeseriesList: TObjectList;
  ADateTimeList: TDateTimeList): TMatrix;
var i, j, m: Integer;
begin
  Result := nil;
  m := TimeseriesList.Count;
  try
    Result := TMatrix.Create(m, m);
    for i := 1 to m do
      for j := 1 to m do
	Result.E[i, j] := TimeseriesCovariance(TTimeseries(TimeseriesList[i-1]),
          TTimeseries(TimeseriesList[j-1]), ADateTimeList);
  except
    Result.Free;
    raise;
  end;
end;

function CreateSumOfProductsMatrix(TimeseriesList: TObjectList;
  ADateTimeList: TDateTimeList): TMatrix;
var i, j, m: Integer;
begin
  Result := nil;
  m := TimeseriesList.Count;
  try
    Result := TMatrix.Create(m, m);
    for i := 1 to m do
      for j := 1 to m do
	Result.E[i, j] := TimeseriesSumOfProducts
	    (TTimeseries(TimeseriesList[i-1]),
             TTimeseries(TimeseriesList[j-1]), ADateTimeList);
  except
    Result.Free;
    raise;
  end;
end;

function TimeseriesMSE(DependentTimeseries: TTimeseries;
  IndependentTimeseries: TObjectList; CommonPeriod: TDateTimeList;
  Coefficients: TVector): Double;
var
  i, j: Integer;
  Date: TDateTime;
  Estimate, Actual: Double;
begin
  Result := 0;
  for i := 0 to CommonPeriod.Count-1 do
  begin
    Date := CommonPeriod[i];
    Estimate := Coefficients[1];
    for j := 0 to IndependentTimeseries.Count-1 do
      with TTimeseries(IndependentTimeseries[j]) do
        Estimate := Estimate + Coefficients[j+2]*Items[IndexOf(Date)].AsFloat;
    with DependentTimeseries do Actual := Items[IndexOf(Date)].AsFloat;
    Result := Result + Sqr(Actual-Estimate);
  end;
  Result := Result/CommonPeriod.Count;
end;

function TimeseriesErrorMeanValue(DependentTimeseries: TTimeseries;
  IndependentTimeseries: TObjectList; CommonPeriod: TDateTimeList;
  Coefficients: TVector): Double;
var
  i, j: Integer;
  Date: TDateTime;
  Estimate, Actual: Double;
begin
  Result := 0;
  for i := 0 to CommonPeriod.Count-1 do
  begin
    Date := CommonPeriod[i];
    Estimate := Coefficients[1];
    for j := 0 to IndependentTimeseries.Count-1 do
      with TTimeseries(IndependentTimeseries[j]) do
        Estimate := Estimate + Coefficients[j+2]*Items[IndexOf(Date)].AsFloat;
    with DependentTimeseries do Actual := Items[IndexOf(Date)].AsFloat;
    Result := Result + Actual-Estimate;
  end;
  Result := Result/CommonPeriod.Count;
end;

procedure TimeseriesLinComb(DependentTimeseries: TTimeseries;
  IndependentTimeseries: TObjectList; Coefficients: TVector);
var CommonPeriod: TDateTimeList;
begin
  CommonPeriod := nil;
  try
    CommonPeriod := GetCommonPeriod(IndependentTimeseries, 0);
    TimeseriesLinComb(DependentTimeseries, IndependentTimeseries, CommonPeriod,
      Coefficients);
  finally
    CommonPeriod.Free;
  end;
end;

procedure TimeseriesLinComb(DependentTimeseries: TTimeseries;
  IndependentTimeseries: TObjectList; ADateTimeList: TDateTimeList;
  Coefficients: TVector);
var
  i, j: Integer;
  Sum: Real;
  ADateTime: TDateTime;
begin
  if IndependentTimeseries.Count<1 then Exit;
  { Prepare some properties of DependentTimeseries. }
  DependentTimeseries.Clear;
  DependentTimeseries.AssignMeta(TTimeseries(IndependentTimeseries[0]));

  { Do the job for each entry in the date list. }
  for i := 0 to ADateTimeList.Count-1 do
  begin
    ADateTime := ADateTimeList[i];
    Sum := Coefficients[1];
    for j := IndependentTimeseries.Count-1 downto 0 do
      with TTimeseries(IndependentTimeseries[j]) do
        Sum := Sum + Coefficients[j+2]*Items[IndexOf(ADateTime)].AsFloat;
    DependentTimeseries.Insert(ADateTime, False, Sum, '', msNew);
  end;
end;

procedure TimeseriesFillMissing(IndependentTimeseries: TObjectList;
  Destination:TTimeseries; RegressionResults: TObject;
  Options: TRegressTimeseriesOptions);

  procedure FillMeanValue(i, j: Integer; AMeanValue: Real);
  var
    Dummy1, m, Dummy2: Word;
  begin
    DecodeDate(Destination[j].Date, Dummy1, m, Dummy2);
    if (m=i) or (i=0) then
    begin
      Destination[j].AsFloat := AMeanValue;
      Destination[j].MStatus := msNew;
    end;
  end;

var
  ATimeseries: TTimeseries;
  CommonPeriod: TDateTimeList;
  AStartDate, AEndDate: TDateTime;
  AllTimeseries: TObjectList;
  ARegressionResults: TObject;
  i,b,e,j: Integer;
  AVector: TVector;
  AIndex: Integer;
begin
  ATimeseries := nil;
  AllTimeseries := nil;
  CommonPeriod := nil;
  AVector := nil;
  AStartDate := Destination.First.Date;
  AEndDate := Destination.Last.Date;
  try
    if rtRandomSeed in Options then
      Randomize else
      RandSeed := 0;
    AVector := TVector.Create;
    ATimeseries := TTimeseries.Create;
    AllTimeseries := TObjectList.Create(False);
    for i := 0 to IndependentTimeseries.Count-1 do
      AllTimeseries.Add(IndependentTimeseries[i]);
    b := 0;
    e := 0;
    if rtSeasonal in Options then
    begin
      b := 1;
      e := 12;
    end;
    for i := b to e do
    begin
      if i = 0 then
        ARegressionResults := RegressionResults else
        ARegressionResults := TObjectList(RegressionResults).Items[i-1];
      AVector.Assign(TPersistent(ARegressionResults));
      CommonPeriod := GetCommonPeriod(AllTimeseries, i);
      if (TRegressionResults(ARegressionResults).CoefficientOfDetermination
        = 0) and not (rtMeanValue in options) then Continue;
      TimeseriesLinComb(ATimeseries, IndependentTimeseries,
        CommonPeriod, AVector);
      if (rtDoExtendBefore in Options) or (rtDoExtendAfter in Options) then
      begin
        for j := 0 to CommonPeriod.Count-1 do
        begin
          if rtDoExtendBefore in Options then
            if DiffInSecs(CommonPeriod[j],AStartDate)<0 then
              if Destination.IndexOf(CommonPeriod[j])<0 then
                Destination.Insert(CommonPeriod[j], True, 0, '', msNew);
          if rtDoExtendAfter in Options then
            if DiffInSecs(CommonPeriod[j],AEndDate)>0 then
              if Destination.IndexOf(CommonPeriod[j])<0 then
                Destination.Insert(CommonPeriod[j], True, 0, '', msNew);
        end;
      end;
      for j := 0 to Destination.Count-1 do
      begin
        if (rtDoExtendBefore in Options) or (rtDoExtendAfter in Options) then
          if rtDonotFillInnerValues in Options then
            if (DiffInSecs(Destination[j].Date,AStartDate)>=0) and
              (DiffInSecs(Destination[j].Date,AEndDate)<=0) then
                Continue;
        if Destination[j].IsNull then
        begin
          AIndex := ATimeseries.IndexOf(Destination[j].Date);
          if (AIndex >= 0) or (rtMeanValue in Options) then
          begin
            if not (rtMeanValue in Options) then
              Destination[j].AsFloat := ATimeseries[AIndex].AsFloat
            else
              FillMeanValue(i, j, TRegressionResults(ARegressionResults)[1]);
            Destination[j].SetFlag('INFILLING',True);
            if rtRandomTerm in Options then
              if not Destination[j].IsNull then
                Destination[j].AsFloat := Destination[j].AsFloat +
                  nrnd(
                  TRegressionResults(ARegressionResults).TimeseriesErrorMeanValue,
                    TRegressionResults(ARegressionResults).TimeseriesErrorStandardDeviation);
            if rtTruncToZero in Options then
              if not Destination[j].IsNull then
                if Destination[j].AsFloat < 0 then
                  Destination[j].AsFloat := 0;
          end;
        end;
      end;
      CommonPeriod.Free;
      CommonPeriod := nil;
    end;
  finally
    ATimeseries.Free;
    CommonPeriod.Free;
    AllTimeseries.Free;
    AVector.Free;
  end;
end;

resourcestring
  rsTimeseriesShiftNotAllowedOnVariableTimeStep =
    'Time series date shift is not allowed on variable or unknown time step';

procedure TimeseriesShiftDates(Source, Destination: TTimeseries;
  ShiftBy: Integer);
var
  ADate: TDateTime;
  IsNull, IsStrict: Boolean;
  AValue: Real;
  AMStatus: TMStatus;
  i: integer;
  AFlagString: string;
begin
  if Source.Timestep.TimeStepIn([tstUnknown, tstVariable]) then
    raise Exception.Create(rsTimeseriesShiftNotAllowedOnVariableTimeStep);
  Destination.AssignMeta(Source);
  IsStrict := Source.TimeStepStrict;
  for i := 0 to Source.Count-1 do
  begin
    IsNull := Source[i].IsNull;
    if not IsNull then
      AValue := Source[i].AsFloat else
      AValue := 0;
    ADate := Source[i].Date;
    ADate := Source.TimeStep.IncStep(ADate, ShiftBy);
    AMStatus := Source[i].MStatus;
    AFlagString := Source[i].GetAllFlags;
    Destination.Add(ADate, IsNull, AValue, AFlagString, AMStatus);
  end;
  Destination.TimeStepStrict := IsStrict;
end;

resourcestring
  rsImpossibleIntegration =
    'Impossible to integrate this time series';
  rsOnlyYearlyOrMonthlyTimeInterval =
    'Integration is allowed on a montly or yearly time interval';

procedure TimeseriesIntegrate(Source, Destination: TTimeseries;TimeUnit: Real;
  TimeInterval: TTimeStep;Denominator: Real;AverageValue: Boolean);
var
  ATimeSeries, ATimeSeries2: TTimeseries;
  ADate, StartDate, EndDate, AFirstDate, ALastDate: TDateTime;
  LastDay, AMonth, AYear: Integer;
  i: Integer;
  AValue: Real;
begin
  ATimeSeries := nil;
  ATimeSeries2 := nil;
  try
  if TimeInterval = tstAnnual then TimeInterval := tstYearly; //Useless!
  if (TimeInterval<>tstYearly) and (TimeInterval<>tstMonthly) then
    raise Exception.Create(rsOnlyYearlyOrMonthlyTimeInterval);
  ATimeSeries2 := TTimeSeries.Create;
  ATimeSeries2.Assign(Source);
  i := 0;
{First deletes any Null records}
  while i<ATimeSeries2.Count do
  begin
    if ATimeSeries2.Items[i].IsNull then
    begin
      ATimeSeries2.Delete(i);
      Dec(i);
    end;
    Inc(i);
  end;
  ATimeSeries := TTimeSeries.Create;
  ATimeSeries.Assign(ATimeSeries2);
{To set a trapezoid, two bases are required!}
  if ATimeSeries.Count < 2 then
    raise Exception.Create(rsImpossibleIntegration);
  StartDate := ATimeSeries.First.Date;
  EndDate := ATimeSeries.Last.Date;
{Initialize Integration timeseries}
  for i := 0 to ATimeSeries.Count-1 do
    ATimeSeries.Items[i].AsFloat := 0;
{Evaluate Trapezoids areas, sumarize them}
  for i := 0 to ATimeSeries.Count-2 do
  begin
    ATimeSeries.Items[i+1].AsFloat := ATimeSeries.Items[i].AsFloat +
      (ATimeSeries2.Items[i+1].AsFloat + ATimeSeries2.Items[i].AsFloat) * 0.5 *
      DiffInSecs(ATimeSeries2.Items[i+1].Date, ATimeSeries2.Items[i].Date)/TimeUnit;
  end;
{ADate is the main time parameter for the loop preceding}
{Find Year and Month for the StartDate}
  AYear := StrToInt(FormatDateTime('yyyy',StartDate));
  AMonth := StrToInt(FormatDateTime('m',StartDate));
{Initialize ADate to the 1st of the month /year}
  if TimeInterval = tstMonthly then
    ADate := FormatStrToDateTime('yyyy/mm/dd hh:nn:ss',
      IntToStr(AYear)+'/'+IntToStr(AMonth)+'/'+'01 00:00:00')
  else
    ADate := FormatStrToDateTime('yyyy/mm/dd hh:nn:ss',
        IntToStr(AYear)+'/01/01 00:00:00');
{The main loop}
  while DiffInSecs(ADate, EndDate)<=0 do
  begin
{Current year,month}
    AYear := StrToInt(FormatDateTime('yyyy',ADate));
    AMonth := StrToInt(FormatDateTime('m',ADate));
{Find last day of the month}
    case Amonth of
      1,3,5,7,8,10,12: LastDay := 31;
      4,6,9,11: LastDay := 30;
    else
      LastDay := 28;
    end;
{  Leap? Feb28->Feb29}
    if IsLeapYear(AYear) and (AMonth = 2) then
      inc(LastDay);
{Express First and Last days of the month as a TTimeDate...}
    if TimeInterval = tstMonthly then
    begin
      AFirstDate := FormatStrToDateTime('yyyy/mm/dd hh:nn:ss',
        IntToStr(AYear)+'/'+IntToStr(AMonth)+'/'+'01 00:00:00');
      ALastDate := FormatStrToDateTime('yyyy/mm/dd hh:nn:ss',
        IntToStr(AYear)+'/'+IntToStr(AMonth)+'/'+IntToStr(LastDay)+' 23:59:59');
    end else begin
{...or Express First and Last days of the year as a TTimeDate}
      AFirstDate := FormatStrToDateTime('yyyy/mm/dd hh:nn:ss',
        IntToStr(AYear)+'/01/01 00:00:00');
      ALastDate := FormatStrToDateTime('yyyy/mm/dd hh:nn:ss',
        IntToStr(AYear)+'/12/31 23:59:59');
    end;
    for i := 1 to ATimeSeries.Count-1 do
    begin
      if DiffInSecs(ALastDate, ATimeSeries.Items[i].Date)<0 then
      begin
        if DiffInSecs(ALastDate, EndDate)<=0 then
        begin
{Linear interpolation}
          AValue := ATimeSeries.Items[i-1].AsFloat +
            (ATimeSeries.Items[i].AsFloat-ATimeSeries.Items[i-1].AsFloat) *
            DiffInSecs(ALastDate,ATimeSeries.Items[i-1].Date) /
            DiffInSecs(ATimeSeries.Items[i].Date,ATimeSeries.Items[i-1].Date);
          Destination.Insert(AFirstDate, False, AValue, '', msNew);
          break;
        end;
      end
      else begin
{  If last month/year, consider last record }
        if DiffInSecs(ALastDate, EndDate)>0 then
        begin
          AValue := ATimeSeries.Last.AsFloat;
          Destination.Insert(AFirstDate, False, AValue, '', msNew);
          break;
        end;
      end;
    end;
{Consider next month or next year...}
    if TimeInterval = tstMonthly then
      ADate := IncMonth(ADate,1)
    else begin
      Inc(AYear);
      ADate := FormatStrToDateTime('yyyy/mm/dd hh:nn:ss',
        IntToStr(AYear)+'/01/01 00:00:00');
    end;
  end;
  ATimeSeries.Assign(Destination);
  for i := 1 to Destination.Count-1 do
{Make substractions to calculate final monthly/yearly values}
  begin
    Destination.Items[i].AsFloat := Destination.Items[i].AsFloat -
    ATimeSeries.Items[i-1].AsFloat;
  end;
{Divide resulting timeseries by the denominator}
  for i := 0 to Destination.Count-1 do
  begin
    Destination.Items[i].AsFloat := Destination.Items[i].AsFloat/Denominator;
  end;
{Find Average values on demand}
  if AverageValue then
  begin
    for i := 0 to Destination.Count-1 do
    begin
      ADate := Destination.Items[i].Date;
      AYear := StrToInt(FormatDateTime('yyyy',ADate));
      AMonth := StrToInt(FormatDateTime('m',ADate));
      AFirstDate := ADate;
      case Amonth of
        1,3,5,7,8,10,12: LastDay := 31;
        4,6,9,11: LastDay := 30;
      else
        LastDay := 28;
      end;
{  Leap? Feb28->Feb29}
      if IsLeapYear(AYear) and (AMonth = 2) then
        inc(LastDay);
      if TimeInterval = tstMonthly then
      begin
        ALastDate := FormatStrToDateTime('yyyy/mm/dd hh:nn:ss',
         IntToStr(AYear)+'/'+IntToStr(AMonth)+'/'+IntToStr(LastDay)+' 23:59:59');
      end else begin
        ALastDate := FormatStrToDateTime('yyyy/mm/dd hh:nn:ss',
          IntToStr(AYear)+'/12/31 23:59:59');
      end;
      if i = 0 then AFirstDate := StartDate;
      if i = Destination.Count-1 then ALastDate := EndDate;
      Destination.Items[i].AsFloat :=
        Destination.Items[i].AsFloat * TimeUnit /
        DiffInSecs(ALastDate,AFirstDate);
    end;
  end;
  finally
    ATimeSeries.Free;
    ATimeSeries2.Free;
  end;
end;

resourcestring
  rsDataShouldBeTenMinuteHourlyDaily =
    'Source time series time step should be less or equal than daily';
  rsDataShouldBeTimeStepStrict =
    'Source time series must have time step strict';
  rsMultiplierShouldBeGreaterThanZero =
    'Multiplier should be integer greater than zero';

procedure TimeseriesToSingleIDF(Source, Dest, MissingPercent, DayTS: TTimeseries;
  Multiplier: Integer; AnalysisTimestep: TTimestep; Options: TIDFOptions;
  MissingFlag, MarginalFlag: string);
var
  FirstDateTime, ADateTime, ANextDateTime, CandidateDate: TDateTime;
  AMonth, AYear, ADay: Word;
  CandidateValue, AValue: Real;
  AMissingFlag, CandidateMissingFlag: Boolean;
  AMarginalFlag, CandidateMarginalFlag: Boolean;
  TotalNumber, MissingNumber: Integer;
  i,j: Integer;
  BaseDivider: Real;
begin
  Assert(AnalysisTimestep>=tstMonthly);
  if Source.TimeStep>tstDaily then
    raise Exception.Create(rsDataShouldBeTenMinuteHourlyDaily);
  if not Source.TimeStepStrict then
    raise Exception.Create(rsDataShouldBeTimeStepStrict);
  if Multiplier<1 then
    raise Exception.Create(rsMultiplierShouldBeGreaterThanZero);
  if (1440 mod Source.TimeStep.LengthMinutes)<>0 then
    raise Exception.Create('Invalid timestep... should be@#$@#$#^$');
  FirstDateTime := Source.First.Date;
  DecodeDate(FirstDateTime, AYear, AMonth, ADay);
  if AnalysisTimestep = tstMonthly then
  begin
    FirstDateTime := EncodeDate(AYear, AMonth, 1);
  end else begin
    if idfoHydrologicalYear in Options then
      FirstDateTime := EncodeDate(FindHydrologicalYear(FirstDateTime),10,1)
    else
      FirstDateTime := EncodeDate(AYear, 1, 1);
  end;
//  BaseDivider := 1;
  BaseDivider := Source.TimeStep.LengthMinutes/60;
{  case Source.TimeStep of
    tstFiveMinute: BaseDivider := 1/12;
    tstTenMinute: BaseDivider := 1/6;
    tstHourly: BaseDivider := 1;
    tstDaily: BaseDivider := 24;
    else Assert(False);
  end;}
  Dest.Clear;
  if MissingPercent<>nil then MissingPercent.Clear;
  if DayTS<>nil then DayTS.Clear;
  ADateTime := FirstDateTime;
  if AnalysisTimestep = tstMonthly then
    ANextDateTime := IncMonth(ADateTime, 1) else
    ANextDateTime := IncMonth(ADateTime, 12);
  CandidateValue := 0;
  CandidateDate := 0;
  CandidateMissingFlag := False;
  CandidateMarginalFlag := False;
  MissingNumber := 0;
  i := 0;
  while True do
  begin
    if i>Source.Count-1 then
      Break;
    AValue := 0;
    AMissingFlag := False;
    for j := i to i + Multiplier - 1 do
    begin
      if j>Source.Count-1 then
      begin
        AMissingFlag := True;
        Break;
      end;
      if Source[j].IsNull and (not (idfoAllowMissing in Options)) then
      begin
        AValue := 0;
        Break;
      end;
      if not Source[j].IsNull then
        AValue := AValue + Source[j].AsFloat
      else
        AMissingFlag := True;
    end;
{Check for marginal values, set flag respectively}
    AMarginalFlag := False;
    if (i>0) then
      if Source[i-1].IsNull then
        AMarginalFlag := True;
    if i + Multiplier - 1 < Source.Count - 1 then
      if Source[i + Multiplier].IsNull then
        AMarginalFlag := True;
    if AValue>CandidateValue then
    begin
      CandidateValue := AValue;
      CandidateDate := Source[i].Date;
      CandidateMissingFlag := AMissingFlag;
      CandidateMarginalFlag := AMarginalFlag;
    end;
    if (i>=0) and (i<Source.Count) then
      if not Source[i].IsNull then
        Inc(MissingNumber);
    Inc(i);
    if (i>Source.Count-1) or (DiffInSecs(Source[i].Date,ANextDateTime)>=0) then
    begin
      if idfoIntensity in Options then
        CandidateValue := CandidateValue / (Multiplier * BaseDivider);
      Dest.Add(ADateTime, False, CandidateValue, '', msNew);
      if DayTS<>nil then
        if CandidateValue>0 then
        begin
          if AnalysisTimestep = tstMonthly then
            DayTS.Add(ADateTime, False, DayOfTheMonth(CandidateDate), '', msNew)
          else
            DayTS.Add(ADateTime, False, DayOfTheYear(CandidateDate), '', msNew);
        end else
          DayTS.Add(ADateTime, True, 0, '', msNew);
      if CandidateMissingFlag then
        Dest.Last.SetFlag(MissingFlag,True);
      if CandidateMarginalFlag then
        Dest.Last.SetFlag(MarginalFlag,True);
      if AnalysisTimestep = tstAnnual then
        if idfoHydrologicalYear in Options then
          if IsLeapYear(YearOf(ADateTime)+1) then
            TotalNumber := 366 else TotalNumber := 365
        else
            TotalNumber := DaysInYear(ADateTime)
      else
        TotalNumber := DaysInMonth(ADateTime);
      TotalNumber := (TotalNumber*1440) div Source.TimeStep.LengthMinutes;
{      case Source.TimeStep of
        tstFiveMinute: TotalNumber := TotalNumber * 288;
        tstTenMinute: TotalNumber := TotalNumber * 144;
        tstHourly: TotalNumber := TotalNumber * 24;
        tstDaily: TotalNumber := TotalNumber;
      else
        Assert(False);
      end;}
      if MissingPercent<>nil then
        if TotalNumber > 0 then
          MissingPercent.Add(ADateTime, False, 100 - 100*MissingNumber/TotalNumber,
            '', msNew);
      MissingNumber := 0;
      CandidateValue := 0;
      ADateTime := ANextDateTime;
      ANextDateTime := AnalysisTimestep.IncStep(ADateTime);
{      if AnalysisTimestep = tstMonthly then
        ANextDateTime := IncMonth(ADateTime, 1) else
        ANextDateTime := IncMonth(ADateTime, 12);}
    end;
  end;
end;

resourcestring
  rsTimestepShouldBeStrict = 'Time series should be of strict time step';
  rsCouldNotDisaggregate =
    'Could not disaggregate time series of five-minutes or variable time step';

procedure TimeseriesDisaggregate(Source, Dest: TTimeseries;
  Method: TDisaggregateMethod);
begin
  TimeseriesDisaggregate(Source, Dest, Method, tdrmUniform);
end;

procedure TimeseriesDisaggregate(Source, Dest: TTimeseries;
  Method: TDisaggregateMethod; RandomModel: TDisaggregateRandomModel);

  function NumberOfSteps(ADate: TDateTime; SourceTimestep: TTimeStep): Integer;
  begin
    Result := -1;
    //case SourceTimestep of
    if  SourceTimestep=tstTenMinute then Result := 2
    else if  SourceTimestep=tstHourly then Result := 6
    else if  SourceTimestep=tstDaily then Result := 24
    else if  SourceTimestep=tstMonthly then Result := DaysInMonth(ADate)
    else if  SourceTimestep=tstAnnual then Result := 12
    else
      Assert(False);
//    end;
  end;

  function FindFirstDate(ADate: TDateTime; SourceTimestep: TTimeStep): TDateTime;
  begin
    Result := idaEmpty;
//    case SourceTimestep of
    if SourceTimestep=tstAnnual then Result := ADate
    else if SourceTimestep=tstMonthly then Result := AddDateTime(ADate, 1)
    else if SourceTimestep=tstDaily then Result := SubtractDateTime(ADate, 23/24)
    else if SourceTimestep=tstHourly then Result := SubtractDateTime(ADate, -(5/6)*(1/24))
    else if SourceTimestep=tstTenMinute then Result := SubtractDateTime(ADate, -1/288)
    else
      Assert(False);
//    end;
  end;

var
  i, j, ACount, AIndex, a, b: Integer;
  ADate, AValue, ASum: Real;
  AFlagString: string;

begin
  Assert(Source<>nil);
  Assert(Dest<>nil);
  if not Source.TimeStepStrict then
    raise Exception.Create(rsTimestepShouldBeStrict);
  if (Source.Timestep.TimeStepIn([tstFiveMinute, tstUnknown, tstVariable])) then
    raise Exception.Create(rsCouldNotDisaggregate);
  Dest.Clear;
//  case Source.TimeStep of
    if Source.TimeStep=tstAnnual then Dest.TimeStep := tstMonthly
    else if Source.TimeStep=tstMonthly then Dest.TimeStep := tstDaily
    else if Source.TimeStep=tstDaily then Dest.TimeStep := tstHourly
    else if Source.TimeStep=tstHourly then Dest.TimeStep := tstTenMinute
    else if Source.TimeStep=tstTenMinute then Dest.TimeStep := tstFiveMinute
  else
    Assert(False, 'Non implemented yet for this time step');
//  end;
  case Method of
    tdoAverageConstant, tdoAverageRandom: Dest.VariableType := vtAverage;
    tdoCumulativeConstant, tdoCumulativeRandom: Dest.VariableType :=
      vtCumulative;
  else
    Assert(False);
  end;
  Dest.TimeStepStrict := True;
  RandSeed := 1973;
  if not Source.DateOffsetUnspecified then
    if Source.TimeStep = tstAnnual then
      Dest.DateOffset := Source.DateOffset;
  for i := 0 to Source.Count-1 do
  begin
    ADate := FindFirstDate(Source[i].Date, Source.TimeStep);
    AFlagString := Source[i].GetAllFlags;
    if not Source.DateOffsetUnspecified then
      if Source.TimeStep = tstMonthly then
        ADate := AddDateTime(ADate, Source.DateOffset);
    ACount := NumberOfSteps(Source[i].Date, Source.TimeStep);
    a := -1;
    b := -1;
    for j := 0 to ACount-1 do
    begin
      AIndex := Dest.Add(ADate, True, 0, AFlagString, msNew);
      if j=0 then a := AIndex;
      if j=(ACount-1) then b := AIndex;
//      case Dest.TimeStep of
      if Dest.TimeStep=tstMonthly then ADate := IncMonth(ADate, 1)
        else if Dest.TimeStep=tstDaily then ADate := AddDateTime(ADate, 1)
        else if Dest.TimeStep=tstHourly then ADate := AddDateTime(ADate, 1/24)
        else if Dest.TimeStep=tstTenMinute then ADate := AddDateTime(ADate, 1/144)
        else if Dest.TimeStep=tstFiveMinute then ADate := AddDateTime(ADate, 1/288)
      else
        Assert(False);
//      end;
    end;
    if not Source[i].IsNull then
    begin
      AValue := Source[i].AsFloat;
      ASum := 0;
      j := a;
      while j<=b do
      begin
        if Method in [tdoAverageConstant, tdoCumulativeConstant] then
          Dest[j].AsFloat := 1
        else if Method in [tdoAverageRandom, tdoCumulativeRandom] then
        begin
          Dest[j].AsFloat := Random;
          case RandomModel of
            tdrmUniform: Dest[j].AsFloat := Dest[j].AsFloat;
            tdrmExponential: Dest[j].AsFloat := Exp(-1*Dest[j].AsFloat) -
              Exp(-1);
            tdrmLogarithmic: Dest[j].AsFloat := -1*Ln((Dest[j].AsFloat+1e-8));
            tdrmQuadric: Dest[j].AsFloat := Sqr(Dest[j].AsFloat);
            tdrmHighOrder: Dest[j].AsFloat := Power(Dest[j].AsFloat, 12);
          else
            Assert(False);
          end;
        end
        else
          Assert(False);
        ASum := ASum + Dest[j].AsFloat;
        Inc(j);
      end;
      j := a;
      if Method in [tdoAverageConstant, tdoAverageRandom] then
        ASum := ASum / ACount;
      while j<=b do
      begin
        Dest[j].AsFloat := Dest[j].AsFloat * AValue / ASum;
        Inc(j);
      end;
    end;
  end;
end;

end.
