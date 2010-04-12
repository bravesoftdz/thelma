{******************************************************************}
{                                                                  }
{  Thelma library                                                  }
{                                                                  }
{  Copyright (c) 2000-09 National Technical University of Athens   }
{                                                                  }
{******************************************************************}

{** Classes for the manipulation of time series. }
unit ts;

interface

uses Classes, SysUtils, Dates, Contnrs, istrutils, GenUtils;

const
  StandardFlags = 'RANGE SPATIAL TEMPORAL INTERNAL ESTIMATED '+
    'SUSPECT SNOW ICE FROST DIVE OVERFLOW PUMP LOGOVERRUN '+
    'LOGNOISY LOGOUTSIDE LOGRANGE HOMOGEN INFILLING PENMAN '+
    'DATEINSERT INCONSISTENT AUTO MISSING';

const
  OldFlagsMeta = 'COMMENT=1 RANGE=1 SPATIAL=1 TEMPORAL=1 INTERNAL=1 '+
    'ESTIMATED=1 SUSPECT=1 SNOW=1 ICE=1 FROST=1 DIVE=1 OVERFLOW=1 PUMP=1 '+
    'LOGOVERRUN=1 LOGNOISY=1 LOGOUTSIDE=1 LOGRANGE=1 HOMOGEN=1 INFILLING=1 '+
    'PENMAN=1 DATEINSERT=1 INCONSISTENT=1 AUTO=1 MISSING=1 USER0=1 USER1=1 '+
    'USER2=1 DUMMY2BIT=2';

const

  {VariableType, defined as in database. }
  vtInstantaneous = 1;
  vtCumulative = 2;
  vtAverage = 3;
  vtMaximum = 4;
  vtMinimum = 5;
  vtStdev = 6;
  vtVectorAverage = 7;
  vtUnknown = 0;

type
  TAggregationOptionsRec = record
    MissingAllowed: Integer;
    MissingFlag: string;
    DummyRun: Boolean;
    DeleteNullEnds: Boolean;
    CalcMissingSeries: Boolean;
    SeasonalAggregation: Boolean;
    FromMonth, FromDay, ToMonth, ToDay: Integer;
  end;

type
  TArrayOfInt = array of Integer;

  TDateOffset = record
    private
      FMinutes, FMonths: Integer;
    public
      constructor Create(AMinutes, AMonths: Integer);
      procedure Store(AMinutes, AMonths: Integer);
      property Minutes: Integer read FMinutes write FMinutes;
      property Months: Integer read FMonths write FMonths;
{** The = (equality) operator. Timestep a=b.
    a=b only when (a.minutes=b.minutes) and (a.months=b.months).
}
      class operator Equal(a: TDateOffset; b: TDateOffset): Boolean;
{** The <> (inequal) operator. Timestep a<>b.
    a<>b := not (a=b)
}
      class operator NotEqual(a: TDateOffset; b: TDateOffset): Boolean;
  end;

{** The Timestep type holding time step length for time series.
    Timestep type is of "advanced record" thus a record type with properties
    and methods like a class. In addition operator overloading is added for
    comparing several time steps.
}
  TTimestep = record
    private
      FLengthMinutes, FLengthMonths: Integer;
      FOrdinalValue: Integer;
      function GetIsVariable: Boolean;
    public
{** Length of time step in minutes a read only property.
}
      property LengthMinutes: Integer read FLengthMinutes;
{** Length of time step in months a read only property.
}
      property LengthMonths: Integer read FLengthMonths;
{** An ordinal type for backwards compatibility with older time series formats.
    It has the value of 0 for unknown time step and 7 for variable time step.
}
      property OrdinalValue: Integer read FOrdinalValue write FOrdinalValue;
{** IsVariable is True when Length in Minutes and Length in Monhts are zero.
    In other languages timestep should be nil when time step is variable, in
    delphi this is done with LengthMinutes=LengthMonths=0.
}
      property IsVariable: Boolean read GetIsVariable;
{** The = (equality) operator. Timestep a=b.
    a=b only when (a.minutes=b.minutes) and (a.months=b.months).
}
      class operator Equal(a: TTimestep; b: TTimestep): Boolean;
{** The <> (inequal) operator. Timestep a<>b.
    a<>b := not (a=b)
}
      class operator NotEqual(a: TTimestep; b: TTimestep): Boolean;
{** The > (greater than) operator for a>b.
    When a.LenghMonths>0 and b.LengthMonths=0 then alwayis a>b.
    Or else a>b when a.months>b.months or when a.minutes>b.minutes
}
      class operator GreaterThan(a: TTimestep; b: TTimestep): Boolean;
{** The >= operator := (a>b) or (a=b)
}
      class operator GreaterThanOrEqual(a: TTimestep; b: TTimestep): Boolean;
{** The < operator := not (a>=b)
}
      class operator LessThan(a: TTimestep; b: TTimestep): Boolean;
{** The <= operator := not (a>b)
}
      class operator LessThanOrEqual(a: TTimestep; b: TTimestep): Boolean;
{** The record constructor. Call ATimestep := TTimestep.Create(..).
    When ALengthMinutes>0 and ALengthMonhts>0 an exception is raised, one
    of them should be zero. AOrdinalValue is kept for backwards compatibility
    and it is used mainly for creating predefined time steps compatible with
    the older version. So, use the overloaded constructor.
}
      constructor Create(ALengthMinutes, ALengthMonths,
        AOrdinalValue: Integer); overload;
{** This is an overloaded version of Create for main use.
    When use this version of Create, OrdinalValue is set to zero.
}
      constructor Create(ALengthMinutes, ALengthMonths: Integer); overload;
{** Check if the timestep is in a given set of timesteps.
    e.g. if TimeStepIn([tstMonhtly,tstAnnual])... for
    if TimeStep in [tstMonthly, tstAnnual] in older versions.
}
      function TimeStepIn(ASetOfTimesteps: array of TTimestep): Boolean;
{** Returns a value of DateTime by increasing ADatimeTime with the current
    time step.
}
      function IncStep(ADateTime: TDateTime): TDateTime; overload;
{** Returns a value of DateTime by increasing ADatimeTime with the current
    time step, IncBy times.
}
      function IncStep(ADateTime: TDateTime; IncBy: Integer):
        TDateTime; overload;
{** Returns a value of DateTime by decreasing ADatimeTime with the current
    time step.
}
      function DecStep(ADateTime: TDateTime): TDateTime; overload;
{** Returns a value of DateTime by decreasing ADatimeTime with the current
    time step, DecBy times.
}
      function DecStep(ADateTime: TDateTime; DecBy: Integer):
        TDateTime; overload;
    end;

var
{** Predifined time steps.
    These time steps are initialized in the initialization section of the
    ts unit.
}
  tstFiveMinute, tstVariable, tstTenMinute, tstHourly, tstDaily, tstMonthly,
    tstFithteenMinute, tstTwentyMinute, tstHalfHour, tstThreeHour,
    tstFourHour, tstSixHour, tstEightHour, tstTwelveHour,
    tstThreeMonth, tstFourMonth, tstSixMonth, tstTwoYear, tstFiveYear,
    tstTenYear, tstYearly, tstAnnual, tstUnknown, tstTwoHour,
    tstTwoMonth: TTimestep;
{** Array holding predifined time steps.
    Between positions 0 and 7, the time steps of older versions are kept.
}
  AvailableTimesteps: array[0..24] of TTimestep;

const
  {** The default precision value when the timeseries is created or loaded from
      a database where precision is not defined.
      @SeeAlso <See Property=TTimeseries.Precision>
  }
  tsDefaultPrecision = 2;

{** Converts a TVariableType to a string.
    This function is mainly used internally, for example when writing time
    series to a stream.
    @SeeAlso <See Routine=StrToVarType>
    @SeeAlso <See Method=TTimeseries.WriteMeta>
}
function VarTypeToStr(Value: Integer): string;

{** Converts a string to a TVariableType.
    This function is mainly used internally, for example when loading time
    series from a stream. For example, StrToVarType will convert 'Instantaneous'
    to vtInstantaneous. It is case-insensitive. If it cannot recognize the
    string, it raises an exception.
    @SeeAlso <See Routine=VarTypeToStr>
    @SeeAlso <See Method=TTimeseries.ReadMeta>
}
function StrToVarType(AString: string): Integer;



type

  {** Exception raised when attempting to read a null value.
      @author A.X.
      @SeeAlso <See Property=TTsRecord.AsFloat>
      @SeeAlso <See Property=TTsRecord.AsInteger>
      @SeeAlso <See Property=TTsRecord.Null>
  }
  EValueIsNull = class(EConvertError);

  {** Exception raised when attempting to insert into a timeseries a record
      in a existing record with the same date
      @author Stefanos
      @SeeAlso <See Method=TTimeseries.Insert>
  }
  ERecordAlreadyExists = class(Exception);

  { TTsRecord }

  {** Used by TTsRecord.MStatus
      @SeeAlso <See Property=TTsRecord.MStatus> }
  TMStatus = (msUnmodified, msModified, msNew);
  {** Used by TTsRecord.Status
      @SeeAlso <See Property=TTsRecord.Status> }
  TStatus = Integer;
  TTimeseries = class;

  {** Stores a time series record.
      TTsRecord is used by TTimeseries, and specifically by the Items
      array; Items is an array of TTsRecord objects.
      @author A.X.
      @SeeAlso <See Class=TTimeseries>
      @SeeAlso <See Property=TTimeseries.Items>
  }
  TTsRecord = class(TPersistent)
  private
    FOwner: TTimeseries;
    FDate: TDateTime;
    FMStatus: TMStatus;
    FStatus: TStatus;
    FValue: Single;
    function GetValue: Double;
    procedure SetValue(Value: Double);
    function GetAsInteger: Integer;
    procedure SetAsInteger(Value: Integer);
    function GetAsString: string;
    procedure SetAsString(Value: string);
    function GetDateAsString: string;
    procedure SetDate(Value: TDateTime);
    procedure SetMStatus(Value: TMStatus);
  protected
    FIsNull: Boolean;
  public
    {** The record's date and time.
        Time series records time stamps are stored as moments in time, although
        in many cases they actually refer to intervals. It is only raw
        measurements of instantaneous variables that occur in moments in time.
        In most other cases, including cumulative variables and aggregated
        instantaneous variables, the time series record refers to a time
        interval.<p>
        For time series with daily or smaller time step, Date always stores the
        end of the interval. This technique is common practice.  For example, on
        a daily time series resulting from aggregation, a hydrologist would
        understand '2002-03-08 08:00' as referring to the interval that
        begins on 2002-03-07 08:00 and ends on 2002-03-08 08:00.<p>
        For monthly time series, the day of month and time have no meaning, and
        TTsRecord requires that the day be 1 and the time be 0; thus, March 2002
        is stored as 2002-03-01 00:00.<p>
        For annual time series, only the year has meaning. TTsRecord requires
        that the day of month be 1, the time be 0, and the month be 10 for the
        hydrological year and 1 otherwise; thus, year 2002 is stored as
        2002-01-01 00:00, whereas hydrological year 2002-03 is stored
        as 2002-10-01 00:00.<p>
        The user should not be shown full dates when viewing monthly or annual
        time series. Use the DateAsString property to get a decent string to
        show to the user.<p>
        You can't set the date of a TTsRecord object which belongs to a time
        series.<p>
        @SeeAlso <See Property=DateAsString>
        @SeeAlso <See Property=TTimeseries.DateOffset>
        @SeeAlso <See Method=SortingDate>
    }
    property Date: TDateTime read FDate write SetDate;
    {** Return the record's date and time as a string.
        Read AsString to read the record's date converted to a string.<p>
        For time series with daily or smaller time step, the string is of the
        form YYYY-MM-DD HH:mm. For monthly time series, the string is of the
        form YYYY-MM. For yearly time series, the string is of the form YYYY-YY
        if Owner.HydrologicalYear is True, or YYYY otherwise.
        @SeeAlso <See Property=Date>
        @SeeAlso <See Property=TTimeseries.HydrologicalYear>
    }
    property DateAsString: string read GetDateAsString;
    {** Returns the date transformed for sorting into a TTimeseriesGrid.
        SortingDate is used internally by TTimeseriesGrid.<p>
        For time steps less than monthly, SortingDate returns Date. For monthly
        and annual time steps, SortingDate returns the date according to which
        the record should be sorted; for example, if a monthly time series is
        shown together with a daily time series, the monthly value should be
        shown after all daily values for that month. The value returned by
        SortingDate should be used only for sorting records and never for
        displaying the value itself.<p>
        For monthly time series, if UseDateOffset is False or the owner's
        DateOffsetUnspecified is True, SortingDate returns
        30 seconds LESS than the beginning of next month, for example if Date
        is 01/03/2002 00:00, then SortingDate will return 31/03/2002 23:59:30.
        If UseDateOffset is True and the owner's DateOffsetUnspecified is False,
        then SortingDate returns 30 seconds MORE than the beginning of next
        month plus the owner's DateOffset; for example, if Date is
        01/03/2002 00:00 and the owner's DateOffset is 8 hours, SortingDate will
        return 01/04/2002 08:00:30. UseDateOffset should be set to True when
        viewing monthly time series together with smaller time steps; if,
        however, many monthly time series are shown together, UseDateOffset
        should be set to False, so that the same months are shown in the same
        row.<p>
        For annual time series, SortingDate returns 15 seconds less than the
        beginning of next year or next hydrological year, depending on whether
        the Owner's HydrologicalYear property is True. If UseDateOffset is
        True and the owner's DateOffsetUnspecified is False, then the owner's
        DateOffset is added to the beginning of the next year or hydrological
        year and then 15 seconds are added.<p>
        @SeeAlso <See Property=Date>
        @SeeAlso <See Class=TTimeseriesGrid>
        @SeeAlso <See Property=TTimeseries.TimeStep>
        @SeeAlso <See Property=TTimeseries.HydrologicalYear>
    }
    function SortingDate(UseDateOffset: Boolean): TDateTime;
    {** Specifies whether the record has been modified since loading from the
        database or file.
        MStatus is automatically set to msUnmodified when the record is read
        from the database or a file and to msNew when it is new (e.g. entered
        by the user or calculated). An msUnmodified record which is modified
        becomes msModified. }
    property MStatus: TMStatus read FMStatus write SetMStatus;
    {** Specifies whether the record contains a null (missing) value.
        If IsNull is True, then AsFloat and AsInteger are meaningless and an
        attempt to read them will result in EValueIsNull being raised.<P>
        To set IsNull to True, use SetNull. To set IsNull to False, assign a
        value to AsFloat or AsInteger.
        @SeeAlso <See Property=AsFloat>
        @SeeAlso <See Property=AsInteger>
        @SeeAlso <See Method=SetNull>
        @SeeAlso <See Class=EValueIsNull> }
    property IsNull: Boolean read FIsNull;
    {** The record's value as a Double number.
        An attempt to read the value of a null record will result in
        EValueIsNull being raised.<p>
        Writing AsFloat automatically sets IsNull to False and
        TTimeseries.Modified of the owner to True.
        @SeeAlso <See Property=AsInteger>
        @SeeAlso <See Property=AsString>
        @SeeAlso <See Class=EValueIsNull>
        @SeeAlso <See Property=IsNull>
        @SeeAlso <See Property=TTimeseries.Modified> }
    property AsFloat: Double read GetValue write SetValue;
    {** The record's value as an Integer.
        An attempt to read the value of a null record will result in
        EValueIsNull being raised.<p>
        Writing AsInteger automatically sets IsNull to False and
        TTimeseries.Modified of the owner to True.
        @SeeAlso <See Property=AsFloat>
        @SeeAlso <See Property=AsString>
        @SeeAlso <See Class=EValueIsNull>
        @SeeAlso <See Property=IsNull>
        @SeeAlso <See Property=TTimeseries.Modified> }
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    {** The record's value as a string.
        Read AsString to read the record's value converted to a string, or
        an empty string if it is null. Assign an empty string to AsString
        to set IsNull to true and TTimeseries.Modified of the owner to
        True. Assign a non-empty string to AsString to set the record's value
        as specified, IsNull to false, and TTimeseries.Modified of the
        owner to True.
        @SeeAlso <See Property=AsFloat>
        @SeeAlso <See Property=AsInteger>
        @SeeAlso <See Property=IsNull>
        @SeeAlso <See Property=TTimeseries.Modified>
    }
    property AsString: string read GetAsString write SetAsString;
    {** Creates a TTsRecord object and sets its properties to specific values.
        Owner specifies the TTimeseries object to which the record
        belongs. For objects not belonging to a TTimeseries object and
        being used temporarily, Owner must be nil.<p>
        Normally you don't need to create TTsRecord objects; they are
        automatically created when you add or insert records in time series.
        @SeeAlso <See Method=TTimeseries.Add>
        @SeeAlso <See Method=TTimeseries.Insert>
        @SeeAlso <See Method=TTimeseries.Items> }
    constructor Create(AOwner: TTimeseries; Date: TDateTime; IsNull: Boolean;
      Value: Real; FlagString: string; MStatus: TMStatus);
    {** Copies a TTsRecord object.
        @SeeAlso <Jump File=Delphi5.hlp K="TPersistent,Assign" Text=Assign>
    }
    procedure Assign(Source: TPersistent); override;
    {** Specifies that this record is a null (missing) value.
        SetNull sets IsNull to True and TTimeseries.Modified of the owner
        to True. It is not possible to set IsNull to False directly; IsNull is
        automatically set to False whenever AsInteger or AsFloat are assigned a
        value.
        @SeeAlso <See Property=IsNull>
        @SeeAlso <See Property=AsInteger>
        @SeeAlso <See Property=AsFloat>
        @SeeAlso <See Property=TTimeseries.Modified> }
    procedure SetNull;
    {** Retrieves the value of a specific flag.
        GetFlag uses the Status property and the Flags property of TTimeseries
        to determine the value of the specified flag, and returns this value. If
        no such flag is defined in TTimeseries.Flags, GetFlag raises an
        exception.
        @SeeAlso <See Property=Status>
        @SeeAlso <See Property=TTimeseries.Flags>
        @SeeAlso <See Method=SetFlag>
        @SeeAlso <See Method=GetAllFlags>
        @SeeAlso <See Method=SetAllFlags>
    }
    function GetFlag(Flag: string): Boolean;
    {** Sets the value of a flag.
        SetFlag alters the Status property (according to the Flags property of
        TTimeseries) so that the specified Flag is assigned the specified
        Value. If no such flag is defined in TTimeseries.Flags, GetFlag
        raises an exception.
        @SeeAlso <See Property=Status>
        @SeeAlso <See Property=TTimeseries.Flags>
        @SeeAlso <See Method=GetFlag>
        @SeeAlso <See Method=GetAllFlags>
        @SeeAlso <See Method=SetAllFlags>
    }
    procedure SetFlag(Flag: string; Value: Boolean);
    {** Returns a string which specifies which flags are set.
        Use GetAllFlags when you need to show to the user which flags are set
        for a specified record.<p>
        GetAllFlags reads the Status property and determines the values of all
        flags (according to the Flags property of TTimeseries). It returns a
        string containing a description of the flag settings, namely:
        <ul>
          <li>Flags with a value of zero do not appear in the string.
          <li>Flags with a value of 1 appear with their name only, not their
          value, if they occupy one bit only.
          <li>Flags occupying more than one bit and having a nonzero value
          appear in the form FLAG=value.
        </ul><p>
        The flags are delimited by a space.
        @SeeAlso <See Property=Status>
        @SeeAlso <See Property=TTimeseries.Flags>
        @SeeAlso <See Method=GetFlag>
        @SeeAlso <See Method=SetFlag>
        @SeeAlso <See Method=SetAllFlags>
    }
    function GetAllFlags: string; overload;
    {** Resets and sets all flags.
        This version of GetAllFlags uses the specified Delimiter instead of a
        space for separating the flags.
    }
    function GetAllFlags(Delimiter: Char): string; overload;
    {** Resets and sets all flags.
        Use SetAllFlags when you need to reset Status entirely.<p>
        SetAllFlags sets Status as specified in the Flags string, which is must
        be a space-delimited string of FLAG=value pairs and/or FLAG symbols.
        Specifically:
        <ul>
          <li>Flags that do not appear in the string are assigned zero.
          <li>Flags in the form FLAG=value are assigned the specified value.
          <li>Flags in the form FLAG must be one-bit, or an exception is raised,
          and they are assigned the value 1.
        </ul>
        @SeeAlso <See Property=Status>
        @SeeAlso <See Property=TTimeseries.Flags>
        @SeeAlso <See Method=GetFlag>
        @SeeAlso <See Method=SetFlag>
        @SeeAlso <See Method=GetAllFlags>
    }
    procedure SetAllFlags(Flags: string); overload;
    {** Resets and sets all flags.
        This version of SetAllFlags accepts Flags delimited by Delimiter
        instead of a space.
    }
    procedure SetAllFlags(Flags: string; Delimiter: Char); overload;
  end;

  { TTimeseries }


  {** Used by TTimeseries.VariableType.
      @SeeAlso <See Property=TTimeseries.VariableType> }
  TVariableType = 0..7;

  {** A method type for methods that update a progress indicator.
      Some methods, such as TTimeseries.WriteData, take a long
      time to execute. If passed a TProgressIndicator method as an
      argument, they periodically call it, so that the called method updates
      some visual progress indicator.<p>
      A TProgressIndicator method accepts two arguments: Completed is the
      number of completed items so far, and Total is the number of total items.
      The method must be able to handle the extreme case where Total is zero.
      @SeeAlso <See Method=TTimeseries.ReadData>
      @SeeAlso <See Method=TTimeseries.WriteData>
  }
  TProgressIndicator = procedure (Completed, Total: Integer) of object;

  {** Used internally to hold list of flags. }
  TFlag = record
    Flag: string;
    nbits: Integer;
  end;

  TArrayOfMethod = array of TMethod;
  TMethodPtr = procedure of object;

  {** Stores a time series.
      TTimeseries is used for time series. It has properties and methods
      for manipulating time series of all types and all time steps, and methods
      for reading and writing time series to databases and files.<P>
  }
  TTimeseries = class(TPersistent)
  private
    Fid: Integer;
    FName: string;
    FRecordList: TObjectList;
    FTimeStep: TTimeStep;
    FVariable: Integer;
    FVariableType: TVariableType;
    FModified: Boolean;
    FHydrologicalYear: Boolean;
    FNominalOffset: TDateOffset;
    FActualOffset: TDateOffset;
    FDateOffsetUnspecified: Boolean;
    FTimeStepStrict: Boolean;
    FStatisticsBgColor: Boolean;
    FComment: string;
    FTitle: string;
    FMUnit: string;
    FGTypeName, FGentityName, FTypeName, FVarName, FTimeStepName: string;
    FPrecision: Integer;
    FFileName: string;
    FFileVersion: Integer;
    FTimeZone: string;
    FFlagStringList: TStringList;
    FFlagUnsortedList: TIntegerList;
    function GetCount: Integer;
    function GetDateOffset: Real;
    function GetFlagsUsed: string;
    function GetSelectionFlags: string;
    procedure SetDateOffset(Value: Real);
    procedure SetTimeStepStrict(Value: Boolean);
    procedure SetTimeStep(Value: TTimeStep);
    function GetHydrologicalYear: Boolean;
    procedure SetVariableType(Value: TVariableType);
    procedure SetModified(Value: Boolean);
    procedure SetTitle(Value: string);
    procedure SetPrecision(Value: integer);
    procedure SetNominalOffset(AOffset: TDateOffset);
    function GetNominalOffset: TDateOffset;
  protected
    function Get(Index: Integer): TTsRecord;
    {** Fully invalidates all data grids that display the time series.
        Do not use DisplayFullInvalidate; it is used internally.
        @SeeAlso <See Method=DisplayInvalidate>
        @SeeAlso <See Method=TTimeseriesGrid.FullInvalidate>
    }
    procedure DisplayFullInvalidate;
    {** Invalidates all data grids that display the time series.
        Do not use DisplayInvalidate; it is used internally.
        @SeeAlso <See Method=DisplayFullInvalidate>
    }
    procedure DisplayInvalidate;
    {** Used internally to adjust DateOffset.
        If the TimeStep is tstMonthly or tstAnnual or tstVariable or tstUnknown,
        does nothing. Otherwise,
        if Count is zero or TimeStepStrict is False, sets DateOffsetUnspecified
        to True. Otherwise, sets DateOffset according to the first record of
        the time series (without checking whether the rest of the records
        agree).<p>
        Only run AdjustDateOffset when needed, as it contains some overhead.
        For example, call it when an empty time series gets its first element,
        or when a time series becomes empty.<p>
        @SeeAlso <See Property=DateOffset>
    }
    procedure AdjustDateOffset; overload;
    procedure AdjustDateOffset(Nonstrict: Boolean); overload;
  public
    {** Keeps a list of "Invalidate" methods to be called whenever the
        timeseries changes.
        You normally don't need to use Invalidators; it is used internally by
        TTimeseries and TTimeseriesGrid. When a TTimeseries object is displayed
        on a TTimeseriesGrid, the grid adds its "Invalidate" method to the
        Invalidators list. Whenever the time series is updated, it calls
        all Invalidators.
    }
    Invalidators: TArrayOfMethod;
    {** Keeps a list of "FullInvalidate" methods to be called whenever the
        timeseries changes.
        You normally don't need to use FullInvalidators; it is used internally
        by TTimeseries and TTimeseriesGrid. When a TTimeseries object is
        displayed on a TTimeseriesGrid, the grid adds its "FullInvalidate"
        method to the FullInvalidators list. Whenever the time series is updated
        so that the number of records chagnes, it calls all FullInvalidators.
    }
    FullInvalidators: TArrayOfMethod;
    {**
    }
    function AddToFlagList(FlagString: string): Integer;
    {**
    }
    function GetFromFlagList(AIndex: Integer): string;
    {** Return the first nominal timestamp that is equal or later than
        timestamp.
    }
    function UpTimestamp(ADateTime: TDateTime; Direction: Integer=1): TDateTime;
    {** Return the last nominal timestamp that is equal or earlier than
        timestamp.
    }
    function DownTimestamp(ADateTime: TDateTime): TDateTime;
    {** Return the next nominal timestamp.
    }
    function NextTimestamp(ADateTime: TDateTime): TDateTime;
    {** Return the previous nominal timestamp.
    }
    function PreviousTimestamp(ADateTime: TDateTime): TDateTime;
    {** Return the actual timestamp that corresponds to the specified nominal
        timestamp.
    }
    function ActualTimestamp(ADateTime: TDateTime): TDateTime;
    {** This function assumes that the timeseries is an interval, even if
        interval_type  is None. It returns the nominal timestamp that denotes
        the interval that contains the specified moment.
    }
    function ContainingInterval(ADateTime: TDateTime): TDateTime;
    {** Return, as a tuple, the two actual timestamps of the interval that has
        the specified nominal timestamp.
    }
    function IntervalStartPoint(ANominalTimestamp: TDateTime): TDateTime;
    {** Return, as a tuple, the two actual timestamps of the interval that has
        the specified nominal timestamp.
    }
    function IntervalEndPoint(ANominalTimestamp: TDateTime): TDateTime;
    {** Return, as a tuple, the two actual timestamps of the interval that has
        the specified nominal timestamp.
    }
    function IntervalMidPoint(ANominalTimestamp: TDateTime): TDateTime;
    {**
    }
    property FlagsUsed: string read GetFlagsUsed;
    {**
    }
    property SelectionFlags: string read GetSelectionFlags;
    {** Name field of a timeseries record stored into the database.
    }
    property Name: string read FName write FName;

    property GTypeName: string read FGTypeName write FGTypeName;
    property GentityName: string read FGentityName write FGentityName;
    property TypeName: string read FTypeName write FTypeName;
    property VarName: string read FVarName write FVarName;
    property TimeStepName: string read FTimeStepName write FTimeStepName;

    {** The number of data in the time series.
	Count is the number of entries in the Items array.
	@SeeAlso <See Method=CountNotNull>
    }
    property Count: Integer read GetCount;
    {** Time series data.
	Items is an array of TTsRecord objects, each of which holds a record.<p>
        Normally you don't need to use Items; you can merely write
        TimeseriesObject[i]. }
    property Items[Index: Integer]: TTsRecord read Get; default;
    {** Specifies whether the data or other properties have been modified since
        loaded.
        If Modified is False, then the time series has not been changed since it
        was read from the data base or the file. Modified becomes True
        automatically whenever the data	or properties are modified.<p>
        When setting Modified to False, then the MStatus of all data is set to
        msUnmodified.
        @SeeAlso <See Property=TTsRecord.MStatus>
    }
    property Modified: Boolean read FModified write SetModified;
    {** The time step of the time series.
        Besides TimeStep, important information on the time step can be found in
        the properties in the links at "See Also".
        @SeeAlso <See Property=HydrologicalYear>
        @SeeAlso <See Property=DateOffset>
        @SeeAlso <See Property=TimeStepStrict>
        @SeeAlso <See Property=TTsRecord.Date>
    }
    property TimeStep: TTimeStep read FTimeStep write SetTimeStep;
    {** Specifies whether an annual time series uses the hydrological year.
        Read HydrologicalYear to determine whether a year YYYY refers to the
        time period YYYY-01-01 to YYYY-12-31 or to the time period YYYY-10-01
        to (YYYY+1)-09-30.
        This property is kept for compatibility reasons.
        @SeeAlso <See Property=TimeStep>
        @SeeAlso <See Property=DateOffset>
        @SeeAlso <See Property=TTsRecord.Date>
        @SeeAlso <See Property=TTsRecord.DateAsString>
        @SeeAlso <See Routine=SetHydrologicalYear>
    }
    property HydrologicalYear: Boolean read GetHydrologicalYear;
    {** Specifies the offset of the time stamps.
        This property is kept only for compatibility reasons and has not its
        own storage. It reads / sets values from NominalOffset and ActualOFfset.
        It is used as an interface to open / write files of older versions.
        Old help text follows:
        When the time step is constant, the time stamps of all records have the
        same remainder when divided with the time step. For example, in a daily
        time series, all time stamps of a daily time series may be at midnight,
        in which case the offset is zero, or they may be at 08:00, in which case
        the offset is 8 hours. DateOffset specifies this offset, in number of
        days. For ten-minute time series, the points of zero offset are the
        multiples of ten minutes (:10, :20, etc.); for hourly time-series, the
        points of zero offset are the exact hours.<p>
        The date offset can be unspecified. In that case, the
        DateOffsetUnspecified property is True and an attempt to read DateOffset
        will raise an exception.<p>
        For ten-minute, hourly, and daily time series, the DateOffset cannot be
        assigned (an attempt to assign it will raise an exception). For these
        time steps, the time stamps are accurate to the minute, thus the
        DateOffset can be determined just by looking at a time stamp. In fact,
        the TTimeseries object automatically determines the DateOffset when the
        first record of the time series is added, thereafter DateOffset being
        used internally for error checking every time another record is added.
        An attempt to add a record which has a different date offset will result
        in an exception. When the time series does not contain any records, the
        date offset is unspecified, otherwise it is zero or positive. All this
        applies only if the time step is strict, as specified by the
        TimeStepStrict property.<p>
        For monthly time series, only the month and year are used in the time
        stamps, the day, hour and minute being by convention 1, 0 and 0
        respectively, as described in TTsRecord.Date. In this case, DateOffset
        can be zero, positive, or negative, and it specifies the offset from the
        end of the month at which the aggregation period ends. For example, if
        DateOffset is 2 days and 13 hours, then a time stamp of March 2002
        refers to the period beginning at 2002-03-02 13:00 and ending at
        2002-04-02 13:00; if DateOffset is minus 2 days and 13 hours, then
        a time stamp of March 2002 refers to the period beginning at
        2002-02-26 11:00 and ending at 2002-03-29 11:00.<p>
        For annual time series, things are essentially the same as for monthly
        time series; the DateOffset can be zero, positive, or negative, and it
        specifies the offset from the end of year at which the aggregation
        period ends. The end of the year is midnight of 1st of January of next
        year or midnight of 1st of October of next year, depending on whether
        HydrologicalYear is set.<p>
        @SeeAlso <See Property=DateOffsetUnspecified>
        @SeeAlso <See Property=TimeStep>
        @SeeAlso <See Property=HydrologicalYear>
        @SeeAlso <See Property=TTsRecord.Date>
        @SeeAlso <See Property=TimeStepStrict>
    }
    property DateOffset: Real read GetDateOffset write SetDateOffset;
    {** A pair of integers indicating the number of minutes and months that must
        be added to a round timestamp to get to the nominal timestamp.
        For example, if an hourly time series has timestamps that end in :13,
        such as 01:13, 02:13, etc., then its nominal offset is 13 minutes,
        0 months, i.e., (13, 0). Monthly time series normally have a nominal
        timestamp of (0, 0), the timestamps usually being of the form
        2008-02-01 00:00, meaning “February 2008” and usually rendered by
        application software as 2008-02. Annual timestamps have a nominal
        timestamp which normally has 0 minutes, but may have nonzero months;
        for example, a common offset in Greece is 9 months, which means that an
        annual timestamp is of the form 2008-10-01 00:00, normally rendered by
        application software as 2008-2009, and denoting the hydrological year
        2008-2009.
    }
    property NominalOffset: TDateOffset read GetNominalOffset
      write SetNominalOffset;
    {** A pair of integers indicating the number of minutes and months that must
        be added to the nominal timestamp to get to the actual timestamp.
        Note the difference from nominal_offset, which is the offset from the
        round timestamp; the actual_offset must be added to the nominal offset
        to find the actual offset from the round timestamp.
        Actual offset for small time steps, such as up to daily,
        is usually zero, except if the nominal timestamp is the beginning of
        an interval, in which case the actual offset is equal to the length of
        the time step, so that the actual timestamp is the end of the interval.
        For monthly and annual time steps, the actual_offset is usually 1 and 12
        months respectively. For a monthly time series, an actual_offset of
        (-475, 1) means that 2003-11-01 00:00 (normally rendered as 2003-11)
        denotes the interval 2003-10-31 18:05 to 2003-11-30 18:05.
    }
    property ActualOffset: TDateOffset read FActualOffset
      write FActualOffset;
    {** Specifies whether the date offset is specified.
        This property is kept only for compatibility reasons to read and write
        time series files with older format.
        For more information on DateOffsetUnspecified, see the DateOffset
        property.
        @SeeAlso <See Property=DateOffset>
    }
    property DateOffsetUnspecified: Boolean read FDateOffsetUnspecified
      write FDateOffsetUnspecified;
    {** Specifies whether the time step has irregularities.
        Time steps smaller than monthly may have irregularities. For example, in
        a daily time series measurement time may normally be 08:00 but
        occasionally a record may have a time stamp at 10:00; in a ten-minute
        time series the offset may be zero, as :10, :20, :30, etc., and then
        change to one, like :41, :51, :01, :11, etc. When irregularities are
        allowed, the TimeStepStrict property should be set to False.<p>
        When TimeStepStrict is False, the date offset is always unspecified,
        and the time step is merely informative. When TimeStepStrict is
        True, the date offset is automatically determined as specified in the
        DateOffset property, and the TTimeseries object makes checks to ensure
        the regularity of the step, for example by raising an exception when
        attempting to add a record with a date offset different from
        DateOffset. Setting TimeStepStrict to True when it was False causes a
        check of the entire time series; if an irregularity is found, an
        exception is raised and TimeStepStrict remains False.<p>
        For monthly and annual time series, TimeStepStrict is always True
        and an attempt to set it to False will raise an exception.<p>
        @SeeAlso <See Property=DateOffset>
        @SeeAlso <See Property=TimeStep>
        @SeeAlso <See Property=TTsRecord.Date>
    }
    property TimeStepStrict: Boolean read FTimeStepStrict
      write SetTimeStepStrict;
    {** Returns True if can set time step strict to True.
    }
    function CheckTimeStepStrict(var ErrMessage: string): Boolean;
    {** Not used yet. }
    property Variable: Integer read FVariable write FVariable;
    {** Specifies whether the variable is instantaneous, cumulative, average,
        maximum or minimum. }
    property VariableType: TVariableType read FVariableType
      write SetVariableType;
    {** Holds the time series database id.
        If the time series has been loaded from or written to the database, id
        holds the id used in the last such operation. You cannot set id;
        @SeeAlso <See Method=SetId>
    }
    property id: Integer read Fid;
    {** Holds the number of decimal digits that the timeseries' values will
        have when displaying.
        Precision is automatically set when timeseries is loaded from database.
        If the value of precision is null in the
        database then a default value of <See Const=tsDefaultPrecision> is used.
        The same value is used when the TTimeseries object is created.
        The maximum value for precision is 18 decimals (actually that's the
        limit of Delphi's Format function). If a value greater than 18 is
        specified, precision will be set to 18.
        @SeeAlso <See Const=tsDefaultPrecision>
    }
    property Precision: Integer read FPrecision write SetPrecision;
    {** Adds a record to the end of the time series.
        Add adds a record to the end of the Items array, and should be used only
        when the data are ordered; otherwise Insert should be used. If IsNull is
        True, Value is ignored.<p>
        @SeeAlso <See Method=Insert>
    }
    function Add(Date: TDateTime; IsNull: Boolean; Value: Real;
      FlagString: string; MStatus: TMStatus): Integer; virtual;
    {** Specifies a comment for displaying in a TTimeseriesGrid.
        The primary purpose of Comment is to help the user remember
        which time series is which when viewing multiple time series in a
        TTimeseriesGrid. Time series are displayed in grids using a short title,
        like an id or a file name; when the user moves the mouse over the title,
        the series' display comment appears in a help hint. New TTimeseries
        objects have an empty Comment; TDBTimeseries sets a default
        Comment based on the time series' meta-data; TTimeseries stored
        in files also have their comment stored in the files. The user can
        change the comment in the TTimeseriesPropertiesDialog.
        @SeeAlso <See Property=Title>
        @SeeAlso <See Class=TTimeseriesGrid>
        @SeeAlso <See Class=TDBTimeseries>
        @SeeAlso <See Class=TTimeseriesPropertiesDialog>
    }
    property Comment: string read FComment write FComment;
    {** Specifies a title for displaying in a TTimeseriesGrid.
        Use Title to specify the short title displayed as a heading in
        a TTimeseriesGrid. New TTimeseries objects have an empty Title;
        TDBTimeseries sets the id as the default Title; TTimeseries
        stored in files have their title stored in the files. The user can
        change the title in the TTimeseriesPropertiesDialog.
        @SeeAlso <See Property=Comment>
        @SeeAlso <See Class=TTimeseriesGrid>
        @SeeAlso <See Class=TDBTimeseries>
        @SeeAlso <See Class=TTimeseriesPropertiesDialog>
    }
    property Title: string read FTitle write SetTitle;
    {** Specifies the unit of measurement.
        Use MUnit to specify the measurement unit.
    }
    property MUnit: string read FMUnit write FMUnit;
    {** Used to store file name when opening or saving a time series from / to
        file.
        Application shoud use this property as appropriate in order to display
        correctly save or save as dialogs, query user to store a time series
        when closing application, etc.
    }
    property FileName: string read FFilename write FFilename;
    {** Used to hold the file version of an opened time series.
        Default value is 2 for a new time series. If a time series is
        read from file of version=1 and user request save, then the
        interface should bring a dialog prompting user for saving to
        new version..
    }
    property FileVersion: Integer read FFileVersion write FFileVersion;
    {** Used to store the Time Zone of the timestamps as a string.
        The time zone of the timestamps, in the format XXX (UTC+HHmm),
        where XXX is a time zone name and +HHmm is the offset from UTC.
        Examples are EET (UTC+0200) and VST (UTC-0430).
    }
    property TimeZone: string read FTimeZone write FTimeZone;
    {** Deletes all records from the Items array. }
    procedure Clear;
    {** Creates a TTimeseries object.
    }
    constructor Create; overload;
    {** Copies a TTimeseries object.
        When copying a TTimeseries object, the destination TimeseriesGrids
        property is left unchanged. Filename and FileVersion are not copied.
        @SeeAlso <See Property=TimeseriesGrids>
        @SeeAlso <See Method=AssignMeta>
        @SeeAlso <Jump File=Delphi5.hlp K="TPersistent,Assign" Text=Assign>
    }
    procedure Assign(Source: TPersistent); override;
    {** Copies a TTimeseries object except for the records.
        AssignMeta is like Assign, but it copies only the meta-data, not the
        Items array; the destination Items array is left unchanged.
        @SeeAlso <See Method=Assign>
    }
    procedure AssignMeta(Source: TTimeseries); virtual;
    {** Compares the Meta section of ATimeseries with Current Timeseries.
        Compares the Meta section of ATimeseries with Current Timeseries.
        Returns True if Timeseries are differing.
    }
    function CompareMeta(ATimeseries: TTimeseries): Boolean;
    {** Deletes a record from the time series.
        Delete deletes an entry from the Items array. }
    procedure Delete(Index: Integer);
    {** Destroys a TTimeseries object.
        Do not call Destroy directly. Call Free instead. Free checks that the
        object reference is not nil before calling Destroy. }
    destructor Destroy; override;
    {** Returns Items[0]. }
    function First: TTsRecord;
    {** Returns number of not-null items.
    }
    function CountNotNull: Integer;
    {** Returns the index of a record.
        IndexOf performs a binary search and returns the index of the record
        that has the same date and time with the Date argument. If this date
        does not exist, IndexOf returns -1. In case of time series with
        overlapping periods, IndexOf's result is undefined.
    }
    function IndexOf(Date: TDateTime): Integer;
    {** Returns the index of a record with a specific SortingDate.
        SortingIndexOf is used internally by TTimeseriesGrid. It is the same
        as IndexOf, but for SortingDate instead of Date.
        @SeeAlso <See Method=SortingDate>
        @SeeAlso <See Method=IndexOf>
        @SeeAlso <See Class=TTimeseriesGrid>
    }
    function SortingIndexOf(SortingDate: TDateTime; UseDateOffset: Boolean):
      Integer;
    {** Inserts a record at the specified position of Items array.
        Insert returns Index. If IsNull is True, the Value is ignored.
        @SeeAlso <See Method=InsertSorted>
        @SeeAlso <See Class=ERecordAlreadyExists>
    }
    function Insert(Index: Integer; Date: TDateTime; IsNull: Boolean;
      Value: Real; FlagString: string; MStatus: TMStatus): Integer; overload;
      virtual;
    {** (Overloaded method - this line does not appear in the help).
        This version of Insert inserts the Item to the correct position of the
        Timeseries. It returns the Item's index.
    }
    function Insert(Date: TDateTime; IsNull: Boolean; Value: Real;
      FlagString: string; MStatus: TMStatus): Integer; overload; virtual;
    {** Returns Items[Count-1]. }
    function Last: TTSRecord;
    {** Import generic data (used to read from raw data files)
        Reads from Stream, then put data into time series. Existing
        data will be overwriten if Overwrite parameters is set to True.
        Decimal separator is treated as '.', delimiter as ',' and
        flag delimiter as white space. Date format yyyy-mm-dd hh:nn'
        @Author Stefanos
    }
    procedure ImportData(AStreamReader: TStreamReader; Overwrite: Boolean);
      overload;
    {** Import generic data. This is an overloaded version using TStream instead
        of TStreamReader.
        Default encoding is UTF8, or else specify and Encoding.
    }
    procedure ImportData(AStream: TStream; Encoding: TEncoding;
      Overwrite: Boolean); overload;
    procedure ImportData(AStream: TStream; Overwrite: Boolean); overload;
    {** Reads the timeseries data from a stream.
        Use ReadData to read the timeseries data from a file, string,
        database text large object, or any other stream. ReadData expects
        the data to come in comma-delimited format: date, value, flags. The
        date is in the format specified by ADateFormat, and the value uses
        ADecimalSeparator as the decimal separator.<p>
        Use ReadMeta in order to read ADecimalSeparator, ADelimiter,
        AFlagDelimiter, ADateFormat; or you may leave the default values
        (point as decimal separator, semicolon as delimiter, space as
        flag delimiter and yyyy-mm-dd hh:nn as date format) as specified
        in procedure declaration.<p>
        Non default values of parameters are often used by the
        LoadFromStream procedure in order to load data from text
        files saved by previous version of our software to keed compability.<p>
        ReadData deletes all data from the timeseries object before starting to
        load it from the stream.<p>
        If StartDate and EndDate both have the value idaEmpty, the entire
        stream is read; if they have other values, records found in the stream
        with dates outside the specified range are ignored.<p>
        FirstLine is the line number of the first line of the stream, to be
        used for error messages. This can be 1, if the stream has just been
        opened, or it can be larger, if information such as headers has already
        been read from the stream.
        <p>
        If ProgressIndicator is not nil, the method it specifies is called
        periodically with progress information in order to update visual
        progress indicators. When it is called, the Completed argument is the
        lines read so far, and the Total argument is an estimate of the total
        lines calculated from stream size and average line size so far.<p>
        @SeeAlso <See Method=ReadMeta>
        @SeeAlso <See Method=ReadCompressedData>
        @SeeAlso <See Method=LoadFromStream>
        @SeeAlso <See Method=WriteData>
    }
    procedure ReadData(AStreamReader: TStreamReader;
      StartDate, EndDate: TDateTime;
      FirstLine: Integer; ProgressIndicator: TProgressIndicator = nil;
      const ADecimalSeparator: Char ='.'; const ADelimiter: Char =',';
      const AFlagDelimiter: Char =' ';
      const ADateFormat: string ='yyyy-mm-dd hh:nn'); overload;
    {** This is an overloaded version of ReadData, in most cases you should
        use the other version.
        This version reads from a Stream instead of StreamReader.
        You should not use this version if the Stream contains data other than
        the time series records (e.g. meta data), since this version of ReadData
        reads the entire stream. It can be used e.g. in the case when read is
        from a database clob containing some of the records.. (as this is
        implemented by the louise.tsdb.loadfromdatabase method).
    }
    procedure ReadData(AStream: TStream;
      StartDate, EndDate: TDateTime;
      FirstLine: Integer; ProgressIndicator: TProgressIndicator = nil;
      const ADecimalSeparator: Char ='.'; const ADelimiter: Char =',';
      const AFlagDelimiter: Char =' ';
      const ADateFormat: string ='yyyy-mm-dd hh:nn'); overload;
    {** Use WriteCompressedData to read compressed data from a stream.
        ReadCompressedData uses the zlib TDecompressionStream to decompress
        data compressed with the TCompressedStream method.
        @SeeAlso <See Method=ReadData>
        @author Stefanos
    }
    procedure ReadCompressedData(AStream: TStream; StartDate,
      EndDate: TDateTime; FirstLine: Integer; ProgressIndicator:
      TProgressIndicator = nil; const ADecimalSeparator: Char ='.';
      const ADelimiter: Char =','; const AFlagDelimiter: Char =' ';
      const ADateFormat: string ='yyyy-mm-dd hh:nn');
    {** Reads the timeseries metadata from a stream.
        Use ReadMeta to read the timeseries metadata from a file, string,
        database text large object, or any other stream. ReadMeta expects the
        metadata as lines of the form "key=value" (without the quotes),
        followed by a blank line. It stops reading the stream after reading the
        blank line, and raises an exception for invalid syntax or unknown
        keys.<p>
        ReadMeta deletes all data from the timeseries object before starting to
        load the metadata from the stream.<p>
        FirstLine is the line number of the first line of the stream, to be
        used for error messages. This is normally 1.<p>
        ReadMeta returns by reference decimal separator, delimiter,
        flag delimiter and date format in order to use in Read Data. This
        process is done automatically in LoadFromStream method. If you do not
        need to read this properties, set some dummies variables in
        function arguments. Finally ReadData returns an integer, the line
        number of the empty line that interupts data parsing.
        @SeeAlso <See Method=ReadData>
        @SeeAlso <See Method=WriteMeta>
        @SeeAlso <See Method=LoadFromStream>
    }
    function ReadMeta(AStreamReader: TStreamReader; FirstLine: Integer;
      var ADecimalSeparator, ADelimiter, AFlagDelimiter: Char;
      var ADateFormat: string; FileVersion: Integer=2): Integer;
    {** LoadFromStream read a timeseries from the stream.
        LoadFromStream calls ReadMeta method, following ReadData
        in order to read a timeseries from the stream. Use LoadFromStream
        for File - type operations, when delimiter etc are specified in
        the preamble of the file.
        @SeeAlso <See Method=ReadData>
        @SeeAlso <See Method=ReadMeta>
        @SeeAlso <See Method=LoadFromFile>
        @SeeAlso <See Method=WriteToStream>
    }
    procedure LoadFromStream(AStream: TStream; StartDate, EndDate: TDateTime;
      ProgressIndicator: TProgressIndicator = nil; FileVersion: Integer=2;
      DoReadData: Boolean=True);
    {** Loads a time series from a file.
        Use LoadFromFile to load a time series from a file that has the format
        described in WriteToFile, with preamble. Any data already existing in
        the timeseries are deleted.<P>
        If ProgressIndicator is not nil, the procedured specified by it will be
        periodically called with progress information in order to update visual
        progress indicators.<p>
        @SeeAlso <See Method=WriteToFile>
        @SeeAlso <See Method=LoadFromStream>
    }
    procedure LoadFromFile(AFileName: string;
    ProgressIndicator: TProgressIndicator = nil; FileVersion: Integer=0);
    {** Load the meta section from a time series file.
        This method is used for loading time series templates for new
        time series wizard.
    }
    procedure LoadMetaFromFile(AFileName: string);
    {** Reads file with filename AFileName and returns the version number.
        Reads the first line of the file. If it is equal with "Version=2"
        (w/o quotes) it returns 2 or else it returns 1.
    }
    function SenseVersion(AFileName: string): Integer;
    {** Writes the time series data to a stream.
        Use WriteData to write the timeseries data to a file, string, database
        large object, or any other stream. WriteData writes the data in
        comma-delimited format: date, value, flags. The date is in
        YYYY-MM-DD&nbsp;HH:mm format, and the value uses a dot as the decimal
        separator, regardless of the settings of Delphi's global formatting
        variables.<p>
        <p>
        If ProgressIndicator is not nil, the method it specifies is called
        periodically with progress information in order to update visual
        progress indicators. When it is called, the Completed argument is the
        records written so far, and the Total argument is the total number of
        records.
        @SeeAlso <See Method=ReadData>
        @SeeAlso <See Method=WriteCompressedData>
        @SeeAlso <See Method=WriteMeta>
        @SeeAlso <See Method=WriteToStream>
    }
    procedure WriteData(AStreamWriter: TStreamWriter; ProgressIndicator:
    TProgressIndicator = nil); overload;
    {** This is an overloaded procedure of WriteData, use the other version
        instead
    }
    procedure WriteData(AStreamWriter: TStreamWriter; AStart, AEnd: Integer;
      ProgressIndicator: TProgressIndicator = nil); overload;
    {** This overloaded version of WriteData writes to a TStream instead of
        a TStreamWriter.
        For more information read the help of the corresponding overloaded
        version of ReadData method.
        @SeeAlso <See Method=ReadData>
    }
    procedure WriteData(AStream: TStream; ProgressIndicator:
    TProgressIndicator = nil); overload;
    {** This overloaded version of WriteData writes to a TStream instead of
        a TStreamWriter.
        For more information read the help of the corresponding overloaded
        version of ReadData method.
        @SeeAlso <See Method=ReadData>
    }
    procedure WriteData(AStream: TStream; AStart, AEnd: Integer;
      ProgressIndicator: TProgressIndicator = nil); overload;
    {** Use WriteCompressedData in order to write compressed data to a stream.
        WriteCompressed data uses the zlib TCompressionStream to compress
        data with the default compression level set.
        @SeeAlso <See Method=WriteData>
        @author Stefanos
    }
    procedure WriteCompressedData(AStream: TStream; ProgressIndicator:
    TProgressIndicator); overload;
    {** This is an overloaded procedure of WriteData, use the other version
        instead
    }
    procedure WriteCompressedData(AStream: TStream; AStart, AEnd: Integer;
      ProgressIndicator: TProgressIndicator = nil); overload;
    {** Writes the time series metadata to a stream.
        Use WriteMeta to write the timeseries metadata to a file, string,
        database text large object, or any other stream. WriteMeta writes the
        metadata as lines of the form "key=value" (without the quotes),
        followed by a blank line.<p>
        Preample contains the date format as
        YYYY-MM-DD&nbsp;HH:mm, and the value uses a dot as the decimal
        separator, regardless of the settings of Delphi's global formatting
        variables.<p>
        @SeeAlso <See Method=ReadMeta>
        @SeeAlso <See Method=WriteMeta>
        @SeeAlso <See Method=WriteToStream>
    }
    procedure WriteMeta(AStreamWriter: TStreamWriter; FileVersion: Integer=2);
    {** WriteToStream write a timeseries to the stream.
        WriteToStream calls WriteMeta method, followed by
        WriteData method in order to write the timeseries to
        the stream.
        @SeeAlso <See Method=WriteMeta>
        @SeeAlso <See Method=WriteData>
        @SeeAlso <See Method=WriteToFile>
        @SeeAlso <See Method=LoadFromStream>
    }
    procedure WriteToStream(AStream: TStream;
      ProgressIndicator: TProgressIndicator = nil; FileVersion: Integer=2);
    {** Writes the time series to a file.
        Use WriteToFile to write the time series to a file. If the file already
        exists, it is overwritten.<p>
        WriteToFile writes one line for each record, in three columns delimited
        by semicolon. The first column contains the date in a
        YYYY-MM-DD&nbsp;HH:mm format, which uses the same model as the Delphi
        FormatDateTime function.
        The second column contains the value, using the point (.) as
        decimal separator, or nothing if the value is null, and the third column
        contains flags delimited by space.<p>
        If ProgressIndicator is not nil, the procedured specified by it will be
        periodically called with progress information in order to update visual
        progress indicators.<p>
        Normally before writing the records a preamble is written to the file,
        consisting of lines of the form "Property=Value"; the preamble is
        followed by an empty line. The preamble contains the DateFormat,
        Delimiter, FlagDelimiter and DecimalSeparator, and all the TTimeseries
        object's properties.
        @SeeAlso <See Method=LoadFromFile>
        @SeeAlso <See Method=WriteToStream>
        @SeeAlso <Jump File=Delphi5.hlp K="FormatDateTime function" Text=FormatDateTime>
    }
    procedure WriteToFile(AFileName: string;
       ProgressIndicator: TProgressIndicator = nil; FileVersion: Integer=2);
    {** Merges two time series.
        Merge reads Source.Items and inserts all its data to Self.Items. For
        entries with the same date and time in both time series, the
        corresponding Self.Items entries are replaced. }
    procedure Merge(Source: TTimeseries);
  {** This is a redundant function, maily use to Import data from text files.
      Use it in conjuction with ImportData in order to prepare undo buffers
      with consistent start and end dates.
      TODO: Check where Merge is used, then use a single Merge
  }
    procedure MergeData(Source: TTimeseries; Overwrite: Boolean);
    {** Returns the index of the entry the date of which is nearest to Date.
        NearestTo is like IndexOf but works even if an entry with the specified
        Date does not exist in the time series. If it exists, it returns its
        index, just like IndexOf. Otherwise, it returns the index of the entry
        the date of which is nearest to Date. If the time series does not have
        any entries at all (Count=0), NearestTo returns -1.
        @SeeAlso <See Method=IndexOf>
        @SeeAlso <See Method=PositionOfNext>
        @SeeAlso <See Method=PositionOfPrevious> }
    function NearestTo(Date: TDateTime): Integer;
    {** Returns the index of the entry the date of which is greater than or
        equal to Date.
        PositionOfNext is like IndexOf but works even if an entry with the
        specified Date does not exist in the time series. If it exists, it
        returns its index, just like IndexOf. Otherwise, it returns the index of
        the first entry with a date greater than Date. If Date is greater than
        the date of the last record of the time series, PositionOfNext returns
        -1.
        @SeeAlso <See Method=IndexOf>
        @SeeAlso <See Method=NearestTo>
        @SeeAlso <See Method=PositionOfPrevious>}
    function PositionOfNext(Date: TDateTime): Integer;
    {** Returns the index of the entry the date of which is less than or
        equal to Date.
        PositionOfPrevious is like IndexOf but works even if an entry with the
        specified Date does not exist in the time series. If it exists, it
        returns its index, just like IndexOf. Otherwise, it returns the index of
        the last entry with a date less than Date. If Date is less than
        the date of the first record of the time series, PositionOfPrevious
        returns -1.
        @SeeAlso <See Method=IndexOf>
        @SeeAlso <See Method=NearestTo>
        @SeeAlso <See Method=PositionOfNext>}
    function PositionOfPrevious(Date: TDateTime): Integer;
    {** Deletes a record with a specified date.
        Remove deletes the record with the specified Date and returns the index
        the record had before deleting. Remove should not be used in time series
        with overlapping time periods. }
    function Remove(Date: TDateTime): Integer;
    {** Returns the mean value of the time series.
        Use Mean to calculate the mean value of the time series.
    }
    function Mean: Double; overload;
    {** (Overloaded function - this line does not appear in the help).
        This version of Mean calculates the mean value for the specified dates
        only. The time series must have not null values for all the dates in
        ADateTimeList, or an exception is raised.
    }
    function Mean(ADateTimeList: TDateTimeList): Double; overload;
    {** Process the time series, produce two new time series, and return
        these new time series as a tuple.
        The first of these series is the aggregated series; the second one is
        the number of missing values in each time step (more on this below).
        Both produced time series have a time step of target_step, which must be
        a TimeStep object. The nominal_offset, actual_offset, and interval_type
        attributes of target_step are taken into account during aggregation;
        so if, for example, target_step is one day with nominal_offset=(480,0),
        actual_offset=(0,0), and an interval_type of IntervalType.SUM,
        then aggregation is performed so that, in the resulting time series,
        a record with timestamp 2008-01-17 08:00 contains the sum of the values
        of the source series from 2008-01-16 08:00 to 2008-01-17 08:00.

        If target_step.interval_type is IntervalType.VECTOR_AVERAGE, then the
        source records are considered to be directions in degrees (as in a wind
        direction time series); each produced record is the direction in degrees
        of the sum of the unit vectors whose direction is specified by the
        source records.

        If some of the source records corresponding to a destination record are
        missing, missing_allowed specifies what will be done. If the ratio of
        missing values to existing values in the source record is greater than
        missing_allowed, the resulting destination record is null; otherwise,
        the destination record is derived even though some records are missing.
        In that case, the flag specified by missing_flag is raised in the
        destination record. The second time series returned in the return tuple
        contains, for each destination record, a record with the same date,
        containing the number of missing source values for that destination
        record.
    }
procedure Aggregate(Dest, Missing: TTimeseries;Options: TAggregationOptionsRec;
  ProgressIndicator: TProgressIndicator=nil);
    {** Returns the variance of the time series.
        Use Variance to calculate the variance of the time series.
    }
    function Variance: Double; overload;
    {** (Overloaded function - this line does not appear in the help).
        This version of Variance calculates the variance for the specified dates
        only. The time series must have not null values for all the dates in
        ADateTimeList, or an exception is raised.
    }
    function Variance(ADateTimeList: TDateTimeList): Double; overload;
    {** Determines the time step of a time series.
        FindTimeStep determines the time step of the time series, and returns
        one of tstYearly, tstMonthly, tstDaily, tstHourly, tstTenMinute, or
        tstIrregular.
        WARNING 2009-12-08: This function is deprecated and not maintained
        anymore. It is kept since I don't know if it is used in
        hydronomeas.
    }
    function FindTimeStep: TTimestep;
    {** Sets HydrologicalYear and alters accordingly all records.
        Use SetHydrologicalYear to specify whether an annual time series uses
        hydrological year or not. SetHydrologicalYear, besides altering the
        HydrologicalYear property, adjusts all the records' dates so that the
        month stored conforms to the convention descibed in TTsRecord.Date. If
        the time series is not annual, SetHydrologicalYear raises an exception.
        @SeeAlso <See Property=HydrologicalYear>
        @SeeAlso <See Property=TTsRecord.Date>
    }
    procedure SetHydrologicalYear(Value: Boolean);
    {** Checks if a certain date would be valid in the time series.
        You probably don't need to use CheckIfDateFits; it has been written
        mainly for internal use, and is automatically called whenever you add
        a record to a time series; an exception is raised if the date doesn't
        fit.<p>
        CheckIfDateFits tests whether ADate would be valid as a record date,
        given all the constraints imposed by TimeStep, DateOffset,
        TimeStepStrict, and HydrologicalYear. See the TTimeseries properties
        TimeStep, DateOffset, TimeStepStrict, and HydrologicalYear for a
        description of these constraints.<p>
        @SeeAlso <See Property=TTimeseries.TimeStep>
        @SeeAlso <See Property=TTimeseries.DateOffset>
        @SeeAlso <See Property=TTimeseries.TimeStepStrict>
        @SeeAlso <See Property=TTimeseries.HydrologicalYear>
    }
    function CheckIfDateFits(ADate: TDateTime;
      AlwaysCheck: Boolean=False): Boolean;
    {** Checks if time series records are continuous.
        Continuous only works on time series with strict time step; otherwise,
        it raises an exception. It returns True if each record differs in time
        from the previous one by one time step. Some records may be null,
        however. To check for the existence of nulls as well, check whether
        Count equals CountNotNull.
        @SeeAlso <See Property=Count>
        @SeeAlso <See Method=CountNotNull>
    }
    function Continuous: Boolean;
    {** Adds an invalidator to the Invalidators array.
        Do not use; only used internally by tsgrid.
        @SeeAlso <See Property=Invalidators>
    }
    procedure AddInvalidator(AInvalidator: TMethod);
    {** Adds an invalidator to the FullInvalidators array.
        Do not use; only used internally by tsgrid.
        @SeeAlso <See Property=FullInvalidators>
    }
    procedure AddFullInvalidator(AFullInvalidator: TMethod);
    {** Removes an invalidator from the Invalidators array.
        Do not use; only used internally by tsgrid.
        @SeeAlso <See Property=Invalidators>
    }
    procedure RemoveInvalidator(AInvalidator: TMethod);
    {** Removes an invalidator from the FullInvalidators array.
        Do not use; only used internally by tsgrid.
        @SeeAlso <See Property=Invalidators>
    }
    procedure RemoveFullInvalidator(AFullInvalidator: TMethod);
    {** Set the ID property to the AID value.
        Do not use SetID directely. Only used internaly when read
        a timeseries from database.
        @SeeAlso <See Property=ID>
    }
    procedure SetID(AID: Integer);
    {** Colorize the timeseries with statistics bg color in a timeseries grid.
        This property is used by multi timeseries timeseries grid and it
        has mainly internal scope.
    }
    property StatisticsBgColor: Boolean read FStatisticsBgColor write
      FStatisticsBgColor;
  end;

  { TMultiTimeseries }

  {** Stores a time series consisting of many sections.
      A TMultiTimeseries is a collection of time series; we call these time
      series "sections". TMultiTimeseries is an array of time series, but
      provides methods to manipulate the entire collection as a whole.
      TMultiTimeseries is intended to be used for synthetic time series, which
      are produced by running a scenario multiple times.
  }
  TMultiTimeseries = class(TPersistent)
  private
    Fid: Integer;
    FName: string;
    FSectionList: TObjectList;
    { The following three variables hold temporary information for progress
      indicators during loading from or saving to the database. }
    function GetTimeStep: TTimeStep;
    procedure SetTimeStep(Value: TTimeStep);
    function GetVariable: Integer;
    procedure SetVariable(Value: Integer);
    function GetVariableType: TVariableType;
    procedure SetVariableType(Value: TVariableType);
    function IsModified: Boolean;
    procedure SetModified(Value: Boolean);
    function GetSectionCount: Integer;
    function GetCount: Integer;
    function GetDateOffset: Real;
    procedure SetDateOffset(Value: Real);
    function IsDateOffsetUnspecified: Boolean;
    procedure SetDateOffsetUnspecified(Value: Boolean);
    function IsTimeStepStrict: Boolean;
    procedure SetTimeStepStrict(Value: Boolean);
    function IsHydrologicalYear: Boolean;
    function GetPrecision: Integer;
    procedure SetPrecision(Value: Integer);
    function GetMUnit: string;
    procedure SetMUnit(Value: string);
    procedure SetGTypeName(Value: string);
    procedure SetGentityName(Value: string);
    procedure SetTypeName(Value: string);
    procedure SetVarName(Value: string);
    procedure SetTimeStepName(Value: string);
    function GetGTypeName: string;
    function GetGentityName: string;
    function GetTypeName: string;
    function GetVarName: string;
    function GetTimeStepName: string;
  protected
    function GetSection(Num: Integer): TTimeseries;
    function GetRecord(Num, Index: Integer): TTsRecord;
  public
    {** Do not use this function.
    }
    procedure EnsureNumExists(Num: Integer);
    {** The number of sections.
    }
    property SectionCount: Integer read GetSectionCount;
    {** The number of data in all sections.
        @SeeAlso <See Method=CountNotNull>
    }
    property Count: Integer read GetCount;
    {** The Sections.
        Sections is an array of TTsRecord objects, each of which is a section of
        the synthetic time series. The section with num=1 is stored in
        Sections[1]; there is no Sections[0].
    }
    property Sections[Num: Integer]: TTimeseries read GetSection;
    {** The records of all sections.
        Use Records to access the records of the sections directly. Note that
        the numbering of Num starts at 1; the first record of the first section
        is Records[1, 0].
    }
    property Records[Num, Index: Integer]: TTsRecord read GetRecord;
    {** Specifies whether the data or other properties have been modified since
        loaded.
        If Modified is False, then the time series has not been changed since it
        was read from the data base or the file. Modified becomes True
        automatically whenever the data	or properties are modified.<p>
    }
    property Modified: Boolean read IsModified write SetModified;
    {** The time step of the time series.
        Besides TimeStep, important information on the time step can be found in
        the properties in the links at "See Also".
        @SeeAlso <See Property=HydrologicalYear>
        @SeeAlso <See Property=DateOffset>
        @SeeAlso <See Property=TimeStepStrict>
        @SeeAlso <See Property=TTsRecord.Date>
    }
    property TimeStep: TTimeStep read GetTimeStep write SetTimeStep;
    {** Specifies whether an annual time series uses the hydrological year.
        Read HydrologicalYear to determine whether a year YYYY refers to the
        time period YYYY-01-01 to YYYY-12-31 or to the time period YYYY-10-01
        to (YYYY+1)-09-30.
    }
    property HydrologicalYear: Boolean read IsHydrologicalYear;
    {** Specifies the offset of the time stamps.
        @SeeAlso <See Property=TTimeseries.DateOffset>
    }
    property DateOffset: Real read GetDateOffset write SetDateOffset;
    {** Specifies whether the date offset is specified.
        @SeeAlso <See Property=TTimeseries.DateOffsetUnspecified>
    }
    property DateOffsetUnspecified: Boolean read IsDateOffsetUnspecified
      write SetDateOffsetUnspecified;
    {** Specifies whether the time step has irregularities.
        @SeeAlso <See Property=TTimeseries.TimeStepStrict>
    }
    property TimeStepStrict: Boolean read IsTimeStepStrict write
      SetTimeStepStrict;
    {** Not used yet. }
    property Variable: Integer read GetVariable write SetVariable;
    {** Specifies whether the variable is instantaneous, cumulative, average,
        maximum or minimum. }
    property VariableType: TVariableType read GetVariableType write
      SetVariableType;
    {** Holds the time series database id.
        If the time series has been loaded from or written to the database, id
        holds the id used in the last such operation. You cannot set id; it is
        automatically set when loading from or writing to database.
    }
    property id: Integer read Fid;
    {** Holds the number of decimal digits shown when displaying.
        @SeeAlso <See Property=TTimeseries.Precision>
    }
    property Precision: Integer read GetPrecision write SetPrecision;
    {** Adds a record to the end of a section.
        Add adds a record to the end of the specified section, and should be
        used only when the data are ordered; otherwise Insert should be used.
        If IsNull is True, Value is ignored.<p>
        @SeeAlso <See Method=Insert>
    }
    function Add(Num: Integer; Date: TDateTime; IsNull: Boolean; Value: Real;
      FlagString: string; MStatus: TMStatus): Integer; virtual;
    {** Specifies the unit of measurement.
        @SeeAlso <See Property=TTimeseries.MUnit>
    }
    property MUnit: string read GetMUnit write SetMUnit;
    property GTypeName: string read GetGTypeName write SetGTypeName;
    property GentityName: string read GetGentityName write SetGentityName;
    property TypeName: string read GetTypeName write SetTypeName;
    property VarName: string read GetVarName write SetVarName;
    property TimeStepName: string read GetTimeStepName write SetTimeStepName;
    {** Deletes all records from all sections. }
    procedure Clear;
    {** Creates a TMultiTimeseries object.
    }
    constructor Create; overload;
    {** Copies a TMultiTimeseries object.
        @SeeAlso <Jump File=Delphi5.hlp K="TPersistent,Assign" Text=Assign>
    }
    procedure Assign(Source: TPersistent); override;
    {** Deletes a record from the time series.
        Delete deletes an entry from the Records array. }
    procedure Delete(Num, Index: Integer);
    {** Destroys a TMultiTimeseries object.
        Do not call Destroy directly. Call Free instead. Free checks that the
        object reference is not nil before calling Destroy. }
    destructor Destroy; override;
    {** Returns number of not-null records.
    }
    function CountNotNull: Integer;
    {** Returns the index of a record.
        IndexOf performs a binary search and returns the index of the record
        that has the same date and time with the Date argument. If this date
        does not exist, IndexOf returns -1. In case of time series with
        overlapping periods, IndexOf's result is undefined.
    }
    function IndexOf(Num: Integer; Date: TDateTime): Integer;
    {** Inserts a record at the specified position.
        Insert returns Index. If IsNull is True, the Value is ignored.
        @SeeAlso <See Method=InsertSorted> }
    function Insert(Num, Index: Integer; Date: TDateTime; IsNull: Boolean;
      Value: Real; FlagString: string; MStatus: TMStatus): Integer; overload;
      virtual;
    {** (Overloaded method - this line does not appear in the help).
        This version of Insert inserts the Item to the correct position of the
	Timeseries. It returns the Item's index.
    }
    function Insert(Num: Integer; Date: TDateTime; IsNull: Boolean; Value: Real;
      FlagString: string; MStatus: TMStatus): Integer; overload; virtual;
    {** Returns the index of the entry the date of which is nearest to Date.
        NearestTo is like IndexOf but works even if an entry with the specified
        Date does not exist in the time series. If it exists, it returns its
        index, just like IndexOf. Otherwise, it returns the index of the entry
        the date of which is nearest to Date. If the time series does not have
        any entries at all (Count=0), NearestTo returns -1.
        @SeeAlso <See Method=IndexOf>
        @SeeAlso <See Method=PositionOfNext>
        @SeeAlso <See Method=PositionOfPrevious> }
    function NearestTo(Num: Integer; Date: TDateTime): Integer;
    {** Returns the index of the entry the date of which is greater than or
        equal to Date.
        PositionOfNext is like IndexOf but works even if an entry with the
        specified Date does not exist in the time series. If it exists, it
        returns its index, just like IndexOf. Otherwise, it returns the index of
        the first entry with a date greater than Date. If Date is greater than
        the date of the last record of the time series, PositionOfNext returns
        -1.
        @SeeAlso <See Method=IndexOf>
        @SeeAlso <See Method=NearestTo>
        @SeeAlso <See Method=PositionOfPrevious>}
    function PositionOfNext(Num: Integer; Date: TDateTime): Integer;
    {** Returns the index of the entry the date of which is less than or
        equal to Date.
        PositionOfPrevious is like IndexOf but works even if an entry with the
        specified Date does not exist in the time series. If it exists, it
        returns its index, just like IndexOf. Otherwise, it returns the index of
        the last entry with a date less than Date. If Date is less than
        the date of the first record of the time series, PositionOfPrevious
        returns -1.
        @SeeAlso <See Method=IndexOf>
        @SeeAlso <See Method=NearestTo>
        @SeeAlso <See Method=PositionOfNext>}
    function PositionOfPrevious(Num: Integer; Date: TDateTime): Integer;
    {** Deletes a record with a specified date.
        Remove deletes the record with the specified Date and returns the index
        the record had before deleting.}
    function Remove(Num: Integer; Date: TDateTime): Integer;
    {** Sets HydrologicalYear and alters accordingly all records.
        Use SetHydrologicalYear to specify whether an annual time series uses
        hydrological year or not. SetHydrologicalYear, besides altering the
        HydrologicalYear property, adjusts all the records' dates so that the
        month stored conforms to the convention descibed in TTsRecord.Date. If
        the time series is not annual, SetHydrologicalYear raises an exception.
        @SeeAlso <See Property=HydrologicalYear>
        @SeeAlso <See Property=TTsRecord.Date>
    }
    procedure SetHydrologicalYear(Value: Boolean);
    {** Checks the multi time series for internal consistency.
        A TMultiTimeseries is consistent when all of the following conditions
        hold:
        <ul>
          <li>All sections have the same number of records.
          <li>There are no null or missing values.
          <li>All sections have identical start and end dates.
          <li>The number of records in each section is a multiple of
              CountDivisor (use 1 to omit this check)
        </ul>
        If any of the conditions does not hold, CheckConsistency raises an
        exception with an informative message.
    }
    procedure CheckConsistency(CountDivisor: Integer);
    {** Set the ID property to the AID value.
        Do not use SetID directely. Only used internaly when read
        a timeseries from database.
        @SeeAlso <See Property=ID>
    }
    procedure SetID(AID: Integer);
    {** The name of the TMultiTimeseries object that is Section[1].Name
    }
    property Name: string read FName write FName;
    {** Adds the ATimeseries object to the sections array.
        Before adding the timeseries you have to create the ATimeseries
        object instance. After adding do not destroy the object as it is
        controled by the sections object list.<p>
        AddSection method checks for ATimeseries consistency before adding it
        into the object list array that is:<p>
        -Timeseries should have the same timestep<p>
        -Timeseries should have the same count<p>
        -Timeseries should have the same begining record date.<p>
        Function returns the index of the new added timeseries.
    }
    function AddSection(ATimeseries: TTimeseries): Integer;
    {** Deletes a single section at the AIndex position.
        DeleteSection raises a EListError exception when AIndex is not
        within 1 and Count (that is the first and the last section).
    }
    procedure DeleteSection(AIndex: Integer);
    {** Clears all the items of the object list array by freeing sections
        memory.
    }
    procedure ClearSections;
    {** Writes meta and data information to a stream.
    }
    procedure WriteToStream(AStream: TStream; ProgressIndicator:
      TProgressIndicator = nil);
    {** Uses WriteToStream in order to write a text file via a TFileStream.
    }
    procedure WriteToFile(AFileName: string;
       ProgressIndicator: TProgressIndicator = nil);
    {** Loads Meta and (multiple section) Data from a Stream.
        LoadFromStream method clears all data stored into a TMultiTimeseries
        object, then retrieves data from Stream.
        Meta and data sections should be separated by empty lines, as
        WriteToStream method does when writing.
    }
    procedure LoadFromStream(AStream: TStream; ProgressIndicator:
      TProgressIndicator = nil);
    {** Uses LoadFromStream method in order to read from text file.
    }
    procedure LoadFromFile(AFileName: string; ProgressIndicator:
      TProgressIndicator = nil);
    {** Forces the multi time series to rearrange its sections so that
        each section has the same length with the given number of
        records, irrespective of the initial number of sections and length.
        The number of sections after the rearrangement results from
        the given section length (NrRecords) and the overall number of records.
        Missing values at the end of the last section are filled with null.
        Any empty final sections (consisting of null values) that may be
        created during this process are deleted.
        This routine is used to cut multi time series consisting of one
        long section into several smaller sections, but may be used for
          other purposes as well.
    }
    procedure RearrangeTS(NrRecords:Integer);
    {** Uses a given array of Real values and meta data to fill the multi
        time series object. Any previously existing data or sections are
        cleared.
    }
    procedure getDataFromArray(RealArray: Array of Real;
          Flags: TObject;  ATimestep:TTimeStep;
          AVariable:Integer; StartDate, EndDate:TDateTime);
  end;


  {** Class of time series.
      Under construction. }
  TTimeseriesClass = class of TTimeseries;

{** Returns the value of the date1 parameter,
    incremented by nrOfSteps time steps.
}
function IncTimeStep(const date1: TDateTime; ATimeStep:TTimeStep;
  nrOfSteps: Integer = 1): TDateTime;

implementation

{$C+}

uses math, Character, DateUtils;

function VarTypeToStr(Value: Integer): string;
begin
  case Value of
    vtInstantaneous: Result := 'Instantaneous';
    vtCumulative: Result := 'Cumulative';
    vtAverage: Result := 'Average';
    vtMaximum: Result := 'Maximum';
    vtMinimum: Result := 'Minimum';
    vtStdev: Result := 'Stdev';
    vtVectorAverage: Result := 'Vector averrage';
    vtUnknown: Result := 'Unknown';
  else
    Assert(False);
  end;
end;

resourcestring
  rsCantConvertToVariableType =
    'This string is not recognized as a variable type.';

function StrToVarType(AString: string): Integer;
var s: string;
begin
  s := LowerCase(AString);
  if s='instantaneous' then Result := vtInstantaneous
  else if s='cumulative' then Result := vtCumulative
  else if s='average' then Result := vtAverage
  else if s='maximum' then Result := vtMaximum
  else if s='minimum' then Result := vtMinimum
  else if s='stdev' then Result := vtStdev
  else if s='Vector averrage' then Result := vtVectorAverage
  else if s='unknown' then Result := vtUnknown
  else
    raise EConvertError.Create(AString+': '+rsCantConvertToVariableType);
end;

{ TDateOffset }

constructor TDateOffset.Create(AMinutes, AMonths: Integer);
begin
  FMinutes := AMinutes;
  FMonths := AMonths;
end;

procedure TDateOffset.Store(AMinutes, AMonths: Integer);
begin
  FMinutes := AMinutes;
  FMonths := AMonths;
end;

class operator TDateOffset.Equal(a: TDateOffset; b: TDateOffset): Boolean;
begin
  Result := (a.Minutes=b.Minutes) and
    (a.Months=b.Months);
end;

class operator TDateOffset.NotEqual(a: TDateOffset; b: TDateOffset): Boolean;
begin
  Result := not (a = b);
end;

{ TTimestep }

constructor TTimestep.Create(ALengthMinutes, ALengthMonths: Integer);
begin
  Create(ALengthMinutes, ALengthMonths, 0);
end;

resourcestring
  rsEitherLength = 'One of length in minutes or length in months should be '+
    'zero';

constructor TTimestep.Create(ALengthMinutes, ALengthMonths,
  AOrdinalValue: Integer);
begin
  if (ALengthMinutes<>0) and (ALengthMonths<>0) then
    raise Exception.Create(rsEitherLength);
  FLengthMinutes := ALengthMinutes;
  FLengthMonths := ALengthMonths;
  FOrdinalValue := AOrdinalValue;
end;

class operator TTimestep.Equal(a: TTimestep; b: TTimestep): Boolean;
begin
  Result := (a.LengthMinutes=b.LengthMinutes) and
    (a.LengthMonths=b.LengthMonths);
end;

class operator TTimestep.NotEqual(a: TTimestep; b: TTimestep): Boolean;
begin
  Result := not (a = b);
end;

class operator TTimestep.GreaterThan(a: TTimestep; b: TTimestep): Boolean;
begin
  if (a.LengthMonths>0) and (b.LengthMonths=0) then Result := True else
  if (a.LengthMonths=0) and (b.LengthMonths>0) then Result := False else
    Result := (a.LengthMinutes+a.LengthMonths)>(b.LengthMinutes+b.LengthMonths);
end;

class operator TTimestep.GreaterThanOrEqual(a: TTimestep;
  b: TTimestep): Boolean;
begin
  Result := (a>b) or (a=b);
end;

class operator TTimestep.LessThan(a: TTimestep; b: TTimestep): Boolean;
begin
  Result := not (a>=b);
end;

class operator TTimestep.LessThanOrEqual(a: TTimestep; b: TTimestep): Boolean;
begin
  Result := not (a>b);
end;

function TTimestep.TimeStepIn(ASetOfTimesteps: array of TTimestep): Boolean;
var i: Integer;
begin
  Result := False;
  for i := 0 to Length(ASetOfTimesteps)-1 do
    Result := Result or (TTimestep(ASetOfTimesteps[i]) = Self);
end;

function TTimestep.GetIsVariable: Boolean;
begin
  Result := (FLengthMinutes = 0) and (FLengthMonths = 0);
end;

function TTimestep.IncStep(ADateTime: TDateTime): TDateTime;
begin
  Result := IncStep(ADateTime, 1);
end;

function TTimestep.IncStep(ADateTime: TDateTime; IncBy: Integer): TDateTime;
begin
  if FLengthMonths>0 then
    Result := IncMonth(ADateTime, FLengthMonths*IncBy) else
    Result := AddDateTime(ADateTime, IncBy*FLengthMinutes/1440);
end;

function TTimestep.DecStep(ADateTime: TDateTime): TDateTime;
begin
  Result := DecStep(ADateTime, 1);
end;

function TTimestep.DecStep(ADateTime: TDateTime; DecBy:Integer): TDateTime;
begin
  if FLengthMonths>0 then
    Result := IncMonth(ADateTime, -FLengthMonths*DecBy) else
    Result := AddDateTime(ADateTime, -DecBy*FLengthMinutes/1440);
end;

resourcestring
  rsMissingValue = 'Missing value';

{ TTimeseries }

{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_CAST OFF}

constructor TTimeseries.Create;

  procedure CreateFlagStringList;
  begin
    FFlagStringList := TStringList.Create;
    with FFlagStringList do
    begin
      Sorted := True;
      CaseSensitive := False;
      Duplicates := dupIgnore;
    end;
    FFlagUnsortedList := TIntegerList.Create;
  end;

begin
  inherited Create;
  FRecordList := TObjectList.Create(True);
  FName := '';
  FHydrologicalYear := False;
  FTimeStepStrict := False;
  FStatisticsBgColor := False;
  FTimeStep := tstUnknown;
  FDateOffsetUnspecified := True;
  FNominalOffset.Store(0,0);
  FActualOffset.Store(0,0);
  Fid := 0;
  FPrecision := tsDefaultPrecision;
  FVariableType := vtInstantaneous;
  FFileName := '';
  FFileVersion := 2;
  FTimeZone := '';
  FVarName := '';
  CreateFlagStringList;
end;

destructor TTimeseries.Destroy;
begin
  FRecordList.Free;
  SetLength(FullInvalidators, 0);
  SetLength(Invalidators, 0);
  FFlagStringList.Free;
  FFlagUnsortedList.Free;
  inherited Destroy;
end;

function TTimeseries.Get(Index: Integer): TTsRecord;
begin
  Result := TTsRecord(FRecordList[Index]);
end;

function TTimeseries.AddToFlagList(FlagString: string): Integer;
var
  i, AIndex: Integer;
begin
  AIndex := FFlagStringList.Add(FlagString);
  if FFlagStringList.Count>FFlagUnsortedList.Count then
  begin
    for i := 0 to FFlagUnsortedList.Count-1 do
      if FFlagUnsortedList[i]>=AIndex then
        FFlagUnsortedList[i] := FFlagUnsortedList[i]+1;
    FFlagUnsortedList.Add(AIndex);
    Result := FFlagUnsortedList.Count-1;
  end else begin
    Result := FFlagUnsortedList.IndexOf(AIndex);
  end;
end;

function TTimeseries.GetFromFlagList(AIndex: Integer): string;
begin
  Result :=
    FFlagStringList[FFlagUnsortedList[AIndex]];
end;

function ExtractSpaceSeparatedItems(AStringList: TStringList; AString: string):
  string;
var
  i, j: Integer;
  FlagsList: TStringList;
begin
  FlagsList := nil;
  try
    FlagsList := TStringList.Create;
    with FlagsList do
    begin
      Sorted := True;
      Duplicates := dupIgnore
    end;
    for i := 0 to AStringList.Count-1 do
      for j := 1 to DelimitedStringCount(AStringList[i], ' ') do
        FlagsList.Add(DelimitedStringItem(AStringList[i], j, ' '));
    if AString<>'' then
      for j := 1 to DelimitedStringCount(AString, ' ') do
        FlagsList.Add(DelimitedStringItem(AString, j, ' '));
    Result := FlagsList.Text;
  finally
    FlagsList.Free;
  end;
end;

function TTimeseries.GetFlagsUsed: string;
begin
  Result := ExtractSpaceSeparatedItems(FFlagStringList,'');
end;

function TTimeseries.GetSelectionFlags: string;
begin
  Result := ExtractSpaceSeparatedItems(FFlagStringList, StandardFlags);
end;

resourcestring
  rsCantPutTsRecord = 'Record date does not match constraints';

procedure TTimeseries.DisplayFullInvalidate;
var i: Integer;
begin
  for i := 0 to Length(FullInvalidators)-1 do
    TMethodPtr(FullInvalidators[i]);
end;

procedure TTimeseries.DisplayInvalidate;
var i: Integer;
begin
  for i := 0 to Length(Invalidators)-1  do
    TMethodPtr(Invalidators[i]);
end;

function TTimeseries.UpTimestamp(ADateTime: TDateTime; Direction: Integer):
  TDateTime;
var
  ARequiredModulo, AActualModulo, d: Integer;
  AReferenceDate: TDateTime;
  Year, Month, Day: Word;
  AYear, ANominalSet, AYearSet: Integer;
begin
  Assert((Direction=1) or (Direction=-1));
  Result := ADateTime;
  DecodeDate(ADateTime, Year, Month, Day);
  if TimeStep.LengthMinutes>0 then
  begin
    Assert(NominalOffset.Months=0);
    ARequiredModulo := NominalOffset.Minutes*60;
    if ARequiredModulo<0 then ARequiredModulo := ARequiredModulo +
      TimeStep.LengthMinutes*60;
    AReferenceDate := EncodeDateTime(Year, Month, 1, 0, 0, 0, 0);
    d := DiffInSecs(ADateTime, AReferenceDate);
    AActualModulo := d mod (TimeStep.LengthMinutes*60);
    Result := AddDateTime(ADateTime, -(AActualModulo-ARequiredModulo)/86400);
    while Direction*DiffInSecs(Result, ADateTime)<0 do
      Result := TimeStep.IncStep(Result, Direction);
  end else if TimeStep.LengthMonths>0 then
  begin
    Assert(NominalOffset.Minutes=0);
    AYear := Year;
    ANominalSet := NominalOffset.Months;
    AYearSet := 0;
    while ANominalSet>=12 do
    begin
      Inc(AYearSet);
      ANominalSet := ANominalSet-12;
    end;
    if TimeStep.LengthMonths>12 then
    begin
      AYear := AYear  - ((AYear-1900-AYearSet) mod (TimeStep.LengthMonths div 12)) -
        Direction*(TimeStep.LengthMonths div 12);
    end else
      AYear := AYear-Direction;
    Result := EncodeDate(AYear, 1+ANominalSet, 1);
    while Direction*DiffInSecs(Result, ADateTime)<0 do
      Result := TimeStep.IncStep(Result, Direction);
  end;
end;

function TTimeseries.DownTimestamp(ADateTime: TDateTime): TDateTime;
begin
  Result := UpTimestamp(ADateTime, -1);
end;

function TTimeseries.NextTimestamp(ADateTime: TDateTime): TDateTime;
begin
  Result := UpTimestamp(ADateTime);
  Result := TimeStep.IncStep(Result);
end;

function TTimeseries.PreviousTimestamp(ADateTime: TDateTime): TDateTime;
begin
  Result := DownTimestamp(ADateTime);
  Result := TimeStep.DecStep(Result);
end;

function TTimeseries.ActualTimestamp(ADateTime: TDateTime): TDateTime;
begin
  Result := AddDateTime(ADateTime, ActualOffset.Minutes/1440);
  Result := Round(Result*86400)/86400;
  Result := IncMonth(Result, ActualOffset.Months);
end;

function TTimeseries.ContainingInterval(ADateTime: TDateTime): TDateTime;
begin
  Result := DownTimestamp(ADateTime);
  while DiffInSecs(ActualTimestamp(Result), ADateTime)>=0 do
    Result := PreviousTimestamp(Result);
  while DiffInSecs(ActualTimestamp(Result), ADateTime)<0 do
    Result := NextTimestamp(Result);
end;

function TTimeseries.IntervalStartPoint(ANominalTimestamp: TDateTime):
  TDateTime;
begin
  Result := ActualTimestamp(PreviousTimestamp(ANominalTimestamp));
end;

function TTimeseries.IntervalEndPoint(ANominalTimestamp: TDateTime): TDateTime;
begin
  Result := ActualTimestamp(ANominalTimestamp);
end;

function TTimeseries.IntervalMidPoint(ANominalTimestamp: TDateTime): TDateTime;
begin
  Result := 0.5*SubtractDateTime(IntervalEndPoint(ANominalTimestamp),
    IntervalStartPoint(ANominalTimestamp));
  Result := AddDateTime(IntervalStartPoint(ANominalTimestamp), Result);
  Result := Round(Result*86400)/86400;
end;

resourcestring
  rsChangingNominalOffsetNotAllowedFor
    = 'Nominal offset can changed only for an empty time series';

procedure TTimeseries.SetNominalOffset(AOffset: TDateOffset);
begin
  if Count>0 then
    raise Exception.Create(rsChangingNominalOffsetNotAllowedFor);
  FNominalOffset := AOffset;
  FTimestepStrict := True;
end;

resourcestring
  rsNominalOffsetCanReadOnlyForStrictTimeStep =
    'Nominal offset can read only if time step is strict';

function TTimeseries.GetNominalOffset: TDateOffset;
begin
  if not FTimeStepStrict then
    raise Exception.Create(rsNominalOffsetCanReadOnlyForStrictTimeStep);
  Result := FNominalOffset;
end;

procedure TTimeseries.AdjustDateOffset;
begin
  AdjustDateOffset(False);
end;

procedure TTimeseries.AdjustDateOffset(Nonstrict: Boolean);
var
  ts, d: Int64;
begin
{  to Annual time step, nominal offset is set manually}
  if TimeStep<>tstAnnual then FNominalOffset.Store(0, 0);
  if TimeStep.IsVariable or
    ((not TimeStepStrict) and (not Nonstrict)) or
    (Count=0) or (TimeStep=tstAnnual) then Exit;
{ Apply only to time series with a time step multiple of minutes not months }
  FDateOffsetUnspecified := False;
  if TimeStep<tstMonthly then
  begin
    ts := TimeStep.FLengthMinutes;
    FDateOffsetUnspecified := True;
    d := Round(DateTimeToC(Items[0].Date)*1440);
    FNominalOffset.Store((d mod ts),0);
  end else if TimeStep=tstMonthly then
  begin
    FNominalOffset.Store(0,0);
  end else if TimeStep<tstAnnual then begin
    FNominalOffset.Store(0, MonthOf(Items[0].Date)-1);
  end else if TimeStep>tstAnnual then
  begin
    FNominalOffset.Store(0, MonthOf(Items[0].Date)-1 +
      12* ((YearOf(Items[0].Date)-1900) mod (TimeStep.LengthMonths div 12)));
  end;
end;

function TTimeseries.Add(Date: TDateTime; IsNull: Boolean; Value: Real;
  FlagString: string; MStatus: TMStatus): Integer;
var NewRecord: TTsRecord;
begin
  if not CheckIfDateFits(Date) then
    raise Exception.Create(rsCantPutTsRecord+' ('+
      FormatDateTime('yyyy-mm-dd hh:nn', Date)+').');
  NewRecord := nil;
  try
    NewRecord := TTsRecord.Create(Self, Date, IsNull, Value, FlagString,
      MStatus);
    Result := FRecordList.Add(NewRecord);
    Modified := True;
    if FRecordList.Count=1 then AdjustDateOffset;
    DisplayFullInvalidate;
  except
    NewRecord.Free;
    raise;
  end;
end;

procedure TTimeseries.Clear;
begin
  FRecordList.Clear;
  FFlagStringList.Clear;
  FFlagUnsortedList.Clear;
  Modified := True;
  AdjustDateOffset;
  DisplayFullInvalidate;
end;

procedure TTimeseries.Delete(Index: Integer);
begin
  FRecordList.Delete(Index);
  Modified := True;
  if Count=0 then AdjustDateOffset;
  DisplayFullInvalidate;
end;

function TTimeseries.First: TTsRecord;
begin
  Result := TTsRecord(FRecordList.First);
end;

function TTimeseries.IndexOf(Date: TDateTime): Integer;
var i: Integer;
begin
  i := PositionOfNext(Date);
  if (i>=0) and (DiffInSecs(Items[i].Date, Date)=0) then
    Result := i
  else
    Result := -1;
end;

function TTimeseries.SortingIndexOf(SortingDate: TDateTime;
  UseDateOffset: Boolean): Integer;
var
  Low, High, Mid: Integer;
  Diff: Int64;
begin
  Low := 0;
  High := Count-1;
  Diff := 0;
  while Low<=High do
  begin
    Mid := (Low+High) div 2;
    Diff := DiffInSecs(SortingDate, Items[Mid].SortingDate(UseDateOffset));
    if Diff < 0 then
      High := Mid-1
    else if Diff > 0 then
      Low := Mid+1
    else
    begin
      Low := Mid;
      Break;
    end;
  end;
  if (Low>=Count) or (Diff<>0) then Result := -1
  else Result := Low;
end;

function TTimeseries.Insert(Index: Integer; Date: TDateTime; IsNull: Boolean;
  Value: Real; FlagString: string; MStatus: TMStatus): Integer;
var NewRecord: TTsRecord;
begin
  if not CheckIfDateFits(Date) then
    raise Exception.Create(rsCantPutTsRecord);
  Result := Index;
  NewRecord := nil;
  try
    NewRecord := TTsRecord.Create(Self, Date, IsNull, Value, FlagString,
      MStatus);
    FRecordList.Insert(Index, NewRecord);
    Modified := True;
    if (FRecordList.Count=1) or ((Index=0) and (TimeStep>tstMonthly)) then
      AdjustDateOffset;
    DisplayFullInvalidate;
  except
    NewRecord.Free;
    raise;
  end;
end;

resourcestring
  rsRecordAlreadyExists = 'A record with that date already exists.';
  
function TTimeseries.Insert(Date: TDateTime; IsNull: Boolean; Value: Real;
  FlagString: string; MStatus: TMStatus): Integer;
begin
  if IndexOf(Date)<>-1 then
    raise ERecordAlreadyExists.Create(rsRecordAlreadyExists);
  Result := PositionOfNext(Date);
  if Result=-1 then Result := Add(Date, IsNull, Value, FlagString, MStatus)
  else Insert(Result, Date, IsNull, Value, FlagString, MStatus);
end;

function TTimeseries.Last: TTsRecord;
begin
  Result := TTsRecord(FRecordList.Last);
end;

function TTimeseries.NearestTo(Date: TDateTime): Integer;
begin
  Result := PositionOfNext(Date);
  if Result=-1 then Result := Count-1;
  if (Result-1>=0) and (Abs(DiffInSecs(Date, Items[Result].Date)) >
  Abs(DiffInSecs(Date, Items[Result-1].Date))) then
    Dec(Result);
end;

function TTimeseries.PositionOfNext(Date: TDateTime): Integer;
var
  Low, High, Mid: Integer;
  Diff: Int64;
begin
  Low := 0;
  High := Count-1;
  while Low<=High do
  begin
    Mid := (Low+High) div 2;
    Diff := DiffInSecs(Date, Items[Mid].Date);
    if Diff < 0 then
      High := Mid-1
    else if Diff > 0 then
      Low := Mid+1
    else
    begin
      Low := Mid;
      Break;
    end;
  end;
  if Low>=Count then Result := -1 else Result := Low;
end;

function TTimeseries.PositionOfPrevious(Date: TDateTime): Integer;
begin
  if (Count=0) or (DiffInSecs(Date, First.Date)<0) then
  begin
    Result := -1;
    Exit;
  end;
  Result := PositionOfNext(Date);
  if Result = -1 then
    Result := Count-1
  else if DiffInSecs(Date, Items[Result].Date)<>0 then
    Dec(Result);
end;

function TTimeseries.Remove(Date: TDateTime): Integer;
begin
  Result := IndexOf(Date);
  if Result<>-1 then Delete(Result);
end;

function TTimeseries.GetCount: Integer;
begin
  Result := FRecordList.Count;
end;

procedure TTimeseries.Assign(Source: TPersistent);
var
  Src: TTimeseries;
  i: Integer;
begin
  Src := TTimeseries(Source);
  AssignMeta(Src);
  Clear;
  for i := 0 to Src.Count-1 do
  begin
    with Src[i] do Self.Add(FDate, FIsNull, FValue, Src[i].GetAllFlags,
      FMStatus);
  end;
  FModified := Src.FModified;
end;

procedure TTimeseries.AssignMeta(Source: TTimeseries);
begin
  FTimeStep := Source.FTimeStep;
  FVariable := Source.FVariable;
  FVarName := Source.VarName;
  FVariableType := Source.FVariableType;
  FHydrologicalYear := Source.FHydrologicalYear;
  FNominalOffset := Source.FNominalOffset;
  FActualOffset := Source.FActualOffset;
  FDateOffsetUnspecified := Source.FDateOffsetUnspecified;
  FTimeStepStrict := Source.FTimeStepStrict;
  Fid := Source.Fid;
  FModified := Source.FModified;
  FComment := Source.FComment;
  FTitle := Source.FTitle;
  FMUnit := Source.FMUnit;
  FPrecision := Source.Precision;
  FTimezone := Source.TimeZone;
end;

function TTimeseries.CompareMeta(ATimeseries: TTimeseries): Boolean;
begin
  Result := False;
  Result := Result or (FTimeStep <> ATimeseries.FTimeStep);
  Result := Result or (FVariable <> ATimeseries.FVariable);
  Result := Result or (FVarName <> ATimeseries.FVarName);
  Result := Result or (FVariableType <> ATimeseries.FVariableType);
  Result := Result or (FNominalOffset <> ATimeseries.FNominalOffset);
  Result := Result or (FActualOffset <> ATimeseries.FActualOffset);
  Result := Result or (FTimeStepStrict <> ATimeseries.FTimeStepStrict);
  Result := Result or (Fid <> ATimeseries.Fid);
  Result := Result or (FComment <> ATimeseries.FComment);
  Result := Result or (FTitle <> ATimeseries.FTitle);
  Result := Result or (FMUnit <> ATimeseries.FMUnit);
  Result := Result or (FPrecision <> ATimeseries.Precision);
  Result := Result or (FTimezone <> ATimeseries.TimeZone);
end;

resourcestring
  rsVariableTypesDontMatch = 'Variable types don''t match';

procedure TTimeseries.Merge(Source: TTimeseries);
var
  Target: TTimeseries;
  i, k: Integer;
begin
  if VariableType<>Source.VariableType then
    raise Exception.Create(rsVariableTypesDontMatch);
  Target := nil;
  try
    Target := TTimeseries.Create;
    Target.Assign(Self);
    for i := 0 to Source.Count-1 do
    begin
      k := PositionOfNext(Source[i].Date);
      if (k<>-1) and (DiffInSecs(Items[k].Date, Source[i].Date)=0) then
        Items[k].Assign(Source[i])
      else
        with Source[i] do Target.Insert(FDate, FIsNull, FValue, GetAllFlags,
          FMStatus);
    end;
    Assign(Target);
  finally
    Target.Free;
  end;
end;

procedure TTimeseries.MergeData(Source: TTimeseries; Overwrite: Boolean);
var
  i, j: Integer;
begin
  try
    for j := 0 to Source.Count-1 do
    begin
      i := IndexOf(Source[j].Date);
      if (i>=0) and (not Overwrite) then
        Continue;
      if i<0 then
        i := Insert(Source[j].Date, True, 0, '', msNew);
      with Items[i] do
      begin
        if not Source[j].IsNull then
          AsFloat := Source[j].AsFloat;
        SetAllFlags(Source[j].GetAllFlags);
        if MStatus <> msNew then MStatus := msModified;
      end;
    end;
  finally
    Modified := True;
  end;
end;

procedure TTimeseries.ImportData(AStream: TStream; Overwrite: Boolean);
begin
  ImportData(AStream, TEncoding.UTF8, Overwrite);
end;

procedure TTimeseries.ImportData(AStream: TStream; Encoding: TEncoding;
  Overwrite: Boolean);
var
  AStreamReader: TStreamReader;
begin
  AStreamReader := nil;
  try
    AStreamReader := TStreamReader.Create(AStream, Encoding);
    ImportData(AStreamREader, Overwrite);
  finally
    if AStreamReader<>nil then AStreamReader.Close;
    AStreamReader.Free;
  end;
end;

procedure TTimeseries.ImportData(AStreamReader: TStreamReader;
  Overwrite: Boolean);
var
  SavedDecimalSeparator: Char;
  i: Integer;
  ADate: TDateTime;
  s: string;
begin
  SavedDecimalSeparator := Sysutils.DecimalSeparator;
  try
    Sysutils.DecimalSeparator := '.';
    while not AStreamReader.EndOfStream do
    begin
      s := AStreamReader.ReadLine;
        ADate := FormatStrToDateTime('yyyy-mm-dd hh:nn',
        DelimitedStringItem(s, 1, ','));
      i := IndexOf(ADate);
      if (i>=0) and (not Overwrite) then
        Continue;
      if i<0 then
        i := Insert(ADate, True, 0, '', msUnmodified);
      with Items[i] do
      begin
        AsString := DelimitedStringItem(s, 2, ',');
        SetAllFlags(DelimitedStringItem(s, 3, ','), ' ');
        if MStatus<>msNew then MStatus := msModified;
      end;
    end;
  finally
    Sysutils.DecimalSeparator := SavedDecimalSeparator;
    Modified := True;
  end;
end;

procedure TTimeseries.ReadData(AStream: TStream;
  StartDate, EndDate: TDateTime;
  FirstLine: Integer; ProgressIndicator: TProgressIndicator;
  const ADecimalSeparator, ADelimiter, AFlagDelimiter: Char;
  const ADateFormat: string);
var
  AStreamReader: TStreamReader;
begin
  AStreamReader := nil;
  try
    AStreamReader := TStreamReader.Create(AStream, TEncoding.Default);
    ReadData(ASTreamREader, StartDate, EndDate, FirstLine, ProgressIndicator,
      ADecimalSeparator, ADelimiter, AFlagDelimiter, ADateFormat);
  finally
    if AStreamReader<>nil then AStreamReader.Close;
    AStreamReader.Free;
  end;
end;

resourcestring
  rsFileIsMixedUp = 'Records in file are incorrectly ordered.';
  rsPropertyNotFound = 'Property not found.';
  rsShouldHaveOneCharacter = 'should have exactly one character.';

procedure TTimeseries.ReadData(AStreamReader: TStreamReader;
  StartDate, EndDate: TDateTime;
  FirstLine: Integer; ProgressIndicator: TProgressIndicator;
  const ADecimalSeparator, ADelimiter, AFlagDelimiter: Char;
  const ADateFormat: string);
var
  SavedDecimalSeparator: Char;
  i, LineNo: Integer;
  ADate: TDateTime;
  ASize: Integer;
  ProgressIndicatorIsActive: Boolean;
  s: string;
begin
  SavedDecimalSeparator := Sysutils.DecimalSeparator;
  Sysutils.DecimalSeparator := ADecimalSeparator;
  LineNo := FirstLine;
  ASize := AStreamReader.BaseStream.Size;
  ProgressIndicatorIsActive := @ProgressIndicator<>nil;
  try try
    i := 0;
    while not AStreamReader.EndOfStream do
    begin
      s := AStreamReader.ReadLine;
      Inc(i);
      if  ProgressIndicatorIsActive then
        if (i div 1000)*1000 = i then
          ProgressIndicator(AStreamReader.BaseStream.Position div 1024,
            ASize div 1024);
      Inc(LineNo);
      ADate := FormatStrToDateTime(ADateFormat,
        DelimitedStringItem(s, 1, ADelimiter));
      if StartDate<>idaEmpty then
        if DiffInSecs(ADate, StartDate)<0 then
          Continue;
      if EndDate<>idaEmpty then
        if DiffInSecs(ADate, EndDate)>0 then
          Continue;
      if (Count>0)
      and (DiffInSecs(ADate, Last.Date)<=0) then
        raise Exception.Create(rsFileIsMixedUp);
      Add(ADate, True, 0, '', msUnmodified);
      Last.AsString := DelimitedStringItem(s, 2, ADelimiter);
      Last.SetAllFlags(DelimitedStringItem(s, 3, ADelimiter), AFlagDelimiter);
      Last.MStatus := msUnmodified;
    end;
    if ProgressIndicatorIsActive then ProgressIndicator(ASize div 100,
      ASize div 100);
  finally
    Sysutils.DecimalSeparator := SavedDecimalSeparator;
    Modified := False;
  end;
  except
    on E:Exception do
    begin
      E.Message := '('+IntToStr(LineNo)+'): '+E.Message;
      Clear;
      raise;
    end;
  end;
end;

procedure TTimeseries.ReadCompressedData(AStream: TStream; StartDate, EndDate:
  TDateTime; FirstLine: Integer; ProgressIndicator: TProgressIndicator;
  const ADecimalSeparator, ADelimiter, AFlagDelimiter: Char;
  const ADateFormat: string);
var
  AMemoryStream: TMemoryStream;
  AStreamReader: TStreamReader;
begin
  AMemoryStream := nil;
  AStreamReader := nil;
  try
    AMemoryStream := TMemoryStream.Create;
    DecompressStream(AStream, AMemoryStream, nil);
    AMemoryStream.Seek(0,0);
    AStreamReader := TStreamReader.Create(AMemoryStream, TEncoding.Default);
    ReadData(AStreamReader, StartDate, EndDate, FirstLine, ProgressIndicator,
      ADecimalSeparator, ADelimiter, AFlagDelimiter, ADateFormat);
  finally
    if AStreamReader<>nil then AStreamReader.Close;
    AStreamReader.Free;
    AMemoryStream.Free;
  end;
end;

function TTimeseries.ReadMeta(AStreamReader: TStreamReader; FirstLine: Integer;
  var ADecimalSeparator, ADelimiter, AFlagDelimiter: Char;
  var ADateFormat: string; FileVersion: Integer): Integer;
var
  s: string;
  LineNo: Integer;
  Preamble: TStringList;
  SavedDecimalSeparator: Char;

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
    if i>1 then
      if (Result[1]='"') and (Result[i]='"') then
        Result := Copy(Result, 2, i-2);
  end;

  function ValueExists(AStringList: TStringList; Name: string): Boolean;
  begin
    Result := AStringList.IndexOfName(LowerCase(Name))>-1
  end;

  function GetMultiLineValue(AStringList: TStringList; Name: string): string;
  var
    i, j: Integer;
    s: string;
  begin
    Result := '';
    i := AStringList.IndexOfName(LowerCase(Name));
    while i>=0 do
    begin
      s := AStringList.Values[LowerCase(Name)];
      AStringList.Delete(i);
      j := Length(s);
      if j>1 then
        if (s[1]='"') and (s[j]='"') then
          s := Copy(s, 2, j-2);
      Result := Result + s;
      i := AStringList.IndexOfName(LowerCase(Name));
      if i>=0 then  Result := Result+#13#10;
    end;
  end;

  function StrToChar(AString: string): Char;
  begin
    if Length(AString)<>1 then
      raise EConvertError.Create(AString+' '+rsShouldHaveOneCharacter);
    Result := AString[1];
  end;

  function GetTimestepFromOrdinalValue(AValue: Integer): TTimestep;
  var i: Integer;
  begin
    Result := tstUnknown;
    for i := 0 to Length(AvailableTimesteps)-1 do
      if AvailableTimesteps[i].OrdinalValue = AValue then
      begin
        Result := AvailableTimesteps[i];
        Break;
      end;
  end;

  procedure ReadVersion1Section;
  begin
    ADelimiter := StrToChar(GetValue(Preamble, 'Delimiter'));
    AFlagDelimiter := StrToChar(GetValue(Preamble, 'FlagDelimiter'));
    ADecimalSeparator := StrToChar(GetValue(Preamble, 'DecimalSeparator'));
    SysUtils.DecimalSeparator := ADecimalSeparator;
    ADateFormat := GetValue(Preamble, 'DateFormat');
    FFileVersion := 1;
    TimeStep := GetTimestepFromOrdinalValue(
      StrToInt(GetValue(Preamble, 'TimeStep')));
    MUnit := GetValue(Preamble, 'MUnit');
    DateOffset := 0;
    if TimeStep>=tstMonthly then
    begin
      if GetValue(Preamble, 'DateOffset')='' then
      begin
        FDateOffsetUnspecified := True;
      end else begin
        FDateOffsetUnspecified := False;
        DateOffset := StrToFloat(GetValue(Preamble, 'DateOffset'));
      end;
    end;
    if TimeStep=tstAnnual then
      SetHydrologicalYear(StrToBool(GetValue(Preamble, 'HydrologicalYear')));
    if (TimeStep<tstMonthly) and (not TimeStep.IsVariable) then
      TimeStepStrict := StrToBool(GetValue(Preamble, 'TimeStepStrict'));
    Variable := StrToInt(GetValue(Preamble, 'Variable'));
    VariableType := StrToVarType(GetValue(Preamble, 'VariableType'));
    Comment := GetMultiLineValue(Preamble, 'Comment');
    Title := GetValue(Preamble, 'Title');
  end;

  procedure ReadVersion2Section;
  var
    vtName: string;
    i: Integer;
  begin
    ADelimiter := ',';
    AFlagDelimiter := ' ';
    ADecimalSeparator := '.';
    SysUtils.DecimalSeparator := ADecimalSeparator;
    ADateFormat := 'yyyy-mm-dd hh:nn';
    FFileVersion := 2;
    MUnit := '';
    if ValueExists(Preamble, 'Unit') then
      MUnit := GetValue(Preamble, 'Unit');
    Title := '';
    if ValueExists(Preamble, 'Title') then
      Title := GetValue(Preamble, 'Title');
    Comment := '';
    if ValueExists(Preamble, 'Comment') then
      Comment := GetMultiLineValue(Preamble, 'Comment');
    Timezone := '';
    if ValueExists(Preamble, 'Timezone') then
      TimeZone := GetValue(Preamble, 'Timezone');
    if ValueExists(Preamble, 'Time_step') then
      TimeStep := TTimestep.Create(
        StrToInt(DelimitedStringItem(GetValue(Preamble, 'Time_step'),1,',')),
        StrToInt(DelimitedStringItem(GetValue(Preamble, 'Time_step'),2,',')),0)
    else
      TimeStep := tstVariable;
    if TimeStep<>tstVariable then
      if ValueExists(Preamble, 'Nominal_offset') then
      begin
        TimeStepStrict := True;
        if TimeStep = tstAnnual then
        FNominalOffset.Store(0, StrToInt(DelimitedStringItem(GetValue(Preamble,
          'Nominal_Offset'),2,',')));
      end else
        TimeStepStrict := False;
    if TimeStep<>tstVariable then
      if ValueExists(Preamble, 'Actual_offset') then
        FActualOffset.Store(
          StrToInt(DelimitedStringItem(GetValue(Preamble, 'Actual_offset'),1,
            ',')),
          StrToInt(DelimitedStringItem(GetValue(Preamble, 'Actual_offset'),2,
          ',')));
    vtName := '';
    if ValueExists(Preamble, 'Interval_type') then
      vtName := GetValue(Preamble, 'Interval_type');
    if vtName = '' then VariableType := vtInstantaneous
    else if vtName = 'sum' then VariableType := vtCumulative
    else if vtName = 'average' then VariableType := vtAverage
    else if vtName = 'minimum' then VariableType := vtMinimum
    else if vtName = 'maximum' then VariableType := vtMaximum
    else if vtName = 'vector_average' then VariableType := vtVectorAverage;
    VarName := '';
    if ValueExists(Preamble, 'Variable') then
      VarName := GetValue(Preamble, 'Variable');
    Precision := tsDefaultPrecision;
    if ValueExists(Preamble, 'Precision') then
      Precision := StrToInt(GetValue(Preamble, 'Precision'));
    {Set cardinal value of time step for backwards compatibility}
    for i := 0 to Length(AvailableTimesteps)-1 do
      if AvailableTimesteps[i] = FTimeStep then
      begin
        FTimeStep.OrdinalValue := AvailableTimesteps[i].OrdinalValue;
        Break;
      end;
  end;

begin
  Assert(FileVersion in [1,2]);
  LineNo := FirstLine-1;
  Preamble := nil;
  SavedDecimalSeparator := SysUtils.DecimalSeparator;
  try try
    Preamble := TStringList.Create;
    Clear;
    while True do
    begin
      s := AStreamReader.ReadLine;
      Inc(LineNo);
      if s='' then Break;
      Preamble.Add(s);
    end;
    if FileVersion=1 then
      ReadVersion1Section
    else if FileVersion=2 then
      ReadVersion2Section;
    Result := LineNo;
  finally
    SysUtils.DecimalSeparator := SavedDecimalSeparator;
    Preamble.Free;
  end;
  except
    on E:Exception do
    begin
      E.Message := '('+IntToStr(LineNo)+'): '+E.Message;
      Clear;
      raise;
    end;
  end;
end;

procedure TTimeseries.LoadFromStream(AStream: TStream; StartDate,
  EndDate: TDateTime; ProgressIndicator: TProgressIndicator;
  FileVersion: Integer; DoReadData: Boolean);
var
  ADecimalSeparator, ADelimiter, AFlagDelimiter: Char;
  ADateFormat: string;
  ALineNo: Integer;
  AStreamReader: TStreamReader;
  AEncoding: TEncoding;
begin
  Assert(FileVersion in [1,2]);
  AEncoding := nil; //to eliminate compiler warnings
  case FileVersion of
    1: AEncoding := TEncoding.Default;
    2: AEncoding := TEncoding.UTF8;
  end;
  ALineNo := 1;
  AStreamReader := nil;
  try
    AStreamReader := TStreamReader.Create(AStream, AEncoding);
    ALineNo := ReadMeta(AStreamReader, ALineNo, ADecimalSeparator, ADelimiter,
      AFlagDelimiter, ADateFormat, FileVersion);
    if DoReadData then
      ReadData(AStreamReader, StartDate, EndDate, ALineNo, ProgressIndicator,
        ADecimalSeparator, ADelimiter, AFlagDelimiter, ADateFormat);
  finally
    if AStreamReader<>nil then AStreamReader.Close;
    AStreamReader.Free;
  end;
end;

procedure TTimeseries.LoadFromFile(AFileName: string;
  ProgressIndicator: TProgressIndicator; FileVersion: Integer);
var
  AFileStream: TFileStream;
  AMemoryStream: TMemoryStream;
begin
  Assert(FileVersion in [0,1,2]);
  if FileVersion = 0 then FileVersion := SenseVersion(AFileName);
  AFileStream := nil;
  AMemoryStream := nil;
  try
    AFileStream := TFileStream.Create(AFileName,
      fmOpenRead or fmShareDenyWrite);
    AMemoryStream := TMemoryStream.Create;
    AMemoryStream.LoadFromStream(AFileStream);
    AMemoryStream.Seek(0,0);
    LoadFromStream(AMemoryStream, idaEmpty, idaEmpty, ProgressIndicator,
      FileVersion);
    FFileName := AFileName;
  finally
    AFileStream.Free;
    AMemoryStream.Free;
  end;
end;

procedure TTimeseries.LoadMetaFromFile(AFileName: string);
var
  AFileStream: TFileStream;
  AMemoryStream: TMemoryStream;
  FileVersion: Integer;
begin
  FileVersion := SenseVersion(AFileName);
  AFileStream := nil;
  AMemoryStream := nil;
  try
    AFileStream := TFileStream.Create(AFileName,
      fmOpenRead or fmShareDenyWrite);
    AMemoryStream := TMemoryStream.Create;
    AMemoryStream.LoadFromStream(AFileStream);
    AMemoryStream.Seek(0,0);
    LoadFromStream(AMemoryStream, idaEmpty, idaEmpty, nil, FileVersion, False);
    FFileName := AFileName;
  finally
    AFileStream.Free;
    AMemoryStream.Free;
  end;
end;

function TTimeseries.SenseVersion(AFileName: string): Integer;
var
  F: TextFile;
  SavedFileMode: Integer;
  s: string;
begin
  Result := 1;
  SavedFileMode := FileMode;
  try
    FileMode := fmOpenRead or fmShareDenyWrite;
    AssignFile(F, AFileName);
    Reset(F);
    ReadLn(F, s);
    s := LowerCase(s);
    if StrPos(PChar(s), 'version=2')<> nil then Result := 2;
    Close(F);
  finally
    FileMode := SavedFileMode;
  end;
end;

procedure TTimeseries.WriteData(AStream: TStream; ProgressIndicator:
TProgressIndicator);
begin
  WriteData(AStream, 0, Count-1, ProgressIndicator);
end;

procedure TTimeseries.WriteData(AStream: TStream; AStart, AEnd: Integer;
  ProgressIndicator: TProgressIndicator);
var
  AStreamWriter: TStreamWriter;
begin
  AStreamWriter := nil;
  try
    AStreamWriter := TStreamWriter.Create(AStream, TEncoding.Default);
    WriteData(AStreamWriter, AStart, AEnd, ProgressIndicator);
  finally
    if AStreamWriter<>nil then AStreamWriter.Close;
    AStreamWriter.Free;
  end;
end;

procedure TTimeseries.WriteData(AStreamWriter: TStreamWriter; ProgressIndicator:
TProgressIndicator);
begin
  WriteData(AStreamWriter, 0, Count-1, ProgressIndicator);
end;

procedure TTimeseries.WriteData(AStreamWriter: TStreamWriter;
  AStart, AEnd: Integer; ProgressIndicator: TProgressIndicator);
var
  i: Integer;
  SavedDecimalSeparator: Char;
  Delimiter, FlagDelimiter: Char;
  DateFormat: string;
  ProgressIndicatorIsActive: Boolean;
begin
  SavedDecimalSeparator := Sysutils.DecimalSeparator;
  ProgressIndicatorIsActive := @ProgressIndicator<>nil;
  try
    Sysutils.DecimalSeparator := '.';
    Delimiter := ',';
    FlagDelimiter := ' ';
    DateFormat := 'yyyy-mm-dd hh:nn';
    for i := AStart to AEnd do
    begin
      if ProgressIndicatorIsActive then
        if (i div 1000)*1000 = i then
          ProgressIndicator(i, Count);
      AStreamWriter.WriteLine(FormatDateTime(DateFormat, Items[i].Date)+
        Delimiter+Items[i].AsString+Delimiter+
        Items[i].GetAllFlags(FlagDelimiter));
    end;
    if ProgressIndicatorIsActive then
      ProgressIndicator(Count, Count);
  finally
    Sysutils.DecimalSeparator := SavedDecimalSeparator;
  end;
end;

procedure TTimeseries.WriteCompressedData(AStream: TStream; ProgressIndicator:
TProgressIndicator);
begin
  WriteCompressedData(AStream, 0, Count-1, ProgressIndicator);
end;

procedure TTimeseries.WriteCompressedData(AStream: TStream; AStart, AEnd:
  Integer; ProgressIndicator: TProgressIndicator);
var
  AMemoryStream: TMemoryStream;
  AStreamWriter: TStreamWriter;
begin
  AMemoryStream := nil;
  AStreamWriter := nil;
  try
    AMemoryStream := TMemoryStream.Create;
    AStreamWriter := TStreamWriter.Create(AMemoryStream, TEncoding.Default);
    WriteData(AStreamWriter, AStart, AEnd, ProgressIndicator);
    if AStreamWriter<>nil then AStreamWriter.Close;
    AMemoryStream.Seek(0,0);
    CompressStream(AMemoryStream, AStream, nil);
  finally
    AStreamWriter.Free;
    AMemoryStream.Free;
  end;
end;

procedure TTimeseries.WriteMeta(AStreamWriter: TStreamWriter;
  FileVersion: Integer);
var
  AStringList: TStringList;

  procedure WriteVersion1;
  var
    i: Integer;
  begin
    with AStreamWriter do
    begin
      WriteLine('Delimiter=","');
      WriteLine('FlagDelimiter=" "');
      WriteLine('DecimalSeparator="."');
      WriteLine('DateFormat="yyyy-mm-dd hh:nn"');
      WriteLine('MUnit="'+MUnit+'"');
      WriteLine('Count='+IntToStr(Count));
      WriteLine('TimeStep='+IntToStr(FTimeStep.OrdinalValue));
      if FTimeStep>=tstMonthly then
        if DateOffset=0 then
          WriteLine('DateOffset=')
        else
          WriteLine('DateOffset='+FloatToStr(DateOffset));
      if (FTimeStep<tstMonthly) and (Timestep.OrdinalValue<>0) then
        WriteLine('TimeStepStrict='+BoolToStr(FTimeStepStrict));
      if FTimeStep=tstAnnual then
        WriteLine('HydrologicalYear='+BoolToStr(FHydrologicalYear));
      WriteLine('Flags="'+OldFlagsMeta+'"');
      WriteLine('Variable='+IntToStr(FVariable));
      WriteLine('VariableType='+VarTypeToStr(FVariableType));
      WriteLine('Title="'+Title+'"');
      AStringList.Text := FComment;
      for i := 0 to AStringList.Count-1 do
        WriteLine('Comment="'+AStringList[i]+'"');
    end
  end;

  procedure WriteVersion2;
  var
    i: Integer;
  begin
    with AStreamWriter do
    begin
      WriteLine('Version=2');
      if MUnit<>'' then WriteLine('Unit='+MUnit);
      WriteLine('Count='+IntToStr(Count));
      if Title<>'' then WriteLine('Title='+Title);
      AStringList.Text := FComment;
      if FComment<>'' then
        for i := 0 to AStringList.Count-1 do
          WriteLine('Comment='+AStringList[i]);
      if TimeZone<>'' then WriteLine('Timezone='+TimeZone);
      with TimeStep do
        if not IsVariable then
          WriteLine('Time_step='+IntToStr(LengthMinutes)+','+
            IntToStr(LengthMonths));
      if (not TimeStep.IsVariable) and TimeStepStrict then
        with NominalOffset do
          WriteLine('Nominal_offset='+IntToStr(Minutes)+','+
            IntToStr(Months));
      if not TimeStep.IsVariable then
        with ActualOffset do
            WriteLine('Actual_offset='+IntToStr(Minutes)+','+
              IntToStr(Months));
      case VariableType of
        vtCumulative: WriteLine('Interval_type=sum');
        vtAverage: WriteLine('Interval_type=average');
        vtMaximum: WriteLine('Interval_type=maximum');
        vtMinimum: WriteLine('Interval_type=minimum');
        vtVectorAverage: WriteLine('Interval_type=vector_average');
      end;
      if VarName<>'' then WriteLine('Variable='+VarName);
      WriteLine('Precision='+IntToStr(Precision));
    end;
  end;

begin
  Assert(FileVersion in [1,2]);
  AStringList := nil;
  try
    AStringList := TStringList.Create;
    if FileVersion=1 then
      WriteVersion1
    else if FileVersion=2 then
      WriteVersion2;
    AStreamWriter.WriteLine('');
  finally
    AStringList.Free;
  end;
end;

procedure TTimeseries.WriteToStream(AStream: TStream;
  ProgressIndicator: TProgressIndicator; FileVersion: Integer);
var
  AStreamWriter: TStreamWriter;
  AEncoding: TEncoding;
begin
  Assert(FileVersion in [1,2]);
  AEncoding := nil; //to eliminate compiler warnings
  case FileVersion of
    1: AEncoding := TEncoding.Default;
    2: AEncoding := TEncoding.UTF8;
  else Assert(False);
  end;
  AStreamWriter := nil;
  try
    AStreamWriter := TStreamWriter.Create(AStream, AEncoding);
    WriteMeta(AStreamWriter, FileVersion);
    WriteData(AStreamWriter, ProgressIndicator);
  finally
    if AStreamWriter<>nil then AStreamWriter.Close;
    AStreamWriter.Free;
  end;
end;

procedure TTimeseries.WriteToFile(AFileName: string;
  ProgressIndicator: TProgressIndicator; FileVersion: Integer);
var
  AFileStream: TFileStream;
  AMemoryStream: TMemoryStream;
begin
  Assert(FileVersion in [1,2]);
  AFileStream := nil;
  AMemoryStream := nil;
  try
    AFileStream := TFileStream.Create(AFileName, fmCreate);
    AMemoryStream := TMemoryStream.Create;
    WriteToStream(AMemoryStream, ProgressIndicator, FileVersion);
    AMemoryStream.Seek(0,0);
    AFileStream.CopyFrom(AMemoryStream,0);
    FFileName := AFileName;
    FFileVersion := FileVersion;
  finally
    AFileStream.Free;
    AMemoryStream.Free;
  end;
end;

function TTimeseries.Mean: Double;
var
  i, NotNullCount, PartCount: Integer;
  s: Double;
begin
  NotNullCount := 0;
  Result := 0;
  s := 0;
  PartCount := 0;
  for i := 0 to Count-1 do
    if not Items[i].IsNull then
    begin
      Inc(NotNullCount);
      Inc(PartCount);
      s := s+Items[i].AsFloat;
      if PartCount=100 then
      begin
        Result := Result + s;
	s := 0;
	PartCount := 0;
      end;
    end;
  Result := Result + s;
  Result := Result/NotNullCount;
end;

function TTimeseries.Mean(ADateTimeList: TDateTimeList): Double;
var i, k, PartCount: Integer;
    s: Double;
begin
  Result := 0;
  PartCount := 0;
  s := 0;
  for i := 0 to ADateTimeList.Count-1 do
  begin
    k := IndexOf(ADateTimeList[i]);
    s := s + Items[k].AsFloat;
    Inc(PartCount);
    if PartCount = 100 then
    begin
      Result := Result+s;
      s := 0;
      PartCount := 0;
    end;
  end;
  Result := Result+s;
  Result := Result/ADateTimeList.Count;
end;

function TTimeseries.Variance: Double;
var
  m, s: Double;
  i, NotNullCount, PartCount: Integer;
begin
  m := Mean;
  Result := 0;
  NotNullCount := 0;
  s := 0;
  PartCount := 0;
  for i := 0 to Count-1 do
    if not Items[i].IsNull then
    begin
      s := s + Sqr(Items[i].AsFloat - m);
      Inc(NotNullCount);
      Inc(PartCount);
      if PartCount=100 then
      begin
        Result := Result + s;
        s := 0;
        PartCount := 0;
      end;
    end;
  Result := Result + s;
  Result := Result/(NotNullCount-1);
end;

function TTimeseries.Variance(ADateTimeList: TDateTimeList): Double;
var
  i, k, PartCount: Integer;
  m, s: Double;
begin
  m := Mean(ADateTimeList);
  Result := 0;
  s := 0;
  PartCount := 0;
  for i := 0 to ADateTimeList.Count-1 do
  begin
    k := IndexOf(ADateTimeList[i]);
    s := s + Sqr(Items[k].AsFloat-m);
    Inc(PartCount);
    if PartCount=100 then
    begin
      Result := Result + s;
      s := 0;
      PartCount := 0;
    end;
  end;
  Result := Result + s;
  Result := Result/(ADateTimeList.Count-1);
end;

type
TOneStepAggregate = record
  Value: Real;
  CountMissing: Real;
  Flag: string;
  IsNull: Boolean;
end;

procedure TTimeseries.Aggregate(Dest, Missing: TTimeseries;
  Options: TAggregationOptionsRec; ProgressIndicator: TProgressIndicator);

  function timedeltadivide(a, b: TDateTime): Real;
  begin
    Result := a/b;
  end;

  function AggregateOneStep(ADateTime: TDateTime): TOneStepAggregate;
  var
    DStartDate, DEndDate: TDateTime;
    SStartDate, SEndDate: TDateTime;
    AStartNominal, AEndNominal: TDateTime;
    AIntervalType: TVariableType;
    ATime: TDateTime;
    x, y: Real;
    AIndex: Integer;
    AUsedInterval, AUnusedInterval, AOut: TDateTime;
    ADivider, ATotalComponents, APctUsed: Real;

    procedure AssessDestInterval;
    var
      AYear: Integer;
    begin
      with Options do
        if not SeasonalAggregation then
        begin
          DStartDate := Dest.IntervalStartPoint(ADateTime);
          DEndDate := Dest.IntervalEndPoint(ADateTime);
        end else
        begin
          AYear := YearOf(ADateTime);
          if FromMonth<Dest.NominalOffset.Months+1 then
            Inc(AYear);
          DStartDate := EncodeDate(AYear, FromMonth, FromDay);
          if ToMonth<=FromMonth then
            Inc(AYear);
          DEndDate := EncodeDate(AYear, ToMonth, ToDay);
          DStartDate := AddDateTime(DStartDate, Dest.ActualOffset.Minutes/1440);
          DEndDate := AddDateTime(DEndDate, Dest.ActualOffset.Minutes/1440);
        end;
    end;

  begin
    Result.IsNull := False;
    Result.Flag := '';
    Result.CountMissing := 0;
    x := 0;
    y := 0;
    AssessDestInterval;
    AStartNominal := ContainingInterval(DStartDate);
    AEndNominal := ContainingInterval(DEndDate);
    ATime := AStartNominal;
    AIntervalType := Dest.VariableType;
    if AIntervalType in [vtCumulative, vtAverage, vtInstantaneous] then
      Result.Value := 0
    else if AIntervalType = vtMaximum then
      Result.Value := -1e38
    else if AIntervalType = vtMinimum then
      Result.Value := 1e38
    else if AIntervalType = vtVectorAverage then
    begin
      x := 0;
      y := 0;
    end else
      Assert(False);
    ATotalComponents := 0;
    ADivider := 0;
    while DiffInSecs(ATime, AEndNominal)<=0 do
    begin
      SStartDate := IntervalStartPoint(ATime);
      SEndDate := IntervalEndPoint(ATime);
      AUsedInterval := SubtractDateTime(SEndDate, SStartDate);
      AUnusedInterval := 0;
      if DiffInSecs(SStartDate, DStartDate)<0 then
      begin
        AOut := SubtractDateTime(DStartDate, SStartDate);
        AUnusedInterval := AddDateTime(AUnusedInterval, AOut);
        AUsedInterval := AddDateTime(AUsedInterval, -AOut);
      end;
      if DiffInSecs(SEndDate, DEndDate)>0 then
      begin
        AOut := SubtractDateTime(SEndDate, DEndDate);
        AUnusedInterval := AddDateTime(AUnusedInterval, AOut);
        AUsedInterval := AddDateTime(AUsedInterval, -AOut);
      end;
      APctUsed := timedeltadivide(AUsedInterval, AddDateTime(AUsedInterval,
        AUnusedInterval));
      ATotalComponents := ATotalComponents + APctUsed;
      AIndex := IndexOf(ATime);
      if (AIndex<0) or Items[AIndex].IsNull then
      begin
        Result.CountMissing := Result.CountMissing + APctUsed;
        ATime := NextTimestamp(ATime);
        Continue;
      end;
      ADivider := ADivider + APctUsed;
      if AIntervalType in [vtCumulative, vtAverage] then
        Result.Value := Result.Value + Items[AIndex].AsFloat*APctUsed
      else if AIntervalType = vtMaximum then
      begin
        if APctUsed>0 then
          Result.Value := Max(Result.Value, Items[AIndex].AsFloat)
      end
      else if AIntervalType = vtMinimum then
      begin
        if APctUsed>0 then
          Result.Value := Min(Result.Value, Items[AIndex].AsFloat)
      end
      else if AIntervalType = vtVectorAverage then
      begin
        x := x+Cos(Items[AIndex].AsFloat*Pi/180);
        y := y+Sin(Items[AIndex].AsFloat*Pi/180);
      end
      else if AIntervalType = vtInstantaneous then
      begin
        ATotalComponents := 1;
        Result.CountMissing := 1;
        AIndex := IndexOf(AEndNominal);
        if AIndex<0 then Break;
        if Items[AIndex].IsNull then Break;
        Result.Value := Items[AIndex].AsFloat;
        Result.CountMissing := 0;
        Break;
      end else Assert(False);
      ATime := NextTimestamp(ATime);
    end;
    if ((Result.CountMissing / ATotalComponents >
      Options.MissingAllowed / ATotalComponents + 1e-5)) or
        (Abs(Result.CountMissing - ATotalComponents)<1e-36 ) then
      Result.IsNull := True
    else begin
      if Result.CountMissing/ATotalComponents>1e-36 then
        Result.Flag := Options.MissingFlag;
      if Dest.VariableType = vtAverage then
        Result.Value := Result.Value / ADivider
      else if Dest.VariableType = vtVectorAverage then
      begin
        if x=0 then x:=1e-5;
        Result.Value := ArcTan2(y, x)*180/Pi;
        if Result.Value<0 then Result.Value := Result.Value + 360;
      end;
    end;
  end;

var
  SourceStartDate, SourceEndDate: TDateTime;
  TargetStartDate, TargetEndDate: TDateTime;
  ADate: TDateTime;
  AOneStepAggregate: TOneStepAggregate;
  AStep, ACount, ACountDiv: Integer;

begin
  if Count<1 then Exit;
  Assert(Dest<>nil);
  Assert(Dest.Count=0);
  if Missing<>nil then Assert(Missing.Count=0);
  if Missing<>nil then Assert(Missing.TimeStep = Dest.TimeStep);
  with Options do
    if SeasonalAggregation then
    begin
      if SeasonalAggregation then Assert(Dest.TimeStep=tstAnnual);
      if FromDay=0 then FromDay := 1;
      if FromDay=32 then begin Inc(FromMonth); FromDay := 1; end;
      if ToDay=0 then ToDay := 1;
      if ToDay=32 then begin Inc(ToMonth); ToDay := 1; end;
      if FromMonth>12 then FromMonth := 1;
      if ToMonth>12 then ToMonth := 1;
    end;
  Assert(not Dest.TimeStep.IsVariable);
  Dest.TimeStepStrict := True;
  SourceStartDate := First.Date;
  SourceEndDate := Last.Date;
  TargetStartDate := Dest.PreviousTimestamp(SourceStartDate);
  TargetEndDate := Dest.NextTimestamp(SourceEndDate);
  ACount := Round(SubtractDateTime(TargetEndDate, TargetStartDate)/
    SubtractDateTime(Dest.TimeStep.IncStep(TargetEndDate), TargetEndDate));
  ACountDiv := Max(1, ACount div 100);
  AStep := 0;
  ADate := TargetStartDate;
  while DiffInSecs(ADate, TargetEndDate)<=0 do
  begin
    AOneStepAggregate := AggregateOneStep(ADate);
    with AOneStepAggregate do
    begin
      if IsNull then
        Dest.Insert(ADate, True, 0, Flag, msNew)
      else
        Dest.Insert(ADate, False, Value, Flag, msNew);
      if Missing<>nil then
        Missing.Insert(ADate, False, CountMissing, '', msNew);
    end;
    ADate := Dest.NextTimestamp(ADate);
    Inc(AStep);
    if @ProgressIndicator<>nil then
      if AStep mod ACountDiv =0 then
      ProgressIndicator(AStep, ACount);
  end;
  if Missing<>nil then Assert(Dest.Count = Missing.Count);
  if Options.DeleteNullEnds then
  begin
    while (Dest.Count>0) and (Dest.First.IsNull) do
    begin
      Dest.Delete(0);
      if Missing<>nil then Missing.Delete(0);
    end;
    while (Dest.Count>0) and (Dest.Last.IsNull) do
    begin
      Dest.Delete(Dest.Count-1);
      if Missing<>nil then Missing.Delete(Missing.Count-1);
    end;
  end;
end;

function TTimeseries.CountNotNull: Integer;
var i: Integer;
begin
  Result := 0;
  for i := 0 to Count-1 do
    if not Items[i].IsNull then
      Inc(Result);
end;

{  WARNING 2009-12-08: This function is deprecated and not maintained
   anymore. It is kept since I don't know if it is used in
   hydronomeas.
}
function TTimeseries.FindTimeStep: TTimestep;
var
  CouldBeMonthly, CouldBeYearlyCalendar, CouldBeYearlyHydrol: Boolean;
  MinInterval, d: Int64;
  i: Integer;
  Year, Month, Day, Hour, Minute, Second, Ms: Word;
begin
  Result := tstUnknown;
  if Count<=1 then Exit;

  CouldBeMonthly := True;
  CouldBeYearlyCalendar := True;
  CouldBeYearlyHydrol := True;
  MinInterval := MaxInt;

  { Run through once to determine MinInterval or whether monthly etc. }
  for i := 1 to Count-1 do
  begin
    DecodeDate(Items[i].Date, Year, Month, Day);
    DecodeTime(Items[i].Date, Hour, Minute, Second, Ms);
    if (Minute<>0) or (Hour<>0) or (Day<>1) or (Month<>1) then
      CouldBeYearlyCalendar := False;
    if (Minute<>0) or (Hour<>0) or (Day<>1) or (Month<>10) then
      CouldBeYearlyHydrol := False;
    if (Minute<>0) or (Hour<>0) or (Day<>1) then
      CouldBeMonthly := False;
    d := DiffInSecs(Items[i].Date, Items[i-1].Date);
    if d<MinInterval then MinInterval := d;
  end;
  Result := tstYearly;
  if CouldBeYearlyCalendar or CouldBeYearlyHydrol then Exit;
  Result := tstMonthly;
  if CouldBeMonthly then Exit;

  Result := tstVariable;
  { Run through second time to see whether only multiples if MinInterval. }
  for i := 1 to Count-1 do
  begin
    d := DiffInSecs(Items[i].Date, Items[i-1].Date);
    if (d mod MinInterval) <> 0 then Exit;
  end;

  case MinInterval of
    300: Result := tstFiveMinute;
    600: Result := tstTenMinute;
    3600: Result := tstHourly;
    86400: Result := tstDaily;
  else
    Result := TTimestep.Create(MinInterval,0);
  end;
end;

resourcestring
  rsTimeStepNotAnnual = 'Time step is not annual.';

function TTimeseries.GetHydrologicalYear: Boolean;
begin
  if TimeStep<>tstAnnual then
    raise Exception.Create(rsTimeStepNotAnnual);
  Result := FNominalOffset.Months<>0;
end;

resourcestring
  rsCannotSetHydrologicalYearToANonEmpty =
    'Cannot alter hydrological year property in a non empty time series';

procedure TTimeseries.SetHydrologicalYear(Value: Boolean);
begin
  if TimeStep<>tstYearly then
    raise Exception.Create(rsTimeStepNotAnnual);
  if Count>0 then
    raise Exception.Create(rsCannotSetHydrologicalYearToANonEmpty);
  if HydrologicalYear=Value then Exit;
  FHydrologicalYear := Value;
  if FHydrologicalYear then
    FNominalOffset.Store(0,9);
  FModified := True;
  DisplayFullInvalidate;
end;

function TTimeseries.GetDateOffset: Real;
begin
  if TimeStep<tstMonthly then
    Result := NominalOffset.Minutes/1440
  else
    Result := ActualOffset.Minutes/1440;
end;

resourcestring
  rsCantSetDateOffset = 'DateOffset can only be set for monthly or annual '+
    'time series.';

procedure TTimeseries.SetDateOffset(Value: Real);
var
  Months: Integer;
begin
  Months := 0;
  if TimeStep=tstMonthly then
    Months := 1
  else if TimeStep=tstAnnual then
    Months := 12;
  ActualOffset.Store(Round(Value*1440), Months);
  FModified := True;
  DisplayFullInvalidate;
end;

resourcestring
  rsCantSetTimeStepStrict = 'Can''t set time step strict. Record nr. %d '+
    'with Date=%s does not fit';
  rsMonthlyAndAnnualAreAlwaysStrict =
    'Time steps greater or equal than monthly are always considered strict.';

function TTimeseries.CheckTimeStepStrict(var ErrMessage: string): Boolean;
var
  i: Integer;
  FSavedNominalOffset: TDateOffset;
begin
  ErrMessage := '';
  Result := True;
  FSavedNominalOffset := FNominalOffset;
  try
    AdjustDateOffset(True);
    for i := 0 to Count-1 do
      if not CheckIfDateFits(Items[i].Date, True) then
      begin
        ErrMessage := Format(rsCantSetTimeStepStrict,
          [i+1, Items[i].DateAsString]);
        Result := False;
        Break;
      end;
  finally
    FNominalOffset := FSavedNominalOffset;
  end;
end;

procedure TTimeseries.SetTimeStepStrict(Value: Boolean);
var
  s: string;
begin
  if FTimeStepStrict=Value then Exit;
  if (not Value) and (TimeStep>=tstMonthly) then
    raise Exception.Create(rsMonthlyAndAnnualAreAlwaysStrict);
  if (Value) and (not CheckTimeStepStrict(s)) then
    raise Exception.Create(s);
  FTimeStepStrict := Value;
  AdjustDateOffset;
  FModified := True;
  DisplayFullInvalidate;
end;

resourcestring
  rsCantSetTimeStep = 'Cannot change time step of a non empty time series';

procedure TTimeseries.SetTimeStep(Value: TTimeStep);
begin
  if FTimeStep=Value then Exit;
  if Count>0 then raise Exception.Create(rsCantSetTimeStep);
  FTimeStep := Value;
  if FTimeStep>=tstMonthly then
    FTimeStepStrict := True;
  if FTimeStep.IsVariable then FTimeStepStrict := False;
  FModified := True;
  DisplayFullInvalidate;
end;

function TTimeseries.CheckIfDateFits(ADate: TDateTime;
  AlwaysCheck: Boolean): Boolean;
var
  Day, Month, Year, Hour, Min, Sec, MSec: Word;
  MonthCount: Integer;
  ts, d: Int64;
begin
  if (TimeStep=tstUnknown) or (TimeStep=tstVariable) or
    ((not TimeStepStrict) and (not AlwaysCheck)) then
  begin
    Result := True;
    Exit;
  end else if Timestep>=tstMonthly then
  begin
    Result := False;
    DecodeDate(ADate, Year, Month, Day);
    DecodeTime(ADate, Hour, Min, Sec, MSec);
    if Timestep=tstAnnual then
      if Month<>1+FNominalOffset.Months then Exit;
    if (Day<>1) or (Hour<>0) or (Min<>0) then Exit;
    if Count>0 then
    begin
      MonthCount := Abs(Round(DiffInMonths(First.Date, ADate)));
      if (MonthCount<TimeStep.LengthMonths) and (MonthCount>0) then Exit;
      if (MonthCount mod TimeStep.LengthMonths)<>0 then
        Exit;
    end;
    Result := True;
  end else
  begin
    Result := True;
    if Count=0 then Exit;
    Result := False;
    ts := TimeStep.FLengthMinutes*60;
    d := Round(DateTimeToC(ADate)*86400);
    if (d mod ts) <> Round(FNominalOffset.Minutes*60) then Exit;
    Result := True;
  end;
end;

procedure TTimeseries.SetVariableType(Value: TVariableType);
begin
  if FVariableType=Value then Exit;
  FVariableType := Value;
  FModified := True;
  DisplayFullInvalidate;
end;

procedure TTimeseries.SetModified(Value: Boolean);
var i: Integer;
begin
  if FModified=Value then Exit;
  DisplayInvalidate;
  FModified := Value;
  if FModified then Exit;
  for i := Count-1 downto 0 do
    Items[i].MStatus := msUnmodified;
  FModified := False;
end;

procedure TTimeseries.SetTitle(Value: string);
begin
  if FTitle=Value then Exit;
  FTitle := Value;
  FModified := True;
  DisplayInvalidate;
end;

procedure TTimeseries.SetPrecision(Value: integer);
begin
  if Value>18 then Value := 18;
  if Value=FPrecision then Exit;
  FPrecision := Value;
  FModified := True;
  DisplayInvalidate;
end;

resourcestring
  rsTimeStepNotStrict = 'Can only check continuity of time series with strict'+
    ' time step.';

function TTimeseries.Continuous: Boolean;
var
  i: Integer;
begin
  if not TimeStepStrict then
    raise Exception.Create(rsTimeStepNotStrict);
  Result := False; // Eliminates compiler warning
  for i := 1 to Count-1 do
    if Abs(DiffInSecs(Items[i].Date, FTimeStep.IncStep(Items[i-1].Date)))>2 then
      Exit;
  Result := True;
end;

{$WARN UNSAFE_TYPE OFF}

procedure RemoveFromArrayOfMethod(var a: TArrayOfMethod; method: TMethod);
var
  len,j,k: Integer;
begin
  len := Length(a);
  for j := 0 to len-1 do
    if (a[j].Data = method.Data) and (a[j].Code = method.Code) then
    begin
      for k := j to len-2 do
        a[k] := a[k+1];
      SetLength(a, len-1);
      Break;
    end;
end;

{$WARN UNSAFE_TYPE ON}

procedure TTimeseries.AddInvalidator(AInvalidator: TMethod);
var i: Integer;
begin
  i := Length(Invalidators);
  SetLength(Invalidators, i+1);
  Invalidators[i] := AInvalidator;
end;

procedure TTimeseries.AddFullInvalidator(AFullInvalidator: TMethod);
var i: Integer;
begin
  i := Length(FullInvalidators);
  SetLength(FullInvalidators, i+1);
  FullInvalidators[i] := AFullInvalidator;
end;

procedure TTimeseries.RemoveInvalidator(AInvalidator: TMethod);
begin
  RemoveFromArrayOfMethod(Invalidators, AInvalidator);
end;

procedure TTimeseries.RemoveFullInvalidator(AFullInvalidator: TMethod);
begin
  RemoveFromArrayOfMethod(FullInvalidators, AFullInvalidator);
end;

procedure TTimeseries.SetID(AID: Integer);
begin
  FID := AID;
end;

{ TTsRecord }

constructor TTsRecord.Create(AOwner: TTimeseries; Date: TDateTime;
  IsNull: Boolean; Value: Real; FlagString: string; MStatus: TMStatus);
begin
  FOwner := AOwner;
  FDate := Round(Date*86400)/86400;
  FIsNull := IsNull;
  FValue := Value;
  FMStatus := MStatus;
  SetAllFlags(FlagString);
end;

procedure TTsRecord.SetNull;
begin
  FIsNull := True;
  if MStatus = msUnmodified then MStatus := msModified;
  if FOwner<>nil then
  begin
    FOwner.Modified := True;
    FOwner.DisplayInvalidate;
  end;
end;

procedure TTsRecord.Assign(Source: TPersistent);
begin
  with Source as TTsRecord do
  begin
    Self.FDate := FDate;
    Self.FMStatus := FMStatus;
    Self.SetAllFlags(TTsRecord(Source).GetAllFlags);
    Self.FIsNull := FIsNull;
    Self.FValue := FValue;
  end;
  if FOwner<>nil then
  begin
    FOwner.Modified := True;
    FOwner.DisplayFullInvalidate;
  end;
end;

function TTsRecord.GetValue: Double;
begin
  if IsNull then
    raise EValueIsNull.Create(DateTimeToStr(Date)+': '+rsMissingValue);
  Result := FValue;
end;

procedure TTsRecord.SetValue(Value: Double);
begin
  FValue := Value;
  FIsNull := False;
  if MStatus = msUnmodified then MStatus := msModified;
  if FOwner<>nil then
  begin
    FOwner.Modified := True;
    FOwner.DisplayInvalidate;
  end;
end;

function TTsRecord.GetAsString: string;
begin
  if IsNull then
    Result := ''
  else begin
    if FOwner.Precision<0 then
      Result := Format('%.0f', [Round(FValue/
        IntPower(10,Abs(FOwner.Precision)))*IntPower(10,Abs(FOwner.Precision))])
    else
      Result := Format('%.'+IntToStr(Max(0,FOwner.Precision))+'f', [FValue]);
  end;
end;

resourcestring
  rsMustHaveOwner = 'TTsRecord must have an owner to do this operation.';

function TTsRecord.GetDateAsString: string;
var
  Year, Month, Day, Year2, Month2: Word;
  ADate2: TDateTime;
begin
  if FOwner=nil then raise Exception.Create(rsMustHaveOwner);
  if FOwner.TimeStep<tstMonthly then
    Result := FormatDateTime('yyyy-mm-dd hh:nn', Date)
  else if FOwner.TimeStep=tstMonthly then
    Result := FormatDateTime('yyyy/mm', Date)
  else if FOwner.TimeStep=tstYearly then
  begin
    DecodeDate(Date, Year, Month, Day);
    if FOwner.HydrologicalYear then
      Result := Format('%4.4d-%2.2d', [Year, (Year+1) mod 100])
    else
      Result := Format('%4d', [Year]);
  end else if FOwner.TimeStep>tstMonthly then
  begin
    DecodeDate(Date, Year, Month, Day);
    ADate2 := IncMonth(Date, FOwner.TimeStep.LengthMonths-1);
    DecodeDate(ADate2, Year2, Month2, Day);
    if FOwner.TimeStep<tstAnnual then
      Result := Format('%4.4d/%2.2d - %4.4d/%2.2d',
        [Year, Month, Year2, Month2])
    else
      Result := Format('%4.4d - %4.4d', [Year, Year2]);
  end;
end;

procedure TTsRecord.SetAsString(Value: string);
begin
  Value := Trim(Value);
  if Value='' then SetNull
  else AsFloat := StrToFloat(Value);
end;

function TTsRecord.GetAsInteger: Integer;
begin
  if IsNull then
    raise EValueIsNull.Create(DateTimeToStr(Date)+': '+rsMissingValue);
  Result := Round(FValue);
end;

procedure TTsRecord.SetAsInteger(Value: Integer);
begin
  FValue := Value;
  FIsNull := False;
  if MStatus = msUnmodified then MStatus := msModified;
  if FOwner<>nil then
  begin
    FOwner.Modified := True;
    FOwner.DisplayInvalidate;
  end;
end;

resourcestring
  rsMustHaveOwnerToDoThis = 'Cannot manipulate flags on a TTsRecord that has '
    +'no owner.';

function TTsRecord.GetFlag(Flag: string): Boolean;
begin
  Assert(FOwner<>nil, rsMustHaveOwnerToDoThis);
  Result := False;
  if Flag = '' then Exit;
  if FStatus = 0 then Exit;
    Result := DelimitedStringContains(FOwner.GetFromFlagList(FStatus-1),
      Flag, ' ');
end;

procedure TTsRecord.SetFlag(Flag: string; Value: Boolean);
var
  s: string;
  ASavedStatus: Integer;
begin
  Assert(FOwner<>nil, rsMustHaveOwnerToDoThis);
  ASavedStatus := FStatus;
  Flag := ToUpper(Flag);
  if FStatus>0 then
    s := FOwner.GetFromFlagList(FStatus-1);
  if Value = True then
  begin
    if not GetFlag(Flag) then
      s := s + ' '+Flag;
  end else begin
    if GetFlag(Flag) then
      s:= StringReplace(s, Flag, '', [rfIgnoreCase, rfReplaceAll]);
  end;
  s := TrimAllSpaces(s);
  if s = '' then
  begin
    FStatus := 0;
  end else
    FStatus := FOwner.AddToFlagList(s)+1;
  if (FOwner<>nil) and (FStatus<>ASavedStatus) then
  begin
    FOwner.Modified := True;
    if MStatus = msUnmodified then MStatus := msModified;
    FOwner.DisplayInvalidate;
  end;
end;

function TTsRecord.GetAllFlags: string;
begin
  Assert(FOwner<>nil, rsMustHaveOwnerToDoThis);
  Result := '';
  if FStatus = 0 then Exit;
  Result := FOwner.GetFromFlagList(FStatus-1);
end;

function TTsRecord.GetAllFlags(Delimiter: Char): string;
var
  i: Integer;
  f: string;
begin
  Assert(FOwner<>nil, rsMustHaveOwnerToDoThis);
  Result := '';
  if FStatus = 0 then Exit;
  f := FOwner.GetFromFlagList(FStatus-1);
  for i := 1 to DelimitedStringCount(f, ' ') do
  begin
    if i>1 then Result := Result + Delimiter;
    Result := Result + DelimitedStringItem(f, i, ' ');
  end;
end;

procedure TTsRecord.SetAllFlags(Flags: string);
var
  ASavedStatus: TStatus;
begin
  Assert(FOwner<>nil, rsMustHaveOwnerToDoThis);
  ASavedStatus := FStatus;
  if Flags = '' then
  begin
    FStatus := 0;
  end else
    FStatus := FOwner.AddToFlagList(TrimAllSpaces(ToUpper(Flags)))+1;
  if (FOwner<>nil) and (ASavedStatus<>FStatus) then
  begin
    FOwner.Modified := True;
    FOwner.DisplayInvalidate;
  end;
end;

procedure TTsRecord.SetAllFlags(Flags: string; Delimiter: Char);
var
  i: Integer;
  f, s: string;
begin
  Assert(FOwner<>nil, rsMustHaveOwnerToDoThis);
  f := Flags;
  s := '';
  for i := 1 to DelimitedStringCount(f, Delimiter) do
    s := s + DelimitedStringItem(f, i, Delimiter)+' ';
  SetAllFlags(s);
end;

resourcestring
  rsCannotSetDateOfTTsRecord =
    'The date of the record of a time series cannot be changed.';

procedure TTsRecord.SetDate(Value: TDateTime);
begin
  if FOwner=nil then
    FDate := Round(Value*86400)/86400
  else
    raise Exception.Create(rsCannotSetDateOfTTsRecord);
end;

procedure TTsRecord.SetMStatus(Value: TMStatus);
begin
  if FMStatus=Value then Exit;
  FMStatus := Value;
  if FOwner<>nil then FOwner.DisplayInvalidate;
end;

resourcestring
  rsCantDoThisOnOrphanRecord = 'This operation cannot be performed on a '+
    'TTsRecord object which is not owned.';

function TTsRecord.SortingDate(UseDateOffset: Boolean): TDateTime;
var
  DateOffset, diff: Real;
begin
  if FOwner=nil then
    raise Exception.Create(rsCantDoThisOnOrphanRecord);
  Result := Date;
  if FOwner.TimeStep<tstMonthly then
    Exit;
  Result := IncMonth(Result, FOwner.ActualOffset.Months);
  if UseDateOffset then
    Result := AddDateTime(Result, FOwner.ActualOffset.Minutes/1440);
  if UseDateOffset then
    diff := 1 else diff := -1;
  if FOwner.TimeStep<tstAnnual then
    DateOffset := (diff*43+FOwner.TimeStep.LengthMonths*2)/86400
  else
    DateOffset := (diff*13+(FOwner.TimeStep.LengthMonths div 6))/86400;
  Result := AddDateTime(Result, DateOffset);
end;

{ TMultiTimeseries }

constructor TMultiTimeseries.Create;
begin
  inherited Create;
  FSectionList := TObjectList.Create(True);
  FSectionList.Add(TTimeseries.Create);
end;

destructor TMultiTimeseries.Destroy;
begin
  FSectionList.Free;
  inherited Destroy;
end;

function TMultiTimeseries.GetRecord(Num, Index: Integer): TTsRecord;
begin
  Result := TTimeseries(FSectionList[Num-1])[Index];
end;

function TMultiTimeseries.GetSection(Num: Integer): TTimeseries;
begin
  Result := TTimeseries(FSectionList[Num-1]);
end;

resourcestring
  rsMultiTimestepDiffers = 'Time series time step differs when inserting into'+
    ' multi time series';
  rsMultiCountDiffers = 'Time series count differs when inserting into'+
    ' multi time series';
  rsMultiBeginingDiffers = 'Time series begining differs when inserting into '+
    ' multi time series';

function TMultiTimeseries.AddSection(ATimeseries: TTimeseries): Integer;
begin
  if FSectionList.Count>1 then
  begin
    if TTimeseries(FSectionList[0]).TimeStep <> ATimeseries.TimeStep then
      raise Exception.Create(rsMultiTimestepDiffers);
    if TTimeseries(FSectionList[0]).Count <> ATimeseries.Count then
      raise Exception.Create(rsMultiCountDiffers);
    if TTimeseries(FSectionList[0]).Count >0 then
      if Abs(DiffInSecs(TTimeseries(FSectionList[0]).First.Date,
        ATimeseries.First.Date ))>1 then
        raise Exception.Create(rsMultiBeginingDiffers);
  end;
  Result := FSectionList.Add(ATimeseries);
  TTimeseries(FSectionList.Last).AssignMeta(TTimeseries(FSectionList.First));
end;

resourcestring
  rsDeleteMultiItem = 'Delete item of multi time series out of bounds (';

procedure TMultiTimeseries.DeleteSection(AIndex: Integer);
begin
  if (AIndex<1) or (AIndex>FSectionList.Count) then
    raise EListError.Create(rsDeleteMultiItem+IntToStr(AIndex)+')');
  FSectionList.Delete(AIndex-1);
end;

procedure TMultiTimeseries.ClearSections;
begin
  FSectionList.Clear;
end;

procedure TMultiTimeseries.EnsureNumExists(Num: Integer);
begin
  while Num > FSectionList.Count do
  begin
    FSectionList.Add(TTimeseries.Create);
    TTimeseries(FSectionList.Last).AssignMeta(TTimeseries(FSectionList.First));
  end;
end;

function TMultiTimeseries.Add(Num: Integer; Date: TDateTime; IsNull: Boolean;
  Value: Real; FlagString: string; MStatus: TMStatus): Integer;
begin
  EnsureNumExists(Num);
  Result := TTimeseries(FSectionList[Num-1]).Add(Date, IsNull, Value,
    FlagString, MStatus);
end;

procedure TMultiTimeseries.Clear;
begin
  TTimeseries(FSectionList[0]).Clear;
  while FSectionList.Count > 1 do
    FSectionList.Delete(1);
end;

procedure TMultiTimeseries.Delete(Num, Index: Integer);
begin
  TTimeseries(FSectionList[Num-1]).Delete(Index);
end;

function TMultiTimeseries.IndexOf(Num: Integer; Date: TDateTime): Integer;
begin
  Result := TTimeseries(FSectionList[Num-1]).IndexOf(Date);
end;

function TMultiTimeseries.Insert(Num, Index: Integer; Date: TDateTime;
  IsNull: Boolean; Value: Real; FlagString: string; MStatus: TMStatus): Integer;
begin
  EnsureNumExists(Num);
  Result := TTimeseries(FSectionList[Num-1]).Insert(Index, Date, IsNull, Value,
    FlagString, MStatus);
end;

function TMultiTimeseries.Insert(Num: Integer; Date: TDateTime; IsNull: Boolean;
  Value: Real; FlagString: string; MStatus: TMStatus): Integer;
begin
  EnsureNumExists(Num);
  Result := TTimeseries(FSectionList[Num-1]).Insert(Date, IsNull, Value,
    FlagString, MStatus);
end;

function TMultiTimeseries.NearestTo(Num: Integer; Date: TDateTime): Integer;
begin
  Result := TTimeseries(FSectionList[Num-1]).NearestTo(Date);
end;

function TMultiTimeseries.PositionOfNext(Num: Integer; Date: TDateTime):
  Integer;
begin
  Result := TTimeseries(FSectionList[Num-1]).PositionOfNext(Date);
end;

function TMultiTimeseries.PositionOfPrevious(Num: Integer; Date: TDateTime):
  Integer;
begin
  Result := TTimeseries(FSectionList[Num-1]).PositionOfPrevious(Date);
end;

function TMultiTimeseries.Remove(Num: Integer; Date: TDateTime): Integer;
begin
  Result := TTimeseries(FSectionList[Num-1]).Remove(Date);
end;

function TMultiTimeseries.GetSectionCount: Integer;
begin
  Result := FSectionList.Count;
end;

function TMultiTimeseries.GetCount: Integer;
var i: Integer;
begin
  Result := 0;
  for i := FSectionList.Count-1 downto 0 do
    Inc(Result, TTimeseries(FSectionList[i]).Count);
end;

procedure TMultiTimeseries.Assign(Source: TPersistent);
var
  Src: TMultiTimeseries;
  num: Integer;
begin
  Src := TMultiTimeseries(Source);
  Fid := Src.Fid;
  for num := 1 to Src.FSectionList.Count do
  begin
    FSectionList.Add(TTimeseries.Create);
    TTimeseries(FSectionList.Last).Assign(Src.Sections[num]);
  end;
end;

function TMultiTimeseries.CountNotNull: Integer;
var i: Integer;
begin
  Result := 0;
  for i := FSectionList.Count-1 downto 0 do
    Inc(Result, TTimeseries(FSectionList[i]).CountNotNull);
end;

function TMultiTimeseries.IsHydrologicalYear: Boolean;
begin
  Result := TTimeseries(FSectionList[0]).HydrologicalYear;
end;

procedure TMultiTimeseries.SetHydrologicalYear(Value: Boolean);
var i: Integer;
begin
  for i := FSectionList.Count-1 downto 0 do
    TTimeseries(FSectionList[i]).SetHydrologicalYear(Value);
end;

function TMultiTimeseries.GetDateOffset: Real;
begin
  Result := TTimeseries(FSectionList[0]).GetDateOffset;
end;

procedure TMultiTimeseries.SetDateOffset(Value: Real);
var i: Integer;
begin
  for i := FSectionList.Count-1 downto 0 do
    TTimeseries(FSectionList[i]).SetDateOffset(Value);
end;

function TMultiTimeseries.IsDateOffsetUnspecified: Boolean;
begin
  Result := TTimeseries(FSectionList[0]).FDateOffsetUnspecified;
end;

procedure TMultiTimeseries.SetDateOffsetUnspecified(Value: Boolean);
var i: Integer;
begin
  for i := FSectionList.Count-1 downto 0 do
    TTimeseries(FSectionList[i]).FDateOffsetUnspecified := Value;
end;

function TMultiTimeseries.GetTimeStep: TTimeStep;
begin
  Result := TTimeseries(FSectionList[0]).TimeStep;
end;

function TMultiTimeseries.IsTimeStepStrict: Boolean;
begin
  Result := TTimeseries(FSectionList[0]).TimeStepStrict;
end;

procedure TMultiTimeseries.SetTimeStepStrict(Value: Boolean);
var i: Integer;
begin
  for i := FSectionList.Count-1 downto 0 do
    TTimeseries(FSectionList[i]).SetTimeStepStrict(Value);
end;

procedure TMultiTimeseries.SetTimeStep(Value: TTimeStep);
var i: Integer;
begin
  for i := FSectionList.Count-1 downto 0 do
    TTimeseries(FSectionList[i]).SetTimeStep(Value);
end;

function TMultiTimeseries.GetVariableType: TVariableType;
begin
  Result := TTimeseries(FSectionList[0]).VariableType;
end;

procedure TMultiTimeseries.SetVariableType(Value: TVariableType);
var i: Integer;
begin
  for i := FSectionList.Count-1 downto 0 do
    TTimeseries(FSectionList[i]).SetVariableType(Value);
end;

function TMultiTimeseries.IsModified: Boolean;
begin
  Result := TTimeseries(FSectionList[0]).Modified;
end;

procedure TMultiTimeseries.SetModified(Value: Boolean);
var i: Integer;
begin
  for i := FSectionList.Count-1 downto 0 do
    TTimeseries(FSectionList[i]).SetModified(Value);
end;

function TMultiTimeseries.GetPrecision: Integer;
begin
  Result := TTimeseries(FSectionList[0]).Precision;
end;

procedure TMultiTimeseries.SetPrecision(Value: integer);
var i: Integer;
begin
  for i := FSectionList.Count-1 downto 0 do
    TTimeseries(FSectionList[i]).SetPrecision(Value);
end;

function TMultiTimeseries.GetMUnit: string;
begin
  Result := TTimeseries(FSectionList[0]).MUnit;
end;

procedure TMultiTimeseries.SetMUnit(Value: string);
var i: Integer;
begin
  for i := FSectionList.Count-1 downto 0 do
    TTimeseries(FSectionList[i]).MUnit := Value;
end;

procedure TMultiTimeseries.SetGTypeName(Value: string);
var i: Integer;
begin
  for i := FSectionList.Count-1 downto 0 do
    TTimeseries(FSectionList[i]).GTypeName := Value;
end;

procedure TMultiTimeseries.SetGentityName(Value: string);
var i: Integer;
begin
  for i := FSectionList.Count-1 downto 0 do
    TTimeseries(FSectionList[i]).GentityName := Value;
end;

procedure TMultiTimeseries.SetTypeName(Value: string);
var i: Integer;
begin
  for i := FSectionList.Count-1 downto 0 do
    TTimeseries(FSectionList[i]).TypeName := Value;
end;

procedure TMultiTimeseries.SetVarName(Value: string);
var i: Integer;
begin
  for i := FSectionList.Count-1 downto 0 do
    TTimeseries(FSectionList[i]).VarName := Value;
end;

procedure TMultiTimeseries.SetTimeStepName(Value: string);
var i: Integer;
begin
  for i := FSectionList.Count-1 downto 0 do
    TTimeseries(FSectionList[i]).TimeStepName := Value;
end;

function TMultiTimeseries.GetGTypeName: string;
begin
  Result := TTimeseries(FSectionList[0]).GTypeName;
end;

function TMultiTimeseries.GetGentityName: string;
begin
  Result := TTimeseries(FSectionList[0]).GentityName;
end;

function TMultiTimeseries.GetTypeName: string;
begin
  Result := TTimeseries(FSectionList[0]).TypeName;
end;

function TMultiTimeseries.GetVarName: string;
begin
  Result := TTimeseries(FSectionList[0]).VarName;
end;

function TMultiTimeseries.GetTimeStepName: string;
begin
  Result := TTimeseries(FSectionList[0]).TimeStepName;
end;

function TMultiTimeseries.GetVariable: Integer;
begin
  Result := TTimeseries(FSectionList[0]).Variable;
end;

procedure TMultiTimeseries.SetVariable(Value: Integer);
var i: Integer;
begin
  for i := FSectionList.Count-1 downto 0 do
    TTimeseries(FSectionList[i]).Variable := Value;
end;

resourcestring
  rsUnequalSections = 'Section %d has a different number of records than '+
    'previous sections.';
  rsNotMultiple = 'The number of records in section %d is not a multiple '+
    'of %d.';
  rsSectionContainsNulls = 'Section %d contains null values.';
  rsSectionNotContinuous = 'Section %d is not continuous; there are missing '+
    'records.';
  rsStartDatesDiffer = 'Section %d has a different start date than '+
    'previous sections.';

procedure TMultiTimeseries.CheckConsistency(CountDivisor: Integer);
var i: Integer;
begin
  for i := 1 to SectionCount do
  begin
    if Sections[i].Count mod CountDivisor > 0 then
      raise Exception.Create(Format(rsNotMultiple, [i, CountDivisor]));
    if Sections[i].Count <> Sections[i].CountNotNull then
      raise Exception.Create(Format(rsSectionContainsNulls, [i]));
    if not Sections[i].Continuous then
      raise Exception.Create(Format(rsSectionNotContinuous, [i]));
    if i = 1 then Continue;
    if Sections[i].Count <> Sections[1].Count then
      raise Exception.Create(Format(rsUnequalSections, [i]));
    if DiffInSecs(Sections[i].First.Date, Sections[1].First.Date) <> 0 then
      raise Exception.Create(Format(rsStartDatesDiffer, [i]));
  end;
end;

procedure TMultiTimeseries.SetID(AID: Integer);
begin
  FID := AID;
end;

procedure TMultiTimeseries.WriteToStream(AStream: TStream;
  ProgressIndicator: TProgressIndicator);
var
  AStreamWriter: TStreamWriter;
  i: Integer;
begin
  AStreamWriter := nil;
  try
    if FSectionList.Count<1 then Exit;
    AStreamWriter := TStreamWriter.Create(AStream, TEncoding.Default);
    Sections[1].WriteMeta(AStreamWriter);
    for i := 1 to FSectionList.Count do
    begin
      Sections[i].WriteData(AStreamWriter, ProgressIndicator);
      if i<>FSectionList.Count then
        AStreamWriter.WriteLine('');
    end;
  finally
    if AStreamWriter<>nil then AStreamWriter.Close;
    AStreamWriter.Free;
  end;
end;

procedure TMultiTimeseries.WriteToFile(AFileName: string;
  ProgressIndicator: TProgressIndicator);
var
  AFileStream: TFileStream;
  AMemoryStream: TMemoryStream;
begin
  AFileStream := nil;
  AMemoryStream := nil;
  try
    AFileStream := TFileStream.Create(AFileName, fmCreate);
    AMemoryStream := TMemoryStream.Create;
    WriteToStream(AMemoryStream, ProgressIndicator);
    AMemoryStream.Seek(0,0);
    AFileStream.CopyFrom(AMemoryStream,0);
  finally
    AFileStream.Free;
    AMemoryStream.Free;
  end;
end;

procedure TMultiTimeseries.LoadFromStream(AStream: TStream; ProgressIndicator:
  TProgressIndicator = nil);
var
  ATimeseries: TTimeseries;
  ADecimalSeparator, ADelimiter, AFlagDelimiter: Char;
  ADateFormat: string;
  AMemoryStream: TMemoryStream;
  AStreamReader: TStreamReader;
  AStreamWriter: TStreamWriter;
  s: string;
  LineNo: Integer;
begin
  ClearSections;
  ATimeseries := nil;
  AStreamReader := nil;
  try try
    ATimeseries := TTimeseries.Create;
    AStreamReader := TStreamReader.Create(AStream, TEncoding.Default);
    LineNo := ATimeseries.ReadMeta(AStreamReader, 1, ADecimalSeparator,
      ADelimiter, AFlagDelimiter, ADateFormat);
    AMemoryStream := nil;
    AStreamWriter := nil;
    repeat
      if ATimeseries=nil then ATimeseries := TTimeseries.Create;
      try
        AMemoryStream := TMemoryStream.Create;
        AStreamWriter := TStreamWriter.Create(AMemoryStream, TEncoding.Default);
        repeat
          s := AStreamReader.ReadLine;
          Inc(LineNo);
          if s<>'' then AStreamWriter.WriteLine(s);
          if AStreamReader.EndOfStream or (s='') then Break;
        until False;
        if AStreamWriter<>nil then AStreamWriter.Close;
        AMemoryStream.Seek(0,0);
        ATimeseries.ReadData(AMemoryStream, idaEmpty, idaEmpty, LineNo,
          ProgressIndicator, ADecimalSeparator, ADelimiter, AFlagDelimiter,
            ADateFormat);
      finally
        AStreamWriter.Free;
        AStreamWriter := nil;
        AMemoryStream.Free;
        AMemoryStream := nil;
      end;
      if Count>0 then ATimeseries.AssignMeta(Sections[1]);
      AddSection(ATimeseries);
      ATimeseries := nil;
    until AStreamReader.EndOfStream;
  finally
    if AStreamReader<>nil then AStreamReader.Close;
    AStreamReader.Free;
  end;
  except
    ATimeseries.Free;
    raise;
  end;
end;

procedure TMultiTimeseries.LoadFromFile(AFileName: string; ProgressIndicator:
  TProgressIndicator = nil);
var
  AFileStream: TFileStream;
  AMemoryStream: TMemoryStream;
begin
  AFileStream := nil;
  AMemoryStream := nil;
  try
    AFileStream := TFileStream.Create(AFileName,
      fmOpenRead or fmShareDenyWrite);
    AMemoryStream := TMemoryStream.Create;
    AMemoryStream.LoadFromStream(AFileStream);
    AMemoryStream.Seek(0,0);
    LoadFromStream(AMemoryStream, ProgressIndicator);
  finally
    AFileStream.Free;
    AMemoryStream.Free;
  end;
end;

procedure TMultiTimeseries.RearrangeTS(NrRecords:Integer);
var tempMTS: TMultiTimeSeries;
    newTS: TTimeSeries;
    i,j,iRec:Integer;
    date1:TDateTime;

   //creates a new section
    procedure createSection;
    begin
         date1 := tempMTS.Records[1,0].FDate;
         newTS := TTimeSeries.Create;
         newTS.AssignMeta(tempMTS.Sections[1]);
    end;
  //fills missing null (empty) values at the end of the section and adds
  //into the multi time series
    procedure fillAndAddSection;
    var j:Integer;
    begin
         if (SectionCount>0) then
           for j:=newTS.Count to TTimeSeries(FSectionList[0]).Count-1 do
           begin
                newTS.Add(date1,true,0,'',msNew);
                date1 := IncTimeStep(date1, tempMTS.Timestep);
           end;
         AddSection(newTS);
    end;

begin
  if (SectionCount<1) or (Sections[1].count<1) or (NrRecords<1) then exit;
  newTS   := nil;
  tempMTS := nil;
  try
     tempMTS := TMultiTimeSeries.create;
     tempMTS.clearSections;
     tempMTS.Assign(self);
     try
         //clear the multiTS
           clearSections;
           createSection; //first section
           iRec := 1;
           for i:=1 to tempMTS.SectionCount do
             for j:=0 to tempMTS.Sections[i].count-1 do
             begin
                if iRec>NrRecords then
                begin
                     FillAndAddSection;
                     createSection;
                     iRec := 1;
                end;
              //add new data
                with tempMTS.Records[i,j] do
                begin
                  newTS.Add(date1, FIsNull, FValue, GetAllFlags, FMStatus);
                  date1 := IncTimeStep(date1, tempMTS.Timestep);
                end;
                iRec := iRec + 1;
             end;
           //fill the last section with null values and add into the mult TS
             FillAndAddSection;
           //delete all not null (empty) final sections that may have been
           //created during the above process
             while (SectionCount>1) and not
               (Sections[SectionCount].CountNotNull>0) do
                 DeleteSection(SectionCount);
     except
              ClearSections;
              self.Assign(tempMTS);
              if newTS<>nil then newTS.free;
              raise;
     end;
  finally
     tempMTS.Free;
  end;
end;

resourcestring
  rsCantCreateTS = 'Cannot create time series';

procedure TMultiTimeSeries.getDataFromArray(RealArray: Array of Real;
          Flags:TObject;  ATimestep:TTimeStep;
          AVariable:Integer; StartDate, EndDate:TDateTime);
var date1:TDateTime;
    sect1: TTimeSeries;

   //creates a new section
    procedure createSection;
    begin
         date1 := StartDate;
         sect1 := TTimeSeries.Create;
         sect1.timestep := ATimestep;
         sect1.variable := AVariable;
    end;
  //fill missing empty values at the end of the section and adds into the multi
  //time series
    procedure fillAndAddSection;
    var j:Integer;
    begin
         if (SectionCount>0) then
           for j:=sect1.Count to TTimeSeries(FSectionList[0]).Count-1 do
           begin
              sect1.Insert(date1,true,0,'',msNew);
              date1 := IncTimeStep(date1, ATimestep);
           end;
         AddSection(sect1);
    end;

var i:Integer;
begin //getDataFromArray
  if StartDate>=EndDate then  raise Exception.Create(rsCantCreateTS);
  ClearSections;
  try
     createSection; //first section
     for i:=0 to length(RealArray)-1 do
     begin
          if date1>EndDate then
          begin
               FillAndAddSection;
               createSection;
          end;
          sect1.Insert(date1,false,RealArray[i],'',msNew);
          date1 := IncTimeStep(date1, ATimestep);
     end;
     FillAndAddSection; //last section
  except
        ClearSections;
        if sect1<>nil then sect1.free;
        raise;
  end;
end; //getDataFromArray

(*******************************************************************)

function IncTimeStep(const date1: TDateTime; ATimeStep:TTimeStep; nrOfSteps:
  Integer = 1): TDateTime;
begin
  Result := ATimeStep.IncStep(date1, nrOfSteps);
end;

initialization

  tstFiveMinute := TTimestep.Create(5,0,7);
  tstVariable := TTimestep.Create(0,0,6);
  tstTenMinute := TTimestep.Create(10,0,1);
  tstFithteenMinute := TTimestep.Create(15,0,0);
  tstTwentyMinute := TTimestep.Create(20,0,0);
  tstHalfHour := TTimestep.Create(30,0,0);
  tstHourly := TTimestep.Create(60,0,2);
  tstTwoHour := TTimestep.Create(120,0,0);
  tstThreeHour := TTimestep.Create(180,0,0);
  tstFourHour := TTimestep.Create(240,0,0);
  tstSixHour := TTimestep.Create(360,0,0);
  tstEightHour := TTimestep.Create(480,0,0);
  tstTwelveHour := TTimestep.Create(720,0,0);
  tstDaily := TTimestep.Create(1440,0,3);
  tstMonthly := TTimestep.Create(0,1,4);
  tstTwoMonth := TTimestep.Create(0,2,0);
  tstThreeMonth := TTimestep.Create(0,3,0);
  tstFourMonth := TTimestep.Create(0,4,0);
  tstSixMonth := TTimestep.Create(0,6,0);
  tstYearly := TTimestep.Create(0,12,5);
  tstAnnual := tstYearly;
  tstTwoYear := TTimestep.Create(0,24,0);
  tstFiveYear := TTimestep.Create(0,60,0);
  tstTenYear := TTimestep.Create(0,120,0);
  tstUnknown := TTimestep.Create(0,0,0);


  AvailableTimesteps[0] := tstFiveMinute;
  AvailableTimesteps[1] := tstTenMinute;
  AvailableTimesteps[2] := tstHourly;
  AvailableTimesteps[3] := tstDaily;
  AvailableTimesteps[4] := tstMonthly;
  AvailableTimesteps[5] := tstAnnual;
  AvailableTimesteps[6] := tstVariable;
  AvailableTimesteps[7] := tstUnknown;
  AvailableTimesteps[8] := tstTwoHour;
  AvailableTimesteps[9] := tstTwoMonth;
  AvailableTimesteps[10] := tstFithteenMinute;
  AvailableTimesteps[11] := tstTwentyMinute;
  AvailableTimesteps[12] := tstHalfHour;
  AvailableTimesteps[13] := tstThreeHour;
  AvailableTimesteps[14] := tstFourHour;
  AvailableTimesteps[15] := tstSixHour;
  AvailableTimesteps[16] := tstEightHour;
  AvailableTimesteps[17] := tstTwelveHour;
  AvailableTimesteps[18] := tstTwoMonth;
  AvailableTimesteps[19] := tstThreeMonth;
  AvailableTimesteps[20] := tstFourMonth;
  AvailableTimesteps[21] := tstSixMonth;
  AvailableTimesteps[22] := tstTwoYear;
  AvailableTimesteps[23] := tstFiveYear;
  AvailableTimesteps[24] := tstTenYear;

end.




