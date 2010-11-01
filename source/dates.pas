{******************************************************************}
{                                                                  }
{  Thelma library                                                  }
{                                                                  }
{  Copyright (c) 2000-03 National Technical University of Athens   }
{                                                                  }
{******************************************************************}

{** Date manipulation functions. }
unit Dates;

interface

uses SysUtils, GenUtils;

{** Tolerance for comparing TDateTime types.
Since TDateTime is a Double, testing for equality is not really possible;
instead, it should be checked whether the difference is less than a small
number. If two TDateTimes differ by less than idtTolerance, they are considered
equal.
@author A.X.
}
const idtTolerance = (1/86400);

type
  {** An alternative to TDateTime, suitable for times older than 1900.
      Although TDateTime can, in theory, store dates older than 1900, the way
      dates prior to 1900 are encoded is such that it is extremely difficult to
      make date calculations. See the TDateTime type for more information.<p>
      TCDateTime (standing for Continuous DateTime) uses the same encoding as
      TDateTime for dates after 1900, that is, it uses the fractional number of
      days since the epoch 1899-12-30 00:00. For older dates, it uses the
      fractional number of days before the epoch. Thus, in TCDateTime, -1.25
      stands for 1899-12-28 18:00 (=1.25 days before the epoch), whereas in
      TDateTime -1.25 stands for 1899-12-29 06:00 (=1 day before the epoch, 0.25
      stands for time).<p>
      Use DateTimeToC and CToDateTime to convert between one format and the
      other.
      @author A.X.
      @SeeAlso <See Routine=DateTimeToC>
      @SeeAlso <See Routine=CToDateTime>
      @SeeAlso <Jump File=Delphi5.hlp K="TDateTime type" Text=TDateTime>
  }
  TCDateTime = Double;

{** Converts a TDateTime to a TCDateTime.
    @author A.X.
    @SeeAlso <See Type=TCDateTime>
    @SeeAlso <See Routine=CToDateTime>
}
function DateTimeToC(DateTime: TDateTime): TCDateTime;

{** Converts a TCDateTime to a TDateTime.
    @author A.X.
    @SeeAlso <See Type=TCDateTime>
    @SeeAlso <See Routine=DateTimeToC>
}
function CToDateTime(CDateTime: TCDateTime): TDateTime;

{** Subtracts or compares two dates.
Returns the time elapsed from a to b; the difference may be negative.  For
example, a result of -1.25 means that a comes later than b by one day and 6
hours.<p>

Unless you need precision better than a second, it is always better to use
DiffInSecs instead, which returns an integer and thus avoids problems with
floats, equality tests, etc.<p>

SubtractDateTime may also be used to compare dates. Specifically:
<ul>
  <li>To check whether a=b check whether
    <code>Abs(SubtractDateTime(a, b)) < idtTolerance</code>
  <li>To check whether a&lt;b check whether
    <code>SubtractDateTime(a, b) < -idtTolerance</code>
  <li>To check whether a&gt;b check whether
    <code>SubtractDateTime(a, b) > idtTolerance</code>
</ul>
For an explanation of why you need SubtractDateTime, see AddDateTime.
@author A.X.
@SeeAlso <See Const=idtTolerance>
@SeeAlso <See Routine=AddDateTime>
@SeeAlso <See Routine=DiffInSecs>
}
function SubtractDateTime(a, b: TDateTime): Real;

{** Subtracts or compares two dates.
Returns the time elapsed from a to b, in seconds; the difference may be
negative.  For example, a result of -90000 means that a comes later than b by
90000 seconds.<p>

If you need precision better than a second, use SubtractDateTime instead.

DiffInSecs may also be used to compare dates. Specifically:
<ul>
  <li>To check whether a=b check whether
    <code>DiffInSecs(a, b) = 0</code>
  <li>To check whether a&lt;b check whether
    <code>DiffInSecs(a, b) < 0</code>
  <li>To check whether a&gt;b check whether
    <code>DiffInSecs(a, b) > 0</code>
</ul>
@author A.X.
@SeeAlso <See Const=idtTolerance>
@SeeAlso <See Routine=AddDateTime>
@SeeAlso <See Routine=SubtractDateTime>
}
function DiffInSecs(a, b: TDateTime): Int64;

{** Adds or subtracts an offset to a date.
If a date offset o is to be added to or subtracted from a date d, for dates
prior to 1900 it is not possible to do it simply with d+o and d-o, due to the
way dates prior to 1900 are encoded; see TDateTime for details.<p>
AddDateTime adds Offset, which may be negative, to Date. Offset is the number of
days that will be added to Date. Thus, for example, AddDateTime(Date,-1.25)
subtracts one day and six hours from Date. To add months or years to the date
use the Delphi IncMonth function.
@author A.X.
@SeeAlso <See Routine=SubtractDateTime>
@SeeAlso <Jump File=Delphi5.hlp K="TDateTime type" Text=TDateTime>
@SeeAlso <Jump File=Delphi5.hlp K="IncMonth function" Text=IncMonth>
}
function AddDateTime(Date: TDateTime; Offset: Real): TDateTime;

type
  TGetDateFormatOption = (gdfRaiseOnFail, gdfAllowHydrologicalYear);
  TGetDateFormatOptions = set of TGetDateFormatOption;

{** Determines the format of a date and time in a string.

Use GetDateFormat when you must read a date or time from a string but do not
know the date's format. GetDateFormat reads the string which is supposed to
contain a date and/or time, and returns a string representing the format of
that date and time. If the string's format cannot be recognized, GetDateFormat
raises an exception if gdfRaiseOnFail is set in Options; otherwise, it returns
an empty string.<p>
The following are recognizable formats:<p>
<ul>
  <li>A date in ShortDateFormat optionally followed by a time in
      ShortTimeFormat.  These variables are described in <Jump File=Delphi5.hlp
      K="date/time formatting variables" Text="date/time formatting
      variables">; the defaults are what is set in the Windows Control Panel.
  <li>YYYY-MM-DD HH:mm
  <li>YYYY-MM-DD
  <li>YYYY/MM
  <li>YYYY-MM
  <li>YYYY-YY (hydrological year)
  <li>YYYY
</ul>
<p>
If AllowHydrologicalYear is contained in Options, then XXXX-XX is interpreted
as YYYY-YY, where YY is YYYY+1; if the latter condition is not true, the string
is not recognized as a date. If AllowHydrologicalYear is not specified, then
XXXX-XX is interpreted as YYYY-MM (where, of course, MM must be between 01 and
12, otherwise the string is unrecognized).<p>
All date separators are equivalent, except for the case of hydrological year,
where the separator must be a hyphen. In addition, the separator returned
might not always be the separator in DateString (this is a bug).<p>
The return value is a date/time formatting string, except in the case of a
hydrological year, in which case the return value is 'aaaa-bb'.<p>
Note: GetDateFormat's implementation is terrible; it uses exceptions to
make testing, which makes it extremely slow. If you really want to use
it in a loop, you must change the implementation.<p>
@author A.X.
@SeeAlso <Jump File=Delphi5.hlp K="FormatDateTime function" Text=FormatDateTime>
}
function GetDateFormat(DateString: string; Options: TGetDateFormatOptions;
  HYearOrigin: Integer=10): string;

{** Converts a hydrological year representation to a TDateTime variable.
    DateString must be of the form yyyy-zz, where zz is yyyy+1. If not,
    HYearToDate raises an exception. Otherwise, HYearToDate returns a
    TDateTime that corresponds to yyyy-10-01 00:00.
}
function HYearToDate(DateString: string; HYearOrigin: Integer=10): TDateTime;

{** Returns four-digit year given two-digit year.
GetFullYear converts a two-digit year into a four-digit year. It uses the global
variable TwoDigitYearCenturyWindow to decide which century to use.
@SeeAlso <Jump file=Delphi5.hlp K="TwoDigitYearCenturyWindow variable" Text=TwoDigitYearCenturyWindow>
}
function GetFullYear(TwoDigitYear: Word): Word;

{** Finds hydrological year to which a date belongs.
    If the hydrological year to which ADate belongs is YYYY-ZZ, where ZZ=YYYY+1,
    FindHydrologicalYear returns YYYY.
}
function FindHydrologicalYear(ADate: TDateTime; HYearOrigin: Integer=10):
  Integer;

type

  {** Maintains a list of TDateTimes.
      TDateTimeList stores a (usually ordered) list of dates and times. Since
      TDateTime=Double, TDateTimeList inherits TFloatList but has some
      additional methods.
      @author A.X.
      @SeeAlso <See Class=TFloatList>
  }
  TDateTimeList = class(TFloatList)
  protected
    function Get(Index: Integer): TDateTime; virtual;
    procedure Put(Index: Integer; Item: TDateTime); virtual;
  public
    property Items[Index: Integer]: TDateTime read Get write Put; default;
    function Add(Item: TDateTime): Integer; virtual;
    function IndexOf(Item: TDateTime): Integer;
    procedure Insert(Index: Integer; Item: TDateTime); virtual;
    {** Inserts an object to the Items array at the correct chronological
        position.
	InsertSorted inserts an entry at the correct position of the Items array
	and returns the index of that position.
    }
    function InsertSorted(Item: TDateTime): Integer; virtual;
    {** Returns the index of the entry that is greater than or equal to Item.
	PositionOfNext is much like IndexOf, but works even if the specified
        date does not exist in the Items array.  If the date exists,
        PositionOfNext returns its index, just like IndexOf. If it does not
        exist, then it returns the index of the first record that is greater
        than Item. If Item is greater than the last entry, PositionOfNext
        returns -1.
    }
    function PositionOfNext(Item: TDateTime): Integer;
    {** Returns the index of the entry that is less than or equal to Item.
	PositionOfPrevious is much like IndexOf, but works even if the specified
        date does not exist in the Items array.  If the date exists,
        PositionOfPrevious returns its index, just like IndexOf. If it does not
        exist, then it returns the index of the last record that is less
        than Item. If Item is less than the first entry, PositionOfNext
        returns -1.
    }
    function PositionOfPrevious(Item: TDateTime): Integer;
    function Remove(Item: TDateTime): Integer; virtual;
  end;

const
  {** @SeeAlso <See Class=TPeriod> }
  idaEmpty = 1000000.0;
  {** @SeeAlso <See Class=TPeriod> }
  idaContinues = 1000000.1;

type

  {** A time period.
      @author A.X.
  }
  TPeriod = class
  private
    FEndDate: TDateTime;
    FStartDate: TDateTime;
  public
    {** The end of the time period.
	EndDate is the end of the time period, or idaEmpty in case it is unknown
	(not set), or idaContinues in case the period still continues. The
	initial value is idaEmpty.<p>
	EndDate is read only. To set it, use SetDate.
	@SeeAlso <See Method=SetDate>
	@SeeAlso <See Property=StartDate>
    }
    property EndDate: TDateTime  read FEndDate;
    {** The start of the time period.
	StartDate is the start of the period, or idaEmpty if it is unknown or
	not set. The initial value is idaEmpty.<p>
	StartDate is read only. To set it, use SetDate.
	@SeeAlso <See Method=SetDate>
	@SeeAlso <See Property=EndDate>
    }
    property StartDate: TDateTime read FStartDate;
    {** Sets the time period.
	SetDate checks the time period, which is considered valid if
	EndDate&gt;=StartDate or EndDate=idaContinues or EndDate=idaEmpty or
	StartDate=idaEmpty. If the period is valid, SetDate sets it, otherwise
	it raises EConvertError.
	@SeeAlso <Jump File=Del5Vcl.hlp K="EConvertError," Text=EConvertError>
    }
    procedure SetDate(StartDate, EndDate: TDateTime); virtual;
    {** Creates an instance of TPeriod.
	Create creates a TPeriod object and sets StartDate and EndDate to
	idaEmpty.
    }
    constructor Create;
  end;

{** Determines a TDateTime from a formatted string.
    FormatStrToDateTime is the opposite of FormatDateTime; it reads a date from
    DateString which must be formatted as specified in Format, and returns that
    date. The year must be specified in Format, otherwise it is considered
    invalid. A format string is also considered invalid if it contains a
    specifier twice.<p>
    The format model described in FormatDateTime is also used here, with the
    extension that a hyphen has the same meaning as '/'; it can match a hyphen,
    a slash, or DateSeparator. This has been added in order to make it easier
    to match ISO 8601 formats.<p>
    FormatStrToDateTime raises EConvertError in all cases of error, namely
    invalid format string, attempt to use day or month name specifiers in the
    format string (this is not supported), and using characters inside quotes in
    the format string which do not match characters in DateString.
    --WARNING (Stefanos 2009-11-05). T Token was changed in order to be equal
    with white space in order to split date and time part according to
    ISO-8601
    According to delphi documentation t token is used for:
           t   Displays the time using the format given by
         the ShortTimeFormat global variable
    Workarround: use actual short time format explicitly from SysUtils.--
    @author A.X.
    @SeeAlso <Jump File=Del5Vcl.hlp K="FormatDateTime function"
      Text=FormatDateTime>
}
function FormatStrToDateTime(const Format, DateString: string;
  HYearOrigin: Integer=10): TDateTime;

{** Months between two dates based in the formula
   (year2-year1)*12+(month2-month1) and the decodedate system function.
   Works only for year1, year2>=1 AD
}
function DiffInMonths(ADate1, ADate2: TDateTime): Integer;

implementation

{$C+}

uses Classes, StrUtils, DateUtils;

function DateTimeToC(DateTime: TDateTime): TCDateTime;
begin
  if DateTime>=0 then Result := DateTime
  else Result := Int(DateTime)-Frac(DateTime);
end;

function CToDateTime(CDateTime: TCDateTime): TDateTime;
begin
  if CDateTime>=0 then
    Result := CDateTime
  else if Int(CDateTime)=CDateTime then
    Result := CDateTime
  else
    Result := -2+Int(CDateTime)-Frac(CDateTime);
end;

function SubtractDateTime(a, b: TDateTime): Real;
begin
  Result := DateTimeToC(a) - DateTimeToC(b);
end;

function AddDateTime(Date: TDateTime; Offset: Real): TDateTime;
begin
  Result := CToDateTime(DateTimeToC(Date)+Offset);
end;

function DiffInSecs(a, b: TDateTime): Int64;
begin
  Result := Round((DateTimeToC(a) - DateTimeToC(b))*86400);
end;

function GetFullYear(TwoDigitYear: Word): Word;
var
  CurrentYear, CurrentTwoDigitYear, Dummy1, Dummy2, CurrentCenturyBase: Word;
  i: Integer;
begin
  DecodeDate(Date, CurrentYear, Dummy1, Dummy2);
  CurrentCenturyBase := (CurrentYear div 100)*100;
  CurrentTwoDigitYear := CurrentYear-CurrentCenturyBase;
  Result := CurrentCenturyBase+TwoDigitYear;
  if TwoDigitYearCenturyWindow=0 then Exit;
  i := CurrentTwoDigitYear-TwoDigitYearCenturyWindow;
  if i>=0 then
  begin
    if TwoDigitYear<i then
      Inc(Result, 100);
    Exit;
  end;
  i := CurrentTwoDigitYear+TwoDigitYearCenturyWindow;
  if i>=100 then Exit;
  if TwoDigitYear>i then
    Dec(Result, 100);
end;

resourcestring
  rsIsNotAValidDateTime = 'is not a valid date/time.';

var
  AllowedFormats: array[1..7] of string = ('yyyy-mm-dd hh:nn',
  'yyyy-mm-dd', 'yyyy-mm', 'yyyy/mm', 'yyyy', '', '');

function GetDateFormat(DateString: string; Options: TGetDateFormatOptions;
  HYearOrigin: Integer):
  string;
var
  i: Integer;
begin
  AllowedFormats[6] := ShortDateFormat + ' ' + ShortTimeFormat;
  AllowedFormats[7] := ShortDateFormat;
  Result := '';
  { Check the special hydrological year case first (because it takes
    precedence over YYYY-MM when the latter also matches). }
  if gdfAllowHydrologicalYear in Options then
  begin
    Result := 'aaaa-bb';
    try HYearToDate(DateString, HYearOrigin)
    except Result := '';
    end;
  end;
  if Result<>'' then Exit;
  for i := 1 to 7 do
  begin
    Result := AllowedFormats[i];
    try FormatStrToDateTime(Result, DateString);
    except Continue;
    end;
    Exit;
  end;
  { Was unable to recognize string. }
  if gdfRaiseOnFail in Options then
    raise EConvertError.Create(DateString + ' ' + rsIsNotAValidDateTime)
  else
    Result := '';
end;

{ TDateTimeList }

function TDateTimeList.Get(Index: Integer): TDateTime;
begin
  Result := TDateTime(inherited Get(Index));
end;

procedure TDateTimeList.Put(Index: Integer; Item: TDateTime);
begin
  inherited Put(Index, Item);
end;

function TDateTimeList.Add(Item: TDateTime): Integer;
begin
  Result := inherited Add(Item);
end;

function TDateTimeList.IndexOf(Item: TDateTime): Integer;
var i: Integer;
begin
  i := PositionOfNext(Item);
  if (i>=0) and (DiffInSecs(Items[i], Item)=0) then
    Result := i
  else
    Result := -1;
end;

procedure TDateTimeList.Insert(Index: Integer; Item: TDateTime);
begin
  inherited Insert(Index, Item);
end;

function TDateTimeList.InsertSorted(Item: TDateTime): Integer;
var i: Integer;
begin
  i := PositionOfNext(Item);
  if i=-1 then
    Result := Add(Item)
  else
  begin
    Insert(i, Item);
    Result := i;
  end;
end;

function TDateTimeList.PositionOfNext(Item: TDateTime): Integer;
var
  Low, High, Mid: Integer;
  Diff: Int64;
begin
  Low := 0;
  High := Count-1;
  while Low<=High do
  begin
    Mid := (Low+High) div 2;
    Diff := DiffInSecs(Item, Items[Mid]);
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

function TDateTimeList.PositionOfPrevious(Item: TDateTime): Integer;
begin
  if (Count=0) or (DiffInSecs(Item, First)<0) then
  begin
    Result := -1;
    Exit;
  end;
  Result := PositionOfNext(Item);
  if Result=-1 then
    Result := Count-1
  else if DiffInSecs(Item, Items[Result])<>0 then
    Dec(Result);
end;

function TDateTimeList.Remove(Item: TDateTime): Integer;
begin
  Result := inherited Remove(Item);
end;

{ TPeriod }

resourcestring
  rsInvalidPeriodDates = 'Start date greater than end date';

procedure TPeriod.SetDate (StartDate, EndDate: TDateTime);
begin
  if (EndDate<>idaContinues) and (EndDate<>idaEmpty) and (StartDate<>idaEmpty)
  and (DiffInSecs(EndDate, StartDate) < 0) then
    raise EConvertError.Create(DateTimeToStr(StartDate)+'-'
      +DateTimeToStr(EndDate)+': '+rsInvalidPeriodDates);
  FStartDate := StartDate;
  FEndDate := EndDate;
end;

constructor TPeriod.Create;
begin
  FStartDate := idaEmpty;
  FEndDate := idaEmpty;
end;

resourcestring
  rsDoesNotMatchFormat = 'is not a valid date and time according to the format';
  rsConvertingByNameNotSupported =
    'Converting days and months by day or month name is not supported.';
  rsIsNotAValidFormat = 'is not a valid date format.';

{$WARN UNSAFE_TYPE OFF}

function FormatStrToDateTime(const Format, DateString: string;
  HYearOrigin: Integer): TDateTime;
var
  PDateString: PChar;
  Year, Month, Day, Hour, Min, Sec, MSec: Word;

  procedure RaiseNoMatch;
  begin
    raise EConvertError.Create(''''+DateString+''' '+rsDoesNotMatchFormat+
      ' '''+Format+'''.');
  end;

  procedure RaiseInvalidFormat;
  begin
    raise EConvertError.Create(''''+Format+''' '+rsIsNotAValidFormat);
  end;

  function ReadFormat(Format: PChar): PChar;
  var
    Starter, Token, LastToken: Char;
    P: PChar;
    Count: Integer;

    procedure GetCount;
    var
      P: PChar;
    begin
      P := Format;
      while Format^ = Starter do Inc(Format);
      Count := Format - P + 1;
    end;

    function ReadNumber: Word;
    var i, c: Integer;
    begin
      Result := 0;
      for i := Count-1 downto 0 do
      begin
        c := Integer(PDateString^);
        if (c<48) or (c>57) then
          { If Count is 2 also allow 1. }
          if (Count=2) and (i=0) then Exit
          else RaiseNoMatch;
        Result := 10*Result+(c - 48);
        Inc(PDateString);
      end;
    end;

  begin
    LastToken := ' ';
    while Format^ <> #0 do
    begin
      Starter := Format^;
      Inc(Format);
      Token := Starter;
      if CharInSet(Token, ['a'..'z']) then Dec(Token, 32);
      if CharInSet(Token, ['A'..'Z']) then
      begin
	if (Token = 'M') and (LastToken = 'H') then Token := 'N';
	LastToken := Token;
      end;
      case Token of
	'Y':
	  begin
	    if Year<>65535 then RaiseInvalidFormat;
	    GetCount;
	    if Count <= 2 then
	      Year := GetFullYear(ReadNumber)
	    else
	      Year := ReadNumber;
	  end;
	'M':
	  begin
	    if Month<>65535 then RaiseInvalidFormat;
	    GetCount;
	    case Count of
	      1, 2: Month := ReadNumber;
	    else
	      raise EConvertError.Create(rsConvertingByNameNotSupported);
	    end;
	  end;
	'D':
	  begin
	    GetCount;
	    if Count=5 then ReadFormat(Pointer(ShortDateFormat))
	    else if Count>=3 then RaiseInvalidFormat
	    else if Day<>65535 then RaiseInvalidFormat
	    else Day := ReadNumber;
	  end;
	'H':
	  begin
	    GetCount;
	    if Hour<>65535 then RaiseInvalidFormat;
	    Hour := ReadNumber;
	  end;
	'N':
	  begin
	    GetCount;
	    if Min<>65535 then RaiseInvalidFormat;
	    Min := ReadNumber;
	  end;
	'S':
	  begin
	    GetCount;
	    if Sec<>65535 then RaiseInvalidFormat;
	    Sec := ReadNumber;
	  end;
	'Z':
	  begin
	    GetCount;
	    if MSec<>65535 then RaiseInvalidFormat;
	    MSec := ReadNumber;
	  end;
	'A':
	  begin
	    if Hour=65535 then RaiseInvalidFormat;
	    P := Format - 1;
	    if StrLIComp(P, 'AM/PM', 5) = 0 then
	    begin
	      Inc(Format, 4);
	      if StrLIComp(PDateString, 'PM', 2) = 0 then
	      begin
	        Inc(PDateString, 2);
		if Hour<>12 then Inc(Hour, 12);
	      end else if StrLIComp(PDateString, 'AM', 2) = 0 then
	      begin
	        Inc(PDateString, 2);
		if Hour=12 then Hour := 0;
	      end else
	        RaiseNoMatch;
	    end else if StrLIComp(P, 'A/P', 3) = 0 then
	    begin
	      Inc(Format, 2);
	      if StrLIComp(PDateString, 'P', 1) = 0 then
	      begin
	        Inc(PDateString);
		if Hour<>12 then Inc(Hour, 12);
	      end else if StrLIComp(PDateString, 'A', 1) = 0 then
	      begin
	        Inc(PDateString);
		if Hour=12 then Hour := 0;
	      end else
	        RaiseNoMatch;
	    end else if StrLIComp(P, 'AMPM', 4) = 0 then
	    begin
	      Inc(Format, 3);
	      if StrLIComp(PDateString, PChar(TimePMString),
              Length(TimePMString)) = 0 then
	      begin
	        Inc(PDateString, Length(TimePMString));
		if Hour<>12 then Inc(Hour, 12);
	      end else
	      if StrLIComp(PDateString, PChar(TimeAMString),
              Length(TimeAMString)) = 0 then
	      begin
	        Inc(PDateString, Length(TimeAMString));
		if Hour=12 then Hour := 0;
	      end else
	        RaiseNoMatch;
	    end
	    else
	      raise EConvertError.Create(rsConvertingByNameNotSupported);
	  end;
	'C':
	  begin
	    GetCount;
	    ReadFormat(Pointer(ShortDateFormat));
	    ReadFormat(Pointer(LongDateFormat));
	  end;
	'/', '-':
	  if CharInSet(PDateString^, ['/', '-', DateSeparator]) then
	    Inc(PDateString)
	  else
	    RaiseNoMatch;
	':':
	  if (PDateString^ = ':') or (PDateString^ = TimeSeparator) then
	    Inc(PDateString)
	  else
	    RaiseNoMatch;
        ' ', 'T':
          if (CharInSet(PDateString^ ,[' ', 'T'])) then Inc(PDateString)
          else RaiseNoMatch;
	'''', '"':
	  begin
	    P := Format;
	    while (Format^ <> #0) and (Format^ <> Starter) do
	      Inc(Format);
	    if StrLIComp(PDateString, P, Format - P)<>0 then
	      RaiseNoMatch;
	    Inc(PDateString, Format - P);
	    if Format^ <> #0 then Inc(Format);
	  end;
      else
	RaiseInvalidFormat;
      end;
    end;
    { Return next character of DateString }
    Result := PDateString;
  end;

var p: PChar;
begin
  PDateString := PChar(DateString);
  Year := 65535; Month := 65535; Day := 65535;
  Hour := 65535; Min := 65535; Sec := 65535; MSec := 65535;
  if Format <> '' then
  begin
    if Format<>'aaaa-bb' then
      p := ReadFormat(Pointer(Format))
    else begin
      Result := HYearToDate(DateString,  HYearOrigin);
      Exit;
    end;
  end else
    p := ReadFormat('C');
  if p^ <> #0 then RaiseNoMatch;
  if Year=65535 then RaiseInvalidFormat;
  if Month=65535 then Month := 1;
  if Day=65535 then Day := 1;
  if Hour=65535 then Hour := 0;
  if Min=65535 then Min := 0;
  if Sec=65535 then Sec := 0;
  if MSec=65535 then MSec := 0;
  if (Month<1) or (Month>12) or (Day>MonthDays[IsLeapYear(Year),Month]) or
  (Hour>23) or (Min>59) or (Sec>59) or (MSec>999) then
    RaiseNoMatch;
  Result := EncodeDateTime(Year, Month, Day, Hour, Min, Sec, MSec);
end;
{$WARN UNSAFE_TYPE ON}

function FindHydrologicalYear(ADate: TDateTime; HYearOrigin:
  Integer): Integer;
var Year, Month, Day: Word;
begin
  DecodeDate(ADate, Year, Month, Day);
  Result := Year;
  if Month >= HYearOrigin then Exit;
  Result := Year-1;
end;

resourcestring
  rsIsNotAValidHydrologicalYear = 'is not a valid hydrological year';

function HYearToDate(DateString: string; HYearOrigin: Integer): TDateTime;

  procedure RaiseException;
  begin
    raise
      EConvertError.Create(DateString + ' ' + rsIsNotAValidHydrologicalYear);
  end;

var
  yyyy, yy: string;
  i: Integer;
begin
  if Length(DateString)<>7 then RaiseException;
  if DateString[5] <> '-' then RaiseException;
  yyyy := Copy(DateString, 1, 4);
  yy := Copy (DateString, 6, 2);
  for i := 1 to 4 do if not CharInSet(yyyy[i], ['0'..'9']) then RaiseException;
  for i := 1 to 2 do if not CharInSet(yy[i], ['0'..'9']) then RaiseException;
  i := StrToInt(yy);
  if i=0 then i := 100;
  if i <> StrToInt(Copy(yyyy, 3, 2))+1 then RaiseException;
  Result := FormatStrToDateTime('yyyy-mm-dd hh:nn', yyyy+'-'+
    IntToStr(HYearOrigin)+'-01 00:00');
end;

function DiffInMonths(ADate1, ADate2: TDateTime): Integer;
var
  Day, Month1, Year1, Month2, Year2: Word;
begin
  DecodeDate(ADate1, Year1, Month1, Day);
  DecodeDate(ADate2, Year2, Month2, Day);
  Assert((Year1>0) and (Year2>0), 'Years should be >=1 AD');
  Result := (Year2-Year1)*12+(Month2-Month1);
end;

end.
