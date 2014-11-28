unit testdates;

interface

uses
  TestFramework, SysUtils, Dates, AnsiStrings, GenUtils;

type

  TTestDates = class(TTestCase)
  private
    FSavedFormatSettings: TFormatSettings;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddDateTime;
    procedure TestHYearToDate;
    procedure TestGetDateFormat;
  end;

implementation

procedure TTestDates.SetUp;
begin
  FSavedFormatSettings := FormatSettings;
  FormatSettings.ShortDateFormat := 'yyyy-mm-dd';
  FormatSettings.DateSeparator := '-';
  FormatSettings.ShortTimeFormat := 'hh:nn:ss';
end;

procedure TTestDates.TearDown;
begin
  FormatSettings := FSavedFormatSettings;
end;

procedure TTestDates.TestAddDateTime;
begin
  CheckEquals(AddDateTime(StrToDateTime('1950-01-01 00:00:00'), 10.25),
              StrToDateTime('1950-01-11 06:00:00'));
  CheckEquals(AddDateTime(StrToDateTime('1950-01-01 00:00:00'), -10.25),
              StrToDateTime('1949-12-21 18:00:00'));
  CheckEquals(AddDateTime(StrToDateTime('1850-01-01 00:00:00'), 10.25),
              StrToDateTime('1850-01-11 06:00:00'));
  CheckEquals(AddDateTime(StrToDateTime('1850-01-01 00:00:00'), -10.25),
              StrToDateTime('1849-12-21 18:00:00'));
  CheckEquals(AddDateTime(StrToDateTime('1899-12-22 00:00:00'), 10.25),
              StrToDateTime('1900-01-01 06:00:00'));
  CheckEquals(AddDateTime(StrToDateTime('1900-01-01 00:00:00'), -10.25),
              StrToDateTime('1899-12-21 18:00:00'));
  CheckEquals(AddDateTime(StrToDateTime('1950-01-01 00:00:00'), 0),
              StrToDateTime('1950-01-01 00:00:00'));
  CheckEquals(AddDateTime(StrToDateTime('1850-01-01 00:00:00'), 0),
              StrToDateTime('1850-01-01 00:00:00'));
  CheckEquals(AddDateTime(StrToDateTime('1899-12-30 00:00:00'), 0),
              StrToDateTime('1899-12-30 00:00:00'));
end;

procedure TTestDates.TestHYearToDate;
begin
  CheckEquals(HYearToDate('1950-51'), StrToDateTime('1950-10-01 00:00'));
  CheckEquals(HYearToDate('1999-00'), StrToDateTime('1999-10-01 00:00'));
  CheckEquals(HYearToDate('2000-01'), StrToDateTime('2000-10-01 00:00'));
  CheckEquals(HYearToDate('1899-00'), StrToDateTime('1899-10-01 00:00'));
  CheckEquals(HYearToDate('1900-01'), StrToDateTime('1900-10-01 00:00'));
  try
    HYearToDate('1950-52');
    Check(False, 'This should never execute');
  except
    on EConvertError do Check(True, 'HYearToDate properly raised exception');
  end;
  try
    HYearToDate('invalid');
    Check(False, 'This should never execute');
  except
    on EConvertError do Check(True, 'HYearToDate properly raised exception');
  end;
end;

procedure TTestDates.TestGetDateFormat;
begin
  FormatSettings.ShortDateFormat := 'dd/mm/yyyy';
  FormatSettings.ShortTimeFormat := 'hh:nn';
  FormatSettings.DateSeparator := '/';
  CheckEquals(GetDateFormat('2003-07-14 09:45', [gdfAllowHydrologicalYear]),
              'yyyy-mm-dd hh:nn');
  CheckEquals(GetDateFormat('2003-7-14 9:45', [gdfAllowHydrologicalYear]),
              'yyyy-mm-dd hh:nn');
  CheckEquals(GetDateFormat('2003-07-14', [gdfAllowHydrologicalYear]),
              'yyyy-mm-dd');
  CheckEquals(GetDateFormat('2003-07', [gdfAllowHydrologicalYear]), 'yyyy-mm');
  CheckEquals(GetDateFormat('2003-07', []), 'yyyy-mm');
  CheckEquals(GetDateFormat('2003-04', [gdfAllowHydrologicalYear]), 'aaaa-bb');
  CheckEquals(GetDateFormat('2003-04', []), 'yyyy-mm');
  CheckEquals(GetDateFormat('2003/04', [gdfAllowHydrologicalYear]), 'yyyy-mm');
  CheckEquals(GetDateFormat('2003', [gdfAllowHydrologicalYear]), 'yyyy');
  CheckEquals(GetDateFormat('14/07/2003 09:45', [gdfAllowHydrologicalYear]),
              'dd/mm/yyyy hh:nn');
  CheckEquals(GetDateFormat('4/7/2003 09:45', [gdfAllowHydrologicalYear]),
              'dd/mm/yyyy hh:nn');

  try
    GetDateFormat('07/14/2003 09:45',
                  [gdfAllowHydrologicalYear, gdfRaiseOnFail]);
    Check(False, 'This should never execute');
  except
    on EConvertError do Check(True, 'GetDateFormat properly raised exception');
  end;

  try
    GetDateFormat('2003-07-14 09:45trailing',
                  [gdfAllowHydrologicalYear, gdfRaiseOnFail]);
    Check(False, 'This should never execute');
  except
    on EConvertError do Check(True, 'GetDateFormat properly raised exception');
  end;

  try
    GetDateFormat('invalid',
                  [gdfAllowHydrologicalYear, gdfRaiseOnFail]);
    Check(False, 'This should never execute');
  except
    on EConvertError do Check(True, 'GetDateFormat properly raised exception');
  end;
end;

initialization
  RegisterTest(TTestDates.Suite);
end.

