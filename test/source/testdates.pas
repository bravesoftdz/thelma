unit testdates;

interface

function test(Verbose: Boolean): string;

implementation

uses dates, sysutils;

var TestsPassed: Integer;

{ AddDateTime }

type TAddDateTimeTestData = record
  Date: string;
  Offset: Real;
  Output: string;
end;

var AddDateTimeTestData: array[1..9] of TAddDateTimeTestData = (
  (Date: '1950-01-01 00:00:00'; Offset:  10.25; Output: '1950-01-11 06:00:00'),
  (Date: '1950-01-01 00:00:00'; Offset: -10.25; Output: '1949-12-21 18:00:00'),
  (Date: '1850-01-01 00:00:00'; Offset:  10.25; Output: '1850-01-11 06:00:00'),
  (Date: '1850-01-01 00:00:00'; Offset: -10.25; Output: '1849-12-21 18:00:00'),
  (Date: '1899-12-22 00:00:00'; Offset:  10.25; Output: '1900-01-01 06:00:00'),
  (Date: '1900-01-01 00:00:00'; Offset: -10.25; Output: '1899-12-21 18:00:00'),
  (Date: '1950-01-01 00:00:00'; Offset:      0; Output: '1950-01-01 00:00:00'),
  (Date: '1850-01-01 00:00:00'; Offset:      0; Output: '1850-01-01 00:00:00'),
  (Date: '1899-12-30 00:00:00'; Offset:      0; Output: '1899-12-30 00:00:00')
);

procedure TestAddDateTime;
var
  i: Integer;
  d: TAddDateTimeTestData;
  date: TDateTime;
  output: string;
begin
  for i := 1 to 9 do
  begin
    d := AddDateTimeTestData[i];
    date := FormatStrToDateTime('yyyy-mm-dd hh:nn:ss', d.Date);
    DateTimeToString(output, 'yyyy-mm-dd hh:nn:ss', AddDateTime(date, d.Offset));
    if output<>d.output then
      raise Exception.Create('Failed test '+IntToStr(i)+' of TestAddDateTime: '+
        #13#10'  Output is '+output+'; should be '+d.output);
    Inc(TestsPassed);
  end;
end;

{ HYearToDate }

type THYearToDateTestData = record
  input, output: string;
end;

var HYearToDateTestData: array [1..7] of THYearToDateTestData = (
  (input: '1950-51'; output: '1950-10-01 00:00'),
  (input: '1999-00'; output: '1999-10-01 00:00'),
  (input: '2000-01'; output: '2000-10-01 00:00'),
  (input: '1899-00'; output: '1899-10-01 00:00'),
  (input: '1900-01'; output: '1900-10-01 00:00'),
  (input: '1950-52'; output: ''),
  (input: 'invalid'; output: '')
);

procedure TestHYearToDate;
var
  i: Integer;
  d: THYearToDateTestData;
  o: string;
  raised: Boolean;
begin
  for i := 1 to 7 do
  begin
    d := HYearToDateTestData[i];
    raised := False;
    try DateTimeToString(o, 'yyyy-mm-dd hh:nn', HYearToDate(d.input));
    except raised := True;
    end;
    if (d.output='') and not raised then
      raise Exception.Create('Failed test '+IntToStr(i)+' of TestHYearToDate:'+
        #13#10'  Did not raise exception when input "'+d.input+'"')
    else if (d.output<>'') and raised then
      raise Exception.Create('Failed test '+IntToStr(i)+' of TestHYearToDate:'+
        #13#10'  Raised exception when input "'+d.input+'"')
    else if not raised and (d.output<>o) then
      raise Exception.Create('Failed test '+IntToStr(i)+' of TestHYearToDate:'+
        #13#10'  Output is '+o+'; should be '+d.output);
    Inc(TestsPassed);
  end;
end;

{ GetDateFormat }

type TGetDateFormatTestData = record
  input: string;
  ahy: Boolean; { Allow hydrological year }
  output: string;
end;

{ Tests are to be made with ShortDateFormat set to dd/mm/yyyy and
  ShortTimeFormat set to hh:nn. }
var GetDateFormatTestData: array[1..14] of TGetDateFormatTestData = (
  (input: '2003-07-14 09:45'; ahy: True;  output: 'yyyy-mm-dd hh:nn'),
  (input: '2003-7-14 9:45';   ahy: True;  output: 'yyyy-mm-dd hh:nn'),
  (input: '2003-07-14';       ahy: True;  output: 'yyyy-mm-dd'),
  (input: '2003-07';          ahy: True;  output: 'yyyy-mm'),
  (input: '2003-07';          ahy: False; output: 'yyyy-mm'),
  (input: '2003-04';          ahy: True;  output: 'aaaa-bb'),
  (input: '2003-04';          ahy: False; output: 'yyyy-mm'),
  (input: '2003/04';          ahy: True;  output: 'yyyy-mm'),
  (input: '2003';             ahy: True;  output: 'yyyy'),
  (input: '14/07/2003 09:45'; ahy: True;  output: 'dd/mm/yyyy hh:nn'),
  (input: '4/7/2003 09:45';   ahy: True;  output: 'dd/mm/yyyy hh:nn'),
  (input: '07/14/2003 09:45'; ahy: True;  output: ''),
  (input: '2003-07-14 09:45trailing'; ahy: True; output: ''),
  (input: 'invalid';          ahy: True;  output: '')
);

procedure TestGetDateFormat;
var
  i: Integer;
  d: TGetDateFormatTestData;
  Options: TGetDateFormatOptions;
  output: string;
  raised: Boolean;
begin
  ShortDateFormat := 'dd/mm/yyyy';
  ShortTimeFormat := 'hh:nn';
  for i := 1 to 14 do
  begin
    d := GetDateFormatTestData[i];
    Options := [];
    if d.ahy then Options := Options + [gdfAllowHydrologicalYear];
    output := GetDateFormat(d.input, Options);
    if output<>d.output then
      raise Exception.Create('Failed test '+IntToStr(i)+' of TestGetDateFormat:'+
        #13#10'  Output is "'+output+'"; should be "'+d.output+'"');
    { If output was empty, meaning "error", also test the exception version. }
    if output='' then
    begin
      Options := Options + [gdfRaiseOnFail];
      raised := false;
      try GetDateFormat(d.input, Options);
      except raised := True;
      end;
      if not raised then
        raise Exception.Create('Failed test '+IntToStr(i)+' of TestGetDateFormat:'+
          #13#10'  Did not raise exception when input "'+d.input+'"')
    end;
    Inc(TestsPassed);
  end;
end;

{ Main }

function test(Verbose: Boolean): string;
begin
  TestsPassed := 0;
  TestAddDateTime;
  TestHYearToDate;
  TestGetDateFormat;
  Result := IntToStr(TestsPassed) + ' tests passed';
end;

end.
