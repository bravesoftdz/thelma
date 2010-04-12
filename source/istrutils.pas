 {******************************************************************}
{                                                                  }
{  Thelma library                                                  }
{                                                                  }
{  Copyright (c) 2000-01 National Technical University of Athens   }
{                                                                  }
{******************************************************************}

{** Various string utilities. }
unit istrutils;

interface

uses SysUtils, Classes;

{** Returns an item from a delimited string.
s must contain elements delimited by Delimiter. This function returns delimited
item no. Index, where items are numbered starting at 1. If s contains less than
Index items, DelimitedStringItem raises EListError.
@author A.X.
@SeeAlso <See Routine=DelimitedStringContains>
@SeeAlso <See Routine=DelimitedStringCount>
@SeeAlso <See Routine=TrimAllSpaces>
}
function DelimitedStringItem(const s: string; Index: Integer; Delimiter: Char):
  string;

{** Counts number of items in a delimited string.
Returns 0 if s is empty or one more than the number of occurrences of the
Delimiter in s.
@author A.X.
@SeeAlso <See Routine=DelimitedStringItem>
@SeeAlso <See Routine=DelimitedStringContains>
@SeeAlso <See Routine=TrimAllSpaces>
}
function DelimitedStringCount(const s: string; Delimiter: Char): Integer;

{** Checks whether a delimited string contains another string.
s must contain elements delimited by Delimiter. If one or more of these is the
same as Item, DelimitedStringContains returns True, else False.
@author A.X.
@SeeAlso <See Routine=DelimitedStringItem>
@SeeAlso <See Routine=DelimitedStringCount>
@SeeAlso <See Routine=TrimAllSpaces>
}
function DelimitedStringContains(const s, Item: string; Delimiter: Char):
  Boolean;

{** Converts all series of consecutive spaces into one space.
TrimAllSpaces removes all leading and trailing white space, and for intermediate
white space it converts all consecutive spaces into one space. It is especially
useful before using the DelimitedString functions if the delimiter is a space.
@author A.X.
@SeeAlso <See Routine=DelimitedStringItem>
@SeeAlso <See Routine=DelimitedStringCount>
@SeeAlso <See Routine=DelimitedStringContains>
}
function TrimAllSpaces(s: string): string;

{** Returns the byte index in S of the first character that matches any character in the Delimiters AnsiString.
Call FirstDelimiter to locate the first delimiter in S.<P>
When working with multi-byte character sets (MBCS), S may contain double byte
characters, but the delimiters listed in the Delimeters parameter must all be
single byte non-null characters.
@author A.X.
@SeeAlso <Jump File=Delphi5.hlp K="LastDelimiter function" Text=LastDelimiter>
}
function FirstDelimiter(const Delimiters, S: string): Integer;

{** Returns the byte index in S of the first character that does not match any character in MatchingChars.
Call FirstNonMatchingChar to locate the first character in S that is not
included in MatchingChars.<P>
When working with multi-byte character sets (MBCS), S may contain double byte
characters, but the characters listed in the MatchingChars parameter must all be
single byte non-null characters.
@author A.X.
@SeeAlso <See Method=FirstDelimiter>
}
function FirstNonMatchingChar(const MatchingChars, S: string): Integer;

type

  {** Maintains a table of strings.
      TStringTable is like a TStringList, but it holds the strings in a
      two-dimensional table rather than a one-dimensional list.
  }
  TStringTable = class(TPersistent)
  private
    FStrings: TStringList;
    FRowCount, FColCount: Integer;
    procedure SetRowCount(Value: Integer);
    procedure ReadColCount(Reader: TReader);
    procedure WriteColCount(Writer: TWriter);
    procedure ReadStrings(Reader: TReader);
    procedure WriteStrings(Writer: TWriter);
  protected
    function Get(ACol, ARow: Integer): string;
    procedure Put(ACol, ARow: Integer; Value: string);
    procedure DefineProperties(Filer: TFiler); override;
  public
    {** Returns or sets the number of rows.
        Read RowCount to determine the number of rows in the string table. Set
        RowCount to truncate the table or to add rows with empty strings.
    }
    property RowCount: Integer read FRowCount write SetRowCount;
    {** Returns the number of columns.
        Read ColCount to determine the number of columns in the string table.
        Currently ColCount is specified at creation time and cannot be changed
        thereafter.
    }
    property ColCount: Integer read FColCount;
    {** Accesses the string on a specified row and column.
        Use Strings to read or modify the string at the specified position,
        using 0-based indexes for the row and column.<p>
        Strings is the default property; the Strings identifier can be omitted
        when accessing the Strings property of a string list object.
    }
    property Strings[ACol, ARow: Integer]: string read Get write Put; default;
    {** Creates a TStringTable object.
    }
    constructor Create(ARowCount, AColCount: Integer);
    {** Destroys a TStringTable object.
        As always, you should always use Free rather than Destroy.
    }
    destructor Destroy; override;
    {** Sets the string table from a source object.
        Use Assign to copy a TStringTable object to another object.
        @SeeAlso <Jump File=Delphi5.hlp K="TPersistent,Assign" Text=TPersistent.Assign>
    }
    procedure Assign(Source: TPersistent); override;
    {** Returns the row of a string in a specified column of the table.
        Call RowIndexOf to search column ACol for string S. RowIndexOf returns
        the 0-based index of the string's row. If the string is not in column
        ACol of the string table, RowIndexOf returns -1.
        @SeeAlso <See Method=RowIndexOf>
    }
    function RowIndexOf(ACol: Integer; const S: string): Integer; virtual;
    {** Returns the column of a string in a specified row of the table.
        Call ColIndexOf to search row ARow for string S. ColIndexOf returns
        the 0-based index of the string's column. If the string is not in row
        ARow of the string table, ColIndexOf returns -1.
        @SeeAlso <See Method=ColIndexOf>
    }
    function ColIndexOf(ARow: Integer; const S: string): Integer; virtual;
  end;

type
  {** These are the options for parsing with ImportDataToTimeseries.
  }
  TImportDataToTimeseriesOptions = record
  {** Set DateColumn to 0 if dates are not parsed.
      If dates are not parsed, then StartDate, and DatesIncrement are used.}
    DateColumn: Integer;
  {** A DataColumne should be specified (first column = 1) or else Assertion
      Failure occurs.
  }
    DataColumn: Integer;
  {** Data delimiter}
    Delimiter: Char;
  {** Enter Flags Column, fist column = 1. If set to zero, no flags are read}
    FlagsColumn: Integer;
  {** Symbol used as Flag Delimiter such as space.}
    FlagsDelimiter: string;
  {** Decimal symbol (such as . )}
    DecimalSymbol: Char;
  {** Use this option to process lines with trimallspaces
  }
    TrimSpaces: Boolean;
  {** Store this property to use later to TTimeseries.ImportData.
  }
    Overwrite: Boolean;
  {** if a NullValueString found, treat this record as null.
  }
    NullValueString: string;
  {** Specify DateFormat or leave-it empty to auto detect.
  }
    DateFormat: string;
  {** Specify StartDate when no parsing dates}
    StartDate: TDateTime;
  {** Specify Increments when no parsing dates}
    DateIncrementMonths: Integer;
  {** Specify Increments when no parsing dates}
    DateIncrementMinutes: Integer;
  {** Specify Encoding of files}
    Encoding: TEncoding;
  {** Halt parsing on read errors}
    HaltOnError: Boolean;
  {** Ignore empty lines (proceed to next)}
    IgnoreEmptyLines: Boolean;
  {** First line, 1 is the first line}
    FirstLine: Integer;
  end;

{** This procedure parses strings from InputStrings and puts to Output for
    futher process with ts.TTimeseries.ImportData by using the Options record.

}
procedure ImportDataToTimeseries(Input, Output: TStream;
  Options: TImportDataToTimeseriesOptions);

implementation

uses
  Dates, StrUtils;

{$C+}

resourcestring
  rsInvalidStringIndex = 'This string does not have that many items: ';

{$WARN UNSAFE_TYPE OFF}

function DelimitedStringItem (const s: string; Index: Integer; Delimiter: Char):
  string;
var p1, p2: PChar;
begin
  p1 := PChar(s);
  while Index>1 do begin
    if p1^ = #0 then
      raise EListError.Create(''''+s+''''+#13#10+rsInvalidStringIndex
        +IntToStr(Index));
    if p1^ = delimiter then Dec(Index);
    Inc(p1);
  end;
  p2 := p1;
  while (p2^ <> #0) and (p2^ <> delimiter) do
    Inc(p2);
  Dec(p2);
  Result := Copy(string(p1), 1, p2-p1+1);
end;

function DelimitedStringCount(const s: string; Delimiter: Char): Integer;
var p: PChar;
begin
  p := PChar(s);
  Result := 0;
  if p^=#0 then Exit;
  Result := 1;
  while p^<>#0 do
  begin
    if p^ = Delimiter then Inc(Result);
    Inc(p);
  end;
end;

function DelimitedStringContains(const s, Item: string; Delimiter: Char):
  Boolean;
var ps, p: PChar;
    Len: Integer;
begin
  Result := False;
  ps := PChar(S);
  p := ps-1;
  Len := Length(Item);
  repeat begin
    Inc(p);
    p := StrPos(p, PChar(Item));
    if p = nil then Exit;
  end
  until ((p = ps) or ((p-1)^ = delimiter))
    and ((((p+len)^ = #0)) or ((p+len)^ = delimiter));
  Result := True;
end;

function TrimAllSpaces(s: string): string;
var
  p: PChar;
  InSpace: Boolean;
begin
  Result := '';
  s := Trim(s);
  p := PChar(s);
  InSpace := False;
  while p^ <> #0 do
  begin
    if p^<>#32 then
    begin
      InSpace := False;
      Result := Result+p^;
    end
    else
    begin
      if not InSpace then
      begin
        Result := Result+p^;
        InSpace := True;
      end;
    end;
    Inc(p);
  end;
end;

function FirstDelimiter(const Delimiters, S: string): Integer;
var
  P: PChar;
begin
  Result := 1;
  P := PChar(Delimiters);
  while Result <= Length(S) do
  begin
    if (S[Result] <> #0) and (StrScan(P, S[Result]) <> nil) then
      if (ByteType(S, Result) = mbTrailByte) then
        Inc(Result)
      else
        Exit;
    Inc(Result);
  end;
  Result := 0;
end;

function FirstNonMatchingChar(const MatchingChars, S: string): Integer;
var
  P: PChar;
begin
  Result := 1;
  P := PChar(MatchingChars);
  while Result <= Length(S) do
  begin
    if (S[Result] <> #0) and (StrScan(P, S[Result]) = nil) then
      if (ByteType(S, Result) = mbTrailByte) then
        Inc(Result)
      else
        Exit;
    Inc(Result);
  end;
  Result := 0;
end;

{$WARN UNSAFE_TYPE ON}

{ TStringTable }

constructor TStringTable.Create(ARowCount, AColCount: Integer);
var i: Integer;
begin
  inherited Create;
  FStrings := TStringList.Create;
  FRowCount := ARowCount;
  FColCount := AColCount;
  for i:=ARowCount*AColCount downto 1 do
    FStrings.Add('');
end;

destructor TStringTable.Destroy;
begin
  FStrings.Free;
  inherited Destroy;
end;

procedure TStringTable.SetRowCount(Value: Integer);
var i: Integer;
begin
  if Value=FRowCount then Exit;
  if Value>FRowCount then
    while FRowCount<Value do
    begin
      for i := ColCount-1 downto 0 do
        FStrings.Add('');
      Inc(FRowCount);
    end
  else
    while FRowCount>Value do
    begin
      for i := ColCount-1 downto 0 do
        FStrings.Delete(FStrings.Count-1);
      Dec(FRowCount);
    end;
  Assert(FRowCount=Value);
  Assert(FStrings.Count=FRowCount*FColCount);
end;

resourcestring
  rsRowIndexOutOfBounds = 'Row index out of bounds';
  rsColIndexOutOfBounds = 'Col index out of bounds';

function TStringTable.Get(ACol, ARow: Integer): string;
begin
  if (ARow<0) or (ARow>=FRowCount) then
    raise EListError.Create(rsRowIndexOutOfBounds+': '+IntToStr(ARow));
  if (ACol<0) or (ACol>=FColCount) then
    raise EListError.Create(rsColIndexOutOfBounds+': '+IntToStr(ACol));
  Result := FStrings[ARow*FColCount+ACol];
end;

procedure TStringTable.Put(ACol, ARow: Integer; Value: string);
begin
  if (ARow<0) or (ARow>=FRowCount) then
    raise EListError.Create(rsRowIndexOutOfBounds+': '+IntToStr(ARow));
  if (ACol<0) or (ACol>=FColCount) then
    raise EListError.Create(rsColIndexOutOfBounds+': '+IntToStr(ACol));
  FStrings[ARow*FColCount+ACol] := Value;
end;

procedure TStringTable.Assign(Source: TPersistent);
var Src: TStringTable;
begin
  Src := TStringTable(Source);
  FStrings.Assign(Src.FStrings);
  FRowCount := Src.FRowCount;
  FColCount := Src.FColCount;
end;

function TStringTable.RowIndexOf(ACol: Integer; const S: string): Integer;
begin
  for Result := 0 to FRowCount-1 do
    if Strings[ACol, Result] = S then Exit;
  Result := -1;
end;

function TStringTable.ColIndexOf(ARow: Integer; const S: string): Integer;
begin
  for Result := 0 to FColCount-1 do
    if Strings[Result, ARow] = S then Exit;
  Result := -1;
end;

procedure TStringTable.ReadColCount(Reader: TReader);
begin
  FColCount := Reader.ReadInteger;
end;

procedure TStringTable.WriteColCount(Writer: TWriter);
begin
  Writer.WriteInteger(FColCount);
end;

procedure TStringTable.ReadStrings(Reader: TReader);
begin
  Reader.ReadListBegin;
  FStrings.Clear;
  while not Reader.EndOfList do FStrings.Add(Reader.ReadString);
  Reader.ReadListEnd;
  FRowCount := FStrings.Count div FColCount;
end;

procedure TStringTable.WriteStrings(Writer: TWriter);
var
  I: Integer;
begin
  Writer.WriteListBegin;
  for I := 0 to FStrings.Count - 1 do Writer.WriteString(FStrings[I]);
  Writer.WriteListEnd;
end;

procedure TStringTable.DefineProperties(Filer: TFiler);
begin
  Filer.DefineProperty('ColCount', ReadColCount, WriteColCount, True);
  Filer.DefineProperty('Strings', ReadStrings, WriteStrings, True);
end;


procedure ImportDataToTimeseries(Input, Output: TStream;
  Options: TImportDataToTimeseriesOptions);
var
  ADateFormat, AValueStr, s, AFlagString: string;
  AStreamReader: TStreamReader;
  AStreamWriter: TStreamWriter;
  ADate: TDateTime;
  ParseDates: Boolean;
  ParseFlags: Boolean;
  SavedDecimalSeparator: Char;
  i: Integer;

  function ConvertTextValue(AValue: string): string;
  var
    ARealValue: Real;
    ASavedDecSeparator: Char;
  begin
    Result := '';
    if AValue = '' then Exit;
    ARealValue := StrToFloat(AValue);
    ASavedDecSeparator := SysUtils.DecimalSeparator;
    try
      SysUtils.DecimalSeparator := '.';
      Result := FloatToStr(ARealValue);
    finally
      SysUtils.DecimalSeparator := ASavedDecSeparator;
    end;
  end;

begin
  AStreamReader := nil;
  AStreamWriter := nil;
  SavedDecimalSeparator := SysUtils.DecimalSeparator;
  try
    SysUtils.DecimalSeparator := Options.DecimalSymbol;
    AStreamReader := TStreamReader.Create(Input, Options.Encoding);
    AStreamWriter := TStreamWriter.Create(Output, Options.Encoding);
    ADateFormat := Options.DateFormat;
    ParseDates := (Options.DateColumn<>0);
    ParseFlags := (Options.FlagsColumn<>0);
    Assert(Options.DataColumn>0);
    ADate := Options.StartDate;
    i := 0;
    with Options do
      while not AStreamReader.EndOfStream do
      begin
        Inc(i);
        s := AStreamReader.ReadLine;
        if i<FirstLine then Continue;
        if TrimSpaces then TrimAllSpaces(s);
        if (IgnoreEmptyLines) and (s = '') then Continue;
        if ParseDates then
          if ADateFormat = '' then
            ADateFormat := GetDateFormat(
              DelimitedStringItem(s, DateColumn, Delimiter), [gdfRaiseOnFail]);
        try
          if ParseDates then
            ADate := FormatStrToDateTime(ADateFormat,
              DelimitedStringItem(s, DateColumn, Delimiter));
          if ParseFlags then
            AFlagString := DelimitedStringItem(s, FlagsColumn, Delimiter)
            else AFlagString := '';
          if (AFlagString<>'') and (FlagsDelimiter<>' ') then
            AFlagString := ReplaceText(AFlagString, FlagsDelimiter, ' ');
          AValueStr := DelimitedStringItem(s, DataColumn, Delimiter);
          if AValueStr = NullValueString then AValueStr := '';
          s := FormatDateTime('yyyy-mm-dd hh:nn', ADate);
          s := s + ',' + ConvertTextValue(AValueStr) + ',' + AFlagString;
          AStreamWriter.WriteLine(s);
          if not ParseDates then
          begin
            ADate := IncMonth(ADate, DateIncrementMonths);
            ADate :=  AddDateTime(ADate, DateIncrementMinutes/1440);
          end;
        except
          if HaltOnError then raise;
        end;
      end;
  finally
    SysUtils.DecimalSeparator := SavedDecimalSeparator;
    if AStreamWriter<>nil then AStreamWriter.Close;
    if AStreamReader<>nil then AStreamReader.Close;
    AStreamWriter.Free;
    AStreamReader.Free;
  end;
end;

end.
