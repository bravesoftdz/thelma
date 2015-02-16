{******************************************************************}
{                                                                  }
{  Thelma library                                                    }
{                                                                  }
{  Copyright (c) 2000-04 National Technical University of Athens   }
{                                                                  }
{******************************************************************}

{** Various general purpose utilities. }
unit GenUtils;

interface

uses SysUtils, Classes, zlib, SHFolder;

type
  TVersionFormat = (ver2Items, ver3Items, ver4Items, ver3ItemsPlusParentheses);

type
  TTimeseriesSelections = record
    Name: string;
    Description: string;
    MaxNum: Integer;
    MinNum: Integer;
    Timeseries: array of Integer;
  end;
  TTsSelectionsArray = array of TTimeseriesSelections;

{** Return a string, containing the special folder acording to folder param.
    Possible values of folder parameter are:
    Folder Name                            folder parameter
    ===============================================================
    //[Current User]\My Documents:         CSIDL_PERSONAL;
    //All Users\Application Data:          CSIDL_COMMON_APPDATA;
    //[User Specific]\Application Data:    CSIDL_LOCAL_APPDATA; (Local Settings)
                                           CSIDL_APPDATA;
    //Program Files:                       CSIDL_PROGRAM_FILES;
    //All Users\Documents                  CSIDL_COMMON_DOCUMENTS;
    You have to add "SHFolder" in uses section in order to use this function.
}
function GetSpecialFolderPath(folder : integer) : string;

///	<summary>
///	  Returns version of a file.
///	</summary>
///	<remarks>
///	  <para>
///	    Returns a string which specifies the file's version numbers separated
///	    by dots. The precise format of the string is specified by <c>Format</c>.
///	     If <c>Format=ver2Items</c>, the version major and minor numbers are
///	    returned. If <c>Format=ver3Items</c>, the release number is also
///	    returned. If <c>Format=ver4Items</c>, the build number is also
///	    returned. Finally, if <c>Format=ver3ItemsPlusParentheses</c>, the
///	    string has the format 'major.minor.release (.build)'.
///	  </para>
///	  <para>
///	    If the file has no version information, it returns "dev". 
///	  </para>
///	  <para>
///	    See also <see cref="GetFileVersion" />.
///	  </para>
///	</remarks>
function GetFileVersionStr(FileName: string; Format: TVersionFormat): string;

///	<summary>
///	  Compares two version strings.
///	</summary>
///	<remarks>
///	  <para>
///	    Returns zero, positive or negative if <c>AVer1</c> is equal to, greater
///	    than or less than <c>AVer2</c>.
///	  </para>
///	  <para>
///	    Version "dev", or any malformed version string, is always considered
///	    less than a normal version (this is because if a program checks for new
///	    versions, and accidentally a release manager uploads version "dev", and
///	    users download it, there would be a big mess if "dev" was considered
///	    greater; the program would never, ever again, find a new version when
///	    checking).
///	  </para>
///	</remarks>
function VersionComp(AVer1, AVer2: string): Integer;

{** Converts a boolean value to a string.
    BoolToStr returns the string 'True' or 'False' if Value is True or False.
    @SeeAlso <See Routine=StrToBool>
}
function BoolToStr(Value: Boolean): string;

{** Converts a string to a boolean value.
    StrToBool returns True or False if AString is 'True' or 'False'; it is
    case-insensitive. If AString is not one of 'True' or 'False', StrToBool
    raises an exception.
    @SeeAlso <See Routine=BoolToStr>
}
function StrToBool(AString: string): Boolean;

{** Converts decimal degrees to a degree, minute and second.
    Use DegreesToDegMinSec to convert Degrees into degree, minute and second.
    For example, 23.5 degrees is converted to 23 degrees, 30 minutes, 0 seconds.
    If Degrees is negative, all Deg, Min and Sec will be negative.
    @author A.X.
    @SeeAlso <See Routine=DegMinSecToDegrees>
}
procedure DegreesToDegMinSec(Degrees: Real; var Deg, Min: Integer; var Sec: Real);

{** Converts degree, minute and second to decimal degrees.
    Use DegMinSecToDegrees to convert degree, minute and second into decimal
    degrees. For example, 23 degrees, 30 minutes, 0 seconds is converted to
    23.5 degrees. Deg, Min and Sec had either all be positive or all be negative;
    otherwise, negative minutes will be subtracted from positive Degrees etc.
    @author A.X.
    @SeeAlso <See Routine=DegreesToDegMinSec>
}
function DegMinSecToDegrees(Deg, Min: Integer; Sec: Real): Real;

{** Decompresses stream AIn to AOut by using zlib capabilities.
    Use AOnProgress TNotifyEvent in order to implement a progress indicator.
    After Decompression of AIn to AOut, AOut position is at the end
    of the stream. You should manualy set AOut.Seek(0,0) in order to
    do some stream operations.
    @author Stefanos
    @SeeAlso <See Routine=CompressStream>
}
procedure DecompressStream(AIn, AOut: TStream; AOnProgress: TNotifyEvent);

{** Compress sream AIn to AOut by using zlib capabilities.
    Use AOnProgress TNotifyEvent in order to implement a progress indicator.
    After Compression of AIn to AOut, AOut position is at the end
    of the stream. You should manualy set AOut.Seek(0,0) in order to
    do some stream operations.
    @author Stefanos
    @SeeAlso <See Routine=DecompressStream>
}
procedure CompressStream(AIn, AOut: TStream; AOnProgress: TNotifyEvent);

type

  {** Stores an array of Integer numbers.
      TIntegerList is similar to TList, but stores integers rather than
      pointers. Its methods and properties have similar functionality to those
      of TList.
      @author A.X.
      @SeeAlso <Jump File=Del5Vcl.hlp K="TList," Text=TList>
  }
  TIntegerList = class(TPersistent)
  private
    Data: array of Integer;
  protected
    function Get(Index: Integer): Integer;
    procedure Put(Index: Integer; Item: Integer);
  public
    procedure Assign(Source: TPersistent); override;
    destructor Destroy; override;
    property Items[Index: Integer]: Integer read Get write Put; default;
    function Add(Item: Integer): Integer;
    function Count: Integer;
    procedure Clear; virtual;
    procedure Delete(Index: Integer);
    function First: Integer;
    function IndexOf(Item: Integer): Integer;
    procedure Insert(Index: Integer; Item: Integer);
    function Last: Integer;
    function Remove(Item: Integer): Integer;
  end;

  {** Stores a list of Double numbers.
      TFloatList is similar to TList, but stores Doubles rather than
      pointers. Its methods and properties have similar functionality to those
      of TList.
      @author A.X.
      @SeeAlso <Jump File=Del5Vcl.hlp K="TList," Text=TList>
   }
  TFloatList = class(TPersistent)
  private
    Data: array of Real;
  protected
    function Get(Index: Integer): Real;
    procedure Put(Index: Integer; Item: Real);
  public
    procedure Assign(Source: TPersistent); override;
    property Items[Index: Integer]: Real read Get write Put;
    destructor Destroy; override;
    function Add(Item: Real): Integer;
    function Count: Integer;
    procedure Clear; virtual;
    procedure Delete(Index: Integer);
    function First: Real;
    function IndexOf(Item: Real): Integer;
    procedure Insert(Index: Integer; Item: Real);
    function Last: Real;
    function Remove(Item: Real): Integer;
    procedure Sort;
  end;


   TFloatPair = record
     float1:Real;
     float2:Real;
   end;

  {** Stores a list of paired Double numbers.
      TFloatPairList is similar to TList, but stores a pair of Doubles rather than
      pointers. Its methods and properties have similar functionality to those
      of TList.
      @author A.X., G.K.
      @SeeAlso <Jump File=Del5Vcl.hlp K="TList," Text=TList>
   }
  TFloatPairList = class(TPersistent)
  private
    Data: array of TFloatPair;
  protected
    function Get(Index: Integer): TFloatPair;
    procedure Put(Index: Integer; Item: TFloatPair);
  public
    procedure Assign(Source: TPersistent); override;
    property Items[Index: Integer]: TFloatPair read Get write Put;
    destructor Destroy; override;
    function Add(Item: TFloatPair): Integer;
    function Count: Integer;
    procedure Clear; virtual;
    procedure Delete(Index: Integer);
    function First: TFloatPair;
    function IndexOf1(Item: Real): Integer;
    function IndexOf2(Item: Real): Integer;
    procedure Insert(Index: Integer; Item: TFloatPair);
    function Last: TFloatPair;
    function Remove1(Item: Real): Integer;
    function Remove2(Item: Real): Integer;
{** Sorts according to the first pair member.}
    procedure Sort1;
  end;


  type
  TStreamReadLnStatus = (srlsSuccessive, srlsEof);

  {** Reads a string from a stream (line feed delimited #10).
      StreamReadLn supplies a functionality similar to the
      standard ReadLn procedure. Stream is read character to character, until
      a line feed is parsed (#10). Characters are placed consecutive to
      the string s.<p>
      Carriage returns (#13) are ignored, this feature allow to read from
      unix text files.<p>
      If StreamReadLn reaches AStream end, then it stops parsing the stream and
      a srlsEof is returned. In other cases, when parsing stops from line
      feed, srlsSuccessive is returned.
      @author Stefanos
      @SeeAlso <See Routine=StreamWriteLn>
  }
  function StreamReadLn(AStream: TStream; var s: string): TStreamReadLnStatus;

  {** Write a string to a stream, with CR, LF (#13, #10).
      StreamWriteLn supplies a functionality similar to the WriteLn procedure.
      Srings are placed to the stream with CR, LF to the end of the stream.
      @SeeAlso <See Routine=StreamReadLn>
  }
  procedure StreamWriteLn(AStream: TStream; const s: string);

  {** Encrypt a string using the Key value by scrambling characters.
      This is a low security algorithm that scrambles the characters
      using the Key. Key is changing on successive characters.<p>
      *Do not use this routine for high security encryption.*
      @SeeAlso <See Routine=Decrypt>
  }
  function Encrypt(const s: string; Key: Word): string;
  {** Decrypt the string encrypted with the Encrypt function.
      @SeeAlso <See Routine=Encrypt>
  }
  function Decrypt (const s: string; Key: Word): string;

implementation

{$C+}

uses Types, Windows, stat2, istrutils, Generics.Collections, Generics.Defaults;

function GetSpecialFolderPath(folder : integer) : string;
const
  SHGFP_TYPE_CURRENT = 0;
var
  path: array [0..MAX_PATH] of char;
begin
  if SUCCEEDED(SHGetFolderPath(0,folder,0,SHGFP_TYPE_CURRENT,@path[0])) then
    Result := path
  else
    Result := '';
end;

resourcestring
  rsCannotGetFileVersionInfo = 'System error in function '+
    'GetFileVersionInfoSize. Error code: ';

{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CODE OFF}

function GetFileVersionStr(FileName: string; Format: TVersionFormat): string;
var
  Major, Minor, Release, Build: Integer;
  p, p1: Pointer;
  Size, DummyDWord: DWord;
  DummyUInt: UInt;
begin
  Result := 'dev';
  Size := GetFileVersionInfoSize(PChar(FileName), DummyDWord);
  if Size=0 then
    Exit;
  GetMem(p, Size);
  try
    GetFileVersionInfo(PChar(FileName), DummyDWord, Size, p);
    VerQueryValue(p, '\', p1, DummyUInt);
    Build := PVSFixedFileInfo(p1)^.dwFileVersionLS mod 65536;
    Release := PVSFixedFileInfo(p1)^.dwFileVersionLS div 65536;
    Minor := PVSFixedFileInfo(p1)^.dwFileVersionMS mod 65536;
    Major := PVSFixedFileInfo(p1)^.dwFileVersionMS div 65536;
  finally
    FreeMem(p);
  end;
  Result := IntToStr(Major) + '.' + IntToStr(Minor);
  if Format = ver2Items then Exit;
  Result := Result + '.' + IntToStr(Release);
  if Format = ver3Items then Exit;
  case Format of
    ver4Items: Result := Result + '.' + IntToStr(Build);
    ver3ItemsPlusParentheses: Result := Result + ' (.' + IntToStr(Build) + ')';
  else
    Assert(False);
  end;
end;

{$WARN UNSAFE_TYPE ON}
{$WARN UNSAFE_CODE ON}

function NormalizeVersion(AVersionString: string): string;
var
  i: Integer;
  s: string;
begin
  Result := '';
  try
    while Length(AVersionString) > 0 do
    begin
      i := Pos('.', AVersionString);
      if i = 0 then
        i := Length(AVersionString) + 1;
      s := Copy(AVersionString, 1, i - 1);
      Result := Result + Format('%.4d', [StrToInt(s)]);
      if i >= Length(AVersionString) then
        Exit;
      AVersionString := Copy(AVersionString, i + 1, Length(AVersionString));
    end;
  except
    on EConvertError do
    begin
      Result := '';
      Exit;
    end;
  end;
end;

function VersionComp(AVer1, AVer2: string): Integer;
var
  s1, s2: string;
begin
  s1 := NormalizeVersion(AVer1);
  s2 := NormalizeVersion(AVer2);
  if s1 > s2 then
    Result := 1
  else if s1 < s2 then
    Result := -1
  else
    Result := 0;
end;

function BoolToStr(Value: Boolean): string;
begin
  if Value then Result := 'True' else Result := 'False';
end;

resourcestring
  rsCantConvertToBoolean =
    'This string cannot be converted to a boolean value.';

function StrToBool(AString: string): Boolean;
begin
  if LowerCase(AString)='true' then
    Result := True
  else if LowerCase(AString)='false' then
    Result := False
  else
    raise EConvertError.Create(AString+': '+rsCantConvertToBoolean);
end;

procedure DegreesToDegMinSec(Degrees: Real; var Deg, Min: Integer; var Sec: Real);
var f: Real;
begin
  Deg := Trunc(Degrees);
  f := Frac(Degrees)*60;
  Min := Trunc(f);
  Sec := Frac(f)*60;
end;

function DegMinSecToDegrees(Deg, Min: Integer; Sec: Real): Real;
begin
  Result := Deg+Min/60+Sec/3600;
end;

{ TIntegerList }

resourcestring
  rsListIndexOutOfBounds = 'List index out of bounds';

destructor TIntegerList.Destroy;
begin
  SetLength(Data, 0);
end;

function TIntegerList.Count: Integer;
begin
  Result := Length(Data);
end;

procedure TIntegerList.Assign(Source: TPersistent);
var
  i: Integer;
begin
  if Source is TIntegerList then
  begin
    Self.Clear;
    with Source as TIntegerList do
      for i := 0 to Count-1 do
        Self.Add(Items[i])
  end else inherited Assign(Source);
end;

function TIntegerList.Get(Index: Integer): Integer;
begin
  Result := Data[Index];
end;

procedure TIntegerList.Put(Index: Integer; Item: Integer);
var i: Integer;
begin
  i := Length(Data);
  if Index > i then
    raise EListError.Create(rsListIndexOutOfBounds)
  else if Index = i then
    SetLength(Data, i+1);
  Data[Index] := Item;
end;

function TIntegerList.Add(Item: Integer): Integer;
begin
  Result := Length(Data);
  SetLength(Data, Result+1);
  Data[Result] := Item;
end;

procedure TIntegerList.Clear;
begin
  SetLength(Data, 0);
end;

function TIntegerList.First: Integer;
begin
  Result := Data[0];
end;

function TIntegerList.IndexOf(Item: Integer): Integer;
begin
  for Result := 0 to Count-1 do
    if Items[Result]=Item then
      Exit;
  Result := -1;
end;

procedure TIntegerList.Insert(Index: Integer; Item: Integer);
var i: Integer;
begin
  i := Length(Data);
  if Index > i then
    raise ERangeError.Create(rsListIndexOutOfBounds);
  SetLength(Data, i+1);
  while i > Index do
  begin
    Data[i] := Data[i-1];
    Dec(i);
  end;
  Data[i] := Item;
end;

function TIntegerList.Last: Integer;
begin
  Result := Data[Length(Data)-1];
end;

procedure TIntegerList.Delete(Index: Integer);
var i: Integer;
begin
  i := Length(Data);
  while Index < i-1 do
  begin
    Data[Index] := Data[Index+1];
    Inc(Index);
  end;
  SetLength(Data, i-1);
end;

function TIntegerList.Remove (Item: Integer): Integer;
begin
  Result := IndexOf(Item);
  Delete(Result);
end;

{ TFloatList }

destructor TFloatList.Destroy;
begin
  SetLength(Data, 0);
end;

procedure TFloatList.Assign(Source: TPersistent);
var
  i: Integer;
begin
  if Source is TFloatList then
  begin
    Self.Clear;
    with Source as TFloatList do
      for i := 0 to Count-1 do
        Self.Add(Items[i])
  end else inherited Assign(Source);
end;

function TFloatList.Count: Integer;
begin
  Result := Length(Data);
end;

procedure TFloatList.Clear;
begin
  SetLength(Data, 0);
end;

function TFloatList.Get(Index: Integer): Real;
begin
  Result := Data[Index];
end;

procedure TFloatList.Put(Index: Integer; Item: Real);
var i: Integer;
begin
  i := Length(Data);
  if Index > i then
    raise EListError.Create(rsListIndexOutOfBounds)
  else if Index = i then
    SetLength(Data, i+1);
  Data[Index] := Item;
end;

function TFloatList.Add(Item: Real): Integer;
begin
  Result := Length(Data);
  SetLength(Data, Result+1);
  Data[Result] := Item;
end;

function TFloatList.First: Real;
begin
  Result := Items[0];
end;

function TFloatList.IndexOf(Item: Real): Integer;
begin
  for Result := 0 to Count-1 do
    if Items[Result]=Item then
      Exit;
  Result := -1;
end;

procedure TFloatList.Insert(Index: Integer; Item: Real);
var i: Integer;
begin
  i := Length(Data);
  if Index > i then
    raise ERangeError.Create(rsListIndexOutOfBounds);
  SetLength(Data, i+1);
  while i > Index do
  begin
    Data[i] := Data[i-1];
    Dec(i);
  end;
  Data[i] := Item;
end;

function TFloatList.Last: Real;
begin
  Result := Data[Length(Data)-1];
end;

procedure TFloatList.Delete(Index: Integer);
var i: Integer;
begin
  i := Length(Data);
  while Index < i-1 do
  begin
    Data[Index] := Data[Index+1];
    Inc(Index);
  end;
  SetLength(Data, i-1);
end;

function TFloatList.Remove (Item: Real): Integer;
begin
  Result := IndexOf(Item);
  Delete(Result);
end;

procedure TFloatList.Sort;
begin
  QuickSortAsc(Data, Count);
end;


{ TFloatPairList }

destructor TFloatPairList.Destroy;
begin
  SetLength(Data, 0);
end;

procedure TFloatPairList.Assign(Source: TPersistent);
var
  i: Integer;
begin
  if Source is TFloatPairList then
  begin
    Self.Clear;
    with Source as TFloatPairList do
      for i := 0 to Count-1 do
        Self.Add(Items[i])
  end else inherited Assign(Source);
end;

function TFloatPairList.Count: Integer;
begin
  Result := Length(Data);
end;

procedure TFloatPairList.Clear;
begin
  SetLength(Data, 0);
end;

function TFloatPairList.Get(Index: Integer): TFloatPair;
begin
  Result := Data[Index];
end;

procedure TFloatPairList.Put(Index: Integer; Item: TFloatPair);
var i: Integer;
begin
  i := Length(Data);
  if Index > i then
    raise EListError.Create(rsListIndexOutOfBounds)
  else if Index = i then
    SetLength(Data, i+1);
  Data[Index].float1 := Item.float1;
  Data[Index].float2 := Item.float2;
end;

function TFloatPairList.Add(Item: TFloatPair): Integer;
begin
  Result := Length(Data);
  SetLength(Data, Result+1);
  Data[Result].float1 := Item.float1;
  Data[Result].float2 := Item.float2;
end;

function TFloatPairList.First: TFloatPair;
begin
  Result := Items[0];
end;

function TFloatPairList.IndexOf1(Item: Real): Integer;
begin
  for Result := 0 to Count-1 do
    if Items[Result].float1=Item then
      Exit;
  Result := -1;
end;

function TFloatPairList.IndexOf2(Item: Real): Integer;
begin
  for Result := 0 to Count-1 do
    if Items[Result].float2=Item then
      Exit;
  Result := -1;
end;

procedure TFloatPairList.Insert(Index: Integer; Item: TFloatPair);
var i: Integer;
begin
  i := Length(Data);
  if Index > i then
    raise ERangeError.Create(rsListIndexOutOfBounds);
  SetLength(Data, i+1);
  while i > Index do
  begin
    Data[i] := Data[i-1];
    Dec(i);
  end;
  Data[i].float1 := Item.float1;
  Data[i].float2 := Item.float2;
end;

function TFloatPairList.Last: TFloatPair;
begin
  Result := Data[Length(Data)-1];
end;

procedure TFloatPairList.Delete(Index: Integer);
var i: Integer;
begin
  i := Length(Data);
  while Index < i-1 do
  begin
    Data[Index] := Data[Index+1];
    Inc(Index);
  end;
  SetLength(Data, i-1);
end;

function TFloatPairList.Remove1 (Item: Real): Integer;
begin
  Result := IndexOf1(Item);
  Delete(Result);
end;

function TFloatPairList.Remove2 (Item: Real): Integer;
begin
  Result := IndexOf2(Item);
  Delete(Result);
end;

procedure TFloatPairList.Sort1;
var Comparer: IComparer<TFloatPair>;
begin
  Comparer := TDelegatedComparer<TFloatPair>.Create(
    function(const Left, Right: TFloatPair): Integer
    begin
      if Left.float1>Right.float1 then
        Result := 1
      else if Left.float1<Right.float1 then
        Result := -1
      else
        Result := 0;
    end);
  TArray.Sort<TFloatPair>(Data, Comparer);
end;



(***************************************************)


function StreamReadLn(AStream: TStream; var s: string): TStreamReadLnStatus;
var
  ReadChar: Char;
  cRead: Integer;
  AIndex: Integer;
  ASize: Integer;
begin
  Result := srlsSuccessive;
  AIndex := 0;
  ASize := 128;
  SetLength(s, ASize);
  ReadChar := #0;
  try
    repeat
      cRead := AStream.Read(ReadChar,1);
      if (cRead>0) and (ReadChar<>#13) and (ReadChar<>#10) then
      begin
        if AIndex=ASize then
        begin
          ASize := ASize+128;
          SetLength(s, ASize);
        end;
{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CODE OFF}
        PChar(s)[AIndex] := ReadChar;
{$WARN UNSAFE_TYPE ON}
{$WARN UNSAFE_CODE ON}
        Inc(AIndex);
      end;
    until (ReadChar = #10) or (cRead=0);
    SetLength(s, AIndex);
  except
    SetLength(s, 0);
    raise;
  end;
  if cRead = 0 then
    Result := srlsEof;
end;

procedure StreamWriteLn(AStream: TStream; const s: string);
var
  AString: string;
begin
  AString := s+#13#10;
{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CODE OFF}
  AStream.Write(PChar(AString)^, Length(AString));
{$WARN UNSAFE_TYPE ON}
{$WARN UNSAFE_CODE ON}
end;

procedure DecompressStream(AIn, AOut: TStream; AOnProgress: TNotifyEvent);
var
  x: TDecompressionStream;
  buf: packed array[Word] of Byte;
  read_in: Integer;
begin
  x := nil;
  try
    x := TDecompressionStream.Create(AIn);
    x.OnProgress := AOnProgress;
    repeat
      read_in := x.Read(buf, SizeOf(buf));
      if read_in > 0 then
        AOut.Write(buf, read_in);
    until read_in < SizeOf(buf);
  finally
    x.Free;
  end;
end;

procedure CompressStream(AIn, AOut: TStream; AOnProgress: TNotifyEvent);
var
  x: TCompressionStream;
  buf: packed array[Word] of Byte;
  write_out: Integer;
begin
  x := nil;
  try
    x := TCompressionStream.Create(clDefault, AOut);
    x.OnProgress := AOnProgress;
    repeat
      write_out := AIn.Read(buf, SizeOf(buf));
      if write_out > 0 then
        x.Write(buf, write_out);
    until write_out < SizeOf(buf);
  finally
    x.Free;
  end;
end;

const
   c1 = 49194;
   c2 = 23526;

function Encrypt(const s: string; Key: Word): string;
var
  i : byte;
  s2: string;
begin
  SetLength(s2, Length(s));
  for i := 1 to Length(s) do
  begin
{$WARN UNSAFE_CODE OFF}
    s2[i] := Char (byte (s[i]) xor (Key shr 8));
{$WARN UNSAFE_CODE ON}
    Key := Word((Word(byte (s2[i])) + Key) * c1 + c2);
  end;
  Result := '';
  for i := 1 to Length(s2) do
    Result := Result + IntToHex(byte(s2[i]),2);
end;

function Decrypt (const s: string; Key: Word): string;
var
  i : byte;
  s2: string;
begin
  SetLength(s2, Length(s) div 2);
  i := 1;
  while i <= Length(s) do
  begin
{$WARN UNSAFE_CODE OFF}
    s2[1+ (i div 2)] := Char(StrToInt('$'+ s[i]+ s[i+1]));
{$WARN UNSAFE_CODE ON}
    i := i + 2;
  end;
  SetLength(Result, Length(s2));
  for i := 1 to Length (s2) do
  begin
{$WARN UNSAFE_CODE OFF}
    Result[i] := Char (byte (s2[i]) xor (Key shr 8));
{$WARN UNSAFE_CODE ON}
    Key := Word((byte (s2[i]) + Key) * c1 + c2);
  end;
end;

end.
