unit testgenutils;

interface

uses TestFramework;

type
  TTestGenUtils = class(TTestCase)
  published
    procedure TestFloatPairListSort1;
    procedure TestGetFileVersionStr;
    procedure TestVersionComp;
  end;

implementation

uses SysUtils, Windows, GenUtils;

type TTestArray = array[1..10,1..2] of Real;

var

  AFloatPairListData: TTestArray = (
    (8.46, 4.05), (5.28, 3.88), (4.63, 7.82), (7.65, 8.78), (4.32, 9.49),
    (2.45, 1.72), (5.84, 5.40), (6.38, 3.46), (7.05, 7.98), (4.58, 8.97)
  );

  ASortedFloatPairListData: TTestArray = (
    (2.45, 1.72), (4.32, 9.49), (4.58, 8.97), (4.63, 7.82), (5.28, 3.88),
    (5.84, 5.40), (6.38, 3.46), (7.05, 7.98), (7.65, 8.78), (8.46, 4.05)
  );

function CompareFloatPairLists(TestedList: TFloatPairList; ReferenceList:
  TTestArray; Tolerance: Real): Boolean;
var i: Integer;
begin
  Result := True;
  for i := 1 to TestedList.Count do
    if (Abs(TestedList.Items[i-1].float1-ReferenceList[i,1])>Tolerance)
        or (Abs(TestedList.Items[i-1].float2-ReferenceList[i,2])>Tolerance) then
    begin
      Result := False;
      Exit;
    end;
end;

procedure TTestGenUtils.TestFloatPairListSort1;
var
  AFloatPairList: TFloatPairList;
  AFloatPair: TFloatPair;
  i: Integer;
begin
  AFloatPairList := TFloatPairList.Create();
  for i := 1 to 10 do
  begin
    AFloatPair.float1 := AFloatPairListData[i,1];
    AFloatPair.float2 := AFloatPairListData[i,2];
    AFloatPairList.Add(AFloatPair);
  end;
  AFloatPairList.Sort1;
  Check(CompareFloatPairLists(AFloatPairList, ASortedFloatPairListData, 1e-4));
end;

function StringCount(s: string; c: Char): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Length(s)-1 do
    if c = s[i] then
      Inc(Result);
end;

procedure TTestGenUtils.TestGetFileVersionStr;
var
  dir: array[0..MAX_PATH] of Char;
  FileName, VersionStr: string;
begin
  // Examine C:\Windows\explorer.exe; result should be 4 dot-separated numbers
  GetWindowsDirectory(dir, MAX_PATH);
  FileName := dir + '\explorer.exe';
  VersionStr := GetFileVersionStr(FileName, ver4Items);
  CheckEquals(StringCount(VersionStr, '.'), 3);

  // Examine thelmaTests.exe; result should be "dev"
  FileName := ParamStr(0);
  VersionStr := GetFileVersionStr(FileName, ver4Items);
  CheckEquals(VersionStr, 'dev');
end;

procedure TTestGenUtils.TestVersionComp;
begin
  CheckEquals(VersionComp('1.0.0.0', '2.0.0.0'), -1);
  CheckEquals(VersionComp('2.0.0.0', '1.0.0.0'), 1);
  CheckEquals(VersionComp('3.0.0.0', '3.0.0.0'), 0);
  CheckEquals(VersionComp('3.0.1.0', '3.0.0.0'), 1);
  CheckEquals(VersionComp('3.0.0.1', '3.0.0.2'), -1);
  CheckEquals(VersionComp('2.0.0.0', '10.0.0.0'), -1);
  CheckEquals(VersionComp('dev', '0.0.0.0'), -1);
  CheckEquals(VersionComp('1.0.0', '2.0.0'), -1);
  CheckEquals(VersionComp('1.2.3.4.5.6.7.8', '1.2.3.4.5.6.7.10'), -1);
end;

initialization
  RegisterTest(TTestGenUtils.Suite);

end.
