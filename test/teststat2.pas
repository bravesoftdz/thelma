unit teststat2;

interface

uses
  TestFramework;

type

  TTestStat2 = class(TTestCase)
  published
    procedure TestQuickSortAsc;
    procedure TestQuickOrderAsc;
  end;

implementation

uses SysUtils, stat2;

type
  TRealArrayOfTen = array[1..10] of Real;

const

  AFloatArray: TRealArrayOfTen = (
    8.46, 5.28, 4.63, 7.65, 4.32, 2.45, 5.84, 6.38, 7.05, 4.58
  );

  ASortedFloatArray: TRealArrayOfTen = (
    2.45, 4.32, 4.58, 4.63, 5.28, 5.84, 6.38, 7.05, 7.65, 8.46
  );

  AOrder: array[1..10] of Integer = (10, 5, 4, 9, 2, 1, 6, 7, 8, 3);

function ArraysEqual(array1, array2: array of Real; Tolerance: Real): Boolean;
var i: Integer;
begin
  Result := True;
  if Length(array1)<>Length(array2) then
  begin
    Result := False;
    Exit;
  end;
  for i := 0 to Length(array1)-1 do
    if Abs(array1[i]-array2[i]) > Tolerance then
    begin
      Result := False;
      Exit;
    end;
end;

procedure TTestStat2.TestQuickSortAsc;
var a: TRealArrayOfTen;
begin
  a := AFloatArray;
  QuickSortAsc(a, 10);
  Check(ArraysEqual(a, ASortedFloatArray, 1e-4));
end;

procedure TTestStat2.TestQuickOrderAsc;
var
  o: array [1..10] of Integer;
  i: Integer;
begin
  QuickOrderAsc(AFloatArray, o, 10);
  for i := 1 to 10 do
    CheckEquals(o[i], AOrder[i]);
end;

initialization
  RegisterTest(TTestStat2.Suite);

end.
