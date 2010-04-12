unit teststat2;

interface

function test(Verbose: Boolean): string;

implementation

uses SysUtils, stat2;

var

  TestsPassed: Integer;

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

procedure CompareFloatArrays(TestedArray: array of Real;
  ReferenceArray: array of Real; Tolerance: Real; Description: string);
var i: Integer;
begin
  if Length(TestedArray)<>Length(ReferenceArray) then
    raise Exception.Create('Failed ' + Description
      + ': arrays have unequal lenghts');
  for i := 0 to Length(TestedArray)-1 do
    if Abs(TestedArray[i]-ReferenceArray[i])>Tolerance then
      raise Exception.Create('Failed ' + Description + ': '
              + FloatToStr(TestedArray[i]) + ' != '
              + FloatToStr(ReferenceArray[i]));
end;

procedure TestQuickSortAsc;
var a: TRealArrayOfTen;
begin
  a := AFloatArray;
  QuickSortAsc(a, 10);
  CompareFloatArrays(a, ASortedFloatArray, 1e-4, 'sorting array');
  Inc(TestsPassed);
end;

procedure TestQuickOrderAsc;
var
  o: array [1..10] of Integer;
  i: Integer;
begin
  QuickOrderAsc(AFloatArray, o, 10);
  for i := 1 to 10 do
    if o[i]<>AOrder[i] then
      raise Exception.Create('Failed finding array order: '
        + IntToStr(o[i]) + ' != ' + IntToStr(AOrder[i]));
  Inc(TestsPassed);
end;

function test(Verbose: Boolean): string;
begin
  TestsPassed := 0;
  TestQuickSortAsc;
  TestQuickOrderAsc;
  Result := IntToStr(TestsPassed) + ' tests passed';
end;

end.
