{******************************************************************}
{                                                                  }
{  Thelma library                                                  }
{                                                                  }
{  Copyright (c) 2000-09 National Technical University of Athens   }
{                                                                  }
{******************************************************************}

{** This is a unit for set operations. It is in a very primitive form.
    Arrays are used. In a future version, OOP will be used.
    Unit first version: September 4, 2009.
    @Author Stefanos
}
unit isets;

interface

type
  TSetOfIntegers = array of Integer;

  TSetOfSetsOfIntegers = array of TSetOfIntegers;

function ConcatenateElementToSetOfSets(AInteger: Integer;
  ASetOfSets: TSetOfSetsOfIntegers): TSetOfSetsOfIntegers;

function IntegerToSet(AInteger: Integer): TSetOfIntegers;

function AddSetOfSets(ASetOfSet1, ASetOfSet2: TSetOfSetsOfIntegers):
  TSetOfSetsOfIntegers;

function GetAllSets(ASet: TSetOfIntegers): TSetOfSetsOfIntegers;

function ElementInSet(Element: Integer; ASet: TSetOfIntegers): Boolean;

implementation

function IntegerToSet(AInteger: Integer): TSetOfIntegers;
begin
  SetLength(Result, 1);
  Result[0] := AInteger;
end;

function ConcatenateElementToSetOfSets(AInteger: Integer;
  ASetOfSets: TSetOfSetsOfIntegers): TSetOfSetsOfIntegers;
var
  i, j: Integer;
begin
  SetLength(Result, Length(ASetOfSets));
  for i := 0 to Length(ASetOfSets)-1 do
  begin
    SetLength(Result[i], Length(ASetOfSets[i])+1);
    Result[i][0] := AInteger;
    for j := 0 to Length(ASetOfSets[i])-1 do
      Result[i][j+1] := ASetOfSets[i][j];
  end;
end;

function AddSetOfSets(ASetOfSet1, ASetOfSet2: TSetOfSetsOfIntegers):
  TSetOfSetsOfIntegers;
var
  i, j: Integer;
begin
  SetLength(Result, Length(ASetOfSet1)+Length(ASetOfSet2));
  j := 0;
  for i := 0 to Length(ASetOfSet1)-1 do
  begin
    Result[j] := ASetOfSet1[i];
    Inc(j);
  end;
  for i := 0 to Length(ASetOfSet2)-1 do
  begin
    Result[j] := ASetOfSet2[i];
    Inc(j);
  end;
end;

function GetAllSets(ASet: TSetOfIntegers): TSetOfSetsOfIntegers;
var
  ASubSet: TSetOfIntegers;
  ASetOfSets: TSetOfSetsOfIntegers;
  i: Integer;
begin
  Result := nil;
  if Length(ASet)<1 then Exit;
  SetLength(ASubSet, Length(ASet)-1);
  for i := 1 to Length(ASet)-1 do
    ASubSet[i-1] := ASet[i];
  ASetOfSets := GetAllSets(ASubSet);
  SetLength(Result, 1);
  Result[0] := IntegerToSet(ASet[0]);
  Result := AddSetOfSets(Result, ConcatenateElementToSetOfSets(ASet[0],
    ASetOfSets));
  Result := AddSetOfSets(Result, ASetOfSets);
end;

function ElementInSet(Element: Integer; ASet: TSetOfIntegers): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to Length(ASet)-1 do
    if ASet[i]=Element then
    begin
      Result := True;
      Break;
    end;
end;

end.
