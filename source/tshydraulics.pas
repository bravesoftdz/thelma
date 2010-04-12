{******************************************************************}
{                                                                  }
{  Thelma library                                                  }
{                                                                  }
{  Copyright (c) 2000-04 National Technical University of Athens   }
{                                                                  }
{******************************************************************}

{** Some simple hydraulic calculations.
}
unit tshydraulics;

interface

uses Classes, math, dates, interpol;

type
{** TXSectionNode represents a node from the section of an open channel.
    The node is determined with the x and y coordinates.
}
  TXSectionNode = record
    x, y: Real;
    PreIndex, PostIndex: Integer;
  end;

type TArrayOfTXSectionNode = array of TXSectionNode;

type TArrayOfReal = array of Real;

type TSliceRegion = record
  x1, x2: Real;
end;

type TArrayOfTSliceRegion = array of TSliceRegion;

type
  THydrometricPair = record
    Depth, DepthRatio, Velocity: Real;
    ID: Integer;
  end;

type
  TArrayOfHydrometricPair = array of THydrometricPair;

type
  THydrometryMode = (thymOnePointStandard=0, thymTwoPointsStandard=1,
    thymThreePointsStandard=2, thymRandomPointsLogarithmic=3,
      thymRandomPointsParabola=4);

type
  THydrometricSliceProperties = record
    XPosition: Real;
    Measures: TArrayOfHydrometricPair;
    Mode: THydrometryMode;
    Date: TDateTime;
    Depth: Real;
    IsMeasuredActualDepth: Boolean;
  end;

type
  THydrometricSliceValues = record
    Properties: THydrometricSliceProperties;
    Area, MeanVelocity, coef1, coef2, detfac: Real;
    x1, x2: Real;
    Sketch: TArrayOfTXSectionNode;
    ID: Integer;
  end;

type
  TSlicesMasterArray = array of THydrometricSliceValues;

type
{** Specifies the calculation model for open channel discharge.
}
  TOpenChannelModel = (ocmManning, ocmChezy);

{** Compares two TXSectionNode records.
    XSectionNodesEqual returns True if its two arguments have identical
    co-ordinates. Equality tests are performed, which might not be
    appropriate for floating point numbers.
}
function XSectionNodesEqual(Node1, Node2: TXSectionNode): Boolean;

type
{** A class to hold Cross-sections objects.
}
  TXSection = class(TPersistent)
  private
    FNodes: array of TXSectionNode;
    FCount: Integer;
  protected
    function Get(Index: Integer): TXSectionNode;
    procedure Put(Index: Integer; Item: TXSectionNode);
  public
    {**
    }
    StartDate: TDateTime;
    {**
    }
    Active: Boolean;
    {**
    }
    EndDate: TDateTime;
    {** The number of nodes in the xsection.
        Count is the number of entries in the Nodes array.
	@SeeAlso <See Property=Points>
    }
    property Count: Integer read FCount;
    {** Stores the nodes.
        Nodes is an array of TXSectionNode records, each of which holds a node
	of the section. These nodes may be either in direct or reverse order.
    }
    property Nodes[Index: Integer]: TXSectionNode read Get write Put;
    {** Creates a TXSection object.
    }
    constructor Create;
    {** Destroyes a TXSection object.
    }
    destructor Destroy; override;
    {** Copies a TXSection object.
        @SeeAlso <Jump File=Delphi5.hlp K="TPersistent,Assign" Text=Assign>
    }
    procedure Assign(Source: TPersistent); override;
    {** Adds a node to the end of the Nodes array.
        Add adds the specified point to the end of the nodes array and
        returns its index.
    }
    function Add(ANode: TXSectionNode): Integer; overload;
    {** An overloaded method in order to add arithmetic values instead directly,
        instead of TXSectionNode).
    }
    function Add(x,y: Real): Integer; overload;
    {** Deletes all nodes.
        Call Clear to empty the Nodes array and set the Count to 0.
    }
    procedure Clear;
    {** Removes the node at the position given by the Index parameter.
        Call Delete to remove from the Nodes array the item with the specified
	Index.
	@SeeAlso <See Method=Remove>
    }
    procedure Delete(Index: Integer);
    {** Returns Nodes[0].
    }
    function First: TXSectionNode;
    {** Returns the index of the first entry in the Nodes array with a specified value.
        Call IndexOf to get the index of a point in the Nodes array.<p>
	IndexOf makes simple equality tests, which may be inappropriate for
	floating point values. However, IndexOf is only provided for
	completeness and compatibility to classes such as TList. At the time of
	this writing, it is not clear if and how IndexOf will be used, so
	specifying tolerances is left for later.
    }
    function IndexOf(Item: TXSectionNode): Integer;
    {** Inserts a node at the position specified by Index.
        Call Insert to add a node to the middle of the Nodes array.
    }
    procedure Insert(Index: Integer; Item: TXSectionNode); overload;
    {** An overloaded method of Insert to specify directely coordinates.
    }
    procedure Insert(Index: Integer; x, y: Real); overload;
    {** Returns Nodes[Count-1].
    }
    function Last: TXSectionNode;
    {** Deletes the first matching item from the Nodes array.
        Call Remove to remove a specific node from the Nodes array when its
	index is unknown. The value returned is the index of the point in the
	Nodes array before it was removed.<p>
	If the Nodes array contains more than one copy of the point, only the
	first copy is deleted.<p>
	Remove uses IndexOf to locate the point, so the tolerance problem
	described in the help for IndexOf also applies here.
	@SeeAlso <See Method=Delete>
	@SeeAlso <See Method=IndexOf>
    }
    function Remove(Item: TXSectionNode): Integer;
    {** Calculates the area and the wet perimeter of the section for a given stage.
        Returns an array with node cuts.
    }
    function CalcAreaAndPerimeter(Stage: Real;
      var Area, Perimeter: Real): TArrayOfTXSectionNode;
    {** Calculates the hydraulic radius of the sectino for a given stage.
    }
    function CalcHydraulicRadius(Stage: Real): Real;
    {** Calculates the velocity for a given stage according to Model.
    }
    function CalcWaterVelocity(Stage, Roughness, Slope: Real;
      Model: TOpenChannelModel): Real; overload;
    {** Calculates the discharge for a given stage according to Model.
    }
    function CalcWaterDischarge(Stage, Roughness, Slope: Real;
      Model: TOpenChannelModel): Real; overload;
    {** Calculates the velocity for a given stage with the manning equation.
    }
    function CalcWaterVelocity(Stage, Roughness, Slope: Real): Real; overload;
    {** Calculates the discharge for a given stage with the manning equation.
    }
    function CalcWaterDischarge(Stage, Roughness, Slope: Real): Real; overload;
    {** Read Nodes data from a text file
    }
    procedure LoadFromFile(FileName: string);
    {** Write Nodes data in a text file, except descriptive fields
        such as remarks and so
    }
    procedure WriteToFile(FileName: string);
    function MinX: Real;
    function MaxX: Real;
    function MinY: Real;
    function MaxY: Real;
    procedure FitOnHydrometricPoints(ATransientCurveList: TTransientCurveList;
      CurveNo, PointsCount: Integer; var Coefficient, Determination: Real); overload;
    procedure FitOnHydrometricPoints(ATransientCurveList: TTransientCurveList;
      CurveNo, PointsCount: Integer; var Coefficient, Determination: Real;
        Model: TOpenChannelModel); overload;
    {**
    }
    procedure CreateSliceSection(x1, x2, Stage: Real;
      ANewSection: TXSection);
    {**
    }
    function CalcDepthAtPosition(XPosition: Real): Real;
    {**
    }
    function FindHydrometricRegions(CutNodes: TArrayOfTXSectionNode;
      HydrometricPositions: TArrayOfReal; IsMeasuredFromCut: Boolean):
      TArrayOfTSliceRegion;
  end;

{** The Manning equation for the water velocity in an open channel.
    Equation: Velocity := (1/Roughness)*(HydrRadius^[2/3])*Sqrt(Slope)
}
function ManningVelocity(Roughness, Slope, Radius: Real): Real;
{** The Manning equation for the water discharge in an open channel.
    Equation: Discharge := ManningVelocity*Area
}
function ManningDischarge(Roughness, Slope, Radius, Area: Real): Real;
{** The Chezy equation for the water velocity in an open channel.
    Equation: Velocity := Roughness*Sqrt(HydrRadius*Slope)
}
function ChezyVelocity(Roughness, Slope, Radius: Real): Real;
{** The Chezy equation for the water discharge in an open channel.
    Equation: Velocity := Roughness*Area*Sqrt(HydrRadius*Slope)
}
function ChezyDischarge(Roughness, Slope, Radius, Area: Real): Real;
{** The Hydraulic radius is the ratio of the Water Area to the wet perimeter.
}
function HydraulicRadius(Area, Perimeter: Real): Real;
{**
}
function CalcSliceMeanVelocity(Properties: THydrometricSliceProperties;
  var Coef1, Coef2, DetFac: Real): Real;
{**
}
function CalcTotalSlicesMeanVelocity(SliceValues: TSlicesMasterArray): Real;
{**
}
function CalcHydrometricMeanVelocityFromSection(Stage: Real;ASection: TXSection;
  IsMeasuredFromCut: Boolean; var Slices: TSlicesMasterArray): Real;

implementation

uses GenUtils, SysUtils, istrutils;

function IsOdd(ANumber: Integer): Boolean;
begin
  Result := (ANumber mod 2) <> 0;
end;

function XSectionNodesEqual(Node1, Node2: TXSectionNode): Boolean;
begin
  Result := ( Abs(Node1.x-Node2.x)<0.0002 ) and ( Abs(Node1.y-Node2.y)<0.0002 );
end;

constructor TXSection.Create;
begin
  inherited Create;
  SetLength(FNodes, 5);
  FCount := 0;
  Active := False;
end;

destructor TXSection.Destroy;
begin
  SetLength(FNodes, 0);
  inherited Destroy;
end;

procedure TXSection.Assign(Source: TPersistent);
var i: Integer;
begin
  with Source as TXSection do
  begin
    Self.Clear;
    for i := 0 to Count-1 do
      Self.Add(Nodes[i]);
  end;
end;

resourcestring
  rsListIndexOutOfBounds = 'List index out of bounds';

function TXSection.Get(Index: Integer): TXSectionNode;
begin
  if (Index<0) or (Index>=FCount) then
    raise EListError.Create(rsListIndexOutOfBounds + '('+IntToStr(Index)+')');
  Result := FNodes[Index];
end;

procedure TXSection.Put(Index: Integer; Item: TXSectionNode);
begin
  if (Index<0) or (Index>=FCount) then
    raise EListError.Create(rsListIndexOutOfBounds + '('+IntToStr(Index)+')');
  FNodes[Index] := Item;
end;

function TXSection.Add(ANode: TXSectionNode): Integer;
begin
  if FCount = Length(FNodes) then
    SetLength(FNodes, Length(FNodes)+5);
  Result := FCount;
  FNodes[Result] := ANode;
  Inc(FCount);
end;

function TXSection.Add(x,y: Real): Integer;
var
  ANode: TXSectionNode;
begin
  ANode.x := x;
  ANode.y := y;
  Result := Add(ANode);
end;

procedure TXSection.Clear;
begin
  FCount := 0;
  SetLength(FNodes, 5);
end;

procedure TXSection.Delete(Index: Integer);
begin
  if (Index<0) or (Index>=FCount) then
    raise EListError.Create(rsListIndexOutOfBounds + '('+IntToStr(Index)+')');
  while Index<FCount-1 do
  begin
    FNodes[Index] := FNodes[Index+1];
    Inc(Index);
  end;
  Dec(FCount);
end;

function TXSection.First: TXSectionNode;
begin
  Result := FNodes[0];
end;

function TXSection.Last: TXSectionNode;
begin
  Result := FNodes[FCount-1];
end;

function TXSection.IndexOf(Item: TXSectionNode): Integer;
begin
  Result := FCount-1;
  while Result>=0 do
  begin
    if XSectionNodesEqual(Item, FNodes[Result]) then Exit;
    Dec(Result);
  end;
end;

procedure TXSection.Insert(Index: Integer; Item: TXSectionNode);
var
  i: Integer;
begin
  if FCount = Length(FNodes) then
    SetLength(FNodes, Length(FNodes)+5);
  Inc(FCount);
  for i := FCount-1 downto Index+1 do
    FNodes[i] := FNodes[i-1];
  FNodes[Index] := Item;
end;

procedure TXSection.Insert(Index: Integer; x, y: Real);
var
  ANode: TXSectionNode;
begin
  ANode.x := x;
  ANode.y := y;
  Insert(Index, ANode);
end;

function TXSection.Remove(Item: TXSectionNode): Integer;
begin
  Result := IndexOf(Item);
  if Result>=0 then
    Delete(Result);
end;

function InterpolateForY(y: Real; x1,y1,x2,y2: Real): Real;
begin
  if abs(y2-y1)<0.0001 then
    Result := 0.5*(y1+y2)
  else
    Result := x1 + (x2-x1)*(y-y1)/(y2-y1);
end;

function InterpolateForX(x: Real; x1,y1,x2,y2: Real): Real;
begin
  Result := InterpolateForY(x, y1, x1, y2, x2);
end;

resourcestring
  rsTooFewPoints = 'Too few points to calculate area and perimeter';
  rsXSectionSeemsNotToBeClosed =
    'Water Cross-Section seems not to be a closed line';
  rsWaterStageSeemsNotToBeCutSection =
    'Water stage seems not to be intersecting the cross-section';
  rsStageIsAboveTheSection =
    'Water stage is above the end points of the section. Cannot calculate '+
      'Water area and perimeter.';
  rsFirstPointShouldHaveLessX =
    'First point should be left of the last point';

function TXSection.CalcAreaAndPerimeter(Stage: Real;
  var Area, Perimeter: Real): TArrayOfTXSectionNode;

  function CalcLength(xi, yi: array of Real): Real;
  var i: Integer;
  begin
    Assert(Length(xi)=Length(yi));
    Result := 0;
    for i := 0 to Length(xi)-2 do
      Result := Result +
        Sqrt(Sqr(xi[i+1]-xi[i])+Sqr(yi[i+1]-yi[i]));
  end;

  function CalcArea(xi, yi: array of Real): Real;
  var i: Integer;

    function AIndex(j: Integer): Integer;
    begin
      if j< Length(xi) then
        Result := j
      else
        Result := AIndex(j-Length(xi));
    end;

  begin
    Assert(Length(xi)=Length(yi));
    Result := 0;
    for i := 0 to Length(xi)-1 do
      Result := Result +
        xi[AIndex(i)]*yi[AIndex(i+1)] - xi[AIndex(i+1)]*yi[AIndex(i)];
    Result := Result*0.5;
  end;

var
  CutNodes, CutNodesTemp: TArrayOfTXSectionNode;
  CutNodesCount, CutNodesTempCount: Integer;
  xi, yi: array of Real;
  i, j: Integer;
  AFlag: Boolean;
begin
  Result := nil;
  if Count<3 then
    raise Exception.Create(rsTooFewPoints);
  if (Stage>Nodes[0].y) or (Stage>Nodes[Count-1].y) then
    raise Exception.Create(rsStageIsAboveTheSection);
  if Nodes[0].x>Nodes[Count-1].x then raise
    Exception.Create(rsFirstPointShouldHaveLessX);
  SetLength(CutNodes, 0);
  SetLength(CutNodesTemp, 0);
  try
    CutNodesCount := 0;
    CutNodesTempCount := 0;
    for i := 0 to Count-2 do
    begin
      if (Stage>=Min(Nodes[i].y, Nodes[i+1].y)) and
        (Stage<=Max(Nodes[i].y,Nodes[i+1].y)) then
      begin
        Inc(CutNodesTempCount);
        SetLength(CutNodesTemp, CutNodesTempCount);
        with CutNodesTemp[CutNodesTempCount-1] do
        begin
          x := InterpolateForY(Stage, Nodes[i].x, Nodes[i].y, Nodes[i+1].x,
            Nodes[i+1].y);
          y := Stage;
          PreIndex := i;
          PostIndex := i+1;
        end;
      end;
    end;
    for i := 0 to CutNodesTempCount-2 do
    begin
      AFlag := False;
      for j := CutNodesTemp[i].PostIndex to CutNodesTemp[i+1].PreIndex do
        if Nodes[j].y>Stage then
          AFlag := True;
      if not AFlag then
      begin
        Inc(CutNodesCount,2);
        SetLength(CutNodes, CutNodesCount);
        CutNodes[Length(CutNodes)-2] := CutNodesTemp[i];
        CutNodes[Length(CutNodes)-1] := CutNodesTemp[i+1];
      end;
    end;
    if IsOdd(cutNodesCount) then
      raise Exception.Create(rsXSectionSeemsNotToBeClosed);
    if cutNodesCount<1 then
      raise Exception.Create(rsWaterStageSeemsNotToBeCutSection);
    Area := 0;
    Perimeter := 0;
    SetLength(xi, 0);
    SetLength(yi, 0);
    i := 0;
    while i<=CutNodesCount -2 do
    begin
      try
        SetLength(xi, CutNodes[i+1].PostIndex-CutNodes[i].PreIndex+1);
        SetLength(yi, CutNodes[i+1].PostIndex-CutNodes[i].PreIndex+1);
        xi[0] := CutNodes[i].x;
        yi[0] := CutNodes[i].y;
        xi[Length(xi)-1] := cutNodes[i+1].x;
        yi[Length(yi)-1] := cutNodes[i+1].y;
        for j := CutNodes[i].PreIndex+1 to CutNodes[i+1].PostIndex-1 do
        begin
          xi[j-CutNodes[i].PreIndex] := Nodes[j].x;
          yi[j-CutNodes[i].PreIndex] := Nodes[j].y;
        end;
        Area := Area + CalcArea(xi, yi);
        Perimeter := Perimeter + CalcLength(xi, yi);
      finally
        SetLength(xi, 0);
        SetLength(yi, 0);
      end;
      Inc(i, 2);
    end;
    Area := Abs(Area);
  finally
    Assert(True);
  end;
  Result := CutNodes;
end;

resourcestring
  rsErrorInFindingSlice = 'Error while trying to evaluate slice';

procedure TXSection.CreateSliceSection(x1, x2, Stage: Real;
  ANewSection: TXSection);
var
  i, AStart, AEnd: Integer;
  y1, y2: Real;
begin
  Assert(x1<=x2);
  AStart := -1;
  AEnd := -1;
  y1 := 0;
  y2 := 0;
  for i := 0 to Count-2 do
    if (x1>=Min(Nodes[i].x, Nodes[i+1].x)) and
      (x1<=Max(Nodes[i].x,Nodes[i+1].x)) then
    begin
      AStart := i+1;
      y1 := InterpolateForX(x1, Nodes[i].x, Nodes[i].y,
        Nodes[i+1].x, Nodes[i+1].y);
      Break;
    end;
  for i := AStart-1 to Count-2 do
    if (x2>=Min(Nodes[i].x, Nodes[i+1].x)) and
      (x2<=Max(Nodes[i].x,Nodes[i+1].x)) then
    begin
      AEnd := i;
      y2 := InterpolateForX(x2, Nodes[i].x, Nodes[i].y,
        Nodes[i+1].x, Nodes[i+1].y);
      Break;
    end;
  if (AStart<0) or (AEnd<0) then raise Exception.Create(rsErrorInFindingSlice);
  ANewSection.Add(x1,Stage+0.001);
  ANewSection.Add(x1,y1);
  for i := AStart to AEnd do
    ANewSection.Add(Self.Nodes[i]);
  ANewSection.Add(x2, y2);
  ANewSection.Add(x2, Stage+0.001);
end;

function TXSection.CalcDepthAtPosition(XPosition: Real): Real;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Count-2 do
    if (XPosition>=Min(Nodes[i].x, Nodes[i+1].x)) and
      (XPosition<=Max(Nodes[i].x,Nodes[i+1].x)) then
    begin
      Result := InterpolateForX(XPosition,Nodes[i].x, Nodes[i].y,
        Nodes[i+1].x, Nodes[i+1].y);
    end;
end;

resourcestring
  rsInvalidHydrometricPointData = 'Invalid hydrometric points positions';

function TXSection.FindHydrometricRegions(CutNodes: TArrayOfTXSectionNode;
    HydrometricPositions: TArrayOfReal; IsMeasuredFromCut: Boolean):
    TArrayOfTSliceRegion;
var
  i, j, AStart, AEnd: Integer;
  AArray: array of Real;
  AOffset: Real;
begin
  Result := nil;
  AOffset := 0;
  if IsMeasuredFromCut then
    AOffset := CutNodes[0].x;
  i := 0;
  while i<=Length(CutNodes) -2 do
  begin
    AStart := Length(HydrometricPositions);
    AEnd := -1;
    for j := 0 to Length(HydrometricPositions)-1 do
      if (Min(CutNodes[i].x, cutNodes[i+1].x)<(HydrometricPositions[j]+AOffset)) and
        (Max(CutNodes[i].x, cutNodes[i+1].x)>(HydrometricPositions[j]+AOffset)) then
      begin
        AStart := Min(AStart, j);
        AEnd := Max(AEnd, j);
      end;
    if AStart>AEnd then
    begin
      Inc(i, 2);
      Continue;
    end;
    SetLength(AArray, 2+AEnd-AStart);
    AArray[0] := CutNodes[i].x;
    AArray[Length(AArray)-1] := CutNodes[i+1].x;
    for j := AStart To AEnd-1 do
      AArray[j-AStart+1] := 0.5*(HydrometricPositions[j]+
        HydrometricPositions[j+1])+AOffset;
    for j := 0 to Length(AArray)-2 do
    begin
      SetLength(Result, Length(Result)+1);
      Result[Length(Result)-1].x1 := AArray[j];
      Result[Length(Result)-1].x2 := AArray[j+1];
    end;
    Inc(i, 2);
  end;
end;

function TXSection.CalcHydraulicRadius(Stage: Real): Real;
var
  Area, Perimeter: Real;
begin
  CalcAreaAndPerimeter(Stage, Area, Perimeter);
  Result := HydraulicRadius(Area, Perimeter);
end;

function TXSection.CalcWaterVelocity(Stage, Roughness, Slope: Real): Real;
begin
  Result := CalcWaterVelocity(Stage, Roughness, Slope, ocmManning);
end;

function TXSection.CalcWaterVelocity(Stage, Roughness, Slope: Real;
  Model: TOpenChannelModel): Real;
var
  Area, Perimeter: Real;
begin
  CalcAreaAndPerimeter(Stage, Area, Perimeter);
  Result := 0; {dummy initialization to avoid compiler warnings}
  case Model of
    ocmManning:
      Result := ManningVelocity(Roughness, Slope,
        HydraulicRadius(Area, Perimeter));
    ocmChezy:
      Result := ChezyVelocity(Roughness, Slope,
        HydraulicRadius(Area, Perimeter));
  else
    Assert(False);
  end;
end;

function TXSection.CalcWaterDischarge(Stage, Roughness, Slope: Real): Real;
begin
  Result := CalcWaterDischarge(Stage, Roughness, Slope, ocmManning);
end;

function TXSection.CalcWaterDischarge(Stage, Roughness, Slope: Real;
  Model: TOpenChannelModel): Real;
var
  Area, Perimeter: Real;
begin
  CalcAreaAndPerimeter(Stage, Area, Perimeter);
  Result := 0; {dummy initialization to avoid compiler warnings}
  case Model of
    ocmManning:
      Result := ManningDischarge(Roughness, Slope,
        HydraulicRadius(Area, Perimeter), Area);
    ocmChezy:
      Result := ChezyDischarge(Roughness, Slope,
        HydraulicRadius(Area, Perimeter), Area);
  else
    Assert(False);
  end;
end;

procedure TXSection.LoadFromFile(FileName: string);
var
  F: TextFile;
  s: string;
  SavedDecimalSeparator, Delimiter: Char;
begin
  AssignFile(F, FileName);
  SavedDecimalSeparator := SysUtils.DecimalSeparator;
  try
    Reset(F);
    Clear;
    Delimiter := ',';
    SysUtils.DecimalSeparator := '.';
    while not eof(F) do
    begin
      Readln(F, s);
      Add(StrToFloat(DelimitedStringItem(s, 1, Delimiter)),
        StrToFloat(DelimitedStringItem(s, 2, Delimiter)));
    end;
  finally
    SysUtils.DecimalSeparator := SavedDecimalSeparator;
    CloseFile(F);
  end;
end;

procedure TXSection.WriteToFile(FileName: string);
var
  F: TextFile;
  i: Integer;
  SavedDecimalSeparator: Char;
begin
  SavedDecimalSeparator := SysUtils.DecimalSeparator;
  AssignFile(F, FileName);
  try
    Rewrite(F);
    SysUtils.DecimalSeparator := '.';
    for i := 0 to Count-1 do
      WriteLn(F, FloatToStr(Nodes[i].x),',',FloatToStr(Nodes[i].y) );
  finally
    SysUtils.DecimalSeparator := SavedDecimalSeparator;
    CloseFile(F);
  end;
end;

function TXSection.MinX: Real;
var i: Integer;
begin
  Result := 1e37;
  for i := 0 to Count-1 do
    Result := Min(Result, Nodes[i].x);
end;

function TXSection.MaxX: Real;
var i: Integer;
begin
  Result := -1e37;
  for i := 0 to Count-1 do
    Result := Max(Result, Nodes[i].x);
end;

function TXSection.MinY: Real;
var i: Integer;
begin
  Result := 1e37;
  for i := 0 to Count-1 do
    Result := Min(Result, Nodes[i].y);
end;

function TXSection.MaxY: Real;
var i: Integer;
begin
  Result := -1e37;
  for i := 0 to Count-1 do
    Result := Max(Result, Nodes[i].y);
end;

resourcestring
  rsPointsLessThanRequired =
    'Hydrometric points are less than the required count';
  rsTooFewPointsToFitLine =
    'Too few points to fit line';

procedure TXSection.FitOnHydrometricPoints(
  ATransientCurveList: TTransientCurveList;
    CurveNo, PointsCount: Integer; var Coefficient, Determination: Real);
begin
  FitOnHydrometricPoints(ATransientCurveList, CurveNo, PointsCount,
    Coefficient, Determination, ocmManning);
end;

procedure TXSection.FitOnHydrometricPoints(
  ATransientCurveList: TTransientCurveList;
    CurveNo, PointsCount: Integer; var Coefficient, Determination: Real;
      Model: TOpenChannelModel);
var
  StageList, DischargeList: TFloatList;

  procedure RemoveMinimum;
  var
    i, AIndex: Integer;
    AFloat: Real;
  begin
    AFloat := 10e37;
    AIndex := -1;
    for i := DischargeList.Count-1 downto 0 do
    begin
      if DischargeList.Items[i]<AFloat then
      begin
        AFloat := DischargeList.Items[i];
        AIndex := i;
      end;
    end;
    if AIndex>-1 then
    begin
      DischargeList.Delete(AIndex);
      StageList.Delete(AIndex);
    end;
  end;

var
  i: Integer;
  AR, SumXY, SumY, SumX2, SumY2, SumW2: Real;
begin
  if PointsCount<3 then
    raise Exception.Create(rsTooFewPointsToFitLine);
  StageList := nil;
  DischargeList := nil;
  try
    StageList := TFloatList.Create;
    DischargeList := TFloatList.Create;
    with ATransientCurveList do
    begin
      for i := 0 to HydrometricPointsCount-1 do
      begin
        if DiffInSecs(HydrometricPoints[i].Date,
          Items[CurveNo].StartDate)<0 then
          Continue;
        if DiffInSecs(HydrometricPoints[i].Date,
          Items[CurveNo].EndDate)>0 then
          Continue;
        StageList.Add(HydrometricPoints[i].Stage);
        DischargeList.Add(HydrometricPoints[i].Discharge);
      end;
    end;
    if StageList.Count<PointsCount then
      raise Exception.Create(rsPointsLessThanRequired);
    while StageList.Count>PointsCount do
      RemoveMinimum;
    Assert(StageList.Count=DischargeList.Count);
    Assert(PointsCount=StageList.Count);
    SumXY := 0;
    SumY := 0;
    SumX2 := 0;
    SumY2 := 0;
    SumW2 := 0;
    for i := 0 to PointsCount-1 do
    begin
      SumY := SumY + DischargeList.Items[i];
      SumY2 := SumY2 + Sqr(DischargeList.Items[i]);
      AR := CalcWaterDischarge(StageList.Items[i], 1, 1, Model);
      SumX2 := SumX2 + Sqr(AR);
      SumXY := SumXY + DischargeList.Items[i]*AR;
    end;
    Coefficient := SumXY/SumX2;
    for i := 0 to PointsCount-1 do
      SumW2 := SumW2 + Sqr(DischargeList.Items[i] -
        CalcWaterDischarge(StageList.Items[i], 1, Sqr(Coefficient), Model ) );
    Determination := 1 - SumW2 / (SumY2 - Sqr(SumY)/ PointsCount);
  finally
    StageList.Free;
    DischargeList.Free;
  end;
end;

function ManningVelocity(Roughness, Slope, Radius: Real): Real;
begin
  Result := (1/Roughness)*Power(Radius,2/3)*Sqrt(Slope);
end;

function ManningDischarge(Roughness, Slope, Radius, Area: Real): Real;
begin
  Result := Area*ManningVelocity(Roughness, Slope, Radius);
end;

function HydraulicRadius(Area, Perimeter: Real): Real;
begin
  Result := Area/Perimeter;
end;

function ChezyVelocity(Roughness, Slope, Radius: Real): Real;
begin
  Result := Roughness*Sqrt(Radius*Slope);
end;

function ChezyDischarge(Roughness, Slope, Radius, Area: Real): Real;
begin
  Result := Area*ChezyVelocity(Roughness, Slope, Radius);
end;

procedure LSLinearFit(Properties: THydrometricSliceProperties; var a, b,
  r: Real);
var
  i, n: Integer;
  x, y, SumX, SumY, SumXsq, SumYsq, SumXY: Real;
begin
  n := Length(Properties.Measures);
  SumX := 0;
  SumY := 0;
  SumXsq := 0;
  SumYsq := 0;
  SumXY := 0;
  x := 0;
  y := 0;
  for i := 0 to n-1 do
  begin
    case Properties.Mode of
      thymRandomPointsLogarithmic:
      begin
        x := Log10(1 - Properties.Measures[i].DepthRatio);
        y := Properties.Measures[i].Velocity;
      end;
      thymRandomPointsParabola:
      begin
        x := (1 - Properties.Measures[i].DepthRatio);
        y := Properties.Measures[i].Velocity/
          (1 - Properties.Measures[i].DepthRatio);
      end;
    else
      Assert(False);
    end;
    SumX := SumX + x;
    SumXsq := SumXsq + Sqr(x);
    SumY := SumY + y;
    SumYsq := SumYsq + Sqr(y);
    SumXY := SumXY + x*y;
  end;
  r := (n*SumXY-SumX*SumY)/Sqrt( (n*SumXsq-Sqr(SumX)) * (n*SumYsq-Sqr(SumY)) );
  b := r * Sqrt( (n*SumYsq-Sqr(SumY)) / (n*SumXsq-Sqr(SumX)) );
  a := (SumY-b*SumX)/n;
end;


function CalcSliceMeanVelocity(Properties: THydrometricSliceProperties;
  var Coef1, Coef2, DetFac: Real): Real;
begin
  Result := 0;
  case Properties.Mode of
    thymOnePointStandard:
    begin
      Assert(Length(Properties.Measures)=1);
      Assert(Abs(Properties.Measures[0].DepthRatio - 0.6)<0.001);
      Result := Properties.Measures[0].Velocity;
    end;
    thymTwoPointsStandard:
    begin
      Assert(Length(Properties.Measures)=2);
      Assert(Abs(Properties.Measures[0].DepthRatio - 0.2)<0.001);
      Assert(Abs(Properties.Measures[1].DepthRatio - 0.8)<0.001);
      Result := 0.5*(Properties.Measures[0].Velocity+
        Properties.Measures[1].Velocity);
    end;
    thymThreePointsStandard:
    begin
      Assert(Length(Properties.Measures)=3);
      Assert(Abs(Properties.Measures[0].DepthRatio - 0.2)<0.001);
      Assert(Abs(Properties.Measures[1].DepthRatio - 0.6)<0.001);
      Assert(Abs(Properties.Measures[2].DepthRatio - 0.8)<0.001);
      Result := 0.25*(Properties.Measures[0].Velocity+
        Properties.Measures[2].Velocity)+0.5*Properties.Measures[1].Velocity;
    end;
    thymRandomPointsLogarithmic:
    begin
      if Length(Properties.Measures)<3 then
        raise Exception.Create(rsTooFewPointsToFitLine);
      LSLinearFit(Properties, Coef1, Coef2, DetFac);
      Result := Coef1-Coef2*Log10(Exp(1));
    end;
    thymRandomPointsParabola:
    begin
      if Length(Properties.Measures)<3 then
        raise Exception.Create(rsTooFewPointsToFitLine);
      LSLinearFit(Properties, Coef1, Coef2, DetFac);
      Result := 0.5*Coef1+Coef2/3;
    end;
  else
    Assert(False);
  end;
end;

function CalcTotalSlicesMeanVelocity(SliceValues: TSlicesMasterArray): Real;
var
  i: Integer;
  TotalArea: Real;
begin
  TotalArea := 0;
  Result := 0;
  for i := 0 to Length(SliceValues)-1 do
    with SliceValues[i] do
    begin
      TotalArea := TotalArea + Area;
      Result := Result + Area * MeanVelocity;
    end;
  Result := Result / TotalArea;
end;

resourcestring
  rsErrorInSlicesPositions = 'Error in slices positions, maybe outside wet '+
    'section.';
  rsErrorMeasurementDepth = 'Error in measurement depth definition. '+
    'Measure bellow bottom or above water surface.';

function CalcHydrometricMeanVelocityFromSection(Stage: Real;ASection: TXSection;
  IsMeasuredFromCut: Boolean; var Slices: TSlicesMasterArray): Real;
var
  i, j: Integer;
  MeasurePoints: TArrayOfReal;
  AnotherSection: TXSection;
  CutNodes: TArrayOfTXSectionNode;
  Area, Perimeter: Real;
  SliceRegions: TArrayOfTSliceRegion;
  x: Real;
begin
  SliceRegions := nil;
  SetLength(MeasurePoints, Length(Slices));
  for i := 0 to Length(Slices)-1 do
    MeasurePoints[i] := Slices[i].Properties.XPosition;
  CutNodes := ASection.CalcAreaAndPerimeter(Stage, Area, Perimeter);
  SliceRegions :=
    ASection.FindHydrometricRegions(CutNodes, MeasurePoints, IsMeasuredFromCut);
  AnotherSection := nil;
  if Length(SliceRegions)<>Length(Slices) then
    raise Exception.Create(rsErrorInSlicesPositions);
  for i := 0 to Length(Slices)-1 do
  begin
    try
      AnotherSection := TXSection.Create;
      ASection.CreateSliceSection(SliceRegions[i].x1, SliceRegions[i].x2,
        Stage, AnotherSection);
      AnotherSection.CalcAreaAndPerimeter(Stage, Area, Perimeter);
      Slices[i].Area := Area;
      Slices[i].x1 := SliceRegions[i].x1;
      Slices[i].x2 := SliceRegions[i].x2;
      x := Slices[i].Properties.XPosition;
      if IsMeasuredFromCut then x := x + CutNodes[0].x;
      Slices[i].Properties.Depth := Stage -
        AnotherSection.CalcDepthAtPosition(x);
      for j := 0 to Length(Slices[i].Properties.Measures)-1 do
      begin
        if Slices[i].Properties.IsMeasuredActualDepth then
          Slices[i].Properties.Measures[j].DepthRatio :=
            Slices[i].Properties.Measures[j].Depth /
            Slices[i].Properties.Depth
        else
          Slices[i].Properties.Measures[j].Depth :=
            Slices[i].Properties.Measures[j].DepthRatio *
            Slices[i].Properties.Depth;
        if (Slices[i].Properties.Measures[j].DepthRatio<0) or
          (Slices[i].Properties.Measures[j].DepthRatio>1) then
          raise Exception.Create(rsErrorMeasurementDepth);
      end;
      with Slices[i] do
        MeanVelocity :=
          CalcSliceMeanVelocity(Properties, coef1, coef2, detfac);
      SetLength(Slices[i].Sketch, AnotherSection.Count);
      for j := 0 to AnotherSection.Count-1 do
        Slices[i].Sketch[j] := AnotherSection.Nodes[j];
    finally
      AnotherSection.Free;
    end;
    AnotherSection := nil;
  end;
  Result := CalcTotalSlicesMeanVelocity(Slices);
end;

end.
