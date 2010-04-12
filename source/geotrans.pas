{******************************************************************}
{                                                                  }
{  Thelma library                                                  }
{                                                                  }
{  Copyright (c) 2000-06 National Technical University of Athens   }
{                                                                  }
{******************************************************************

{** Geographical transformations.
}
unit geotrans;

interface

uses Classes, Matrix;

type
{** A point described by real-number coordinates x, y.
}
  TRealPoint = record
    X, Y: Real;
  end;

type
{** Affined Transformation, used in maps - cartography.
    Affined transformation includes:<p>
      * A 2D displacement<p>
      * Anisotropic scale kx<>ky<p>
      * Rotation<p>
      * Trapezoidal Skew<p>
    So, it transforms a square of the Geographical cartesian (X,Y) space
    to an arbitrary trapezoid on the map (screen) (x,y) space.<p>
    x = a1 + a2 X + a3 Y + a4 X Y <p>
    y = b1 + b2 X + b3 Y + b4 X Y <p>
    a1-4, b1-4 8 coefficients calculated by 4 points defined in both
    coordinate systems.
    @author Stefanos
}
  TAffinedTransformer = class(TPersistent)
  private
    AFactors, BFactors: TVector;
    XYCoef: TVector;
  protected

  public
    {** Creates a TAffinedTransformer object.
    }
    constructor Create;
    {** Frees associated memory.
    }
    destructor Destroy; override;
    {** Set four calibration points into both coordinate systems.
        GeoPoints and ScreenPoints should be arrays [1..4] of TReal points.
        This function evaluates the a and b coefficients.
    }
    procedure SetCalibration(const GeoPoints,
      ScreenPoints: array of TRealPoint);
    {** Straight transform from (X, Y) -> (x, y) (geographic coords to screen coords.)
    }
    function Transform(APoint: TRealPoint): TRealPoint;
    {** Inverted transform from (x, y) -> (X, Y) (screen coords to geographic coords).
        Evaluation is based on solving the non linear equations of the affined
        transform by an iterrative process.
    }
    function InvTransform(APoint: TRealPoint): TRealPoint;
    {** Copies a TAffinedTransformer object.
    }
    procedure Assign(Source: TPersistent); override;
  end;

implementation

constructor TAffinedTransformer.Create;
begin
  inherited Create;
  AFactors := TVector.Create(4);
  BFactors := TVector.Create(4);
  XYCoef := TVector.Create(4);
  AFactors.Zero;
  BFactors.Zero;
  XYCoef.Zero;    
end;

destructor TAffinedTransformer.Destroy;
begin
  AFactors.Free;
  BFactors.Free;
  XYCoef.Free;
  inherited Destroy;
end;

function TAffinedTransformer.Transform(APoint: TRealPoint): TRealPoint;
var
  i: Integer;
begin
  Result.X := 0;
  Result.Y := 0;
  XYCoef[1] := 1;
  XYCoef[2] := APoint.X;
  XYCoef[3] := APoint.Y;
  XYCoef[4] := APoint.X * APoint.Y;
  for i := 1 to 4 do
  begin
    Result.X := Result.X + AFactors[i]* XYCoef[i];
    Result.Y := Result.Y + BFactors[i]* XYCoef[i];
  end;
end;

function TAffinedTransformer.InvTransform(APoint: TRealPoint): TRealPoint;
var
  i: Integer;
  dx, dy: Real;
  Det: Real;
  AproxPoint: TRealPoint;
begin
  Result.X := 0;
  Result.Y := 0;
  for i := 1 to 6 do {6 iterations should be sufficient}
  begin
    AproxPoint := Transform(Result);
    Det := (AFactors[2]+AFactors[4]*Result.Y) * (BFactors[3]+BFactors[4]*Result.X)-
      (AFactors[3]+AFactors[4]*Result.X) * (BFactors[2]+BFactors[4]*Result.Y);
    dx := (BFactors[3]+BFactors[4]*Result.X) * (APoint.X - AproxPoint.X)-
      (AFactors[3]+AFactors[4]*Result.X) * (APoint.Y - AproxPoint.Y);
    dx := dx / Det;
    dy := (AFactors[2]+AFactors[4]*Result.Y) * (APoint.Y - AproxPoint.Y)-
      (BFactors[2]+BFactors[4]*Result.Y) * (APoint.X - AproxPoint.X);
    dy := dy / Det;
    Result.X := Result.X + dx;
    Result.Y := Result.Y + dy;
    {Stop iterative process when achieve the desired precision}
    if (dx*dx+ dy*dy) <= 0.000001 then
      Break;
  end;
end;

procedure TAffinedTransformer.SetCalibration(const GeoPoints, ScreenPoints:
  array of TRealPoint);
var
  i: Integer;
  N: TMatrix;
  xvect, yvect: TVector;
begin
  Assert((Low(GeoPoints)=0) and (Low(ScreenPoints)=0) and
    (High(GeoPoints)=3) and (High(ScreenPoints)=3));
  N := nil;
  xvect := nil;
  yvect := nil;
  try
    N := TMatrix.Create(4,4);
    xvect := TVector.Create(4);
    yvect := TVector.Create(4);
    xvect.Zero;
    yvect.Zero;
    N.Zero;    
    for i := 1 to 4 do
    begin
      N[i, 1] := 1;
      N[i, 2] := GeoPoints[i-1].X;
      N[i, 3] := GeoPoints[i-1].Y;
      N[i, 4] := GeoPoints[i-1].X * GeoPoints[i-1].Y;
      xvect[i] := ScreenPoints[i-1].X;
      yvect[i] := ScreenPoints[i-1].Y;
    end;
    N.Invert;
    AFactors.Assign(xvect);
    AFactors.Multiply(N, maLeft);
    BFactors.Assign(yvect);
    BFactors.Multiply(N, maLeft);
  finally
    N.Free;
    xvect.Free;
    yvect.Free;
  end;
end;

procedure TAffinedTransformer.Assign(Source: TPersistent);
begin
  with Source as TAffinedTransformer do
  begin
    Self.AFactors.Assign(AFactors);
    Self.BFactors.Assign(BFactors);
  end;
end;

end.
 