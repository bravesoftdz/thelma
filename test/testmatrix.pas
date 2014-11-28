unit testmatrix;

interface

uses TestFramework;

type

  TTestMatrix = class(TTestCase)
  published
    procedure TestInvertMatrix;
    procedure TestDeterminant;
    procedure TestCholeskyDecompose;
  end;

implementation

uses SysUtils, Matrix;

type TTestArray = array[1..4,1..4] of Real;

var

  UMatrixData: TTestArray = (
    (0.32, 0.56, 0.56, 0.01),
    (0.00, 0.23, 0.76, 0.81),
    (0.00, 0.00, 0.31, 0.44),
    (0.00, 0.00, 0.00, 0.31)
  );

  UInvertedMatrixData: TTestArray = (
    (3.125000, -7.608696,  13.008415,  1.316422),
    (0.000000,  4.347826, -10.659187,  3.768719),
    (0.000000,  0.000000,   3.225806, -4.578564),
    (0.000000,  0.000000,   0.000000,  3.225806)
  );

  LMatrixData: TTestArray = (
    (0.32, 0.00, 0.00, 0.00),
    (0.98, 0.23, 0.00, 0.00),
    (0.31, 0.63, 0.31, 0.00),
    (0.66, 0.02, 0.57, 0.31)
  );

  LInvertedMatrixData: TTestArray = (
    (  3.125000,  0.000000,   0.000000,  0.000000),
    (-13.315217,  4.347826,   0.000000,  0.000000),
    ( 23.934958, -8.835905,   3.225806,  0.000000),
    (-49.803618, 15.966158,  -5.931322,  3.225806)
  );

  AMatrixData: TTestArray = (
    (0.32, 0.56, 0.56, 0.01),
    (0.98, 0.23, 0.76, 0.81),
    (0.31, 0.63, 0.31, 0.44),
    (0.66, 0.02, 0.57, 0.31)
  );

  AInvertedMatrixData: TTestArray = (
    (-11.74867, -20.30242,  16.92162,  29.40946),
    ( -2.87276,  -6.38974,   6.22098,   7.95867),
    ( 11.29281,  17.79621, -15.75734, -24.49881),
    (  4.43444,  10.91467,  -7.45486, -14.85515)
  );

  AMatrixDeterminant: Real = -0.00923747;

{ Also test a matrix whose lu decomposition will result in an odd number of
  permutations (this has an effect on the determinant).
}

  BMatrixData: array[1..3,1..3] of Real = (
    (0.79, 0.77, 0.98),
    (0.92, 0.02, 0.39),
    (0.21, 0.14, 0.27)
  );

  BMatrixDeterminant: Real = -0.044965;

  { Symmetric positive-definite matrix to test Cholesky decomposition. }
  CMatrixData: TTestArray = (
    (1.02,  3.14, 0.99, 0.50),
    (3.14, 10.13, 4.49, 2.00),
    (0.99,  4.49, 9.89, 3.50),
    (0.50,  2.00, 3.50, 9.50)
  );

  CCholeskyDecomposedMatrixData: TTestArray = (
    (1.009950, 0.000000, 0.000000, 0.000000),
    (3.109063, 0.680974, 0.000000, 0.000000),
    (0.980246, 2.118074, 2.107814, 0.000000),
    (0.495074, 0.676655, 0.750304, 2.869509)
  );

function MatrixesEqual(Matrix1: TMatrix; Matrix2: TTestArray; Tolerance: Real):
  Boolean;
var i, j: Integer;
begin
   Result := True;
   for i := 1 to Matrix1.RowCount do
      for j := 1 to Matrix1.ColCount do
        if Abs(Matrix1.E[i,j]-Matrix2[i,j]) > Tolerance then
        begin
          Result := False;
          Exit;
        end;
end;

procedure TTestMatrix.TestInvertMatrix;

  procedure TestInvertOneMatrix(TestedMatrix, ReferenceMatrix: TTestArray;
                                    Tolerance: Real; Description: string);
  var
    AMatrix: TMatrix;
  begin
    AMatrix := TMatrix.CreateFromArray(Length(TestedMatrix),
      Length(TestedMatrix[1]), @TestedMatrix, maCopyArray);
    try
      AMatrix.Invert;
      Check(MatrixesEqual(AMatrix, ReferenceMatrix, Tolerance));
    finally
      AMatrix.Free();
    end;
  end;

begin
  TestInvertOneMatrix(UMatrixData, UInvertedMatrixData, 0.000002,
                                   'inverting upper triangular matrix');
  TestInvertOneMatrix(LMatrixData, LInvertedMatrixData, 0.000002,
                                   'inverting lower triangular matrix');
  TestInvertOneMatrix(AMatrixData, AInvertedMatrixData, 0.00002,
                                   'inverting uninteresting matrix');
end;

procedure TTestMatrix.TestDeterminant;
var
  AMatrix: TMatrix;
  d: Real;
begin
  AMatrix := TMatrix.CreateFromArray(4, 4, @AMatrixData, maCopyArray);
  try
    d := AMatrix.Determinant;
    Check(Abs(d - AMatrixDeterminant) <= 0.00000002);
  finally
    AMatrix.Free();
  end;
  AMatrix := TMatrix.CreateFromArray(3, 3, @BMatrixData, maCopyArray);
  try
    d := AMatrix.Determinant;
    Check(Abs(d - BMatrixDeterminant) <= 0.0000002);
  finally
    AMatrix.Free();
  end;
end;

procedure TTestMatrix.TestCholeskyDecompose;
var AMatrix: TMatrix;
begin
  AMatrix := TMatrix.CreateFromArray(4, 4, @CMatrixData, maCopyArray);
  try
    AMatrix.CholeskyDecompose;
    Check(MatrixesEqual(AMatrix, CCholeskyDecomposedMatrixData, 0.000002));
  finally
    AMatrix.Free();
  end;
end;

initialization
  RegisterTest(TTestMatrix.Suite);

end.
