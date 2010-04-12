{******************************************************************}
{                                                                  }
{  Thelma library                                                  }
{                                                                  }
{  Copyright (c) 2006   National Technical University of Athens    }
{                                                                  }
{******************************************************************}

unit VagMatrix;

interface



uses
  {SysUtils, Classes, math, Dialogs;}
  SysUtils, Classes, math ;

function Gauss(const AA: array of Double; const B:array of Double;
                 var Heads:array of Double; n: Integer): Integer;
{ Solve linear systems by GGauss elimination method.
  Arguments are:
  1. the coefficients (**)
  2. the constant terms (*)
  3. the uknowns
  4. number of equations

  (*) :   (1,2,3..,n)
  (**):   (11,12,13..,1n,
           21,22,23..,2n,
              ..
           n1,n2,n3..,nn)
}

Implementation


(*******************************************************************************
                                   Gauss elimination
*******************************************************************************)

function Gauss(const AA: array of Double; const B:array of Double;
                 var Heads:array of Double; n: Integer): Integer;
  var
    LIn: array of array of Double;
    X: array of Double;
    temp, mult, epsil: Double;
    k, i, j: Integer;
    abspiv: Double;
    pivrow: Integer;
  Begin

  { Initilize }
  epsil:=1e-15;

  { Allocate memory }
  SetLength(LIn, (n+1), (n+2));
  SetLength(X, n+1);

  { Copy AA to LIn }
  for i:=1 to n do begin
    for j:=1 to n do LIn[i,j]:=AA[(i-1)*n+(j-1)];
    LIn[i,n+1]:=B[i-1];
  end;

  for i:=1 to n do begin

    { Locate pivot element }
    abspiv:= abs(LIn[i,i]);
    pivrow:= i;
    for k:= i+1 to n do begin
      if abs(LIn[k,i])>abspiv then begin
        abspiv:= abs(LIn[k,i]);
        pivrow:=k;
      end;
    end;

    { Check if matrix is (nearly) singular }
    if abspiv<epsil then begin
      SetLength(LIn, 0, 0);
      SetLength(X,0);
      Gauss:=1;
      Exit
    end;

    { It isn't, so interchange rows PIVROW and I if necessary }
    if (pivrow<>i) then begin
      for j:=1 to n+1 do begin
        temp:= LIn[i,j];
        LIn[i,j]:= LIn[pivrow, j];
        LIn[pivrow,j]:= temp;
      end;
    end;

    { Eliminate Ith unknown from equations I + 1, ..., N }
    for j:=i +1 to n do begin
      mult:= -LIn[j,i] / LIn[i,i];
      for k:=i to n+1 do LIn[j,k]:= LIn[j,k] + mult*LIn[i,k];
    end;

  end;

  { Find the solutions by back substitution }
  X[n]:= LIn[n, n+1] / LIn[n,n];
  for j:= n-1 downto 1 do begin
    X[j]:= LIn[j, n+1];
    for k:=j+1 to n do X[j]:= X[j] - LIn[j,k]*X[k];
    X[j]:= X[j] / LIn[j,j];
  end;

  { X[1..n] -> Head[0..n-1] }
  for i:=0 to n-1 do Heads[i]:=X[i+1];

  { Free memory }
  SetLength(LIn, 0, 0);
  SetLength(X,0);

  Gauss:=0;

End;

END.


