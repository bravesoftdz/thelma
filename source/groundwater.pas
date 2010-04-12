unit GroundWater;
{
This is the unit with essential functions that calculate groundwater movement
Copyright (C) 2006  Evangelos Rozos

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.
}
interface

uses
   SysUtils, Classes, math, VagMatrix, SparSolv; 


procedure xlGrdFlow(var xlTblCond: Double; var xlTblPor : Double;
                    var xlTblArea: Double; var xlTblTop:  Double;
                    var xlTblBot: Double; var xlTblType: LongInt;
                    var xlTblDist: Double; var xlTblEdge: Double;
                    var xlTblStrs: Double; var xlTblWatLev: Double;
                    var xlTblDisch: Double;
                    lamda: Double; 
                    cnfunc, arhar: Integer; fct1, fct2, fct3: Double;
                    dt: Double; ndt, cellsNm, eqType, slvType: LongInt;
                    var Returned: Double);
stdcall; export;
{ It is a wraper.
  Arguments are:
  1. vector with conductivities of cells (*)
  2. vector with porosities of cells (*)
  3. vector with the area of cells (*)
  4. vector with Top of cells (*)
  5. vector with Bottom of cells (*)
  6. vector with type of cell (*)
  7. vector with distance between centers of cells (**)
  8. vector with common edges of cells (**)
  9. vector with reservoir stress (*)
  10. vector with water level in reservoirs (*)
  11. vector with discharge between reservoirs (**)
  12. lamda coefficient
  13. cnfunc (1=constant Trans , 2=variable Trans )
  14. arhar (1=arithmetic mean, 2=harmonic mean, 3=geometr. mean, 4=log. mean )
  15. fct1 (J in MixEq)
  16. fct2 (alpha in MixEq)
  17. fct3 (D in MixEq)
  18. time step
  19. number of time steps
  20. number of cells
  21. flow Equation type (1=Darcy, 2=Mixed Equation)
  22. solver type (1=Explicit, 2=Implicit)
  23. returned maximum dw


  (*) :   (1,2,3..,n)
  (**):   (11,12,13..,1n,
           21,22,23..,2n,
              ..
           n1,n2,n3..,nn)

  where n=cellsNm
}


function GrdFlow(const CellCond:array of Double; const CellPor:array of Double;
                 const CellArea:array of Double; const CellTop:array of Double;
                 const CellBot:array of Double; const CellType:array of Integer;
                 var DIst: array of Double; var EDge: array of Double;
                 const Strs: array of Double;
                 var WatLev: array of Double; var DIsch: array of Double;
                 lamda: Double;
                 cnfunc, arhar: Integer; fct1, fct2, fct3: Double;
                 dt: Double; ndt, cellsNm, eqType, slvType: Integer): Double;

{ The main ground water function.  Returns maximum dw
  Arguments are:
  1. vector with conductivities of cells (*)
  2. vector with porosities of cells (*)
  3. vector with the area of cells (*)
  4. vector with Top of cells (*)
  5. vector with Bottom of cells (*)
  6. vector with type of cell (*)
  7. vector with distance between centers of cells (**)
  8. vector with common edges of cells (**)
  9. vector with reservoir stress (*)
  10. vector with water level in reservoirs (*)
  11. vector with discharge between reservoirs (**)
  12. lamda coefficient
  13. cnfunc (1=constant Trans , 2=variable Trans )
  14. arhar (1=arithmetic mean, 2=harmonic mean, 3=geometr. mean, 4=log. mean )
  15. fct (J in MixEq)
  16. fct2 (alpha in MixEq)
  17. fct3 (D in MixEq)
  18. time step
  19. number of time steps
  20. number of cells
  21. flow Equation type (1=Darcy, 2=Mixed Equation)
  22. solver type (1=Explicit, 2=Implicit)


  (*) :   (1,2,3..,n)
  (**):   (11,12,13..,1n,
           21,22,23..,2n,
              ..
           n1,n2,n3..,nn)

  where n=cellsNm
}


function ExpSolv( const CellTop:array of Double; const CellBot:array of Double;
                  const CellType: array of Integer;
                  const CellCond: array of Double;
                  const TR: array of Double; const DIst: array of Double;
                  const Area: array of Double;
                  var WatLev: array of Double; var DIsch: array of Double;
                  lamda: Double; 
                  cnfunc, arhar: Integer;
                  fct1, fct2, fct3: Double; dt: Double;
                  cellsNm, eqType: Integer): Double;
{ It calculates the water exchange between cells fo a time step
  Arguments are:
  1. vector with aquifer cells top (*)
  2. vector with aquifer cells bottom (*)
  3. vector with aquifer cells type (*)
  4. vector with conductivities (*)
  5. vector with transmissivities (**)
  6. vector with distance between centers of cells (**)
  7. vector with area of bases of reservoirs (*)
  8. vector with water level in reservoirs   (*)
  9. vector with discharge between reservoirs (**)
  10. lamda coefficient
  13. cnfunc (1=constant Trans, 2=variable Trans  )
  14. arhar (1=arithmetic mean, 2=harmonic mean, 3=geometr. mean, 4=log. mean )
  15. fct1 (J in MixEq)
  16. fct2 (alpha in MixEq)
  17. fct3 (D in MixEq)
  13. time step
  14. number of cells
  15. Flow equation type


  (*) :   (1,2,3..,n)
  (**):   (11,12,13..,1n,
           21,22,23..,2n,
              ..
           n1,n2,n3..,nn)

  where n=cellsNm
}


function ImpSolv( const CellTop:array of Double; const CellBot:array of Double;
                  const CellType: array of Integer;
                  const CellCond: array of Double;
                  const TR: array of Double; const DIst: array of Double;
                  const Area: array of Double;
                  var WatLev: array of Double; var DIsch: array of Double;
                  lamda: Double;
                  cnfunc, arhar: Integer;
                  fct1, fct2, fct3: Double; dt: Double;
                  cellsNm, eqType: Integer): Double;
{ It calculates the water exchange between cells fo a time step using implicit
  scheme. The linear system is:


    SIGMA  Coeffs(i,j) hj  = hi(t-1) Fi/Dt
    j=1..n

                       |Cij, i<>j
    where Coeffs(i,j)= |
                       |-(SIGMA Cij - Fi/Dt - Cii)hi, i=j
                          j=1..n

  Arguments are: (see ExpSolv)

  The system is soleved with Gauss's elimination method.
}

function SparSolv( const CellTop:array of Double; const CellBot:array of Double;
                  const CellType: array of Integer;
                  const CellCond: array of Double;
                  const TR: array of Double; const DIst: array of Double;
                  const Area: array of Double;
                  var WatLev: array of Double; var DIsch: array of Double;
                  lamda: Double;
                  cnfunc, arhar: Integer;
                  fct1, fct2, fct3: Double; dt: Double;
                  cellsNm, eqType: Integer): Double;
{ It calculates the water exchange between cells fo a time step using implicit
  scheme. The linear system is:


    SIGMA  Coeffs(i,j) hj  = hi(t-1) Fi/Dt
    j=1..n

                       |Cij, i<>j
    where Coeffs(i,j)= |
                       |-(SIGMA Cij - Fi/Dt - Cii)hi, i=j
                          j=1..n

  Arguments are: (see ExpSolv)

  The system is solved with algorithms appropriate for sparse matrices.
}


procedure Coeffs( const TR: array of Double; const DIst: array of Double;
                  const RsrvBase: array of Double; var AA: array of Double;
                  dt:Double; n:Integer);
{ It calculates the Coeffs (see ImpSolv)
  Arguments are:
  1. transmissivities between reservoirs (**)
  2. distance between reservoirs (**)
  3. reservoir bases (*)
  4. Calculated Array of coefficients (**)
  5. Time step
  6. Number of cells

  (*) :   (1,2,3..,n)
  (**):   (11,12,13..,1n,
           21,22,23..,2n,
              ..
           n1,n2,n3..,nn)

  where n=cellsNm
}


function Symetrize(var SYmetr: array of Double; n: Integer): Boolean;
{ It copies non zero values to its symetric place. If matrix is not symetric
  it returns false.
  Arguments are:
  1. Matrix to make symetric
  2. n where nxn is the dimension of matrix
}


function WlHd( top, bot, watlev, lamda : Double): Double;
{ It returns the water level.
  Arguments are:
  1. reservoir top
  2. reservoir bottom
  3. reservoir water level
  4. lamda coefficient
}


function HdWl( top, bot, head, lamda : Double): Double;
{ It returns the hydraulic head.
  Arguments are:
  1. reservoir top
  2. reservoir bottom
  3. head
  4. lamda coefficient
}


procedure  TranConf( const CellCond: array of Double;
                    const CellType: array of Integer;
                    const CellTop: array of Double;
                    const CellBot : array of Double;
                    const CellArea: array of Double;
                    const EDge: array of Double; var TRan: array of Double;
                    arhar: Integer; cellsNm: Integer);
{ It calculates the transmisivity between reservoirs for Darcy equation
  and confined conditions.
  Arguments are:
  1. vector with conductivities of cells (*)
  2. vector with cell type (*)
  3. vector with aquifer top of cells (*)
  4. vector with aquifer bottom of cells (*)
  5. vector with aquifer cells areas (*)
  6. vector with common edges of cells (**)
  7. returned vector with transmisivities (**)
  8. mean procedure (1=arithmetic mean, 2=harmonic mean)
  9. Number of cells


  (*) :   (1,2,3..,n)
  (**):   (11,12,13..,1n,
           21,22,23..,2n,
              ..
           n1,n2,n3..,nn)

  where n=cellsNm
}


procedure TranUnconf( const  CellCond: array of Double;
                    const CellType: array of Integer;
                    const CellTop: array of Double;
                    const CellBot : array of Double;
                    const WatLev : array of Double;
                    const EDge: array of Double; var TRan: array of Double;
                    arhar: Integer; cellsNm: Integer);
{ It calculates the transmisivity between reservoirs for Darcy equation
  unconfined conditions.
  Arguments are:
  1. vector with conductivities of cells (*)
  2. vector with cell type (*)
  3. vector with aquifer top of cells (*)
  4. vector with aquifer bottom of cells (*)
  5. vector with water levels of cells (*)
  6. vector with common edges of cells (**)
  7. returned vector with transmisivities (**)
  8. mean procedure (1=arithmetic mean, 2=harmonic mean)
  9. Number of cells


  (*) :   (1,2,3..,n)
  (**):   (11,12,13..,1n,
           21,22,23..,2n,
              ..
           n1,n2,n3..,nn)

  where n=cellsNm
}


function Darcy(tr, h1, h2, l : Double): Double;
{ It returns the discharge according to Darcy
  Arguments are:
  1. transmissivity
  2. head in reservoir 1
  3. head in reservoir 2 (h1>h2)
  4. distance
}


function Mixed(c, h1, h2, l, J, alpha, D: Double): Double;
{ It returns the discharge according to open channel pressurize flow
  mixed equation.
  Arguments are:
  1. generalised conductivity
  2. head in reservoir close to spring
  3. head in spring
  4. distance
  5. slope
}


function VertTouch(topi,boti,topj,botj: Double): Double;
{ It returns the common vertical edge.
  Arguments are:
  1. top of aquifer cell i
  2. bottom of aquifer cell i
  3. top of aquifer cell j
  4. bottom of aquifer cell j
}


function TestRealEq(a, b: Double): Boolean;
{ It test if a=b with tolerance.
  Arguments are:
  1. real a
  2. real b
}


function xlThiem( var xlWellxyq: Double; nmwells: Integer;
                  x, y,  hoo, roo, tran: Double): Double;
stdcall; export;
{ Calculate the drawdown at a point of the aquifer caused by pumps
  Arguments are:
  1. vector with wells x,y,Q (x1,y1,Q1, x2,y2,Q2,...)
  2. number of wells
  3. x of the point
  4. y of the point
  5. head at the infinity
  6. distance of infinity
  7. transmissivity
}


function Thiem( var Wellx: array of Double; var Welly: array of Double;
                var Wellq: array of Double;
                nmwells: Integer; x, y,  hoo, roo, tran: Double): Double;
{ Calculate the drawdown at a point of the aquifer caused by pumps
  Arguments are:
  1. vector with wells x
  2. vector with wells y
  3. vector with wells Q
  4. number of wells
  5. x of the point
  6. y of the point
  7. head at the infinity
  8. distance of infinity
  9. transmissivity
}


implementation

{*******************************************************************************
                         Test equality of real numbers
*******************************************************************************}
function TestRealEq(a, b: Double): Boolean;
Begin
  if abs(a-b)<1e-12 then TestRealEq:=True
  else TestRealEq:=False;
End;


{*******************************************************************************
                        Water Level -> Hydraulic Head 
                 (this implementation does not cover > 1 layer) 
*******************************************************************************}
function WlHd( top, bot, watlev, lamda : Double): Double;
var
  absw: Double;
Begin
  absw:=watlev+bot;
  If (absw>top) then WlHd:=(absw-top)*lamda + top
  else if (watlev>0) then WlHd:=absw
  else WlHd:=bot;
End;


{*******************************************************************************
                        Hydraulic Head -> Water Level 
                 (this implementation does not cover > 1 layer)
*******************************************************************************}
function HdWl( top, bot, head, lamda : Double): Double;
Begin
  if (head>top) then HdWl:=(head-top)/lamda + top -bot
  else if(head>bot) then HdWl:=head-bot
  else HdWl:=0;
End;


{*******************************************************************************
          Conductivity to transmisivity Confined aquifer - Darcy equation
               (this implementation does not cover > 1 layer)
*******************************************************************************}
procedure TranConf( const CellCond: array of Double;
                    const CellType: array of Integer;
                    const CellTop: array of Double;
                    const CellBot: array of Double;
                    const CellArea: array of Double;
                    const EDge: array of Double; var TRan: array of Double;
                    arhar: Integer; cellsNm: Integer);
var
  i,j,ij: Integer;
  dzij: Double;
  ratio: Double;
Begin
  for i:=0 to cellsNm -1 do begin
    for j:=0 to cellsNm -1 do begin
      { (i,j) -> ij }
      ij:= i*cellsNm + j;
      { Common vertical }
      dzij:= VertTouch(CellTop[i],CellBot[i],CellTop[j],CellBot[j]);
      { Ratio of conductivities }
      if (CellCond[j]<>0) then ratio:= CellCond[i]/CellCond[j]
      else ratio:=1;
      { Caclulate transmisivity (1=arithmetic mean, 2=harmonic, 3=geo, 4=log) }
      if (arhar=1) or ((ratio<1.005) and (ratio>0.995)) then TRan[ij]:=
	 (CellCond[i]+CellCond[j])/2*EDge[ij]*dzij
	 {((CellArea[i])*CellCond[i]+(CellArea[j])*CellCond[j])
	 /((CellArea[i])+(CellArea[j]))*EDge[ij]*dzij}
        {(CellCond[i]+CellCond[j])*EDge[ij]*dzij/2}
      else if (arhar=2) then TRan[ij]:=
        (CellCond[i]*CellCond[j])*((CellArea[i])+(CellArea[j]))/
        ((CellArea[j])*CellCond[i]+(CellArea[i])*CellCond[j])*
        EDge[ij]*dzij
      else if (arhar=3) then TRan[ij]:=
	 Sqrt(((CellArea[i])*CellCond[i]+(CellArea[j])*CellCond[j])
	 /((CellArea[i])+(CellArea[j]))*
        (CellCond[i]*CellCond[j])*((CellArea[i])+(CellArea[j]))/
        ((CellArea[j])*CellCond[i]+(CellArea[i])*CellCond[j]))*
        EDge[ij]*dzij
        {sqrt(CellCond[i])*sqrt(CellCond[j])*EDge[ij]*dzij}
      else if (arhar=4) then TRan[ij]:=
        (CellCond[i]-CellCond[j])/Ln(ratio)*EDge[ij]*dzij
      else begin 
        raise Exception.Create('arhar should be an integer between 1 and 4!'); 
        Exit; 
      end;

      { If spring then spring conductivity dominates }
      if (CellType[i]=-1) then TRan[ij]:=CellCond[i]*EDge[ij]*dzij;
      if (CellType[j]=-1) then TRan[ij]:=CellCond[j]*EDge[ij]*dzij;

    end;
  end;
End;


{*******************************************************************************
        Conductivity to transmisivity Unconfined aquifer - Darcy equation
               (this implementation does not cover > 1 layer)
*******************************************************************************}
procedure TranUnconf( const CellCond: array of Double;
                      const CellType: array of Integer;
                      const CellTop: array of Double;
                      const CellBot: array of Double;
                      const WatLev: array of Double;
                      const EDge: array of Double; var TRan: array of Double;
                      arhar: Integer; cellsNm: Integer);
var
  i,j,ij: Integer;
  dzij: Double;
  ratio: Double;
Begin

  for i:=0 to cellsNm -1 do begin
    for j:=0 to cellsNm -1 do begin
      if (CellTop[i]<CellBot[j]) or (CellTop[j]<CellBot[i]) then continue;
      { (i,j) -> ij }
      ij:= i*cellsNm + j;
      { Ratio of conductivities }
      if (CellCond[j]<>0) then ratio:= CellCond[i]/CellCond[j]
      else ratio:=1;
      { Common vertical }
      if (Watlev[i]+CellBot[i]>Watlev[j]+CellBot[j]) then 
      begin
        if (CellBot[i]>Watlev[j]+CellBot[j]) then dzij:=Watlev[i]
        else dzij:=Watlev[i]+CellBot[i]-CellBot[j];
      end
      else 
      begin
        if (CellBot[j]>Watlev[i]+CellBot[i]) then dzij:=Watlev[j]
        else dzij:=Watlev[j]+CellBot[j]-CellBot[i];
      end;
      { Caclulate transmisivity (1=arithmetic mean, 2=harmonic mean, 3=geo) }
      if (arhar=1) or ((ratio<1.005) and (ratio>0.995)) then TRan[ij]:=
        (CellCond[i]+CellCond[j])*EDge[ij]*dzij/2
      else if (arhar=2) then TRan[ij]:=
        2*(CellCond[i]*CellCond[j])/(CellCond[i]+CellCond[j])*EDge[ij]*dzij
      else if (arhar=3) then TRan[ij]:=
        sqrt(CellCond[i])*sqrt(CellCond[j])*EDge[ij]*dzij
      else if (arhar=4) then TRan[ij]:=
        (CellCond[i]-CellCond[j])/Ln(ratio)*EDge[ij]*dzij
      else begin 
        raise Exception.Create('arhar should be integer between 1 and 4!'); 
        Exit; 
      end;
      { If spring then spring conductivity dominates }
      if (CellType[i]=-1) and (CellType[j]<>-1) then 
        TRan[ij]:=CellCond[i]*EDge[ij]*dzij;
      if (CellType[j]=-1) and (CellType[i]<>-1) then 
        TRan[ij]:=CellCond[j]*EDge[ij]*dzij;

      { If cell next to spring has lower head then make trans=0 }
      if (CellType[i]=-1) and (CellType[j]<>-1) and
          (WatLev[i]+CellBot[i]>WatLev[j]+CellBot[j]) then TRan[ij]:=0;
      if (CellType[j]=-1) and (CellType[i]<>-1) and
          (WatLev[i]+CellBot[i]<WatLev[j]+CellBot[j]) then TRan[ij]:=0;
    end;
  end;
End;


{*******************************************************************************
                            Common vertical endge
*******************************************************************************}
function VertTouch(topi,boti,topj,botj: Double): Double;
Begin
  if (topi>topj) and (topj>boti) and (boti>botj) then VertTouch:=topj-boti
  else if (topj>topi) and (topi>botj) and (botj>boti) then VertTouch:=topi-botj
  else if (topi>topj) and (botj>boti) then VertTouch:=topj-botj
  else if (topj>topi) and (boti>botj) then VertTouch:=topi-boti
  else if TestRealEq(topi,topj) then VertTouch:=topi-max(boti,botj)
  else if TestRealEq(boti,botj) then VertTouch:=min(topi,topj)-boti
  else VertTouch:=0;

  { Test for valid data }
  if (topi<boti) or (topj<botj) then VertTouch:=-1e+10;

End;


{*******************************************************************************
                                     Darcy
*******************************************************************************}
function Darcy(tr, h1, h2, l: Double): Double;
Begin
  Darcy:=tr*(h1-h2)/l;
End;


{*******************************************************************************
                                 Mixed equation
*******************************************************************************}
function Mixed(c, h1, h2, l, J, alpha, D: Double): Double;
var 
  y_D:Double;
Begin
  y_D:=(h1-h2-J*l)/D; 
  if (y_D<=0.1) then y_D:=0.1;
  {begin Mixed:=0; Exit; end }
  if y_D<1 then Mixed:= c * power(y_D, alpha) * sqrt(J)
  else Mixed:= c * sqrt((h1-h2)/l);
End;


{*******************************************************************************
                              Explicit flow Solver
*******************************************************************************}
function ExpSolv( const CellTop:array of Double; const CellBot: array of Double;
                  const CellType: array of Integer; 
                  const CellCond: array of Double;
                  const TR: array of Double; const DIst: array of Double;
                  const Area: array of Double;
                  var WatLev: array of Double; var DIsch: array of Double;
                  lamda: Double;
                  cnfunc, arhar: Integer; fct1, fct2, fct3: Double; dt: Double;
                  cellsNm, eqType: Integer): Double;
var
WatLevBack: array of Double;
curHd, nextHd: Double;
curV, nextV: Double;
tran, l: Double;
exchwater: Double;
maxdw: Double;
i,j,ij: Integer;

Begin

  { Initialize-allocate }
  maxdw:=0;
  SetLength(WatLevBack, cellsNm);

  { Water movements are calculated with water level of previous time step }
  for i:=0 to cellsNm-1 do WatLevBack[i]:=WatLev[i];

  { i is current, j is next }
  for i:=0 to cellsNm -1 do begin
    for j:=i+1 to cellsNm -1 do begin
      ij:=i*cellsNm+j;
      tran:=TR[ij];
      if (tran<=0) then continue;
      curHd:=WlHd(CellTop[i],CellBot[i], WatLevBack[i],lamda);
      nextHd:=WlHd(CellTop[j],CellBot[j], WatLevBack[j],lamda);
      curV:= WatLev[i]*Area[i];
      nextV:=WatLev[j]*Area[j];

      l:=DIst[ij];
      DIsch[ij]:=0;
      exchWater:=Darcy(tran, curHd, nextHd, l)*dt;
      { flow i->j }
      if (curHd>nextHd) then
      begin 
        if (CellType[j]=-1) and (eqType=2) then 
          exchWater:=Mixed(CellCond[j], curHd, nextHd, l, fct1, fct2, fct3 )*dt;
        if (curV<exchWater) then exchWater:=curV;
        if (CellType[i]=-1) and (CellType[j]<>-1) then exchWater:=0;
      end
      { flow j->i }
      else 
      begin
        if (CellType[i]=-1) and (eqType=2) then 
          exchWater:=-Mixed(CellCond[i], nextHd, curHd, l, fct1, fct2, fct3 )*dt;
        if (nextV<-exchWater) then exchWater:=-nextV;
        if (CellType[j]=-1) and (CellType[i]<>-1) then exchWater:=0;
      end;
      { Calculate water level changes }
      WatLev[i]:= WatLev[i] - exchWater/Area[i];
      WatLev[j]:= WatLev[j] + exchWater/Area[j];
      DIsch[ij]:=exchWater;
    end;
  end;

  { Find maximum dw }
  for i:=0 to cellsNm -1 do
    maxdw:=max(maxdw, abs(WatLev[i]-WatLevBack[i]) );

  { Free memory }
  SetLength(WatLevBack, 0);
  ExpSolv:=maxdw;

End;


{*******************************************************************************
                              Implicit Flow Solver
*******************************************************************************}
function ImpSolv( const CellTop:array of Double; const CellBot: array of Double;
                  const CellType: array of Integer;
                  const CellCond: array of Double;
                  const TR: array of Double; const DIst: array of Double;
                  const Area: array of Double;
                  var WatLev: array of Double; var DIsch: array of Double;
                  lamda: Double; cnfunc, arhar: Integer;
                  fct1, fct2, fct3: Double; dt: Double; 
                  cellsNm, eqType: Integer): Double;
var
  AA: array of Double;
  B: array of Double;
  Heads: array of Double;
  i: Integer;
  singular: Integer;
Begin

  { ERROR: The implicit solver does not guaranties zero inflow from spring
           cells!}

  { Implicit solver works only with Darcy }
  if eqType<>1 then Begin ImpSolv:=-1; Exit; end;

  { Allocate memory }
  SetLength(AA, cellsNm*cellsNm);
  SetLength(B, cellsNm);
  SetLength(Heads, cellsNm);

  { Calculate constant terms }
  for i:=0 to cellsNm-1 do
    B[i]:=-WlHd(CellTop[i], CellBot[i], WatLev[i], lamda) * Area[i]/dt;

  { Calculate matrix with coefficients }
  Coeffs(TR, DIst, Area, AA, dt , cellsNm);

  { Solve the linear system }
  singular:=Gauss(AA, B, Heads, cellsNm);

  { Calculate water levels from heads }
  for i:=0 to CellsNm-1 do
    WatLev[i]:=HdWl(CellTop[i],CellBot[i], Heads[i],lamda);

  { Free memory }
  SetLength(AA, 0);
  SetLength(B, 0);
  SetLength(Heads, 0);

  ImpSolv:=singular;
End;


{*******************************************************************************
                              Sparse Flow Solver
*******************************************************************************}
function SparSolv( const CellTop:array of Double; const CellBot: array of Double;
                  const CellType: array of Integer;
                  const CellCond: array of Double;
                  const TR: array of Double; const DIst: array of Double;
                  const Area: array of Double;
                  var WatLev: array of Double; var DIsch: array of Double;
                  lamda: Double; cnfunc, arhar: Integer;
                  fct1, fct2, fct3: Double; dt: Double; 
                  cellsNm, eqType: Integer): Double;
var
  AA: array of Double;
  B: array of Double;
  Heads: array of Double;
  i,j,ij: Integer;
  allOK: Boolean;
Begin

  { ERROR: The implicit solver does not guaranties zero inflow from spring
           cells!}

  { Implicit solver works only with Darcy }
  if eqType<>1 then Begin SparSolv:=-1; Exit; end;

  { Allocate memory }
  SetLength(AA, cellsNm*cellsNm);
  SetLength(B, cellsNm);
  SetLength(Heads, cellsNm);

  { Calculate constant terms }
  for i:=0 to cellsNm-1 do
    B[i]:=-WlHd(CellTop[i], CellBot[i], WatLev[i], lamda) * Area[i]/dt;

  { Calculate matrix with coefficients }
  Coeffs(TR, DIst, Area, AA, dt , cellsNm);

  { Initialize sparse solver }
  allOK:=InitStruc(cellsNm);
  if not allOK then Begin SparSolv:=-2; Exit; end; 

  { Prepare the Left Hand Side of the sparse matrix}
  for i:=0 to cellsNm-1 do begin
    for j:=0 to cellsNm-1 do begin
      ij:=i*cellsNm+j;
      if (AA[ij]<>0) then allOK:= AddLHS(i+1, j+1, AA[ij]) and allOK;
    end;
  end;
  if not allOK then Begin SparSolv:=-3; Exit; end;

  { Prepare the Right Hand Side of the sparse matrix}
  for i:=0 to cellsNm-1 do allOK:=AddRHS(i+1, B[i]) and allOK;
  if not allOK then Begin SparSolv:=-4; Exit; end; 

  { Solve with sparse matrix algorithm }
  allOK:=Solve1;
  if not allOK then Begin SparSolv:=-5; Exit; end; 
  for i:=0 to cellsNm-1 do
    allOK:=GetAnswer(i+1,Heads[i]) and allOK;


  { Calculate water levels from heads }
  for i:=0 to cellsNm-1 do
    WatLev[i]:=HdWl(CellTop[i],CellBot[i], Heads[i],lamda);

  { Free memory }
  SetLength(AA, 0);
  SetLength(B, 0);
  SetLength(Heads, 0);
  ReleaseStruc;

  SparSolv:=1;
End;


{*******************************************************************************
                              Coeffs calculation
*******************************************************************************}
procedure Coeffs( const TR: array of Double; const DIst: array of Double;
                  const RsrvBase: array of Double;var AA: array of Double;
                  dt:Double; n:Integer);
var
  TRdist: array of Double;
  sum: Double;
  i,j: Integer;
  ij: Integer;
  ijs: Integer;

Begin

  { Allocate memory }
  SetLength(TRdist, n*n);

  { Calulate Tr/L }
  for i:=0 to n-1 do begin
    for j:=0 to n-1 do begin
      ij:=i*n+j;
      if (not TestRealEq(DIst[ij],0) ) then TRdist[ij]:=TR[ij]/DIst[ij];
    end;
  end;

  { Calculate AA }
  for i:=0 to n-1 do begin
    for j:=0 to n-1 do begin
      sum:=0;
      ij:=i*n+j;
      if (not TestRealEq(DIst[ij],0) ) then TRdist[ij]:=TR[ij]/DIst[ij];
      if i<>j then AA[ij]:=TRdist[ij]
      else begin
        for ijs:=i*n to i*n+n-1 do sum := sum + TRdist[ijs];
        AA[ij] := -( sum + RsrvBase[i]/dt - TRdist[i*n+i]);
        {ERROR: for confined it should be 
                  -( sum + RsrvBase[i]/dt/lambda - TRdist[i*n+i]);
         if lambda is <>0 this line introduces error!}
      end;
    end;
  end;

  { Free memory }
  SetLength(TRdist,0);

End;


{*******************************************************************************
                            Make sure it is symetric
*******************************************************************************}
function Symetrize(var SYmetr: array of Double; n: Integer): Boolean;
var
  i,j, ij,ji: Integer;
Begin

  for i:=0 to n-1 do begin
    for j:=i+1 to n-1 do begin
      ij:=i*n+j;
      ji:=j*n+i;
      if(not TestRealEq(SYmetr[ji], SYmetr[ij])) then
      begin
        if (not (TestRealEq(SYmetr[ji], 0)) and (not TestRealEq(SYmetr[ij], 0)))
          then begin Symetrize:=False; Exit; end
        else if TestRealEq(SYmetr[ji], 0) then SYmetr[ji]:=SYmetr[ij]
        else SYmetr[ij]:=SYmetr[ji];
      end;
    end;
  end;

  Symetrize:=True;

End;


{*******************************************************************************
                                 Ground Water
*******************************************************************************}
function GrdFlow(const CellCond:array of Double; const CellPor:array of Double;
                 const CellArea:array of Double; const CellTop:array of Double;
                 const CellBot:array of Double; const CellType:array of Integer;
                 var DIst: array of Double; var EDge: array of Double;
                 const Strs: array of Double;
                 var WatLev: array of Double; var DIsch: array of Double;
                 lamda : Double; cnfunc, arhar: Integer; 
                 fct1, fct2, fct3: Double;
                 dt: Double; ndt, cellsNm, eqType, slvType: Integer): Double;
Var
  RsvArea: array of Double;
  TRan: array of Double;
  i,n: Integer;
  maxdw: Double;
  Solver: function(const CellTop:array of Double; const CellBot:array of Double;
                   const CellType: array of Integer;
                   const CellCond: array of Double;
                   const TR: array of Double; const DIst: array of Double;
                   const Area: array of Double;
                   var WatLev: array of Double; var DIsch: array of Double;
                   lamda: Double; cnfunc, arhar: Integer; fct1, fct2, 
                   fct3: Double; dt: Double; cellsNm, eqType: Integer): Double;
Begin

  { Allocate memory initilize }
  SetLength(RsvArea, cellsNm);
  SetLength(TRan, cellsNm*cellsNm);
  maxdw:=0;

  { Make sure DIst and EDge are symmetric }
  if not Symetrize(DIst, cellsNm) then 
    Raise Exception.Create('DIst is not symmetric');
  if not Symetrize(EDge, cellsNm) then
    Raise Exception.Create('EDge is not symmetric');

  { Set Reservoir_base_Area or empty spring reservoir }
  for i:=0 to cellsNm-1 do
  begin
    if (CellType[i]<0) then
    begin
      RsvArea[i]:=1E+12;
      if (CellType[i]=-1) then WatLev[i]:=0;
    end
    else RsvArea[i]:=CellArea[i]*CellPor[i];
  end;

  { Calculate transmisivities }
  TranConf(CellCond, CellType, CellTop, CellBot, CellArea, EDge, TRan, arhar, 
           cellsNm);

  { Choose solver and flow equation }
  if (slvType=1) then Solver:=ExpSolv
  else if (slvType=2) and (eqType=1) then Solver:=ImpSolv
  else if (slvType=3) and (eqType=1) then Solver:=SparSolv
  else begin
    raise Exception.Create('slvType can be [1,2,3]. [2,3] only with eqType 1!');
    Exit;
  end;

  { Loop for step numbers }
  for n:=1 to ndt do
  begin

    { Apply stress }
    for i:=0 to cellsNm-1 do
    begin
      if  CellType[i]>0 then
      begin
        if (WatLev[i]+Strs[i]/CellPor[i]/ndt) >0  then
            WatLev[i]:=WatLev[i]+Strs[i]/CellPor[i]/ndt
        else WatLev[i]:=0;
      end;
    end;

    { Variable transmisivities in Darcy equation is selected }
    if ((cnfunc=2)) then TranUnconf(CellCond, CellType, CellTop, 
        CellBot, WatLev, Edge, TRan, arhar, cellsNm);

    { Calculate water movements }
    maxdw:=Solver( CellTop, CellBot, CellType, CellCond, TRan, DIst, RsvArea, 
                   WatLev, DIsch, lamda, cnfunc, arhar, fct1, fct2,fct3, dt/ndt,
                   cellsnm, eqtype );

  end;

  { Free memory }
  SetLength(RsvArea, 0);
  SetLength(TRan, 0);

  GrdFlow:=maxdw;

End;


{*******************************************************************************
                                 Wraper
*******************************************************************************}
procedure xlGrdFlow(var xlTblCond: Double; var xlTblPor : Double;
                    var xlTblArea: Double; var xlTblTop:  Double;
                    var xlTblBot: Double; var xlTblType: LongInt;
                    var xlTblDist: Double; var xlTblEdge: Double;
                    var xlTblStrs: Double; var xlTblWatLev: Double;
                    var xlTblDisch: Double;
                    lamda: Double; cnfunc, arhar: Integer; fct1, fct2, 
                    fct3: Double; dt: Double; 
                    ndt, cellsNm, eqType, slvType: LongInt;
                    var Returned: Double);
var
  CellCond: array of Double;
  CellPor:  array of Double;
  CellArea: array of Double;
  CellTop:  array of Double;
  CellBot:  array of Double;
  CellType: array of Integer;
  DIst: array of Double;
  EDge: array of Double;
  Strs: array of Double;
  WatLev: array of Double;
  DIsch: array of Double;
  pCond: ^Double;
  PArea: ^Double;
  pPor: ^Double;
  pTop: ^Double;
  pBot: ^Double;
  pType: ^LongInt;
  pDist: ^Double;
  pEdge: ^Double;
  pStrs: ^Double;
  pWatLev: ^Double;
  pDisch:^Double;
  i: Integer;
Begin

  { Set length of local arrays }
  SetLength(CellCond, cellsNm);
  SetLength(CellPor , cellsNm);
  SetLength(CellArea, cellsNm);
  SetLength(CellTop, cellsNm);
  SetLength(CellBot, cellsNm);
  SetLength(CellType, cellsNm);
  SetLength(DIst, cellsNm*cellsNm);
  SetLength(EDge, cellsNm*CellsNm);
  SetLength(Strs, cellsNm);
  SetLength(WatLev, cellsNm);
  SetLength(DIsch, cellsNm*cellsNm);

  { Copy external to local arrays }
  pCond:=@xlTblCond;
  pPor:=@xlTblPor;
  pArea:=@xlTblArea;
  pTop:=@xlTblTop;
  pBot:=@xlTblBot;
  pType:=@xlTblType;
  pDist:=@xlTblDist;
  pEdge:=@xlTblEdge;
  pStrs:=@xlTblStrs;
  pWatLev:=@xlTblWatLev;
  pDisch:=@xlTblDisch;
  for i:=0 to cellsNm*cellsNm-1 do
  begin
    Dist[i]:=pDist^; Inc(pDist);
    Edge[i]:=pEdge^; Inc(pEdge);
    DIsch[i]:=pDisch^; Inc(pDisch);
  end;
  for i:=0 to cellsNm-1 do
  begin
    CellCond[i]:=pCond^; Inc(pCond);
    CellPor[i]:= pPor^ ; Inc(pPor);
    CellArea[i]:=pArea^; Inc(pArea);
    CellTop [i]:=pTop^; Inc(pTop);
    CellBot [i]:=pBot^; Inc(pBot);
    CellType[i]:=pType^; Inc(pType);
    Strs[i]:=pStrs^; Inc(pStrs);
    WatLev[i]:=pWatLev^; Inc(pWatLev);
    DIsch[i]:=pDisch^; Inc(pDisch);
  end;

  { Call groundwater procedure }
  GrdFlow(CellCond, CellPor, CellArea, CellTop, CellBot, CellType,
          DIst, EDge, Strs, WatLev, DIsch, lamda, cnfunc, arhar, fct1, fct2
          ,fct3, Dt, ndt, cellsNm, eqType, slvType);

  { Copy local to external arrays }
  pWatLev:=@xlTblWatLev;
  for i:=0 to cellsNm-1 do
  begin
    pWatLev^:=WatLev[i]; Inc(pWatLev);
  end;
  pDisch:=@xlTblDisch;
  for i:=0 to cellsNm*cellsNm-1 do
  begin
    pDisch^:=DIsch[i]; Inc(pDisch);
  end;

  { Free memory }
  SetLength(CellCond, 0);
  SetLength(CellArea, 0);
  SetLength(CellPor , 0);
  SetLength(CellTop, 0);
  SetLength(CellBot, 0);
  SetLength(CellType, 0);
  SetLength(DIst, 0);
  SetLength(EDge, 0);
  SetLength(Strs, 0);
  SetLength(WatLev, 0);
  SetLength(DIsch, 0);

  returned:=1;

End;

{*******************************************************************************
                               Thiem Wraper
*******************************************************************************}
function xlThiem( var xlWellxyq: Double; nmwells: Integer;
                  x, y,  hoo, roo, tran: Double): Double;
var
  Wellx: array of Double;
  Welly: array of Double;
  Wellq: array of Double;
  pWellxyq: ^Double;
  i: Integer;
Begin

  { Initilize, allocate }
  SetLength(Wellx, nmwells);
  SetLength(Welly, nmwells);
  SetLength(Wellq, nmwells);
  pWellxyq:=@xlWellxyq;

  { Copy xl data to local data }
  for i:=0 to nmwells-1 do
  begin
    Wellx[i]:=pWellxyq^; Inc(pWellxyq);
    Welly[i]:=pWellxyq^; Inc(pWellxyq);
    Wellq[i]:=pWellxyq^; Inc(pWellxyq);
  end;

  { call thiem }
  xlThiem:=thiem(Wellx, Welly, Wellq, nmwells, x, y,  hoo, roo, tran);

  { Free memory }
  SetLength(Wellx, 0);
  SetLength(Welly, 0);
  SetLength(Wellq, 0);

End;



{*******************************************************************************
                                 Thiem
*******************************************************************************}
function Thiem( var Wellx: array of Double; var Welly: array of Double;
                var Wellq: array of Double;
                nmwells: Integer; x, y,  hoo, roo, tran: Double): Double;
var
dist: Double;
dh: Double;
i: Integer;
Begin

  { Initilize }
  dh:=hoo;

  { Calculate drawdown for the given point }
  for i:=0 to nmwells-1 do begin
    dist:=sqrt((wellx[i]-x)*(wellx[i]-x)+(welly[i]-y)*(welly[i]-y));
    if (dist=0) then dist:=0.3;
    if (dist>roo) then dist:=roo;
    dh:= dh + wellq[i]/(6.28*tran)*log10(roo/dist);
  end;

  Thiem:=dh;
End;

Initialization

{showmessage('Geia!'); }


End.
