{******************************************************************}
{                                                                  }
{  Thelma library                                                  }
{                                                                  }
{  Copyright (c) 2000-04 National Technical University of Athens   }
{                                                                  }
{******************************************************************}

{** Calculations of evapotranspiration. }
unit tsevap;

interface

uses Ts, Dates;

type
    TPenmanEvapMode = (pevmPenman, pevmPenmanMonteith);
    TPenmanSunDurMode = (psdmDuration, psdmPercentage, psdmRadiation);
    TPenmanMonthlyDay = (pmodMiddle, pmodRepresentative);

    TPenmanOptions = record
      EvapMode: TPenmanEvapMode;
      SunDurMode: TPenmanSunDurMode;
      MonthlyDay: TPenmanMonthlyDay;
    end;

{** Creates an evaporation time series using the Penman method.
    Use CalcPenmanEvap to create an evaporation time series from time series
    of air temperature, relative humidity, wind speed, and sunshine duration,
    using the Penman method. AirTempSeries, RelHumSeries, WindSpSeries and
    SunDurSeries are the source time series in degrees Celsius, %, m/s, and
    minutes respectively; Dest is the destination time series in mm. Any already
    existing records in Dest are deleted. The timestamps for
    which records are created in Dest are those for which not null records
    exist in all source time series (i.e. the common period).<p>
    Latitude is the latitude in degrees; use DegMinSecToDegrees to convert
    latitude from degrees, minutes, seconds to decimal degrees.<p>
    a_e and b_e are the co-efficients of the Brunt formula, which gives the
    net thermal emission capacity. Their values are 0.56 and 0.08 according
    to Penman (1948), and 0.34 and 0.044 according to Doorenbos and Pruitt
    (1977).<p>
    Set Mode to pevmPenman for Penman calculation or to pevmPenmanMonteith
    for Penman-Monteith.<p>
    Penman-Monteith uses a reduced value of Gamma (Gammat := Gamma*(1+0.33*u),
    and a Wind function of F(u) := 90*u/(T+273) ( = 0.13+0.140u in Penman).<p>
    a_L and b_L are the co-efficients of the formula which gives the
    cloud effect co-efficient; their values are normally 0.1 and 0.9 according
    to Penman (1948).<p>
    a_s and b_s are the co-efficients of the formula which gives the Prescott
    co-efficient; their values are typically 0.25 and 0.50.<p>
    The method used is described in Koutsoyiannis and Xanthopoulos, Technical
    Hydrology, Athens 1997. If Application 3.4.5 (page 205) is calculated with
    this procedure, the results are almost identical, the differences being
    solely due to rounding errors.<p>
    @author A.X.
    @SeeAlso <See Routine=DegMinSecToDegrees>
}
procedure CalcPenmanEvap(Dest, AirTempSeries, RelHumSeries, WindSpSeries,
  SunDurSeries: TTimeseries;  Latitude: Real; Altitude: Integer; Albedo,
  a_e, b_e, a_L, b_L, a_s, b_s: Real; Options: TPenmanOptions); overload;

{** Creates an evaporation time series using the Penman method.
    This version of CalcPenmanEvap calculates evaporation for a specific
    list of dates. For all dates in ADateTimeList not null values must exist in
    all source time series, or an exception is raised.<p>
    Normally you don't have to call this version; use the other overloaded
    version of CalcPenmanEvap, which will determine the common period and
    call this one.<p>
    @author A.X.
}
procedure CalcPenmanEvap(Dest, AirTempSeries, RelHumSeries, WindSpSeries,
  SunDurSeries: TTimeseries; ADateTimeList: TDateTimeList;  Latitude: Real;
  Altitude: Integer; Albedo, a_e, b_e, a_L, b_L, a_s, b_s: Real;
  Options: TPenmanOptions); overload;

{** Creates an evaporation time series using the Thornthwaite method.
    The Iota parameter must be evaluated from mean Temperature of
    12 months.<p>
    @author Stefanos
    @SeeAlso <See Routine=CalcThornthwaiteIota>
}
procedure CalcThornthwaiteEvap(Dest, AirTempSeries: TTimeseries;
  Latitude, Iota: Real);

{** Calculate Iota parameter of Thornthwaite method by using monthly
    temperature timeseries.<p>
    @author Stefanos
    @SeeAlso <See Routine=CalcThornthwaiteEvap>
}
function CalcThornthwaiteIota(AirTempSeries: TTimeseries): Real;

{** Calculate the evaporation timeseries by using the parametric
    empirical equation E=(aSo-b)/(1-cTa) where a,b,c emprical
    paramters, So the extraterestial  radiation (kJ/(m^2d)).
    Alpha, Beta, Ce : the a,b,c respectively.<p>
    AirTempSeries should be of daily or monthly time step or
    else an exception is raised.
    @author Stefanos
    @SeeAlso <See Routine=ParametricEvapFind>
}
procedure CalcParametricEvap(Dest, AirTempSeries: TTimeseries;
  Latitude, Alpha, Beta, Ce: Real; MonthlyDay: TPenmanMonthlyDay); overload;

{** An overloaded version of CalcParametricEvap.
    This version is for internal use only.
    @author Stefanos
}
procedure CalcParametricEvap(Dest, AirTempSeries, S0: TTimeseries;
  Latitude, Alpha, Beta, Ce: Real; MonthlyDay: TPenmanMonthlyDay); overload;

{** Use this function in order to calculate parameters Alpa, Beta, Ce.
    Use this function in conjuction with CalcParametricEvap.
    ParametricEvapFind estimate values for Alpha, Beta and Ce with
    the Least Squares Method by using a timeseries with measured
    temperature and a Evapo(traspi)ration timeseries calculated with
    a complex method such as Penman or Penman-Monteith.<p>
    Function returns the coefficient of determination, namely the R (=r^2).
    AirTempSeries and EvaporationSeriesshould be of daily or monthly time step
    or else an exception is raised. In addition they should be of the same
    time step.
    @author Stefanos
    @SeeAlso <See Routine=CalcParametricEvap>
}
function ParametricEvapFind(AirTempSeries, EvaporationSeries: TTimeseries;
  Latitude: Real; var Alpha, Beta, Ce: Real; MonthlyDay: TPenmanMonthlyDay): Real;

{** Calculate the Blaney Criddle Evapotranspiration.
    AirTempSeries should be of monthly time step or else and assertion
    exception is raised. kc is the crop coefficient.
    @author Stefanos
}
procedure CalcBlaneyCriddleEvap(Dest, AirTempSeries: TTimeseries;
  Latitude, kc: Real);

{** Calculate the Hargreaves Evapotranspiration for a reference crop.
    AirTempSeries should be of monthly or daily time step or else
    an exception is raised. Lambda coefficient is considered constant
    with a value of 2460.
    @author Stefanos
}
procedure CalcHargreavesEvap(Dest, AirTempSeries, AirMaxSeries,
  AirMinSeries: TTimeseries; Latitude: Real; MonthlyDay: TPenmanMonthlyDay);

implementation

uses SysUtils, Math, Classes, TsProcess, contnrs, matrix, dateutils;

const
  Epsilon = 0.622; { Ratio of molecular weight of water to dry air. }
  Cp = 1.013; { Specific heat of air (for constant pressure); kJ/kg/degC }
  I_s = 4921; { Solar constant; kJ/m2/h }
  Sigma = 4.9e-6; { Stefan-Boltzman constant in kJ/(m2 K4 d) }

{ Returns pressure in hPa as a function of altitude in m. }
function Pressure(Altitude: Integer): Real;
begin
  Result := 1013*Power(1-2.218e-5*Altitude, 5.256);
end;

{ Returns the latent heat of evaporation in kJ/kg
  given water surface temperature in degrees C.
}
function LatentHeat(Ts: Real): Real;
begin
  Result := 2501-2.361*Ts;
end;

{ Returns the psychrometric co-efficient
  given pressure and latent heat of evaporation.
}
function Psychrometric(p, Lambda: Real): Real;
begin
  Result := Cp*p/Epsilon/lambda;
end;

{ Returns the saturation vapor pressure in hPa given air temperature in degC. }
function SatVaporPressure(T: Real): Real;
begin
  Result := 6.11*Exp(17.27*T/(T+237.3));
end;

{ Returns the slope of the saturation vapor pressure curve
  given saturation vapor pressure and temperature.
}
function VaporSlope(eStar, T: Real): Real;
begin
  Result := 4098*eStar/Sqr(T+237.3);
end;

{ Returns solar declination in rad as a function of the day of year. }
function SolarDeclination(J: Integer): Real;
begin
  Result := 0.4093*Cos(2*Pi/365*J-2.98);
end;

resourcestring
  rsCannotCalcEvapLat =
    'Cannot calculate evaporation for latitudes greater than 66.5 degrees';

{ Returns sunset time angle in rad given latitude in rad and solar declination
  in rad.
}
function SunsetTimeAngle(Phi, Delta: Real): Real;
var
  InCos: Real;
begin
  InCos := -Tan(phi)*Tan(delta);
  if abs(InCos)>1 then
    raise EMathError.Create(rsCannotCalcEvapLat);
  Result := ArcCos(InCos);
end;

{ Returns astronomical duration of day (in hours) given sunset time angle. }
function DayDuration(Omega: Real): Real;
begin
  Result := (24/Pi)*Omega;
end;

{ Returns eccentricity given day of year. }
function Eccentricity(J: Integer): Real;
begin
  Result := 1 + 0.033*Cos(2*Pi/365*J);
end;

{ Returns extraterrestrial solar radiation (kJ/m2/d) given eccentricity,
  sunset time angle in rad, latitude in rad, solar declination in rad.
}
function ETSolarRad(Dr, Omega, Phi, Delta: Real): Real;
begin
  Result := 24*I_s*Dr/Pi*
            (Omega*Sin(Phi)*Sin(Delta)+Sin(Omega)*Cos(Phi)*Cos(Delta));
end;

{ Returns co-efficient of atmospheric absorbtion given day sunshine duration
  and day duration.
}
function Prescott(a_s, b_s, SunDur, DayDur: Real): Real;
begin
  Result := a_s+b_s*SunDur/DayDur;
end;

{ Returns net short wave radiation given albedo, co-efficient of
  atmospheric absorbtion and extraterrestrial solar radiation. }
function SWRad(Albedo, Fs, S0: Real): Real;
begin
  Result := (1-Albedo)*Fs*S0;
end;

{ Returns net thermal emission capacity given co-efficients and vapor
  pressure. }
function ThermEmissionCap(a_e, b_e, e: Real): Real;
begin
  Result := a_e-b_e*Sqrt(e);
end;

{ Returns the cloud effect co-efficient given sunshine duration of day and
  day duration. }
function CloudCoef(a_L, b_L, SunDur, DayDur: Real): Real;
begin
  Result := a_L+b_L*SunDur/DayDur;
end;

{ Returns net long wave radiation in kJ/m2/d given thermal emission capacity,
  cloud co-efficient, and air temperature in degC. }
function LWRad(en, fL, Ta: Real): Real;
begin
  Result := en*fL*Sigma*IntPower(Ta+273, 4);
end;

{ Returns Penman wind function given wind speed in m/s and temperature in degC.
}
function WindFunction(Mode: TPenmanEvapMode; u, T: Real): Real;
begin
  Result := 0;
  case Mode of
    pevmPenman: Result := 0.13 + 0.140*u;
    pevmPenmanMonteith: Result := 90 * u /(T+273);
  else
    Assert(False);
  end;
end;

{ Reduction factor for the psychrometric gamma
}
function PsychrometricReduction(Mode: TPenmanEvapMode; u: Real): Real;
begin
  Result := 1;
  case Mode of
    pevmPenman: Result := 1;
    pevmPenmanMonteith: Result := 1 + 0.33*u;
  else
    Assert(False);
  end;
end;

{ Returns the saturation deficit given saturation vapor pressure in hPa, air
  temperature in degC, and relative humidity as a number between 0 and 1. }
function SatDeficit(eStar, U: Real): Real;
begin
  Result := eStar*(1-U);
end;

function Penman(Mode: TPenmanEvapMode; SunMode: TPenmanSunDurMode;
  ADateTime, AirTemp, RelHum, WindSp, SunshDuration: Real;
  Latitude: Real; Altitude: Integer; Albedo, a_e, b_e, a_L, b_L, a_s,
  b_s: Real): Real;
var
  eStar, delta, Gamma, Rn, Fu, D, Sn, Ln, N, Fs, S0, Omega, fL, eN,
  BigDelta, lambda, Gammat, ASunshDuration: Real;
  J: Integer;
begin
  eStar := SatVaporPressure(AirTemp);
  BigDelta := VaporSlope(eStar, AirTemp);
  lambda := LatentHeat(AirTemp);
  Gamma := Psychrometric(Pressure(Altitude), lambda);
  Gammat := Gamma * PsychrometricReduction(Mode, WindSp);
  J := DayOfTheYear(ADateTime);
  delta := SolarDeclination(J);
  Omega := SunsetTimeAngle(Latitude, delta);
  N := DayDuration(Omega);
  S0 := ETSolarRad(Eccentricity(J), Omega, Latitude, delta);
  ASunshDuration := SunshDuration/60;
  case SunMode of
    psdmDuration: ASunshDuration := SunshDuration/60;
    psdmPercentage: ASunshDuration := N * SunshDuration;
    psdmRadiation: ASunshDuration := (N/b_s)* ( SunshDuration / S0 - a_s);
  else
    Assert(False);
  end;
  Fs := Prescott(a_s, b_s, ASunshDuration, N);
  Sn := SWRad(Albedo, Fs, S0);
  fL := CloudCoef(a_L, b_L, ASunshDuration, N);
  eN := ThermEmissionCap(a_e, b_e, RelHum*eStar);
  Ln := LWRad(eN, fL, AirTemp);
  Rn := Sn-Ln;
  Fu := WindFunction(Mode, WindSp, AirTemp);
  D := SatDeficit(eStar, RelHum);
  Result := (BigDelta/(BigDelta+Gammat)) * Rn/lambda +
            (Gamma/(BigDelta+Gammat)) * Fu * D;
end;

procedure CalcPenmanEvap(Dest, AirTempSeries, RelHumSeries, WindSpSeries,
  SunDurSeries: TTimeseries;  Latitude: Real; Altitude: Integer; Albedo,
  a_e, b_e, a_L, b_L, a_s, b_s: Real; Options: TPenmanOptions);
var
  CommonPeriod, AllRecords: TDateTimeList;
  AList: TObjectList;
  ATimeseries: TTimeseries;
  i: Integer;
begin
  CommonPeriod := nil;
  AList := nil;
  ATimeseries := nil;
  AllRecords := nil;
  try
    AList := TObjectList.Create(False);
    AList.Add(AirTempSeries);
    AList.Add(RelHumSeries);
    AList.Add(WindSpSeries);
    AList.Add(SunDurSeries);
    CommonPeriod := GetCommonPeriod(AList, 0);
    AllRecords := GetAllRecords(AList, 0);
    ATimeseries := TTimeseries.Create;
    CalcPenmanEvap(ATimeseries, AirTempSeries, RelHumSeries, WindSpSeries,
      SunDurSeries, CommonPeriod, Latitude, Altitude, Albedo,
      a_e, b_e, a_L, b_L, a_s, b_s, Options);
    Dest.AssignMeta(ATimeseries);
    for i := 0 to AllRecords.Count-1 do
    begin
      Dest.Add(AllRecords[i], True, 0, '', msNew);
      if CommonPeriod.IndexOf(AllRecords[i])<0 then Continue;
      Dest.Last.AsFloat :=
        ATimeseries[ATimeseries.IndexOf(AllRecords[i])].AsFloat;
    end;
  finally
    AList.Free;
    CommonPeriod.Free;
    ATimeseries.Free;
    AllRecords.Free;
  end;
end;

const
  RepresentativeMonthDays: array[1..12] of Word =
    (18, 15, 16, 15, 15, 11, 18, 17, 16, 16, 14, 11);

function MonthReprDate(ADateTime: TDateTime; ATimeStep: TTimeStep;
  MonthlyDay: TPenmanMonthlyDay): TDateTime;
var
  MDays: Word;
begin
  Result := ADateTime;
  if ATimeStep<>tstMonthly then Exit;
  if MonthlyDay = pmodMiddle then
  begin
    MDays := DaysInMonth(ADateTime);
    Result := AddDateTime(ADateTime, ((MDays+1) div 2)-1);
  end else begin
    Result := EncodeDate(YearOf(ADateTime), MonthOf(ADateTime),
      RepresentativeMonthDays[MonthOf(ADateTime)]);
  end;
end;

resourcestring
  rsSourceTimeSeriesHaveDifferentSteps =
    'All source time series must have the same time step.';
  rsInvalidTimeStep =
    'Evaporation can only be computed for daily and monthly time series.';

procedure CalcPenmanEvap(Dest, AirTempSeries, RelHumSeries, WindSpSeries,
  SunDurSeries: TTimeseries; ADateTimeList: TDateTimeList;  Latitude: Real;
  Altitude: Integer; Albedo, a_e, b_e, a_L, b_L, a_s, b_s: Real;
  Options: TPenmanOptions);
var
  AirTemp, RelHum, WindSp, SunDur, Evap: Real;
  i: Integer;
  MDays: Word;
  ADateTime: TDateTime;
begin
  { Make sure all source time series have a good time step. }
  if (AirTempSeries.TimeStep<>RelHumSeries.TimeStep) or
  (AirTempSeries.TimeStep<>WindSpSeries.TimeStep) or
  (AirTempSeries.TimeStep<>SunDurSeries.TimeStep) then
    raise Exception.Create(rsSourceTimeSeriesHaveDifferentSteps);
  if (AirTempSeries.TimeStep<>tstDaily) and
  (AirTempSeries.TimeStep<>tstMonthly) then
    raise Exception.Create(rsInvalidTimeStep);
  Dest.Clear;
  Dest.AssignMeta(AirTempSeries);
  { Do the job for each entry in the date list. }
  for i := ADateTimeList.Count-1 downto 0 do
  begin
    ADateTime := ADateTimeList[i];
    AirTemp := AirTempSeries[AirTempSeries.IndexOf(ADateTime)].AsFloat;
    RelHum := RelHumSeries[RelHumSeries.IndexOf(ADateTime)].AsFloat;
    WindSp := WindSpSeries[WindSpSeries.IndexOf(ADateTime)].AsFloat;
    SunDur := SunDurSeries[SunDurSeries.IndexOf(ADateTime)].AsFloat;
    MDays := DaysInMonth(ADateTime);
    if Dest.TimeStep = tstMonthly then
      if Options.SunDurMode = psdmDuration then
        SunDur := SunDur/MDays;
    Evap := Penman(Options.EvapMode, Options.SunDurMode, MonthReprDate(
      ADateTime, Dest.TimeStep, Options.MonthlyDay), AirTemp,
      RelHum/100, WindSp,  SunDur, DegToRad(Latitude), Altitude, Albedo,
      a_e, b_e, a_L, b_L, a_s, b_s);
    if Dest.TimeStep=tstMonthly then
      Evap := Evap*MDays;
    Dest.Insert(ADateTime, False, Evap, '', msNew);
  end;
end;

resourcestring
  rsInsufficientSample = 'Insufficient sample to calculate Thornthwaite '+
    'evaporation';
  rsThornthwaiteAllowedForMonthly = 'Temperature time series should have '+
    'monthly time step for Thornthwaite calculations';

{Thornthwaite evaporation}
function CalcThornthwaiteIota(AirTempSeries: TTimeseries): Real;
var
  i: Integer;
  countMonths: array [1..12] of Integer;
  MonthI: array [1..12] of Real;
  ADay,AMonth,AYear: Word;
begin
  Assert(AirTempSeries.TimeStep=tstMonthly, rsThornthwaiteAllowedForMonthly);
{Initialize arrays}
  for i := 1 to 12 do
  begin
    countMonths[i] := 0;
    MonthI[i] := 0.00;
  end;
{First calculate mean temperatures for each month}
  for i := 0 to AirTempSeries.Count-1 do
  begin
    if not AirTempSeries.Items[i].IsNull then
    begin
      DecodeDate(AirTempSeries.Items[i].Date, AYear, AMonth, ADay);
      Inc(countMonths[AMonth]);
      MonthI[AMonth] := MonthI[AMonth] + AirTempSeries.Items[i].AsFloat;
    end;
  end;
  for i := 1 to 12 do
  begin
    if countMonths[i] = 0 then
      raise Exception.Create(rsInsufficientSample);
{truncate negative values to zero}
    if MonthI[i]<0 then MonthI[i] := 0.00001;
  end;
  Result := 0.000;
  for i := 1 to 12 do
  begin
    Result := Result +
      0.09*Power(MonthI[i]/countMonths[i],1.5);
  end;
end;

procedure CalcThornthwaiteEvap(Dest, AirTempSeries: TTimeseries;
  Latitude, Iota: Real);
var
  Alpha: real;
  i: Integer;
  ADayOfYear: Integer;
  DaysOfMonth: Real;
  Evap,Delta, Omega, MeanDayLength: Real;
begin
  Assert(AirTempSeries.TimeStep=tstMonthly, rsThornthwaiteAllowedForMonthly);
  Latitude := DegToRad(Latitude);
  Alpha := 0.016*Iota+0.5;
  { Prepare some properties of Dest. }
  Dest.Clear;
  Dest.AssignMeta(AirTempSeries);
  for i := 0 to AirTempSeries.Count-1 do
  begin
    DaysOfMonth := DaysInMonth(AirTempSeries.Items[i].Date);
    ADayOfYear := DayOfTheYear(AirTempSeries.Items[i].Date+DaysOfMonth/2-1);
    Delta := SolarDeclination(ADayOfYear);
    Omega := SunsetTimeAngle(Latitude, Delta);
    MeanDayLength := DayDuration(Omega);
    Evap := 0.00;
    if not AirTempSeries.Items[i].IsNull then
      if AirTempSeries.Items[i].AsFloat >0 then
        Evap := 16*Power( 10*AirTempSeries.Items[i].AsFloat/Iota, Alpha )*
          DaysOfMonth*MeanDayLength/360;
    if AirTempSeries.Items[i].IsNull then
      Dest.Add(AirTempSeries.Items[i].Date, True, 0, '', msNew)
    else
      Dest.Add(AirTempSeries.Items[i].Date, False, Evap, '', msNew);
  end;
end;

function ParametricEvap(AirTemperature, Latitude, Alpha, Beta, Ce: Real;
  ADate: TDateTime; var S0: Real): Real;
var
  ADayOfYear: Integer;
  Delta, Omega, Dr: Real;
begin
  ADayOfYear := DayOfTheYear(ADate);
  Delta := SolarDeclination(ADayOfYear);
  Omega := SunsetTimeAngle(Latitude, Delta);
  Dr := Eccentricity(ADayOfYear);
  S0 := ETSolarRad(Dr, Omega, Latitude, Delta);
  Result := (Alpha*S0-Beta)/ (1-Ce*AirTemperature);
end;

procedure CalcParametricEvap(Dest, AirTempSeries, S0: TTimeseries;
  Latitude, Alpha, Beta, Ce: Real; MonthlyDay: TPenmanMonthlyDay);
var
  i: Integer;
  ADateTime: TDateTime;
  Evap, S0v: Real;
  MDays: Word;
begin
  if (AirTempSeries.TimeStep<>tstDaily) and
    (AirTempSeries.TimeStep<>tstMonthly) then
    raise Exception.Create(rsInvalidTimeStep);
  if S0<>nil then
  begin
    S0.AssignMeta(AirTempSeries);
  end;
  Dest.Clear;
  Dest.AssignMeta(AirTempSeries);
  for i := 0 to AirTempSeries.Count-1 do
  begin
    ADateTime := AirTempSeries[i].Date;
    MDays := DaysInMonth(ADateTime);
    ADateTime := MonthReprDate(ADateTime, Dest.TimeStep, MonthlyDay);
    Evap := 0.000;
    if not AirTempSeries.Items[i].IsNull then
      if AirTempSeries.Items[i].AsFloat >0 then
        Evap := ParametricEvap(AirTempSeries[i].AsFloat, DegToRad(Latitude),
          Alpha, Beta, Ce, ADateTime, S0v);
    if AirTempSeries.TimeStep = tstMonthly then
      Evap := Evap * MDays;
    if AirTempSeries.Items[i].IsNull then
      Dest.Add(AirTempSeries.Items[i].Date, True, 0, '', msNew)
    else
      Dest.Add(AirTempSeries.Items[i].Date, False, Evap, '', msNew);
    if S0<>nil then
    begin
      if AirTempSeries.Items[i].IsNull then
        S0.Add(AirTempSeries.Items[i].Date, True, 0, '', msNew)
      else
        S0.Add(AirTempSeries.Items[i].Date, False, S0v, '', msNew);
    end;
  end;
end;

procedure CalcParametricEvap(Dest, AirTempSeries: TTimeseries;
  Latitude, Alpha, Beta, Ce: Real; MonthlyDay: TPenmanMonthlyDay);
begin
  CalcParametricEvap(Dest, AirTempSeries, nil, Latitude, Alpha, Beta, Ce,
    MonthlyDay);
end;

resourcestring
  rsTimeseriesShouldBeOfTheSameTimeStep =
    'Time series should be of the same time step';
  rsToFewRecords = 'Too few records to calculate parameters';

function ParametricEvapFind(AirTempSeries, EvaporationSeries: TTimeseries;
  Latitude: Real; var Alpha, Beta, Ce: Real; MonthlyDay: TPenmanMonthlyDay): Real;
var
  i, MDays: Integer;
  ACommonPeriod: TDateTimeList;
  ATimeseriesList: TObjectList;
  CalcEvaporationSeries, S0Timeseries: TTimeseries;
  S0, AirTemperature: Real;
  X0, DX, DL: TVector;
  Am, Nm: TMatrix;

  function DoLoop: Real;
  var
    j: Integer;
  begin
    CalcParametricEvap(CalcEvaporationSeries, AirTempSeries, S0Timeseries,
      Latitude, X0[1], X0[2], X0[3], MonthlyDay);
    Am.Resize(ACommonPeriod.Count, 3);
    for j := 1 to ACommonPeriod.Count do
    begin
      S0 := S0Timeseries[S0Timeseries.IndexOf(ACommonPeriod[j-1])].AsFloat;
      AirTemperature :=
        AirTempSeries[AirTempSeries.IndexOf(ACommonPeriod[j-1])].AsFloat;
      Am[j, 1] := S0/(1-X0[3]*Airtemperature);
      Am[j, 2] := -1/(1-X0[3]*Airtemperature);
      Am[j, 3] := (X0[1]* S0- X0[2])* AirTemperature/
        Sqr(1- X0[3]* AirTemperature);
    end;
    Nm.Assign(Am);
    Am.Transpose;
    Nm.Multiply(Am, maLeft);
    Nm.Invert;
    DL.Resize(ACommonPeriod.Count);
    Result := 0;
    for j := 1 to ACommonPeriod.Count do
    begin
      DL[j] := EvaporationSeries[
        EvaporationSeries.IndexOf(ACommonPeriod[j-1])].AsFloat -
          CalcEvaporationSeries[
            CalcEvaporationSeries.IndexOf(ACommonPeriod[j-1])].AsFloat;
      Result := Result + Sqr(DL[j]);
      if AirTempSeries.TimeStep = tstMonthly then
      begin
        MDays := DaysInMonth(ACommonPeriod[j-1]);
        DL[j] := DL[j]/MDays;
      end;
    end;
    Result := 1- Result /
      (CalcEvaporationSeries.Variance(ACommonPeriod)*(ACommonPeriod.Count-1));
    DL.Multiply(Am, maLeft);
    DX.Assign(DL);
    DX.Multiply(Nm, maLeft);
    X0.Add(DX);
  end;

begin
  Result := 0;
  if AirTempSeries.TimeStep<>EvaporationSeries.TimeStep then
    raise Exception.Create(rsTimeseriesShouldBeOfTheSameTimeStep);
  if (AirTempSeries.TimeStep<>tstDaily) and
    (AirTempSeries.TimeStep<>tstMonthly) then
    raise Exception.Create(rsInvalidTimeStep);
  ATimeseriesList := nil;
  ACommonPeriod := nil;
  CalcEvaporationSeries := nil;
  S0Timeseries := nil;
  try
    ATimeseriesList := TObjectList.Create(False);
    ATimeseriesList.Add(AirTempSeries);
    ATimeseriesList.Add(EvaporationSeries);
    ACommonPeriod := GetCommonPeriod(ATimeseriesList, 0);
    if ACommonPeriod.Count<4 then
      raise Exception.Create(rsToFewRecords);
    CalcEvaporationSeries := TTimeseries.Create;
    S0timeseries := TTimeseries.Create;
    X0 := nil;
    DX := nil;
    DL := nil;
    Am := nil;
    Nm := nil;
    try
      X0 := TVector.Create(3);
      DX := TVector.Create(3);
      DL := TVector.Create;
      DL.Transpose;
      Am := TMatrix.Create;
      Nm := TMatrix.Create;
      X0[1] := 0.0001;
      X0[2] := 0.6;
      X0[3] := 0.02;
      i := 0;
      Repeat
        Inc(i);
        Result := DoLoop;
      Until (i>3) and
        ((Sqrt(Sqr(DX[1])+Sqr(DX[2])+Sqr(DX[3]))<0.0001) or (i>10));
      Alpha := X0[1];
      Beta := X0[2];
      Ce := X0[3];
    finally
      X0.Free;
      DX.Free;
      DL.Free;
      Am.Free;
      Nm.Free;
    end;
  finally
    CalcEvaporationSeries.Free;
    S0Timeseries.Free;
    ATimeseriesList.Free;
    ACommonPeriod.Free;
  end;
end;

function BlaneyCriddleEvap(AirTemperature, kc, Latitude: Real;
  ADate: TDateTime): Real;
var
  AMeanDayLength, ADaysOfMonth, APercentage: Real;
  ADayOfYear: Integer;
  Delta, Omega: Real;
begin
  ADaysOfMonth := DaysInMonth(ADate);
  ADayOfYear := DayOfTheYear(ADate+ ADaysOfMonth/ 2- 1);
  Delta := SolarDeclination(ADayOfYear);
  Omega := SunsetTimeAngle(Latitude, Delta);
  AMeanDayLength := DayDuration(Omega);
  APercentage := 100* ADaysOfMonth* AMeanDayLength/ (365* 12);
  Result := 0.254* kc* APercentage* (32+ 1.8*AirTemperature);
end;

resourcestring
  rsOnlyMonthlyForBlaneyCriddle = 'Temperature time series should have '+
    'monthly time step for Blaney-Criddle calculations';

procedure CalcBlaneyCriddleEvap(Dest, AirTempSeries: TTimeseries;
  Latitude, kc: Real);
var
  i: Integer;
  Evap: Real;
begin
  Assert(AirTempSeries.TimeStep=tstMonthly, rsOnlyMonthlyForBlaneyCriddle);
  Dest.Clear;
  Dest.AssignMeta(AirTempSeries);
  for i := 0 to AirTempSeries.Count-1 do
  begin
    Evap := 0;
    if not AirTempSeries[i].IsNull then
      Evap := BlaneyCriddleEvap(AirTempSeries[i].AsFloat, kc,
        DegToRad(Latitude), AirTempSeries[i].Date);
    if AirTempSeries.Items[i].IsNull then
      Dest.Add(AirTempSeries.Items[i].Date, True, 0, '', msNew)
    else
      Dest.Add(AirTempSeries.Items[i].Date, False, Evap, '', msNew);
  end;
end;

function HargreavesEvap(AirTemperature, Latitude, TMax, TMin: Real;
  ADate: TDateTime): Real;
var
  ADayOfYear: Integer;
  Delta, Omega, Dr, S0: Real;
begin
  ADayOfYear := DayOfTheYear(ADate);
  Delta := SolarDeclination(ADayOfYear);
  Omega := SunsetTimeAngle(Latitude, Delta);
  Dr := Eccentricity(ADayOfYear);
  S0 := ETSolarRad(Dr, Omega, Latitude, Delta);
  Result := 0.0023*(S0/2460)*(AirTemperature+17.8)*Sqrt(TMax-TMin);
end;

resourcestring
  rsOnlyMonthlyForHargreaves = 'Temperature time series should have '+
    'monthly or daily time step for Hargreaves calculations';
  rsAtLeastMinMax = 'Max and Min temperature timeseries should be specified '+
    'or at least difference (Max-Min) timeseries';

procedure CalcHargreavesEvap(Dest, AirTempSeries, AirMaxSeries,
  AirMinSeries: TTimeseries; Latitude: Real; MonthlyDay: TPenmanMonthlyDay);
var
  i: Integer;
  Evap, TMin, TMax, AirTemp: Real;
  MDays: Word;
  ADateTime: TDateTime;
  ADateTimeList, AllRecords: TDateTimeList;
  ATimesereriesList: TObjectList;
begin
  if (AirMaxSeries=nil) then
    raise Exception.Create(rsAtLeastMinMax);
  if (AirTempSeries.TimeStep<>tstDaily) and
    (AirTempSeries.TimeStep<>tstMonthly) then
    raise Exception.Create(rsOnlyMonthlyForHargreaves);
  if (AirTempSeries.TimeStep<>AirMaxSeries.TimeStep) then
    raise Exception.Create(rsTimeseriesShouldBeOfTheSameTimeStep);
  if AirMinSeries<>nil then
    if (AirMinSeries.TimeStep<>AirMaxSeries.TimeStep) then
      raise Exception.Create(rsTimeseriesShouldBeOfTheSameTimeStep);
  ATimesereriesList := nil;
  ADateTimeList := nil;
  AllRecords := nil;
  try
    ATimesereriesList := TObjectList.Create(False);
    ATimesereriesList.Add(AirTempSeries);
    ATimesereriesList.Add(AirMaxSeries);
    if AirMinSeries<> nil then
      ATimesereriesList.Add(AirMinSeries);
    ADateTimeList := GetCommonPeriod(ATimesereriesList, 0);
    AllRecords := GetAllRecords(ATimesereriesList, 0);
    Dest.Clear;
    Dest.AssignMeta(AirTempSeries);
    for i := 0 to AllRecords.Count-1 do
    begin
      ADateTime := AllRecords[i];
      Dest.Add(ADateTime, True, 0, '', msNew);
      if ADateTimeList.IndexOf(ADateTime)<0 then Continue;
      MDays := DaysInMonth(ADateTime);
      TMin := 0;
      with AirTempSeries do
        AirTemp := Items[IndexOf(ADateTime)].AsFloat;
      with AirMaxSeries do
        TMax := Items[IndexOf(ADateTime)].AsFloat;
      if AirMinSeries<>nil then
        with AirMinSeries do
          TMin := Items[IndexOf(ADateTime)].AsFloat;
      Evap := HargreavesEvap(AirTemp, DegToRad(Latitude), TMax, TMin,
        MonthReprDate(ADateTime, Dest.TimeStep, MonthlyDay));
      if AirTempSeries.TimeStep = tstMonthly then
        Evap := Evap * MDays;
      Dest.Last.AsFloat := Evap;
    end;
  finally
    ATimesereriesList.Free;
    ADateTimeList.Free;
    AllRecords.Free;
  end;
end;

end.
