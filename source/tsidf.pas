{******************************************************************}
{                                                                  }
{  Thelma library                                                  }
{                                                                  }
{  Copyright (c) 2000-04 National Technical University of Athens   }
{                                                                  }
{******************************************************************}

{** IDF curves calculations - Consistent method (Koutsogiannis).}
unit tsidf;

interface

uses
  classes, ts, contnrs, statprocesses;

type
  TIDFTimeseriesType = (idftsIntensity, idftsHeight);

const
  idfcFix = 1;
  idfcContinue = 2;
  idfcAbort = 3;

type
  TCheckConsistencyHandler = function(AMessage: string): Integer of object;

type
{** TIDFTimeseries is a class containing a TTimeseries object and some
    IDF specific features.<p>
    Each IDF Timeseries is characterised by a rainfall duration expressed
    in hours. IDF Timeseries holds rainfall intensity values. If source
    timeseries is rainfall height, then values are altered in creation
    process.<p>
    You don't have normaly to use TIDFTimeseries. Use TIDFTimeseriesCollection
    class, an object list class holding TIDFTimeseries items.
    @author Stefanos
    @SeeAlso <See Class=TIDFTimeseriesCollection>
}
  TIDFTimeseries = class(TPersistent)
  private
    FDuration: Real;
    FBaseTimeStep: Real;
    FTimeseries: TTimeseries;
    FIsActiveEtaTheta, FIsActiveDataList: Boolean;
    function GetTimeResolutionFactor: Real;
  public
{** Create is the standard constructor for the TIDFTimeseries class. <p>
    Specify source timeseries with ATimeseries. A copy of the ATimeseries
    is created, so ATimeseries object could be freed. Specify with
    TimeseriesType if ATimeseries values are rainfall intensity or height.
    If source timeseries (ATimeseries) values are height, then holded values
    are intensity values calculated with Duration.<p>
    Duration is expressed in hours. BaseTimeStep is the segmentation unit
    of the rainfall measurement instrument (e.g. 10 minute, 1 hour or
    1 day) expressed in hours.<p>
    ATimeseries should be of yearly timestep or else an exception is raised.
}
    constructor Create(ATimeseries: TTimeseries;
      TimeseriesType: TIDFTimeseriesType; Duration: Real;
      BaseTimeStep: Real);
{** Use destroy to free memory holded by the TIDFTimeseries object. Do not
    use destroy directly, use Free instead.
}
    destructor Destroy; override;
{** The actual timeseries object. Values are rainfall intensity.
}
    property Timeseries: TTimeseries read FTimeseries;
{** The duration of yearly timeseries with rainfall maximums.
}
    property Duration: Real read FDuration write FDuration;
{** Base time step, the minimum segmentation unit.
}
    property BaseTimeStep: Real read FBaseTimeStep write FBaseTimeStep;
{** Returns the Time resolution correction factor for this timeseries,
    based on duration and BaseTimeStep.
    @SeeAlso <See Routine=IDFTimeResolutionFactor>
}
    property TimeResolutionFactor: Real read GetTimeResolutionFactor;
{** Set IsActiveEtaTheta to true in order to consider timeseries in Eta,
    Theta evaluation.
    IsActiveEtaTheta default value is true, set by the create method.
}
    property IsActiveEtaTheta: Boolean read FIsActiveEtaTheta write
      FIsActiveEtaTheta;
{** Set IsActiveEtaTheta to true in order to consider timeseries in data
    list evaluation (for statistical distribution fitting).
    IsActiveDataList default value is true, set by the create method.
}
    property IsActiveDataList: Boolean read FIsActiveDataList write
      FIsActiveDataList;
  end;

{** TIDFTimeseriesCollection is a collection (object list) of
    TIDFTimeseries objects.<p>
    TIDFTimeseries objects are created automatically with Add Method.<p>
    A property called Amount, returns the amount of timeseries items
    used by IDF calculations.
    @author Stefanos
    @SeeAlso <See Class=TIDFTimeseries>
    @SeeAlso <See Class=TIDFVectorCollection>
}
  TIDFTimeseriesCollection = class(TPersistent)
  private
    FIDFTimeseriesList: TObjectList;
    FDesiredAmount: Real;
    function GetCount: Integer;
    function GetAmount: Real;
    function GetMCSampleCount: Integer;
  protected
{** Returns a TIDFTimeseries object from the items array.
}
    function Get(Index: Integer): TIDFTimeseries;
{** Returns first TIDFTimeseries object.
}
    function GetFirst: TIDFTimeseries;
{** Returns last TIDFTimeseries object.
}
    function GetLast: TIDFTimeseries;
  public
{** Use create method to initialize a TIDFTimeseriesCollection object.
    Set ADesiredAmount to the desired amount for the Items used by
    IDF calculations (e.g. 0.33333, 0.5 or 1.0). Read then, Amount property
    to get the actual Amount based on DesiredAmount and Count properties
    of TIDFTimeseries objects.
    @SeeAlso <See property=Amount>
    @SeeAlso <See Method=TIDFTimeseriesCollection.Add>
}
    constructor Create(ADesiredAmount: Real);
{** Use Destroy method in order to free memory. Do not call Destroy
    directely, call Free method instead.
}
    destructor Destroy; override;
{** Count property returns the number of TIDFTimeseries object contained
    in the Items list.
}
    property Count: Integer read GetCount;
{** The actual TIDFTimeseries Items array. Items property is default,
    you may read items by AIDFTimeseriesCollection[i].
}
    property Items[Index: Integer]: TIDFTimeseries read Get; default;
{** The first TIDFTimeseries object.
}
    property First: TIDFTimeseries read GetFirst;
{** The last TIDFTimeseries object.
}
    property Last: TIDFTimeseries read GetLast;
{** The desired amount. Desired amount is set on Create Method. You may
    alter the value with this property. Read DesiredAmount in order to
    get the stored value. After setting DesiredAmount, you may read
    Amount property, namely, the actual (calculated) amount.
    @SeeAlso <See Property=Amount>
    @SeeAlso <See Method=Create>
}
    property DesiredAmount: Real read FDesiredAmount write FDesiredAmount;
{** The amount of items in TIDFTimeseries objects used in IDF calculations.<p>
    Amount is altered from ADesiredAmount set in Create method as follows:<p>
      1. Find the TIDFTimeseries Item with maximum number of items
        (MaxCount).<p>
      2. If ADesiredAmount*MaxCount>10 then Amount := ADesiredAmount.<p>
      3. If ADesiredAmount*MaxCount<10 then:<p>
        3a. Amount := 10/MaxCount if MaxCount>=10 or<p>
        3b. Amount := 1.000 if MaxCount<10<p>
    Only TIDFTimesereries with IsActiveEtaTheta set to True are considered.
    @SeeAlso <See Method=Create>
    @SeeAlso <See Property=DesiredAmount>
}
    property Amount: Real read GetAmount;
{** Use MCSampleCount in conjuction with MonteCarloLimits function
    in order to calculate confidence interval for the IDF curve.
    Only TIDFTimesereries with IsActiveDataList set to True are considered.    
    @SeeAlso <See Routine=MonteCarloLimits>
}
    property MCSampleCount: Integer read GetMCSamplecount;
{** Use Add method to add TIDFTimeseries objects in the Items array.<p>
    Add calls TIDFTimeseries.Create to initialize objects. Items list
    owns objects an destroy then on Clear or Free method call.
    @SeeAlso <See Method=TIDFTimeseries.Create>
}
    procedure Add(ATimeseries: TTimeseries;
      TimeseriesType: TIDFTimeseriesType; Duration: Real;
      BaseTimeStep: Real);
{** Use Insert method to insert TIDFTimeseries objects in the Items array at
    the specified index.<p>
    Add calls TIDFTimeseries.Create to initialize objects. Items list
    owns objects an destroy then on Clear or Free method call.
    @SeeAlso <See Method=TIDFTimeseries.Create>
}
    procedure Insert(Index: Integer; ATimeseries: TTimeseries;
      TimeseriesType: TIDFTimeseriesType; Duration: Real;
      BaseTimeStep: Real);
{** Use Clear method to empty the items array.
}
    procedure Clear;
{** Use Delete to remove a record from the items array. Specify
    the appropriate Index.
}
    procedure Delete(Index: Integer);
{** Use SingleIDFEvaluation in order to calculate the parameters of a IDF
    curves for a specified ReturnPeriod with a conventional estimation.<p>
    A Single IDF for a specified return period (Max) is desribed by a power
    law:<p>
    i = Omega/d^Eta<p>
    where, i the rainfall intensity in mm/h, d the duration in hours, Omega
    and Eta the parameters to be calculated. Set a ReturnPeriod value in
    years (e.g. 5, 50 or 2000). Specify the Statistical Distribution Type
    e.g. sdtEV1Max for Gumbel Max. Use a suitable statistical distribution
    for extreme values such as EV1Max, EV2Max or GEVMax.<p>
    If you plan to use GEVMax with k specified, the default value of k
    will be used (0.15).<p>
    Results are returned by reference to Omega, Eta and correlation factor.<p>
    Estimation of Omega and Eta is done by Least Square method,
    logarithmising d and i values.<p>
    This method is obsolete and it is included for backwards compability.
    It is recommended to use a consistent method, e.g. by calling
    IDFEtaThetaEvaluation and IDFDataList procedures.<p>
    All timeseries are considered independently the values of
    IsActiveEtaTheta, IsActiveDataList properties of each TIDFTimeseries. 
    @SeeAlso <See Routine=IDFEtaThetaEvaluation>
    @SeeAlso <See Routine=IDFDataList>
}
    procedure SingleIDFEvaluation(AStatisticalDistributionType: TStatisticalDistributionType;
      ReturnPeriod: Real; ConsiderTimeResolution: Boolean; AGEVShape: Real;
        var Omega, Eta, correlation: Real);
{** CheckConsistency checks the consistency of timeseries collections.
    Yearly intensity values in order to be consistent should decrease whith
    duration increase. Heights should increase with duration increase.
    If fails to test, an exception is raised;
}
    procedure CheckConsistency(Handler: TCheckConsistencyHandler);
  end;

{** TIDFItem is a single record (vector element) of TIDFVector objects.<p>
    TIDFItem holds for each record Value, Duration as well as Order to
    consider Koutsogiannis Method for IDF curves evalution.
    @author Stefanos
    @SeeAlso <See Class=TIDFVector>
}
  TIDFItem = class(TPersistent)
  private
    FValue: Real;
    FDuration: Real;
    FOrder: Integer;
  public
{** Create initilize record as well as set Value and Duration to
    AValue, ADuration.
    @SeeAlso <See Method=TIDFVector.Add>
}
    constructor Create(AValue, ADuration: Real);
{** Use Value in order to read or to alter value.
}
    property Value: Real read FValue write FValue;
{** Use Duration to read
}
    property Duration: Real read FDuration;
{** Order of the IDFItem within all IDFVectors to calculate ranks.<p>
    Order property is written by the Refresh method of
    TIDFVectorCollection.
    @SeeAlso <See Method=TIDFVectorCollection.Refresh>
    @SeeAlso <See Method=TIDFVectorCollection.Create>
}
    property Order: Integer read FOrder write FOrder;
  end;

{** TIDFVector is an ordered list of IDFItems of same duration.<p>
    TIDFVector stores ordered values of source Timeseries as well as
    the number of items used (nj) and mean rank (rjm).
    @author Stefanos
    @SeeAlso <See Class=TIDFItem>
    @SeeAlso <See Class=TIDFVectorCollection>
}
  TIDFVector = class(TPersistent)
  private
    FEta, FTheta: Real;
    Fnj: Integer;
    Frjm: Real;
    FConsiderTimeResolution: Boolean;
    FIDFItemsList: TObjectList;
    function GetCount: Integer;
  protected
{** Get method returns a TIDFItem of index Index.
}
    function Get(Index: Integer): TIDFItem;
  public
{** Use Create method in order to initialise a TIDFVector object.<p>
    Specify Eta and Theta parameters to transform timeseries values
    (intensity) to a "dimesionless" form of intensity. To achieve
    that, each value of timeseries is multiplied with
    IDFDurationToIntensity = b(d) = (d+theta)^eta where, d the duration in
    hours. (b(d) is the duration term in the consistent IDF curve equation:
    i=a(T)/b(d)).<p>
    Is ConsiderTimeResolution set to True, then each value of the
    timeseries is multiplied with IDFTimeResolutionFactor. This factor
    depends on the fragmentation of the duration by the base time
    unit, it is equal to 1.13 when base time unit = duration.<p>
    Finally, a sort process (desceding) is done within Items in order
    to choose nj first items to the Calculate Process.
    @SeeAlso <See Routine=IDFDurationToIntensity>
    @SeeAlso <See Routine=IDFTimeResolutionFactor>
    @SeeAlso <See Method=IDFVectorCollection.Add>
}
    constructor Create(AIDFTimeseries: TIDFTimeseries;
      AConsiderTimeResolution: Boolean; AEta, ATheta: Real);
{** Use destroy to free memory. Do not use Destroy method directely,
    use Free method instead.
}
    destructor Destroy; override;
{** Read Count: the number of elements of each TIDFVector object.
}
    property Count: Integer read GetCount;
{** The items array. You don't have to use Items property since it
    is default property. Use AIDFVector[Index] instead.
}
    property Items[Index: Integer]: TIDFItem read Get; default;
{** Use Add to Create TIDFItem objects and place them in the
    Items array. AValue should be multiplied with IDFDurationToIntensity and
    IDFTimeResolutionFactor factors before you add it with Add method.
    ADuration is expressed in hours.<p>
    Normaly you don't have to call Add method, it is used internally by
    Create method.
    @SeeAlso <See Method=TIDFItem.Create>
}
    procedure Add(AValue, ADuration: Real);
{** nj is the number of n first elements of IDFVector used by calculations.<p>
    nj is evaluated automatically by the Refresh method of TIDFVectorCollection
    class.
    @SeeAlso <See Method=TIDFVectorCollection.Refresh>
}
    property nj: Integer read Fnj write Fnj;
{** rjm is the mean rank of each TIDFVector object used by calculations.<p>
    rjm is evaluated automatically by the Refresh method of TIDFVectorCollection
    class.
    @SeeAlso <See Method=TIDFVectorCollection.Refresh>
}
    property rjm: Real read Frjm write Frjm;
{** Read Eta parameter.
}
    property Eta: Real read FEta;
{** Read Theta parameter.
}
    property Theta: Real read FTheta;
  end;

{** TIDFVectorCollection is a list of TIDFVector Items.<p>
    TIDFVectorCollection:<p>
      1. Assign multiple timeseries from TIDFTimeseriesCollection to
        TIDFVector objects<p>
      2. Calculate nj and rjm for each TIDFVector<p>
      3. Sort all items of all TIDFVector, then calculates Kruskal Wallis
        Parameter<p>
      4. Assign values to a TDataList object in order to evaluate parameters
        of statistical distribution.<p>
    TIDFVectorCollection objects are used internally by IDFEtaThetaEvaluation
    and IDFDataList routines.
    @author Stefanos
    @SeeAlso <See Class=TIDFVector>
    @SeeAlso <See Class=TIDFTimeseriesCollection>
    @SeeAlso <See Routine=IDFEtaThetaEvaluation>
    @SeeAlso <See Routine=IDFDataList>
}
  TIDFVectorCollection = class(TPersistent)
  private
    FIDFVectorList: TObjectList;
    FIDFTimeseriesCollection: TIDFTImeseriesCollection;
    FAmount: Real;
    FEta, FTheta: Real;
    FConsiderTimeResolution: Boolean;
    function GetCount: Integer;
    procedure CalcOrder;
    procedure Calcnj;
    procedure Calcrjm;
    function GetKruskalWallis: Real;
  protected
{** Get method returns a TIDFVector object of index Index.
}
    function Get(Index: Integer): TIDFVector;
  public
{** Use Create to initialise a TIDFVectorCollection object. <p>
    Create reads AIDFTimeseriesCollection object and use the Add method to
    create TIDFVector items. Only Timeseries with IsActiveEtaTheta set to True
    are added. Finally it calls Refresh method.
    @SeeAlso <See Method=Refresh>
    @SeeAlso <See Method=Add>
}
    constructor Create(AIDFTimeseriesCollection:
      TIDFTimeseriesCollection;AConsiderTimeResolution: Boolean;
        AEta, ATheta, Amount: Real);
{** Use Destroy to free memory. Do not call Destroy directly, use
    Free method instead.
}
    destructor Destroy; override;
{** Refresh is called automatically from Create method to calculate nj and
    rjm for each TIDFVector. <p> Refresh sorts desceding all items of all
    TIDFVector objects and invalidates the Order property of TIDFItem(s).
    nj calculation process:<p>
      1. Multiply Count of each IDFVector with Amount property of
        TIDFTimeseriesCollection. (nj=Count*Amount)<p>
      2. If nj<5 then:<p>
        2a. If Count>5 then nj = 5 else<p>
        2b. nj = Count
    @SeeAlso <see Method=Create>
    @SeeAlso <see Property=TIDFItem.Order>
}
    procedure Refresh;
{** The number of TIDFVector objects holded in the items list.
}
    property Count: Integer read GetCount;
{** The actual Items array. You don't have to use Items property since it
    is the default property. Use instead: AIDFVectorCollection[Index].
}
    property Items[Index: Integer]: TIDFVector read Get; default;
{** Use Add method to create and place TIDFVector in the Items array.<p>
    See Create method of TIDFVector class for parameters explanation.
    @SeeAlso <see Method=TIDFVector.Create>
}
    procedure Add(AIDFTimeseries: TIDFTimeseries;
      AConsiderTimeResolution: Boolean; Eta, Theta: Real);
{** Read KruskalWallisParameter to get the statistical parameter of
    Kruskal & Wallis.<p>
    Read property directly after Create method or after a call to Refresh
    method if you have added items with the Add method.
    @SeeAlso <see Routine=KruskalWallis>
}
    property KruskalWallisParameter: Real read GetKruskalWallis;
{** Assign a values to a TDataList object.<p>
    Existing values of ADataList are cleared after the exectution of this
    Method. You may use TDataList object within
    a TStatisticalDistribution object in order to calculate parameters of
    statistical distributions (e.g. Gumbel or GEV).<p>
    ADataList should be created before the call of the method or an Assert
    Exception is raised. After assignement of values to ADataList,
    PrepareData is called automatically in order to calculate statistical
    properties of the sample. PrepareData is called with all parameters
    set to True (Truncate to zero and Sort).<p>
    Only TIDFTimeseries with IsActiveDataList set to true are considered.
    @SeeAlso <see class=TDataList>
    @SeeAlso <see method=TDataList.PrepareData>
    @SeeAlso <see class=TStatisticalDistribution>
}
    procedure AssignDataList(ADataList: TDataList);
{** Read Eta parameter.
}
    property Eta: Real read FEta;
{** Read Theta parameter.
}
    property Theta: Real read FTheta;
  end;

{** IDFTimeResolutionFactor returns a factor to multiply rainfall intensity.<p>
    Factor depends on the fragmentation of Duration (D) for the extreme
    rainfall by the BaseTimeStep (R). Values of the Factor:<p>
      D/R=1 Factor=1.13<p>
      D/R=2 Factor=1.04<p>
      D/R=3-4 Factor=1.03<p>
      D/R=5-8 Factor=1.02<p>
      D/R=9-24 Factor=1.01<p>
    e.g. if 48h extreme rainfall intensity is evaluated by rainfall measurement
    with 24h conventional unit, a factor 1.04 should be used.<p>
    IDFTimeResolutionFactor is called automatically by the Create method
    of TIDFVector class.
    @author Stefanos
    @SeeAlso <See Method=TIDFVector.Create>
}
function IDFTimeResolutionFactor(Duration, BaseTimeStep: Real): Real;

{** The Kruskal - Wallis statistical parameter.<p>
    The parameter considers mean ranks of multiple samples and it is used
    by the Koutsogiannis method for the evaluation of IDF curves.<p>
    Parameter equation:<p>
    h=12/(m(m+1))*Sum(nj(rjm-(m+1)/2)^2) where j=1..k<p>
    rjm the mean ranks of each (j) of the k samples, m the sum
    of all nj m=Sum(nj) j=1..k and nj the number of elements of
    each (j) of the k samples.<p>
    This function is used by the KruskalWallis property of the
    TIDFVectorCollection class.
    @author Stefanos
    @SeeAlso <See property=TIDFVectorCollection.KruskalWallisParameter>
}
function KruskalWallis(const nj: array of Integer;
  const rjm: array of Real): Real;

{** IDFDurationToIntensity it is the b(d) term in the consistent
    form for the IDF curves: i=a(T)/b(d) where a(T) is the term depented
    on the statistical properties of the samples and the return period (T).
    b(d) term depends on duration expressed in hours.<p>
    b(d) = (d+Theta)^Eta, where Theta, Eta parameters.
    @author Stefanos
}
function IDFDurationToIntensity(Duration, Eta, Theta: Real): Real;

{** Use IDFEtaThetaEvaluation to estimate Eta and Theta parameters.<p>
    Parameters are estimated by test, minimizig the Kruskal Wallis
    parameter. A number of 2000 tests are considered to achieve a
    precision of 1/1000 (=0.001) on canonical grids of (Eta,Theta)
    values.<p>
    For each test, a TIDFVectorCollection object is created from
    TIDFTimeseriesCollection object assigned. Eta and Theta values are
    returned by reference within the arguments of the procedure.
    @author Stefanos
    @SeeAlso <See class=TIDFTimeseriesCollection>
    @SeeAlso <See class=TIDFVectorCollection>
    @SeeAlso <See routine=IDFDurationToIntensity>
    @SeeAlso <See routine=KruskalWallis>
}
procedure IDFEtaThetaEvaluation(AIDFTimeseriesCollection:
      TIDFTimeseriesCollection;ConsiderTimeResolution: Boolean;
        var Eta, Theta: Real); overload;

{** In this overloaded method, specify if the explicit values are used.
    Use this method to let the user specify explicit values for eta theta.
}
procedure IDFEtaThetaEvaluation(AIDFTimeseriesCollection:
      TIDFTimeseriesCollection;ConsiderTimeResolution: Boolean;
        var Eta, Theta: Real; Explicit: Boolean); overload;

{** Use IDFDataList to assign values of the merged sample in ADataList
    object.<p>
    Initialize ADataList before the call to this routine or else
    an Assert Exception is raised. Existing values of ADataList are cleared
    after the execution of the routine.<p>
    IDFDataList initialize a TIDFTimeseriesCollection object, then it use
    the AssignDataList method to assign the data list to ADataList.
    @author Stefanos
    @SeeAlso <See Method=TIDFTimeseriesCollection.AssignDataList>
}
procedure IDFDataList(AIDFTimeseriesCollection:
  TIDFTimeseriesCollection;ConsiderTimeResolution: Boolean;
    Eta, Theta: Real; ADataList: TDataList);

implementation

uses math, tsprocess, dates, sysutils;

{ TIDFTimeseries }

constructor TIDFTimeseries.Create(ATimeseries: TTimeseries;
  TimeseriesType: TIDFTimeseriesType; Duration: Real;
  BaseTimeStep: Real);
var
  i: Integer;
begin
  inherited Create;
  Assert(ATimeseries.TimeStep = tstYearly);
  FDuration := Duration;
  FBaseTimeStep := BaseTimeStep;
  FIsActiveEtaTheta := True;
  FIsActiveDataList := True;
  FTimeseries := nil;
  try
    FTimeseries := TTimeseries.Create;
    FTimeseries.Assign(ATimeseries);
    if TimeseriesType = idftsHeight then
      for i := 0 to FTimeseries.Count-1 do
        if not FTimeseries[i].IsNull then
          FTimeseries[i].AsFloat := FTimeseries[i].AsFloat / FDuration;
  except
    FTimeseries.Free;
  end;
end;

destructor TIDFTimeseries.Destroy;
begin
  FTimeseries.Free;
  inherited Destroy;
end;

function TIDFTimeseries.GetTimeResolutionFactor: Real;
begin
  Result := IDFTimeResolutionFactor(FDuration, FBaseTimeStep);
end;

{ TIDFTimeseriesCollection }

constructor TIDFTimeseriesCollection.Create(ADesiredAmount: Real);
begin
  inherited Create;
  FDesiredAmount := ADesiredAmount;
  FIDFTimeseriesList := nil;
  try
    FIDFTimeseriesList := TObjectList.Create(True);
  except
    FIDFTimeseriesList.Free;
  end;
end;

destructor TIDFTimeseriesCollection.Destroy;
begin
  FIDFTImeseriesList.Free;
  inherited Destroy;
end;

function TIDFTimeseriesCollection.GetCount: Integer;
begin
  Result := FIDFTimeseriesList.Count;
end;

function TIDFTimeseriesCollection.Get(Index: Integer): TIDFTimeseries;
begin
  Result := TIDFTImeseries(FIDFTimeseriesList.Items[Index]);
end;

function TIDFTimeseriesCollection.GetFirst: TIDFTimeseries;
begin
  Result := TIDFTImeseries(FIDFTimeseriesList.First);
end;

function TIDFTimeseriesCollection.GetLast: TIDFTimeseries;
begin
  Result := TIDFTimeseries(FIDFTimeseriesList.Last);
end;

procedure TIDFTimeseriesCollection.Add(ATimeseries: TTimeseries;
      TimeseriesType: TIDFTimeseriesType; Duration: Real;
      BaseTimeStep: Real);
var
  AIDFTimeseries: TIDFTImeseries;
begin
  AIDFTimeseries := nil;
  try
    AIDFTimeseries := TIDFTimeseries.Create(ATimeseries, TimeseriesType,
      Duration, BaseTimestep);
    FIDFTimeseriesList.Add(AIDFTimeseries);
    AIDFTimeseries := nil;
  finally
    AIDFTimeseries.Free;
  end;
end;

procedure TIDFTimeseriesCollection.Insert(Index: Integer;
      ATimeseries: TTimeseries;
      TimeseriesType: TIDFTimeseriesType; Duration: Real;
      BaseTimeStep: Real);
var
  AIDFTimeseries: TIDFTImeseries;
begin
  AIDFTimeseries := nil;
  try
    AIDFTimeseries := TIDFTimeseries.Create(ATimeseries, TimeseriesType,
      Duration, BaseTimestep);
    FIDFTimeseriesList.Insert(Index, AIDFTimeseries);
    AIDFTimeseries := nil;
  finally
    AIDFTimeseries.Free;
  end;
end;

procedure TIDFTimeseriesCollection.Clear;
begin
  FIDFTimeseriesList.Clear;
end;

procedure TIDFTimeseriesCollection.Delete(Index: Integer);
begin
  FIDFTimeseriesList.Delete(Index);
end;

function TIDFTimeseriesCollection.GetAmount: Real;
var
  i,j: Integer;
begin
  j := 0;
  for i := 0 to Count-1 do
  begin
    if not Items[i].IsActiveEtaTheta then
      Continue;
    if Items[i].Timeseries.Count>j then
      j := Items[i].Timeseries.Count;
  end;
  if Round(FDesiredAmount*j)<10 then
  begin
    if j<10 then
      Result := 1
    else
      Result := 10/j;
  end else
    Result := FDesiredAmount;
end;

procedure LSLinearFit(x, y: array of Real; var a, b, r: Real);
var
  i, n: Integer;
  SumX, SumY, SumXsq, SumYsq, SumXY: Real;
begin
  Assert(Length(x) = Length(y));
  n := Length(x);
  SumX := 0;
  SumY := 0;
  SumXsq := 0;
  SumYsq := 0;
  SumXY := 0;
  for i := 0 to n-1 do
  begin
    SumX := SumX + x[i];
    SumXsq := SumXsq + Sqr(x[i]);
    SumY := SumY + y[i];
    SumYsq := SumYsq + Sqr(y[i]);
    SumXY := SumXY + x[i]*y[i];
  end;
  r := (n*SumXY-SumX*SumY)/Sqrt( (n*SumXsq-Sqr(SumX)) * (n*SumYsq-Sqr(SumY)) );
  b := r * Sqrt( (n*SumYsq-Sqr(SumY)) / (n*SumXsq-Sqr(SumX)) );
  a := (SumY-b*SumX)/n;
end;

procedure TIDFTimeseriesCollection.SingleIDFEvaluation(AStatisticalDistributionType: TStatisticalDistributionType;
  ReturnPeriod: Real; ConsiderTimeResolution: Boolean; AGEVShape: Real;
  var Omega, Eta, correlation: Real);
var
  ADataList: TFullDataList;
  AStatisticalDistribution: TStatisticalDistribution;
  i, j, NotNullCount: Integer;
  x, y: array of Real;
  a, b: Real;
begin
  ADataList := nil;
  AStatisticalDistribution := nil;
  x := nil;
  y := nil;
  NotNullCount := 0;
  try
    ADataList := TFullDataList.Create;
    SetLength(x, Count);
    SetLength(y, Count);
    for i := 0 to Count-1 do
    begin
      ADataList.Clear;
      for j := 0 to Items[i].Timeseries.Count-1 do
      begin
        if not Items[i].Timeseries[j].IsNull then
          ADataList.AddValue(Items[i].Timeseries[j].AsFloat);
        if ConsiderTimeResolution then
          if not Items[i].Timeseries[j].IsNull then
            ADataList.Last.Value := ADataList.Last.Value *
              Items[i].TimeResolutionFactor;
      end;
      if ADataList.Count<2 then Continue;
      Inc(NotNullCount);
      ADataList.PrepareData(True, True);
      if AStatisticalDistribution=nil then
        AStatisticalDistribution :=
          TStatisticalDistribution.Create(AStatisticalDistributionType,
            ADataList, AGEVShape)
      else
        AStatisticalDistribution.Refresh;
      x[NotNullCount-1] := Ln(Items[i].Duration);
      y[NotNullCount-1] :=
        Ln(AStatisticalDistribution.InvcdfValue(1-1/ReturnPeriod));
    end;
    SetLength(x, NotNullCount);
    SetLength(y, NotNullCount);
    LSLinearFit(x, y, a, b, correlation);
    Omega := Exp(a);
    Eta := -b;
  finally
    ADataList.Free;
    AStatisticalDistribution.Free;
    SetLength(x, 0);
    SetLength(y, 0);
  end;
end;

function TIDFTimeseriesCollection.GetMCSampleCount: Integer;
var
  i,j: Integer;
begin
  Result := 0;
  j := 0;
  for i := 0 to Count-1 do
  begin
    if not Items[i].IsActiveDataList then
      Continue;
    if Items[i].Timeseries.CountNotNull >0 then
    begin
      Result := Result + Items[i].Timeseries.CountNotNull;
      Inc(j);
    end;
  end;
  if j>0 then
  Result := Round(Result/j);
end;

resourcestring
  rsInconsistentValues = 'Inconsistent values at the year: ';
  rsWithDurationsOf = ' with durations of: ';
  rsInconsistentHeight = ' (inconsistent heights)';
  rsInconsistentIntensity = ' (inconsistent intensities)';

procedure TIDFTimeseriesCollection.CheckConsistency(Handler:
  TCheckConsistencyHandler);
var
  i, j: Integer;
  AFirstIndex, ASecondIndex: Integer;
  ACommonPeriod: TDateTimeList;
  AList: TObjectList;
  ExceptionMessage: string;

  function GetDurationString(ADuration: Real): string;
  begin
    if ADuration<1 then
      Result := FormatFloat('0', ADuration*60)+''''
    else
      Result := FormatFloat('0.0', ADuration)+'h';
  end;

const
  tol = 0.015;
begin
  AList := nil;
  ACommonPeriod := nil;
  try
    AList := TObjectList.Create(False);
    for i := 0 to Count-1 do
      AList.Add(Items[i].Timeseries);
    ACommonPeriod := GetCommonPeriod(AList, 0);
    for i := 0 to ACommonPeriod.Count-1 do
    begin
      for j := 0 to Count-2 do
      begin
        ExceptionMessage := rsInconsistentValues + FormatDateTime('yyyy',
          ACommonPeriod[i])+ rsWithDurationsOf+
            GetDurationString(Items[j].Duration)+', '+
              GetDurationString(Items[j+1].Duration);
        AFirstIndex := Items[j].Timeseries.IndexOf(ACommonPeriod[i]);
        ASecondIndex := Items[j+1].Timeseries.IndexOf(ACommonPeriod[i]);
        if Items[j].Timeseries[AFirstIndex].AsFloat+tol<
          Items[j+1].Timeseries[ASecondIndex].AsFloat then
            case Handler(ExceptionMessage+rsInconsistentIntensity) of
              idfcFix: Items[j+1].Timeseries[ASecondIndex].AsFloat :=
                Items[j].Timeseries[AFirstIndex].AsFloat;
              idfcAbort: raise Exception.Create(ExceptionMessage);
            end;
        if Items[j].Timeseries[AFirstIndex].AsFloat* Items[j].Duration>(tol+
          Items[j+1].Timeseries[ASecondIndex].AsFloat)* Items[j+1].Duration then
            case Handler(ExceptionMessage+rsInconsistentHeight) of
              idfcFix: Items[j+1].Timeseries[ASecondIndex].AsFloat :=
                Items[j].Timeseries[AFirstIndex].AsFloat* Items[j].Duration/
                  Items[j+1].Duration;
              idfcAbort: raise Exception.Create(ExceptionMessage);
            end;
      end;
    end;
  finally
    AList.Free;
    ACommonPeriod.Free;
  end;
end;

{ TIDFItem }

constructor TIDFItem.Create(AValue, ADuration: Real);
begin
  inherited Create;
  FValue := AValue;
  FDuration := ADuration;
  FOrder := 0;
end;

{ TIDFVector }

{ type TListSortCompare = function (Item1, Item2: Pointer): Integer;}

{$WARN UNSAFE_CAST OFF}
{$WARN UNSAFE_TYPE OFF}

function RecordsCompare(Value1, Value2: Pointer): Integer;
begin
 if (TIDFItem(Value1).Value - TIDFItem(Value2).Value)
   < 0 then
   Result := 1
 else if (TIDFItem(Value1).Value - TIDFItem(Value2).Value)
   > 0 then
   Result := -1
 else
   Result := 0;
end;

{$WARN UNSAFE_TYPE ON}
{$WARN UNSAFE_CAST ON}

constructor TIDFVector.Create(AIDFTimeseries: TIDFTimeseries;
      AConsiderTimeResolution: Boolean; AEta, ATheta: Real);
var
  i: Integer;
begin
  inherited Create;
  FIDFItemsList := nil;
  FEta := AEta;
  FTheta := ATheta;
  FConsiderTimeResolution := AConsiderTimeResolution;
  try
    FIDFItemsList := TObjectList.Create(True);
    with AIDFTimeseries do
    begin
      for i := 0 to Timeseries.Count -1 do
        if not Timeseries[i].IsNull then Add(Timeseries[i].AsFloat,Duration);
    end;
    for i := 0 to Count-1 do
      if FConsiderTimeResolution then
        Items[i].Value := Items[i].Value * AIDFTimeseries.TimeResolutionFactor;
    for i := 0 to Count-1 do
      Items[i].Value := Items[i].Value *
        IDFDurationToIntensity(AIDFTimeseries.Duration, FEta, FTheta);
    FIDFItemsList.Sort(RecordsCompare);
  except
    FIDFItemsList.Free;
  end;
end;

destructor TIDFVector.Destroy;
begin
  FIDFItemsList.Free;
  inherited Destroy;
end;

procedure TIDFVector.Add(AValue, ADuration: Real);
var
  AIDFItem: TIDFItem;
begin
  AIDFItem := nil;
  try
    AIDFItem := TIDFItem.Create(AValue, ADuration);
    FIDFItemsList.Add(AIDFItem);
    AIDFItem := nil;
  finally
    AIDFItem.Free;
  end;
end;

function TIDFVector.GetCount: Integer;
begin
  Result := FIDFItemsList.Count;
end;

function TIDFVector.Get(Index: Integer): TIDFItem;
begin
  Result := TIDFItem(FIDFItemsList.Items[Index]);
end;

{ TIDFVectorCollection }

constructor TIDFVectorCollection.Create(AIDFTimeseriesCollection:
  TIDFTimeseriesCollection; AConsiderTimeResolution: Boolean;
    AEta, ATheta, Amount: Real);
var
  i: Integer;
begin
  inherited Create;
  FAmount := Amount;
  FEta := AEta;
  FTheta := ATheta;
  FConsiderTimeResolution := AConsiderTimeResolution;
  FIDFVectorList := nil;
  try
    FIDFVectorList := TObjectList.Create;
  except
    FIDFVectorList.Free;
  end;
  FIDFTimeseriesCollection := AIDFTimeseriesCollection;
  for i := 0 to FIDFTimeseriesCollection.Count -1 do
  begin
    if FIDFTimeseriesCollection[i].IsActiveEtaTheta then
      Add(FIDFTimeseriesCollection[i], FConsiderTimeResolution, FEta, FTheta);
  end;
  Refresh;
end;

destructor TIDFVectorCollection.Destroy;
begin
  FIDFVectorList.Free;
  inherited Destroy;
end;

procedure TIDFVectorCollection.Refresh;
begin
  Calcnj;
  CalcOrder;
  Calcrjm;
end;

procedure TIDFVectorCollection.CalcOrder;
var
  AList: TObjectList;
  i, j: Integer;
begin
  AList := nil;
  try
    AList := TObjectList.Create(False);
    for i := 0 to Count-1 do
      for j := 0 to Items[i].nj-1 do
        AList.Add(Items[i].Items[j]);
    AList.Sort(RecordsCompare);
    for i := 0 to AList.Count-1 do
      TIDFItem(AList.Items[i]).Order := i+1;
    for i := 1 to AList.Count-1 do
      if Abs(TIDFItem(AList.Items[i]).Value-
        TIDFItem(AList.Items[i-1]).Value)<0.0005 then
          TIDFItem(AList.Items[i]).Order :=
            TIDFItem(AList.Items[i-1]).Order;
  finally
    AList.Free;
  end;
end;

procedure TIDFVectorCollection.Calcrjm;
var
  i, j: Integer;
  AFloat: Real;
begin
  for i := 0 to Count-1 do
  begin
    AFloat := 0;
    for j := 0 to Items[i].nj-1 do
      AFloat := AFloat + Items[i].Items[j].Order;
    Items[i].rjm := AFloat / Items[i].nj;
  end;
end;

function TIDFVectorCollection.GetCount: Integer;
begin
  Result := FIDFVectorList.Count;
end;

function TIDFVectorCollection.Get(Index: Integer): TIDFVector;
begin
  Result := TIDFVector(FIDFVectorList.Items[Index]);
end;

procedure TIDFVectorCollection.Add(AIDFTimeseries: TIDFTimeseries;
  AConsiderTimeResolution: Boolean; Eta, Theta: Real);
var
  AIDFVector: TIDFVector;
begin
  AIDFVector := nil;
  try
    AIDFVector := TIDFVector.Create(AIDFTimeseries, AConsiderTimeResolution,
      Eta, Theta);
    FIDFVectorList.Add(AIDFVector);
    AIDFVector := nil;
  finally
    AIDFVector.Free;
  end;
end;

procedure TIDFVectorCollection.Calcnj;
var
  i: Integer;
begin
  for i := 0 to Count-1 do
    Items[i].nj := Round(Items[i].Count*FAmount);
  for i := 0 to Count-1 do
  begin
    if Items[i].nj<5 then
    begin
      if Items[i].Count >5 then
        Items[i].nj := 5 else
        Items[i].nj := Items[i].Count;
    end;
  end;
end;

function TIDFVectorCollection.GetKruskalWallis;
var
  i: Integer;
  nj: array of Integer;
  rjm: array of Real;
begin
  try
    SetLength(nj, Count);
    SetLength(rjm, Count);
    for i := 0 to Count-1 do
    begin
      nj[i] := Items[i].nj;
      rjm[i] := Items[i].rjm;
    end;
    Result := KruskalWallis(nj, rjm);
  finally
    SetLength(nj, 0);
    SetLength(rjm, 0);
  end;
end;

procedure TIDFVectorCollection.AssignDataList(ADataList: TDataList);
var
  i, j: Integer;
  AFloat: Real;
begin
  Assert(ADataList<>nil);
  ADataList.Clear;
  for i := 0 to FIDFTimeseriesCollection.Count-1 do
  begin
    if FIDFTimeseriesCollection[i].IsActiveDataList then
    begin
      for j := 0 to FIDFTimeseriesCollection[i].Timeseries.Count-1 do
      begin
          if not FIDFTimeseriesCollection[i].Timeseries[j].IsNull then
          begin
            AFloat := FIDFTimeseriesCollection[i].Timeseries[j].AsFloat*
                IDFDurationToIntensity(FIDFTimeseriesCollection[i].Duration,
                  FEta, FTheta);
            if FConsiderTimeResolution then
              AFloat := AFloat *
                FIDFTimeseriesCollection[i].TimeResolutionFactor;
            ADataList.AddValue(AFloat);
          end;
      end;
    end;
  end;
  ADataList.PrepareData(True, True);
end;

{ functions...}

function IDFTimeResolutionFactor(Duration, BaseTimeStep: Real): Real;
var
  Ratio: Integer;
begin
  Ratio := Round(Duration/BaseTimeStep);
  case Ratio of
    1: Result := 1.13;
    2: Result := 1.04;
    3..4: Result := 1.03;
    5..8: Result := 1.02;
    9..24: Result := 1.01;
  else
    Result := 1.00;
  end;
end;

function KruskalWallis(const nj: array of Integer;
  const rjm: array of Real): Real;
var
  j, m : Integer;
begin
  Assert(Length(nj)=Length(rjm));
  m := 0;
  Result := 0;
  for j := 0 to Length(nj)-1 do
    m := m+nj[j];
  for j := 0 to Length(nj)-1 do
    Result := Result + nj[j]*Sqr(rjm[j]- 0.5*(m+1));
  Result := Result *12 /(m*(m+1));
end;

function IDFDurationToIntensity(Duration, Eta, Theta: Real): Real;
begin
  Result := Power(Duration + Theta, Eta);
end;

procedure IDFSimulationSimulateOnce(
  AIDFTimeseriesCollection: TIDFTimeseriesCollection;
  ConsiderTimeResolution: Boolean; Step: real;
  var Eta, Theta: Real);
var
  href,h : Real;
  i, j: Integer;
  AIDFVectorCollection: TIDFVectorCollection;
  AEta, ATheta: Real;
  Eta0, Theta0: Real;
  AAmount: Real;
begin
  href := 1e37;
  Eta0 := Eta;
  Theta0 := Theta;
  AAmount := AIDFTimeseriesCollection.Amount;
  for i := 0 to 30 do
  begin
    AEta := Eta0 + (i-15)*Step;
    for j := 0 to 30 do
    begin
      ATheta := Theta0 + (j-15)*Step;
      AIDFVectorCollection := nil;
      try
         AIDFVectorCollection := TIDFVectorCollection.Create(
          AIDFTimeseriesCollection, ConsiderTimeResolution, AEta, ATheta,
            AAmount);
        h := AIDFVectorCollection.KruskalWallisParameter;
        if h < href then
        begin
          href := h;
          Eta := AEta;
          Theta := ATheta;
        end;
      finally
        AIDFVectorCollection.Free;
      end;
    end;
  end;
end;

procedure IDFEtaThetaEvaluation(AIDFTimeseriesCollection:
  TIDFTimeseriesCollection;ConsiderTimeResolution: Boolean;
    var Eta, Theta: Real);
begin
  IDFEtaThetaEvaluation(AIDFTimeseriesCollection, ConsiderTimeResolution,
    Eta, Theta, False);
end;

procedure IDFEtaThetaEvaluation(AIDFTimeseriesCollection:
  TIDFTimeseriesCollection;ConsiderTimeResolution: Boolean;
    var Eta, Theta: Real; Explicit: Boolean);
begin
  if Explicit then
    Exit;
  Eta := 0.5;
  Theta := 0.5;
  {Simulation in 2 stages}
  IDFSimulationSimulateOnce(AIDFTimeseriesCollection, ConsiderTimeResolution,
    (1/31), Eta, Theta);
  IDFSimulationSimulateOnce(AIDFTimeseriesCollection, ConsiderTimeResolution,
    (1/(31*31)), Eta, Theta);
end;

procedure IDFDataList(AIDFTimeseriesCollection:
  TIDFTimeseriesCollection;ConsiderTimeResolution: Boolean;
    Eta, Theta: Real; ADataList: TDataList);
var
  AIDFVectorCollection: TIDFVectorCollection;
begin
  AIDFVectorCollection := nil;
  try
    AIDFVectorCollection := TIDFVectorCollection.Create(
      AIDFTimeseriesCollection, ConsiderTimeResolution, Eta, Theta, 1);
    AIDFVectorCollection.AssignDataList(ADataList);
  finally
    AIDFVectorCollection.Free;
  end;
end;

end.
