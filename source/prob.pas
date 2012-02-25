{******************************************************************}
{                                                                  }
{  Thelma library                                                  }
{                                                                  }
{  Copyright (c) 2004   National Technical University of Athens    }
{                                                                  }
{******************************************************************}

{** Probability functions, inverted probability functions,
    parameter evaluation of probability functions.
}
unit prob;

interface

uses dkmath, math, sysutils;

{** Normalpdf returns the probability density function for the
    normal distribution, where x is the value of a random variable,
    Mi the Mean value and Sigma the standard deviation
    of the sample.
    @SeeAlso <See Routine=Normalcdf>
}
function Normalpdf (x, Mi, Sigma : Real): Real;
{** Normalcdf returns is the cumulative distribution
    function for the normal distribution, that is
    the probability of not exceeding the
    value of x. Mi is the Mean value and Sigma is the
    standard deviation of the sample.
    @SeeAlso <See Routine=Normalexpr>
    @SeeAlso <See Routine=Normalpdf>
    @SeeAlso <See Routine=NormalLParam>    
}
function Normalcdf (x, Mi, Sigma : Real): Real;
{** Normalexpr returns the exceedence probability for
    the value x of a random variable. Normalexpr is defined
    as 1 - Normalcdf. Mi is the Mean value and Sigma is
    the standard deviation of the sample.
    @SeeAlso <See Routine=Normalcdf>
}
function Normalexpr (x, Mi, Sigma : Real): Real;
{** InvNormalcdf returns the inverted cumulative distribution
    function for the normal distribution, that is the
    value of a random variable that does not exceeded
    for a probability value of F. Mi is the Mean value and
    Sigma is the standard deviation of the sample. Calculations
    are made with Newton method.
    @SeeAlso <See Routine=Normalcdf>
    @SeeAlso <See Routine=InvNormalexpr>
}
function InvNormalcdf (F, Mi, Sigma : Real): Real;
{** InvNormalexpr returns the inverted cumulative exceedence
    distribution function for the normal distribution, that is
    the value of a random variable that is exceeded with a
    probability of F1 (where F1 = 1 - F). Mi is the Mean
    value and Sigma is the standard deviation of the sample.
    Calculations are made with newton method.
    @SeeAlso <See Routine=Normalexpr>
    @SeeAlso <See Routine=InvNormalcdf>
}
function InvNormalexpr (F1, Mi, Sigma : Real): Real;
{** Calculates Normal distribution parameters by L-Moments method.<p>
    Set L1, L2, L3 with L-Moments. However, only L1 and L2 are used.
    Results are returned by reference to Mi and Sigma, namely
    the mean value and the standard deviation.
}
procedure NormalLParam (L1, L2, L3: Real; var Mi, Sigma: Real);
{** LogNormal2m return the cumulative distribution function of
    a 2 parameters Log-Normal distribution, that is the
    probability of not exceeding the value x of a random
    variable. Mi is the Mean value and Sigma the standard
    deviation of the sample.
    @SeeAlso <See Routine=InvLogNormal2m>
    @SeeAlso <See Routine=LogNormalParam>
}
function LogNormal2m (x, Mi, Sigma : Real) : Real;
{** InvLogNormal2m returns the inverted cumulative distribution
    function of a 2 parameters Log-Normal distribution, that
    is the value of a random variable that does not exceeded
    for a probability value of F. Mi is the Mean value and
    Sigma the standard deviation of the sample.
    @SeeAlso <See Routine=LogNormal2m>
}
function InvLogNormal2m (F, Mi, Sigma : Real) : Real;
{** LogNormalcdf return the cumulative distribution function of
    a 2 parameters Log-Normal distribution, that is the
    probability of not exceeding the value x of a random
    variable. Use Log-Normal parameters miy, sigmay.
    @SeeAlso <See Routine=LogNormal2m>
    @SeeAlso <See Routine=InvLogNormalcdf>
    @SeeAlso <See Routine=LogNormalParam>
}
function LogNormalcdf( x, miy, sigmay: Real): Real;
{**
}
function LogNormalpdf( x, miy, sigmay: Real): Real;
{** InvLogNormal2m returns the inverted cumulative distribution
    function of a 2 parameters Log-Normal distribution, that
    is the value of a random variable that does not exceeded
    for a probability value of F. Use Log-Normal parameters miy, sigmay.
    @SeeAlso <See Routine=InvLogNormal2m>
    @SeeAlso <See Routine=LogNormalcdf>
    @SeeAlso <See Routine=LogNormalParam>
}
function InvLogNormalcdf( F, miy, sigmay: Real): Real;
{** Calculates Log Normal distribution parameters with moments method.<p>
    Set statistical parameters to Mean and StandardDeviation.
    Log-Normal parameters are returned by reference to miy and sigmay values.
    @SeeAlso <See Routine=LogNormal2m>
}
procedure LogNormalParam(Mean, StandardDeviation: Real;
  var miy, sigmay: Real);
{** Galtoncdf returns the cumulative distribution function of a 3
    parameters Log-Normal (Galton) distribution, that is the
    probability of not exceeding the value x of a random
    variable. Miy (Scale), Sigmay (Shape), c (Position) are the
    Galton parameters (for description see Koytsogiannhs book:
    Statistical hydrology).
    @SeeAlso <See Routine=InvGaltoncdf>
    @SeeAlso <See Routine=GaltonParam>    
}
function Galtoncdf (X, Miy, Sigmay, c: Real): Real;
{**
}
function Galtonpdf (X, Miy, Sigmay, c: Real): Real;
{** InvGaltoncdf returns the inverted cumulative distribution function
    for the Galton distribution, that is the value of a random
    variable that does not exceeded for a probability value of F.
    Miy (Scale), Sigmay (Shape), c (Position) are the
    Galton parameters (for description see Koytsogiannhs book:
    Statistical hydrology).
    @SeeAlso <See Routine=Galtoncdf>
}
function InvGaltoncdf (F, Miy, Sigmay, c: Real): Real;
{** Calculates Galton distribution parameters with moments method.<p>
    Set statistical parameters to Mean, StandardDeviation and
    AsymetryFactor parameters. Galton parameters are returned
    by reference to c, miy and sigmay values.
    @SeeAlso <See Routine=Galtoncdf>
}
procedure GaltonParam(Mean, StandardDeviation, AsymetryFactor: Real;
  var c, miy, sigmay: Real);
{** Exponentialpdf returns the Probability Density Function for
    the Exponential distribution.
}
function Exponentialpdf(X, Lambda, Psi: Real): Real;
{** Exponentialcdf returns the cumulative distribution function
    for the Exponential distribution, that is the probability of
    not exceeding the value x of a random variable.
    Lambda is the Scale factor and Psi the Position.
    (see Koytsogiannis book - Statistical Hydrology)
    @SeeAlso <See Routine=InvExponentialcdf>
    @SeeAlso <See Routine=ExponentialParam>
    @SeeAlso <See Routine=ExponentialLParam>
}
function Exponentialcdf(X, Lambda, Psi: Real): Real;
{** InvExponentialcdf returns the inverted cumulative distribution
    function for the Exponential distribution, that is the value
    of a random variable that does not exceeded for a probability
    value of F. Lambda is the Scale factor and Psi the Position.
    @SeeAlso <See Routine=Exponentialcdf>
}
function InvExponentialcdf (F, Lambda, Psi: Real): Real;
{** Calculate exponential distribution parameters by the Moment method.<p>
    Set mean value and standard deviationto Mi and Sigma respectively.
    Results are returned by reference to c and Lambda parameters.
    @SeeAlso <See Routine=Exponentialcdf>
}
procedure ExponentialParam( Mi, Sigma: Real; var c, Lambda: Real);
{** Calculate exponential distribution parameters by the L-Moments method.<p>
    Set L-Moment 1,2,3 to L1 - L3. However, only the L1 and L2 are
    used.
    Results are returned by reference to c and Lambda parameters.
    @SeeAlso <See Routine=Exponentialcdf>
}
procedure ExponentialLParam( L1, L2, L3: Real; var c, Lambda: Real);
{** Gammapdf returns the probability density function
    for the Gamma distribution, where x is a random variable.
    Kappa is the Shape factor and Lambda the Scale factor.
    @SeeAlso <See Routine=Gammacdf>
}
function Gammapdf (x, Kappa, Lambda : Real) : Real;
{** Gammacdf returns the cumulative distribution function
    for the Gamma distribution, that is the probability of not
    exceeding the value x of a random variable.
    Kappa is the Shape factor and Lambda the Scale factor.
    @SeeAlso <See Routine=Gammapdf>
    @SeeAlso <See Routine=InvGammacdf>
}
function Gammacdf (x, Kappa, Lambda : Real) : Real;
{** InvGammacdf returns the inverted cumulative distribution
    function for the Gamma distribution, that is the value of
    a random variable that does not exceeded for a probability
    value of F. Kappa is the the Shape factor and Lambda
    the Scale factor.
}
function InvGammacdf(g, Kappa, Lambda : Real) : Real;
{** GammaShape calculates the Shape factor (Kappa) of the Gamma distribution
    by the terms of Mean value and Variance (stddev^2).
}
function GammaShape (Mean, Variance : Real) : Real;
{** GammaScale calculates the Scale factor (Lambda) of the Gamma distribution
    by the terms of Mean value and Variance (stddev^2).
}
function GammaScale (Mean, Variance : Real) : Real;
{** GammaMean calculates the Mean value for the Gamma distribution
    by the terms of Shape (Kappa) and Scale (Lambda) factors.
}
function GammaMean (Shape, Scale : Real) : Real;
{** GammaVariance calculates the Variance (stdev^2) for the Gamma distribution
    by the terms of Shape (Kappa) and Scale (Lambda) factors.
}
function GammaVariance (Shape, Scale : Real) : Real;
{** The X^2 cdf, usefull distribution function for statistical tests.
    For the test purposes the inverted function may be more usefull.
    X^2 is calculated by the Gamma cdf by using n/2 as the shape parameter
    and 1/2 as the scale parameter, where n the degrees of freedom.
    @SeeAlso <See Routine=InvXSquareCdf>
    @SeeAlso <See Routine=Gammacdf>
}
function XSquareCdf(x: Real; DegreesOfFreedom: Integer): Real;
{** The inverted X^2 cdf, usefull for statistical tests.
    @SeeAlso <See Routine=XSquareCdf>
    @SeeAlso <See Routine=Gammacdf>
}
function InvXSquareCdf(u: Real; DegreesOfFreedom: Integer): Real;
{** PearsonIIIcdf returns the cumulative distribution function for
    the PearsonIII distribution (3 parameters Gamma distribution).
    X-Psi follows the Gamma distribution. See Koytsogiannhs book
    for definition. Kappa: shape factor, Lambda: scale and
    Psi: Position.
    @SeeAlso <See Routine=PearsonIIIParam>
    @SeeAlso <See Routine=Gammacdf>
    @SeeAlso <See Routine=InvPearsonIIIcdf>
    @SeeAlso <See Routine=LogPearsonIIIcdf>
}
function PearsonIIIcdf (X, Kappa, Lambda, Psi: Real): Real;
{**
}
function PearsonIIIpdf (X, Kappa, Lambda, Psi: Real): Real;
{** InvPearsonIIIcdf returns the inverted cumulative distribution
    function for the PearsonIII distribution.
    @SeeAlso <See Routine=PearsonIIIcdf>
}
function InvPearsonIIIcdf (F, Kappa, Lambda, Psi: Real): Real;
{** PearsonIII parameters by Moments method.<p>
    @SeeAlso <See Routine=PearsonIIIcdf>
}
procedure PearsonIIIParam(Mean, StandardDeviation, AsymetryFactor: Real;
  var Kappa, Lambda, c: Real);
{** LogPearsonIIIcdf returns the cumulative distribution function
    for the LogPearsonIII distribution. X is following LPIII when
    Ln(X) is Following PerasonIII distribution. See PearsonIIIcdf
    help for more.
    @SeeAlso <See Routine=Gammacdf>
    @SeeAlso <See Routine=PearsonIIIcdf>
    @SeeAlso <See Routine=InvLogPearsonIIIcdf>
}
function LogPearsonIIIcdf (X, Kappa, Lambda, Psi: Real): Real;
{**
}
function LogPearsonIIIpdf (X, Kappa, Lambda, Psi: Real): Real;
{** InvLogPearsonIIIcdf returns the inverted cumulative distribution
    function for the LogPearsonIII distribution.
    @SeeAlso <See Routine=LogPearsonIIIcdf>
}
function InvLogPearsonIIIcdf (F, Kappa, Lambda, Psi: Real): Real;
{** Betapdf returns the probability density function of the
    Beta distribution, where x is a random variable.
}
function Betapdf (x, Mi, Ni: Real) :Real ;
{** BetaMi returns the Mi parameter of the Beta distribution.
}
function BetaMi (Mean, Variance : Real) : Real;
{** BetaNi returns the Ni parameter of the Beta distribution.
}
function BetaNi (Mean, Variance : Real) : Real;
{** BetaMean returns the Mean value for the Beta distribution.
}
function BetaMean (Mi, Ni : Real) : Real;
{** BetaMean returns the Variance for the Beta distribution.
}
function BetaVariance (Mi, Ni : Real) : Real;
{** EV1Maxcdf returns the cumulative distribution function for the
    EV1Max (Gumbel Max) distribution. Lambda is the scale factor and
    Psi the Position parameter. See Koytsogiannhs notes:
    1999AdvHydroProb.pdf for definitions.<p>
    F := Exp (- Exp (X/Lambda + Psi) )
    @SeeAlso <See Routine=InvEV1Maxcdf>
    @SeeAlso <See Routine=EV1Mincdf>
    @SeeAlso <See Routine=GEVMaxcdf>
    @SeeAlso <See Routine=EV1MaxParam>
    @SeeAlso <See Routine=EV1MaxLParam>
}
function EV1Maxcdf(X, Lambda, Psi: Real): Real;
{** EV1 - max (Gambel Max), PDF
}
function EV1Maxpdf (X, Lambda, Psi: Real): Real;
{** InvEV1Maxcdf returns the inverted cumulative distribution function for
    the EV1MAX (Gumbel Max) distribution.
    @SeeAlso <See Routine=EV1Maxcdf>
}
function InvEV1Maxcdf(F, Lambda, Psi: Real): Real;
{** Calculates EV1 Max (Gumbel Max) parameters by the Moments method.<p>
    Set mean value and standard deviation to Mean and Sigma parameters.
    Results are returned by reference to Lambda and Psi.
    @SeeAlso <See Routine=EV1Maxcdf>
}
procedure EV1MaxParam(Mi, Sigma: Real; var Lambda, Psi: Real);
{** Calculates EV1 Max (Gumbel Max) parameters by the L-Moments method.<p>
    Set L1, L2, L3 with L-Moments. However, only L1 and L2 are used.
    Results are returned by reference to Lambda and Psi.
    @SeeAlso <See Routine=EV1Maxcdf>
}
procedure EV1MaxLParam(L1, L2, L3: Real; var Lambda, Psi: Real);
{** EV1Mincdf returns the cumulative distribution function for the
    EV1Min (Gumbel Min) distribution. Lambda is the scale factor and
    Psi the Position parameter. See Koytsogiannhs notes:
    1999AdvHydroProb.pdf for definitions.<p>
    F := Exp (- Exp (X/Lambda + Psi) )
    @SeeAlso <See Routine=InvEV1Mincdf>
    @SeeAlso <See Routine=EV1MinParam>
    @SeeAlso <See Routine=EV1MinLParam>
    @SeeAlso <See Routine=EV1Maxcdf>
    @SeeAlso <See Routine=GEVMincdf>
}
function EV1Mincdf(X, Lambda, Psi: Real): Real;
{** EV1 Min PDF.
}
function EV1Minpdf(X, Lambda, Psi: Real): Real;
{** InvEV1Mincdf returns the cumulative distribution function for the
    EV1Min (Gumbel Min) distribution.
    @SeeAlso <See Routine=EV1Mincdf>
}
function InvEV1Mincdf(F, Lambda, Psi: Real): Real;
{** Calculates EV1 Min (Gumbel Min) parameters by the Moments method.<p>
    Set mean value and standard deviation to Mean and Sigma parameters.
    Results are returned by reference to Lambda and Psi.
    @SeeAlso <See Routine=EV1Mincdf>
}
procedure EV1MinParam(Mi, Sigma: Real; var Lambda, Psi: Real);
{** Calculates EV1 Min (Gumbel Min) parameters by the L-Moments method.<p>
    Set L1, L2, L3 with L-Moments. However, only L1 and L2 are used.
    Results are returned by reference to Lambda and Psi.
    @SeeAlso <See Routine=EV1Mincdf>
}
procedure EV1MinLParam(L1, L2, L3: Real; var Lambda, Psi: Real);
{** EV2Maxcdf returns the cumulative distribution function for the
    two parameters EV2Max distribution, that is the probability of not
    exceeding the value X of a random variable. EV2Max distribution is
    described by Koytsogiannhs in 1999AdvHydroProb.pdf notes.
    @SeeAlso <See Routine=InvEV2Maxcdf>
    @SeeAlso <See Routine=GevMaxcdf>
    @SeeAlso <See Routine=EV2MaxParam>
    @SeeAlso <See Routine=EV2MaxLParam>
}
function EV2Maxcdf(X, Kappa, Lambda: Real): Real;
{** EV2Max PDF
}
function EV2Maxpdf(X, Kappa, Lambda: Real): Real;
{** InvEV2MAXcdf returns the inverted cumulative distribution function
    for the thow parameters EV2Max distribution.
    @SeeAlso <See Routine=EV2Maxcdf>
}
function InvEV2Maxcdf(F, Kappa, Lambda: Real): Real;
{** EVMAXParam returns the Shape and Scale parameters for the
    EV2Max (2p) distribution with the accurate method (iterative solution).
    EV2Max is beeing called by reference to the Shape and Scale parameters
    where the results are returned. Variance = stddev^2.
    @SeeAlso <See Routine=EV2MaxAproxParam>
    @SeeAlso <See Routine=EV2Maxcdf>
}
procedure EV2MaxParam (Mean, Variance: Real;
  var Shape, Scale: Real);
{** EV2MaxAproxParam returns the Shape, Scale parameters for the EV2Max
    (2p) distribution with an aproximate method according Koytsogiannhs.
    @SeeAlso <See Routine=EV2MaxParam>
}
procedure EV2MaxAproxParam (Mean, Variance: Real;
  var Shape, Scale: Real);
{** Calculates the EV2-Max parameters by the L-Moments method.<p>
    Set L1, L2, L3 with L-Moments. However, only L1 and L2 are used.
    Results are returned by reference to Kappa and Lambda.
    @SeeAlso <See Routine=EV2Maxcdf>
}
procedure EV2MaxLParam(L1, L2, L3: Real; var Kappa, Lambda: Real);
{** Weibpdf returns the probability density function of the
    Weibull (EV3-MIN) distribution, where x is a random
    variable. Kappa is the Shape factor and Lambda the
    Scale factor. The Weibpdf equation is included in
    Koytsogiannhs notes: 1999AdvHydroProb.pdf
    @SeeAlso <See Routine=Weibcdf>
}
function Weibpdf (x, Kappa, Lambda : Real) : Real;
{** Weibcdf returns the cumulative distribution function for the
    Weibull (two parameters EV3-MIN) distribution,
    that is the probability of not
    exceeding the value x of a random variable.
    Kappa is the Shape factor and Lambda the Scale factor
    respectivelly. The Weibcdf equation is included in
    Koytsogiannhs notes: 1999AdvHydroProb.pdf
    @SeeAlso <See Routine=InvWeibcdf>
    @SeeAlso <See Routine=Weibpdf>
    @SeeAlso <See Routine=WeibParam>
    @SeeAlso <See Routine=WeibLParam>
}
function Weibcdf (x, Kappa, Lambda : Real) : Real;
{** InvWeibcdf returns the inverted cumulative distribution
    function for the Weibull (EV3-MIN) distribution, that is the value of
    a random variable that does not exceeded for a probability
    value of F. Kappa is the Shape factor and Lambda the Scale
    factor.
    @SeeAlso <See Routine=Weibcdf>
}
function InvWeibcdf (F, Kappa, Lambda: Real) : Real;
{** WeibParam calculates the Shape and Scale factors for the Weibull (EV3-MIN)
    distribution with the accurate method. Shape and Scale factors
    are returned by reference and a initial value for these parameters
    must be given before a call to this function. For example set
    Shape := 0.2, Scale := 2, then call: WeibParam(3,4,Shape,Scale).
    Now Shape and Scale has been calculated with an iterative solution.
    @SeeAlso <See Routine=WeibAproxParam>
    @SeeAlso <See Routine=Weibcdf>
}
procedure WeibParam (Mean, Variance :Real;
                     var Shape, Scale : Real);
{** WeibAproxParam calculates the Shape and Scale factors for the Weibull
    distribution with an aproximate method according Koytsigiannhs.
    @SeeAlso <See Routine=WeibParam>
}
procedure WeibAproxParam (Mean, Variance: Real;
                          var Shape, Scale: Real);
{** Calculates the Weibull (EV3-Min) parameters by the L-Moments method.<p>
    Set L1, L2, L3 with L-Moments. However, only L1 and L2 are used.
    Results are returned by reference to Kappa and Lambda.
    @SeeAlso <See Routine=Weibcdf>
}
procedure WeibLParam (L1, L2, L3: Real; var Kappa, Lambda: Real);
{** WeibMean returns the Mean value for the Weibull distribution.
}
function WeibMean (Shape, Scale : Real) : Real;
{** WeibMean returns the Variance for the Weibull distribution.
}
function WeibVariance (Shape, Scale : Real) : Real;
{** GEVMaxParam returns the Shape, Scale and Position parameters for the
    GEV-MAX distribution by the accurate method (iterrative solution).
    GEVMaxParam is beeing called by reference to the Shape, Scale and
    Position parameters where the results are returned.
    @SeeAlso <See Routine=GEVMaxParamKS>
    @SeeAlso <See Routine=GEVMaxAproxParam>
    @SeeAlso <See Routine=GEVMaxLParam>
    @SeeAlso <See Routine=GEVMaxcdf>
}
procedure GEVMaxParam (MeanValue, StandardDeviation, AsymetryFactor: Real;
  var Shape, Scale, Position: Real);
{** GEVMaxParamKS returns Scale and Position by using a specified
    shape parameter (k). You should set a value for k before you
    call GEVMaxParamKS. If shape is out of range of 0.001-0.50
    the procedure raise exception. A default value of k=0.15
    may be considered.
    @SeeAlso <See Routine=GEVMaxParam>
}
procedure GEVMaxParamKS (MeanValue, StandardDeviation, Shape: Real;
  var Scale, Position: Real);
{** GevMaxAproxParam calcualtes Shape, Scale and Position parameters for the
    GEV-Max distribution by an aproximate method according Koytsogiannhs.
    @SeeAlso <See Routine=GEVMaxParam>
}
procedure GEVMaxAproxParam (MeanValue, StandardDeviation, AsymetryFactor: Real;
  var Shape, Scale, Position: Real);
{** GEVMaxLParam returns the Shape, Scale and Position parameters for the
    GEV-MAX distribution by the accurate method (iterrative solution) using
    the L-Moments L1, L2 an L3.
    GEVMaxLParam is beeing called by reference to the Shape, Scale and
    Position parameters where the results are returned.
    @SeeAlso <See Routine=GEVMaxParam>
    @SeeAlso <See Routine=GEVMaxcdf>
    @SeeAlso <See Routine=GEVMaxLParamKS>
}
procedure GEVMaxLParam (L1,L2,L3: Real; var Shape, Scale, Position: Real);
{** GEVMaxLParamKS returns Scale and Position by using a specified
    shape parameter (k). You should set a value for k before you
    call GEVLMaxParamKS. If shape is out of range of 0.001-0.50
    the procedure raise exception. A default value of k=0.15
    may be considered.
    @SeeAlso <See Routine=GEVMaxLParam>
}
procedure GEVMaxLParamKS (L1, L2, L3, Shape: Real;
  var Scale, Position: Real);
{** GEVMAXcdf returns the cumulative distribution function for the
    GEVMAX distribution, that is the probability of not
    exceeding the value X of a random variable. GEVMAX distribution is
    described by Koytsogiannhs in 1999AdvHydroProb.pdf notes.
    @SeeAlso <See Routine=InvGEVMaxcdf>
    @SeeAlso <See Routine=GEVMaxParam>
    @SeeAlso <See Routine=GEVMaxLParam>
}
function GEVMaxcdf (X, Kappa, Lambda, Psi: Real): Real;
{** GEV Max pdf
}
function GEVMaxpdf (X, Kappa, Lambda, Psi: Real): Real;
{** InvGEVMaxcdf returns the inverted cumulative distribution function for the
    GEVMax distribution.
    @SeeAlso <See Routine=GEVMaxcdf>
}
function InvGEVMaxcdf(F, Kappa, Lambda, Psi: Real): Real;
{** GEVMinParam returns the Shape, Scale and Position parameters for the
    GEV-MIN distribution by the accurate method (iterrative solution).
    GEVMinParam is beeing called by reference to the Shape, Scale and
    Position parameters where the results are returned.
    @SeeAlso <See Routine=GEVMinParamKS>
    @SeeAlso <See Routine=GEVMinAproxParam>
    @SeeAlso <See Routine=GEVMinLParam>
    @SeeAlso <See Routine=GEVMincdf>
}
procedure GEVMinParam (MeanValue, StandardDeviation, AsymetryFactor: Real;
  var Shape, Scale, Position: Real);
{** GEVMinParamKS returns Scale and Position by using a specified
    shape parameter (k). You should set a value for k before you
    call GEVMinParamKS. If shape is out of range of 0.001-0.50
    the procedure raise exception. A default value of k=0.15
    may be considered.
    @SeeAlso <See Routine=GEVMinParam>
}
procedure GEVMinParamKS (MeanValue, StandardDeviation, Shape: Real;
  var Scale, Position: Real);
{** GevMinParam calcualtes Shape, Scale and Position parameters for the
    GEV-Min distribution by an aproximate method according Koytsogiannhs.
    @SeeAlso <See Routine=GEVMinParam>
}
procedure GEVMinAproxParam (MeanValue, StandardDeviation, AsymetryFactor: Real;
  var Shape, Scale, Position: Real);
{** GEVMinLParam returns the Shape, Scale and Position parameters for the
    GEV-MIN distribution by the accurate method (iterrative solution) using
    the L-Moments L1, L2 an L3.
    GEVMinLParam is beeing called by reference to the Shape, Scale and
    Position parameters where the results are returned.
    @SeeAlso <See Routine=GEVMinParam>
    @SeeAlso <See Routine=GEVMincdf>
    @SeeAlso <See Routine=GEVMinLParamKS>
}
procedure GEVMinLParam (L1,L2,L3: Real; var Shape, Scale, Position: Real);
{** GEVMinLParamKS returns Scale and Position by using a specified
    shape parameter (k). You should set a value for k before you
    call GEVLMinParamKS. If shape is out of range of 0.001-0.50
    the procedure raise exception. A default value of k=0.15
    may be considered.
    @SeeAlso <See Routine=GEVMinLParam>
}
procedure GEVMinLParamKS (L1, L2, L3, Shape: Real;
  var Scale, Position: Real);
{** GEVMINcdf returns the cumulative distribution function for the
    GEVMIN distribution, that is the probability of not
    exceeding the value X of a random variable. GEVMIN distribution is
    described by Koytsogiannhs in 1999AdvHydroProb.pdf notes.
    @SeeAlso <See Routine=InvGEVMincdf>
    @SeeAlso <See Routine=GEVMinParam>
    @SeeAlso <See Routine=GEVMinLParam>
}
function GEVMincdf (X, Kappa, Lambda, Psi: Real): Real;
{** GEV Min pdf.
}
function GEVMinpdf (X, Kappa, Lambda, Psi: Real): Real;
{** InvGEVMincdf returns the inverted cumulative distribution function for the
    GEVMin distribution.
    @SeeAlso <See Routine=GEVMincdf>
}
function InvGEVMincdf (F, Kappa, Lambda, Psi: Real): Real;
{** Paretocdf returns the cumulative distribution function for the
    Pareto distribution, that is the probability of not
    exceeding the value X of a random variable.
    @SeeAlso <See Routine=InvParetocdf>
    @SeeAlso <See Routine=ParetoParam>
    @SeeAlso <See Routine=ParetoLParam>
}
function Paretocdf (X, Kappa, Lambda, Psi: Real): Real;
{** Pareto PDF.
}
function Paretopdf (X, Kappa, Lambda, Psi: Real): Real;
{** InvParetocdf returns the inverted cumulative distribution function
    for the pareto distribution.
    @SeeAlso <See Routine=Paretocdf>
}
function InvParetocdf (F, Kappa, Lambda, Psi: Real): Real;
{** ParetoParam returns the Shape, Scale and Position parameters for the
    Pareto distribution by the accurate method (iterrative solution).
    ParetoParam is beeing called by reference to the Shape and Scale
    parameters where the results are returned.
    @SeeAlso <See Routine=ParetoLParam>
    @SeeAlso <See Routine=Paretocdf>
}
procedure ParetoParam(MeanValue, StandardDeviation, AsymetryFactor: Real;
  var Shape, Scale, Position: Real);
{** ParetoLParam returns the Shape, Scale and Position parameters for the
    PAreto distribution by the accurate method (iterrative solution) using
    the L-Moments L1, L2 an L3.
    ParetoParam is beeing called by reference to the Shape and Scale
    parameters where the results are returned.
    @SeeAlso <See Routine=ParetoParam>
    @SeeAlso <See Routine=Paretocdf>
}
procedure ParetoLParam(L1, L2, L3: Real; var Shape, Scale, Position: Real);
{** Probability function of an ordered sample, calculated by
    the Weibull empirical distribution.<p>
    Order should be an integer (1..Count): 1 for the smaller value of the
    sample. Count is the number of measurments for the sample.
    (Thus, sample is sorted ascending).
}
function WeibullEmpirical(Order, Count: Integer): Real;
{** Probability function of an ordered sample, calculated by
    the Blom empirical distribution.<p>
    Order should be an integer (1..Count): 1 for the smaller value of the
    sample. Count is the number of measurments for the sample.
    (Thus, sample is sorted ascending).
}
function BlomEmpirical(Order, Count: Integer): Real;
{** Probability function of an ordered sample, calculated by
    the Cunnane empirical distribution.<p>
    Order should be an integer (1..Count): 1 for the smaller value of the
    sample. Count is the number of measurments for the sample.
    (Thus, sample is sorted ascending).
}
function CunnaneEmpirical(Order, Count: Integer): Real;
{** Probability function of an ordered sample, calculated by
    the Gringorten empirical distribution.<p>
    Order should be an integer (1..Count): 1 for the smaller value of the
    sample. Count is the number of measurments for the sample.
    (Thus, sample is sorted ascending).
}
function GringortenEmpirical(Order, Count: Integer): Real;

implementation
const Sqrt2 = 1.414213562;

{Normal distribution family}

resourcestring
  rsNegativeStdDevNormalpdf = 'Normal distribution probability density '+
   'function - Negative standard deviation';
  rsNegativeStdDevNormalcdf = 'Normal distribution cumulative function - '+
    'Negative standard deviation';
  rsNegativeStdDevNormalexprcdf = 'Normal distribution exceedence probability '+
   'cumulative function - Negative standard deviation';
  rsInvalidArgumentsInvNormal = 'Inverted normal distribution cumulative'+
    ' function - Invalid arguments';
  rsInvalidArgumentsInvNormalexpr = 'Inverted normal exceedence probability'+
    ' cumulative function - Invalid arguments';
  rsInvalidGaltonParameters = 'Invalid galton parameters';
  rsInvaliedInvertedGaltonParameters = 'Invalid inverted galton parameters';  

{ Συνάρτηση πυκνότητας πιθανότητας κανονικής κατανομής
  (Normal probability density function) }
function normalpdf (x, Mi, Sigma : Real): Real;
begin
  if Sigma <= 0 then
    begin
      raise EInvalidArgument.Create(rsNegativeStdDevNormalpdf);
    end;
    x := (x - Mi)/Sigma;
    normalpdf := exp(-Sqr(x) / 2) / Sigma / Sqrt (2 * pi);
end;

{ Συνάρτηση κανονικής κατανομής (Normal distribution function)}
function normalcdf (x, Mi, Sigma : Real): Real;
  begin
  if Sigma <= 0 then
    begin
      raise EInvalidArgument.Create(rsNegativeStdDevNormalcdf);
    end;
  x := (x - Mi)/Sigma;
  if x >= 0 then normalcdf := 0.5 + erf(x / Sqrt2) / 2
  else normalcdf := normalexpr (-x, 0, 1);
  end;

{ Πιθανότητα υπέρβασης κανονικής κατανομής
 (Exceedence probability for normal distribution)}
function normalexpr (x, Mi, Sigma : Real): Real;
  begin
  if Sigma <= 0 then
    begin
    raise EInvalidArgument.Create(rsNegativeStdDevNormalexprcdf);
    end;
  x := (x - Mi)/Sigma;
  if x >= 0 then normalexpr := erfc (x / Sqrt2) / 2
  else normalexpr := normalcdf (-x, 0, 1);
  end;

{ Αντίστροφη της κανονικής συνάρτησης κατανομής
  (Inverted Normal distribution function) }
{ Υπολογισμός με τη μέθοδο Newton }
function InvNormalcdf (F, Mi, Sigma : Real): Real;
  const
    accuracy = 1.0E-8;
    MaxStep  = 0.5;
  var
    x, lastx : Real;
    ACount: Integer;
  begin
  if (F <= 0) or (F >= 1) then
    begin
    raise EInvalidArgument.Create(rsInvalidArgumentsInvNormal);
    end;
  x := Mi;
  ACount := 0;
  repeat
    lastx := x;
    Inc(ACount);
    x := x - (normalcdf(x, Mi, Sigma) - F) / normalpdf(x, Mi, Sigma);
    until (abs(x-lastx) < accuracy) or (ACount>25);
  InvNormalcdf := x;
  end;

{ Αντίστροφη συνάρτηση της πιθανότητας υπέρβασης κανονικής κατανομής
  (Inverted function of the exceedence probability for normal distribution) }
{ Υπολογισμός με τη μέθοδο Newton }
function InvNormalexpr (F1, Mi, Sigma : Real): Real;
  const
    accuracy = 1.0E-8;
    MaxStep  = 0.5;
  var
    x, lastx : Real;
    ACount: Integer;
  begin
  if (F1 <= 0) or (F1 >= 1) then
    begin
    raise EInvalidArgument.Create(rsInvalidArgumentsInvNormalexpr);
    end;
  x := Mi;
  ACount := 0;
  repeat
    Inc(ACount);
    lastx := x;
    x := x + (normalexpr(x, Mi, Sigma) - F1) / normalpdf(x, Mi, Sigma);
    until (abs(x-lastx) < accuracy) or (ACount>25) ;
  InvNormalexpr := x;
  end;

procedure NormalLParam(L1, L2, L3: Real; var Mi, Sigma: Real);
begin
  Mi := L1;
  Sigma := L2*1.77245385;
end;

function LogNormal2m (x, Mi, Sigma : Real) : Real;
  var miy, sigmay : Real;
  begin
  sigmay := Sqrt(ln(Sqr(Sigma/Mi) + 1));
  miy := ln(Mi) - Sqr(sigmay)/2;
  LogNormal2m := normalcdf (ln(x), miy, sigmay);
  end;

function InvLogNormal2m (F, Mi, Sigma : Real) : Real;
  var miy, sigmay : Real;
  begin
  sigmay := Sqrt(ln(Sqr(Sigma/Mi) + 1));
  miy := ln(Mi) - Sqr(sigmay)/2;
  InvLogNormal2m := exp(invnormalcdf (F, miy, sigmay));
  end;

function LogNormalcdf( x, miy, sigmay: Real): Real;
begin
  Result := normalcdf (ln(x), miy, sigmay);
end;

function LogNormalpdf( x, miy, sigmay: Real): Real;
begin
  Result := Normalpdf(Ln(x), miy, sigmay)/x;
end;

function InvLogNormalcdf( F, miy, sigmay: Real): Real;
begin
  Result := exp(invnormalcdf (F, miy, sigmay));
end;

procedure LogNormalParam(Mean, StandardDeviation: Real;
  var miy, sigmay: Real);
begin
  sigmay := Sqrt(Ln(1+Sqr(StandardDeviation/Mean)));
  miy := Ln(Mean)-Sqr(sigmay)*0.5;
end;

function Galtoncdf (X, Miy, Sigmay, c: Real): Real;
begin
  if (X<=c) or (Sigmay<=0) then
    raise EInvalidArgument.Create(rsInvalidGaltonParameters);
  Result := Normalcdf( Ln(X-c),Miy, Sigmay);
end;

function Galtonpdf (X, Miy, Sigmay, c: Real): Real;
begin
  Result := Normalpdf(Ln(x-c), Miy, Sigmay)/(x-c);
end;

function InvGaltoncdf (F, Miy, Sigmay, c: Real): Real;
begin
  if (F>=1) or (F<=0) or (Sigmay<=0) then
    raise EInvalidArgument.Create(rsInvaliedInvertedGaltonParameters);
  Result := c+Exp(InvNormalcdf (F, Miy, Sigmay));
end;

resourcestring
  rsInvalidSkewnessForGaltonParameters =
    'Skewness should be positive in order to calculate Galton parameters';

procedure GaltonParam(Mean, StandardDeviation, AsymetryFactor: Real;
  var c, miy, sigmay: Real);
var
  AOmega, APhi :Real;
begin
  c := 0;
  miy := 0;
  sigmay := -1;
  if AsymetryFactor <0 then
    raise EInvalidArgument.Create(rsInvalidSkewnessForGaltonParameters);
  AOmega := 0.5* ( -1*AsymetryFactor+sqrt(Sqr(AsymetryFactor)+4));
  APhi := (1-Power(AOmega,2/3))/Power(AOmega,1/3);
  sigmay := Sqrt(Ln(1+Sqr(APhi)));
  miy := Ln(StandardDeviation/APhi)-Sqr(sigmay)*0.5;
  c := Mean -StandardDeviation/APhi;
end;

{Gamma distribution family}
resourcestring
  rsInvalidInvertedExponentialParameters =
    'Invalid Inverted Exponential Parameters';
  rsInvalidExponentialCDFArgument =
    'Invalid argument for Exponential CDF';
  rsInvalidGammaShapeParameter =
    'Invalied Gamma distribution shape parameter';
  rsInvaliedInvGammaArgument =
    'Invalid inverted Gamma distribution argument';
  rsInvalidGammaRandomVariable =
    'Invalid Gamma random variable';

function Exponentialpdf(X, Lambda, Psi: Real): Real;
begin
  if X<Psi then
    raise EInvalidArgument.Create(rsInvalidExponentialCDFArgument);
  Result := Lambda*Exp(-Lambda*(X-Psi));
end;

function Exponentialcdf(X, Lambda, Psi: Real): Real;
begin
  if X<Psi then
    raise EInvalidArgument.Create(rsInvalidExponentialCDFArgument);
  Result := 1-Exp(-Lambda*(X-Psi));
end;

function InvExponentialcdf(F, Lambda, Psi: Real): Real;
begin
  if (Lambda <= 0) or (F<=0) or (F>=1) then
    raise EInvalidArgument.Create(rsInvalidInvertedExponentialParameters);
  Result := Psi - Ln(1-F)/Lambda;
end;

procedure ExponentialParam( Mi, Sigma: Real; var c, Lambda: Real);
begin
  c := Mi - Sigma;
  Lambda := 1/Sigma;
end;

procedure ExponentialLParam( L1, L2, L3: Real; var c, Lambda: Real);
begin
  c := L1-2*L2;
  Lambda := 0.5/L2;
end;

{ Συνάρτηση πυκνότητας πιθανότητας κατανομής Γάμα
  (Gamma probability density function) }
{ f(x) = λ^κ * x^(κ-1) * exp(-λ*x) / Γ(κ)
  όπου :
    κ = παράμετρος μορφής       ( > 0)
    λ = παράμετρος κλίμακας
    x > 0 αν λ > 0 ή x < 0 αν λ < 0                        }
function Gammapdf (x, Kappa, Lambda : Real) : Real;

  {ratio = a^(k-1)/Γ(k)} {ΥΠΑΡΧΕΙ ΠΡΟΒΛΗΜΑ ΜΕ ΤΟΝ COMPILER, VER. 6.0}
  function ratio (a, k : Real) : Real;
    begin
    if k <= 25 then ratio := Power (a, k-1) / Gamma(k)
    else ratio := (a/(k-1)) * ratio (a, k-1);
    end;

  begin
  if x = 0 then Gammapdf := 0
  else
    begin
    if (Kappa <= 0) then
      begin
      raise EInvalidArgument.Create(rsInvalidGammaShapeParameter);
      end;
    if ((x < 0) and (Lambda > 0)) or ((x > 0) and (Lambda < 0))  then
      begin
      raise EInvalidArgument.Create(rsInvalidGammaRandomVariable);
      x := 0;
      end;
    Gammapdf := abs(Lambda) * exp (-Lambda*x) * ratio (Lambda*x, Kappa);
    end;
  end;

{ Συνάρτηση κατανομής Γάμα  (Gamma distribution function)
 Υπολογισμός με βάση τη σχέση :
  F(x) = x  f(x) * Σ<i=0 μέχρι άπειρο>(t(i))
  όπου :
    F(x) = συνάρτηση κατανομής Γάμα
    f(x) = συνάρτηση πυκνότητας πιθανότητας Γάμα
    t(i) = (λ*x)^i/[κ*(κ+1)*...*(κ+i)]
  (στην πραγματικότητα το άθροισμα Σ υπολογίζεται για πεπερασμένο αριθμό
  όρων t(i) )
  Περιορισμοί  χ > 0, κ > 0, λ > 0 }

function Gammacdf (x, Kappa, Lambda : Real) : Real;
  const
    accuracy = 1.0E-10;
  var
    term : Real;
    sum  : Real;
    temp : Real;
    i, ACount    : integer;
    gpdf : Real;
    gcdf : Real;
  begin
{If Kappa grater than 150, return normal cumulative distribution function}
  if Kappa>150 then
  begin
    Result := normalcdf(x, Kappa/Lambda, Sqrt(Kappa)/Lambda);
    Exit;
  end;
  i := 1; term := 1 / Kappa; sum := 1/Kappa ;
  gpdf := Gammapdf(x, Kappa, Lambda);
  ACount := 0;
  repeat
    Inc(ACount);
    temp := sum;
    term := term * Lambda * x / (Kappa + i);
    i := i + 1;
    sum := sum + term;
    gcdf := x * gpdf * sum;
    until (abs(sum - temp) < accuracy) or (gcdf >= 1) or (ACount>100);
  if gcdf >= 1 then Gammacdf := 1 else Gammacdf := gcdf;
  end;

resourcestring
  rsMathErrorInInvGamma =
    'A mathematical error (divide by zero) occured when trying to invert the gamma distribution '+
      'function. Possibly a gamma cumulative distribution function value '+
      'approaching the limit value of 1: gammacdf=';

{ Αντίστροφη της συνάρτησης κατανομής Γάμα
  (Inverted Gamma distribution function) }
{ Υπολογισμός με τη μέθοδο Newton : Περιορισμοί 0 <= g <= 1, κ > 0, λ > 0}
function InvGammacdf(g, Kappa, Lambda : Real) : Real;
  const
    accuracy = 1.0E-5;
  var
    x, lastx : Real;
    ACount: Integer;
  begin
  if (g <= 0) or (g >= 1) then
    begin
    raise EInvalidArgument.Create(rsInvaliedInvGammaArgument);
    end;
  if Kappa>150 then
  begin
    Result := Invnormalcdf(g, Kappa/Lambda, Sqrt(Kappa)/Lambda);
    Exit;
  end;
  x := 2 * g * Kappa / Lambda;
  lastx := x;
  ACount := 0;
  repeat
    Inc(ACount);
    if x < lastx /2 then x := lastx / 2
    else if x > 2.5 * lastx then x := 2.5 * lastx;
    lastx := x;
    try
      x := x - (Gammacdf(x, Kappa, Lambda) - g) / Gammapdf(x, Kappa, Lambda);
    except
      on EZeroDivide do
        raise EMathError.Create(rsMathErrorInInvGamma+FloatToStr(g));
      else raise;
    end;
    until (abs((x-lastx) / x) < accuracy) or (ACount>100) ;
  InvGammacdf := x;
  end;

function GammaShape (Mean, Variance : Real) : Real;
  begin
  GammaShape := Mean * Mean / Variance;
  end;

function GammaScale (Mean, Variance : Real) : Real;
  begin
  GammaScale := Mean / Variance;
  end;

function GammaMean (Shape, Scale : Real) : Real;
  begin
  GammaMean := Shape/Scale;
  end;

function GammaVariance (Shape, Scale : Real) : Real;
  begin
  GammaVariance := Shape/Scale/Scale;
  end;

function PearsonIIIcdf (X, Kappa, Lambda, Psi: Real): Real;
begin
  Result := Gammacdf(X-Psi, Kappa, Lambda);
end;

function PearsonIIIpdf (X, Kappa, Lambda, Psi: Real): Real;
begin
  Result := Gammapdf(x-psi, Kappa, Lambda);
end;

resourcestring
  rsInvalidArgumentsXSquare =
    'Invalid arguments for X-Square distribution function';

function XSquareCdf(x: Real; DegreesOfFreedom: Integer): Real;
begin
  if DegreesOfFreedom<1 then
    raise EInvalidArgument.Create(rsInvalidArgumentsXSquare );
  Result := Gammacdf(x, DegreesOfFreedom*0.50, 0.50);
end;

function InvXSquareCdf(u: Real; DegreesOfFreedom: Integer): Real;
begin
  if DegreesOfFreedom<1 then
    raise EInvalidArgument.Create(rsInvalidArgumentsXSquare );
  Result := InvGammacdf(u, DegreesOfFreedom*0.50, 0.50);
end;

function InvPearsonIIIcdf (F, Kappa, Lambda, Psi: Real): Real;
begin
  Result := InvGammacdf(F, Kappa, Lambda) + Psi;
end;

procedure PearsonIIIParam(Mean, StandardDeviation, AsymetryFactor: Real;
  var Kappa, Lambda, c: Real);
begin
  Kappa := 4 / Sqr(AsymetryFactor);
  Lambda := Sqrt(Kappa)/StandardDeviation;
  c := Mean - Kappa/Lambda;
end;

function LogPearsonIIIcdf (X, Kappa, Lambda, Psi: Real): Real;
begin
  Result := PearsonIIIcdf(Ln(X), Kappa, Lambda, Psi);
end;

function LogPearsonIIIpdf (X, Kappa, Lambda, Psi: Real): Real;
begin
  Result := PearsonIIIpdf(Ln(x), Kappa, Lambda, Psi)/x;
end;

function InvLogPearsonIIIcdf (F, Kappa, Lambda, Psi: Real): Real;
begin
  Result := Exp(InvPearsonIIIcdf(F, Kappa, Lambda, Psi));
end;

{Beta...}

resourcestring
  rsInvalidBetaPDFArguments = 'Invalid arguments for Beta PDF';

{ Συνάρτηση πυκνότητας πιθανότητας κατανομής Βήτα
  (Beta probability density function)}
{ f(x) = x^(μ-1) * (1-x)^(ν-1) / Β(μ, ν)               }
function Betapdf (x, Mi, Ni: Real) :Real ;
  begin
  if (Mi <= 0) or (Ni <= 0) or (x < 0) or (x > 1) then
    begin
    raise EInvalidArgument.Create(rsInvalidBetaPDFArguments);
    end;
  Betapdf := Power(x, Mi-1) * Power(1-x, Ni-1)/ Beta(Mi, Ni);
  end;

function BetaMi (Mean, Variance : Real) : Real;
  begin
  BetaMi := Mean * Mean * (1 - Mean) / Variance - Mean;
  end;

function BetaNi (Mean, Variance : Real) : Real;
  begin
  BetaNi := Mean * (1 - Mean) * (1 - Mean) / Variance - (1 - Mean);
  end;

function BetaMean (Mi, Ni : Real) : Real;
  begin
  BetaMean := Mi / (Mi + Ni);
  end;

function BetaVariance (Mi, Ni : Real) : Real;
  begin
  BetaVariance := Mi * Ni / Sqr (Mi + Ni) / (Mi + Ni + 1);
  end;

{Extreme Value distributions}

resourcestring
  rsInvalidEV1Maxparameters = 'Invalid EV1Max (Gumbel Max) parameters';
  rsInvalidInvEV1Maxparameters =
    'Invalid InvEV1Max (Inv Gumbel Max) parameters';
  rsInvalidEV1Minparameters = 'Invalid EV1Min (Gumbel Min) parameters';
  rsInvalidInvEV1Minparameters =
    'Invalid InvEV1Min (Inv Gumbel Min) parameters';
  rsInvalidEV2Maxparameters = 'Invalid EV2Max parameters';
  rsInvalidInvEV2Maxparameters = 'Invalid Inverted EV2Max parameters';
  rsInvalidWeibPDFArguments = 'Invalid Weibull pdf arguments';
  rsInvalidWeibCDFArguments = 'Invalid Weibull distribution function arguments';
  rsInvalidInvWeibCDFArguments =
    'Invalid inverted Weibull distribution function arguments';

function EV1Maxcdf(X, Lambda, Psi: Real): Real;
begin
  if Lambda<=0 then
    raise EInvalidArgument.Create(rsInvalidEV1Maxparameters);
  Result := Exp( -Exp( -X/Lambda + Psi ) );
end;

function EV1Maxpdf (X, Lambda, Psi: Real): Real;
var
  F: Real;
begin
  F := EV1Maxcdf(X, Lambda, Psi);
  Result := F*(-Ln(F))/Lambda;
end;

function InvEV1Maxcdf(F, Lambda, Psi: Real): Real;
begin
  if (Lambda<=0) or (F>=1) or (F<=0) then
    raise EInvalidArgument.Create(rsInvalidInvEV1Maxparameters);
  Result := Lambda * Psi - Lambda*Ln(-Ln(F));
end;

procedure EV1MaxParam(Mi, Sigma: Real; var Lambda, Psi: Real);
begin
  Lambda := 0.78*Sigma;
  Psi := Mi/(0.78*Sigma)-0.5772;
end;

procedure EV1MaxLParam(L1, L2, L3: Real; var Lambda, Psi: Real);
begin
  Lambda := L2/Ln(2);
  Psi := L1*Ln(2)/L2-0.5772;
end;

function EV1Mincdf(X, Lambda, Psi: Real): Real;
begin
  if Lambda<=0 then
    raise EInvalidArgument.Create(rsInvalidEV1Minparameters);
  Result := 1 - Exp( -Exp( X/Lambda - Psi ) );
end;

function EV1Minpdf (X, Lambda, Psi: Real): Real;
var
  One_minus_F: Real;
begin
  One_minus_F := 1-EV1Mincdf(X, Lambda, Psi);
  Result := One_minus_F*(-Ln(One_minus_F))/Lambda;
end;

function InvEV1Mincdf(F, Lambda, Psi: Real): Real;
begin
  if (Lambda<=0) or (F>=1) or (F<=0) then
    raise EInvalidArgument.Create(rsInvalidInvEV1Minparameters);
  Result := Lambda*Psi + Lambda*Ln(-Ln(1-F));
end;

procedure EV1MinParam(Mi, Sigma: Real; var Lambda, Psi: Real);
begin
  Lambda := 0.78*Sigma;
  Psi := Mi/(0.78*Sigma)+0.5772;
end;

procedure EV1MinLParam(L1, L2, L3: Real; var Lambda, Psi: Real);
begin
  Lambda := L2/Ln(2);
  Psi := L1*Ln(2)/L2+0.5772;
end;

function EV2Maxcdf(X, Kappa, Lambda: Real): Real;
begin
  if (Lambda<=0) or (Kappa<=0) then
    raise EInvalidArgument.Create(rsInvalidEV2Maxparameters);
  Result := Exp( -1 * Power(Kappa*X/Lambda, -1/Kappa) );
end;

function EV2Maxpdf(X, Kappa, Lambda: Real): Real;
var
  F: Real;
begin
  F := EV2Maxcdf(X, Kappa, Lambda);
  Result := F*Power(-Ln(F), 1+Kappa)/Lambda;
end;

function InvEV2Maxcdf(F, Kappa, Lambda: Real): Real;
begin
  if (Lambda<=0) or (Kappa<=0) or (F>=1) or (F<=0) then
    raise EInvalidArgument.Create(rsInvalidInvEV2Maxparameters);
  Result := (Lambda/Kappa)*Power(-Ln(F),-Kappa);
end;

procedure EV2MaxParam (Mean, Variance: Real;
  var Shape, Scale: Real);

  function TargetEvaluation(t: Real): Real;
  begin
    Result := Gamma(1-2*t)/Sqr(Gamma(1-t));
  end;
var
  tmax,tmin, t: Real;
  ATarget: Real;
  ACount: Integer;
begin
  ATarget := 1+Variance/Sqr(Mean);
  t := 0.25;
  tmin := 0;
  tmax := 0.4999;
  ACount := 0;
  while (Abs(tmin-tmax)>0.0001) and (ACount <100) do
  begin
    Inc(ACount);
    t := 0.5*(tmin+tmax);
    if ATarget-TargetEvaluation(t)>0 then
      tmin := t else
      tmax := t;
  end;
  Shape := t;
  Scale := Mean*Shape/Gamma(1-Shape);
end;

procedure EV2MaxAproxParam (Mean, Variance: Real;
  var Shape, Scale: Real);
begin
  Shape := 0.5 - 1/( Exp(2.59*Power(Ln(1+Variance/Sqr(Mean)),0.475)) + 1 );
  Scale := Mean*Shape/Gamma(1-Shape);
end;

procedure EV2MaxLParam(L1, L2, L3: Real; var Kappa, Lambda: Real);
begin
  Kappa := Ln(1+L2/L1)/Ln(2);
  Lambda := L1*Kappa/Gamma(1-Kappa);
end;

{ Συνάρτηση πυκνότητας πιθανότητας κατανομής Weibull
  (Weibull probability density function) }
{See Koytsogiannhs notes 1999AdvHydroProb.pdf}
function Weibpdf (x, Kappa, Lambda : Real) : Real;
  var t1, t2 : Real;
  begin
  if (x <= 0) or (Kappa <= 0) or (Lambda <= 0) then
    begin
    raise EInvalidArgument.Create(rsInvalidWeibPDFArguments);
    end;
  t1 := Power (x*Kappa/Lambda, 1/Kappa);
  t2 := Power (x*Kappa/Lambda, 1/Kappa-1 );
  Weibpdf := (1/Lambda) * t2 * exp(-t1);
  end;

{ Συνάρτηση κατανομής Weibull  (Weibull distribution function)}
function Weibcdf (x, Kappa, Lambda : Real) : Real;
  var t1 : Real;
  begin
  if (x <= 0) or (Kappa <= 0) or (Lambda <= 0) then
    begin
      raise EInvalidArgument.Create(rsInvalidWeibCDFArguments);
    end;
  t1 := Power (x*Kappa/Lambda, 1/Kappa);
  Weibcdf := 1 - exp(-t1);
  end;

{ Αντίστροφη της συνάρτησης κατανομής Weibull
  (Inverted Weibull distribution function) }
function InvWeibcdf (F, Kappa, Lambda: Real) : Real;
  var t1 : Real;
  begin
  if (F <= 0) or (F >= 1) or (Kappa <= 0) or (Lambda <= 0) then
    begin
    raise EInvalidArgument.Create(rsInvalidInvWeibCDFArguments);
    end;
  t1 := -Ln(1-F);
  InvWeibcdf := (Lambda/Kappa)*Power(t1,Kappa);
  end;

procedure WeibParam (Mean, Variance :Real;
                     var Shape, Scale : Real);
var
  Shape_prev: Real;
  stored_Shape, stored_Scale: Real;
  ACount: Integer;
begin
{Calculatin Shape, Scale according Koutsogiannis book (1997)}
{Then, transform Shape, Scale to new Koutsogiannis notes 1999AdvHydroProb.pdf}
{Get a first aproximation:}
  WeibAproxParam(Mean, Variance, Shape, Scale);  
  stored_Shape := Shape;
  stored_Scale := Scale;
  ACount := 0;
  try
    Shape := 1/Shape;
    Scale := Scale*Shape;
    Repeat
      Shape_prev := Shape;
      Shape := 0.5*(1+Variance/(Mean*Mean))*Power(Gamma(1/Shape_prev),2)/
        Gamma(2/Shape_prev);
      Shape := 0.5*(3*Shape_prev - Shape);
      if Shape < 0 then
        Shape := Shape_prev*0.5;
      Inc(ACount)
    Until (Abs(Shape-Shape_prev)<0.001) or (ACount>100);
    Scale := Mean / Gamma (1 + 1/Shape);
{turn back to Shape, Scale as defined to KoytsogianNis notes}
    Shape := 1 / Shape;
    Scale := Scale*Shape;
  except
    Shape := stored_Shape;
    Scale := stored_Scale;
  end;
end;

procedure WeibAproxParam (Mean, Variance: Real;
                          var Shape, Scale: Real);
begin
  Shape := 2.56*(Exp(0.41*Power(Ln(1+Variance/Sqr(Mean)),0.58))-1);
  Scale := Mean *Shape / Gamma (1 + Shape);
end;

procedure WeibLParam (L1, L2, L3: Real; var Kappa, Lambda: Real);
begin
  Kappa := -1*Ln(1-L2/L1)/Ln(2);
  Lambda := L1*Kappa/Gamma(1+Kappa);
end;

function WeibRatio (Shape, Scale : Real) : Real;
  var t1 : Real;
  begin
    t1 := Gamma(1+Shape);
    Result := (Scale/Shape)*(Gamma(1+2*Shape)-t1*t1)/t1;
  end;

function WeibMean (Shape, Scale : Real) : Real;
  begin
  WeibMean := Scale / Shape * Gamma(1 + Shape);
  end;

function WeibVariance (Shape, Scale : Real) : Real;
  begin
  WeibVariance := Sqr(WeibMean(Shape, Scale) * WeibRatio (Shape, Scale));
  end;

procedure GEVMaxParam (MeanValue, StandardDeviation, AsymetryFactor: Real;
  var Shape, Scale, Position: Real);
  function Sign(Value: Real): Real;
  begin
   if Value>=0 then
     Result := 1 else
     Result := -1;
  end;
  function AsymetryEvaluation(Kappa: Real): Real;
  begin
    if (Kappa<=-0.001) or (Kappa>=0.001) then
    begin
      Result := Sign(Kappa)* ( Gamma(1-3*Kappa)-3*Gamma(1-2*Kappa)*
        Gamma(1-Kappa)+2*Power(Gamma(1-Kappa),3) ) /
        Power(Gamma(1-2*Kappa)-Power(Gamma(1-Kappa),2),3/2);
    end else
    begin
      Result := AsymetryEvaluation(-0.001)+
        (AsymetryEvaluation(0.001001)-AsymetryEvaluation(-0.001001))*
        (Kappa+0.001)/0.002;
    end;
  end;
var
  tmax,tmin, t: Real;
  ACount: Integer;
begin
  t := 0;
  tmin := -10;
  tmax := 0.33333;
  ACount := 0;
  while (Abs(tmin-tmax)>0.0005) and (ACount <100) do
  begin
    Inc(ACount);
    t := 0.5*(tmin+tmax);
    if AsymetryFactor-AsymetryEvaluation(t)>0 then
      tmin := t else
      tmax := t;
  end;
  Shape := t;
  if Abs(Shape)>0.001 then
  begin
    Scale := Abs(Shape)*StandardDeviation / Sqrt( Gamma(1-2*Shape)-
      Sqr(Gamma(1-Shape)) );
    Position := MeanValue/Scale-(Gamma(1-Shape)-1)/Shape;
  end else begin
    Scale := 0.78*StandardDeviation;
    Position := MeanValue/Scale - 0.5772;
  end;
end;

resourcestring
  rsSpecifiedShapeShouldBeWithin =
    'Specified shape factor for GEV should be within a range of 0.001-0.5';

procedure GEVMaxParamKS (MeanValue, StandardDeviation, Shape: Real;
  var Scale, Position: Real);
begin
  if (Shape<0.001) or (Shape>0.5) then
    raise EInvalidArgument.Create(rsSpecifiedShapeShouldBeWithin);
  Scale := Abs(Shape)*StandardDeviation / Sqrt( Gamma(1-2*Shape)-
    Sqr(Gamma(1-Shape)) );
  Position := MeanValue/Scale-(Gamma(1-Shape)-1)/Shape;
end;

procedure GEVMaxAproxParam (MeanValue, StandardDeviation, AsymetryFactor: Real;
  var Shape, Scale, Position: Real);
begin
  Shape := (1/3) - 1/
    (0.31+0.91*AsymetryFactor+Sqrt(Sqr(0.91*AsymetryFactor)+1.8));
  if Abs(Shape)>0.001 then
  begin
    Scale := Abs(Shape)*StandardDeviation / Sqrt( Gamma(1-2*Shape)-
      Sqr(Gamma(1-Shape)) );
    Position := MeanValue/Scale-(Gamma(1-Shape)-1)/Shape;
  end else begin
    Scale := 0.78*StandardDeviation;
    Position := MeanValue/Scale - 0.5772;
  end;
end;

procedure GEVMaxLParam (L1,L2,L3: Real; var Shape, Scale, Position: Real);
var
  c,t3: Real;
begin
  t3 := L3/L2;
  c := Ln(2)/Ln(3) - 2/(3+t3);
  Shape := 7.8*c-1.43*c*c;
  if Abs(Shape) >= 0.001 then
  begin
    Scale := Shape * L2 / (Gamma(1-Shape)*(Power(2,Shape)-1));
    Position := L1/Scale - (Gamma(1-Shape)-1)/Shape;
{If k~0 then return Gumbel Max}
  end else begin
    Scale := L2/Ln(2);
    Position := L1/Scale - 0.5772;
  end;
end;

procedure GEVMaxLParamKS (L1,L2,L3, Shape: Real; var Scale, Position: Real);
begin
  if (Shape<0.001) or (Shape>0.5) then
    raise EInvalidArgument.Create(rsSpecifiedShapeShouldBeWithin);
  Scale := Shape * L2 / (Gamma(1-Shape)*(Power(2,Shape)-1));
  Position := L1/Scale - (Gamma(1-Shape)-1)/Shape;
end;

{GEV Functions}
resourcestring
  rsInvalidGEVMaxParameters = 'Invalid GEVMax Parameters';
  rsInvalidInvGEVMaxParameters = 'Invalid Inverted GEVMax Parameters';
  rsInvalidGEVMinParameters = 'Invalid GEVMin Parameters';
  rsInvalidInvGEVMinParameters = 'Invalid Inverted GEVMin Parameters';

{Συνάρτηση κατανομής ΓΑΤ - Μεγίστων}
function GEVMaxcdf (X, Kappa, Lambda, Psi: Real): Real;
begin
  if Lambda <= 0 then
    raise EInvalidArgument.Create(rsInvalidGEVMaxParameters);
  if Abs(Kappa)>=0.001 then
    Result := Exp( -1*Power(1+Kappa*(X/Lambda - Psi),-1/Kappa))
  else
    Result := Exp( -1*Exp(-1*X/Lambda+Psi));
end;

function GEVMaxpdf(X, Kappa, Lambda, Psi: Real): Real;
var
  F: Real;
begin
  F := GEVMaxcdf(X, Kappa, Lambda, Psi);
  Result := F*Power(-Ln(F), 1+Kappa)/Lambda;
end;

function InvGEVMaxcdf(F, Kappa, Lambda, Psi: Real): Real;
begin
  if (Lambda <= 0) or (F<=0) or (F>=1) then
    raise EInvalidArgument.Create(rsInvalidInvGEVMaxParameters);
  if Abs(Kappa)>=0.001 then
    Result := Lambda*Psi + (Lambda/Kappa)*(Power(-Ln(F),-Kappa)-1)
  else
    Result := Lambda*Psi - Lambda*Ln(-Ln(F));
end;

procedure GEVMinParam (MeanValue, StandardDeviation, AsymetryFactor: Real;
  var Shape, Scale, Position: Real);
  function Sign(Value: Real): Real;
  begin
   if Value>=0 then
     Result := 1 else
     Result := -1;
  end;
  function AsymetryEvaluation(Kappa: Real): Real;
  begin
    if (Kappa<=-0.001) or (Kappa>=0.001) then
    begin
      Result := Sign(Kappa)* ( Gamma(1+3*Kappa)-3*Gamma(1+2*Kappa)*
        Gamma(1+Kappa)+2*Power(Gamma(1+Kappa),3) ) /
        Power(Gamma(1+2*Kappa)-Power(Gamma(1+Kappa),2),3/2);
    end else
    begin
      Result := AsymetryEvaluation(-0.001)+
        (AsymetryEvaluation(0.001001)-AsymetryEvaluation(-0.001001))*
        (Kappa+0.001)/0.002;
    end;
  end;
var
  tmax,tmin, t: Real;
  ACount: Integer;
begin
  t := 0;
  tmin := -0.33333;
  tmax := 10;
  ACount := 0;
  while (Abs(tmin-tmax)>0.0005) and (ACount <100) do
  begin
    Inc(ACount);
    t := 0.5*(tmin+tmax);
    if AsymetryFactor-AsymetryEvaluation(t)>0 then
      tmin := t else
      tmax := t;
  end;
  Shape := t;
  if Abs(Shape)>0.001 then
  begin
    Scale := Abs(Shape)*StandardDeviation / Sqrt( Gamma(1+2*Shape)-
      Sqr(Gamma(1+Shape)) );
    Position := MeanValue/Scale+(1-Gamma(1+Shape))/Shape;
{If k~0 then return Gumbel Min}
  end else begin
    Scale := 0.78*StandardDeviation;
    Position := MeanValue/Scale + 0.5772;
  end;
end;

procedure GEVMinParamKS (MeanValue, StandardDeviation, Shape: Real;
  var Scale, Position: Real);
begin
  if (Shape<0.001) or (Shape>0.5) then
    raise EInvalidArgument.Create(rsSpecifiedShapeShouldBeWithin);
  Scale := Abs(Shape)*StandardDeviation / Sqrt( Gamma(1+2*Shape)-
    Sqr(Gamma(1+Shape)) );
  Position := MeanValue/Scale+(1-Gamma(1+Shape))/Shape;
end;

procedure GEVMinAproxParam (MeanValue, StandardDeviation, AsymetryFactor: Real;
  var Shape, Scale, Position: Real);
begin
  Shape := (-1/3) + 1/
    (0.28-0.9*AsymetryFactor+0.998*Sqrt(Power(0.9*AsymetryFactor,2)+1.93));
  if Abs(Shape)>0.001 then
  begin
    Scale := Abs(Shape)*StandardDeviation / Sqrt( Gamma(1+2*Shape)-
      Sqr(Gamma(1+Shape)) );
    Position := MeanValue/Scale+(1-Gamma(1+Shape))/Shape;
{If k~0 then return Gumbel Min}
  end else begin
    Scale := 0.78*StandardDeviation;
    Position := MeanValue/Scale + 0.5772;
  end;
end;

procedure GEVMinLParam (L1,L2,L3: Real; var Shape, Scale, Position: Real);
var
  c,t3: Real;
begin
  t3 := L3/L2;
  c := 2/(3-t3) - Ln(2)/Ln(3);
  Shape := 7.8*c+4.71*c*c;
  if Abs(Shape) >= 0.001 then
  begin
    Scale := Shape * L2 / (Gamma(1+Shape)*(1-Power(2,-1*Shape)));
    Position := L1/Scale + (1-Gamma(1+Shape))/Shape;
  end else begin
    Scale := L2/Ln(2);
    Position := L1/Scale + 0.5772;
  end;
end;

procedure GEVMinLParamKS (L1,L2,L3, Shape: Real; var Scale, Position: Real);
begin
  if (Shape<0.001) or (Shape>0.5) then
    raise EInvalidArgument.Create(rsSpecifiedShapeShouldBeWithin);
  Scale := Shape * L2 / (Gamma(1+Shape)*(1-Power(2,-1*Shape)));
  Position := L1/Scale + (1-Gamma(1+Shape))/Shape;
end;

{Συνάρτηση κατανομής ΓΑΤ - Ελαχίστων}
function GEVMincdf (X, Kappa, Lambda, Psi: Real): Real;
begin
  if Lambda <= 0 then
    raise EInvalidArgument.Create(rsInvalidGEVMinParameters);
  if Abs(Kappa)>=0.001 then
    Result := 1-Exp( -1*Power(1+Kappa*(X/Lambda - Psi),1/Kappa))
  else
    Result := 1-Exp(-1*Exp(X/Lambda-Psi));
end;

function GEVMinpdf(X, Kappa, Lambda, Psi: Real): Real;
var
  One_minus_F: Real;
begin
  One_minus_F := 1 - GEVMincdf(X, Kappa, Lambda, Psi);
  Result := One_minus_F*Power(-Ln(One_minus_F), 1-Kappa)/Lambda;
end;

function InvGEVMincdf (F, Kappa, Lambda, Psi: Real): Real;
begin
  if (Lambda <= 0) or (F<=0) or (F>=1) then
    raise EInvalidArgument.Create(rsInvalidInvGEVMinParameters);
  if Abs(Kappa)>=0.001 then
    Result := Lambda*Psi + (Lambda/Kappa)*( Power(-Ln(1-F),Kappa)-1 )
  else
    Result := Lambda*Psi+Lambda*Ln(-Ln(1-F));
end;

{Pareto}

resourcestring
  rsInvalidParetoParameters = 'Invalid Pareto distribution parameters';
  rsInvalidInvParetoParameters =
    'Invalid inverted Pareto distribution parameters';

function Paretocdf (X, Kappa, Lambda, Psi: Real): Real;
begin
  if Lambda<=0 then
    raise EInvalidArgument.Create(rsInvalidParetoParameters);
  if Abs(Kappa)>=0.001 then
    Result := 1-Power(1-Kappa*(X/Lambda-Psi),1/Kappa)
  else
    Result := 1-Exp(Psi-X/Lambda);
end;

function Paretopdf(X, Kappa, Lambda, Psi: Real): Real;
begin
  if Lambda<=0 then
    raise EInvalidArgument.Create(rsInvalidParetoParameters);
  if Abs(Kappa)>=0.001 then
    Result := Power(1-Kappa*(X/Lambda-Psi),1/Kappa-1)/Lambda
  else
    Result := Exp(Psi-X/Lambda)/Lambda;
end;

function InvParetocdf (F, Kappa, Lambda, Psi: Real): Real;
begin
  if (F>=1) or (F<=0) or (Lambda<=0) then
    raise EInvalidArgument.Create(rsInvalidInvParetoParameters);
  if Abs(Kappa)>=0.001 then
    Result := (Lambda/Kappa)*(1-Power(1-F,Kappa)+Psi*Kappa)
  else
    Result := Lambda*(Psi-Ln(1-F));
end;

procedure ParetoParam(MeanValue, StandardDeviation, AsymetryFactor: Real;
  var Shape, Scale, Position: Real);
function AsymetryEvaluation(Kappa: Real): Real;
begin
  Result := 2*(1-Kappa)*Sqrt(1+2*Kappa)/(1+3*Kappa);
end;

var
  tmax,tmin, t: Real;
  ACount: Integer;
begin
  if AsymetryFactor >0 then
  begin
    t := 0;
    tmin := -0.3332;
    tmax := 1.0;
  end else begin
    t := 3;
    tmin := 1;
    tmax := 100;
  end;
  ACount := 0;
  while (Abs(tmin-tmax)>0.0005) and (ACount <100) do
  begin
    Inc(ACount);
    t := 0.5*(tmin+tmax);
    if AsymetryFactor-AsymetryEvaluation(t)>0 then
      tmax := t else
      tmin := t;
  end;
  Shape := t;
  Scale := StandardDeviation * (1+Shape) * Sqrt(1+2*Shape);
  Position := MeanValue/Scale - 1/(1+Shape);
end;

procedure ParetoLParam(L1, L2, L3: Real; var Shape, Scale, Position: Real);
var
  t3: Real;
begin
  t3 := L3/L2;
  Shape := (1-3*t3)/(t3+1);
  Scale := L2 * (1+Shape)*(2+Shape);
  Position := L1/Scale - 1/(1+Shape);
end;

function WeibullEmpirical(Order, Count: Integer): Real;
begin
  Result := Order/(Count + 1);
end;

function BlomEmpirical(Order, Count: Integer): Real;
begin
  Result := 1-(1+Count-Order - 0.375)/(Count + 0.25);
end;

function CunnaneEmpirical(Order, Count: Integer): Real;
begin
  Result := 1-(1+Count-Order - 0.40)/(Count + 0.20);
end;

function GringortenEmpirical(Order, Count: Integer): Real;
begin
  Result := 1-(1+Count-Order - 0.44)/(Count + 0.12);
end;

end.
