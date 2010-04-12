{
  Copyright (C) 1998-2010 National Technical University of Athens

  This file is licensed under the same terms as the rest of thelma.
  The LAPACK library, on which some code of this file is based, is
  licensed under the following license:

    Copyright (c) 1992-2009 The University of Tennessee.  All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions are
    met:

    - Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer. 
      
    - Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer listed
      in this license in the documentation and/or other materials
      provided with the distribution.
      
    - Neither the name of the copyright holders nor the names of its
      contributors may be used to endorse or promote products derived from
      this software without specific prior written permission.
      
    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT  
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
    A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT 
    OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
    SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
    LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
    DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
    THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT  
    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
    OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 

}

{** Matrix manipulation functions }
unit Matrix;

{ Coded by:     Initially A. Christofides (September 1998), then D.
		Koutsoyiannis and A. Efstratiadis, followed by naming
		and styling improvements by A. Christofides, then rewriting
    of some functionality by A. Christofides using the LAPACK library.
}

{ All "unsafe" warnings turned off. Turn them back on if you convert the code
  to not use dynamic memory allocation (bug 1155).
}
{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CODE OFF}

interface

uses Classes, SysUtils, stat2, dkmath;

{** Specifies the maximum number of array elements.
    maMaxSize is mostly used internally.
}
const maMaxSize = MaxLongInt div SizeOf(Real);

type
  {** Used internally to store a matrix.
      TMatrix stores matrices as single dimensional arrays, using TMatrixArray
      as their type.
      @SeeAlso <See Class=TMatrix>
      @SeeAlso <See Type=PMatrixArray>
  }
  TMatrixArray = array[0..maMaxSize-1] of Real;
  {** Used internally to store a matrix.
  }
  PMatrixArray = ^TMatrixArray;
  {** Used internally to store an integer matrix.
      TMatrixIntArray is like TMatrixArray, but for integers. It is currently
      used internally in TIntVector.
      @SeeAlso <See Type=TMatrixArray>
      @SeeAlso <See Type=TIntVector>
      @SeeAlso <See Type=PMatrixIntArray>
  }
  TMatrixIntArray = array[0..maMaxSize-1] of Integer;
  {** Used internally to store a matrix.
  }
  PMatrixIntArray = ^TMatrixIntArray;
  {** Used by TMatrix.CreateFromArray to specify array ownership.
      @SeeAlso <See Method=TMatrix.CreateFromArray>
  }
  TCreateFromArrayMethod = (maCopyArray, maOwnArray, maDontOwnArray);
  {** Used by matrix operation methods to specify operand position.
      @SeeAlso <See Method=TMatrix.Multiply>
      @SeeAlso <See Method=TMatrix.Subtract>
  }
  TOperandPosition = (maLeft, maRight);
  TVector = class;
  TIntVector = class;

  {** Stores a two-dimensional matrix.
      TMatrix provides functionality for the creation, copying, and manipulation
      of matrices.

      @author Originally designed by A. Christofides (November 1998).
      Subsequently D. Koutsoyiannis and A. Efstratiadis made changes and added
      most of the existing functionality (1999-2001). Subsequently
      A. Christofides made naming and styling improvements and added
      documentation (2002).<p>
      @SeeAlso <See Class=TVector>
  }
  TMatrix = class(TPersistent)
  private
    FRowCount: Integer;
    FColCount: Integer;
    FOwnsElements: Boolean;
    FElements: PMatrixArray;
  protected
    function GetElement(i, j: Integer): Real;
    procedure SetElement(i, j: Integer; Value: Real);
    function GetSize: Integer;
  public
    {** Specifies the number of rows of the matrix.
        Read RowCount to determine the number of rows of the matrix.
	@SeeAlso <See Property=ColCount>
	@SeeAlso <See Method=Resize>
    }
    property RowCount: Integer read FRowCount;
    {** Specifies the number of columns of the matrix.
        Read ColCount to determine the number of columns of the matrix.
	@SeeAlso <See Property=RowCount>
	@SeeAlso <See Method=Resize>
    }
    property ColCount: Integer read FColCount;
    {** Used internally to specify whether the object owns the array.
        OwnsElements is used internally; for more information on this property,
	see the CreateFromArray method.
	@SeeAlso <See Method=CreateFromArray>
    }
    property OwnsElements: Boolean read FOwnsElements;
    {** Returns or sets a matrix element.
	Read or set E to access the element at row i and column j of the matrix.
	Row and column numbering starts at 1. An attempt to use invalid
	co-ordinates results in an exception.<p>
	E is the default property, so normally you don't have to write
	AMatrix.E[i,j]; use AMatrix[i,j] instead.
    }
    property E[i, j: Integer]: Real read GetElement write SetElement; default;
    {** Returns the number of elements of the matrix.
        Size returns RowCount*ColCount.
	@SeeAlso <See Property=RowCount>
	@SeeAlso <See Property=ColCount>
    }
    property Size: Integer read GetSize;
    {** Copies a TMatrix object.
	When assigning one matrix to another matrix, the destination
	matrix gets a copy of the elements and is set to OwnsElements.
	@author A.X.
	@SeeAlso <See Property=OwnsElements>
        @SeeAlso <Jump File=Delphi5.hlp K="TPersistent,Assign" Text=Assign>
    }
    procedure Assign(Source: TPersistent); override;
    {** Creates a TMatrix object.
	Call Create to create a new TMatrix object.  TMatrix objects created
	with the version of Create without arguments should not be used before
	Resizing.
	@SeeAlso <See Method=Resize>
	@SeeAlso <See Method=CreateFromArray>
    }
    constructor Create; overload;
    {** (Overloaded method - no need for title).
	Call Create to create a new TMatrix object. Create does not initialize
	the elements; to initialize them, use Fill or Zero.
	@SeeAlso <See Method=Fill>
	@SeeAlso <See Method=Zero>
    }
    constructor Create(RowCount, ColCount: Integer); overload;
    {** Creates a TMatrix object using an existing one-dimensional array.
        Use CreateFromArray to create a TMatrix object from an existing
	one-dimensional array. This may be useful for communication with other
	applications via DLLs. RowCount and ColCount specify the array
	dimensions. PArrayOfDouble contains the elements, starting from the
	elements of the first row, then the elements of the second row, and so
	on.<p>
	Method specifies whether the array pointed to by PArrayOfDouble will be
	copied, owned, or not owned. If Method is maCopyArray, then the created
	TMatrix object will create a copy of the array which it will use
	internally to store the elements. If it is maOwnArray or maDontOwnArray,
	it will use the original array itself.<p>
	In the case of maOwnArray, the created object becomes the owner of the
	memory allocated for the original array. When the object is destroyed,
	it will also attempt to free the memory occupied by the array, using
	FreeMem. Thus, maOwnArray should not be used unless the memory for the
	array has been dynamically allocated, or a mess will occur when
	the object is destroyed.<p>
	In the case of maDontOwnArray, the created object will not own the
	memory allocated for the original array. When it is destroyed, it will
	not free the array. In addition, an attempt to Resize the matrix will
	raise an exception.<p>
	@author D.K.,A.X.
	@SeeAlso <See Method=Create>
	@SeeAlso <See Property=OwnsElements>
	@SeeAlso <See Method=Resize>
    }
    constructor CreateFromArray(RowCount, ColCount: Integer;
      PArrayOfDouble: Pointer; Method: TCreateFromArrayMethod);
    {** Destroys the TMatrix object.
        Do not use Destroy; use Free instead, which checks if the object
	reference is nil.
        @SeeAlso <Jump File=Delphi5.hlp K="TObject,Free" Text=TObject.Free>
    }
    destructor Destroy; override;
    {** Returns the matrix in ASCII format.
        Use Ascii when debugging your application, to easily view the contents
	of the matrix.
	@author D.K.
	@SeeAlso <See Method=BriefInfo>
    }
    function Ascii: string;
    {** Returns brief debugging information on the matrix.
        Use BriefInfo when debugging your application, to view the matrix's
	dimensions and corner elements.
	@author D.K.
	@SeeAlso <See Method=Ascii>
    }
    function BriefInfo: string;
    {** Extracts a row from the matrix.
        Use GetRow to extract the ith row of the matrix into AVector, whose
	original contents are replaced.
	@author D.K.
	@SeeAlso <See Method=GetColumn>
    }
    procedure GetRow(AVector: TVector; i: Integer);
    {** Extracts a column from the matrix.
        Use GetColumn to extract the jth row of the matrix into AVector, whose
	original contents are replaced.
	@author D.K.
	@SeeAlso <See Method=GetRow>
    }
    procedure GetColumn(AVector: TVector; j: Integer);
    {** Shifts a submatrix within the matrix.
	Use Shift to shift the submatrix specified by StartRow, StartCol, EndRow
	and EndCol. The submatrix is shifted downward by ShiftRows and rightward
	by ShiftCols; to shift to the opposite direction, use negative values
	for ShiftRows and/or ShiftCols. Elements that are not overwritten by the
	shifting remain unchanged (including not overwritten elements of the
	original submatrix). If after shifting the submatrix would reach outside
	the matrix boundaries, an exception is raised. 
	@author A.X.
    }
    procedure Shift(StartRow, StartCol, EndRow, EndCol, ShiftRows,
      ShiftCols: Integer);
    {** Extracts a vector from a row with specific starting column and interval.
        ExtractVectorFromRow extracts a vector from the matrix into AVector,
	whose original contents are replaced. The vector extracted consists of
	elements of row i starting at column StartCol and continuing every
	Interval elements.
	@author D.K.
	@SeeAlso <See Method=ExtractVectorFromColumn>
        @SeeAlso <See Method=GetRow>
    }
    procedure ExtractVectorFromRow(AVector: TVector;
      i, StartCol, Interval: integer);
    {** Extracts a vector from a column with specific starting row and interval.
        ExtractVectorFromColumn extracts a vector from the matrix into AVector,
	whose original contents are replaced. The vector extracted consists of
	elements of column i starting at row StartRow and continuing every
	Interval elements.
	@author D.K.
	@SeeAlso <See Method=ExtractVectorFromRow>
        @SeeAlso <See Method=GetColumn>
    }
    procedure ExtractVectorFromColumn(AVector: TVector;
      j, StartRow, Interval: integer);
    {** Changes the dimensions of a matrix.
        Call Resize to change the dimensions of a matrix. If NewRowCount or
	NewColCount is less than RowCount or ColCount, the excess rows or
	columns are cropped; if greater than RowCount or ColCount, the new extra
	rows or columns are filled with zero.
	@author D.K., A.X.
    }
    procedure Resize(NewRowCount, NewColCount: Integer);
    {** Assigns the same value to all elements of the matrix.
        Use Fill to assign Value to all elements of the matrix.
	@SeeAlso <See Method=Zero>
    }
    procedure Fill(Value: Real);
    {** Assigns zero to all the elements of the matrix.
        Zero is the same as Fill(0.0). The use of Zero is deprecated; use Fill
	instead.
	@SeeAlso <See Method=Fill>
    }
    procedure Zero;
    {** Copies a submatrix of a matrix.
        Use CopyFrom to copy the submatrix of Source whose top-left element is
	at (SourceStartRow, SourceStartCol) into this matrix, starting at
	(DestStartRow, DestStartCol). If the copied subset would reach outside
	the dimensions of the matrix, an exception is raised.
	@author D.K.
    }
    procedure CopyFrom(Source: TMatrix; SourceStartRow, SourceStartCol,
      RowCount, ColCount, DestStartRow, DestStartCol: Integer);
    {** Performs matrix addition.
	Use Add to add AMatrix to this matrix, which is overwritten. AMatrix and
	this matrix must have the same dimensions, or an exception is raised.
	@author A.X.
	@SeeAlso <See Method=Subtract>
    }
    procedure Add(AMatrix: TMatrix);
    {** Performs matrix subtraction.
        Use Subtract to perform matrix subtraction. If OperandPosition is
	maLeft, (AMatrix)-(this matrix) is performed; if OperandPosition is
	maRight, (this matrix)-(AMatrix) is performed. The result overwrites
	this matrix. The two matrices must have the same dimensions, or an
	exception is raised.
	@author A.X.
	@SeeAlso <See Method=Add>
    }
    procedure Subtract(AMatrix: TMatrix; OperandPosition: TOperandPosition);
    {** Multiplies the matrix with a scalar.
        Use ScalarMultiply to multiply the matrix with Scalar. The result
	overwrites the matrix.
	@author D.K.
    }
    procedure ScalarMultiply(Scalar: Real);
    {** Performs matrix multiplication.
        Use Multiply to perform matrix multiplication. If OperandPosition is
	maLeft, (AMatrix)*(this matrix) is performed; if OperandPosition is
	maRight, (this matrix)*(AMatrix) is performed. The result overwrites
	this matrix. The RowCount of the matrix on the left must equal the
	ColCount of the matrix on the right, or an exception is raised.
	@author D.K.,A.X.
	@SeeAlso <See Method=Add>
    }
    procedure Multiply(AMatrix: TMatrix; OperandPosition: TOperandPosition);
    {** Raises the matrix to an integer exponent.
        Use ScalarIntPower to raise all elements of the matrix to Exponent. The
	matrix is overwritten with the result.
	@author D.K.
    }
    procedure ScalarIntPower(Exponent: Integer);
    {** Transposes the matrix.
        Use Transpose to transpose the matrix. The result overwrites the
	original matrix.
    }
    procedure Transpose; virtual;
    {** Calculates the inverse of a square matrix.
        Use Invert to invert a square matrix and overwrite the matrix with its
	inverse. If the matrix is not square, Exception is raised; if it is not
	invertible, EZeroDivide is raised; if there is not enough memory,
	EOutOfMemory is raised.<p>
        The implementation of Invert is based on functions dgetri and dtrti2
        of the LAPACK library (http://www.netlib.org/lapack/).  Note that the
        LAPACK library also has a "blocked" version of the algorithm, which is
        presumably faster; but this implementation only implements the
        unblocked version.
    }
    procedure Invert;
    {** Returns the number of elements that are greater than a threshold.
        Use CountLarger to determine the number of elements of the matrix that
	are greater than Threshold.
    }
    function CountLarger(Threshold: Real): LongInt;
    {** Determines whether the matrix is symmetric.
        Use IsSymmetric to determine whether the matrix is symmetric.
	@author D.K.
    }
    function IsSymmetric: Boolean;
    {** Performs LU decomposition.
        This method computes P, L, and U such that A=P*L*U, where A is this
        matrix, P is a permutation matrix, L is lower triangular with unit
        diagonal elements, and U is upper triangular. The matrix must be
        square (NxN), otherwise an exception is raised. The method replaces
        the matrix with another matrix such that the upper triangle contains
        U, and the lower triangle without the diagonal contains L (the unit
        diagonal elements of L are not stored). Ipiv is a size N output vector
        containing the pivot indices; that is, row i of the matrix was
        interchanged with row Ipiv(i).<p>
        For theory on LuDecompose, see Press et al., Numerical Recipes in C,
        Second Edition, Cambridge University Press, 1992, pages 43-46. The
        implementation of this method is based on the dgetf2 function of
        the LAPACK library (http://www.netlib.org/lapack/).
        Note that the LAPACK library also has a "blocked" version of the
        algorithm, which is presumably faster; but this implementation only
        implements the unblocked version.
	@author A.X.
	@SeeAlso <See Method=Invert>
    }
    procedure LuDecompose(var Ipiv: TIntVector);
    {** Performs Cholesky decomposition of a positive-definite symmetric matrix.
        CholeskyDecompose replaces the matrix with its Cholesky decomposition
        L, such that A=L*L' and L is lower triangular.<p>
	For more information on CholeskyDecompose, see Press et al., Numerical
	Recipes in C, Second Edition, Cambridge University Press, 1992, pages
        96-98. The implementation of this method is based on the dpotf2
        function of the LAPACK library (http://www.netlib.org/lapack/).  Note
        that the LAPACK library also has a "blocked" version of the algorithm,
        which is presumably faster; but this implementation only implements
        the unblocked version.
	@author A.X.
    }
    procedure CholeskyDecompose;
    {** Checks whether the matrix is lower triangular.
        IsLowerTriangular returns True if the matrix is lower triangular, and
	False in all other cases, including when the matrix is not square.
        @author A.X.
	@SeeAlso <See Method=IsUpperTriangular>
    }
    function IsLowerTriangular: Boolean;
    {** Checks whether the matrix is upper triangular.
        IsUpperTriangular returns True if the matrix is upper triangular, and
	False in all other cases, including when the matrix is not square.
        @author A.X.
	@SeeAlso <See Method=IsLowerTriangular>
    }
    function IsUpperTriangular: Boolean;
    {** Calculates the mean value of the matrix's elements.
        @author D.K.
	@SeeAlso <See Method=Variance>
	@SeeAlso <See Method=Moment>
	@SeeAlso <See Method=Skewness>
	@SeeAlso <See Method=Kurtosis>
	@SeeAlso <See Method=Variation>
    }
    function Mean: Real;
    {** Calculates the variance of the matrix's elements.
        @author D.K.
	@SeeAlso <See Method=Mean>
	@SeeAlso <See Method=Moment>
	@SeeAlso <See Method=Skewness>
	@SeeAlso <See Method=Kurtosis>
	@SeeAlso <See Method=Variation>
    }
    function Variance: Real;
    {** Calculates the moment of the specified order of the matrix's elements.
        @author D.K.
	@SeeAlso <See Method=Mean>
	@SeeAlso <See Method=Variance>
	@SeeAlso <See Method=Skewness>
	@SeeAlso <See Method=Kurtosis>
	@SeeAlso <See Method=Variation>
    }
    function Moment(Order: Integer): Real;
    {** Calculates the variation of the matrix's elements.
        The variation is the standard deviation divided by the mean.
        @author D.K.
	@SeeAlso <See Method=Mean>
	@SeeAlso <See Method=Moment>
	@SeeAlso <See Method=Variance>
	@SeeAlso <See Method=Skewness>
	@SeeAlso <See Method=Kurtosis>
    }
    function Variation: Real;
    {** Calculates the skewness of the matrix's elements.
        @author D.K.
	@SeeAlso <See Method=Mean>
	@SeeAlso <See Method=Variance>
	@SeeAlso <See Method=Moment>
	@SeeAlso <See Method=Kurtosis>
	@SeeAlso <See Method=Variation>
    }
    function Skewness: Real;
    {** Calculates the kurtosis of the matrix's elements.
        @author D.K.
	@SeeAlso <See Method=Mean>
	@SeeAlso <See Method=Variance>
	@SeeAlso <See Method=Skewness>
	@SeeAlso <See Method=Moment>
	@SeeAlso <See Method=Variation>
    }
    function Kurtosis: Real;
    {** Calculates the determinant of a square matrix.
        If the matrix is not square, an exception is raised.
        @Author A.E.
    }
    function Determinant: Real;
    function Covariance(AMatrix: TMatrix): Real;
    function Correlation(AMatrix: TMatrix): Real;
    function CrossMoment(Order1, Order2: Integer; AMatrix: TMatrix): Real;
    procedure LMoments (Biased: Boolean;
       var Lamda1, Lamda2, Lamda3, Lamda4: Real);
    function EuclNorm: Real;

  end;

  {** Stores a vector.
      TVector is a special case of TMatrix with either one row or one column.<p>
      @author D.K.
      @SeeAlso <See Class=TMatrix>
  }
  TVector = class(TMatrix)
  private
    FIsColumn: Boolean;
  protected
    function GetElement(i: Integer): Real;
    procedure SetElement(i: Integer; Value: Real);
  public
    {** Specifies whether the vector is a column.
        Normally it is not needed to use IsColumn; RowCount and ColCount can be
        used instead. The primary reason for the existence of this property
        is that when the vector contains only one element, it cannot be
        determined from RowCount and ColCount whether it is a column or a row,
        and it would thus not be known how to resize it or how to append
        another vector.<p>
        This property is read-only; use Transpose to alter it.
        @SeeAlso <See Method=Transpose>
        @SeeAlso <See Property=TMatrix.RowCount>
        @SeeAlso <See Property=TMatrix.ColCount>
        @SeeAlso <See Method=Resize>
        @SeeAlso <See Method=Append>
    }
    property IsColumn: Boolean read FIsColumn;
    {** Changes the size of a vector.
        Call Resize to change the number of elements of a vector. If NewSize
	is less than Size, the excess elements are cropped; if greater than
	Size, the new extra elements are filled with zero.<p>
	A vector can also be resized by setting the Size property.
	@SeeAlso <See Property=Size>
    }
    procedure Resize(NewSize: Integer);
    {** Returns or sets the size of a vector.
        Use Size to determine the number of elements of the vector. Setting Size
	is the same as using the Resize method.
	@SeeAlso <See Method=Resize>
    }
    property Size: Integer read GetSize write Resize;
    {** Returns or sets a vector element.
	Read or set E to access element i of the vector. Element numbering
	starts at 1. If i is greater than Size an exception is raised.<p>
	E is the default property, so you don't normally need to write
	AVector.E[i]; write AVector[i] instead.
	@SeeAlso <See Property=Size>
    }
    property E[i: Integer]: Real read GetElement write SetElement; default;
    {** Copies a TVector object.
        Assign is the same as the inherited except that:
        <ul>
          <li>If Source is a TVector, it also copies IsColumn.
          <li>If Source is not a TVector but is a TMatrix, it requires that one
              of RowCount and ColCount are 1, or an exception is raised.
        </ul>
	@author A.X.
        @SeeAlso <See Method=TMatrix.Assign>
        @SeeAlso <Jump File=Delphi5.hlp K="TPersistent,Assign" Text=Assign>
    }
    procedure Assign(Source: TPersistent); override;
    {** Creates a TVector object.
	Call Create to create a new TVector object.  TVector objects created
	with the version of Create without arguments should not be used before
	resizing.<p>
	When creating a TVector object, it is always created as a matrix with
	one column and many rows; use Transpose after creation if the opposite
	is required.
	@SeeAlso <See Method=CreateFromArray>
	@SeeAlso <See Method=Transpose>
    }
    constructor Create; overload;
    {** (Overloaded method - no title required).
        Call Create to create a new TVector object.<p>
	Create does not initialize the elements; to initialize them, use Fill or
	Zero.<p>
	When creating a TVector object, it is always created as a matrix with
	one row and many columns; use Transpose after creation if the opposite
	is required.
	@SeeAlso <See Method=TMatrix.Fill>
	@SeeAlso <See Method=TMatrix.Zero>
    }
    constructor Create(Size: Integer); overload;
    {** Creates a TVector object using an existing one-dimensional array.
        For more information on CreateFromArray, see TMatrix.CreateFromArray.
	When creating a TVector object, it is always created as a matrix with
	one column and many rows; use Transpose after creation if the opposite
	is required.
	@SeeAlso <See Method=TMatrix.CreateFromArray>
	@SeeAlso <See Method=Transpose>
    }
    constructor CreateFromArray(Size: Integer; PArrayOfDouble: Pointer;
      Method: TCreateFromArrayMethod);
    {** Appends a vector to this vector.
        Call Append to append AVector to this vector, which is overwritten.
	@author D.K.
    }
    procedure Append(AVector: TVector);
    {** Copies a subvector of a vector.
        Use CopyFrom to copy the subvector of Source whose first element is
	SourceStart, into this vector, starting at DestStart. If the copied
	subset would reach outside the size of the vector, an exception is
	raised.
	@author D.K.
    }
    procedure CopyFrom(Source: TVector; SourceStart, ElementCount,
      DestStart: integer);
    {** Transposes the vector.
        Vectors are initially created as one column with many rows. Use
	Transpose if this needs to be changed (for example, to multiply with a
	TMatrix object).
	@author D.K.
    }
    procedure Transpose; override;
    {** Sorts the vector.
        Use Sort to sort the vector elements in asceding order.
	@author D.K.
    }
    procedure Sort;
    {** Shifts a subvector within the vector.
	Use Shift to shift the subvector specified by Start and End.  The
	subvector is shifted rightward or downward by ElementCount; to shift to
	the opposite direction, use a negative value for ElementCount. Elements
	that are not overwritten by the shifting remain unchanged (including not
	overwritten elements of the original subvector). If after shifting the
	subvector would reach outside the vector boundaries, an exception is
	raised. 
	@author A.X.
    }
    procedure Shift(StartElement, EndElement, ElementCount: Integer);
    function AutoCovariance(Order: Integer): Real;
    function AutoCorrelation(Order: Integer): Real;
    function WeightedNorm(AVector: TVector): Real;
    function WeightedMaxNorm(AVector: TVector): Real;
    function OrderOfMax: Integer;
    function OrderOfMin: Integer;
  end;

  {** Stores an integer vector.
      TIntVector is an alternative implementation of TVector for
      vectors with integer elements only; TIntVersion is more
      efficient than TVector.
      @author A.E.
      @SeeAlso <See Class=TVector>
      @SeeAlso <See Class=TMatrix>
      @SeeAlso <See Class=TIntegerList>
  }
  TIntVector = class(TPersistent)
  private
    FSize: Integer;
    FElements: PMatrixIntArray;
  protected
    function GetElement(i: Integer): Integer;
    procedure SetElement(i: Integer; Value: Integer);
  public
    {** Changes the size of a vector.
        Call Resize to change the number of elements of a vector. If NewSize
	is less than Size, the excess elements are cropped; if greater than
	Size, the new extra elements are filled with zero.<p>
	A vector can also be resized by setting the Size property.
	@SeeAlso <See Property=Size>
    }
    procedure Resize(NewSize: Integer);
    {** Returns or sets the size of a vector.
        Use Size to determine the number of elements of the vector. Setting Size
	is the same as using the Resize method.
	@SeeAlso <See Method=Resize>
    }
    property Size: Integer read FSize write Resize;
    {** Returns a pointer to the array of elements.
        Use PElements to access directly the internal array which stores the
	elements.
    }
    property PElements: PMatrixIntArray read FElements;
    {** Returns or sets a vector element.
	Read or set E to access element i of the vector. Element numbering
	starts at 1. If i is greater than Size an exception is raised.<p>
	E is the default property, so you don't normally need to write
	AIntVector.E[i]; write AIntVector[i] instead.
	@SeeAlso <See Property=Size>
    }
    property E[i: Integer]: Integer read GetElement write SetElement; default;
    {** Creates a TIntVector object.
	Call Create to create a new TIntVector object.
	TIntVector objects created with the version of Create
	without arguments should not be used before resizing.<p>
	@SeeAlso <See Property=Size>
	@SeeAlso <See Method=Resize>
    }
    constructor Create; overload;
    {** (Overloaded method - no title required).
	Call Create to create a new TIntVector object. Create does not
	initialize the elements; to initialize them, use Fill.<p>
	@SeeAlso <See Method=Fill>
    }
    constructor Create(Size: Integer); overload;
    {** Destroys the TIntVector object.
        Do not use Destroy; use Free instead, which checks if the object
	reference is nil.
        @SeeAlso <Jump File=Delphi5.hlp K="TObject,Free" Text=TObject.Free>
    }
    destructor Destroy; override;
    {** Assigns the same value to all elements of the vector.
        Use Fill to assign Value to all elements of the vector.
    }
    procedure Fill(Value: Integer);
  end;


implementation

{$C+}

constructor TMatrix.Create;
begin
  inherited Create;
  FRowCount := 0;
  FColCount := 0;
  FOwnsElements := True;
  FElements := nil;
end;

constructor TMatrix.Create(RowCount, ColCount: Integer);
begin
  inherited Create;
  FRowCount := RowCount;
  FColCount := ColCount;
  FOwnsElements := True;
  FElements := nil;
  GetMem(FElements, RowCount*ColCount*SizeOf(Real));
end;

constructor TMatrix.CreateFromArray(RowCount, ColCount: Integer;
  PArrayOfDouble: Pointer; Method: TCreateFromArrayMethod);
begin
  inherited Create;
  FRowCount := RowCount;
  FColCount := ColCount;
  FElements := nil;
  if Method = maDontOwnArray then
  begin
    FElements := PArrayOfDouble;
    FOwnsElements := False;
  end else if Method = maOwnArray then
  begin
    FElements := PArrayOfDouble;
    FOwnsElements := True;
  end else if Method = maCopyArray then
  begin
    GetMem(FElements, RowCount*ColCount*SizeOf(Real));
    Move(PArrayOfDouble^, FElements^, RowCount*ColCount*SizeOf(Real));
    FOwnsElements := True;
  end else
    Assert(False);
end;

destructor TMatrix.Destroy;
begin
  if OwnsElements and Assigned(FElements) then
    FreeMem(FElements);
  inherited Destroy;
end;

resourcestring
  rsMatrixIndexOutOfBounds = 'Matrix index out of bounds.';

function TMatrix.GetElement(i, j: Integer): Real;
begin
  if (i<1) or (j<1) or (i>RowCount) or (j>ColCount) then
    raise EListError.Create(rsMatrixIndexOutOfBounds+' ('+IntToStr(i)+', '+
      IntToStr(j)+')');
  Result := FElements^[(i-1)*ColCount + j-1];
end;


procedure TMatrix.SetElement(i, j: Integer; Value: Real);
begin
  if (i<1) or (j<1) or (i>RowCount) or (j>ColCount) then
    raise EListError.Create(rsMatrixIndexOutOfBounds+' ('+IntToStr(i)+', '+
      IntToStr(j)+')');
  FElements^[(i-1)*ColCount + j-1] := Value;
end;

function TMatrix.GetSize: Integer;
begin
  Result := FRowCount*FColCount;
end;

function TMatrix.IsSymmetric: Boolean;
const Tolerance = 1e-16;
var i, j: Integer;
begin
  Result := FRowCount = FColCount;
  if Result then
    for i := 1 to RowCount do
      for j := i + 1 to ColCount do
      begin
	Result := Result and (Abs(E[i, j] - E[j, i]) < Tolerance);
	if not Result then Break;
      end;
end;

procedure TMatrix.Shift(StartRow, StartCol, EndRow, EndCol, ShiftRows,
  ShiftCols: Integer);
var i, j: integer;
begin
  if ShiftRows>=0 then i := EndRow else i := StartRow;
  while (i>=StartRow) and (i<=EndRow) do
  begin
    if ShiftCols>=0 then j := EndCol else j := StartCol;
    while (j>=StartCol) and (j<=EndCol) do
    begin
      E[i+ShiftRows, j+ShiftCols] := E[i, j];
      if ShiftCols>=0 then Dec(j) else Inc(j);
    end;
    if ShiftRows>=0 then Dec(i) else Inc(i);
  end;
end;

procedure TMatrix.GetRow(AVector: TVector; i: Integer);
var j: Integer;
begin
  AVector.Resize(ColCount);
  for j := 1 to ColCount do
    AVector[j] := E[i, j];
end;


procedure TMatrix.GetColumn(AVector: TVector; j: Integer);
var i: Integer;
begin
  AVector.Resize(RowCount);
  for i := 1 to RowCount
    do AVector[i] := E[i, j];
end;

procedure TMatrix.ExtractVectorFromRow(AVector: TVector;
  i, StartCol, Interval: integer);
var j: Integer;
begin
  AVector.Resize((ColCount - StartCol) div Interval + 1);
  for j := 1 to AVector.Size do
    AVector[j] := E[i, StartCol + Interval * (j - 1)];
end;

procedure TMatrix.ExtractVectorFromColumn(AVector: TVector;
  j, StartRow, Interval: integer);
var i: Integer;
begin
  AVector.Resize((RowCount - StartRow) div Interval + 1);
  for i := 1 to AVector.Size do
    AVector[i] := E[StartRow + Interval * (i - 1), j];
end;

procedure TMatrix.Resize(NewRowCount, NewColCount: Integer);
var
  AMatrix: TMatrix;
  MinRowCount, MinColCount: Integer;
resourcestring
  rsCannotResizeUnlessOwnsElements = 'Cannot change the number of elements '+
    'of a matrix which does not own the elements.';
begin
  MinRowCount := NewRowCount;
  if MinRowCount>RowCount then MinRowCount := RowCount;
  MinColCount := NewColCount;
  if MinColCount>ColCount then MinColCount := ColCount;
  AMatrix := nil;
  try
    AMatrix := TMatrix.Create(MinRowCount, MinColCount);
    AMatrix.CopyFrom(Self, 1, 1, MinRowCount, MinColCount, 1, 1);
    if Size <> NewRowCount * NewColCount then
    begin
      if not OwnsElements then
        raise Exception.Create(rsCannotResizeUnlessOwnsElements);
      ReallocMem(FElements, NewRowCount*NewColCount*SizeOf(Real));
    end;
    FColCount := NewColCount;
    FRowCount := NewRowCount;
    CopyFrom(AMatrix, 1, 1, MinRowCount, MinColCount, 1, 1);
  finally
    AMatrix.Free;
  end;
end;

procedure TMatrix.Assign(Source: TPersistent);
var
  SourceMatrix: TMatrix;
begin
  if not (Source is TMatrix) then
  begin
    inherited Assign(Source);
    Exit;
  end;
  SourceMatrix := TMatrix(Source);
  if OwnsElements then
    ReallocMem(FElements, SourceMatrix.Size*SizeOf(Real))
  else
  begin
    GetMem(FElements, SourceMatrix.Size*SizeOf(Real));
    FOwnsElements := True;
  end;
  FRowCount := SourceMatrix.RowCount;
  FColCount := SourceMatrix.ColCount;
  Move(SourceMatrix.FElements^, FElements^, Size*SizeOf(Real));
end;

function TMatrix.Ascii: string;
const LF = #13#10;
var i, j: integer;
begin
  Result := LF;
  for i := 1 to RowCount do
  begin
    for j := 1 to ColCount do
      Result := Result + Format('%:12.6g ', [E[i, j]]);
    Result := Result + LF;
  end;
end;

function TMatrix.BriefInfo: string;
begin;
  Result := Format('Ptr: %p; RowCount: %d; ColCount: %d; Size: %d',
    [FElements, RowCount, ColCount, Size]);
  if Size<=0 then Exit;
  if (RowCount = 1) or (ColCount = 1) then
    Result := Result + Format('; Data: %g, ..., %g',
      [FElements^[0], FElements^[Size-1]])
  else
    Result := Result + Format('; Row 1: %g, ..., %g; Row %d: %g, ..., %g',
      [E[1,1], E[1, ColCount], RowCount, E[RowCount,1], E[RowCount, ColCount]]);
end;

procedure TMatrix.Fill(Value: Real);
var i, j: Integer;
begin
  for i := 1 to RowCount do
    for j := 1 to ColCount do
      E[i, j] := Value;
end;

procedure TMatrix.Zero;
begin
  Fill(0.0);
end;

procedure TMatrix.CopyFrom(Source: TMatrix; SourceStartRow, SourceStartCol,
      RowCount, ColCount, DestStartRow, DestStartCol: Integer);
var i, j: integer;
begin
  if (SourceStartRow+RowCount-1 > Source.RowCount)
  or (SourceStartCol+ColCount-1 > Source.ColCount)
  or (DestStartRow+RowCount-1 > Self.RowCount)
  or (DestStartCol+ColCount-1 > Self.ColCount)
  then
    raise EListError.Create(rsMatrixIndexOutOfBounds);
  for i := 1 to RowCount do
    for j := 1 to ColCount do
      E[DestStartRow + i - 1, DestStartCol + j - 1] :=
      Source[SourceStartRow + i - 1, SourceStartCol + j - 1];
end;

resourcestring
  rsInconsistentMatrixDimensions = 'Inconsistent matrix dimensions';

procedure TMatrix.Add(AMatrix: TMatrix);
var i, j: Integer;
begin
  if (ColCount<>AMatrix.ColCount) or (RowCount<>AMatrix.RowCount) then
    raise Exception.Create(rsInconsistentMatrixDimensions);
  for i := 1 to RowCount do
    for j := 1 to ColCount do
      E[i, j] := E[i, j]+AMatrix[i, j];
end;

procedure TMatrix.Subtract(AMatrix: TMatrix; OperandPosition:
  TOperandPosition);
var
  i, j: Integer;
  LeftMatrix, RightMatrix: TMatrix;
begin
  if (ColCount<>AMatrix.ColCount) or (RowCount<>AMatrix.RowCount) then
    raise Exception.Create(rsInconsistentMatrixDimensions);
  LeftMatrix := Self;
  RightMatrix := Self;
  if OperandPosition=maLeft then LeftMatrix := AMatrix
  else RightMatrix := AMatrix;
  for i := 1 to RowCount do
    for j := 1 to ColCount do
      E[i, j] := LeftMatrix[i, j]-RightMatrix[i, j];
end;

procedure TMatrix.ScalarMultiply(Scalar: Real);
var i, j: Integer;
begin
  for i := 1 to RowCount do
    for j := 1 to ColCount do
      E[i, j] := Scalar * E[i, j];
end;


procedure TMatrix.Multiply(AMatrix: TMatrix; OperandPosition:
  TOperandPosition);
var
  i, j, k: integer;
  Sum: Real;
  LeftMatrix, RightMatrix, ResultMatrix: TMatrix;
begin
  LeftMatrix := Self;
  RightMatrix := Self;
  if OperandPosition=maLeft then LeftMatrix := AMatrix
  else RightMatrix := AMatrix;
  if LeftMatrix.ColCount<>RightMatrix.RowCount then
    raise Exception.Create(rsInconsistentMatrixDimensions);
  ResultMatrix := nil;
  try
    ResultMatrix := TMatrix.Create(LeftMatrix.RowCount, RightMatrix.ColCount);
    for i := 1 to LeftMatrix.RowCount do
      for j := 1 to RightMatrix.ColCount do
      begin
	Sum := 0;
	for k := 1 to LeftMatrix.ColCount do
	  Sum := Sum + LeftMatrix[i, k] * RightMatrix[k, j];
	ResultMatrix[i, j] := Sum;
      end;
    Assign(ResultMatrix);
  finally
    ResultMatrix.Free;
  end;
end;

procedure TMatrix.ScalarIntPower(Exponent: Integer);
var i, j: Integer;
begin
  for i := 1 to RowCount do
    for j := 1 to ColCount do
      E[i, j] := IntRaise(E[i, j], Exponent);
  { ??? Why IntRaise and not IntPower? }
end;

function TMatrix.EuclNorm: Real;
const SafeLimit=100;
var
 i, j, k: Integer;
 tsum, sum: Real;
begin
  sum := 0; tsum := 0; k := 0;
  for i := 1 to RowCount do
    for j:=1 to ColCount do
    begin
      tsum := tsum + IntRaise(e[i, j], 2);
      k := k + 1;
      if k = SafeLimit then
      begin
        sum := sum + tsum;
        k := 0; tsum := 0;
      end;
    end;
  sum := sum + tsum;
  Result:=SQRT(sum);
end;

procedure TMatrix.Transpose;
var
  i, j: Integer;
  AMatrix: Tmatrix;
begin
  AMatrix := nil;
  try
    AMatrix := TMatrix.Create(ColCount, RowCount);
    for i := 1 to RowCount do
      for j := 1 to ColCount do
        AMatrix[j, i] := E[i, j];
    Assign(AMatrix);
  finally
    AMatrix.Free;
  end;
end;

function TMatrix.IsLowerTriangular: Boolean;
var i, j: Integer;
begin
  Result := False;
  if RowCount<>ColCount then Exit;
  for i := 1 to RowCount do
    for j := i + 1 to RowCount do
      if E[i, j]<>0 then Exit;
  Result := True;
end;

function TMatrix.IsUpperTriangular: Boolean;
var i, j: Integer;
begin
  Result := False;
  if RowCount<>ColCount then Exit;
  for i := 1 to RowCount do
    for j := i - 1 downto 1 do
      if E[i, j]<>0 then Exit;
  Result := True;
end;

procedure TMatrix.Invert;

  procedure InvertUpperTriangular;
  { Inverts the upper triangle ignoring the lower one. }
  var 
    j, k, p: Integer;
    a, ajj: Real;
  begin
    for j := 1 to RowCount do
    begin
      E[j,j] := 1/E[j,j];
      ajj := -E[j,j];

      { Compute elements 1:j-1 of j-th column. }
      for k := 1 to j-1 do
      begin
        a := 0.0;
        for p := k to j-1 do
          a := a + E[k,p]*E[p,j];
        E[k,j] := a*ajj;
      end;
    end;
  end;

  procedure InvertLowerTriangular;
  { Inverts the lower triangle ignoring the upper one. }
  var
    j, k, p: Integer;
    a, ajj: Real;
  begin
    for j := RowCount downto 1 do
    begin
      E[j,j] := 1/E[j,j];
      ajj := -E[j,j];

      { Compute elements j+1:n of j-th column. }
      for k := RowCount downto j+1 do
      begin
        a := 0.0;
        for p := k downto j+1 do a := a + E[k,p]*E[p,j];
        E[k,j] := a*ajj;
      end;
    end;
  end;

  procedure InvertNormal;
  var
    i, j, k, p: Integer;
    Ipiv: TIntVector;
    w: TVector;
    a: Real;
  begin
    Ipiv := nil;
    w := nil;
    try
      Ipiv := TIntVector.Create(RowCount);
      w := TVector.Create(RowCount);
      LuDecompose(Ipiv);
      { Form inv(U). }
      InvertUpperTriangular;
      { Solve the equation inv(A)*L = inv(U) for inv(A). }
      for j := ColCount-1 downto 1 do
      begin
        { Copy column j of L to w and replace with zeros. }
        for i := j+1 to RowCount do
        begin
          w[i] := E[i,j];
          E[i,j] := 0.0;
        end;
        { Compute column j of inv(A). }
        for k := 1 to RowCount do
        begin
          a := 0;
          for p := j+1 to RowCount do a := a + E[k,p]*w[p];
          E[k,j] := E[k,j] - a;
        end;
      end;
      { Apply column interchanges. }
      for j := ColCount-1 downto 1 do
        if Ipiv[j]<>j then
          for k := 1 to RowCount do
          begin
            a := E[k,j];
            E[k,j] := E[k,Ipiv[j]];
            E[k,Ipiv[j]] := a;
          end;
    finally
      w.Free();
      Ipiv.Free();
    end;
  end;

begin
  if RowCount<>ColCount then
    raise Exception.Create(rsInconsistentMatrixDimensions);

  if IsLowerTriangular then
    InvertLowerTriangular
  else if IsUpperTriangular then
    InvertUpperTriangular
  else
    InvertNormal;
end;

resourcestring
  rsMatrixIsSingular = 'Matrix is singular.';

procedure TMatrix.LuDecompose (var Ipiv: TIntVector);
const accur = 1e-10;
var
  j, jp, k, p: Integer;
  a, dmax: Real;
begin
  if RowCount<>ColCount then
    raise Exception.Create(rsInconsistentMatrixDimensions);
  Ipiv.Size := RowCount;
  for j := 1 to RowCount do
  begin
    { Determine Ipiv[j] = index of max absolute value in E[j,j]...E[j,n] }
(*
    Not fully test for the change jp := 0 -> jp := 1
    See bug 2898
    jp := 0;
*)
    jp := 1;
    dmax := 0.0;
    for k := j to RowCount do
      if Abs(E[k,j])>dmax then
      begin
        jp := k;
        dmax := Abs(E[k,j]);
      end;
    Ipiv[j] := jp;
    { Test for singularity }
    if Abs(E[jp,j])<accur then raise EZeroDivide.Create(rsMatrixIsSingular);

    { Swap rows j and jp }
    if jp<>j then
      for k := 1 to ColCount do
      begin
        a := E[j,k];
        E[j,k] := E[jp,k];
        E[jp,k] := a;
      end;

    if j<RowCount then
    begin
      { Compute elements j+1 to m of j-th column }
      for k := j+1 to RowCount do E[k,j] := E[k,j]/E[j,j];

      { Update trailing submatrix }
      for k := j+1 to RowCount do
        for p := j+1 to ColCount do
          E[k,p] := E[k,p] - E[k,j] * E[j,p];
    end;
  end;
end;

resourcestring
  rsMatrixIsNotSymmetric = 'Matrix is not symmetric';
  rsMatrixIsNotPositiveDefinite = 'Matrix is not positive definite';

procedure TMatrix.CholeskyDecompose;
var
  j, k, p: Integer;
  a, ajj: Real;
begin
  { Note: It is nothing more than a waste of time to check that the matrix is
    symmetric at the beginning, and to fill the irrelevant part of the matrix
    with zeros at the end. All we should care about is the lower triangle; we
    should be ignoring the rest. However, we are doing it in order to reproduce
    past behaviour of the method exactly. If you fix that, take care that
    thelma users update their code if necessary. }
  if not IsSymmetric then
    raise Exception.Create(rsMatrixIsNotSymmetric);
  for j := 1 to RowCount do
  begin
    { Compute E[j, j] and test for non-positive-definiteness. }
    a := 0;
    for k := 1 to j-1 do a := a + E[j,k]*E[j,k];
    ajj := E[j, j] - a;
    if ajj<=0 then raise Exception.Create(rsMatrixIsNotPositiveDefinite);
    ajj := SQRT(ajj);
    E[j, j] := ajj;

    if j = RowCount then Continue;

    { Compute elements j+1:N of column j. }
    
    for k := j+1 to RowCount do
    begin
      a := 0;
      for p := 1 to j-1 do a := a + E[k,p]*E[j,p];
      E[k,j] := (E[k,j] - a)/ajj;
    end;
  end;

  { Fill upper triangle (except diagonal) with zeros. }
  for k := 1 to RowCount do
    for j := k+1 to ColCount do
      E[k, j] := 0;

end;

function TMatrix.Determinant: Real;
var
  i: Integer;
  Ipiv: TIntVector;
begin
  { The determinant is the product of the diagonal elements of U (where U is
    the U of the LU decomposition), negated if the number of permutations in
    the partial pivoting is odd.
  }
  Result := 1.0;
  Ipiv := TIntVector.Create(RowCount);
  try
    for i := 1 to RowCount do Ipiv[i] := i;
    if not IsUpperTriangular and not IsLowerTriangular then
      LuDecompose(Ipiv);
    for i := 1 to RowCount do
    begin
      Result := Result*E[i, i];
      if i<>Ipiv[i] then Result := -Result;
    end;
  finally
    Ipiv.Free;
  end;
end;

function TMatrix.CountLarger(Threshold: Real): LongInt;
begin
  Result := stat2.CountLarge(Threshold, FElements^, Size);
end;

function TMatrix.Mean: Real;
begin
  Result := stat2.mean(FElements^, Size);
end;

function TMatrix.Variance: Real;
begin
  Result := stat2.variance(FElements^, Size);
end;

function TMatrix.Moment(Order: Integer): Real;
begin
  Result := stat2.CentralMoment(Order, FElements^, Size);
end;

function TMatrix.Variation: Real;
begin
  Result := stat2.Variation(FElements^, Size);
end;

function TMatrix.Skewness: Real;
begin
  Result := stat2.Skewness(FElements^, Size);
end;

function TMatrix.Kurtosis: Real;
begin
  Result := stat2.Kurtosis (FElements^, Size);
end;

function TMatrix.CrossMoment(Order1, Order2: Integer; AMatrix: TMatrix): Real;
begin
  if Size<>AMatrix.Size then
    raise Exception.Create(rsInconsistentMatrixDimensions);
  Result := stat2.CentralCrossMoment(Order1, Order2, FElements^,
    AMatrix.FElements^, Size);
end;

function TMatrix.Covariance(AMatrix: TMatrix): Real;
begin
  if Size<>AMatrix.Size then
    raise Exception.Create(rsInconsistentMatrixDimensions);
  Result := stat2.covariance(FElements^, AMatrix.FElements^, Size);
end;

function TMatrix.Correlation(AMatrix: TMatrix): Real;
begin
  result := stat2.correlation(FElements^, AMatrix.FElements^, Size);
end;

procedure TMatrix.LMoments (Biased: Boolean;
          var Lamda1, Lamda2, Lamda3, Lamda4: Real);
begin
  stat2.LMoments(FElements^, Size, biased, lamda1, lamda2, lamda3, lamda4);
end;

constructor TVector.Create;
begin
  inherited Create;
  FIsColumn := False;
end;

constructor TVector.Create(Size: Integer);
begin
  inherited Create(Size, 1);
  FIsColumn := True;
end;

constructor TVector.CreateFromArray(size: Integer; PArrayOfDouble: Pointer;
  Method: TCreateFromArrayMethod);
begin
  inherited CreateFromArray(Size, 1, PArrayOfDouble, Method);
  FIsColumn := True;
end;

resourcestring
  rsCantAssignTMatrixToTVector = 'Cannot assign a TMatrix to a TVector '+
    'unless one of its dimensions is 1.';

procedure TVector.Assign(Source: TPersistent);
begin
  if (Source is TMatrix) then
    with TMatrix(Source) do
      if (RowCount<>1) and (ColCount<>1) then
        raise EConvertError.Create(rsCantAssignTMatrixToTVector);
  inherited Assign(Source);
  if Source is TVector then
    FIsColumn := TVector(Source).IsColumn
  else if RowCount<>1 then
    FIsColumn := True
  else
    FIsColumn := False;
end;

resourcestring
  rsVectorIndexOutOfBounds = 'Vector index out of bounds.';

function TVector.GetElement(i: Integer): Real;
begin
  if (i<1) or (i>Size) then
    raise EListError.Create(rsVectorIndexOutOfBounds+' ('+IntToStr(i)+')');
  Result := FElements^[i-1];
end;

procedure TVector.SetElement(i: Integer; Value: Real);
begin
  if (i<1) or (i>Size) then
    raise EListError.Create(rsVectorIndexOutOfBounds+' ('+IntToStr(i)+')');
  FElements^[i-1] := Value;
end;

procedure TVector.Resize(NewSize: Integer);
begin
  if IsColumn then
    inherited Resize(NewSize, 1)
  else
    inherited Resize(1, NewSize);
end;

procedure TVector.Append(AVector: TVector);
var i, OldSize: integer;
begin
  OldSize := Size;
  Resize(Size + AVector.Size);
  for i := 1 to AVector.Size do E[OldSize+i] := AVector[i];
end;

procedure TVector.CopyFrom(Source: TVector; SourceStart, ElementCount,
  DestStart: Integer);
var
  i: integer;
begin
  if (SourceStart+ElementCount-1 > Source.Size)
  or (DestStart+ElementCount-1 > Self.Size)
  then
    raise EListError.Create(rsVectorIndexOutOfBounds);
  for i := 1 to ElementCount do
    E[DestStart + i - 1] := Source[SourceStart + i - 1];
end;

procedure TVector.Transpose;
var t: integer;
begin
  t := FRowCount;
  FRowCount := FColCount;
  FColCount := t;
  FIsColumn := not IsColumn;
end;

function TVector.AutoCovariance(Order: Integer): Real;
begin
  Result := stat2.autocovariance(Order, FElements^, Size);
end;

function TVector.AutoCorrelation(Order: Integer): Real;
begin
  Result := stat2.autocorrelation(Order, FElements^, size);
end;

procedure TVector.Sort;
begin
  stat2.QuickSortAsc(FElements^, Size);
end;

function TVector.OrderOfMax: Integer;
begin
  result := stat2.OrderOfMax(FElements^, size);
end;

function TVector.OrderOfMin: Integer;
begin
  result := stat2.OrderOfMin(FElements^, size);
end;

resourcestring
  rsInconsistentVectorDimensions = 'Inconsistent vector dimensions';

function TVector.WeightedMaxNorm(AVector: TVector): Real;
const SafeLimit=100;
var
 i: Integer;
 tmp, max: Real;
begin
  if Size<>AVector.Size then
    raise Exception.Create(rsInconsistentVectorDimensions);
  max := 0;
  for i := 1 to Size do
  begin
    tmp := sqr(e[i]) * AVector[i];
    if max < tmp then max := tmp
  end;
  Result:=SQRT(max);
end;

function TVector.WeightedNorm(AVector: TVector): Real;
const SafeLimit=100;
var
 i,  k: Integer;
 tsum, sum: Real;
begin
  if Size<>AVector.Size then
    raise Exception.Create(rsInconsistentVectorDimensions);
  sum := 0; tsum := 0; k := 0;
  for i := 1 to size do
  begin
    tsum := tsum + sqr(e[i]) * AVector[i];
    k := k + 1;
    if k = SafeLimit then
      begin
      sum := sum + tsum;
      k := 0; tsum := 0;
      end;
  end;
  sum := sum + tsum;
  Result := Sqrt(sum);
end;

procedure TVector.Shift(StartElement, EndElement, ElementCount: Integer);
begin
  if IsColumn then
    inherited Shift(StartElement, 1, EndElement, 1, ElementCount, 0)
  else
    inherited Shift(1, StartElement, 1, EndElement, 0, ElementCount);
end;

{******************************************************************************}

constructor TIntVector.Create;
begin
  inherited Create;
  FElements := nil;
  FSize := 0;
end;

constructor TIntVector.Create(Size: Integer);
begin
  inherited Create;
  GetMem(FElements, Size*SizeOf(integer));
  FSize := Size;
end;

destructor TIntVector.Destroy;
begin
  if Assigned(FElements) then
    FreeMem(FElements, Size*SizeOf(Integer));
  inherited Destroy;
end;

function TIntVector.GetElement(i: Integer): Integer;
begin
  if (i<1) or (i>Size) then
    raise EListError.Create(rsVectorIndexOutOfBounds+' ('+IntToStr(i)+')');
  Result := FElements^[i-1];
end;

procedure TIntVector.SetElement(i: Integer; Value: Integer);
begin
  if (i<1) or (i>Size) then
    raise EListError.Create(rsVectorIndexOutOfBounds+' ('+IntToStr(i)+')');
  FElements^[(i-1)] := Value;
end;

procedure TIntVector.Resize(NewSize: Integer);
var i, OldSize: Integer;
begin
  ReallocMem(FElements, NewSize*SizeOf(Integer));
  OldSize := Size;
  FSize := NewSize;
  for i := OldSize+1 to Size do
    E[i] := 0;
end;

procedure TIntVector.Fill(Value: Integer);
var i: Integer;
begin
  for i := 1 to Size do E[i] := Value;
end;

end.
