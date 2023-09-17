#pragma once

#include "LTemplate.h"

#define LTEMPLATE_H
#define MATHEMATICA

//#define TOOL_ENABLE_PROFILER

#define LAPACK_DISABLE_NAN_CHECK
#define ACCELERATE_NEW_LAPACK
#include <Accelerate/Accelerate.h>

//#define LAPACK_DISABLE_NAN_CHECK
//#include <cblas.h>
//#include <lapack.h>

#include "Tensors/Tensors.hpp"
#include "Tensors/MyBLAS.hpp"
#include "Tensors/Sparse.hpp"

using namespace Tensors;
using namespace Tools;

using Real    = Real64;
using Complex = Complex64;
using Int     = Int64;
using LInt    = Int64;


class SOCS
{
    public:
     
        using Solver_T = Sparse::CholeskyDecomposition<Real,Int,LInt>;
    
    private:
    
        std::unique_ptr<Solver_T> S;
    
    public:
    
        SOCS() = default;
    
        ~SOCS() = default;
    
    
    
        void Init(
            const mma::TensorRef<Int>  outer,
            const mma::TensorRef<Int>  inner,
            const mma::TensorRef<Real> values,
            const mma::TensorRef<Int>  perm,
            const Int thread_count
        )
        {
            S = std::make_unique<Solver_T>(
                outer.data(),
                inner.data(),
                perm.data(),
                static_cast<LInt>(outer.size()) - Scalar::One<LInt>,
                thread_count
            );
            
            S->NumericFactorization( values.data() );
        }
    
        void Refactorize( const mma::TensorRef<Real> values )
        {
            S->NumericFactorization( values.data() );
        }

        mma::TensorRef<Real> SolveVector( const mma::TensorRef<Real> b )
        {
            mma::TensorRef<Real> x = mma::makeVector<Real>( S->ColCount() );
            
            S->Solve( b.data(), x.data() );
            
            return x;
        }
    
        mma::MatrixRef<Real> SolveMatrix( const mma::MatrixRef<Real> B )
        {
            mma::MatrixRef<Real> X = mma::makeMatrix<Real>( S->ColCount(), B.cols() );
            
            S->Solve( B.data(), X.data(), B.cols() );
            
            return X;
        }
    
    
        mma::TensorRef<Int> Dimensions() const
        {
            
            const Int dims [2] = { S->RowCount(), S->ColCount() };
            
            return mma::makeVector<Int>(2 , &dims[0] );
        }
    
//        mma::TensorRef<Real> NonzeroValues() const
//        {
//            return S->Values().to_MTensorRef();
//        }
};



