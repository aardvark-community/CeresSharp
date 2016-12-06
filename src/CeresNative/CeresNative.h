#include "stdafx.h"
#include <iostream>

#define GLOG_NO_ABBREVIATED_SEVERITIES

#include <ceres/ceres.h>
#include <ceres/rotation.h>


using namespace std;
using ceres::AutoDiffCostFunction;
using ceres::CostFunction;
using ceres::SizedCostFunction;
using ceres::Problem;
using ceres::Solver;
using ceres::Solve;

#ifdef __GNUC__
#define DllExport(t) extern "C" t
#else
#define DllExport(t) extern "C"  __declspec( dllexport ) t __cdecl
#endif


typedef struct {

	int MaxIterations;
	int SolverType;
	int PrintProgress;
	double GradientTolerance;
	double FunctionTolerance;
	double ParameterTolerance;

} CeresOptions;


class CustomCostFunction : public CostFunction
{
private:
	int(*evaluate)(double const * const * parameters, double * residuals, double ** jacobians);

public:

	CustomCostFunction(int nParameterBlocks, int* parameterCounts, int residualCount, int(*eval)(double const * const * parameters, double * residuals, double ** jacobians))
	{
		for (int i = 0; i < nParameterBlocks; i++)
		{
			mutable_parameter_block_sizes()->push_back(parameterCounts[i]);
		}
		set_num_residuals(residualCount);
		evaluate = eval;
	}


	// Inherited via CostFunction
	virtual bool Evaluate(double const * const * parameters, double * residuals, double ** jacobians) const override
	{
		if (evaluate(parameters, residuals, jacobians))
			return true;
		else
			return false;
	}
};



DllExport(Problem*) cCreateProblem();
DllExport(void) cReleaseProblem(Problem* problem);

DllExport(CustomCostFunction*) cCreateCostFunction(int nParameterBlocks, int* parameterCounts, int residualCount, int(*eval)(double const * const * parameters, double * residuals, double ** jacobians));
DllExport(void) cReleaseCostFunction(CustomCostFunction* function);

DllExport(void) cAddResidualFunction1(Problem* problem, CustomCostFunction* cost, double* p0);
DllExport(void) cAddResidualFunction2(Problem* problem, CustomCostFunction* cost, double* p0, double* p1);
DllExport(void) cAddResidualFunction3(Problem* problem, CustomCostFunction* cost, double* p0, double* p1, double* p2);
DllExport(void) cAddResidualFunction4(Problem* problem, CustomCostFunction* cost, double* p0, double* p1, double* p2, double* p3);

DllExport(void) cSolve(Problem* problem, CeresOptions* options);


