#include "stdafx.h"
#include "CeresNative.h"

using namespace std;
using ceres::AutoDiffCostFunction;
using ceres::CostFunction;
using ceres::SizedCostFunction;
using ceres::Problem;
using ceres::Solver;
using ceres::Solve;

static void disableGoogleLogging()
{
	static int off = 1;

	if (off)
	{
		off = 0;
		google::InitGoogleLogging("-logtostderr");
	}
}



DllExport(ceres::LossFunction*) cCreateLossFunction(CeresLossFunction f)
{
	switch (f.Kind)
	{
		case Trivial: return new ceres::TrivialLoss();
		case Huber: return new ceres::HuberLoss(f.P0);
		case SoftLOne: return new ceres::SoftLOneLoss(f.P0);
		case Cauchy: return new ceres::CauchyLoss(f.P0);
		case ArcTan: return new ceres::ArctanLoss(f.P0);
		case Tolerant: return new ceres::TolerantLoss(f.P0, f.P1);
		default: return new ceres::TrivialLoss();
	}

}DllExport(void) cReleaseLossFunction(ceres::LossFunction* f)
{
	if(f) delete f;
}

DllExport(Problem*) cCreateProblem()
{
	disableGoogleLogging();
	return new Problem();
}

DllExport(void) cReleaseProblem(Problem* problem)
{
	disableGoogleLogging();
	if (problem) delete problem;
}

DllExport(CustomCostFunction*) cCreateCostFunction(int nParameterBlocks, int* parameterCounts, int residualCount, int(*eval)(double const * const * parameters, double * residuals, double ** jacobians))
{
	disableGoogleLogging();
	return new CustomCostFunction(nParameterBlocks, parameterCounts, residualCount, eval);
}

DllExport(void) cReleaseCostFunction(CustomCostFunction* function)
{
	disableGoogleLogging();
	if (function) delete function;
}

DllExport(void) cAddResidualFunction1(Problem* problem, ceres::LossFunction* loss, CustomCostFunction* cost, double* p0)
{
	disableGoogleLogging();
	problem->AddResidualBlock(cost, loss, p0);
}

DllExport(void) cAddResidualFunction2(Problem* problem, ceres::LossFunction* loss, CustomCostFunction* cost, double* p0, double* p1)
{
	disableGoogleLogging();
	problem->AddResidualBlock(cost, loss, p0, p1);
}

DllExport(void) cAddResidualFunction3(Problem* problem, ceres::LossFunction* loss, CustomCostFunction* cost, double* p0, double* p1, double* p2)
{
	disableGoogleLogging();
	problem->AddResidualBlock(cost, loss, p0, p1, p2);
}

DllExport(void) cAddResidualFunction4(Problem* problem, ceres::LossFunction* loss, CustomCostFunction* cost, double* p0, double* p1, double* p2, double* p3)
{
	disableGoogleLogging();
	problem->AddResidualBlock(cost, loss, p0, p1, p2, p3);
}

DllExport(double) cSolve(Problem* problem, CeresOptions* options)
{
	disableGoogleLogging();
	ceres::Solver::Options opt;

	opt.max_num_iterations = options->MaxIterations;
	opt.linear_solver_type = (ceres::LinearSolverType)options->SolverType;

	opt.minimizer_progress_to_stdout = options->PrintProgress != 0;
	opt.gradient_tolerance = options->GradientTolerance;
	opt.function_tolerance = options->FunctionTolerance;
	opt.parameter_tolerance = options->ParameterTolerance;

	ceres::Solver::Summary summary;
	ceres::Solve(opt, problem, &summary);

	if(options->PrintProgress != 0) std::cout << summary.FullReport() << "\n";

	if (summary.termination_type == ceres::TerminationType::CONVERGENCE)
	{
		return summary.final_cost;
	}
	else
	{
		return INFINITY;
	}

}




