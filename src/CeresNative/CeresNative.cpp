#include "stdafx.h"
#include "CeresNative.h"

using namespace std;
using ceres::AutoDiffCostFunction;
using ceres::CostFunction;
using ceres::SizedCostFunction;
using ceres::Problem;
using ceres::Solver;
using ceres::Solve;

static void disableLogging()
{
	static int off = 1;

	if (off)
	{
		off = 0;
		google::InitGoogleLogging("-logtostderr");
	}
}


DllExport(Problem*) cCreateProblem()
{
	disableLogging();
	return new Problem();
}

DllExport(void) cReleaseProblem(Problem* problem)
{
	disableLogging();
	if (problem) delete problem;
}

DllExport(CustomCostFunction*) cCreateCostFunction(int nParameterBlocks, int* parameterCounts, int residualCount, int(*eval)(double const * const * parameters, double * residuals, double ** jacobians))
{
	disableLogging();
	return new CustomCostFunction(nParameterBlocks, parameterCounts, residualCount, eval);
}

DllExport(void) cReleaseCostFunction(CustomCostFunction* function)
{
	disableLogging();
	if (function) delete function;
}

DllExport(void) cAddResidualFunction1(Problem* problem, CustomCostFunction* cost, double* p0)
{
	disableLogging();
	auto loss_function = new ceres::HuberLoss(1.0);
	problem->AddResidualBlock(cost, loss_function, p0);
}

DllExport(void) cAddResidualFunction2(Problem* problem, CustomCostFunction* cost, double* p0, double* p1)
{
	disableLogging();
	auto loss_function = new ceres::HuberLoss(1.0);
	problem->AddResidualBlock(cost, loss_function, p0, p1);
}

DllExport(void) cAddResidualFunction3(Problem* problem, CustomCostFunction* cost, double* p0, double* p1, double* p2)
{
	disableLogging();
	auto loss_function = new ceres::HuberLoss(1.0);
	problem->AddResidualBlock(cost, loss_function, p0, p1, p2);
}

DllExport(void) cAddResidualFunction4(Problem* problem, CustomCostFunction* cost, double* p0, double* p1, double* p2, double* p3)
{
	disableLogging();
	auto loss_function = new ceres::HuberLoss(1.0);
	problem->AddResidualBlock(cost, loss_function, p0, p1, p2, p3);
}

DllExport(void) cSolve(Problem* problem, CeresOptions* options)
{
	disableLogging();
	ceres::Solver::Options opt;

	opt.max_num_iterations = options->MaxIterations;
	opt.linear_solver_type = (ceres::LinearSolverType)options->SolverType;
	opt.minimizer_progress_to_stdout = options->PrintProgress != 0;
	opt.gradient_tolerance = options->GradientTolerance;
	opt.function_tolerance = options->FunctionTolerance;
	opt.parameter_tolerance = options->ParameterTolerance;


	ceres::Solver::Summary summary;
	ceres::Solve(opt, problem, &summary);
	std::cout << summary.FullReport() << "\n";

}




