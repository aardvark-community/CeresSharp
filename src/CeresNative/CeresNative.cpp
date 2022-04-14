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

	if(options->PrintProgress != 0) printf("%s\n", summary.FullReport().c_str());

	if (summary.termination_type == ceres::TerminationType::CONVERGENCE)
	{
		return summary.final_cost;
	}
	else
	{
		return INFINITY;
	}

}

class MyCostFunction : public AutoDiffCostFunction<MyCostFunction, 1, 1>
{
	private:

};

struct CostFunctor {

	V2d observation;

	CostFunctor(V2d obs) {
		observation = obs;
	}

   	template <typename T>
   	bool operator()(const T* const projection, const T* const camera, const T* const world, T* residual) const {
		
		T viewSpace[3];
		ceres::AngleAxisRotatePoint(camera, world, viewSpace);
		viewSpace[0] += camera[3];
		viewSpace[1] += camera[4];
		viewSpace[2] += camera[5];


        // let p = (proj.focalLength * p.XY * V2d(1.0, proj.aspect)) / -p.Z 
        // Distortion.distort p proj.distortion

		// let d = ndc + dist.principalPoint
        // let px = 0.5 * d * V2d dist.imageSize
        // let pxd = dist.distortion.TransformPos px
        // 2.0 * pxd / V2d dist.imageSize
		T focal = projection[0];
		T aspect = projection[1];
		T ppx = projection[2];
		T ppy = projection[3];
		T projSpace[2];
		projSpace[0] = focal * viewSpace[0] / -viewSpace[2] + ppx;
		projSpace[1] = focal * viewSpace[1] / -viewSpace[2] + ppy;



		residual[0] = projSpace[0] - observation.X;
		residual[1] = projSpace[1] - observation.Y;
     	return true;
   	}
};

DllExport(double) cOptimizePhotonetwork(
		CeresOptions* options,
		int nInterations, IterationConfig* config,
		int nProjections, Projection* projs, 
		int nCams, Euclidean3d* cams, 
		int nPoints, V3d* world, V2d* observations,
		int nResiduals, Residual* residuals) {	

	disableGoogleLogging();
	Problem problem;

	problem.AddParameterBlock((double*)projs, nProjections * PROJECTION_DOUBLES);
	problem.AddParameterBlock((double*)cams, nCams * CAMERA_DOUBLES);
	problem.AddParameterBlock((double*)world, nPoints * POINT_DOUBLES);

	for(int ri = 0; ri < nResiduals; ri++) {
		auto res = residuals[ri];
		auto obs = observations[res.PointIndex];
  		CostFunction* cost_function = new AutoDiffCostFunction<CostFunctor, 2, PROJECTION_DOUBLES, CAMERA_DOUBLES, POINT_DOUBLES>(new CostFunctor(obs));
  		problem.AddResidualBlock(cost_function, nullptr, (double*)&projs[res.ProjectionIndex], (double*)&cams[res.CameraIndex], (double*)&world[res.PointIndex]);
	}

	ceres::Solver::Options opt;

	opt.max_num_iterations = options->MaxIterations;
	opt.linear_solver_type = (ceres::LinearSolverType)options->SolverType;

	opt.minimizer_progress_to_stdout = options->PrintProgress != 0;
	opt.gradient_tolerance = options->GradientTolerance;
	opt.function_tolerance = options->FunctionTolerance;
	opt.parameter_tolerance = options->ParameterTolerance;

	ceres::Solver::Summary summary;

	for(int i = 0; i < nInterations; i++) {
		if(config[i].ProjectionsConstant) problem.SetParameterBlockConstant((double*)projs);
		else problem.SetParameterBlockVariable((double*)projs);

		if(config[i].CamerasConstant) problem.SetParameterBlockConstant((double*)cams);
		else problem.SetParameterBlockVariable((double*)cams);

		if(config[i].PointsConstant) problem.SetParameterBlockConstant((double*)world);
		else problem.SetParameterBlockVariable((double*)world);

		ceres::Solve(opt, &problem, &summary);
	}

	if(options->PrintProgress != 0) printf("%s\n", summary.FullReport().c_str());

	if (summary.termination_type == ceres::TerminationType::CONVERGENCE)
	{
		return summary.final_cost;
	}
	else
	{
		return INFINITY;
	}
}



