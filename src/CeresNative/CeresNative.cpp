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
	opt.num_threads = (int)std::thread::hardware_concurrency();

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

struct CostFunctor {

	V2d observation;
	V2i imageSize;
	CostFunctor(V2d obs, V2i size) {
		observation = obs;
		imageSize = size;
	}

   	template <typename T>
   	bool operator()(const T* const projection, const T* const distortion, const T* const camera, const T* const world, T* residual) const {
		
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
		T k1 = distortion[0];
		T k2 = distortion[1];
		T k3 = distortion[2];
		T p1 = distortion[3];
		T p2 = distortion[4];

		T px = viewSpace[0] / -viewSpace[2];
		T py = viewSpace[1] / -viewSpace[2];

		T r2 = px*px + py*py;
		T r4 = r2*r2;
		T r6 = r4*r2;
		T factor = 1.0 + k1*r2 + k2*r4 + k3*r6;
		T px1 = px * factor + 2.0*p1*px*py + p2;
		T py1 = py * factor + p1*(r2 + 2.0*py*py) + 2.0*p2*px*py;

		px = focal * px1 + ppx;
		py = focal * aspect * py1 + ppy;

		px = (0.5 + 0.5 * px) * (double)imageSize.X;
		py = (0.5 - 0.5 * py) * (double)imageSize.Y;

		residual[0] = px - observation.X;
		residual[1] = py - observation.Y;
     	return true;
   	}
};

DllExport(double) cOptimizePhotonetwork(
		CeresOptions* options,
		int nInterations, IterationConfig* config,
		int nProjections, Projection* projs, Distortion* distortions,
		int nCams, Euclidean3d* cams, 
		int nPoints, V3d* world,
		int nResiduals, Residual* residuals) {	

	disableGoogleLogging();
	Problem problem;

	for (int i = 0; i < nProjections; i++) {
		problem.AddParameterBlock((double*)&projs[i], PROJECTION_DOUBLES);
		problem.AddParameterBlock((double*)&distortions[i], DISTORTION_DOUBLES);
	}
	for (int i = 0; i < nCams; i++) problem.AddParameterBlock((double*)&cams[i], CAMERA_DOUBLES);
	for (int i = 0; i < nPoints; i++) problem.AddParameterBlock((double*)&world[i], POINT_DOUBLES);

	for(int ri = 0; ri < nResiduals; ri++) {
		auto res = residuals[ri];
		auto obs = res.Observation;
  		CostFunction* cost_function = new AutoDiffCostFunction<CostFunctor, 2, PROJECTION_DOUBLES, DISTORTION_DOUBLES, CAMERA_DOUBLES, POINT_DOUBLES>(new CostFunctor(obs, res.ImageSize));
  		problem.AddResidualBlock(cost_function, nullptr, (double*)&projs[res.ProjectionIndex], (double*)&distortions[res.ProjectionIndex], (double*)&cams[res.CameraIndex], (double*)&world[res.PointIndex]);
	}

	ceres::Solver::Options opt;

	opt.max_num_iterations = options->MaxIterations;
	opt.linear_solver_type = (ceres::LinearSolverType)options->SolverType;

	opt.minimizer_progress_to_stdout = options->PrintProgress != 0;
	opt.gradient_tolerance = options->GradientTolerance;
	opt.function_tolerance = options->FunctionTolerance;
	opt.parameter_tolerance = options->ParameterTolerance;
	opt.num_threads = (int)std::thread::hardware_concurrency();
	ceres::Solver::Summary summary;

	for(int i = 0; i < nInterations; i++) {
		if(config[i].ProjectionsConstant) {
			for(int pi = 0; pi < nProjections; pi++) { problem.SetParameterBlockConstant((double*)&projs[pi]); }
		}
		else {
			for(int pi = 0; pi < nProjections; pi++) { problem.SetParameterBlockVariable((double*)&projs[pi]); }
		}

		if(config[i].DistortionsConstant) {
			for(int pi = 0; pi < nProjections; pi++) { problem.SetParameterBlockConstant((double*)&distortions[pi]); }
		}
		else {
			for(int pi = 0; pi < nProjections; pi++) { problem.SetParameterBlockVariable((double*)&distortions[pi]); }
		}

		if(config[i].CamerasConstant) {
			for(int pi = 0; pi < nCams; pi++) { problem.SetParameterBlockConstant((double*)&cams[pi]); }
		}
		else {
			for(int pi = 0; pi < nCams; pi++) { problem.SetParameterBlockVariable((double*)&cams[pi]); }
		}


		if(config[i].PointsConstant) {
			for(int pi = 0; pi < nPoints; pi++) { problem.SetParameterBlockConstant((double*)&world[pi]); }
		}
		else {
			for(int pi = 0; pi < nPoints; pi++) { problem.SetParameterBlockVariable((double*)&world[pi]); }
		}


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



