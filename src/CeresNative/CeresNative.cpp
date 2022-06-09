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
	Euclidean3d pose0;
	V2d observation;
	V2i imageSize;
	double weight;

	CostFunctor(V2d obs, V2i size, Euclidean3d pose, double w) {
		observation = obs;
		imageSize = size;
		weight = w;
		pose0 = pose;
	}

   	template <typename T>
   	bool operator()(const T* const projection, const T* const distortion, const T* const camera, const T* const world, T* residual) const {

		T temp[3];
		T p0[6] = { (T)pose0.Rx, (T)pose0.Ry, (T)pose0.Rz, (T)pose0.Tx, (T)pose0.Ty, (T)pose0.Tz };
		ceres::AngleAxisRotatePoint(p0, world, temp);
		temp[0] += p0[3];
		temp[1] += p0[4];
		temp[2] += p0[5];


		T viewSpace[3];
		ceres::AngleAxisRotatePoint(camera, temp, viewSpace);
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
		T px1 = px * factor + 2.0*p1*px*py + p2*(r2 + 2.0*px*px);
		T py1 = py * factor + p1*(r2 + 2.0*py*py) + 2.0*p2*px*py;

		px = focal * px1 + ppx;
		py = focal * aspect * py1 + ppy;

		px = (0.5 + 0.5 * px) * (double)imageSize.X;
		py = (0.5 - 0.5 * py) * (double)imageSize.Y;

		residual[0] = (px - observation.X) * weight;
		residual[1] = (py - observation.Y) * weight;
     	return true;
   	}
};

Euclidean3d composeEuclidean(Euclidean3d a, Euclidean3d b) {
	double ra[3] = { a.Rx, a.Ry, a.Rz };
	double rb[3] = { b.Rx, b.Ry, b.Rz };
	double ta[3] = { a.Tx, a.Ty, a.Tz };
	double tb[3] = { b.Tx, b.Ty, b.Tz };
	double tc[3];
	double qa[4];
	double qb[4];
	double qc[4];

	// qb = qa * qb
	ceres::AngleAxisToQuaternion(ra, qa);
	ceres::AngleAxisToQuaternion(rb, qb);
	ceres::QuaternionProduct(qa, qb, qc);

	// tc = ta + qa*tb
	ceres::AngleAxisRotatePoint(ra, tb, tc);
	tc[0] += ta[0];
	tc[1] += ta[1];
	tc[2] += ta[2];
	ceres::QuaternionToAngleAxis(qc, rb);

	return { rb[0], rb[1], rb[2], tc[0], tc[1], tc[2] };
}

M33d composeMatrix(M33d a, M33d b) {
	M33d res;

	res.M[0] = a.M[0] * b.M[0] + a.M[1] * b.M[3] + a.M[2] * b.M[6];
	res.M[1] = a.M[0] * b.M[1] + a.M[1] * b.M[4] + a.M[2] * b.M[7];
	res.M[2] = a.M[0] * b.M[2] + a.M[1] * b.M[5] + a.M[2] * b.M[8];
	res.M[3] = a.M[3] * b.M[0] + a.M[4] * b.M[3] + a.M[5] * b.M[6];
	res.M[4] = a.M[3] * b.M[1] + a.M[4] * b.M[4] + a.M[5] * b.M[7];
	res.M[5] = a.M[3] * b.M[2] + a.M[4] * b.M[5] + a.M[5] * b.M[8];
	res.M[6] = a.M[6] * b.M[0] + a.M[7] * b.M[3] + a.M[8] * b.M[6];
	res.M[7] = a.M[6] * b.M[1] + a.M[7] * b.M[4] + a.M[8] * b.M[7];
	res.M[8] = a.M[6] * b.M[2] + a.M[7] * b.M[5] + a.M[8] * b.M[8];

	return res;
}
M33d transpose(M33d a) {
	M33d res;

	res.M[0] = a.M[0];
	res.M[1] = a.M[3];
	res.M[2] = a.M[6];
	res.M[3] = a.M[1];
	res.M[4] = a.M[4];
	res.M[5] = a.M[7];
	res.M[6] = a.M[2];
	res.M[7] = a.M[5];
	res.M[8] = a.M[8];

	return res;
}

DllExport(double) cOptimizePhotonetwork(
		CeresOptions* options, bool nonmonotonic, bool useDifferentialPoses,
		int nInterations, IterationConfig* config,
		int nProjections, Projection* projs, Distortion* distortions,
		int nCams, Euclidean3d* cams, 
		int nPoints, V3d* world,
		int nResiduals, Residual* residuals,
		M33d* pointCovariances, M33d* cameraLocationCovariances) {	

	disableGoogleLogging();
	Problem problem;

	Euclidean3d* extra = new Euclidean3d[nCams];
	Euclidean3d* poses;
	Euclidean3d* differentialPoses;

	for(int i = 0; i < nCams; i++) {
		extra[i].Rx = 0.0;
		extra[i].Ry = 0.0;
		extra[i].Rz = 0.0;
		extra[i].Tx = 0.0;
		extra[i].Ty = 0.0;
		extra[i].Tz = 0.0;
	}

	if(useDifferentialPoses) {
		poses = cams;
		differentialPoses = extra;
	}
	else {
		poses = extra;
		differentialPoses = cams;
	}

	for (int i = 0; i < nProjections; i++) {
		
		double* proj = (double*)&projs[i];
		problem.AddParameterBlock(proj, PROJECTION_DOUBLES);
		problem.AddParameterBlock((double*)&distortions[i], DISTORTION_DOUBLES);

		// double FocalLength;
		// double Aspect;
		// double PrincipalPointX;
		// double PrincipalPointY;
		// if(minProjections) {
		// 	Projection* l = &minProjections[i];
		// 	problem.SetParameterLowerBound(proj, 0, l->FocalLength);
		// 	problem.SetParameterLowerBound(proj, 1, l->Aspect);
		// 	problem.SetParameterLowerBound(proj, 2, l->PrincipalPointX);
		// 	problem.SetParameterLowerBound(proj, 3, l->PrincipalPointY);
		// }
		// if(maxProjections) {
		// 	Projection* l = &maxProjections[i];
		// 	problem.SetParameterUpperBound(proj, 0, l->FocalLength);
		// 	problem.SetParameterUpperBound(proj, 1, l->Aspect);
		// 	problem.SetParameterUpperBound(proj, 2, l->PrincipalPointX);
		// 	problem.SetParameterUpperBound(proj, 3, l->PrincipalPointY);
		// }

	}
	for (int i = 0; i < nCams; i++) problem.AddParameterBlock((double*)&differentialPoses[i], CAMERA_DOUBLES);
	for (int i = 0; i < nPoints; i++) problem.AddParameterBlock((double*)&world[i], POINT_DOUBLES);

	for(int ri = 0; ri < nResiduals; ri++) {
		auto res = residuals[ri];
		
		if(res.CameraIndex < 0 || res.CameraIndex >= nCams) printf("[%03d] Camera index out of bounds: %d\n", ri, res.CameraIndex);
		else if(res.ProjectionIndex < 0 || res.ProjectionIndex >= nProjections) printf("[%03d] Projection index out of bounds: %d\n", ri, res.ProjectionIndex);
		else if(res.PointIndex < 0 || res.PointIndex >= nPoints) printf("[%03d] Point index out of bounds: %d\n", ri, res.PointIndex);
		else {
			auto obs = res.Observation;
			auto pose = poses[res.CameraIndex];
			CostFunction* cost_function = new AutoDiffCostFunction<CostFunctor, 2, PROJECTION_DOUBLES, DISTORTION_DOUBLES, CAMERA_DOUBLES, POINT_DOUBLES>(new CostFunctor(obs, res.ImageSize, pose, res.Weight));
			problem.AddResidualBlock(cost_function, nullptr, (double*)&projs[res.ProjectionIndex], (double*)&distortions[res.ProjectionIndex], (double*)&differentialPoses[res.CameraIndex], (double*)&world[res.PointIndex]);
		}
	}

	ceres::Solver::Options opt;

	auto solver = (ceres::LinearSolverType)options->SolverType;	
	opt.max_num_iterations = options->MaxIterations;
	opt.linear_solver_type = solver;

	opt.minimizer_progress_to_stdout = options->PrintProgress != 0;
	opt.gradient_tolerance = options->GradientTolerance;
	opt.function_tolerance = options->FunctionTolerance;
	opt.parameter_tolerance = options->ParameterTolerance;
	opt.use_nonmonotonic_steps = nonmonotonic;
	if (solver == ceres::LinearSolverType::ITERATIVE_SCHUR) opt.preconditioner_type = ceres::SCHUR_JACOBI;
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
			for(int pi = 0; pi < nCams; pi++) { problem.SetParameterBlockConstant((double*)&differentialPoses[pi]); }
		}
		else {
			for(int pi = 0; pi < nCams; pi++) { problem.SetParameterBlockVariable((double*)&differentialPoses[pi]); }
		}


		if(config[i].PointsConstant) {
			for(int pi = 0; pi < nPoints; pi++) { problem.SetParameterBlockConstant((double*)&world[pi]); }
		}
		else {
			for(int pi = 0; pi < nPoints; pi++) { problem.SetParameterBlockVariable((double*)&world[pi]); }
		}

		for(int fi = 0; fi < config[i].FixedPointCount; fi++) {
			int pi = config[i].FixedPoints[fi];
			if(pi < 0 || pi >= nPoints) printf("[%03d] Constant point index out of bounds: %d\n", fi, pi);
			else problem.SetParameterBlockConstant((double*)&world[pi]);
		}

		ceres::Solve(opt, &problem, &summary);
		
		for(int fi = 0; fi < config[i].FixedPointCount; fi++) {
			int pi = config[i].FixedPoints[fi];
			if(pi >= 0 && pi < nPoints) problem.SetParameterBlockVariable((double*)&world[pi]);
		}

		if(i == nInterations - 1) {

			if(cameraLocationCovariances != nullptr) {
				
				ceres::Covariance::Options options;
				options.algorithm_type = ceres::CovarianceAlgorithmType::SPARSE_QR;
				ceres::Covariance covariance(options);
				std::vector<pair<const double*, const double*> > covariance_blocks;

				for(int pi = 0; pi < nPoints; pi++) { problem.SetParameterBlockConstant((double*)&world[pi]); }
				for(int pi = 0; pi < nCams; pi++) { problem.SetParameterBlockVariable((double*)&differentialPoses[pi]); }
				for(int ci = 0; ci < nCams; ci++) {
					covariance_blocks.push_back(make_pair((const double*)&differentialPoses[ci], (const double*)&differentialPoses[ci]));
				}

				if(covariance.Compute(covariance_blocks, &problem)) {
					double cov[36];
					for(int ci = 0; ci < nCams; ci++) {
						// rx ry rz tx ty tz
						covariance.GetCovarianceBlock((const double*)&differentialPoses[ci], (const double*)&differentialPoses[ci], cov);
						M33d tCov;
						tCov.M[0] = cov[21]; tCov.M[1] = cov[22]; tCov.M[2] = cov[23];
						tCov.M[3] = cov[27]; tCov.M[4] = cov[28]; tCov.M[5] = cov[29];
						tCov.M[6] = cov[33]; tCov.M[7] = cov[34]; tCov.M[8] = cov[35];


						// Rf = Rd * Rp
						// tf = Rd * tp + td + eps

						// COV(eps) = tCov

						// loc = Rf^-1 * -tf
						// loc = Rp^-1*Rd^-1*(Rd*tp + td + eps)
						// loc = Rp^-1*Rd^-1*Rd*tp + Rp^-1*Rd^-1*td + Rp^-1*Rd^-1*eps

						// COV(loc) 
						// = COV(Rp^-1*Rd^-1*Rd*tp + Rp^-1*Rd^-1*td + Rp^-1*Rd^-1*eps)
						// = COV(Rp^-1*Rd^-1*Rd*tp) + COV(Rp^-1*Rd^-1*td) + COV(Rp^-1*Rd^-1*eps)
						// = COV(Rf^-1*eps)
						// = COV(Rf^T*eps)
						// = Rf^T * COV(eps) * Rf

						M33d rot;
						auto cam = composeEuclidean(differentialPoses[ci], poses[ci]);
						double r[3] = { cam.Rx, cam.Ry, cam.Rz };
						ceres::AngleAxisToRotationMatrix(r, ceres::RowMajorAdapter3x3(rot.M));
						M33d fin = composeMatrix(composeMatrix(transpose(rot), tCov), rot);

						cameraLocationCovariances[ci] = fin;
					}
				}
			}


			if(pointCovariances != nullptr) {
				ceres::Covariance::Options options;
				options.algorithm_type = ceres::CovarianceAlgorithmType::SPARSE_QR;
				ceres::Covariance covariance(options);
				std::vector<pair<const double*, const double*> > covariance_blocks;

				for(int pi = 0; pi < nPoints; pi++) { problem.SetParameterBlockVariable((double*)&world[pi]); }
				for(int pi = 0; pi < nCams; pi++) { problem.SetParameterBlockConstant((double*)&differentialPoses[pi]); }
				for(int pi = 0; pi < nPoints; pi++) {
					covariance_blocks.push_back(make_pair((const double*)&world[pi], (const double*)&world[pi]));
				}

				if(covariance.Compute(covariance_blocks, &problem)) {
					for(int pi = 0; pi < nPoints; pi++) {
						covariance.GetCovarianceBlock((const double*)&world[pi], (const double*)&world[pi], (double*)&pointCovariances[pi]);
					}
				}
			}
			

		}

	}

	if(options->PrintProgress != 0) printf("%s\n", summary.FullReport().c_str());


	for(int i = 0; i < nCams; i++) {
		if(useDifferentialPoses) cams[i] = composeEuclidean(differentialPoses[i], poses[i]);
		else cams[i] = differentialPoses[i];
	}
	delete[] extra;

	if (summary.termination_type == ceres::TerminationType::CONVERGENCE)
	{
		return summary.final_cost;
	}
	else
	{
		return INFINITY;
	}
}



