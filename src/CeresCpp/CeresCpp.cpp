// CeresCpp.cpp : Defines the exported functions for the DLL application.
//

#include "stdafx.h"

#define GLOG_NO_ABBREVIATED_SEVERITIES

#include <ceres/ceres.h>
#include <ceres/rotation.h>

using ceres::AutoDiffCostFunction;
using ceres::CostFunction;
using ceres::Problem;
using ceres::Solver;
using ceres::Solve;


struct SnavelyReprojectionError {
	SnavelyReprojectionError(double observed_x, double observed_y, double observed_xi, double observed_yi)
		: observed_x(observed_x), observed_y(observed_y), observed_xi(observed_xi), observed_yi(observed_yi) {}

	template <typename T>
	bool operator()(const T* const camera,
		T* residuals) const {
		// camera[0,1,2] are the angle-axis rotation.
		T p[3];
		T pi[3];
		pi[0] = T(observed_xi);
		pi[1] = T(observed_yi);
		pi[2] = T(0.0);

		ceres::AngleAxisRotatePoint(camera, pi, p);
		// camera[3,4,5] are the translation.
		p[0] += camera[3]; p[1] += camera[4]; p[2] += camera[5];

		// Compute the center of distortion. The sign change comes from
		// the camera model that Noah Snavely's Bundler assumes, whereby
		// the camera coordinate system has a negative z axis.
		T xp = -p[0] / p[2];
		T yp = -p[1] / p[2];

		// Apply second and fourth order radial distortion.
		const T& l1 = camera[7];
		const T& l2 = camera[8];
		T r2 = xp*xp + yp*yp;
		T distortion = T(1.0) + r2  * (l1 + l2  * r2);

		// Compute final projected point position.
		const T& focal = camera[6];
		T predicted_x = focal * distortion * xp;
		T predicted_y = focal * distortion * yp;

		// The error is the difference between the predicted and observed position.
		residuals[0] = predicted_x - T(observed_x);
		residuals[1] = predicted_y - T(observed_y);
		return true;
	}

	// Factory to hide the construction of the CostFunction object from
	// the client code.
	static ceres::CostFunction* Create(const double observed_x,
		const double observed_y, const double observed_xi, const double observed_yi) {
		return (new ceres::AutoDiffCostFunction<SnavelyReprojectionError, 2, 9>(
			new SnavelyReprojectionError(observed_x, observed_y, observed_xi, observed_yi)));
	}

	double observed_x;
	double observed_y;
	double observed_xi;
	double observed_yi;
};

DllExport(int) calibrate_camera(int observation_count, double* observations, double* camera)
{

	// Create residuals for each observation in the bundle adjustment problem. The
	// parameters for cameras and points are added automatically.
	ceres::Problem problem;
	for (int i = 0; i < observation_count; ++i) {
		// Each Residual block takes a point and a camera as input and outputs a 2
		// dimensional residual. Internally, the cost function stores the observed
		// image location and compares the reprojection against the observation.
		ceres::CostFunction* cost_function =
			SnavelyReprojectionError::Create(observations[4 * i + 0],
				observations[4 * i + 1], observations[4 *i + 2], observations[4 * i + 3]);
		problem.AddResidualBlock(cost_function,
			NULL /* squared loss */,
			camera);
	}
	// Make Ceres automatically detect the bundle structure. Note that the
	// standard solver, SPARSE_NORMAL_CHOLESKY, also works fine but it is slower
	// for standard bundle adjustment problems.
	ceres::Solver::Options options;
	options.linear_solver_type = ceres::DENSE_SCHUR;
	options.minimizer_progress_to_stdout = true;
	ceres::Solver::Summary summary;
	ceres::Solve(options, &problem, &summary);
	std::cout << summary.FullReport() << "\n";

	return 0;
}

// A templated cost functor that implements the residual r = 10 -
// x. The method operator() is templated so that we can then use an
// automatic differentiation wrapper around it to generate its
// derivatives.
struct CostFunctor {
	template <typename T> bool operator()(const T* const x, T* residual) const {
		residual[0] = T(10.0) - x[0];
		return true;
	}
};

DllExport(int) main() {
	google::InitGoogleLogging("urdar");

	// The variable to solve for with its initial value. It will be
	// mutated in place by the solver.
	double x = 0.5;
	const double initial_x = x;

	// Build the problem.
	Problem problem;

	// Set up the only cost function (also known as residual). This uses
	// auto-differentiation to obtain the derivative (jacobian).
	CostFunction* cost_function =
		new AutoDiffCostFunction<CostFunctor, 1, 1>(new CostFunctor);
	problem.AddResidualBlock(cost_function, NULL, &x);

	// Run the solver!
	Solver::Options options;
	options.minimizer_progress_to_stdout = true;
	Solver::Summary summary;
	Solve(options, &problem, &summary);

	std::cout << summary.BriefReport() << "\n";
	std::cout << "x : " << initial_x
		<< " -> " << x << "\n";
	return 0;
}
