functions {
    vector system(vector y,
              vector theta,
              data array[] real x_r,
              array[] int x_i) {
    vector[2] z;
    z[1] = y[1] - theta[1];
    z[2] = y[1] * y[2] - theta[2];
    return z;
  }
}

transformed data {
  vector[2] y_guess = [1, 1]';
  array[0] real x_r;
  array[0] int x_i;
}

transformed parameters {
  vector[2] theta = [3, 6]';
  vector[2] y;
  
  y = algebra_solver_newton(system, y_guess, theta, x_r, x_i);
}
