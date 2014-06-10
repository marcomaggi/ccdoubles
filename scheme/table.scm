;; table.scm

(foreign-library
 (name "ccdoubles")
 (version
  (major	0)
  (minor	1)
  (patch	0))
 (functions

  ;;These are not interesting in a binding.
  ;;
  ;; (double-complex ccdoubles_cplx_mul (double-complex O1 double-complex O2))
  ;; (double-complex ccdoubles_cplx_div (double-complex O1 double-complex O2))
  ;; (double-complex ccdoubles_cplx_neg (double-complex O))

  (void ccdoubles_real_vector_clear (unsigned-int nslots double* vector))
  (void ccdoubles_real_vector_set   (unsigned-int nslots double* vector double value))
  (void ccdoubles_real_vector_copy (unsigned-int nslots double* dst double* src))

  (void ccdoubles_real_vector_add (unsigned-int nslots double* result double* operand1 double* operand2))
  (void ccdoubles_real_vector_sub (unsigned-int nslots double* result double* operand1 double* operand2))
  (void ccdoubles_real_vector_mul (unsigned-int nslots double* result double* operand1 double* operand2))
  (void ccdoubles_real_vector_div (unsigned-int nslots double* result double* operand1 double* operand2))
  (void ccdoubles_real_vector_neg (unsigned-int nslots double* result double* operand))
  (void ccdoubles_real_vector_abs (unsigned-int nslots double* result double* operand))

  (void ccdoubles_real_vector_fmod (unsigned-int nslots double* result double* operand1 double* operand2))
  (void ccdoubles_real_vector_drem (unsigned-int nslots double* result double* operand1 double* operand2))
  (void ccdoubles_real_vector_remainder (unsigned-int nslots double* result double* operand1 double* operand2))

  (void ccdoubles_real_vector_ceil (unsigned-int nslots double* result double* operand))
  (void ccdoubles_real_vector_floor (unsigned-int nslots double* result double* operand))
  (void ccdoubles_real_vector_trunc (unsigned-int nslots double* result double* operand))
  (void ccdoubles_real_vector_round (unsigned-int nslots double* result double* operand))
  (void ccdoubles_real_vector_rint (unsigned-int nslots double* result double* operand))

  (void ccdoubles_real_vector_isgreater (unsigned-int nslots signed-int* result double* operand1 double* operand2))
  (void ccdoubles_real_vector_isgreaterequal (unsigned-int nslots signed-int* result double* operand1 double* operand2))
  (void ccdoubles_real_vector_isless (unsigned-int nslots signed-int* result double* operand1 double* operand2))
  (void ccdoubles_real_vector_islessequal (unsigned-int nslots signed-int* result double* operand1 double* operand2))
  (void ccdoubles_real_vector_islessgreater (unsigned-int nslots signed-int* result double* operand1 double* operand2))
  (void ccdoubles_real_vector_isunordered (unsigned-int nslots signed-int* result double* operand1 double* operand2))

  (void ccdoubles_real_vector_min (unsigned-int nslots double* result double* operand1 double* operand2))
  (void ccdoubles_real_vector_max (unsigned-int nslots double* result double* operand1 double* operand2))

  (void ccdoubles_real_vector_fpclassify (unsigned-int nslots signed-int* result double* operand))
  (void ccdoubles_real_vector_isfinite (unsigned-int nslots signed-int* result double* operand))
  (void ccdoubles_real_vector_isinfinite (unsigned-int nslots signed-int* result double* operand))
  (void ccdoubles_real_vector_isnormal (unsigned-int nslots signed-int* result double* operand))
  (void ccdoubles_real_vector_isnan (unsigned-int nslots signed-int* result double* operand))

  (double ccdoubles_real_vector_scalar_product (unsigned-int nslots double* operand1 double* operand2))
  (void ccdoubles_real_vector_scalar_mul (unsigned-int nslots double* result double lambda double* operand))
  (void ccdoubles_real_vector_linear_combination (unsigned-int nslots double* result double alpha double* operand1 double beta double* operand2))
  (void ccdoubles_real_vector_linspace (unsigned-int nslots double* result double start double past))
  (void ccdoubles_real_vector_logspace (unsigned-int nslots double* result double start double past))

  (void ccdoubles_real_vector_exp (unsigned-int nslots double* result double* operand))
  (void ccdoubles_real_vector_exp10 (unsigned-int nslots double* result double* operand))
  (void ccdoubles_real_vector_exp2 (unsigned-int nslots double* result double* operand))
  (void ccdoubles_real_vector_log (unsigned-int nslots double* result double* operand))
  (void ccdoubles_real_vector_log10 (unsigned-int nslots double* result double* operand))
  (void ccdoubles_real_vector_log2 (unsigned-int nslots double* result double* operand))
  (void ccdoubles_real_vector_logb (unsigned-int nslots double* result double* operand))
  (void ccdoubles_real_vector_pow (unsigned-int nslots double* result double* operand1 double* operand2))
  (void ccdoubles_real_vector_sqrt (unsigned-int nslots double* result double* operand))
  (void ccdoubles_real_vector_cbrt (unsigned-int nslots double* result double* operand))
  (void ccdoubles_real_vector_hypot (unsigned-int nslots double* result double* operand1 double* operand2))
  (void ccdoubles_real_vector_expm1 (unsigned-int nslots double* result double* operand))
  (void ccdoubles_real_vector_log1p (unsigned-int nslots double* result double* operand))

  (void ccdoubles_real_vector_sin (unsigned-int nslots double* result double* operand))
  (void ccdoubles_real_vector_cos (unsigned-int nslots double* result double* operand))
  (void ccdoubles_real_vector_tan (unsigned-int nslots double* result double* operand))
  (void ccdoubles_real_vector_asin (unsigned-int nslots double* result double* operand))
  (void ccdoubles_real_vector_acos (unsigned-int nslots double* result double* operand))
  (void ccdoubles_real_vector_atan (unsigned-int nslots double* result double* operand))
  (void ccdoubles_real_vector_atan2 (unsigned-int nslots double* result double* operand1 double* operand2))

  (void ccdoubles_real_vector_sinh (unsigned-int nslots double* result double* operand))
  (void ccdoubles_real_vector_cosh (unsigned-int nslots double* result double* operand))
  (void ccdoubles_real_vector_tanh (unsigned-int nslots double* result double* operand))
  (void ccdoubles_real_vector_asinh (unsigned-int nslots double* result double* operand))
  (void ccdoubles_real_vector_acosh (unsigned-int nslots double* result double* operand))
  (void ccdoubles_real_vector_atanh (unsigned-int nslots double* result double* operand))

  (void ccdoubles_real_matrix_clear (unsigned-int nrows unsigned-int ncols double* matrix))
  (void ccdoubles_real_matrix_set   (unsigned-int nrows unsigned-int ncols double* matrix double value))
  (void ccdoubles_real_matrix_copy (unsigned-int nrows unsigned-int ncols double* dst double* src))

  (void ccdoubles_real_matrix_add (unsigned-int nrows unsigned-int ncols double* result double* operand1 double* operand2))
  (void ccdoubles_real_matrix_sub (unsigned-int nrows unsigned-int ncols double* result double* operand1 double* operand2))
  (void ccdoubles_real_matrix_mul (unsigned-int nrows unsigned-int ncols double* result double* operand1 double* operand2))
  (void ccdoubles_real_matrix_div (unsigned-int nrows unsigned-int ncols double* result double* operand1 double* operand2))
  (void ccdoubles_real_matrix_neg (unsigned-int nrows unsigned-int ncols double* result double* operand))
  (void ccdoubles_real_matrix_abs (unsigned-int nrows unsigned-int ncols double* result double* operand))

  (void ccdoubles_real_matrix_fmod (unsigned-int nrows unsigned-int ncols double* result double* operand1 double* operand2))
  (void ccdoubles_real_matrix_drem (unsigned-int nrows unsigned-int ncols double* result double* operand1 double* operand2))
  (void ccdoubles_real_matrix_remainder (unsigned-int nrows unsigned-int ncols double* result double* operand1 double* operand2))

  (void ccdoubles_real_matrix_ceil (unsigned-int nrows unsigned-int ncols double* result double* operand))
  (void ccdoubles_real_matrix_floor (unsigned-int nrows unsigned-int ncols double* result double* operand))
  (void ccdoubles_real_matrix_trunc (unsigned-int nrows unsigned-int ncols double* result double* operand))
  (void ccdoubles_real_matrix_round (unsigned-int nrows unsigned-int ncols double* result double* operand))
  (void ccdoubles_real_matrix_rint (unsigned-int nrows unsigned-int ncols double* result double* operand))

  (void ccdoubles_real_matrix_isgreater (unsigned-int nrows unsigned-int ncols signed-int* result double* operand1 double* operand2))
  (void ccdoubles_real_matrix_isgreaterequal (unsigned-int nrows unsigned-int ncols signed-int* result double* operand1 double* operand2))
  (void ccdoubles_real_matrix_isless (unsigned-int nrows unsigned-int ncols signed-int* result double* operand1 double* operand2))
  (void ccdoubles_real_matrix_islessequal (unsigned-int nrows unsigned-int ncols signed-int* result double* operand1 double* operand2))
  (void ccdoubles_real_matrix_islessgreater (unsigned-int nrows unsigned-int ncols signed-int* result double* operand1 double* operand2))
  (void ccdoubles_real_matrix_isunordered (unsigned-int nrows unsigned-int ncols signed-int* result double* operand1 double* operand2))

  (void ccdoubles_real_matrix_min (unsigned-int nrows unsigned-int ncols double* result double* operand1 double* operand2))
  (void ccdoubles_real_matrix_max (unsigned-int nrows unsigned-int ncols double* result double* operand1 double* operand2))

  (void ccdoubles_real_matrix_fpclassify (unsigned-int nrows unsigned-int ncols signed-int* result double* operand))
  (void ccdoubles_real_matrix_isfinite (unsigned-int nrows unsigned-int ncols signed-int* result double* operand))
  (void ccdoubles_real_matrix_isinfinite (unsigned-int nrows unsigned-int ncols signed-int* result double* operand))
  (void ccdoubles_real_matrix_isnormal (unsigned-int nrows unsigned-int ncols signed-int* result double* operand))
  (void ccdoubles_real_matrix_isnan (unsigned-int nrows unsigned-int ncols signed-int* result double* operand))

  (void ccdoubles_real_matrix_scalar_mul (unsigned-int nrows unsigned-int ncols double* result double lambda double* operand))
  (void ccdoubles_real_matrix_linear_combination (unsigned-int nrows unsigned-int ncols double* result double alpha double* operand1 double beta double* operand2))

  (void ccdoubles_real_matrix_transpose (unsigned-int nrows unsigned-int ncols double* result double* operand))

  (void ccdoubles_real_matrix_rowcol_mul (unsigned-int result_nrows unsigned-int operand_n unsigned-int result_ncols double* result double* operand1 double* operand2))

  (void ccdoubles_real_matrix_linspace (unsigned-int nrows unsigned-int ncols double* result double start double row_past double col_past))

;;; this is hidden
  ;;(void ccdoubles_real_matrix_logspace (unsigned-int nrows unsigned-int ncols double* result double start double row_past double col_past))

  (void ccdoubles_real_matrix_exp (unsigned-int nrows unsigned-int ncols double* result double* operand))
  (void ccdoubles_real_matrix_exp10 (unsigned-int nrows unsigned-int ncols double* result double* operand))
  (void ccdoubles_real_matrix_exp2 (unsigned-int nrows unsigned-int ncols double* result double* operand))
  (void ccdoubles_real_matrix_log (unsigned-int nrows unsigned-int ncols double* result double* operand))
  (void ccdoubles_real_matrix_log10 (unsigned-int nrows unsigned-int ncols double* result double* operand))
  (void ccdoubles_real_matrix_log2 (unsigned-int nrows unsigned-int ncols double* result double* operand))
  (void ccdoubles_real_matrix_logb (unsigned-int nrows unsigned-int ncols double* result double* operand))
  (void ccdoubles_real_matrix_pow (unsigned-int nrows unsigned-int ncols double* result double* operand1 double* operand2))
  (void ccdoubles_real_matrix_sqrt (unsigned-int nrows unsigned-int ncols double* result double* operand))
  (void ccdoubles_real_matrix_cbrt (unsigned-int nrows unsigned-int ncols double* result double* operand))
  (void ccdoubles_real_matrix_hypot (unsigned-int nrows unsigned-int ncols double* result double* operand1 double* operand2))
  (void ccdoubles_real_matrix_expm1 (unsigned-int nrows unsigned-int ncols double* result double* operand))
  (void ccdoubles_real_matrix_log1p (unsigned-int nrows unsigned-int ncols double* result double* operand))

  (void ccdoubles_real_matrix_sin (unsigned-int nrows unsigned-int ncols double* result double* operand))
  (void ccdoubles_real_matrix_cos (unsigned-int nrows unsigned-int ncols double* result double* operand))
  (void ccdoubles_real_matrix_tan (unsigned-int nrows unsigned-int ncols double* result double* operand))
  (void ccdoubles_real_matrix_asin (unsigned-int nrows unsigned-int ncols double* result double* operand))
  (void ccdoubles_real_matrix_acos (unsigned-int nrows unsigned-int ncols double* result double* operand))
  (void ccdoubles_real_matrix_atan (unsigned-int nrows unsigned-int ncols double* result double* operand))
  (void ccdoubles_real_matrix_atan2 (unsigned-int nrows unsigned-int ncols double* result double* operand1 double* operand2))

  (void ccdoubles_real_matrix_sinh (unsigned-int nrows unsigned-int ncols double* result double* operand))
  (void ccdoubles_real_matrix_cosh (unsigned-int nrows unsigned-int ncols double* result double* operand))
  (void ccdoubles_real_matrix_tanh (unsigned-int nrows unsigned-int ncols double* result double* operand))
  (void ccdoubles_real_matrix_asinh (unsigned-int nrows unsigned-int ncols double* result double* operand))
  (void ccdoubles_real_matrix_acosh (unsigned-int nrows unsigned-int ncols double* result double* operand))
  (void ccdoubles_real_matrix_atanh (unsigned-int nrows unsigned-int ncols double* result double* operand))

  (void ccdoubles_cplx_vector_clear (unsigned-int nslots double-complex* vector))
  ;;(void ccdoubles_cplx_vector_set   (unsigned-int nslots double-complex* vector double-complex value))
  (void ccdoubles_cplx_vector_set_split (unsigned-int nslots double-complex* vector double value_re double value_im))
  (void ccdoubles_cplx_vector_copy (unsigned-int nslots double-complex* dst double-complex* src))

  (void ccdoubles_cplx_vector_real (unsigned-int nslots double* result double-complex* operand))
  (void ccdoubles_cplx_vector_imag (unsigned-int nslots double* result double-complex* operand))
  (void ccdoubles_cplx_vector_magnitude (unsigned-int nslots double* result double-complex* operand))
  (void ccdoubles_cplx_vector_angle (unsigned-int nslots double* result double-complex* operand))
  (void ccdoubles_cplx_vector_conj (unsigned-int nslots double-complex* result double-complex* operand))
  (void ccdoubles_cplx_vector_from_rect (unsigned-int nslots double-complex* result double* real double* imag))
  (void ccdoubles_cplx_vector_from_polar (unsigned-int nslots double-complex* result double* magnitude double* angle))

  (void ccdoubles_cplx_vector_add (unsigned-int nslots double-complex* result double-complex* operand1 double-complex* operand2))
  (void ccdoubles_cplx_vector_sub (unsigned-int nslots double-complex* result double-complex* operand1 double-complex* operand2))
  (void ccdoubles_cplx_vector_mul (unsigned-int nslots double-complex* result double-complex* operand1 double-complex* operand2))
  (void ccdoubles_cplx_vector_div (unsigned-int nslots double-complex* result double-complex* operand1 double-complex* operand2))
  (void ccdoubles_cplx_vector_neg (unsigned-int nslots double-complex* result double-complex* operand))
  (double-complex ccdoubles_cplx_vector_scalar_product (unsigned-int nslots double-complex* operand1 double-complex* operand2))
  ;; (void ccdoubles_cplx_vector_scalar_mul (unsigned-int nslots double-complex* result
  ;; 						       double-complex lambda double-complex* operand))
  (void ccdoubles_cplx_vector_scalar_mul_split (unsigned-int nslots double-complex* result
							     double lambda_re double lambda_im
							     double-complex* operand))
  ;; (void ccdoubles_cplx_vector_linear_combination
  ;; 	(unsigned-int nslots double-complex* result
  ;; 		      double-complex alpha double-complex* operand1
  ;; 		      double-complex beta double-complex* operand2))
  (void ccdoubles_cplx_vector_linear_combination_split
	(unsigned-int nslots double-complex* result
		      double alpha_re double alpha_im double-complex* operand1
		      double beta_re double beta_im double-complex* operand2))

  (void ccdoubles_cplx_vector_exp (unsigned-int nslots double-complex* result double-complex* operand))
  (void ccdoubles_cplx_vector_log (unsigned-int nslots double-complex* result double-complex* operand))
  (void ccdoubles_cplx_vector_log10 (unsigned-int nslots double-complex* result double-complex* operand))
  (void ccdoubles_cplx_vector_sqrt (unsigned-int nslots double-complex* result double-complex* operand))
  (void ccdoubles_cplx_vector_pow (unsigned-int nslots double-complex* result double-complex* operand1 double-complex* operand2))

  (void ccdoubles_cplx_vector_sin (unsigned-int nslots double-complex* result double-complex* operand))
  (void ccdoubles_cplx_vector_cos (unsigned-int nslots double-complex* result double-complex* operand))
  (void ccdoubles_cplx_vector_tan (unsigned-int nslots double-complex* result double-complex* operand))
  (void ccdoubles_cplx_vector_asin (unsigned-int nslots double-complex* result double-complex* operand))
  (void ccdoubles_cplx_vector_acos (unsigned-int nslots double-complex* result double-complex* operand))
  (void ccdoubles_cplx_vector_atan (unsigned-int nslots double-complex* result double-complex* operand))

  (void ccdoubles_cplx_vector_sinh (unsigned-int nslots double-complex* result double-complex* operand))
  (void ccdoubles_cplx_vector_cosh (unsigned-int nslots double-complex* result double-complex* operand))
  (void ccdoubles_cplx_vector_tanh (unsigned-int nslots double-complex* result double-complex* operand))
  (void ccdoubles_cplx_vector_asinh (unsigned-int nslots double-complex* result double-complex* operand))
  (void ccdoubles_cplx_vector_acosh (unsigned-int nslots double-complex* result double-complex* operand))
  (void ccdoubles_cplx_vector_atanh (unsigned-int nslots double-complex* result double-complex* operand))

  (void ccdoubles_cplx_matrix_clear (unsigned-int nrows unsigned-int ncols double-complex* matrix))
  ;;(void ccdoubles_cplx_matrix_set   (unsigned-int nrows unsigned-int ncols double-complex* matrix double-complex value))
  (void ccdoubles_cplx_matrix_set_split (unsigned-int nrows unsigned-int ncols double-complex* matrix double value_re double value_im))
  (void ccdoubles_cplx_matrix_copy (unsigned-int nrows unsigned-int ncols double-complex* dst double-complex* src))

  (void ccdoubles_cplx_matrix_real (unsigned-int nrows unsigned-int ncols double* result double-complex* operand))
  (void ccdoubles_cplx_matrix_imag (unsigned-int nrows unsigned-int ncols double* result double-complex* operand))
  (void ccdoubles_cplx_matrix_magnitude (unsigned-int nrows unsigned-int ncols double* result double-complex* operand))
  (void ccdoubles_cplx_matrix_angle (unsigned-int nrows unsigned-int ncols double* result double-complex* operand))
  (void ccdoubles_cplx_matrix_conj (unsigned-int nrows unsigned-int ncols double-complex* result double-complex* operand))
  (void ccdoubles_cplx_matrix_from_rect (unsigned-int nrows unsigned-int ncols double-complex* result double* real double* imag))
  (void ccdoubles_cplx_matrix_from_polar (unsigned-int nrows unsigned-int ncols double-complex* result double* magnitude double* angle))

  (void ccdoubles_cplx_matrix_add (unsigned-int nrows unsigned-int ncols double-complex* result double-complex* operand1 double-complex* operand2))
  (void ccdoubles_cplx_matrix_sub (unsigned-int nrows unsigned-int ncols double-complex* result double-complex* operand1 double-complex* operand2))
  (void ccdoubles_cplx_matrix_mul (unsigned-int nrows unsigned-int ncols double-complex* result double-complex* operand1 double-complex* operand2))
  (void ccdoubles_cplx_matrix_div (unsigned-int nrows unsigned-int ncols double-complex* result double-complex* operand1 double-complex* operand2))
  (void ccdoubles_cplx_matrix_neg (unsigned-int nrows unsigned-int ncols double-complex* result double-complex* operand))

  ;; (void ccdoubles_cplx_matrix_scalar_mul (unsigned-int nrows unsigned-int ncols double-complex* result
  ;; 						       double-complex lambda double-complex* operand))
  (void ccdoubles_cplx_matrix_scalar_mul_split
	(unsigned-int nrows unsigned-int ncols double-complex* result
		      double lambda_re double lambda_im double-complex* operand))
  ;; (void ccdoubles_cplx_matrix_linear_combination
  ;; 	(unsigned-int nrows unsigned-int ncols
  ;; 		      double-complex* result
  ;; 		      double-complex alpha double-complex* operand1
  ;; 		      double-complex beta double-complex* operand2))
  (void ccdoubles_cplx_matrix_linear_combination_split
	(unsigned-int nrows unsigned-int ncols
		      double-complex* result
		      double alpha_re double alpha_im double-complex* operand1
		      double beta_re double beta_im double-complex* operand2))

  (void ccdoubles_cplx_matrix_transpose (unsigned-int nrows unsigned-int ncols double-complex* result double-complex* operand))

  (void ccdoubles_cplx_matrix_conjugate_transpose (unsigned-int operand_nrows unsigned-int operand_ncols double-complex* result double-complex* operand))

  (void ccdoubles_cplx_matrix_rowcol_mul (unsigned-int result_nrows unsigned-int operand_n unsigned-int result_ncols double-complex* result double-complex* operand1 double-complex* operand2))

  (void ccdoubles_cplx_matrix_exp (unsigned-int nrows unsigned-int ncols double-complex* result double-complex* operand))
  (void ccdoubles_cplx_matrix_log (unsigned-int nrows unsigned-int ncols double-complex* result double-complex* operand))
  (void ccdoubles_cplx_matrix_log10 (unsigned-int nrows unsigned-int ncols double-complex* result double-complex* operand))
  (void ccdoubles_cplx_matrix_sqrt (unsigned-int nrows unsigned-int ncols double-complex* result double-complex* operand))
  (void ccdoubles_cplx_matrix_pow (unsigned-int nrows unsigned-int ncols double-complex* result double-complex* operand1 double-complex* operand2))

  (void ccdoubles_cplx_matrix_sin (unsigned-int nrows unsigned-int ncols double-complex* result double-complex* operand))
  (void ccdoubles_cplx_matrix_cos (unsigned-int nrows unsigned-int ncols double-complex* result double-complex* operand))
  (void ccdoubles_cplx_matrix_tan (unsigned-int nrows unsigned-int ncols double-complex* result double-complex* operand))
  (void ccdoubles_cplx_matrix_asin (unsigned-int nrows unsigned-int ncols double-complex* result double-complex* operand))
  (void ccdoubles_cplx_matrix_acos (unsigned-int nrows unsigned-int ncols double-complex* result double-complex* operand))
  (void ccdoubles_cplx_matrix_atan (unsigned-int nrows unsigned-int ncols double-complex* result double-complex* operand))

  (void ccdoubles_cplx_matrix_sinh (unsigned-int nrows unsigned-int ncols double-complex* result double-complex* operand))
  (void ccdoubles_cplx_matrix_cosh (unsigned-int nrows unsigned-int ncols double-complex* result double-complex* operand))
  (void ccdoubles_cplx_matrix_tanh (unsigned-int nrows unsigned-int ncols double-complex* result double-complex* operand))
  (void ccdoubles_cplx_matrix_asinh (unsigned-int nrows unsigned-int ncols double-complex* result double-complex* operand))
  (void ccdoubles_cplx_matrix_acosh (unsigned-int nrows unsigned-int ncols double-complex* result double-complex* operand))
  (void ccdoubles_cplx_matrix_atanh (unsigned-int nrows unsigned-int ncols double-complex* result double-complex* operand))

  (void ccdoubles_int_vector_clear (unsigned-int nslots signed-int* vector))
  (void ccdoubles_int_vector_set (unsigned-int nslots signed-int* vector int value))
  (void ccdoubles_int_vector_copy (unsigned-int nslots signed-int* dst signed-int* src))

  (void ccdoubles_int_matrix_clear (unsigned-int nrows unsigned-int ncols signed-int* matrix))
  (void ccdoubles_int_matrix_set (unsigned-int nrows unsigned-int ncols signed-int* matrix int value))
  (void ccdoubles_int_matrix_copy (unsigned-int nrows unsigned-int ncols signed-int* dst signed-int* src))

  (char* ccdoubles_version_string (void))
  (signed-int ccdoubles_version_interface_current	(void))
  (signed-int ccdoubles_version_interface_revision (void))
  (signed-int ccdoubles_version_interface_age (void))
  #| end of functions |#)
 #| end of library |# )

