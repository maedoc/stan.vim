" Vim syntax file
" Language:	Stan (http://mc-stan.org)
" Maintainer:	J. Guo <guojq28@gmail.com> 
" Last Change:  Aug 7 2016
" Filenames:	*.stan
" URL:		

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

if version >= 600
  setlocal iskeyword=@,48-57,_,.
else
  set iskeyword=@,48-57,_,.
endif

syn case match

syntax match stanCommentError display "\*/" 
syntax match stanCommentStartError display "/\*"me=e-1 contained 

syn keyword	stanTodo	TODO FIXME TBD contained 
syn cluster	stanCommentGroup	contains=stanTodo

" Comment
" copied from c.vim, not exactly know what is going on
syn region stanCommentL start="//" skip="\\$" end="$" keepend contains=@stanCommentGroup,@Spell 
syn match  stanCommentL /\#.*/ 
" syn region stanComment start="/\*" end="\*/" contains=stanTodo 
syn region stanComment start="/\*" end="\*/" contains=@stanCommentGroup,@stanCommentError,@Spell extend 

" Constant
" string enclosed in double quotes
syn region stanString start=/"/ skip=/\\\\\|\\"/ end=/"/
" string enclosed in single quotes
syn region stanString start=/'/ skip=/\\\\\|\\'/ end=/'/
" number with no fractional part or exponent
syn match stanNumber /\d\+/
" floating point number with integer and fractional parts and optional exponent
syn match stanFloat /\d\+\.\d*\([Ee][-+]\=\d\+\)\=/
" floating point number with no integer part and optional exponent
syn match stanFloat /\.\d\+\([Ee][-+]\=\d\+\)\=/
" floating point number with no fractional part and optional exponent
syn match stanFloat /\d\+[Ee][-+]\=\d\+/

" Identifier
" identifier with leading letter and optional following keyword characters
syn match stanIdentifier /\a\k*/
" identifier with leading period, one or more digits, and at least one non-digit keyword character
syn match stanIdentifier /\.\d*\K\k*/

" Statement
" syn keyword stanStatement   break next return
syn keyword stanConditional if else
syn keyword stanRepeat      for in while

" Constant
" syn keyword stanConstant LETTERS letters month.ab month.name pi
" syn keyword stanConstant NULL
" syn keyword stanBoolean  FALSE TRUE
" syn keyword stanNumber   NA
syn match stanArrow /<\{1}-/
syn match stanDistributed /\~/ 

" Type
" syn keyword stanType data model  array category character complex real function integer list logical matrix numeric vector data.frame 
" syn keyword stanType var 

syn keyword stanType cholesky_factor_corr cholesky_factor_cov corr_matrix cov_matrix int matrix ordered positive_ordered real row_vector simplex unit_vector vector
syn keyword stanBlk functions data model parameters transformed generated quantities 
" syn keyword stanBlk derived
syn match equalSign /=/
syn keyword stanLU lower upper nextgroup=equalSign skipwhite

syn match leftParen /(/
syn keyword stanDistributions normal uniform multi_normal inv_gamma 
syn keyword stanDistributions normal_log uniform_log multi_normal_log inv_gamma_log 
syn keyword stanDistributions categorical categorical_log  
syn keyword stanDistributions normal_trunc_l normal_trunc_h  normal_trunc_lh exponential inv_chi_square student cauchy  
syn keyword stanDistributions normal_trunc_l_log normal_trunc_h_log  normal_trunc_lh_log exponential_log inv_chi_square_log student_log cauchy_log  
syn keyword stanDistributions double_exponential weibull logistic lognormal dirichlet skew_normal
syn keyword stanDistributions double_exponential_log weibull_log logistic_log lognormal_log dirichlet_log 
syn keyword stanDistributions bernoulli binomial neg_binomial beta_binomial poisson 
syn keyword stanDistributions bernoulli_log binomial_log neg_binomial_log beta_binomial_log poisson_log 
syn keyword stanDistributions bernoulli_logit bernoulli_logit_log 
syn keyword stanDistributions wishart inv_wishart 
syn keyword stanDistributions wishart_log inv_wishart_log  exp_mod_normal frechet scaled_inv_chi_square

syn keyword stanDistributions normal_cdf_log exponential_cdf_log  weibull_cdf_log gamma_cdf_log 
syn keyword stanDistributions normal_ccdf_log exponential_ccdf_log  weibull_ccdf_log gamma_ccdf_log 
" only highight gamma beta such that there is '(' after
" that indicating it is distribution.
" For the time being, not define others. `normal` should 
" not be a name used. 
syn match stanDistributionsII  /gamma\(\s*(\)\@=/
syn match stanDistributionsII  /beta\(\s*(\)\@=/


syn keyword stanFunctions exp sum log pow mean abs inv_logit  determinant  fabs pi 
syn keyword stanFunctions col row sd 
syn keyword stanFunctions int_step inv_cloglog inverse lbeta lgamma lmgamma log log10 log1m log1m_inv_logit log1p logp1p_exp log2 log_determinant log_inv_logit log_sum_exp logit max mdivide_left_tri_low midivide_right_tri_low mean min multiply_Log multiply_lower_tri_self_transpose negative_epsilon negative_infinity not_a_number pi positive_infinity pow prod round row rows sd sin singuar_values sinh size softmax sqrt sqrt2 square step sum tan tanh
syn keyword stanFunctions binary_log_loss binomial_coefficient_log  atan2 atan asin asinh atanh block cbrt ceil cholesy_decompose col cols cos cosh crossprod cumulative_sum diag_matrix diag_post_multiply diag_pre_multiply diagnoal dims dot_product dot_self e eigenvalues_sym eigenvectors_sym epsilon erf erfc exp exp2 expm1 fdim floor fma fmax fmin fmod hypot if_else 
syn keyword stanFunctions bernoulli_cdf beta_binomial_cdf beta_cdf binomial_cdf exponential_cdf inv_chi_square_cdf inv_gamma_cdf logistic_cdf lognormal_cdf neg_binomial_cdf normal_cdf pareto_cdf poisson_cdf scaled_inv_chi_square_cdf student_t_cdf
syn keyword stanFunctions bernoulli_ccdf beta_binomial_ccdf beta_ccdf binomial_ccdf exponential_ccdf inv_chi_square_ccdf inv_gamma_ccdf logistic_ccdf lognormal_ccdf neg_binomial_ccdf normal_ccdf pareto_ccdf poisson_ccdf scaled_inv_chi_square_ccdf student_t_ccdf rows_dot_self rows_dot_product 
syn keyword stanFunctions increment_log_prob falling_factorial rising_factorial frechet_rng frechet_log frechet_cdf frechet_cdf_log frechet_ccdf_log get_lp gumbel_rng gumbel_cdf_log gumbel_ccdf_log head tail hypergeometric_rng hypergeometric_log  sub_col sub_row squared_distance  sort_indices_asc sort_indices_desc sort_asc sort_desc skew_normal_rng skew_normal_log skew_normal_cdf_log skew_normal_ccdf_log
syn keyword stanFunctions double_exponential_cdf  double_exponential_cdf_log double_exponential_ccdf_log double_exponential_rng segment scaled_inv_chi_square_ccdf scaled_inv_chi_square_cdf_log scaled_inv_chi_square_ccdf_log scaled_inv_chi_square_rng 
syn keyword stanFunctions exp_mod_normal_rng  exp_mod_normal_log exp_mod_normal_cdf exp_mod_normal_ccdf exp_mod_normal_ccdf_log log_diff_exp log_mix modified_bessel_first_kind modified_bessel_second_kind multi_gp_log multi_gp_cholesky_log log_softmax log_rising_factorial  log_falling_factorial tcrossprod 
syn keyword stanDistributions multi_gp_cholesky multi_gp rayleigh von_mises
syn keyword stanFunctions bernoulli_rng beta_binomial_rng beta_rng binomial_rng exponential_rng inv_chi_square_rng inv_gamma_rng logistic_rng lognormal_rng neg_binomial_rng normal_rng pareto_rng poisson_rng scaled_inv_chi_square_rng student_t_rng rep_array rep_matrix von_mises_rng von_mises_log trace_gen_quad_form trace_quad_form trace to_array_1d to_vector to_array_2d to_matrix 
syn keyword stanFunctions eigenvectors_sym eigenvectors_sym inverse_spd inv_sqrt num_elements qr_Q qr_R quad_form quad_form_sym quad_form_diag rank  rayleigh_rng rayleigh_log rayleigh_cdf rayleigh_cdf_log rayleigh_ccdf_log


syn keyword stanFunctions beta_lpdf cauchy_lpdf chi_square_lpdf dirichlet_lpdf double_exponential_lpdf exp_mod_normal_lpdf exponential_lpdf frechet_lpdf gamma_lpdf gaussian_dlm_obs_lpdf gumbel_lpdf inv_chi_square_lpdf inv_gamma_lpdf inv_wishart_lpdf lkj_corr_cholesky_lpdf lkj_corr_lpdf logistic_lpdf lognormal_lpdf multi_gp_cholesky_lpdf multi_gp_lpdf multi_normal_cholesky_lpdf multi_normal_lpdf multi_normal_prec_lpdf multi_student_t_lpdf normal_lpdf pareto_lpdf pareto_type_2_lpdf rayleigh_lpdf scaled_inv_chi_square_lpdf skew_normal_lpdf student_t_lpdf uniform_lpdf von_mises_lpdf weibull_lpdf wiener_lpdf wishart_lpdf
syn keyword stanFunctions bernoulli_logit_lpmf bernoulli_lpmf beta_binomial_lpmf binomial_logit_lpmf binomial_lpmf categorical_logit_lpmf categorical_lpmf hypergeometric_lpmf multinomial_lpmf neg_binomial_2_log_lpmf neg_binomial_2_lpmf neg_binomial_lpmf ordered_logistic_lpmf poisson_log_lpmf poisson_lpmf
syn keyword stanFunctions bernoulli_cdf bernoulli_lccdf bernoulli_lcdf beta_binomial_cdf beta_binomial_lccdf beta_binomial_lcdf beta_cdf beta_lccdf beta_lcdf binomial_cdf binomial_lccdf binomial_lcdf cauchy_cdf cauchy_lccdf cauchy_lcdf chi_square_cdf chi_square_lccdf chi_square_lcdf double_exponential_cdf double_exponential_lccdf double_exponential_lcdf exp_mod_normal_cdf exp_mod_normal_lccdf exp_mod_normal_lcdf exponential_cdf exponential_lccdf exponential_lcdf frechet_cdf frechet_lccdf frechet_lcdf gamma_cdf gamma_lccdf gamma_lcdf gumbel_cdf gumbel_lccdf gumbel_lcdf inv_chi_square_cdf inv_chi_square_lccdf inv_chi_square_lcdf inv_gamma_cdf inv_gamma_lccdf inv_gamma_lcdf logistic_cdf logistic_lccdf logistic_lcdf lognormal_cdf lognormal_lccdf lognormal_lcdf neg_binomial_2_cdf neg_binomial_2_lccdf neg_binomial_2_lcdf neg_binomial_cdf neg_binomial_lccdf neg_binomial_lcdf normal_cdf normal_lccdf normal_lcdf pareto_cdf pareto_lccdf pareto_lcdf pareto_type_2_cdf pareto_type_2_lccdf pareto_type_2_lcdf poisson_cdf poisson_lccdf poisson_lcdf rayleigh_cdf rayleigh_lccdf rayleigh_lcdf scaled_inv_chi_square_cdf scaled_inv_chi_square_lccdf scaled_inv_chi_square_lcdf skew_normal_cdf skew_normal_lccdf skew_normal_lcdf student_t_cdf student_t_lccdf student_t_lcdf uniform_cdf uniform_lccdf uniform_lcdf weibull_cdf weibull_lccdf weibull_lcdf
syn keyword stanFunctions bernoulli_lcdf beta_binomial_lcdf beta_lcdf binomial_lcdf cauchy_lcdf chi_square_lcdf double_exponential_lcdf exp_mod_normal_lcdf exponential_lcdf frechet_lcdf gamma_lcdf gumbel_lcdf inv_chi_square_lcdf inv_gamma_lcdf logistic_lcdf lognormal_lcdf neg_binomial_2_lcdf neg_binomial_lcdf normal_lcdf pareto_lcdf pareto_type_2_lcdf poisson_lcdf rayleigh_lcdf scaled_inv_chi_square_lcdf skew_normal_lcdf student_t_lcdf uniform_lcdf weibull_lcdf
syn keyword stanFunctions bernoulli_lccdf beta_binomial_lccdf beta_lccdf binomial_lccdf cauchy_lccdf chi_square_lccdf double_exponential_lccdf exp_mod_normal_lccdf exponential_lccdf frechet_lccdf gamma_lccdf gumbel_lccdf inv_chi_square_lccdf inv_gamma_lccdf logistic_lccdf lognormal_lccdf neg_binomial_2_lccdf neg_binomial_lccdf normal_lccdf pareto_lccdf pareto_type_2_lccdf poisson_lccdf rayleigh_lccdf scaled_inv_chi_square_lccdf skew_normal_lccdf student_t_lccdf uniform_lccdf weibull_lccdf
syn keyword stanFunctions bernoulli_logit_rng bernoulli_rng beta_binomial_rng beta_rng binomial_rng categorical_logit_rng categorical_rng cauchy_rng chi_square_rng dirichlet_rng double_exponential_rng exp_mod_normal_rng exponential_rng frechet_rng gamma_rng gumbel_rng hypergeometric_rng inv_chi_square_rng inv_gamma_rng inv_wishart_rng lkj_corr_cholesky_rng lkj_corr_rng logistic_rng lognormal_rng multi_normal_cholesky_rng multi_normal_rng multi_student_t_rng multinomial_rng neg_binomial_2_log_rng neg_binomial_2_rng neg_binomial_rng normal_rng ordered_logistic_rng pareto_rng pareto_type_2_rng poisson_log_rng poisson_rng rayleigh_rng scaled_inv_chi_square_rng skew_normal_rng student_t_rng uniform_rng von_mises_rng weibull_rng wishart_rng
syn keyword stanFunctions logical_eq sort_asc prod sqrt2 if_else log10 rep_vector not_a_number tgamma columns_dot_product pi mdivide_right_tri_low log1m_exp inv_sqrt exp2 asinh rank singular_values step fmin row fma abs to_row_vector mean cos num_elements diag_matrix mdivide_left_spd logical_gt expm1 distance multiply sum csr_extract_w squared_distance sinh trace gamma_q trace_gen_quad_form diag_pre_multiply owens_t log_sum_exp choose matrix_exp positive_infinity acosh is_nan trigamma inv_logit erf dot_product sub_row falling_factorial sort_indices_asc inv_cloglog logical_and size log1m csr_matrix_times_vector sort_indices_desc rows logical_lte floor log append_row crossprod log_diff_exp inv_square lchoose head log2 Phi get_lp rep_row_vector softmax mdivide_right_spd pow machine_precision qr_Q sort_desc fdim trace_quad_form divide tail acos cov_exp_quad cumulative_sum modified_bessel_first_kind min Phi_approx eigenvalues_sym multiply_lower_tri_self_transpose qr_R log1p_exp minus block atan2 rep_array erfc log_rising_factorial logical_gte elt_divide square atanh rising_factorial diagonal rows_dot_self inverse_spd log_falling_factorial cosh tan lmgamma logical_or tcrossprod diag_post_multiply modified_bessel_second_kind csr_extract_v elt_multiply trunc eigenvectors_sym quad_form append_col asin quad_form_diag logit cols dims max bessel_first_kind rep_matrix negative_infinity dot_self cbrt csr_extract_u modulus e is_inf logical_negation determinant subtract fmax inv_Phi exp segment to_array_1d col quad_form_sym sub_col logical_lt log_softmax rows_dot_product atan lmultiply cholesky_decompose lgamma log_mix sd tanh variance columns_dot_self lbeta log_inv_logit to_array_2d csr_to_dense_matrix inverse inc_beta mdivide_left fmod log_determinant digamma hypot gamma_p to_matrix to_vector log1p target log1m_inv_logit fabs sqrt transpose add ceil logical_neq mdivide_left_tri_low int_step sin binary_log_loss inv round bessel_second_kind mdivide_right

" step function in stan
" defined as step(y) = y < 0 ? 0 : 1
syn keyword stanFunctions step append_row append_col 
" normal CDF 
syn keyword stanFunctions Phi  Phi_approx 

" Special
syn match stanDelimiter /[,;:><]/

" Error
syn region stanRegion matchgroup=Delimiter start=/(/ matchgroup=Delimiter end=/)/ transparent contains=ALLBUT,stanError,stanBraceError,stanCurlyError
syn region stanRegion matchgroup=Delimiter start=/{/ matchgroup=Delimiter end=/}/ transparent contains=ALLBUT,stanError,stanBraceError,stanParenError
syn region stanRegion matchgroup=Delimiter start=/\[/ matchgroup=Delimiter end=/]/ transparent contains=ALLBUT,stanError,stanCurlyError,stanParenError
syn match stanError      /[)\]}]/
syn match stanBraceError /[)}]/ contained
syn match stanCurlyError /[)\]]/ contained
syn match stanParenError /[\]}]/ contained
syn match stanDAError  /<\{2,}-/
" syn match stanEqError  /\(lower\s*\|upper\s*\|>\|<\)\@<!=/
syn match stanCmp      /==/
syn match stanCmp      /!=/
syn match stanCmp      />=/
syn match stanCmp      /<=/
syn match stanCmp      />/
syn match stanCmp      /</
" transpose 
syn match stanCmp      /'/

syntax keyword stanCppConflict  alignas alignof and and_eq asm auto bitand bitor bool break case catch char
syntax keyword stanCppConflict  char16_t char32_t class compl const constexpr const_cast continue decltype
syntax keyword stanCppConflict  default delete do double dynamic_cast enum explicit export extern false
syntax keyword stanCppConflict  float friend goto inline long mutable namespace new noexcept not not_eq
syntax keyword stanCppConflict  nullptr operator or or_eq private protected public register reinterpret_cast
syntax keyword stanCppConflict  short signed sizeof static static_assert static_cast struct switch
syntax keyword stanCppConflict  template this thread_local throw true try typedef typeid typename union
syntax keyword stanCppConflict  unsigned using virtual volatile wchar_t xor xor_eq

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_r_syn_inits")
  if version < 508
    let did_r_syn_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif
  " copied from sas.vim 
  hi sComment term=bold cterm=NONE ctermfg=Blue  ctermbg=Black gui=NONE guifg=DarkGreen guibg=White 

  HiLink stanComment     Comment
  HiLink stanCommentL    Comment
  HiLink stanConstant    Constant
  HiLink stanString      String
  HiLink stanNumber      Number
  HiLink stanBoolean     Boolean
  HiLink stanFloat       Float
  HiLink stanStatement   Statement
  HiLink stanConditional Conditional
  HiLink stanRepeat      Repeat
  HiLink stanIdentifier  Normal
  HiLink stanArrow	 Statement	
  HiLink stanDistributed Statement 
  HiLink stanType        Type
  HiLink stanFunctions   Function 
  HiLink stanDistributions Type 
  HiLink stanDistributionsII Type 
  HiLink stanDelimiter   Delimiter
  HiLink stanError       Error
  HiLink stanBraceError  Error
  HiLink stanCurlyError  Error
  HiLink stanParenError  Error
  HiLink stanEqError     Error
  HiLink stanDaError     Error
  HiLink stanCppConflict Error
  HiLink stanBlk         Special 
  HiLink stanLU          Special 
  HiLink stanCmp         Operator
  HiLink equalSign  Operator
  delcommand HiLink
endif

let b:current_syntax="stan"

" vim: ts=8 sw=2
" If more fancy colors (or modes) are needed, see sas.vim 
" for examples. 
