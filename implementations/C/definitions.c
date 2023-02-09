/*
Auto-generated file by convert_defs.py
Do not edit.
*/

#include "joy.h"
#include "definitions.h"


/*
Declare a bunch of list pointers to eventually hold the body expressions
of the definitions.
*/
    
JoyList def_eq_body;
JoyList def_gt_body;
JoyList def_lt_body;
JoyList def_neq_body;
JoyList def_le_body;
JoyList def_ge_body;
JoyList def_HYPHEN_MINUS_HYPHEN_MINUS_body;
JoyList def_QUESTION_MARK_body;
JoyList def_AMPERSAND_AMPERSAND_body;
JoyList def_PLUS_SIGN_PLUS_SIGN_body;
JoyList def_VERTICAL_LINE_VERTICAL_LINE_body;
JoyList def_EXCLAMATION_MARK_HYPHEN_MINUS_body;
JoyList def_LESS_THAN_SIGN_LEFT_CURLY_BRACKET_RIGHT_CURLY_BRACKET_body;
JoyList def_LESS_THAN_SIGN_LESS_THAN_SIGN_LEFT_CURLY_BRACKET_RIGHT_CURLY_BRACKET_body;
JoyList def_abs_body;
JoyList def_anamorphism_body;
JoyList def_app1_body;
JoyList def_app2_body;
JoyList def_app3_body;
JoyList def_appN_body;
JoyList def_at_body;
JoyList def_average_body;
JoyList def_b_body;
JoyList def_binary_body;
JoyList def_ccccons_body;
JoyList def_ccons_body;
JoyList def_cleave_body;
JoyList def_clop_body;
JoyList def_codi_body;
JoyList def_codireco_body;
JoyList def_dinfrirst_body;
JoyList def_dipd_body;
JoyList def_disenstacken_body;
JoyList def_divmod_body;
JoyList def_down_to_zero_body;
JoyList def_drop_body;
JoyList def_dupd_body;
JoyList def_dupdd_body;
JoyList def_dupdip_body;
JoyList def_dupdipd_body;
JoyList def_enstacken_body;
JoyList def_flatten_body;
JoyList def_fork_body;
JoyList def_fourth_body;
JoyList def_gcd_body;
JoyList def_genrec_body;
JoyList def_grabN_body;
JoyList def_grba_body;
JoyList def_hypot_body;
JoyList def_ifte_body;
JoyList def_ii_body;
JoyList def_infra_body;
JoyList def_infrst_body;
JoyList def_make_generator_body;
JoyList def_neg_body;
JoyList def_not_body;
JoyList def_nulco_body;
JoyList def_null_body;
JoyList def_nullary_body;
JoyList def_of_body;
JoyList def_pam_body;
JoyList def_pm_body;
JoyList def_popd_body;
JoyList def_popdd_body;
JoyList def_popop_body;
JoyList def_popopop_body;
JoyList def_popopd_body;
JoyList def_popopdd_body;
JoyList def_product_body;
JoyList def_quoted_body;
JoyList def_range_body;
JoyList def_range_to_zero_body;
JoyList def_reco_body;
JoyList def_reverse_body;
JoyList def_roll_GREATER_THAN_SIGN_body;
JoyList def_roll_LESS_THAN_SIGN_body;
JoyList def_rollup_body;
JoyList def_rolldown_body;
JoyList def_rrest_body;
JoyList def_run_body;
JoyList def_second_body;
JoyList def_shift_body;
JoyList def_shunt_body;
JoyList def_size_body;
JoyList def_small_body;
JoyList def_spiral_next_body;
JoyList def_split_at_body;
JoyList def_split_list_body;
JoyList def_sqr_body;
JoyList def_stackd_body;
JoyList def_step_zero_body;
JoyList def_stuncons_body;
JoyList def_sum_body;
JoyList def_swapd_body;
JoyList def_swons_body;
JoyList def_swoncat_body;
JoyList def_tailrec_body;
JoyList def_take_body;
JoyList def_ternary_body;
JoyList def_third_body;
JoyList def_tuck_body;
JoyList def_unary_body;
JoyList def_uncons_body;
JoyList def_unit_body;
JoyList def_unquoted_body;
JoyList def_unstack_body;
JoyList def_unswons_body;
JoyList def_while_body;
JoyList def_x_body;
JoyList def_step_body;
JoyList def__step0_body;
JoyList def__step1_body;
JoyList def__stept_body;
JoyList def_times_body;
JoyList def__times0_body;
JoyList def__times1_body;
JoyList def__timest_body;
JoyList def_map_body;
JoyList def__map_QUESTION_MARK_body;
JoyList def__mape_body;
JoyList def__map0_body;
JoyList def__map1_body;
JoyList def__map2_body;
JoyList def_xor_body;
JoyList def_and_body;
JoyList def_or_body;


/*
Next, we want an initializer function to fill out the body pointers.
*/

void
init_defs(void)
{
    
	def_eq_body = text_to_expression("[false] [true] [false] cmp");
	def_gt_body = text_to_expression("[true] [false] [false] cmp");
	def_lt_body = text_to_expression("[false] [false] [true] cmp");
	def_neq_body = text_to_expression("[true] [false] [true] cmp");
	def_le_body = text_to_expression("[false] [true] [true] cmp");
	def_ge_body = text_to_expression("[true] [true] [false] cmp");
	def_HYPHEN_MINUS_HYPHEN_MINUS_body = text_to_expression("1 -");
	def_QUESTION_MARK_body = text_to_expression("dup bool");
	def_AMPERSAND_AMPERSAND_body = text_to_expression("nulco [nullary [false]] dip branch");
	def_PLUS_SIGN_PLUS_SIGN_body = text_to_expression("1 +");
	def_VERTICAL_LINE_VERTICAL_LINE_body = text_to_expression("nulco [nullary] dip [true] branch");
	def_EXCLAMATION_MARK_HYPHEN_MINUS_body = text_to_expression("0 >=");
	def_LESS_THAN_SIGN_LEFT_CURLY_BRACKET_RIGHT_CURLY_BRACKET_body = text_to_expression("[] swap");
	def_LESS_THAN_SIGN_LESS_THAN_SIGN_LEFT_CURLY_BRACKET_RIGHT_CURLY_BRACKET_body = text_to_expression("[] rollup");
	def_abs_body = text_to_expression("dup 0 < [] [neg] branch");
	def_anamorphism_body = text_to_expression("[pop []] swap [dip swons] genrec");
	def_app1_body = text_to_expression("grba infrst");
	def_app2_body = text_to_expression("[grba swap grba swap] dip [infrst] cons ii");
	def_app3_body = text_to_expression("3 appN");
	def_appN_body = text_to_expression("[grabN] codi map reverse disenstacken");
	def_at_body = text_to_expression("drop first");
	def_average_body = text_to_expression("[sum] [size] cleave /");
	def_b_body = text_to_expression("[i] dip i");
	def_binary_body = text_to_expression("unary popd");
	def_ccccons_body = text_to_expression("ccons ccons");
	def_ccons_body = text_to_expression("cons cons");
	def_cleave_body = text_to_expression("fork popdd");
	def_clop_body = text_to_expression("cleave popdd");
	def_codi_body = text_to_expression("cons dip");
	def_codireco_body = text_to_expression("codi reco");
	def_dinfrirst_body = text_to_expression("dip infrst");
	def_dipd_body = text_to_expression("[dip] codi");
	def_disenstacken_body = text_to_expression("swaack pop");
	def_divmod_body = text_to_expression("[/] [%] clop");
	def_down_to_zero_body = text_to_expression("[0 >] [dup --] while");
	def_drop_body = text_to_expression("[rest] times");
	def_dupd_body = text_to_expression("[dup] dip");
	def_dupdd_body = text_to_expression("[dup] dipd");
	def_dupdip_body = text_to_expression("dupd dip");
	def_dupdipd_body = text_to_expression("dup dipd");
	def_enstacken_body = text_to_expression("stack [clear] dip");
	def_flatten_body = text_to_expression("<{} [concat] step");
	def_fork_body = text_to_expression("[i] app2");
	def_fourth_body = text_to_expression("rest third");
	def_gcd_body = text_to_expression("true [tuck mod dup 0 >] loop pop");
	def_genrec_body = text_to_expression("[[genrec] ccccons] nullary swons concat ifte");
	def_grabN_body = text_to_expression("<{} [cons] times");
	def_grba_body = text_to_expression("[stack popd] dip");
	def_hypot_body = text_to_expression("[sqr] ii + sqrt");
	def_ifte_body = text_to_expression("[nullary] dipd swap branch");
	def_ii_body = text_to_expression("[dip] dupdip i");
	def_infra_body = text_to_expression("swons swaack [i] dip swaack");
	def_infrst_body = text_to_expression("infra first");
	def_make_generator_body = text_to_expression("[codireco] ccons");
	def_neg_body = text_to_expression("0 swap -");
	def_not_body = text_to_expression("[true] [false] branch");
	def_nulco_body = text_to_expression("[nullary] cons");
	def_null_body = text_to_expression("[] concat bool not");
	def_nullary_body = text_to_expression("[stack] dinfrirst");
	def_of_body = text_to_expression("swap at");
	def_pam_body = text_to_expression("[i] map");
	def_pm_body = text_to_expression("[+] [-] clop");
	def_popd_body = text_to_expression("[pop] dip");
	def_popdd_body = text_to_expression("[pop] dipd");
	def_popop_body = text_to_expression("pop pop");
	def_popopop_body = text_to_expression("pop popop");
	def_popopd_body = text_to_expression("[popop] dip");
	def_popopdd_body = text_to_expression("[popop] dipd");
	def_product_body = text_to_expression("1 swap [*] step");
	def_quoted_body = text_to_expression("[unit] dip");
	def_range_body = text_to_expression("[0 <=] [1 - dup] anamorphism");
	def_range_to_zero_body = text_to_expression("unit [down_to_zero] infra");
	def_reco_body = text_to_expression("rest cons");
	def_reverse_body = text_to_expression("<{} shunt");
	def_roll_GREATER_THAN_SIGN_body = text_to_expression("swap swapd");
	def_roll_LESS_THAN_SIGN_body = text_to_expression("swapd swap");
	def_rollup_body = text_to_expression("roll>");
	def_rolldown_body = text_to_expression("roll<");
	def_rrest_body = text_to_expression("rest rest");
	def_run_body = text_to_expression("<{} infra");
	def_second_body = text_to_expression("rest first");
	def_shift_body = text_to_expression("uncons [swons] dip");
	def_shunt_body = text_to_expression("[swons] step");
	def_size_body = text_to_expression("[pop ++] step_zero");
	def_small_body = text_to_expression("dup null [rest null] [pop true] branch");
	def_spiral_next_body = text_to_expression("[[[abs] ii <=] [[<>] [pop !-] ||] &&] [[!-] [[++]] [[--]] ifte dip] [[pop !-] [--] [++] ifte] ifte");
	def_split_at_body = text_to_expression("[drop] [take] clop");
	def_split_list_body = text_to_expression("[take reverse] [drop] clop");
	def_sqr_body = text_to_expression("dup mul");
	def_stackd_body = text_to_expression("[stack] dip");
	def_step_zero_body = text_to_expression("0 roll> step");
	def_stuncons_body = text_to_expression("stack uncons");
	def_sum_body = text_to_expression("[+] step_zero");
	def_swapd_body = text_to_expression("[swap] dip");
	def_swons_body = text_to_expression("swap cons");
	def_swoncat_body = text_to_expression("swap concat");
	def_tailrec_body = text_to_expression("[i] genrec");
	def_take_body = text_to_expression("<<{} [shift] times pop");
	def_ternary_body = text_to_expression("binary popd");
	def_third_body = text_to_expression("rest second");
	def_tuck_body = text_to_expression("dup swapd");
	def_unary_body = text_to_expression("nullary popd");
	def_uncons_body = text_to_expression("[first] [rest] cleave");
	def_unit_body = text_to_expression("[] cons");
	def_unquoted_body = text_to_expression("[i] dip");
	def_unstack_body = text_to_expression("[[] swaack] dip swoncat swaack pop");
	def_unswons_body = text_to_expression("uncons swap");
	def_while_body = text_to_expression("swap nulco dupdipd concat loop");
	def_x_body = text_to_expression("dup i");
	def_step_body = text_to_expression("[_step0] x");
	def__step0_body = text_to_expression("_step1 [popopop] [_stept] branch");
	def__step1_body = text_to_expression("[?] dipd roll<");
	def__stept_body = text_to_expression("[uncons] dipd [dupdipd] dip x");
	def_times_body = text_to_expression("[_times0] x");
	def__times0_body = text_to_expression("_times1 [popopop] [_timest] branch");
	def__times1_body = text_to_expression("[dup 0 >] dipd roll<");
	def__timest_body = text_to_expression("[[--] dip dupdipd] dip x");
	def_map_body = text_to_expression("[_map0] cons [[] [_map?] [_mape]] dip tailrec");
	def__map_QUESTION_MARK_body = text_to_expression("pop bool not");
	def__mape_body = text_to_expression("popd reverse");
	def__map0_body = text_to_expression("[_map1] dipd _map2");
	def__map1_body = text_to_expression("stackd shift");
	def__map2_body = text_to_expression("[infrst] cons dipd roll< swons");
	def_xor_body = text_to_expression("[not not] [not] branch");
	def_and_body = text_to_expression("[pop false] [not not] branch");
	def_or_body = text_to_expression("[not not] [pop true] branch");
}


/*
Last, a set of functions to go in the wordlist, one for each definition.
*/
    
void def_eq(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_eq_body, expression); }
void def_gt(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_gt_body, expression); }
void def_lt(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_lt_body, expression); }
void def_neq(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_neq_body, expression); }
void def_le(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_le_body, expression); }
void def_ge(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_ge_body, expression); }
void def_HYPHEN_MINUS_HYPHEN_MINUS(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_HYPHEN_MINUS_HYPHEN_MINUS_body, expression); }
void def_QUESTION_MARK(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_QUESTION_MARK_body, expression); }
void def_AMPERSAND_AMPERSAND(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_AMPERSAND_AMPERSAND_body, expression); }
void def_PLUS_SIGN_PLUS_SIGN(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_PLUS_SIGN_PLUS_SIGN_body, expression); }
void def_VERTICAL_LINE_VERTICAL_LINE(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_VERTICAL_LINE_VERTICAL_LINE_body, expression); }
void def_EXCLAMATION_MARK_HYPHEN_MINUS(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_EXCLAMATION_MARK_HYPHEN_MINUS_body, expression); }
void def_LESS_THAN_SIGN_LEFT_CURLY_BRACKET_RIGHT_CURLY_BRACKET(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_LESS_THAN_SIGN_LEFT_CURLY_BRACKET_RIGHT_CURLY_BRACKET_body, expression); }
void def_LESS_THAN_SIGN_LESS_THAN_SIGN_LEFT_CURLY_BRACKET_RIGHT_CURLY_BRACKET(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_LESS_THAN_SIGN_LESS_THAN_SIGN_LEFT_CURLY_BRACKET_RIGHT_CURLY_BRACKET_body, expression); }
void def_abs(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_abs_body, expression); }
void def_anamorphism(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_anamorphism_body, expression); }
void def_app1(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_app1_body, expression); }
void def_app2(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_app2_body, expression); }
void def_app3(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_app3_body, expression); }
void def_appN(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_appN_body, expression); }
void def_at(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_at_body, expression); }
void def_average(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_average_body, expression); }
void def_b(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_b_body, expression); }
void def_binary(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_binary_body, expression); }
void def_ccccons(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_ccccons_body, expression); }
void def_ccons(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_ccons_body, expression); }
void def_cleave(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_cleave_body, expression); }
void def_clop(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_clop_body, expression); }
void def_codi(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_codi_body, expression); }
void def_codireco(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_codireco_body, expression); }
void def_dinfrirst(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_dinfrirst_body, expression); }
void def_dipd(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_dipd_body, expression); }
void def_disenstacken(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_disenstacken_body, expression); }
void def_divmod(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_divmod_body, expression); }
void def_down_to_zero(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_down_to_zero_body, expression); }
void def_drop(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_drop_body, expression); }
void def_dupd(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_dupd_body, expression); }
void def_dupdd(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_dupdd_body, expression); }
void def_dupdip(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_dupdip_body, expression); }
void def_dupdipd(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_dupdipd_body, expression); }
void def_enstacken(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_enstacken_body, expression); }
void def_flatten(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_flatten_body, expression); }
void def_fork(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_fork_body, expression); }
void def_fourth(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_fourth_body, expression); }
void def_gcd(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_gcd_body, expression); }
void def_genrec(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_genrec_body, expression); }
void def_grabN(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_grabN_body, expression); }
void def_grba(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_grba_body, expression); }
void def_hypot(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_hypot_body, expression); }
void def_ifte(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_ifte_body, expression); }
void def_ii(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_ii_body, expression); }
void def_infra(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_infra_body, expression); }
void def_infrst(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_infrst_body, expression); }
void def_make_generator(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_make_generator_body, expression); }
void def_neg(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_neg_body, expression); }
void def_not(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_not_body, expression); }
void def_nulco(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_nulco_body, expression); }
void def_null(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_null_body, expression); }
void def_nullary(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_nullary_body, expression); }
void def_of(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_of_body, expression); }
void def_pam(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_pam_body, expression); }
void def_pm(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_pm_body, expression); }
void def_popd(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_popd_body, expression); }
void def_popdd(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_popdd_body, expression); }
void def_popop(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_popop_body, expression); }
void def_popopop(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_popopop_body, expression); }
void def_popopd(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_popopd_body, expression); }
void def_popopdd(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_popopdd_body, expression); }
void def_product(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_product_body, expression); }
void def_quoted(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_quoted_body, expression); }
void def_range(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_range_body, expression); }
void def_range_to_zero(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_range_to_zero_body, expression); }
void def_reco(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_reco_body, expression); }
void def_reverse(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_reverse_body, expression); }
void def_roll_GREATER_THAN_SIGN(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_roll_GREATER_THAN_SIGN_body, expression); }
void def_roll_LESS_THAN_SIGN(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_roll_LESS_THAN_SIGN_body, expression); }
void def_rollup(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_rollup_body, expression); }
void def_rolldown(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_rolldown_body, expression); }
void def_rrest(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_rrest_body, expression); }
void def_run(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_run_body, expression); }
void def_second(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_second_body, expression); }
void def_shift(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_shift_body, expression); }
void def_shunt(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_shunt_body, expression); }
void def_size(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_size_body, expression); }
void def_small(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_small_body, expression); }
void def_spiral_next(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_spiral_next_body, expression); }
void def_split_at(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_split_at_body, expression); }
void def_split_list(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_split_list_body, expression); }
void def_sqr(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_sqr_body, expression); }
void def_stackd(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_stackd_body, expression); }
void def_step_zero(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_step_zero_body, expression); }
void def_stuncons(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_stuncons_body, expression); }
void def_sum(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_sum_body, expression); }
void def_swapd(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_swapd_body, expression); }
void def_swons(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_swons_body, expression); }
void def_swoncat(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_swoncat_body, expression); }
void def_tailrec(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_tailrec_body, expression); }
void def_take(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_take_body, expression); }
void def_ternary(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_ternary_body, expression); }
void def_third(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_third_body, expression); }
void def_tuck(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_tuck_body, expression); }
void def_unary(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_unary_body, expression); }
void def_uncons(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_uncons_body, expression); }
void def_unit(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_unit_body, expression); }
void def_unquoted(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_unquoted_body, expression); }
void def_unstack(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_unstack_body, expression); }
void def_unswons(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_unswons_body, expression); }
void def_while(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_while_body, expression); }
void def_x(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_x_body, expression); }
void def_step(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_step_body, expression); }
void def__step0(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def__step0_body, expression); }
void def__step1(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def__step1_body, expression); }
void def__stept(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def__stept_body, expression); }
void def_times(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_times_body, expression); }
void def__times0(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def__times0_body, expression); }
void def__times1(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def__times1_body, expression); }
void def__timest(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def__timest_body, expression); }
void def_map(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_map_body, expression); }
void def__map_QUESTION_MARK(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def__map_QUESTION_MARK_body, expression); }
void def__mape(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def__mape_body, expression); }
void def__map0(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def__map0_body, expression); }
void def__map1(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def__map1_body, expression); }
void def__map2(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def__map2_body, expression); }
void def_xor(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_xor_body, expression); }
void def_and(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_and_body, expression); }
void def_or(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_or_body, expression); }
