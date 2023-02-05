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
JoyList def_swons_body;
JoyList def_infra_body;


/*
Next, we want an initializer function to fill out the body pointers.
*/

void
init_defs(void)
{
    
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
	def_disenstacken_body = text_to_expression("? [uncons ?] loop pop");
	def_swons_body = text_to_expression("swap cons");
	def_infra_body = text_to_expression("swons swaack [i] dip swaack");
}


/*
Last, a set of functions to go in the wordlist, one for each definition.
*/
    
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
void def_swons(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_swons_body, expression); }
void def_infra(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) { push_quote_onto_expression(def_infra_body, expression); }
