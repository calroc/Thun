
u32 parse_head;

u32
make_non_list_node(u32 rest) {
	// A this point text[parse_head:rest] is a term: symbol or int.
	return push_symbol("foo", empty_list);
}


// Extract terms from the text until a closing bracket is found.
u32
parse_list(char *text)
{
	// trim blanks
	while (text[parse_head] && text[parse_head] == ' ') ++parse_head;

	if (!text[parse_head]) {
		print_str("Missing ']' bracket. A");
		print_endl();
		error = MISSING_CLOSING_BRACKET;
		return 0;
	};

	// So now we want to collect all chars up until the
	// next '[', ']', blank, or the end of the string.
	u32 rest = parse_head;
	while (text[rest]) {
		if (text[rest] == '[' || text[rest] == ']' || text[rest] == ' ')
			break;
		//print_str(text + rest);print_endl();
		++rest;
	}

	if (!text[rest]) {
		print_str("Missing ']' bracket. B");
		print_endl();
		error = MISSING_CLOSING_BRACKET;
		return 0;
	};

	// A this point text[parse_head:rest] is a term: symbol or int.
	// or it's empty
	u32 diff = rest - parse_head;
	u32 result = 0;
	if (diff) {
		result = make_non_list_node(rest);
		parse_head = rest;
	/*} else if ('[' == text[rest]) {*/
	/*	parse_head = rest + 1;*/
	/*	result = cons(parse_list(text), empty_list);*/
	} else if (']' == text[rest]) {
		parse_head = rest + 1;
		result = empty_list;
	}
	tails[VALUE_OF(result)] = parse_list(text);
	return result;
}

u32
parse(char *text)
{
	parse_head = 0;

	// trim blanks
	while (text[parse_head] && text[parse_head] == ' ') ++parse_head;

	if (!text[parse_head]) return empty_list;
	
	if ('[' == text[parse_head]) {
		++parse_head;
		u32 list = parse_list(text);
		if (error != NO_ERROR)
			return 0;
		return list;
		/*foo = cons(list, foo);*/
	}
	/*if (']')*/
	/*print_str(text + parse_head);*/
	/*print_str(text + parse_head);*/
}
