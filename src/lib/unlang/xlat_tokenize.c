/*
 *   This program is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 2 of the License, or
 *   (at your option) any later version.
 *
 *   This program is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.
 *
 *   You should have received a copy of the GNU General Public License
 *   along with this program; if not, write to the Free Software
 *   Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301, USA
 */

/**
 * $Id$
 *
 * @file xlat_tokenize.c
 * @brief String expansion ("translation").  Tokenizes xlat expansion strings.
 *
 * @copyright 2000 Alan DeKok (aland@freeradius.org)
 * @copyright 2000,2006 The FreeRADIUS server project
 * @copyright 2017-2020 Arran Cudbard-Bell (a.cudbardb@freeradius.org)
 */

RCSID("$Id$")

#include <freeradius-devel/util/debug.h>
#include <freeradius-devel/util/sbuff.h>
#include <freeradius-devel/server/request.h>
#include <freeradius-devel/server/regex.h>
#include <freeradius-devel/unlang/xlat_priv.h>

#include <ctype.h>

#undef XLAT_DEBUG
#ifdef DEBUG_XLAT
#  define XLAT_DEBUG(_fmt, ...)		DEBUG3("%s[%i] "_fmt, __FILE__, __LINE__, ##__VA_ARGS__)
#else
#  define XLAT_DEBUG(...)
#endif

/** These rules apply to literals and function arguments inside of an expansion
 *
 */
static fr_sbuff_escape_rules_t const xlat_escape = {
	.chr = '\\',
	.subs = {
		['a'] = '\a',
		['b'] = '\b',
		['e'] = '\\',
		['n'] = '\n',
		['r'] = '\r',
		['t'] = '\t',
		['v'] = '\v',
		['\\'] = '\\',
		['%'] = '%',	/* Expansion begin */
		[':'] = ':',	/* Alternation begin */
		['}'] = '}'	/* Expansion end */
	},
	.do_hex = true,
	.do_oct = true
};

/** Wrap the escapes in a structure to allow them to be passed to xlat_literal
 *
 */
static fr_sbuff_parse_rules_t const xlat_rules = {
	.escapes = &xlat_escape
};

/** Allocate an xlat node
 *
 * @param[in] ctx	to allocate node in.
 * @param[in] type	of the node.
 * @param[in] in	original input string.
 * @return A new xlat node.
 */
static inline xlat_exp_t *xlat_exp_alloc(TALLOC_CTX *ctx, xlat_type_t type, char const *in, size_t inlen)
{
	xlat_exp_t *node;

	MEM(node = talloc_zero(ctx, xlat_exp_t));
	node->type = type;
	if (in) node->fmt = talloc_bstrndup(node, in, inlen);

	return node;
}

/** Allocate an xlat node with no name, and no type set
 *
 * @param[in] ctx	to allocate node in.
 * @return A new xlat node.
 */
static inline xlat_exp_t *xlat_exp_alloc_null(TALLOC_CTX *ctx)
{
	xlat_exp_t *node;

	MEM(node = talloc_zero(ctx, xlat_exp_t));
	return node;
}

/** Set the type of an xlat node
 *
 * @param[in] node	to set type for.
 * @param[in] type	to set.
 */
static inline void xlat_exp_set_type(xlat_exp_t *node, xlat_type_t type)
{
	node->type = type;
}

/** Set the format string for an xlat node
 *
 * @param[in] node	to set fmt for.
 * @param[in] fmt	talloced buffer to set as the fmt string.
 */
static inline void xlat_exp_set_name_buffer(xlat_exp_t *node, char const *fmt)
{
	if (node->fmt) talloc_const_free(node->fmt);
	node->fmt = talloc_bstrdup(node, fmt);
}

/** Set the format string for an xlat node
 *
 * @param[in] node	to set fmt for.
 * @param[in] fmt	talloced buffer to set as the fmt string.
 */
static inline void xlat_exp_set_name_buffer_shallow(xlat_exp_t *node, char *fmt)
{
	if (node->fmt) talloc_const_free(node->fmt);
	node->fmt = fmt;
}

/** Try to convert an xlat to a tmpl for efficiency
 *
 * @param ctx to allocate new tmpl_t in.
 * @param node to convert.
 * @return
 *	- NULL if unable to convert (not necessarily error).
 *	- A new #tmpl_t.
 */
tmpl_t *xlat_to_tmpl_attr(TALLOC_CTX *ctx, xlat_exp_t *node)
{
	tmpl_t *vpt;

	if (node->next || (node->type != XLAT_ATTRIBUTE) || !tmpl_is_attr(node->attr)) return NULL;

	/*
	 *   Concat means something completely different as an attribute reference
	 *   Count isn't implemented.
	 */
	if ((tmpl_num(node->attr) == NUM_COUNT) || (tmpl_num(node->attr) == NUM_ALL)) return NULL;

	vpt = tmpl_alloc(ctx, TMPL_TYPE_ATTR, T_BARE_WORD, node->fmt, talloc_array_length(node->fmt) - 1);
	if (!vpt) return NULL;

	tmpl_attr_copy(vpt, node->attr);

	TMPL_VERIFY(vpt);

	return vpt;
}

/** Convert attr tmpl to an xlat for &attr[*]
 *
 * @param ctx to allocate new xlat_expt_t in.
 * @param vpt to convert.
 * @return
 *	- NULL if unable to convert (not necessarily error).
 *	- a new #tmpl_t.
 */
xlat_exp_t *xlat_from_tmpl_attr(TALLOC_CTX *ctx, tmpl_t *vpt)
{
	xlat_exp_t *node;

	if (!tmpl_is_attr(vpt)) return NULL;

	node = xlat_exp_alloc(ctx, XLAT_ATTRIBUTE, vpt->name, vpt->len);
	node->attr = tmpl_alloc(node, TMPL_TYPE_ATTR, T_BARE_WORD, node->fmt, talloc_array_length(node->fmt) - 1);
	tmpl_attr_copy(node->attr, vpt);

	return node;
}

static int xlat_tokenize_expansion(TALLOC_CTX *ctx, xlat_exp_t **head, fr_sbuff_t *in,
				   tmpl_rules_t const *ar_rules);

static int xlat_tokenize_literal(TALLOC_CTX *ctx, xlat_exp_t **head, fr_sbuff_t *in, bool brace,
				 fr_sbuff_parse_rules_t const *p_rules, tmpl_rules_t const *ar_rules);

static inline int xlat_tokenize_alternation(TALLOC_CTX *ctx, xlat_exp_t **head, fr_sbuff_t *in,
					    tmpl_rules_t const *ar_rules)
{
	xlat_exp_t	*node;

	XLAT_DEBUG("ALTERNATE <-- %pV", fr_box_strvalue_len(fr_sbuff_current(in), fr_sbuff_remaining(in)));

	node = xlat_exp_alloc_null(ctx);
	xlat_exp_set_type(node, XLAT_ALTERNATE);
	if (xlat_tokenize_expansion(node, &node->child, in, ar_rules) < 0) {
	error:
		*head = NULL;
		talloc_free(node);
		return -1;
	}

	if (!fr_sbuff_adv_past_str_literal(in, ":-")) {
		fr_strerror_printf("Expected ':-' after first expansion");
		goto error;
	}

	/*
	 *	Allow the RHS to be empty as a special case.
	 */
	if (fr_sbuff_next_if_char(in, '}')) {
		node->alternate = xlat_exp_alloc(node, XLAT_LITERAL, "", 0);
		node->async_safe = node->child->async_safe;
		*head = node;
		return 0;
	}

	/*
	 *	Parse the alternate expansion.
	 */
	if (xlat_tokenize_literal(node, &node->alternate, in, true, &xlat_rules, ar_rules) < 0) goto error;

	if (!node->alternate) {
		talloc_free(node);
		fr_strerror_printf("Empty expansion is invalid");
		goto error;
	}

	if (!fr_sbuff_next_if_char(in, '}')) {
		fr_strerror_printf("Missing closing brace");
		goto error;
	}

	node->async_safe = (node->child->async_safe && node->alternate->async_safe);
	*head = node;

	return 0;
}

#ifdef HAVE_REGEX
/** Parse an xlat reference
 *
 * Allows access to a subcapture groups
 * @verbatim %{<num>} @endverbatim
 *
 */
static inline int xlat_tokenize_regex(TALLOC_CTX *ctx, xlat_exp_t **head, fr_sbuff_t *in)
{
	uint8_t			num;
	xlat_exp_t		*node;
	fr_sbuff_parse_error_t	err;
	fr_sbuff_marker_t	m_s;

	XLAT_DEBUG("REGEX <-- %pV", fr_box_strvalue_len(fr_sbuff_current(in), fr_sbuff_remaining(in)));

	fr_sbuff_marker(&m_s, in);

	fr_sbuff_out(&err, &num, in);
	if (err != FR_SBUFF_PARSE_OK) {
	invalid_ref:
		fr_strerror_printf("Invalid regex reference.  Must be in range 0-%u", REQUEST_MAX_REGEX);
		fr_sbuff_marker_release(&m_s);
		return -1;
	}

	if (num > REQUEST_MAX_REGEX) {
		fr_sbuff_set(in, &m_s);
		goto invalid_ref;
	}

	if (!fr_sbuff_is_char(in, '}')) {
		if (!fr_sbuff_remaining(in)) {
			fr_strerror_printf("Missing closing brace");
			fr_sbuff_marker_release(&m_s);
			return -1;
		}
		fr_sbuff_set(in, &m_s);
		fr_sbuff_marker_release(&m_s);
		return 1;
	}

	node = xlat_exp_alloc(ctx, XLAT_REGEX, fr_sbuff_current(&m_s), fr_sbuff_behind(&m_s));
	node->regex_index = num;
	node->async_safe = true;

	fr_sbuff_marker_release(&m_s);
	fr_sbuff_next(in);	/* Skip '}' */

	*head = node;

	return 0;
}
#endif

/** Parse an xlat function and its child arguments
 *
 * Parses a function call string in the format
 * @verbatim %{<func>:<arguments} @endverbatim
 *
 */
static inline int xlat_tokenize_function(TALLOC_CTX *ctx, xlat_exp_t **head, fr_sbuff_t *in,
					 tmpl_rules_t const *rules)
{
	xlat_exp_t		*node;
	xlat_t			*func;
	fr_sbuff_marker_t	m_s;

	/*
	 *	Special characters, spaces, etc. cannot be
	 *	module names.
	 */
	static bool const	func_chars[UINT8_MAX + 1] = {
					SBUFF_CHAR_CLASS_ALPHA_NUM,
					['.'] = true, ['-'] = true, ['_'] = true,
				};

	XLAT_DEBUG("FUNC <-- %pV", fr_box_strvalue_len(fr_sbuff_current(in), fr_sbuff_remaining(in)));

	/*
	 *	%{module:args}
	 */
	fr_sbuff_marker(&m_s, in);
	fr_sbuff_adv_past_allowed(in, SIZE_MAX, func_chars);

	if (!fr_sbuff_is_char(in, ':')) {
	not_a_function:
		fr_sbuff_set(in, &m_s);		/* backtrack */
		fr_sbuff_marker_release(&m_s);
		return 1;
	}

	func = xlat_func_find(fr_sbuff_current(&m_s), fr_sbuff_behind(&m_s));
	if (!func) goto not_a_function;

	/*
	 *	Allocate a node to hold the function
	 */
	node = xlat_exp_alloc(ctx, XLAT_FUNC, fr_sbuff_current(&m_s), fr_sbuff_behind(&m_s));
	node->xlat = func;

	fr_sbuff_next(in);			/* Skip the ':' */
	XLAT_DEBUG("FUNC-ARGS <-- %s ... %pV",
		   node->fmt, fr_box_strvalue_len(fr_sbuff_current(in), fr_sbuff_remaining(in)));

	fr_sbuff_marker_release(&m_s);

	/*
	 *	Now parse the child nodes that form the
	 *	function's arguments.
	 */
	if (xlat_tokenize_literal(node, &node->child, in, true, &xlat_rules, rules) < 0) {
	error:
		*head = NULL;
		talloc_free(node);
		return -1;
	}

	if (!fr_sbuff_next_if_char(in, '}')) {
		fr_strerror_printf("Missing closing brace");
		goto error;
	}

	node->async_safe = (func->async_safe && (!node->child || node->child->async_safe));
	*head = node;

	return 0;
}

/** Parse an attribute ref or a virtual attribute
 *
 */
static inline int xlat_tokenize_attribute(TALLOC_CTX *ctx, xlat_exp_t **head, fr_sbuff_t *in,
					  fr_sbuff_parse_rules_t const *p_rules, tmpl_rules_t const *ar_rules)
{
	ssize_t			slen;
	attr_ref_error_t	err;
	tmpl_t		*vpt = NULL;
	xlat_exp_t		*node;
	xlat_t			*func;
	fr_sbuff_marker_t	m_s;

	XLAT_DEBUG("ATTRIBUTE <-- %pV", fr_box_strvalue_len(fr_sbuff_current(in), fr_sbuff_remaining(in)));

	/*
	 *	We need a local copy as we always allow unknowns.
	 *	This is because not all attribute references
	 *	reference real attributes in the dictionaries,
	 *	and instead are "virtual" attributes like
	 *	Foreach-Variable-N.
	 */
	tmpl_rules_t		 our_ar_rules;

	if (ar_rules) {
		memcpy(&our_ar_rules, ar_rules, sizeof(our_ar_rules));
	} else {
		memset(&our_ar_rules, 0, sizeof(our_ar_rules));
	}

	our_ar_rules.allow_unparsed = true;		/* So we can check for virtual attributes later */
  	our_ar_rules.prefix = TMPL_ATTR_REF_PREFIX_NO;	/* Must be NO to stop %{&User-Name} */

	fr_sbuff_marker(&m_s, in);

	MEM(node = xlat_exp_alloc_null(ctx));
	slen = tmpl_afrom_attr_substr(node, &err, &vpt, in, p_rules, &our_ar_rules);
	if (slen <= 0) {
		fr_sbuff_advance(in, slen * -1);

		/*
		 *	If the parse error occurred before the ':'
		 *	then the error is changed to 'Unknown module',
		 *	as it was more likely to be a bad module name,
		 *	than a request qualifier.
		 */
		if ((err == ATTR_REF_ERROR_MISSING_TERMINATOR) && fr_sbuff_is_char(in, ':')) {
			fr_strerror_printf("Unknown expansion function or invalid list qualifier");
			fr_sbuff_set(in, &m_s);
		}
	error:
		*head = NULL;
		fr_sbuff_marker_release(&m_s);
		talloc_free(node);
		return -1;
	}

	/*
	 *	Might be a virtual XLAT attribute, which is identical
	 *	to a normal function but called without an argument
	 *	list.
	 */
	switch (vpt->type) {
	case TMPL_TYPE_ATTR_UNPARSED:
		func = xlat_func_find(tmpl_attr_unparsed(vpt), -1);
		if (func && (func->type == XLAT_FUNC_SYNC)) {
			xlat_exp_set_type(node, XLAT_VIRTUAL);
			xlat_exp_set_name_buffer(node, tmpl_attr_unparsed(vpt));
			talloc_free(vpt);	/* Free the tmpl, we don't need it */

			XLAT_DEBUG("VIRTUAL <-- %pV", fr_box_strvalue_len(fr_sbuff_current(in), fr_sbuff_remaining(in)));
			node->xlat = func;
			node->async_safe = func->async_safe;

			if (!fr_sbuff_next_if_char(in, '}')) {
				fr_strerror_printf("Missing closing brace");
				goto error;
			}

			goto done;
		}

		if (!ar_rules || !ar_rules->allow_unparsed) {
			talloc_free(vpt);

			fr_strerror_printf("Unknown attribute");
			fr_sbuff_set(in, &m_s);		/* Error at the start of the attribute */
			goto error;
		}
		FALL_THROUGH;

	default:
		xlat_exp_set_name_buffer(node, vpt->name);
		xlat_exp_set_type(node, XLAT_ATTRIBUTE);
		break;
	}

	if (!fr_sbuff_next_if_char(in, '}')) {
		fr_strerror_printf("Missing closing brace");
		goto error;
	}

	node->attr = vpt;
	node->async_safe = true;		/* attribute expansions are always async-safe */

done:
	*head = node;
	fr_sbuff_marker_release(&m_s);
	return 0;
}

static int xlat_tokenize_expansion(TALLOC_CTX *ctx, xlat_exp_t **head, fr_sbuff_t *in, tmpl_rules_t const *ar_rules)
{
	size_t			len;
	fr_sbuff_marker_t	s_m;
	char			hint;
	int			ret;
	fr_sbuff_term_t		hint_tokens = FR_SBUFF_TERMS(
					L(" "),		/* First special token is a ' ' - Likely a syntax error */
					L(":"),		/* First special token is a ':' i.e. '%{func:' */
					L("["),		/* First special token is a '[' i.e. '%{attr[<idx>]}' */
					L("}")		/* First special token is a '}' i.e. '%{<attrref>}' */
				);

	fr_sbuff_parse_rules_t	attr_p_rules = {
					.escapes = &xlat_escape,
					.terminals = &FR_SBUFF_TERM("}")
				};

	XLAT_DEBUG("EXPANSION <-- %pV", fr_box_strvalue_len(fr_sbuff_current(in), fr_sbuff_remaining(in)));

	/*
	 *	%{...}:-bar}
	 */
	if (fr_sbuff_adv_past_str_literal(in, "%{")) return xlat_tokenize_alternation(ctx, head, in, ar_rules);

	/*
	 *	:-bar}
	 */
	if (fr_sbuff_is_str_literal(in, ":-")) {
		fr_strerror_printf("First item in alternation cannot be empty");
		return -2;
	}

#ifdef HAVE_REGEX
	/*
	 *	Handle regex's %{<num>} specially.
	 */
	if (fr_sbuff_is_digit(in)) {
		ret = xlat_tokenize_regex(ctx, head, in);
		if (ret <= 0) return ret;
	}
#endif /* HAVE_REGEX */

	/*
	 *	%{Attr-Name}
	 *	%{Attr-Name[#]}
	 *	%{Tunnel-Password:1}
	 *	%{Tunnel-Password:1[#]}
	 *	%{request:Attr-Name}
	 *	%{request:Tunnel-Password:1}
	 *	%{request:Tunnel-Password:1[#]}
	 *	%{mod:foo}
	 */

	/*
	 *	Check for empty expressions %{} %{: %{[
	 */
	fr_sbuff_marker(&s_m, in);
	len = fr_sbuff_adv_until(in, SIZE_MAX, &hint_tokens, '\0');

	/*
	 *      This means the end of a string not containing any of the other
	 *	tokens was reached.
	 *
	 *	e.g. '%{myfirstxlat'
	 */
	if (!fr_sbuff_remaining(in)) {
		fr_strerror_printf("Missing closing brace");
		fr_sbuff_marker_release(&s_m);
		return -1;
	}

	hint = *fr_sbuff_current(in);

	XLAT_DEBUG("EXPANSION HINT TOKEN '%c'", hint);
	if (len == 0) {
		switch (hint) {
		case '}':
			fr_strerror_printf("Empty expression is invalid");
			return -1;

		case ':':
			fr_strerror_printf("Missing expansion function or list qualifier");
			return -1;

		case '[':
			fr_strerror_printf("Missing attribute name");
			return -1;

		default:
			break;
		}
	}

	/*
	 *      Hint token is a ':' it's either:
	 *	- An xlat function %{<func>:<args}
	 *	- An attribute reference with a list separator %{<list>:<attr>}
	 */
	switch (hint) {
	case ':':
	{
		fr_sbuff_set(in, &s_m);		/* backtrack */
		fr_sbuff_marker_release(&s_m);

		ret = xlat_tokenize_function(ctx, head, in, ar_rules);
		if (ret <= 0) return ret;

		if (xlat_tokenize_attribute(ctx, head, in, &attr_p_rules, ar_rules) < 0) return -1;
	}
		break;

	/*
	 *	Hint token is a:
	 *	- '[' - Which is an attribute index, so it must be an attribute.
	 *      - '}' - The end of the expansion, which means it was a bareword.
	 */
	case '}':
	case '[':
		fr_sbuff_set(in, &s_m);		/* backtrack */
		fr_sbuff_marker_release(&s_m);

		if (xlat_tokenize_attribute(ctx, head, in, &attr_p_rules, ar_rules) < 0) return -1;
		break;

	/*
	 *	Hint token was whitespace
	 *
	 *	e.g. '%{my '
	 */
	default:
		/*
		 *	Box print is so we get \t \n etc..
		 */
		fr_strerror_printf("Invalid char '%pV' in expression", fr_box_strvalue_len(fr_sbuff_current(in), 1));
		return -1;
	}

	return 0;
}

/** Parse an xlat literal i.e. a non-expansion or non-function
 *
 * When this function is being used outside of an xlat expansion, i.e. on a string
 * which contains one or more xlat expansions, it uses the terminal grammar and
 * escaping rules of that string type.
 *
 * Which this function is being used inside of an xlat expansion, it uses xlat specific
 * terminal grammar and escaping rules.
 *
 * This allows us to be smart about processing quotes within the expansions themselves.
 */
static int xlat_tokenize_literal(TALLOC_CTX *ctx, xlat_exp_t **head, fr_sbuff_t *in, bool brace,
				 fr_sbuff_parse_rules_t const *p_rules, tmpl_rules_t const *ar_rules)
{
	TALLOC_CTX			*our_ctx = ctx;
	xlat_exp_t			*node = NULL;
	size_t				len;
	fr_sbuff_term_t			expansions = FR_SBUFF_TERMS(
						L("%C"),
						L("%D"),
						L("%G"),
						L("%H"),
						L("%I"),
						L("%M"),
						L("%S"),
						L("%T"),
						L("%Y"),
						L("%c"),
						L("%d"),
						L("%e"),
						L("%l"),
						L("%m"),
						L("%n"),
						L("%s"),
						L("%t"),
						L("%v"),
						L("%{"),
						L("}")
					);
	fr_sbuff_term_t			*tokens;
	fr_cursor_t			cursor;
	fr_sbuff_escape_rules_t	const	*escapes;

	*head = NULL;

	fr_cursor_init(&cursor, head);
	escapes = p_rules ? p_rules->escapes : NULL;
	tokens = p_rules && p_rules->terminals ?
			fr_sbuff_terminals_amerge(NULL, p_rules->terminals, &expansions) : &expansions;

	for (;;) {
		char *str;

		/*
		 *	pre-allocate the node so we don't have to steal it later.
		 */
		node = xlat_exp_alloc_null(our_ctx);

		/*
		 *	Find the next token
		 */
		len = fr_sbuff_out_aunescape_until(node, &str, in, SIZE_MAX, tokens, escapes);

		/*
		 *	It's a literal, create a literal node...
		 */
		if (len > 0) {
			xlat_exp_set_type(node, XLAT_LITERAL);
			xlat_exp_set_name_buffer_shallow(node, str);

			XLAT_DEBUG("LITERAL <-- %pV",
				   fr_box_strvalue_len(str, talloc_array_length(str) - 1));
			node->async_safe = true; /* literals are always true */
			fr_cursor_insert(&cursor, node);
			our_ctx = node;
			node = NULL;
		}

		if (fr_sbuff_adv_past_str_literal(in, "%{")) {
			if (len == 0) TALLOC_FREE(node); /* Free the empty node */

			if (xlat_tokenize_expansion(our_ctx, &node, in, ar_rules) < 0) {
			error:
				*head = NULL;
				talloc_free(node);
				fr_cursor_head(&cursor);
				fr_cursor_free_list(&cursor);

				/*
				 *	Free our temporary array of terminals
				 */
				if (tokens != &expansions) talloc_free(tokens);
				return -1;
			}
			fr_cursor_insert(&cursor, node);
			our_ctx = node;
			node = NULL;
			continue;
		}

		/*
		 *	%[a-z] - A one letter expansion
		 */
		if (fr_sbuff_next_if_char(in, '%') && fr_sbuff_is_alpha(in)) {
			XLAT_DEBUG("ONE-LETTER <-- %pV",
				   fr_box_strvalue_len(str, talloc_array_length(str) - 1));

			if (len == 0) {
				talloc_free_children(node);	/* re-use empty nodes */
			} else {
				node = xlat_exp_alloc_null(our_ctx);
			}

			fr_sbuff_out_abstrncpy(node, &str, in, 1);
			xlat_exp_set_type(node, XLAT_ONE_LETTER);
			xlat_exp_set_name_buffer_shallow(node, str);

			node->async_safe = true; /* literals are always true */
			fr_cursor_insert(&cursor, node);
			our_ctx = node;
			node = NULL;
			continue;
		}

		/*
		 *	We were told to look for a brace, but we ran off of
		 *	the end of the string before we found one.
		 */
		if (brace) {
			if (len == 0) TALLOC_FREE(node); /* Free the empty node */

			if (!fr_sbuff_is_char(in, '}')) {
				fr_strerror_printf("Missing closing brace");
				goto error;
			}
		/*
		 *	We're parsing the string *containing* the xlat
		 *	expansions.
		 */
		} else {
			/*	If we have an empty node, finish building it and
			 *	emit it.
			 *
			 *	We're about to return, and it's a useful
			 *	indication to the caller that this wasn't a parse
			 *	error but just an empty string.
			 */
			if (len == 0) {
				/*
				 *	This isn't the only node in the sequence
				 *	don't emit an empty trailing literal.
				 */
				if (*head) {
					talloc_free(node);
					break;
				}

				xlat_exp_set_type(node, XLAT_LITERAL);
				xlat_exp_set_name_buffer_shallow(node, str);

				XLAT_DEBUG("LITERAL <-- (empty)");
				node->async_safe = true; /* literals are always true */
				fr_cursor_insert(&cursor, node);
			}
		}
		break;
	}

	/*
	 *	Free our temporary array of terminals
	 */
	if (tokens != &expansions) talloc_free(tokens);

	return 0;
}

static fr_table_num_sorted_t const xlat_quote_table[] = {
	{ L("\""),	T_DOUBLE_QUOTED_STRING	},	/* Don't re-order, backslash throws off ordering */
	{ L("'"),	T_SINGLE_QUOTED_STRING	},
	{ L("`"),	T_BACK_QUOTED_STRING	}
};
static size_t xlat_quote_table_len = NUM_ELEMENTS(xlat_quote_table);

/** Tokenize an xlat expansion into a series of XLAT_TYPE_CHILD arguments
 *
 * @param[in] ctx	to allocate dynamic buffers in.
 * @param[out] head	the head of the xlat list / tree structure.
 * @param[in] in	the format string to expand.
 * @param[in] ar_rules	controlling how attribute references are parsed.
 * @return
 *	- <=0 on error.
 *	- >0  on success which is the number of characters parsed.
 */
ssize_t xlat_tokenize_argv(TALLOC_CTX *ctx, xlat_exp_t **head, fr_sbuff_t *in,
			   fr_sbuff_parse_rules_t const *p_rules, tmpl_rules_t const *ar_rules)
{
	fr_sbuff_t			our_in = FR_SBUFF_NO_ADVANCE(in);
	ssize_t				slen;
	TALLOC_CTX			*our_ctx = ctx;
	fr_cursor_t			cursor;
	fr_sbuff_marker_t		m;
	fr_sbuff_parse_rules_t const	*our_p_rules;	/* Bareword parse rules */

	*head = NULL;

	if (p_rules && p_rules->terminals) {
		our_p_rules = &(fr_sbuff_parse_rules_t){
			.terminals = fr_sbuff_terminals_amerge(NULL, p_rules->terminals,
							       tmpl_parse_rules_bareword_quoted.terminals),
			.escapes = tmpl_parse_rules_bareword_quoted.escapes
		};
	} else {
		our_p_rules = &tmpl_parse_rules_bareword_quoted;
	}

	/*
	 *	skip spaces at the beginning as we
	 *	don't want them to become a whitespace
	 *	literal.
	 */
	fr_sbuff_adv_past_whitespace(in, SIZE_MAX);
	fr_sbuff_marker(&m, &our_in);

	fr_cursor_init(&cursor, head);
	while (!FR_SBUFF_CANT_EXTEND(&our_in)) {
		xlat_exp_t	*node = NULL;
		fr_token_t	type;
		char		*fmt;
		size_t		len;

		fr_sbuff_set(&m, &our_in);	/* Record start of argument */

		fr_sbuff_out_by_longest_prefix(&slen, &type, xlat_quote_table, &our_in, T_BARE_WORD);

		/*
		 *	Alloc a new node to hold the child nodes
		 *	that make up the argument.
		 */
		node = xlat_exp_alloc_null(our_ctx);
		xlat_exp_set_type(node, XLAT_CHILD);

		switch (type) {
		/*
		 *	Barewords --may-contain=%{expansions}
		 */
		case T_BARE_WORD:
			if (xlat_tokenize_literal(node, &node->child, &our_in,
						  false, our_p_rules, ar_rules) < 0) {
			error:
				if (our_p_rules != &tmpl_parse_rules_bareword_quoted) {
					talloc_const_free(our_p_rules->terminals);
				}
				talloc_free(node);
				fr_cursor_free_list(&cursor);
				return -fr_sbuff_used(&our_in);	/* error */
			}
			break;

		/*
		 *	"Double quoted strings may contain %{expansions}"
		 */
		case T_DOUBLE_QUOTED_STRING:
			if (xlat_tokenize_literal(node, &node->child, &our_in,
						  false, &tmpl_parse_rules_double_quoted, ar_rules) < 0) goto error;
			break;

		/*
		 *	'Single quoted strings get parsed as literals'
		 */
		case T_SINGLE_QUOTED_STRING:
		{
			char		*str;

			node->child = xlat_exp_alloc_null(node);
			xlat_exp_set_type(node->child, XLAT_LITERAL);

			slen = fr_sbuff_out_aunescape_until(node->child, &str, &our_in, SIZE_MAX,
							    tmpl_parse_rules_single_quoted.terminals,
							    tmpl_parse_rules_single_quoted.escapes);
			if (slen < 0) goto error;
			xlat_exp_set_name_buffer_shallow(node->child, str);
		}
			break;

		/*
		 *	`back quoted strings aren't supported`
		 */
		case T_BACK_QUOTED_STRING:
			fr_strerror_printf("Unexpected `...` string");
			goto error;

		default:
			fr_assert(0);
			break;
		}

		if ((type != T_BARE_WORD) && !fr_sbuff_next_if_char(&our_in, fr_token_quote[type])) { /* Quoting */
			fr_strerror_printf("Unterminated string");
			fr_sbuff_set(&our_in, &m);
			goto error;
		}

		fmt = talloc_bstrndup(node, fr_sbuff_current(&m), fr_sbuff_behind(&m));
		xlat_exp_set_name_buffer_shallow(node, fmt);
		node->quote = type;
		node->async_safe = node->child->async_safe;

		fr_cursor_insert(&cursor, node);
		our_ctx = node;
		node = NULL;

		/*
		 *	If we're not and the end of the string
		 *	and there's no whitespace between tokens
		 *	then error.
		 */
		fr_sbuff_set(&m, &our_in);
		len = fr_sbuff_adv_past_whitespace(&our_in, SIZE_MAX);

		/*
		 *	Check to see if we have a terminal char
		 */
		if ((p_rules && p_rules->terminals) && fr_sbuff_is_terminal(&our_in, p_rules->terminals)) break;

		/*
		 *	Otherwise, if we can extend, and found
		 *	no additional whitespace, it means two
		 *	arguments were smushed together.
		 */
		if (!FR_SBUFF_CANT_EXTEND(&our_in) && (len == 0)) {
			fr_strerror_printf("Unexpected text after argument");
			goto error;
		}
	}

	if (our_p_rules != &tmpl_parse_rules_bareword_quoted) {
		talloc_const_free(our_p_rules->terminals);
	}

	return fr_sbuff_set(in, &our_in);
}

static void xlat_tokenize_debug(REQUEST *request, xlat_exp_t const *node)
{
	fr_assert(node != NULL);

	RINDENT();
	while (node) {
		switch (node->type) {
		case XLAT_LITERAL:
			RDEBUG3("literal --> %s", node->fmt);
			break;

		case XLAT_CHILD:
			RDEBUG3("child --> %s", node->fmt);
			RDEBUG3("{");
			RINDENT();
			xlat_tokenize_debug(request, node->child);
			REXDENT();
			RDEBUG3("}");
			break;

		case XLAT_ONE_LETTER:
			RDEBUG3("percent --> %c", node->fmt[0]);
			break;

		case XLAT_ATTRIBUTE:
			fr_assert(tmpl_da(node->attr) != NULL);
			RDEBUG3("attribute --> %s", tmpl_da(node->attr)->name);
			fr_assert(node->child == NULL);
			if ((tmpl_tag(node->attr) != TAG_ANY) || (tmpl_num(node->attr) != NUM_ANY)) {
				RDEBUG3("{");

				RINDENT();
				RDEBUG3("ref  %d", tmpl_request(node->attr));
				RDEBUG3("list %d", tmpl_list(node->attr));

				if (tmpl_tag(node->attr) != TAG_ANY) {
					RDEBUG3("tag %d", tmpl_tag(node->attr));
				}
				if (tmpl_num(node->attr) != NUM_ANY) {
					if (tmpl_num(node->attr) == NUM_COUNT) {
						RDEBUG3("[#]");
					} else if (tmpl_num(node->attr) == NUM_ALL) {
						RDEBUG3("[*]");
					} else {
						RDEBUG3("[%d]", tmpl_num(node->attr));
					}
				}
				REXDENT();
				RDEBUG3("}");
			}
			break;

		case XLAT_VIRTUAL:
			fr_assert(node->fmt != NULL);
			RDEBUG3("virtual --> %s", node->fmt);
			break;

		case XLAT_FUNC:
			fr_assert(node->xlat != NULL);
			RDEBUG3("xlat --> %s", node->xlat->name);
			if (node->child) {
				RDEBUG3("{");
				xlat_tokenize_debug(request, node->child);
				RDEBUG3("}");
			}
			break;

#ifdef HAVE_REGEX
		case XLAT_REGEX:
			RDEBUG3("regex-var --> %d", node->regex_index);
			break;
#endif

		case XLAT_ALTERNATE:
			DEBUG("XLAT-IF {");
			xlat_tokenize_debug(request, node->child);
			DEBUG("}");
			DEBUG("XLAT-ELSE {");
			xlat_tokenize_debug(request, node->alternate);
			DEBUG("}");
			break;
		}
		node = node->next;
	}
	REXDENT();
}

ssize_t xlat_print(fr_sbuff_t *out, xlat_exp_t const *node)
{
	ssize_t			slen;
	size_t			at_in = fr_sbuff_used_total(out);

	if (!node) return 0;

	while (node) {
		switch (node->type) {
		case XLAT_LITERAL:
		case XLAT_CHILD:
			FR_SBUFF_IN_BSTRCPY_BUFFER_RETURN(out, node->fmt);
			goto next;

		case XLAT_ONE_LETTER:
			FR_SBUFF_IN_CHAR_RETURN(out, '%', node->fmt[0]);
			goto next;

		default:
			break;
		}

		FR_SBUFF_IN_STRCPY_LITERAL_RETURN(out, "%{");
		switch (node->type) {
		case XLAT_ATTRIBUTE:
			slen = tmpl_print_attr_str(out, node->attr);
			if (slen < 0) {
			error:
				return slen;
			}
			break;
#ifdef HAVE_REGEX
		case XLAT_REGEX:
			FR_SBUFF_IN_SPRINTF_RETURN(out, "%i", node->regex_index);
			break;
#endif
		case XLAT_VIRTUAL:
			FR_SBUFF_IN_BSTRCPY_BUFFER_RETURN(out, node->fmt);
			break;

		case XLAT_FUNC:
			FR_SBUFF_IN_BSTRCPY_BUFFER_RETURN(out, node->xlat->name);
			FR_SBUFF_IN_CHAR_RETURN(out, ':');

			if (node->child) {
				slen = xlat_print(out, node->child);
				if (slen < 0) goto error;
			}
			break;

		case XLAT_ALTERNATE:
			slen = xlat_print(out, node->child);
			if (slen < 0) goto error;

			FR_SBUFF_IN_STRCPY_LITERAL_RETURN(out, ":-");
			slen = xlat_print(out, node->alternate);
			if (slen < 0) goto error;
			break;

		default:
			fr_assert_fail(NULL);
			break;
		}
		FR_SBUFF_IN_CHAR_RETURN(out, '}');
	next:
		node = node->next;
	}

	return fr_sbuff_used(out) - at_in;
}

/** Tokenize an xlat expansion at runtime
 *
 * This is used for runtime parsing of xlat expansions, such as those we receive from datastores
 * like LDAP or SQL.
 *
 * @param[in] ctx	to allocate dynamic buffers in.
 * @param[out] head	the head of the xlat list / tree structure.
 * @param[in] request	the input request.  Memory will be attached here.
 * @param[in] in	the format string to expand.
 * @param[in] p_rules	from the encompassing grammar.
 * @param[in] ar_rules	controlling how attribute references are parsed.
 * @return
 *	- >0 on success.
 *	- 0 and *head == NULL - Parse failure on first char.
 *	- 0 and *head != NULL - Zero length expansion
 *	- <0 the negative offset of the parse failure.
 */
ssize_t xlat_tokenize_ephemeral(TALLOC_CTX *ctx, xlat_exp_t **head, REQUEST *request,
			        fr_sbuff_t *in,
			        fr_sbuff_parse_rules_t const *p_rules, tmpl_rules_t const *ar_rules)
{
	fr_sbuff_t	our_in = FR_SBUFF_NO_ADVANCE(in);

	*head = NULL;

	fr_strerror();	/* Clear error buffer */
	if (xlat_tokenize_literal(request, head, &our_in, false, p_rules, ar_rules) < 0) return -fr_sbuff_used(&our_in);

	/*
	 *	Zero length expansion, return a zero length node.
	 */
	if (fr_sbuff_used(&our_in) == 0) {
		MEM(*head = talloc_zero(ctx, xlat_exp_t));
		(*head)->async_safe = true;
	}

	if (RDEBUG_ENABLED3) {
		RDEBUG3("%pV", fr_box_strvalue_len(fr_sbuff_start(&our_in), fr_sbuff_used(&our_in)));
		RDEBUG3("Parsed xlat tree:");
		xlat_tokenize_debug(request, *head);
	}

	/*
	 *	Create ephemeral instance data for the xlat
	 */
	if (xlat_instantiate_ephemeral(*head) < 0) {
		fr_strerror_printf("Failed performing ephemeral instantiation for xlat");
		TALLOC_FREE(*head);
		return 0;
	}

	return fr_sbuff_set(in, &our_in);
}

/** Tokenize an xlat expansion
 *
 * @param[in] ctx	to allocate dynamic buffers in.
 * @param[out] head	the head of the xlat list / tree structure.
 * @param[in] in	the format string to expand.
 * @param[in] p_rules	controlling how the string containing the xlat
 *			expansions should be parsed.
 * @param[in] ar_rules	controlling how attribute references are parsed.
 * @return
 *	- >0 on success.
 *	- 0 and *head == NULL - Parse failure on first char.
 *	- 0 and *head != NULL - Zero length expansion
 *	- < 0 the negative offset of the parse failure.
 */
ssize_t xlat_tokenize(TALLOC_CTX *ctx, xlat_exp_t **head, fr_sbuff_t *in,
		      fr_sbuff_parse_rules_t const *p_rules, tmpl_rules_t const *ar_rules)
{
	fr_sbuff_t	our_in = FR_SBUFF_NO_ADVANCE(in);

	*head = NULL;

	fr_strerror();	/* Clear error buffer */

	if (xlat_tokenize_literal(ctx, head, &our_in, false, p_rules, ar_rules) < 0) return -fr_sbuff_used(&our_in);

	/*
	 *	Add nodes that need to be bootstrapped to
	 *	the registry.
	 */
	if (xlat_bootstrap(*head) < 0) {
		TALLOC_FREE(*head);
		return 0;
	}

	return fr_sbuff_set(in, &our_in);
}

/** Check to see if the expansion consists entirely of literal elements
 *
 * @param[in] head	to check.
 * @return
 *	- true if expansion contains only literal elements.
 *	- false if expansion contains expandable elements.
 */
bool xlat_is_literal(xlat_exp_t const *head)
{
	xlat_exp_t const *node;

	for (node = head;
	     node;
	     node = node->next) {
		if (node->type != XLAT_LITERAL) return false;
	}

	return true;
}

/** Convert an xlat node to an unescaped literal string and free the original node
 *
 * @param[in] ctx	to allocate the new string in.
 * @param[out] str	a duplicate of the node's fmt string.
 * @param[in,out] head	to convert.
 * @return
 *	- true	the tree consists of a single literal node which was converted.
 *      - false the tree was more complex than a single literal, op was a noop.
 */
bool xlat_to_literal(TALLOC_CTX *ctx, char **str, xlat_exp_t **head)
{
	if (!*head) return false;

	if (!xlat_is_literal(*head)) return false;

	*str = talloc_bstrdup(ctx, (*head)->fmt);
	TALLOC_FREE(*head);

	return true;
}

