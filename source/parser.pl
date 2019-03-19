:- set_prolog_flag(double_quotes, codes).


% oneOf_(-Char, +String).
oneOf_(Char, String) -->
	[ Char ],
	{ member(Char, String) }.

% oneOrMore(-[Fst|Rst], +Template).
oneOrMore([Fst | Rst], Template) -->
	{ copy_term(Template, Fst^ Goal) },
	phrase(Goal),
	many(Rst, Template).

% many(-List, +Template).
many(List, Template) -->
	oneOrMore(List, Template) ; ( [], { List = [] } ).


% upper(-Char).
upper(Char) -->
	oneOf_(Char, "ABCDEFGHIJKLMNOPQRSTUVWXYZ").

% lower(-Char).
lower(Char) -->
	oneOf_(Char, "abcdefghijklmnopqrstuvwxyz").

% digit(-Digit).
digit(Digit) -->
	oneOf_(Digit, "0123456789").

% intLiteral(-Integer).
intLiteral(Integer) -->
	digit(Fst),
	many(Rst, Char ^ (digit(Char) ; char(Char, "_"))),
	{
		name(Integer, [Fst | Rst])
	}.

% floatLiteral(-Float).
floatLiteral(Float) -->
	digit(Fst),
	many(Rst, Char ^digit(Char)),
	".",
	digit(Fst1),
	many(Rst1, Char ^digit(Char)),

	{
		Whole = [Fst | Rst],
		Fractional = [Fst1 | Rst1],
		append(Whole, ".", A),
		append(A, Fractional, B),
		name(Float, B)
	}.


% operator(-Operator).
operator(Operator) -->
	oneOrMore(Chars, Char ^ oneOf_(Char, "-+=!$%^&*.<>:~/\\@#~;,")),
	{
		name(Operator, Chars)
	}.


char(Chr, Str) --> [Chr], { [Chr] = Str }.
whiteSpace --> oneOrMore(_, (Char^oneOf_(Char, " \t\n"))).
maybeSpace --> whiteSpace ; [].
maybeScore --> char(_, "_") ; [].
reserved(Str) -->
	maybeSpace,
	phrase(Str),
	maybeSpace.

end(List) --> { List = [] }.
maybe(Goal) --> phrase(Goal) ; [].


bracketed(Goal) -->
	reserved("("),
		phrase(Goal),
	reserved(")").

s_name(Name) --> s_name_(Chars), { name(Name, Chars) }.
s_name_([Fst | Rst]) -->
	( upper(Fst) ; lower(Fst) ; char(Fst, "_") ),
	many(Rst, Char ^(
		upper(Char) ; lower(Char) ; digit(Char) ; char(Char, "_")
	)).

s_comma_args([Fst | Rst]) -->
	s_item(Fst),
	(
		reserved(","),
		s_comma_args(Rst)
	;
		end(Rst)
	).


%s_list(empty(list)) --> reserved("[]").
s_list([]) --> reserved("[]").
s_list(Args) -->
	reserved("["),
		s_comma_args(Args),
	reserved("]")
	.


s_list(comprehension(Expr, Name, Source)) -->
	reserved("["),
			s_arguments(Expr),

		reserved("|"),

			s_name(Name),

		reserved("<-"),
			(
				s_list(Source)
			;
				s_expression(Source)
			;
				s_name(Source)
			),
	reserved("]").


%s_tuple(empty(tuple)) --> reserved("{}").
s_tuple({}) --> reserved("{}").
s_tuple(Tuple) -->
	reserved("{"),
		s_comma_args(Args),
	reserved("}"),
	{
		Tuple =.. [tuple | Args]
	}.


s_array(Array) -->
	reserved("[|"),
		s_comma_args(Args),
	reserved("|]"),
	{
		Array =.. [array | Args]
	}.


s_lambda([lambda, Args, Body]) -->
	bracketed((
		reserved("\\"),
			s_arguments(Args),
		reserved("->"),
			s_arguments(Body)
	)).


s_item(Item) -->
	(
		floatLiteral(Item)
	;
		operator(Item)
	;
		s_name(Item)
	;
		intLiteral(Item)
	;
		s_lambda(Item)
	;
		s_expression(Item)
	;
		s_list(Item)
	;
		s_tuple(Item)
	;
		s_array(Item)
	).

% s_arguments(-[Fst|Rst]).
s_arguments([Fst | Rst]) -->
	s_item(Fst),
	(
		maybeSpace,
		s_arguments(Rst)
	;
		end(Rst)
	).

% s_expression(-s_expression(Functor, List)).
s_expression(s_expression(Functor, List)) -->
	bracketed( s_arguments([Functor | List]) ).
