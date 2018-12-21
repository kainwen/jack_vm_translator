Definitions.


Rules.

%%
%% Comments
//.*\n                   : {token, {comment}}.

%% Tokens for memory segment commands
push                     : {token, push}.
pop                      : {token, pop}.

local                    : {token, {segment, local}}.
argument                 : {token, {segment, argument}}.
static                   : {token, {segment, static}}.
constant                 : {token, {segment, constant}}.
this                     : {token, {segment, this}}.
that                     : {token, {segment, that}}.
pointer                  : {token, {segment, pointer}}.
temp                     : {token, {segment, temp}}.

%% Tokens for math, logic, and compare commands
add                      : {token, {math, add}}.
sub                      : {token, {math, sub}}.
neg                      : {token, {math, neg}}.

gt                       : {token, {cmp, gt}}.
eq                       : {token, {cmp, eq}}.
lt                       : {token, {cmp, lt}}.

and                      : {token, {logic, 'and'}}.
or                       : {token, {logic, 'or'}}.
not                      : {token, {logic, 'not'}}.

%% function
function                 : {token, define}.
call                     : {token, call}.
return                   : {token, return}.

%% label
label                    : {token, def_label}.
goto                     : {token, goto}.
if-goto                  : {token, ifgoto}.

%% The label is an arbitrary string composed of any sequence of letters, digits, underscore (_), dot (.), and colon (:) that does not begin with a digit.
[_.:a-zA-Z][_.:a-zA-Z0-9]* : {token, {label, list_to_atom(TokenChars)}}.


%% Positive Integer
[0-9]+                   : {token, list_to_integer(TokenChars)}.

%% Space to ignore
\t                       : skip_token.
\n                       : skip_token.
\r                       : skip_token.
\s                       : skip_token.

Erlang code.
