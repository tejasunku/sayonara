:- module(utils, [
    trim_whitespace/2,
    split_by_whitespace/2,
    split_by_json_chars/2,
    is_valid_json/1
]).

:- use_package(assertions).
:- use_package(dcg).
:- use_module(library(lists)).
:- use_module(engine(hiord_rt)).  % For call/2
:- use_module(library(pillow/json)).      % For string_to_json/2
:- use_module(library(strings)).

% Custom maplist implementation
maplist(_, [], []).
maplist(Pred, [X|Xs], [Y|Ys]) :-
    call(Pred, X, Y),
    maplist(Pred, Xs, Ys).

% Trim whitespace from start and end of atom
trim_whitespace(Input, Output) :-
    atom(Input),
    atom_chars(Input, Chars),
    phrase(trimmed(TrimmedChars), Chars),
    atom_chars(Output, TrimmedChars).

% Grammar for trimming whitespace
trimmed(Chars) --> wspaces, nonws_chars(Chars), wspaces.

% Collect non-whitespace characters with any internal whitespace
nonws_chars([]) --> [].
nonws_chars([C|Cs]) --> [C], { \+ member(C, [' ', '\t', '\n', '\r']) }, !, nonws_chars(Cs).
nonws_chars([C|Cs]) --> [C], nonws_chars(Cs).

% Split atom by whitespace into list of atoms
split_by_whitespace(Input, Words) :-
    atom(Input),
    atom_chars(Input, Chars),
    phrase(split_words(WordChars), Chars),
    maplist(atom_chars, Words, WordChars).

% Grammar for splitting into words
split_words([]) --> wspaces.
split_words([Word|RestWords]) --> 
    wspaces, 
    word(Word), 
    split_words(RestWords).

% Collect a single word (non-whitespace characters)
word([C|Cs]) --> [C], { \+ member(C, [' ', '\t', '\n', '\r']) }, !, word(Cs).
word([]) --> [].

% Whitespace (space, tab, newline, carriage return)
wspace --> [W], { member(W, [' ', '\t', '\n', '\r']) }.
wspaces --> wspace, wspaces | [].

% Split atom by JSON control characters
split_by_json_chars(Input, Tokens) :-
    atom(Input),
    atom_chars(Input, Chars),
    phrase(json_tokens(TokenChars), Chars),
    maplist(atom_chars, Tokens, TokenChars).

% Grammar for splitting into JSON tokens
json_tokens([]) --> [].
json_tokens([Token|Tokens]) --> 
    token(Token),
    json_tokens(Tokens).

% Single token can be either a control character or a sequence of non-control characters
token([C]) --> 
    [C], 
    { member(C, ['{', '}', '[', ']', ':', ',']) }, 
    !.
token(Chars) --> 
    non_control_chars(Chars).

% Collect non-control characters
non_control_chars([C|Cs]) --> 
    [C], 
    { \+ member(C, ['{', '}', '[', ']', ':', ',']) },
    !,
    non_control_chars(Cs).
non_control_chars([]) --> [].

% Check if string is valid JSON
is_valid_json(String) :-
    catch(string_to_json(String, _Term), _Error, fail).
