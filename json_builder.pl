:- module(json_builder, []).

:- use_module(library(json_builder/utils)).
:- use_module(library(json_builder/tests)).
:- use_package(assertions).
:- use_package(dcg).
:- use_module(library(lists)).
:- use_module(engine(hiord_rt)).  % For call/2
:- use_module(library(pillow/json)).      % For string_to_json/2
:- use_module(library(strings)).


% Custom assertion for testing
assertion(Goal) :- Goal -> true ; throw(assertion_failed(Goal)).

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

% Tests for trim_whitespace
test_trim :-
    test_trim_basic,
    test_trim_mixed_whitespace,
    test_trim_internal_whitespace,
    test_trim_no_whitespace,
    test_trim_empty_string,
    test_trim_only_whitespace,
    write('All trim tests passed!'), nl.

test_trim_basic :-
    trim_whitespace('  hello  ', X),
    assertion(X == 'hello').

test_trim_mixed_whitespace :-
    trim_whitespace('\n\t hello \t\r\n', X),
    assertion(X == 'hello').

test_trim_internal_whitespace :-
    trim_whitespace('  hello  world  ', X),
    assertion(X == 'hello  world').

test_trim_no_whitespace :-
    trim_whitespace('hello', X),
    assertion(X == 'hello').

test_trim_empty_string :-
    trim_whitespace('', X),
    assertion(X == '').

test_trim_only_whitespace :-
    trim_whitespace('   ', X),
    assertion(X == '').

% Tests for split_by_whitespace
test_split :-
    test_split_basic,
    test_split_extra_whitespace,
    test_split_mixed_whitespace,
    test_split_single_word,
    test_split_empty_string,
    test_split_multiple_words,
    test_split_mixed_content,
    write('All split tests passed!'), nl.

test_split_basic :-
    split_by_whitespace('hello world', X),
    assertion(X == ["hello", "world"]).

test_split_extra_whitespace :-
    split_by_whitespace('  hello   world  ', X),
    assertion(X == ["hello", "world"]).

test_split_mixed_whitespace :-
    split_by_whitespace('\thello\nworld\r', X),
    assertion(X == ["hello", "world"]).

test_split_single_word :-
    split_by_whitespace('single', X),
    assertion(X == ["single"]).

test_split_empty_string :-
    split_by_whitespace('', X),
    assertion(X == []).

test_split_multiple_words :-
    split_by_whitespace('one two three four', X),
    assertion(X == ["one", "two", "three", "four"]).

test_split_mixed_content :-
    split_by_whitespace('hello123 world456', X),
    assertion(X == ["hello123", "world456"]).

% Tests for JSON splitting
test_split_json :-
    test_split_json_basic,
    test_split_json_nested,
    test_split_json_array,
    test_split_json_mixed,
    write('All JSON split tests passed!'), nl.

test_split_json_basic :-
    split_by_json_chars('{hello}', X),
    assertion(X == ['{', "hello", '}']).

test_split_json_nested :-
    split_by_json_chars('{{}}', X),
    assertion(X == ['{', '{', '}', '}']).

test_split_json_array :-
    split_by_json_chars('[1,2,3]', X),
    assertion(X == ['[', '1', ',', '2', ',', '3', ']']).

test_split_json_mixed :-
    split_by_json_chars('{key:value}', X),
    assertion(X == ['{', "key", ':', "value", '}']).

% Tests for JSON validation
test_valid_json :-
    test_valid_json_simple,
    test_valid_json_object,
    test_valid_json_array,
    test_valid_json_nested,
    test_invalid_json_incomplete,
    test_invalid_json_malformed,
    write('All JSON validation tests passed!'), nl.

test_valid_json_simple :-
    assertion(is_valid_json("{}")),
    assertion(is_valid_json("[]")),
    assertion(is_valid_json("\"test\"")).

test_valid_json_object :-
    assertion(is_valid_json("{\"name\":\"John\"}")),
    assertion(is_valid_json("{}")).

test_valid_json_array :-
    assertion(is_valid_json("[1,2,3]")),
    assertion(is_valid_json("[]")).

test_valid_json_nested :-
    assertion(is_valid_json("{\"user\":{\"name\":\"John\",\"age\":30}}")),
    assertion(is_valid_json("{\"items\":[1,2,3]}")).

test_invalid_json_incomplete :-
    assertion(\+ is_valid_json("{\"name\":")),
    assertion(\+ is_valid_json("[1,2,")).

test_invalid_json_malformed :-
    assertion(\+ is_valid_json("{name}")),
    assertion(\+ is_valid_json("[1,2]]")).