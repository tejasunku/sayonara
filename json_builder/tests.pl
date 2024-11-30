:- module(tests, [
    test_trim/0,
    test_split/0,
    test_split_json/0,
    test_valid_json/0
]).

:- use_package(assertions).
:- use_module('json_builder/utils').

% Custom assertion for testing
assertion(Goal) :- Goal -> true ; throw(assertion_failed(Goal)).

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
    assertion(X == [hello, world]).

test_split_extra_whitespace :-
    split_by_whitespace('  hello   world  ', X),
    assertion(X == [hello, world]).

test_split_mixed_whitespace :-
    split_by_whitespace('\thello\nworld\r', X),
    assertion(X == [hello, world]).

test_split_single_word :-
    split_by_whitespace('single', X),
    assertion(X == [single]).

test_split_empty_string :-
    split_by_whitespace('', X),
    assertion(X == []).

test_split_multiple_words :-
    split_by_whitespace('one two three four', X),
    assertion(X == [one, two, three, four]).

test_split_mixed_content :-
    split_by_whitespace('hello123 world456', X),
    assertion(X == [hello123, world456]).

% Tests for JSON splitting
test_split_json :-
    test_split_json_basic,
    test_split_json_nested,
    test_split_json_array,
    test_split_json_mixed,
    write('All JSON split tests passed!'), nl.

test_split_json_basic :-
    split_by_json_chars('{hello}', X),
    assertion(X == ['{', hello, '}']).

test_split_json_nested :-
    split_by_json_chars('{{}}', X),
    assertion(X == ['{', '{', '}', '}']).

test_split_json_array :-
    split_by_json_chars('[1,2,3]', X),
    assertion(X == ['[', '1', ',', '2', ',', '3', ']']).

test_split_json_mixed :-
    split_by_json_chars('{key:value}', X),
    assertion(X == ['{', key, ':', value, '}']).

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
    assertion(is_valid_json('{}')),
    assertion(is_valid_json('[]')),
    assertion(is_valid_json('"test"')).

test_valid_json_object :-
    assertion(is_valid_json('{"name":"John"}')),
    assertion(is_valid_json('{}')).

test_valid_json_array :-
    assertion(is_valid_json('[1,2,3]')),
    assertion(is_valid_json('[]')).

test_valid_json_nested :-
    assertion(is_valid_json('{"user":{"name":"John","age":30}}')),
    assertion(is_valid_json('{"items":[1,2,3]}')).

test_invalid_json_incomplete :-
    assertion(\+ is_valid_json('{"name":')),
    assertion(\+ is_valid_json('[1,2,')).

test_invalid_json_malformed :-
    assertion(\+ is_valid_json('{name}')),
    assertion(\+ is_valid_json('[1,2]]')).
