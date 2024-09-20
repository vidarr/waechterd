-module(test_userdef).

-export([start/0]).

-import (userdef,
         [user_create/3,
          password_is/2,
          permissions/1,
          has_permission/2]).

start() ->

    U1 = user_create("Michael", "trolladynga", ["1.2.3", "2", "4.1"]),
    U2 = user_create("Fackelmann", "not a password", ["1.2", "2.1.1", "4.1.2"]),

    ok = password_is("trolladynga", U1),
    error = password_is("trolladynga", U2),
    error = password_is("trolla", U1),

    ok = password_is("not a password", U2),
    error = password_is("trolla", U2),
    error = password_is("not a password", U1),
    error = password_is("trolladynga", "beta"),

    false = has_permission("1", U1),
    false = has_permission("1.2", U1),
    true = has_permission("1.2.3", U1),
    true = has_permission("1.2.3.1", U1),
    true = has_permission("1.2.3.2", U1),

    true = has_permission("2", U1),
    true = has_permission("2.1", U1),
    true = has_permission("2.1.21.3", U1),

    false = has_permission("3", U1),
    false = has_permission("3.1.5.3", U1),

    false = has_permission("4", U1),
    true = has_permission("4.1", U1),
    false = has_permission("4.2", U1),
    true = has_permission("4.1.4.3.1111", U1).
