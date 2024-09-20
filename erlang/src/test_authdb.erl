-module(test_authdb).

-export([start/0]).

-import (authdb, [create/0, add_user/4, authenticate/3, authorize/3]).

start() ->

    Adb = create(),
    Adb2 = add_user("michael", "trolladynga", ["1.2.3", "4", "17.3.1"], Adb),
    Adb3 = add_user("ramses", "trolladynga", ["1", "4", "31.1"], Adb2),
    {none, Adb3} = authenticate("horst", "brakk", Adb3),
    {none, Adb3} = authenticate("michael", "brakk", Adb3),
    {Token, Adb4} = authenticate("michael", "trolladynga", Adb3),
    false = authorize(Token, "1.2.4", Adb4),
    true = authorize(Token, "1.2.3", Adb4).
