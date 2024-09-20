-module(authdb).

-export([create/0, add_user/4, authenticate/3, authorize/3]).

-import(userdef, [user_create/3, password_is/2, has_permission/2]).
-import(crypto, [strong_rand_bytes/1]).
-import(base64, [encode/1]).
-import(dict, [store/3]).

-record(authEntry, {user, validUntilEpoch}).
-record(authDb, {tokenDb, userDb}).

%% -----------------------------------------------------------------------------

create() -> #authDb{tokenDb=dict:new(), userDb=dict:new()}.

add_user(Uname, Password, Permissions, AuthDb) ->
    User = user_create(Uname, Password, Permissions),
    TokenDb = AuthDb#authDb.tokenDb,
    UserDb = AuthDb#authDb.userDb,
    #authDb{tokenDb=TokenDb, userDb=dict:store(Uname, User, UserDb)}.

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                 AUTHENTICATE                                 %
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generate_token() ->
    base64:encode(crypto:strong_rand_bytes(16)).

generate_token(AuthDb) ->
    Token = generate_token(),
    case dict:find(Token, AuthDb) of
         error -> Token;
         {ok, _} -> generate_token(AuthDb)
    end.

now_epoch() ->
    {Megasecs, Secs, _} = erlang:timestamp(),
    Megasecs * 1000000 + Secs.

%% Invalidation Period 1min
get_invalidation_time() ->
    now_epoch() + 60.

token_for_user(User, AuthDb) ->
    TokenDb = AuthDb#authDb.tokenDb,
    Token = generate_token(TokenDb),
    InvalidationTime=get_invalidation_time(),
    Entry = #authEntry{user=User, validUntilEpoch=InvalidationTime},
    TokenDbUpdated =dict:store(Token, Entry, TokenDb),
    AuthDbUpdated = #authDb{tokenDb=TokenDbUpdated, userDb=AuthDb#authDb.userDb},
    {Token, AuthDbUpdated}.

authenticate_user(User, Password, AuthDb) ->
    case password_is(Password, User) of
        ok -> token_for_user(User, AuthDb);
        error -> {none, AuthDb}
    end.


authenticate(Uname, Password, AuthDb) ->
    case dict:find(Uname, AuthDb#authDb.userDb) of
        error -> {none, AuthDb};
        {ok, User} -> authenticate_user(User, Password, AuthDb)
    end.

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                  AUTHORIZE                                   %
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_entry_valid(Entry) ->
    Now = now_epoch(),
    if Entry#authEntry.validUntilEpoch >= Now ->
           ok;
       true -> error
    end.

check_permission_entry(Entry, Permission) ->
    EntryValid = is_entry_valid(Entry),
    case EntryValid of
        ok -> has_permission(Permission, Entry#authEntry.user);
        error -> invalid
    end.

authorize(Token, Permission, Db) ->
    TokenDb = Db#authDb.tokenDb,
    case dict:find(Token, TokenDb) of
        error -> invalid;
        {ok, Entry} -> check_permission_entry(Entry, Permission)
    end.

