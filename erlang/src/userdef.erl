-module(userdef).

-export([user_create/3,
         password_is/2,
         permissions/1,
         has_permission/2]).

-record(user, {name, password, permissions=[]}).

%% -----------------------------------------------------------------------------

user_create(User, Password, Permissions) ->
    #user{name=User, password=Password, permissions=split_permissions(Permissions)}.

%%-----------------------------------------------------------------------------

password_is(P1, User) when P1 == User#user.password -> ok;
password_is(_, _) -> error.

permissions(U) ->
    U#user.permissions.

% -----------------------------------------------------------------------------

split_permission(Permission) ->
    string:tokens(Permission, ".").

split_permissions(Permissions) ->
    lists:map( fun(X) -> string:tokens(X, ".") end, Permissions).

permission_includes([], []) -> true;
permission_includes(_, []) -> false;
permission_includes([], _) -> true;
permission_includes([P|Permission], [O|OtherPermission]) ->
    if
        P == O -> permission_includes(Permission, OtherPermission);
        true -> false
    end.

permission_is_contained([], _) -> false;
permission_is_contained([P|PermissionNode], Permission) ->
    PermIncluded = permission_includes(P, Permission),
    if PermIncluded -> true;
       true -> permission_is_contained(PermissionNode, Permission)
    end;
permission_is_contained(PermissionNode, Permission) ->
    permission_includes(PermissionNode, Permission).

find_permission(Permissions, Permission) ->
    permission_is_contained(Permissions, split_permission(Permission)).

has_permission(Permission, User) ->
    find_permission(User#user.permissions, Permission).


