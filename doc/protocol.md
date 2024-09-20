# General

## Structure

The protocol is a simple request-response protocol.

Everything starts with a request, answered with a response.

A request can be anything, a response is basically either a

'ok' or 'error' followed by some optional parameters.

## Semantics

The protocol works in two steps:

1. Authenticate a user. If successful
2. Authorize a user for a specific permission

A typical session would look like

```

; authenticate user 'herrmann'

> authenticate herrmann plain Herrmanns's wrong password
failed
> authenticate herrmann plain Herrmanns's password
token abc43C21hF
> authorize abc43C21hF 2.1.13.2
r:ok
> authorize abc43C21hF 2.1.13.3
r:ok
> authorize abc43C21hF 1.1.1.1
r:error denied
> authorize abc43C21hF 2.1.13.2
r:ok
> authorize abc43C21hF 2.1.13.2
r:error token expired
> authenticate herrmann plain Herrmanns's password
r:ok token F31chAk2
```

## Authentication

Authentication means checking whether the user is actually the user he
proclaims to be.
This is currently done by checking his password.

This yields either
`r:error [MSG]` if authentication failed or
`r:ok token TOKEN` if successful. TOKEN is a random string which can be used to
                             check for authorization.

The format for an authentication request is

```
authenticate USER AUTH_METHOD [PARAMETERS ...]
```

Currently the only supported AUTH_METHOD is 'plain', meaning plain-text
passwords. The only parameter is the (plain-text) password.

## Tokens

Tokens are a kind of caching of the result of a successful authentication.
They comprise random strings, which the server associates with a particular
user.
That means whenever a token is transmitted to the server, it is either
found to belong to a particular user, or is invalid.

A token has a limited life span.
After expiration, the server deletes the token and the token will be invalid
from then on.

## Authorization

Authorization is the process of checking whether a user has access to a
particular resource.
The resource is represented by a string.

To check whether a user has permission to access a particular resource 'res.ource.string',
one has to first authenticate as this user, gain a valid token, and then check
whether the token has access permissions:

```
authorize TOKEN res.ource.string
```

Possible results are

`r:ok` if authorization was successful and the user might access the resource
`r:error [MSG]` if authorization was unsuccessful and the user should not access the
         resource.

## Resource strings

A resource string resembles a hierarchical structure:

'1' is a resource, '1.2' is resource '2' within resource '1'.
'2.1' by contrast is resource '1' inside of resource '2'.

If a user is allowed to access resource '1', he is automatically allowed to
access all resources within, e.g. '1.1', '1.2', '1.17.9.34' etc.

How these strings are interpreted is up the application using waechter.

It might resemble access permissions to a file system, like

`webserver.public.audio` might resemble all files on a webserver underneath `public/audio`,
or
`read.public.audio` might resemble read permission on the directory `/public/audio`,
or
`writeread.public.audio` might resemble read and write permissions on the directory `/public/audio`.


# Network protocol

The waechter daemon provides a socket for the clients to connect to, either
TCP or via UNIX local sockets/pipes.

Anyhow, one request or response is encoded as one single line of ASCII text
starting with a 32 bit unsigned integer, transmitted in ASCII base 10,
followed by a space, data and terminated by a line-feed ( [LF], '\n' in C).

all keywords are small letters.
Words in a request are separated by spaces.
The client should only send one space as separator, the server should accept
several spaces however, collapsing several consecutive spaces into a single one
and dropping any trailing spaces:

```
authenticate     franz       plain       not-very-secure     [LF]
```

should be treated as

```
authenticate franz plain not-very-secure[LF]
```

## Responses from the server

The server generally responses with a single line of ASCII text, terminated
by a line-feed ( [LF] ).


Each response starts with the repetition of the random 32 bit integer of the
request line the response belongs to, followed by a space, an 'r:' followed by
either

`ok` or `error`.

More words might follow.

E.g.

```
21341 r:ok[LF]
```

signals that the authorization request was successful.

```
45111 r:error[LF]
```

or

```
11111 r:error Permission denied[LF]
```

or

```
65222 r:error XXXXX[LF]
```

etc

denotes a failed authorization request.

In general, the type of answer can always be determined by the very first word
in the line.
Additional words only carry some specifications like additional information
why something happened, or the actual token required for authorization requests.
However, e.g. whether or not an authentication request was successful,
can be determined by just checking whether the first word in the response is
'token' or not.

## Authentication request

The general format is

```
12341 authenticate USER METHOD PARAMETER[LF]
```

Currently only 'plain' is supported as METHOD, with the PARAMETER being the
actual password. The password must not contain spaces.

Response Is either a standard

```
12341 r:error[LF]
```

or

```
12341 r:ok token TOKEN[LF]
```

## Authorization request

The general format is

```
12342 authorize TOKEN RESOURCE-STRING
```

Response is either

```
12342 r:ok [POSSIBLE FURTHER INFO]
```

or

```
12342 r:error [POSSIBLE FURTHER INFO]
```
