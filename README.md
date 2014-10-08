About Plaster
-------------
Plaster is a pasting service for [Radiance](https://github.com/Shinmera/Radiance). It uses [Code Mirror](http://codemirror.net/) for its editor and thus supports highlighting and editing features for a plethora of languages. It also offers paste annotations, encrypting and protecting pastes, as well as anonymous pasting.

How To
------
Set up Radiance and load plaster through quicklisp or ASDF. Plaster occupies the subdomain `plaster`.

Interface Dependencies
----------------------
* database
* data-model
* user
* auth
* profile

Configuration Variables
-----------------------
* `(:plaster :maxpastes)`  
Maximum pastes allowed per user. Defaults to NIL (infinite).
* `(:plaster :cooldown)`  
Amount of seconds required between pastes. Defaults to NIL (none).
* `(:plaster :anon)`  
Whether anonymous pasting is allowed or not. Defaults to T.
* `(:plaster :captcha)`  
Whether to use a captcha for anonymous pastes. Defaults to T.
* `(:plaster :encrypt-salt)`  
The salt to use for encrypting pastes. Defaults to a random string.

Permissions
-----------
* `(plaster admin)`
* `(plaster preferences)`
