-module(my_jwt).
-export([generate_jwk/1, load_jwk/0]).

-define(JWK_FILE, "/kchat_server/jwk.json").

generate_jwk(UserID) ->
  Payload = #{<<"id">> => UserID},
  {_, JWT} = jose_jwt:sign(load_jwk(), #{<<"alg">> => <<"RS256">>}, Payload),
  {_, CompactJwt} = jose_jws:compact(JWT),
  CompactJwt.

load_jwk() ->
  case file:read_file(?JWK_FILE) of
    {ok, _} ->
      jose_jwk:from_file(?JWK_FILE);
    {error, _} ->
      JWK = jose_jwk:generate_key({rsa, 2048}),
      save_jwk(JWK),
      JWK
  end.

save_jwk(JWK) ->
  jose_jwk:to_file(?JWK_FILE, JWK).
