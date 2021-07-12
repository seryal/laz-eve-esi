{==============================================================================|
| Project : EVE Online ESI Library                                             |
|==============================================================================|
| Copyright (c)2021, Yuri Serebrennikov                                        |
| All rights reserved.                                                         |
|==============================================================================|
| The Initial Developer of the Original Code is Yuri Serebrennikov             |
| All Rights Reserved.                                                         |
|==============================================================================|
|          (Found at URL: https://github.com/seryal/laz-eve-esi/)              |
|==============================================================================}
unit esiauthorization;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, URIParser, fphttpclient,
  ssockets, opensslsockets, Generics.Collections, base64, DCPsha256, LazLogger;

const
  OAUTH2_URL = 'https://login.eveonline.com/v2/oauth/authorize/';
  TOKEN_URL = 'https://login.eveonline.com/v2/oauth/token';

type

  TAccessToken = record
    AccessToken: string;
    RefreshToken: string;
    TokenType: string;
    Expires: integer;
  end;

  TCharacter = record
    CharacterID: uint64;
    CharacterName: string;
    ExpiresOn: string;
    Scopes: string;
    TokenType: string;
    CharacterOwnerHash: string;
    ClientID: string;
  end;

  TEVEScopeList = specialize TList<string>;

  { TEVEESIAuth }

  TEVEESIAuth = class
  private
    FPort: word;
    FRandom: array [0..31] of byte;
    FCodeChallenge: string;
    FCodeVerifier: string;
    FCallbackURL: string;
    FClientID: string;
    FRedirectURI: string;
    FRefreshToken: string;
    FScopeList: TEVEScopeList;
    FAccountID: int64;
    FAccountName: string;
    FState: string;
    function GetAuthURL: string;
    function GetScope(Index: integer): string;
    function GenerateCodeChallenge: string;
    function Base64UrlSafe(AValue: string): string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddScope(AScope: string);
    procedure ClearScope;
    procedure DeleteScope(AIndex: integer);
    function AuthByAccessToken(ACode: string; ACodeVerifier: string): TAccessToken;
    function AuthByRefreshToken(ARefreshCode: string): TAccessToken;
    function GetCharacter(AAccessToken: string): TCharacter;
    property ClientID: string read FClientID write FClientID;
    property CallbackURL: string read FCallbackURL write FCallbackURL;
    property Scope[Index: integer]: string read GetScope;
    property State: string read FState write FState;
    property AuthURL: string read GetAuthURL;
    property CodeVerifier: string read FCodeVerifier;
    property CodeChallenge: string read FCodeChallenge;
  end;

implementation

{ TEVEESIAuth }

function TEVEESIAuth.GetScope(Index: integer): string;
begin
  Result := FScopeList[Index];
end;

function TEVEESIAuth.GenerateCodeChallenge: string;
var
  ms: TStringStream;
  be: TBase64EncodingStream;
  str: string;
  hash: TDCP_sha256;
  Digest: array[0..31] of byte;
begin
  ms := TStringStream.Create('');
  be := TBase64EncodingStream.Create(ms);
  try
    be.Write(FRandom, 32);
    be.Flush;
    str := ms.DataString;
    str := Base64UrlSafe(str);
    FCodeVerifier := str;
  finally
    FreeAndNil(be);
    FreeAndNil(ms);
  end;
  ms := TStringStream.Create('');
  be := TBase64EncodingStream.Create(ms);
  hash := TDCP_sha256.Create(nil);
  try
    hash.Init;
    hash.UpdateStr(str);
    hash.Final(Digest);
    be.Write(Digest, 32);
    be.Flush;
    str := Base64UrlSafe(ms.DataString);
    str := StringReplace(str, '=', '', []);
  finally
    FreeAndNil(hash);
    FreeAndNil(be);
    FreeAndNil(ms);
  end;
  Result := str;
end;

function TEVEESIAuth.Base64UrlSafe(AValue: string): string;
var
  val: string;
begin
  val := AValue;
  val := StringReplace(val, '+', '-', [rfReplaceAll]);
  val := StringReplace(val, '/', '_', [rfReplaceAll]);
  Result := val;
end;

function TEVEESIAuth.GetAuthURL: string;
const
  _URL =
    '?response_type=code&redirect_uri=%s&client_id=%s&scope=%s&code_challenge=%s&code_challenge_method=S256';
var
  url: string;
  scopes: string;
  tmp: string;
  code_challenge_str: string;
begin
  scopes := '';
  for tmp in FScopeList do
    scopes := scopes + ' ' + tmp;
  scopes := trim(scopes);
  url := OAUTH2_URL + _URL;
  Result := format(url, [EncodeURLElement(FCallbackURL), FClientID,
    EncodeURLElement(scopes), Base64UrlSafe(FCodeChallenge)]);
  if State <> '' then
    Result := Result + format('&state=%s', [state]);
end;

constructor TEVEESIAuth.Create;
var
  i: integer;
begin
  FScopeList := TEVEScopeList.Create;
  FState := '';
  for i := 0 to 31 do
    FRandom[i] := Random($FF);
  FCodeChallenge := GenerateCodeChallenge;
end;

destructor TEVEESIAuth.Destroy;
begin
  FreeAndNil(FScopeList);
  inherited Destroy;
end;

procedure TEVEESIAuth.AddScope(AScope: string);
begin
  FScopeList.Add(AScope);
end;

procedure TEVEESIAuth.ClearScope;
begin
  FScopeList.Clear;
end;

procedure TEVEESIAuth.DeleteScope(AIndex: integer);
begin
  FScopeList.Delete(AIndex);
end;

function TEVEESIAuth.AuthByAccessToken(ACode: string;
  ACodeVerifier: string): TAccessToken;
const
  AUTH_STR = 'grant_type=authorization_code&code=%s&client_id=%s&code_verifier=%s';
var
  http: TFPHTTPClient;
  resp: string;
  form: string;
  jData: TJSONData;
begin
  http := TFPHTTPClient.Create(nil);
  try
    http.AddHeader('Content-Type', 'application/x-www-form-urlencoded');
    http.AddHeader('Host', 'login.eveonline.com');
    form := format(AUTH_STR, [ACode, ClientID, FCodeVerifier]);
    resp := http.FormPost(TOKEN_URL, form);
    if http.ResponseStatusCode <> 200 then
      raise Exception.Create(http.ResponseStatusCode.ToString + ': ' +
        http.ResponseStatusText);
    try
      jData := GetJSON(resp);
      Result.AccessToken := TJSONObject(jData).Get('access_token');
      Result.Expires := TJSONObject(jData).Get('expires_in');
      Result.TokenType := TJSONObject(jData).Get('token_type');
      Result.RefreshToken := TJSONObject(jData).Get('refresh_token');
    finally
      FreeAndNil(jData);
    end;
  finally
    FreeAndNil(http);
  end;
end;

function TEVEESIAuth.AuthByRefreshToken(ARefreshCode: string): TAccessToken;
const
  AUTH_STR = 'grant_type=refresh_token&refresh_token=%s&client_id=%s';
var
  http: TFPHTTPClient;
  resp: string;
  form: string;
  jData: TJSONData;
begin
  http := TFPHTTPClient.Create(nil);
  try
    http.AddHeader('Content-Type', 'application/x-www-form-urlencoded');
    http.AddHeader('Host', 'login.eveonline.com');
    form := format(AUTH_STR, [EncodeURLElement(ARefreshCode), ClientID]);
    resp := http.FormPost(TOKEN_URL, form);
    if http.ResponseStatusCode <> 200 then
      raise Exception.Create(http.ResponseStatusCode.ToString + ': ' +
        http.ResponseStatusText);
    jData := GetJSON(resp);
    try
      Result.AccessToken := TJSONObject(jData).Get('access_token');
      Result.Expires := TJSONObject(jData).Get('expires_in');
      Result.TokenType := TJSONObject(jData).Get('token_type');
      Result.RefreshToken := TJSONObject(jData).Get('refresh_token');
    finally
      FreeAndNil(jData);
    end;
  finally
    FreeAndNil(http);
  end;
end;

function TEVEESIAuth.GetCharacter(AAccessToken: string): TCharacter;
const
  URL = 'https://esi.evetech.net/verify';
var
  http: TFPHTTPClient;
  resp: string;
  jData: TJSONData;
begin
  http := TFPHTTPClient.Create(nil);
  try
    http.AddHeader('Content-Type', 'application/x-www-form-urlencoded');
    http.AddHeader('Host', 'login.eveonline.com');
    http.AddHeader('Authorization', 'Bearer ' + AAccessToken);
    resp := http.Get(URL);
    if http.ResponseStatusCode <> 200 then
      raise Exception.Create(http.ResponseStatusCode.ToString + ': ' +
        http.ResponseStatusText);
    try
      jData := GetJSON(resp);
      Result.CharacterID := TJSONObject(jData).Get('CharacterID');
      Result.CharacterName := TJSONObject(jData).Get('CharacterName');
      Result.CharacterOwnerHash := TJSONObject(jData).Get('CharacterOwnerHash');
      Result.ClientID := TJSONObject(jData).Get('ClientID');
      Result.ExpiresOn := TJSONObject(jData).Get('ExpiresOn');
      Result.Scopes := TJSONObject(jData).Get('Scopes', '');
      Result.TokenType := TJSONObject(jData).Get('TokenType');
    finally
      FreeAndNil(jData);
    end;
  finally
    FreeAndNil(http);
  end;
end;

end.
