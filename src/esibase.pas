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
unit esibase;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fphttpclient, opensslsockets, fpjsonrtti, TypInfo, fpjson;

type

  { TESIBase }

  TESIBase = class
  protected
    function Get(AAuthKey, AURL: string): string;
    function Get(AURL: string): string;
    function Post(AURL: string; AValue: string): string;
    function Post(AAuthKey, AURL: string; AValue: string): string;
    function Put(AAuthKey, AURL: string; AValue: string): string;
    procedure DeStreamerObject(AJsonString: string; var AObject: TObject);
    procedure DeStreamerArray(AJsonString: string; var AObject: TCollection);
    procedure DeStreamerArray(AJsonString: string; var V: variant);
    procedure RestorePropertyNotify(Sender: TObject; AObject: TObject; Info: PPropInfo; AValue: TJSONData; var Handled: boolean); virtual;
  private
    FDataSource: string;
  public
    constructor Create;
    property DataSource: string read FDataSource write FDataSource;
  end;


implementation

{ TESIBase }

constructor TESIBase.Create;
begin
  DataSource := 'tranquility';
end;

function TESIBase.Get(AURL: string): string;
begin
  Result := Get('', AURL);
end;

function TESIBase.Post(AURL: string; AValue: string): string;
begin
  Result := Post('', AURL, AValue);
end;

function TESIBase.Post(AAuthKey, AURL: string; AValue: string): string;
var
  http: TFPHTTPClient;
  res: integer;
begin
  http := TFPHTTPClient.Create(nil);
  try
    http.AddHeader('Content-Type', 'application/x-www-form-urlencoded');
    http.AddHeader('Host', 'login.eveonline.com');
    if AAuthKey <> '' then
      http.AddHeader('Authorization', 'Bearer ' + AAuthKey);
    Result := http.FormPost(AURL, AValue);
    if http.ResponseStatusCode <> 200 then
      raise Exception.Create(http.ResponseStatusCode.ToString + ': ' + http.ResponseStatusText);
  finally
    FreeAndNil(http);
  end;
end;

function TESIBase.Put(AAuthKey, AURL: string; AValue: string): string;
var
  http: TFPHTTPClient;
  res: integer;
begin
  http := TFPHTTPClient.Create(nil);
  try
    http.AddHeader('Content-Type', 'application/x-www-form-urlencoded');
    http.AddHeader('Host', 'login.eveonline.com');
    if AAuthKey <> '' then
      http.AddHeader('Authorization', 'Bearer ' + AAuthKey);
    http.RequestBody := TRawByteStringStream.Create(AValue);
    Result := http.Put(AURL);
    if http.ResponseStatusCode <> 204 then
      raise Exception.Create(http.ResponseStatusCode.ToString + ': ' + http.ResponseStatusText);
  finally
    http.RequestBody.Free;
    FreeAndNil(http);
  end;
end;

procedure TESIBase.DeStreamerObject(AJsonString: string; var AObject: TObject);
var
  jsoDeSerialize: TJSONDeStreamer;
begin
  jsoDeSerialize := TJSONDeStreamer.Create(nil);
  jsoDeSerialize.OnRestoreProperty := @RestorePropertyNotify;
  try
    jsoDeSerialize.JSONToObject(AJsonString, AObject);
  finally
    FreeAndNil(jsoDeSerialize);
  end;
end;

procedure TESIBase.DeStreamerArray(AJsonString: string; var AObject: TCollection);
var
  jsoDeSerialize: TJSONDeStreamer;
begin
  jsoDeSerialize := TJSONDeStreamer.Create(nil);
  jsoDeSerialize.OnRestoreProperty := @RestorePropertyNotify;
  try
    jsoDeSerialize.JSONToObject(AJsonString, AObject);
  finally
    FreeAndNil(jsoDeSerialize);
  end;
end;

procedure TESIBase.DeStreamerArray(AJsonString: string; var V: variant);
var
  jsoDeSerialize: TJSONDeStreamer;
begin
  jsoDeSerialize := TJSONDeStreamer.Create(nil);
  try
    V := jsoDeSerialize.JSONToVariant(AJsonString);
  finally
    FreeAndNil(jsoDeSerialize);
  end;
end;

procedure TESIBase.RestorePropertyNotify(Sender: TObject; AObject: TObject; Info: PPropInfo; AValue: TJSONData; var Handled: boolean);
begin

end;

function TESIBase.Get(AAuthKey, AURL: string): string;
var
  http: TFPHTTPClient;
begin
  http := TFPHTTPClient.Create(nil);
  try
    http.AddHeader('Content-Type', 'application/x-www-form-urlencoded');
    http.AddHeader('Host', 'login.eveonline.com');
    if AAuthKey <> '' then
      http.AddHeader('Authorization', 'Bearer ' + AAuthKey);
    Result := http.Get(AURL);
    if http.ResponseStatusCode <> 200 then
      raise Exception.Create(http.ResponseStatusCode.ToString + ': ' + http.ResponseStatusText);
  finally
    FreeAndNil(http);
  end;
end;

end.
