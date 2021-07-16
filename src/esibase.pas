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
  Classes, SysUtils, fphttpclient, opensslsockets, fpjsonrtti;

type

  { TEVEBase }

  TEVEBase = class
  protected
    function Get(AAuthKey, AURL: string): string;
    function Get(AURL: string): string;
    function Post(AURL: string; AValue: string): string;
    procedure DeStreamerObject(AJsonString: string; var AObject: TObject);
    procedure DeStreamerArray(AJsonString: string; var AObject: TCollection);
  private
    FDataSource: string;
  public
    constructor Create;
    property DataSource: string read FDataSource write FDataSource;
  end;


implementation

{ TEVEBase }

constructor TEVEBase.Create;
begin
  DataSource := 'tranquility';
end;

function TEVEBase.Get(AURL: string): string;
begin
  Result := Get('', AURL);
end;

function TEVEBase.Post(AURL: string; AValue: string): string;
var
  http: TFPHTTPClient;
  res: integer;
begin
  http := TFPHTTPClient.Create(nil);
  try
    http.AddHeader('Content-Type', 'application/x-www-form-urlencoded');
    http.AddHeader('Host', 'login.eveonline.com');
    Result := http.FormPost(AURL, AValue);
    if http.ResponseStatusCode <> 200 then
      raise Exception.Create(http.ResponseStatusCode.ToString + ': ' + http.ResponseStatusText);
  finally
    FreeAndNil(http);
  end;
end;

procedure TEVEBase.DeStreamerObject(AJsonString: string; var AObject: TObject);
var
  jsoDeSerialize: TJSONDeStreamer;
begin
  jsoDeSerialize := TJSONDeStreamer.Create(nil);
  try
    jsoDeSerialize.JSONToObject(AJsonString, AObject);
  finally
    FreeAndNil(jsoDeSerialize);
  end;
end;

procedure TEVEBase.DeStreamerArray(AJsonString: string; var AObject: TCollection);
var
  jsoDeSerialize: TJSONDeStreamer;
begin
  jsoDeSerialize := TJSONDeStreamer.Create(nil);
  try
    jsoDeSerialize.JSONToCollection(AJsonString, AObject);
  finally
    FreeAndNil(jsoDeSerialize);
  end;
end;

function TEVEBase.Get(AAuthKey, AURL: string): string;
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
