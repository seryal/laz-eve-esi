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

  TESIArrayIntegerValue = class(TCollectionItem)
  private
    FValue: integer;
  published
    property Value: integer read FValue write FValue;
  end;

  TESIArrayIntegerValueList = class(TCollection)
  private
    function GetItems(Index: integer): integer;
  public
    constructor Create;
    property Items[Index: integer]: integer read GetItems;
  end;

  TESIBase = class
  protected
    function Get(const AAuthKey, AURL: string): string;
    function Get(const AURL: string): string;
    function Post(const AURL, AValue: string): string;
    function Post(const AAuthKey, AURL, AValue: string): string;
    function Put(const AAuthKey, AURL, AValue: string): string;
    procedure DeStreamerObject(const AJsonString: string; var AObject: TObject);
    procedure DeStreamerArray(const AJsonString: string; var AObject: TCollection);
    procedure DeStreamerArray(const AJsonString: string; var V: variant);
    procedure RestorePropertyNotify(Sender: TObject; AObject: TObject; Info: PPropInfo; AValue: TJSONData; var Handled: boolean); virtual;
    procedure SetArrayIntegerValue(AESIArrayIntegerValueList: TESIArrayIntegerValueList; AValue: TJSONArray);
  private
    FDataSource: string;
  public
    constructor Create;
    property DataSource: string read FDataSource write FDataSource;
  end;

implementation

{ TESIArrayIntegerValueList }

function TESIArrayIntegerValueList.GetItems(Index: integer): integer;
begin
  Result := TESIArrayIntegerValue(inherited Items[Index]).Value;
end;

constructor TESIArrayIntegerValueList.Create;
begin
  inherited Create(TESIArrayIntegerValue);
end;

{ TESIBase }

constructor TESIBase.Create;
begin
  DataSource := 'tranquility';
end;

function TESIBase.Get(const AURL: string): string;
begin
  Result := Get('', AURL);
end;

function TESIBase.Post(const AURL, AValue: string): string;
begin
  Result := Post('', AURL, AValue);
end;

function TESIBase.Post(const AAuthKey, AURL, AValue: string): string;
var
  http: TFPHTTPClient;
begin
  http := TFPHTTPClient.Create(nil);
  try
    http.AddHeader('Content-Type', 'application/x-www-form-urlencoded');
    http.AddHeader('Host', 'login.eveonline.com');
    if AAuthKey <> '' then
      http.AddHeader('Authorization', 'Bearer ' + AAuthKey);
    Result := http.FormPost(AURL, AValue);
    if http.ResponseStatusCode <> 200 then
      raise Exception.Create(http.ResponseStatusCode.ToString + ': ' +
        http.ResponseStatusText);
  finally
    FreeAndNil(http);
  end;
end;

function TESIBase.Put(const AAuthKey, AURL, AValue: string): string;
var
  http: TFPHTTPClient;
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
      raise Exception.Create(http.ResponseStatusCode.ToString + ': ' +
        http.ResponseStatusText);
  finally
    http.RequestBody.Free;
    FreeAndNil(http);
  end;
end;

procedure TESIBase.DeStreamerObject(const AJsonString: string; var AObject: TObject);
var
  jsoDeSerialize: TJSONDeStreamer;
begin
  jsoDeSerialize := TJSONDeStreamer.Create(nil);
  try
    jsoDeSerialize.OnRestoreProperty := @RestorePropertyNotify;
    jsoDeSerialize.JSONToObject(AJsonString, AObject);
  finally
    FreeAndNil(jsoDeSerialize);
  end;
end;

procedure TESIBase.DeStreamerArray(const AJsonString: string; var AObject: TCollection);
var
  jsoDeSerialize: TJSONDeStreamer;
begin
  jsoDeSerialize := TJSONDeStreamer.Create(nil);
  try
    jsoDeSerialize.OnRestoreProperty := @RestorePropertyNotify;
    jsoDeSerialize.JSONToObject(AJsonString, AObject);
  finally
    FreeAndNil(jsoDeSerialize);
  end;
end;

procedure TESIBase.DeStreamerArray(const AJsonString: string; var V: variant);
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

procedure TESIBase.RestorePropertyNotify(Sender: TObject; AObject: TObject;
  Info: PPropInfo; AValue: TJSONData; var Handled: boolean);
var
  lObject: TObject;
begin
  if AValue.JSONType = jtArray then
  begin
    lObject := GetObjectProp(AObject, Info^.Name);
    if (lObject <> nil) and (lObject.ClassType = TESIArrayIntegerValueList) then
    begin
      SetArrayIntegerValue(TESIArrayIntegerValueList(lObject), TJSONArray(AValue));
      Handled := True;
    end;
  end;
end;

procedure TESIBase.SetArrayIntegerValue(AESIArrayIntegerValueList:
  TESIArrayIntegerValueList; AValue: TJSONArray);
var
  lIntegerObject: TESIArrayIntegerValue;
  I: integer;
begin
  for I := 0 to AValue.Count - 1 do
  begin
    lIntegerObject := TESIArrayIntegerValue(AESIArrayIntegerValueList.Add);
    lIntegerObject.Value := AValue.Items[I].AsInteger;
  end;
end;

function TESIBase.Get(const AAuthKey, AURL: string): string;
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
      raise Exception.Create(http.ResponseStatusCode.ToString + ': ' +
        http.ResponseStatusText);
  finally
    FreeAndNil(http);
  end;
end;

end.
