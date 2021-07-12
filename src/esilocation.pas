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
unit esilocation;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpjson, esibase;

type

  TEVELocationLocation = record
    SolarSystemId: uint64;
    StationId: uint64;
    StructureId: uint64;
  end;

  TEVELocationOnline = record
    LastLogin: string;
    LastLogout: string;
    Logins: uint64;
    Online: boolean;
  end;

  TEVELocationShip = record
    ShipItemId: uint64;
    ShipName: string;
    ShipTypeId: uint64;
  end;

  { TEVEESILocation }

  TEVEESILocation = class(TEVEBase)
  private
    FAuthKey: string;
  public
    function GetLocation(AAccessToken: string; ACharacterId: uint64): TEVELocationLocation;
    function GetOnline(AAccessToken: string; ACharacterId: uint64): TEVELocationOnline;
    function GetShip(AAccessToken: string; ACharacterId: uint64): TEVELocationShip;
  end;


implementation

{ TEVEESILocation }

function TEVEESILocation.GetLocation(AAccessToken: string; ACharacterId: uint64): TEVELocationLocation;
const
  URL = 'https://esi.evetech.net/latest/characters/%s/location/?datasource=%s';
var
  req_url: string;
  res: string;
  jData: TJSONData;
begin
  req_url := Format(URL, [ACharacterId.ToString, DataSource]);
  res := Get(AAccessToken, req_url);
  try
    jData := GetJSON(res);
    Result.SolarSystemId := TJSONObject(jData).Get('solar_system_id', 0);
    Result.StationId := TJSONObject(jData).Get('station_id', 0);
    Result.StructureId := TJSONObject(jData).Get('structure_id', 0);
  finally
    FreeAndNil(jData);
  end;
end;

function TEVEESILocation.GetOnline(AAccessToken: string; ACharacterId: uint64): TEVELocationOnline;
const
  URL = 'https://esi.evetech.net/latest/characters/%s/online/?datasource=%s';
var
  req_url: string;
  res: string;
  jData: TJSONData;
begin
  req_url := Format(URL, [ACharacterId.ToString, DataSource]);
  res := Get(AAccessToken, req_url);
  try
    jData := GetJSON(res);
    Result.LastLogin := TJSONObject(jData).Get('last_login', '');
    Result.LastLogout := TJSONObject(jData).Get('last_logout', '');
    Result.Logins := TJSONObject(jData).Get('logins', 0);
    Result.Online := TJSONObject(jData).Get('online', False);
  finally
    FreeAndNil(jData);
  end;
end;

function TEVEESILocation.GetShip(AAccessToken: string; ACharacterId: uint64): TEVELocationShip;
const
  URL = 'https://esi.evetech.net/latest/characters/%s/ship/?datasource=%s';
var
  req_url: string;
  res: string;
  jData: TJSONData;
begin
  req_url := Format(URL, [ACharacterId.ToString, DataSource]);
  res := Get(AAccessToken, req_url);
  try
    jData := GetJSON(res);
    Result.ShipItemId := TJSONObject(jData).Get('ship_item_id', 0);
    Result.ShipName := TJSONObject(jData).Get('ship_name', '');
    Result.ShipTypeId := TJSONObject(jData).Get('ship_type_id', 0);
  finally
    FreeAndNil(jData);
  end;
end;

end.
