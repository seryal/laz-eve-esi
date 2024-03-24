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
  Classes, SysUtils, esibase;

type

  { TEVELocationLocation }

  TEVELocationLocation = class(TCollectionItem)
  private
    FSolarSystemId: UInt64;
    FStationId: integer;
    FStructureId: UInt64;
  published
    property solar_system_id: UInt64 read FSolarSystemId write FSolarSystemId;
    property station_id: integer read FStationId write FStationId;
    property structure_id: UInt64 read FStructureId write FStructureId;
  end;

  { TEVELocationOnline }

  TEVELocationOnline = class(TCollectionItem)
  private
    FLastLogin: string;
    FLastLogout: string;
    FLogins: UInt64;
    FOnline: boolean;
  published
    property last_login: string read FLastLogin write FLastLogin;
    property last_logout: string read FLastLogout write FLastLogout;
    property logins: UInt64 read FLogins write FLogins;
    property online: boolean read FOnline write FOnline;
  end;

  { TEVELocationShip }

  TEVELocationShip = class(TCollectionItem)
  private
    FShipItemId: UInt64;
    FShipName: string;
    FShipTypeId: integer;
  published
    property ship_item_id: UInt64 read FShipItemId write FShipItemId;
    property ship_name: string read FShipName write FShipName;
    property ship_type_id: integer read FShipTypeId write FShipTypeId;
  end;

  { TESILocation }

  TESILocation = class(TESIBase)
  private
    FAuthKey: string;
  public
    function GetLocation(const AAccessToken: string; const ACharacterId: UInt64): TEVELocationLocation;
    function GetOnline(const AAccessToken: string; const ACharacterId: UInt64): TEVELocationOnline;
    function GetShip(const AAccessToken: string; const ACharacterId: UInt64): TEVELocationShip;
  end;


implementation

{ TESILocation }

function TESILocation.GetLocation(const AAccessToken: string; const ACharacterId: UInt64): TEVELocationLocation;
const
  URL = 'https://esi.evetech.net/latest/characters/%s/location/?datasource=%s';
begin
  Result := TEVELocationLocation.Create(nil);
  DeStreamerObject(Get(AAccessToken, Format(URL, [ACharacterId.ToString, DataSource])), TObject(Result));
end;

function TESILocation.GetOnline(const AAccessToken: string; const ACharacterId: UInt64): TEVELocationOnline;
const
  URL = 'https://esi.evetech.net/latest/characters/%s/online/?datasource=%s';
begin
  Result := TEVELocationOnline.Create(nil);
  DeStreamerObject(Get(AAccessToken, Format(URL, [ACharacterId.ToString, DataSource])), TObject(Result));
end;

function TESILocation.GetShip(const AAccessToken: string; const ACharacterId: UInt64): TEVELocationShip;
const
  URL = 'https://esi.evetech.net/latest/characters/%s/ship/?datasource=%s';
begin
  Result := TEVELocationShip.Create(nil);
  DeStreamerObject(Get(AAccessToken, Format(URL, [ACharacterId.ToString, DataSource])), TObject(Result));
end;

end.
