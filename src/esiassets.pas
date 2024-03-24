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
unit esiassets;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, esibase;

type

  { TEVEAssetCharacter }

  TEVEAssetCharacter = class(TCollectionItem)
  private
    Fis_blueprint_copy: boolean;
    Fis_singleton: boolean;
    Fitem_id: uint64;
    Flocation_flag: string;
    Flocation_id: uint64;
    Flocation_type: string;
    Fquantity: integer;
    Ftype_id: integer;
  published
    property is_blueprint_copy: boolean read Fis_blueprint_copy write Fis_blueprint_copy;
    property is_singleton: boolean read Fis_singleton write Fis_singleton;
    property item_id: uint64 read Fitem_id write Fitem_id;
    property location_flag: string read Flocation_flag write Flocation_flag;
    property location_id: uint64 read Flocation_id write Flocation_id;
    property location_type: string read Flocation_type write Flocation_type;
    property quantity: integer read Fquantity write Fquantity;
    property type_id: integer read Ftype_id write Ftype_id;
  end;

  { TEVEAssetCharacterList }

  TEVEAssetCharacterList = class(TCollection)
  private
    function GetItems(Index: integer): TEVEAssetCharacter;
  public
    property Items[Index: integer]: TEVEAssetCharacter read GetItems;
  end;

  { TEVEAssetLocationPosition }

  TEVEAssetLocationPosition = class(TCollectionItem)
  private
    Fx: double;
    Fy: double;
    Fz: double;
  published
    property x: double read Fx write Fx;
    property y: double read Fy write Fy;
    property z: double read Fz write Fz;
  end;

  { TEVEAssetLocation }

  TEVEAssetLocation = class(TCollectionItem)
  private
    Fitem_id: uint64;
    Fposition: TEVEAssetLocationPosition;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
  published
    property item_id: uint64 read Fitem_id write Fitem_id;
    property position: TEVEAssetLocationPosition read Fposition write Fposition;
  end;

  { TEVEAssetLocationList }

  TEVEAssetLocationList = class(TCollection)
  private
    function GetItems(Index: integer): TEVEAssetLocation;
  public
    property Items[Index: integer]: TEVEAssetLocation read GetItems;
  end;


  { TEVEAssetName }

  TEVEAssetName = class(TCollectionItem)
  private
    Fitem_id: uint64;
    Fname: string;
  published
    property item_id: uint64 read Fitem_id write Fitem_id;
    property &name: string read Fname write Fname;
  end;

  { TEVEAssetNameList }

  TEVEAssetNameList = class(TCollection)
  private
    function GetItems(Index: integer): TEVEAssetName;
  public
    property Items[Index: integer]: TEVEAssetName read GetItems;
  end;

  { TEVEAssetCorporation }

  TEVEAssetCorporation = class(TCollectionItem)
  private
    Fis_blueprint_copy: boolean;
    Fis_singleton: boolean;
    Fitem_id: uint64;
    Flocation_flag: string;
    Flocation_id: uint64;
    Flocation_type: string;
    Fquantity: integer;
    Ftype_id: integer;
  published
    property is_blueprint_copy: boolean read Fis_blueprint_copy write Fis_blueprint_copy;
    property is_singleton: boolean read Fis_singleton write Fis_singleton;
    property item_id: uint64 read Fitem_id write Fitem_id;
    property location_flag: string read Flocation_flag write Flocation_flag;
    property location_id: uint64 read Flocation_id write Flocation_id;
    property location_type: string read Flocation_type write Flocation_type;
    property quantity: integer read Fquantity write Fquantity;
    property type_id: integer read Ftype_id write Ftype_id;
  end;

  { TEVEAssetCorporationList }

  TEVEAssetCorporationList = class(TCollection)
  private
    function GetItems(Index: integer): TEVEAssetCorporation;
  public
    property Items[Index: integer]: TEVEAssetCorporation read GetItems;
  end;

  { TESIAssets }

  TESIAssets = class(TESIBase)
  public
    function GetAssets(const AAccessToken: string; ACharacterId: integer; APage: integer): TEVEAssetCharacterList;
    function GetLocation(const AAccessToken: string; ACharacterId: integer; AList: TStringList): TEVEAssetLocationList;
    {Return names for a set of item ids, which you can get from character assets endpoint. Typically used for items that can customize names, like containers or ships.}
    function GetNames(const AAccessToken: string; ACharacterId: integer; AList: TStringList): TEVEAssetNameList;
    {Return a list of the corporation assets }
    function GetCorporation(const AAccessToken: string; ACharacterId: integer; APage: integer): TEVEAssetCorporationList;
    function GetCorporationLocation(const AAccessToken: string; ACorporationId: integer; AList: TStringList): TEVEAssetLocationList;
    function GetCorporationNames(const AAccessToken: string; ACorporationId: integer; AList: TStringList): TEVEAssetNameList;
  end;

implementation

{ TEVEAssetCorporationList }

function TEVEAssetCorporationList.GetItems(Index: integer): TEVEAssetCorporation;
begin
  Result := TEVEAssetCorporation(inherited Items[Index]);
end;

{ TEVEAssetNameList }

function TEVEAssetNameList.GetItems(Index: integer): TEVEAssetName;
begin
  Result := TEVEAssetName(inherited Items[Index]);
end;

{ TEVEAssetLocationList }

function TEVEAssetLocationList.GetItems(Index: integer): TEVEAssetLocation;
begin
  Result := TEVEAssetLocation(inherited Items[Index]);
end;

{ TEVEAssetLocation }

constructor TEVEAssetLocation.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  Fposition := TEVEAssetLocationPosition.Create(nil);
end;

destructor TEVEAssetLocation.Destroy;
begin
  FreeAndNil(Fposition);
  inherited Destroy;
end;

{ TEVEAssetCharacterList }

function TEVEAssetCharacterList.GetItems(Index: integer): TEVEAssetCharacter;
begin
  Result := TEVEAssetCharacter(inherited Items[Index]);
end;

{ TESIAssets }

function TESIAssets.GetAssets(const AAccessToken: string; ACharacterId: integer; APage: integer): TEVEAssetCharacterList;
const
  URL = 'https://esi.evetech.net/latest/characters/%s/assets/?datasource=%s&page=%d';
begin
  Result := TEVEAssetCharacterList.Create(TEVEAssetCharacter);
  DeStreamerArray(Get(AAccessToken, Format(URL, [ACharacterId.ToString, DataSource, APage])), TCollection(Result));
end;

function TESIAssets.GetLocation(const AAccessToken: string; ACharacterId: integer; AList: TStringList): TEVEAssetLocationList;
const
  URL = 'https://esi.evetech.net/latest/characters/%s/assets/locations/?datasource=%s';
var
  str: string;
  res: string;
begin
  Result := TEVEAssetLocationList.Create(TEVEAssetLocation);
  res := '';
  for str in AList do
    res := res + str + ',';
  res := Copy(res, 0, Length(res) - 1);
  res := '[' + res + ']';
  DeStreamerArray(Post(AAccessToken, Format(URL, [ACharacterId.ToString, DataSource]), res), TCollection(Result));
end;

function TESIAssets.GetNames(const AAccessToken: string; ACharacterId: integer; AList: TStringList): TEVEAssetNameList;
const
  URL = 'https://esi.evetech.net/latest/characters/%s/assets/names/?datasource=%s';
var
  str: string;
  res: string;
begin
  Result := TEVEAssetNameList.Create(TEVEAssetName);
  res := '';
  for str in AList do
    res := res + str + ',';
  res := Copy(res, 0, Length(res) - 1);
  res := '[' + res + ']';
  DeStreamerArray(Post(AAccessToken, Format(URL, [ACharacterId.ToString, DataSource]), res), TCollection(Result));
end;

function TESIAssets.GetCorporation(const AAccessToken: string; ACharacterId: integer; APage: integer): TEVEAssetCorporationList;
const
  URL = 'https://esi.evetech.net/latest/corporations/%s/assets/?datasource=%s&page=%d';
begin
  Result := TEVEAssetCorporationList.Create(TEVEAssetCorporation);
  DeStreamerArray(Get(AAccessToken, Format(URL, [ACharacterId.ToString, DataSource, APage])), TCollection(Result));
end;

function TESIAssets.GetCorporationLocation(const AAccessToken: string; ACorporationId: integer; AList: TStringList): TEVEAssetLocationList;
const
  URL = 'https://esi.evetech.net/latest/corporations/%s/assets/locations/?datasource=%s';
var
  str: string;
  res: string;
begin
  Result := TEVEAssetLocationList.Create(TEVEAssetLocation);
  res := '';
  for str in AList do
    res := res + str + ',';
  res := Copy(res, 0, Length(res) - 1);
  res := '[' + res + ']';
  DeStreamerArray(Post(AAccessToken, Format(URL, [ACorporationId.ToString, DataSource]), res), TCollection(Result));
end;

function TESIAssets.GetCorporationNames(const AAccessToken: string; ACorporationId: integer; AList: TStringList): TEVEAssetNameList;
const
  URL = 'https://esi.evetech.net/latest/corporations/%s/assets/names/?datasource=%s';
var
  str: string;
  res: string;
begin
  Result := TEVEAssetNameList.Create(TEVEAssetName);
  res := '';
  for str in AList do
    res := res + str + ',';
  res := Copy(res, 0, Length(res) - 1);
  res := '[' + res + ']';
  DeStreamerArray(Post(AAccessToken, Format(URL, [ACorporationId.ToString, DataSource]), res), TCollection(Result));
end;

end.
