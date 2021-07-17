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

  { TEVEAssets }

  TEVEAssets = class(TEVEBase)
  public
    function GetAssets(AAccessToken: string; ACharacterId: integer; APage: integer): TEVEAssetCharacterList;
  end;

implementation

{ TEVEAssetCharacterList }

function TEVEAssetCharacterList.GetItems(Index: integer): TEVEAssetCharacter;
begin
  Result := TEVEAssetCharacter(inherited Items[Index]);
end;

{ TEVEAssets }

function TEVEAssets.GetAssets(AAccessToken: string; ACharacterId: integer; APage: integer): TEVEAssetCharacterList;
const
  URL = 'https://esi.evetech.net/latest/characters/%s/assets/?datasource=%s&page=%d';
begin
  Result := TEVEAssetCharacterList.Create(TEVEAssetCharacter);
  DeStreamerArray(Get(AAccessToken, Format(URL, [ACharacterId.ToString, DataSource, APage])), TCollection(Result));
end;

end.
