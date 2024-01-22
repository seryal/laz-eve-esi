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
unit esiuniverse;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, esibase;

type
  TEVEUniverseTypesType = class(TCollectionItem)
  private
    Fcapacity: Double;
    Fdescription: String;
//    Fdogma_attributes: TEVEUniverseTypesTypeDogmaAttributes;
//    Fdogma_effects: TEVEUniverseTypesTypeDogmaEffects;
    Fgraphic_id: Integer;
    Fgroup_id: Integer;
    Ficon_id: Integer;
    Fmarket_group_id: Integer;
    Fmass: Double;
    Fname: String;
    Fpackaged_volume: Double;
    Fportion_size: Integer;
    Fpublished: Boolean;
    Fradius: Double;
    Ftype_id: Integer;
    Fvolume: Double;
  published
    property capacity: Double read Fcapacity write Fcapacity;
    property description: String read Fdescription write Fdescription;
//    property   Fdogma_attributes: TEVEUniverseTypesTypeDogmaAttributes;
//    property   Fdogma_effects: TEVEUniverseTypesTypeDogmaEffects;
    property graphic_id: Integer read Fgraphic_id write Fgraphic_id;
    property group_id: Integer read Fgroup_id write Fgroup_id;
    property icon_id: Integer read Ficon_id write Ficon_id;
    property market_group_id: Integer read Fmarket_group_id write Fmarket_group_id;
    property mass: Double read Fmass write Fmass;
    property name: String read Fname write Fname;
    property packaged_volume: Double read Fpackaged_volume write Fpackaged_volume;
    property portion_size: Integer read Fportion_size write Fportion_size;
    property published: Boolean read Fpublished write Fpublished;
    property radius: Double read Fradius write Fradius;
    property type_id: Integer read Ftype_id write Ftype_id;
    property volume: Double read Fvolume write Fvolume;
  end;

  TEVEUniverseGroupsGroup = class(TCollectionItem)
  private
    Fcategory_id: Integer;
    Fgroup_id: Integer;
    Fname: String;
    Fpublished: Boolean;
//    Ftypes
  published
    property category_id: Integer read Fcategory_id write Fcategory_id;
    property group_id: Integer read Fgroup_id write Fgroup_id;
    property name: String read Fname write Fname;
    property published: Boolean read Fpublished write Fpublished;
//    property types
  end;

  TEVEUniverseCategoriesCategory = class(TCollectionItem)
  private
    Fcategory_id: Integer;
//    Fgroups
    Fname: String;
    Fpublished: Boolean;
  published
    property category_id: Integer read Fcategory_id write Fcategory_id;
//    property groups
    property name: String read Fname write Fname;
    property published: Boolean read Fpublished write Fpublished;
  end;

  { TEVEUniverse }

  TEVEUniverse = class(TESIBase)
  public
    { Get information on a type }
    function GetUniverseTypesType(ATypeId: Integer): TEVEUniverseTypesType;
    { Get information on an item group }
    function GetUniverseGroupsGroup(AGroupId: Integer): TEVEUniverseGroupsGroup;
    { Get information of an item category }
    function GetUniverseCategoriesCategory(ACategoryId: Integer): TEVEUniverseCategoriesCategory;
  end;

implementation

{ TEVEUniverse }

function TEVEUniverse.GetUniverseTypesType(ATypeId: Integer): TEVEUniverseTypesType;
const
  URL = 'https://esi.evetech.net/latest/universe/types/%d/?datasource=%s&language=en';
begin
  Result := TEVEUniverseTypesType.Create(nil);
  DeStreamerObject(Get(Format(URL, [ATypeId, DataSource])), TObject(Result));
end;

function TEVEUniverse.GetUniverseGroupsGroup(AGroupId: Integer): TEVEUniverseGroupsGroup;
const
  URL = 'https://esi.evetech.net/latest/universe/groups/%d/?datasource=%s&language=en';
begin
  Result := TEVEUniverseGroupsGroup.Create(nil);
  DeStreamerObject(Get(Format(URL, [AGroupId, DataSource])), TObject(Result));
end;

function TEVEUniverse.GetUniverseCategoriesCategory(ACategoryId: Integer): TEVEUniverseCategoriesCategory;
const
  URL = 'https://esi.evetech.net/latest/universe/categories/%d/?datasource=%s&language=en';
begin
  Result := TEVEUniverseCategoriesCategory.Create(nil);
  DeStreamerObject(Get(Format(URL, [ACategoryId, DataSource])), TObject(Result));
end;

end.

