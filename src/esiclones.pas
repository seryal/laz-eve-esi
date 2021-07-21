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
unit esiclones;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, esibase, Generics.Collections, Variants;

type

  { TEVEClonesHomeLocation }

  TEVEClonesHomeLocation = class(TCollectionItem)
  private
    Flocation_id: uint64;
    Flocation_type: string;
  published
    property location_id: uint64 read Flocation_id write Flocation_id;
    property location_type: string read Flocation_type write Flocation_type;
  end;

  TEVEImplantList = specialize TList<longint>;

  { TEVEClonesJumpClone }

  TEVEClonesJumpClone = class(TCollectionItem)
  private
    Fimplants: TEVEImplantList;
    Fjump_clone_id: longint;
    Flocation_id: uint64;
    Flocation_type: string;
    Fname: string;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
  published
    property implants: TEVEImplantList read Fimplants write Fimplants;
    property jump_clone_id: integer read Fjump_clone_id write Fjump_clone_id;
    property location_id: uint64 read Flocation_id write Flocation_id;
    property location_type: string read Flocation_type write Flocation_type;
    property &name: string read Fname write Fname;
  end;

  { TEVEClonesJumpCloneList }

  TEVEClonesJumpCloneList = class(TCollection)
  private
    function GetItems(Index: integer): TEVEClonesJumpClone;
  public
    property Items[Index: integer]: TEVEClonesJumpClone read GetItems;
  end;

  { TEVEClonesCharacter }

  TEVEClonesCharacter = class(TCollectionItem)
  private
    Fhome_location: TEVEClonesHomeLocation;
    Fjump_clones: TEVEClonesJumpCloneList;
    Flast_clone_jump_date: string;
    Flast_station_change_date: string;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
  published
    property home_location: TEVEClonesHomeLocation read Fhome_location write Fhome_location;
    property jump_clones: TEVEClonesJumpCloneList read Fjump_clones write Fjump_clones;
    property last_clone_jump_date: string read Flast_clone_jump_date write Flast_clone_jump_date;
    property last_station_change_date: string read Flast_station_change_date write Flast_station_change_date;
  end;

  { TESIClones }

  TESIClones = class(TESIBase)
  public
    {A list of the character’s clones}
    function GetClones(AAccessToken: string; ACharacterId: integer): TEVEClonesCharacter;
    {Return implants on the active clone of a character}
    function GetImplants(AAccessToken: string; ACharacterId: integer): TEVEImplantList;
  end;

implementation

{ TEVEClonesCharacter }

constructor TEVEClonesCharacter.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  Fhome_location := TEVEClonesHomeLocation.Create(nil);
  Fjump_clones := TEVEClonesJumpCloneList.Create(nil);
end;

destructor TEVEClonesCharacter.Destroy;
begin
  FreeAndNil(Fjump_clones);
  FreeAndNil(Fhome_location);
  inherited Destroy;
end;

{ TEVEClonesJumpClone }

constructor TEVEClonesJumpClone.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  Fimplants := TEVEImplantList.Create;
end;

destructor TEVEClonesJumpClone.Destroy;
begin
  FreeAndNil(Fimplants);
  inherited Destroy;
end;

{ TEVEClonesJumpCloneList }

function TEVEClonesJumpCloneList.GetItems(Index: integer): TEVEClonesJumpClone;
begin
  Result := TEVEClonesJumpClone(inherited Items[Index]);
end;

{ TESIClones }

function TESIClones.GetClones(AAccessToken: string; ACharacterId: integer): TEVEClonesCharacter;
const
  URL = 'https://esi.evetech.net/latest/characters/%s/clones/?datasource=%s';
begin
  Result := TEVEClonesCharacter.Create(nil);
  DeStreamerObject(Get(AAccessToken, Format(URL, [ACharacterId.ToString, DataSource])), TObject(Result));
end;

function TESIClones.GetImplants(AAccessToken: string; ACharacterId: integer): TEVEImplantList;
const
  URL = 'https://esi.evetech.net/latest/characters/%s/implants/?datasource=%s';
var
  v: variant;
  i: integer;
begin
  Result := TEVEImplantList.Create;
  DeStreamerArray(Get(AAccessToken, Format(URL, [ACharacterId.ToString, DataSource])), V);
  for I := VarArrayLowBound(V, 1) to VarArrayHighBound(V, 1) do
  begin
    case VarType(V[I]) of
      varInteger: Result.Add(V[I]);
    end;
  end;
end;

end.
