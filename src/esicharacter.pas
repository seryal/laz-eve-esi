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
unit esicharacter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, esibase, Generics.Collections;

type

  { TEVECharacterPublic }

  TEVECharacterPublic = class(TCollectionItem)
  private
    FAllianceID: integer;
    FAncestryId: integer;
    FBirthDay: string;
    FBloodlineID: integer;
    FCorporationID: integer;
    FDescription: string;
    FFactionID: integer;
    FGender: string;
    FName: string;
    FRaceID: integer;
    FSecuityStatus: double;
    FTitle: string;
  published
    property alliance_id: integer read FAllianceID write FAllianceID;
    property ancestry_id: integer read FAncestryId write FAncestryId;
    property birthday: string read FBirthDay write FBirthDay;
    property bloodline_id: integer read FBloodlineID write FBloodlineID;
    property corporation_id: integer read FCorporationID write FCorporationID;
    property description: string read FDescription write FDescription;
    property faction_id: integer read FFactionID write FFactionID;
    property gender: string read FGender write FGender;
    property &name: string read FName write FName;
    property race_id: integer read FRaceID write FRaceID;
    property security_status: double read FSecuityStatus write FSecuityStatus;
    property title: string read FTitle write FTitle;
  end;

  { TEVECharacterAgent }

  TEVECharacterAgent = class(TCollectionItem)
  private
    Fagent_id: integer;
    Fpoints_per_day: double;
    Fremainder_points: double;
    Fskill_type_id: integer;
    Fstarted_at: string;
  published
    property agent_id: integer read Fagent_id write Fagent_id;
    property points_per_day: double read Fpoints_per_day write Fpoints_per_day;
    property remainder_points: double read Fremainder_points write Fremainder_points;
    property skill_type_id: integer read Fskill_type_id write Fskill_type_id;
    property started_at: string read Fstarted_at write Fstarted_at;
  end;

  { TEVECharacterAgentList }

  TEVECharacterAgentList = class(TCollection)
  private
    function GetItems(Index: integer): TEVECharacterAgent;
    procedure SetItems(Index: integer; AValue: TEVECharacterAgent);
  public
    property Items[Index: integer]: TEVECharacterAgent read GetItems write SetItems;
  end;

  { TEVECharacterBlueprint }

  TEVECharacterBlueprint = class(TCollectionItem)
  private
    Fitem_id: uint64;
    Flocation_flag: string;
    Flocation_id: uint64;
    Fmaterial_efficiency: integer;
    Fquantity: integer;
    Fruns: integer;
    Ftime_efficiency: integer;
    Ftype_id: integer;
  published
    property item_id: uint64 read Fitem_id write Fitem_id;
    property location_flag: string read Flocation_flag write Flocation_flag;
    property location_id: uint64 read Flocation_id write Flocation_id;
    property material_efficiency: integer read Fmaterial_efficiency write Fmaterial_efficiency;
    property quantity: integer read Fquantity write Fquantity;
    property runs: integer read Fruns write Fruns;
    property time_efficiency: integer read Ftime_efficiency write Ftime_efficiency;
    property type_id: integer read Ftype_id write Ftype_id;
  end;


  { TEVECharacterBlueprintList }

  TEVECharacterBlueprintList = class(TCollection)
  private
    function GetItems(Index: integer): TEVECharacterBlueprint;
    procedure SetItems(Index: integer; AValue: TEVECharacterBlueprint);
  public
    property Items[Index: integer]: TEVECharacterBlueprint read GetItems write SetItems;
  end;

  { TEVECharacterCorporation }

  TEVECharacterCorporation = class(TCollectionItem)
  private
    Fcorporation_id: integer;
    Fis_deleted: boolean;
    Frecord_id: integer;
    Fstart_date: string;
  published
    property corporation_id: integer read Fcorporation_id write Fcorporation_id;
    property is_deleted: boolean read Fis_deleted write Fis_deleted;
    property record_id: integer read Frecord_id write Frecord_id;
    property start_date: string read Fstart_date write Fstart_date;
  end;

  { TEVECharacterCorporationList }

  TEVECharacterCorporationList = class(TCollection)
  private
    function GetItems(Index: integer): TEVECharacterCorporation;
    procedure SetItems(Index: integer; AValue: TEVECharacterCorporation);
  public
    property Items[Index: integer]: TEVECharacterCorporation read GetItems write SetItems;
  end;

  { TEVECharacterJumpFatigue }

  TEVECharacterJumpFatigue = class(TCollectionItem)
  private
    FJumpFatigueExpireDate: string;
    FLastJumpDate: string;
    FLastUpdateDate: string;
  published
    property jump_fatigue_expire_date: string read FJumpFatigueExpireDate write FJumpFatigueExpireDate;
    property last_jump_date: string read FLastJumpDate write FLastJumpDate;
    property last_update_date: string read FLastUpdateDate write FLastUpdateDate;
  end;

  { TEVECharactersMedalsGraphic }

  TEVECharactersMedalsGraphic = class(TCollectionItem)
  private
    Fcolor: integer;
    Fgraphic: string;
    Flayer: integer;
    Fpart: integer;
  published
    property color: integer read Fcolor write Fcolor;
    property graphic: string read Fgraphic write Fgraphic;
    property layer: integer read Flayer write Flayer;
    property part: integer read Fpart write Fpart;
  end;

  { TEVECharactersMedalsGraphicList }

  TEVECharactersMedalsGraphicList = class(TCollection)
  private
    function GetItems(Index: integer): TEVECharactersMedalsGraphic;
    procedure SetItems(Index: integer; AValue: TEVECharactersMedalsGraphic);
  public
    property Items[Index: integer]: TEVECharactersMedalsGraphic read GetItems write SetItems;
  end;

  { TCharactersCharacterMedals }

  TEVECharactersMedals = class(TCollectionItem)
  private
    Fcorporation_id: integer;
    Fdate: string;
    Fdescription: string;
    Fgraphics: TEVECharactersMedalsGraphicList;
    Fcolor: integer;
    Fgraphic: string;
    Flayer: integer;
    Fpart: integer;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
  published
    property corporation_id: integer read Fcorporation_id write Fcorporation_id;
    property date: string read Fdate write Fdate;
    property description: string read Fdescription write Fdescription;
    property &graphics: TEVECharactersMedalsGraphicList read Fgraphics write Fgraphics;
    property color: integer read Fcolor write Fcolor;
    property graphic: string read Fgraphic write Fgraphic;
    property layer: integer read Flayer write Flayer;
    property part: integer read Fpart write Fpart;
  end;

  { TEVECharactersMedalsList }

  TEVECharactersMedalsList = class(TCollection)
  private
    function GetItems(Index: integer): TEVECharactersMedals;
    procedure SetItems(Index: integer; AValue: TEVECharactersMedals);
  public
    property Items[Index: integer]: TEVECharactersMedals read GetItems write SetItems;
  end;

  { TEVECharacter }

  TEVECharacter = class(TEVEBase)
  public
    {Get character's public information.}
    function GetPublicInfo(ACharacterId: uint64): TEVECharacterPublic;
    {Get agent research list
     Free memory after use.}
    function GetAgentResearch(AAccessToken: string; ACharacterId: uint64): TEVECharacterAgentList;
    {Get blueprint list
     Free memory after use.}
    function GetBlueprints(AAccessToken: string; ACharacterId: uint64; APage: integer): TEVECharacterBlueprintList;
    {Get Corporation list
     Free memory after use.}
    function GetCorporationHistory(ACharacterId: uint64): TEVECharacterCorporationList;
    {Get Jump Fatigue
     Free memory after use.}
    function GetJumpFatigue(AAccessToken: string; ACharacterId: uint64): TEVECharacterJumpFatigue;
    {Get Characters Medals
     Free memory after use.}
    function GetCharactersMedals(AAccessToken: string; ACharacterId: uint64): TEVECharactersMedalsList;

  end;

implementation

{ TEVECharactersMedals }

constructor TEVECharactersMedals.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  Fgraphics := TEVECharactersMedalsGraphicList.Create(TEVECharactersMedalsGraphic);
end;

destructor TEVECharactersMedals.Destroy;
begin
  FreeAndNil(Fgraphics);
  inherited Destroy;
end;

{ TEVECharactersMedalsList }

function TEVECharactersMedalsList.GetItems(Index: integer): TEVECharactersMedals;
begin
  Result := TEVECharactersMedals(inherited Items[Index]);
end;

procedure TEVECharactersMedalsList.SetItems(Index: integer; AValue: TEVECharactersMedals);
begin
  Items[Index].Assign(AValue);
end;

{ TEVECharactersMedalsGraphicList }

function TEVECharactersMedalsGraphicList.GetItems(Index: integer): TEVECharactersMedalsGraphic;
begin
  Result := TEVECharactersMedalsGraphic(inherited Items[Index]);
end;

procedure TEVECharactersMedalsGraphicList.SetItems(Index: integer; AValue: TEVECharactersMedalsGraphic);
begin
  Items[Index].Assign(AValue);
end;

{ TEVECharacterCorporationList }

function TEVECharacterCorporationList.GetItems(Index: integer): TEVECharacterCorporation;
begin
  Result := TEVECharacterCorporation(inherited Items[Index]);
end;

procedure TEVECharacterCorporationList.SetItems(Index: integer; AValue: TEVECharacterCorporation);
begin
  Items[Index].Assign(AValue);
end;

{ TEVECharacterBlueprintList }

function TEVECharacterBlueprintList.GetItems(Index: integer): TEVECharacterBlueprint;
begin
  Result := TEVECharacterBlueprint(inherited Items[Index]);
end;

procedure TEVECharacterBlueprintList.SetItems(Index: integer; AValue: TEVECharacterBlueprint);
begin
  Items[Index].Assign(AValue);
end;

{ TEVECharacterAgentList }

function TEVECharacterAgentList.GetItems(Index: integer): TEVECharacterAgent;
begin
  Result := TEVECharacterAgent(inherited Items[Index]);
end;

procedure TEVECharacterAgentList.SetItems(Index: integer; AValue: TEVECharacterAgent);
begin
  Items[Index].Assign(AValue);
end;

{ TEVECharacter }

function TEVECharacter.GetPublicInfo(ACharacterId: uint64): TEVECharacterPublic;
const
  URL = 'https://esi.evetech.net/latest/characters/%s/?datasource=%s';
begin
  Result := TEVECharacterPublic.Create(nil);
  DeStreamerObject(Get(Format(URL, [ACharacterId.ToString, DataSource])), TObject(Result));
end;

function TEVECharacter.GetAgentResearch(AAccessToken: string; ACharacterId: uint64): TEVECharacterAgentList;
const
  URL = 'https://esi.evetech.net/latest/characters/%s/agents_research/?datasource=%s';
begin
  Result := TEVECharacterAgentList.Create(TEVECharacterAgent);
  DeStreamerArray(Get(AAccessToken, Format(URL, [ACharacterId.ToString, DataSource])), TCollection(Result));
end;

function TEVECharacter.GetBlueprints(AAccessToken: string; ACharacterId: uint64; APage: integer): TEVECharacterBlueprintList;
const
  URL = 'https://esi.evetech.net/latest/characters/%s/blueprints/?datasource=%s&page=%d';
begin
  Result := TEVECharacterBlueprintList.Create(TEVECharacterBlueprint);
  DeStreamerArray(Get(AAccessToken, Format(URL, [ACharacterId.ToString, DataSource, APage])), TCollection(Result));
end;

function TEVECharacter.GetCorporationHistory(ACharacterId: uint64): TEVECharacterCorporationList;
const
  URL = 'https://esi.evetech.net/latest/characters/%s/corporationhistory/?datasource=%s';
begin
  Result := TEVECharacterCorporationList.Create(TEVECharacterCorporation);
  DeStreamerArray(Get(Format(URL, [ACharacterId.ToString, DataSource])), TCollection(Result));
end;

function TEVECharacter.GetJumpFatigue(AAccessToken: string; ACharacterId: uint64): TEVECharacterJumpFatigue;
const
  URL = 'https://esi.evetech.net/latest/characters/%s/fatigue/?datasource=%s';
begin
  Result := TEVECharacterJumpFatigue.Create(nil);
  DeStreamerObject(Get(AAccessToken, Format(URL, [ACharacterId.ToString, DataSource])), TObject(Result));
end;

function TEVECharacter.GetCharactersMedals(AAccessToken: string; ACharacterId: uint64): TEVECharactersMedalsList;
const
  URL = 'https://esi.evetech.net/latest/characters/%s/medals/?datasource=%s';
begin
  Result := TEVECharactersMedalsList.Create(TEVECharactersMedals);
  DeStreamerArray(Get(AAccessToken, Format(URL, [ACharacterId.ToString, DataSource])), TCollection(Result));
end;

end.
