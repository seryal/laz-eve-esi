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
  public
    property Items[Index: integer]: TEVECharacterAgent read GetItems;
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
  public
    property Items[Index: integer]: TEVECharacterBlueprint read GetItems;
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
  public
    property Items[Index: integer]: TEVECharacterCorporation read GetItems;
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

  { TEVECharacterMedalGraphic }

  TEVECharacterMedalGraphic = class(TCollectionItem)
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

  { TEVECharacterMedalGraphicList }

  TEVECharacterMedalGraphicList = class(TCollection)
  private
    function GetItems(Index: integer): TEVECharacterMedalGraphic;
  public
    property Items[Index: integer]: TEVECharacterMedalGraphic read GetItems;
  end;

  { TCharactersCharacterMedals }

  TEVECharacterMedal = class(TCollectionItem)
  private
    Fcorporation_id: integer;
    Fdate: string;
    Fdescription: string;
    Fgraphics: TEVECharacterMedalGraphicList;
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
    property &graphics: TEVECharacterMedalGraphicList read Fgraphics write Fgraphics;
    property color: integer read Fcolor write Fcolor;
    property graphic: string read Fgraphic write Fgraphic;
    property layer: integer read Flayer write Flayer;
    property part: integer read Fpart write Fpart;
  end;

  { TEVECharacterMedalsList }

  TEVECharacterMedalsList = class(TCollection)
  private
    function GetItems(Index: integer): TEVECharacterMedal;
  public
    property Items[Index: integer]: TEVECharacterMedal read GetItems;
  end;

  { TCharactersNotifications }

  TEVECharacterNotification = class(TCollectionItem)
  private
    Fis_read: boolean;
    Fnotification_id: uint64;
    Fsender_id: integer;
    Fsender_type: string;
    Ftext: string;
    Ftimestamp: string;
    Ftype: string;
  published
    property is_read: boolean read Fis_read write Fis_read;
    property notification_id: uint64 read Fnotification_id write Fnotification_id;
    property sender_id: integer read Fsender_id write Fsender_id;
    property sender_type: string read Fsender_type write Fsender_type;
    property &text: string read Ftext write Ftext;
    property timestamp: string read Ftimestamp write Ftimestamp;
    property &type: string read Ftype write Ftype;
  end;

  { TEVECharacterNotificationList }

  TEVECharacterNotificationList = class(TCollection)
  private
    function GetItems(Index: integer): TEVECharacterNotification;
  public
    property Items[Index: integer]: TEVECharacterNotification read GetItems;
  end;

  { TEVECharacterContact }

  TEVECharacterContact = class(TCollectionItem)
  private
    Fmessage: string;
    Fnotification_id: integer;
    Fsend_date: string;
    Fsender_character_id: integer;
    Fstanding_level: double;
  published
    property message: string read Fmessage write Fmessage;
    property notification_id: integer read Fnotification_id write Fnotification_id;
    property send_date: string read Fsend_date write Fsend_date;
    property sender_character_id: integer read Fsender_character_id write Fsender_character_id;
    property standing_level: double read Fstanding_level write Fstanding_level;
  end;

  { TEVECharacterContactList }

  TEVECharacterContactList = class(TCollection)
  private
    function GetItems(Index: integer): TEVECharacterContact;
  public
    property Items[Index: integer]: TEVECharacterContact read GetItems;
  end;

  { TEVECharacterPortrait }

  TEVECharacterPortrait = class(TCollectionItem)
  private
    Fpx128x128: string;
    Fpx256x256: string;
    Fpx512x512: string;
    Fpx64x64: string;
  published
    property px128x128: string read Fpx128x128 write Fpx128x128;
    property px256x256: string read Fpx256x256 write Fpx256x256;
    property px512x512: string read Fpx512x512 write Fpx512x512;
    property px64x64: string read Fpx64x64 write Fpx64x64;
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
    {Get Character Medals
     Free memory after use.}
    function GetMedals(AAccessToken: string; ACharacterId: uint64): TEVECharacterMedalsList;
    {Get Character Notification
     Free memory after use.}
    function GetNotifications(AAccessToken: string; ACharacterId: uint64): TEVECharacterNotificationList;
    {Get Character Notification
     Free memory after use.}
    function GetContacts(AAccessToken: string; ACharacterId: uint64): TEVECharacterContactList;
    {Get Character Notification
     Free memory after use.}
    function GetPortrait(ACharacterId: uint64): TEVECharacterPortrait;

  end;

implementation

{ TEVECharacterContactList }

function TEVECharacterContactList.GetItems(Index: integer): TEVECharacterContact;
begin
  Result := TEVECharacterContact(inherited Items[Index]);
end;

{ TEVECharacterNotificationList }

function TEVECharacterNotificationList.GetItems(Index: integer): TEVECharacterNotification;
begin
  Result := TEVECharacterNotification(inherited Items[Index]);
end;

{ TEVECharacterMedal }

constructor TEVECharacterMedal.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  Fgraphics := TEVECharacterMedalGraphicList.Create(TEVECharacterMedalGraphic);
end;

destructor TEVECharacterMedal.Destroy;
begin
  FreeAndNil(Fgraphics);
  inherited Destroy;
end;

{ TEVECharacterMedalsList }

function TEVECharacterMedalsList.GetItems(Index: integer): TEVECharacterMedal;
begin
  Result := TEVECharacterMedal(inherited Items[Index]);
end;

{ TEVECharacterMedalGraphicList }

function TEVECharacterMedalGraphicList.GetItems(Index: integer): TEVECharacterMedalGraphic;
begin
  Result := TEVECharacterMedalGraphic(inherited Items[Index]);
end;

{ TEVECharacterCorporationList }

function TEVECharacterCorporationList.GetItems(Index: integer): TEVECharacterCorporation;
begin
  Result := TEVECharacterCorporation(inherited Items[Index]);
end;

{ TEVECharacterBlueprintList }

function TEVECharacterBlueprintList.GetItems(Index: integer): TEVECharacterBlueprint;
begin
  Result := TEVECharacterBlueprint(inherited Items[Index]);
end;

{ TEVECharacterAgentList }

function TEVECharacterAgentList.GetItems(Index: integer): TEVECharacterAgent;
begin
  Result := TEVECharacterAgent(inherited Items[Index]);
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

function TEVECharacter.GetMedals(AAccessToken: string; ACharacterId: uint64): TEVECharacterMedalsList;
const
  URL = 'https://esi.evetech.net/latest/characters/%s/medals/?datasource=%s';
begin
  Result := TEVECharacterMedalsList.Create(TEVECharacterMedal);
  DeStreamerArray(Get(AAccessToken, Format(URL, [ACharacterId.ToString, DataSource])), TCollection(Result));
end;

function TEVECharacter.GetNotifications(AAccessToken: string; ACharacterId: uint64): TEVECharacterNotificationList;
const
  URL = 'https://esi.evetech.net/latest/characters/%s/notifications/?datasource=%s';
begin
  Result := TEVECharacterNotificationList.Create(TEVECharacterNotification);
  DeStreamerArray(Get(AAccessToken, Format(URL, [ACharacterId.ToString, DataSource])), TCollection(Result));
end;

function TEVECharacter.GetContacts(AAccessToken: string; ACharacterId: uint64): TEVECharacterContactList;
const
  URL = 'https://esi.evetech.net/latest/characters/%s/notifications/contacts/?datasource=%s';
begin
  Result := TEVECharacterContactList.Create(TEVECharacterContact);
  DeStreamerArray(Get(AAccessToken, Format(URL, [ACharacterId.ToString, DataSource])), TCollection(Result));
end;

function TEVECharacter.GetPortrait(ACharacterId: uint64): TEVECharacterPortrait;
const
  URL = 'https://esi.evetech.net/latest/characters/%s/portrait/?datasource=%s';
begin
  Result := TEVECharacterPortrait.Create(nil);
  DeStreamerObject(Get(Format(URL, [ACharacterId.ToString, DataSource])), TObject(Result));
end;

end.
