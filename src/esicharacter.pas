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
  TEVECharacterPublic = record
    AncestryId: integer;
    BirthDay: string;
    BloodlineID: integer;
    CorporationID: UInt64;
    Description: string;
    Gender: string;
    Name: string;
    RaceID: integer;
    SecuityStatus: double;
  end;

  TEVECharacterAgent = record
    AgentID: integer;
    PointsPerDay: double;
    RemainderPoints: double;
    SkillTypeID: integer;
    StartedAt: string;
  end;

  TEVECharacterAgentList = specialize TList<TEVECharacterAgent>;

  TEVECharacterBlueprint = record
    ItemID: UInt64;
    LocationFlag: string;
    LocationID: UInt64;
    MaterialEfficiency: integer;
    Quantity: integer;
    Runs: integer;
    TimeEfficiency: integer;
    TypeID: integer;
  end;

  TEVECharacterBlueprintList = specialize TList<TEVECharacterBlueprint>;

  TEVECharacterCorporation = record
    CorporationID: integer;
    IsDeleted: boolean;
    RecordID: integer;
    StartDate: string;
  end;

  TEVECharacterCorporationList = specialize TList<TEVECharacterCorporation>;

  TEVECharacterJumpFatigue = record
    JumpFatigueExpireDate: string;
    LastJumpDate: string;
    LastUpdateDate: string;
  end;

  { TEVECharacter }

  TEVECharacter = class(TEVEBase)
  public
    {Get character's public information.}
    function GetPublicInfo(ACharacterId: UInt64): TEVECharacterPublic;
    {Get agent research list
     Free memory after use.}
    function GetAgentResearch(AAccessToken: string; ACharacterId: uint64): TEVECharacterAgentList;
    {Get blueprint list
     Free memory after use.}
    function GetBlueprints(AAccessToken: string; ACharacterId: uint64): TEVECharacterBlueprintList;
    {Get Corporation list
     Free memory after use.}
    function GetCorporationHistory(ACharacterId: uint64): TEVECharacterCorporationList;
    {Get Jump Fatigue}
    function GetJumpFatigue(AAccessToken: string; ACharacterId: uint64): TEVECharacterJumpFatigue;
  end;

implementation

{ TEVECharacter }

function TEVECharacter.GetPublicInfo(ACharacterId: UInt64): TEVECharacterPublic;
const
  URL = 'https://esi.evetech.net/latest/characters/%s/?datasource=%s';
var
  req_url: string;
  res: string;
  jData: TJSONData;
begin
  req_url := Format(URL, [ACharacterId.ToString, DataSource]);
  res := Get(req_url);
  try
    jData := GetJSON(res);
    Result.AncestryId := TJSONObject(jData).Get('ancestry_id');
    Result.BirthDay := TJSONObject(jData).Get('birthday');
    Result.BloodlineID := TJSONObject(jData).Get('bloodline_id');
    Result.CorporationID := TJSONObject(jData).Get('corporation_id');
    Result.Description := TJSONObject(jData).Get('description');
    Result.Gender := TJSONObject(jData).Get('gender');
    Result.Name := TJSONObject(jData).Get('name');
    Result.RaceID := TJSONObject(jData).Get('race_id');
    Result.SecuityStatus := TJSONObject(jData).Get('security_status');
  finally
    FreeAndNil(jData);
  end;
end;

function TEVECharacter.GetAgentResearch(AAccessToken: string; ACharacterId: uint64): TEVECharacterAgentList;
const
  URL = 'https://esi.evetech.net/latest/characters/%s/agents_research/?datasource=%s';
var
  req_url: string;
  res: string;
  jData: TJSONData;
  agent: TEVECharacterAgent;
  i: integer;
begin
  req_url := Format(URL, [ACharacterId.ToString, DataSource]);
  res := Get(AAccessToken, req_url);
  try
    jData := GetJSON(res);
    Result := TEVECharacterAgentList.Create;
    for i := 0 to TJSONArray(jData).Count - 1 do
    begin
      agent.AgentID := TJSONObject(TJSONArray(jData).Items[i]).Get('agent_id');
      agent.PointsPerDay := TJSONObject(TJSONArray(jData).Items[i]).Get('points_per_day');
      agent.RemainderPoints := TJSONObject(TJSONArray(jData).Items[i]).Get('remainder_points');
      agent.SkillTypeID := TJSONObject(TJSONArray(jData).Items[i]).Get('skill_type_id');
      agent.StartedAt := TJSONObject(TJSONArray(jData).Items[i]).Get('started_at');
      Result.Add(agent);
    end;
  finally
    FreeAndNil(jData);
  end;
end;

function TEVECharacter.GetBlueprints(AAccessToken: string;
  ACharacterId: uint64): TEVECharacterBlueprintList;
const
  URL = 'https://esi.evetech.net/latest/characters/%s/blueprints/?datasource=%s&page=%d';
var
  req_url: string;
  res: string;
  jData: TJSONData;
  blueprint: TEVECharacterBlueprint;
  i: integer;
  page: integer;
begin
  page := 1;
  Result := TEVECharacterBlueprintList.Create;
  while True do
  begin
    req_url := Format(URL, [ACharacterId.ToString, DataSource, page]);
    res := Get(AAccessToken, req_url);
    try
      jData := GetJSON(res);
      if TJSONArray(jData).Count = 0 then
        exit;
      for i := 0 to TJSONArray(jData).Count - 1 do
      begin
        blueprint.ItemID := TJSONObject(TJSONArray(jData).Items[i]).Get('item_id');
        blueprint.LocationFlag := TJSONObject(TJSONArray(jData).Items[i]).Get('location_flag');
        blueprint.LocationID := TJSONObject(TJSONArray(jData).Items[i]).Get('location_id');
        blueprint.MaterialEfficiency := TJSONObject(TJSONArray(jData).Items[i]).Get('material_efficiency');
        blueprint.Quantity := TJSONObject(TJSONArray(jData).Items[i]).Get('quantity');
        blueprint.Runs := TJSONObject(TJSONArray(jData).Items[i]).Get('runs');
        blueprint.TimeEfficiency := TJSONObject(TJSONArray(jData).Items[i]).Get('time_efficiency');
        blueprint.TypeID := TJSONObject(TJSONArray(jData).Items[i]).Get('type_id');
        Result.Add(blueprint);
      end;
    finally
      FreeAndNil(jData);
    end;
    Inc(page);
  end;
end;

function TEVECharacter.GetCorporationHistory(ACharacterId: uint64): TEVECharacterCorporationList;
const
  URL = 'https://esi.evetech.net/latest/characters/%s/corporationhistory/?datasource=%s';
var
  req_url: string;
  res: string;
  jData: TJSONData;
  corporation: TEVECharacterCorporation;
  i: integer;
begin
  req_url := Format(URL, [ACharacterId.ToString, DataSource]);
  res := Get(req_url);
  try
    Result := TEVECharacterCorporationList.Create;
    jData := GetJSON(res);
    for i := 0 to TJSONArray(jData).Count - 1 do
    begin
      corporation.CorporationID := TJSONObject(TJSONArray(jData).Items[i]).Get('corporation_id');
      corporation.IsDeleted := TJSONObject(TJSONArray(jData).Items[i]).Get('is_deleted', False);
      corporation.RecordID := TJSONObject(TJSONArray(jData).Items[i]).Get('record_id');
      corporation.StartDate := TJSONObject(TJSONArray(jData).Items[i]).Get('start_date');
      Result.Add(corporation);
    end;
  finally
    FreeAndNil(jData);
  end;
end;

function TEVECharacter.GetJumpFatigue(AAccessToken: string; ACharacterId: uint64): TEVECharacterJumpFatigue;
const
  URL = 'https://esi.evetech.net/latest/characters/%s/fatigue/?datasource=%s';
var
  req_url: string;
  res: string;
  jData: TJSONData;
  i: integer;
begin
  req_url := Format(URL, [ACharacterId.ToString, DataSource]);
  res := Get(AAccessToken, req_url);
  try
    jData := GetJSON(res);
    Result.JumpFatigueExpireDate := TJSONObject(jData).Get('jump_fatigue_expire_date', '');
    Result.LastJumpDate := TJSONObject(jData).Get('last_jump_date', '');
    Result.LastUpdateDate := TJSONObject(jData).Get('last_update_date', '');
  finally
    FreeAndNil(jData);
  end;
end;

end.
