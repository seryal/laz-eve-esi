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


  { TEVECharacter }

  TEVECharacter = class(TEVEBase)
  public
    {Get character's public information.}
    function GetPublicInfo(ACharacterId: UInt64): TEVECharacterPublic;
    {Get agent research list
     Free memory after use.}
    function GetAgentResearch(AAccessToken: string; ACharacterId: uint64): TEVECharacterAgentList;
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

end.
