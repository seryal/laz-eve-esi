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
  Classes, SysUtils, fpjson, esibase;

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

  { TEVECharacter }

  TEVECharacter = class(TEVEBase)
  public
    function GetPublicInfo(ACharacterId: UInt64): TEVECharacterPublic;
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

end.
