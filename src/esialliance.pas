unit esialliance;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, esibase, variants;

type

  { TEVEAllianceAlliance }

  TEVEAllianceNum = class(TCollectionItem)
  private
    Falliance_id: integer;
  public
    property alliance_id: integer read Falliance_id write Falliance_id;
  end;

  { TEVEAllianceList }

  TEVEAllianceList = class(TCollection)
  private
    function GetItems(Index: integer): TEVEAllianceNum;
  public
    property Items[Index: integer]: TEVEAllianceNum read GetItems;
  end;

  { TEVEAllianceInfo }

  TEVEAllianceInfo = class(TCollectionItem)
  private
    Fcreator_corporation_id: integer;
    Fcreator_id: integer;
    Fdate_founded: string;
    Fexecutor_corporation_id: integer;
    Ffaction_id: integer;
    Fname: string;
    Fticker: string;
  published
    property creator_corporation_id: integer read Fcreator_corporation_id write Fcreator_corporation_id;
    property creator_id: integer read Fcreator_id write Fcreator_id;
    property date_founded: string read Fdate_founded write Fdate_founded;
    property executor_corporation_id: integer read Fexecutor_corporation_id write Fexecutor_corporation_id;
    property faction_id: integer read Ffaction_id write Ffaction_id;
    property &name: string read Fname write Fname;
    property ticker: string read Fticker write Fticker;
  end;

  { TEVEAllianceCorporation }

  TEVEAllianceCorporation = class(TCollectionItem)
  private
    Fcorporation_id: integer;
  public
    property corporation_id: integer read Fcorporation_id write Fcorporation_id;
  end;

  { TEVEAllianceCorporationList }

  TEVEAllianceCorporationList = class(TCollection)
  private
    function GetItems(Index: integer): TEVEAllianceCorporation;
  public
    property Items[Index: integer]: TEVEAllianceCorporation read GetItems;
  end;

  TEVEAllianceIcons = class(TCollectionItem)
  private
    Fpx128x128: string;
    Fpx64x64: string;
  published
    property px128x128: string read Fpx128x128 write Fpx128x128;
    property px64x64: string read Fpx64x64 write Fpx64x64;
  end;


  { TEVEAlliance }

  TEVEAlliance = class(TEVEBase)
  public
    {List all active player alliances}
    function GetAlliances: TEVEAllianceList;
    {Public information about an alliance}
    function GetInfo(AAllianceID: integer): TEVEAllianceInfo;
    {List all current member corporations of an alliance}
    function GetCorporations(AAllianceID: integer): TEVEAllianceCorporationList;
    {Get the icon urls for a alliance}
    function GetIcon(AAllianceID: integer): TEVEAllianceIcons;
  end;

implementation

{ TEVEAllianceCorporationList }

function TEVEAllianceCorporationList.GetItems(Index: integer): TEVEAllianceCorporation;
begin
  Result := TEVEAllianceCorporation(inherited Items[Index]);
end;

{ TEVEAllianceList }

function TEVEAllianceList.GetItems(Index: integer): TEVEAllianceNum;
begin
  Result := TEVEAllianceNum(inherited Items[Index]);
end;

{ TEVEAlliance }

function TEVEAlliance.GetAlliances: TEVEAllianceList;
const
  URL = 'https://esi.evetech.net/latest/alliances/?datasource=tranquility';
var
  V: variant;
  i: integer;
  id: integer;
  alliance: TEVEAllianceNum;
begin
  Result := TEVEAllianceList.Create(TEVEAllianceNum);
  DeStreamerArray(Get(Format(URL, [DataSource])), V);
  for I := VarArrayLowBound(V, 1) to VarArrayHighBound(V, 1) do
  begin
    case VarType(V[I]) of
      varInteger: id := V[I];
    end;
    alliance := TEVEAllianceNum(Result.Add);
    alliance.alliance_id := id;
  end;
end;

function TEVEAlliance.GetInfo(AAllianceID: integer): TEVEAllianceInfo;
const
  URL = 'https://esi.evetech.net/latest/alliances/%s/?datasource=%s';
begin
  Result := TEVEAllianceInfo.Create(nil);
  DeStreamerObject(Get(Format(URL, [AAllianceID.ToString, DataSource])), TObject(Result));
end;

function TEVEAlliance.GetCorporations(AAllianceID: integer): TEVEAllianceCorporationList;
const
  URL = 'https://esi.evetech.net/latest/alliances/%s/corporations/?datasource=%s';
var
  V: variant;
  i: integer;
  id: integer;
  alliance: TEVEAllianceCorporation;
begin
  Result := TEVEAllianceCorporationList.Create(TEVEAllianceCorporation);
  DeStreamerArray(Get(Format(URL, [AAllianceID.ToString, DataSource])), V);
  for I := VarArrayLowBound(V, 1) to VarArrayHighBound(V, 1) do
  begin
    case VarType(V[I]) of
      varInteger: id := V[I];
    end;
    alliance := TEVEAllianceCorporation(Result.Add);
    alliance.corporation_id := id;
  end;
end;

function TEVEAlliance.GetIcon(AAllianceID: integer): TEVEAllianceIcons;
const
  URL = 'https://esi.evetech.net/latest/alliances/%s/icons/?datasource=%s';
begin
  Result := TEVEAllianceIcons.Create(nil);
  DeStreamerObject(Get(Format(URL, [AAllianceID.ToString, DataSource])), TObject(Result));
end;

end.
