unit esicontacts;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, esibase, Generics.Collections, typinfo, fpjson;

type
  TEVEContactLabelID = specialize TList<longint>;

  { TEVEContactAlliance }

  TEVEContactAlliance = class(TCollectionItem)
  private
    Fcontact_id: integer;
    Fcontact_type: string;
    Flabel_ids: TEVEContactLabelID;
    Fstanding: double;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
  published
    property contact_id: integer read Fcontact_id write Fcontact_id;
    property contact_type: string read Fcontact_type write Fcontact_type;
    property label_ids: TEVEContactLabelID read Flabel_ids write Flabel_ids;
    property standing: double read Fstanding write Fstanding;
  end;

  { TEVEContactAllianceList }

  TEVEContactAllianceList = class(TCollection)
  private
    function GetItems(Index: integer): TEVEContactAlliance;
  public
    property Items[Index: integer]: TEVEContactAlliance read GetItems;
  end;

  { TEVEContactCharacter }

  TEVEContactCharacter = class(TEVEContactAlliance)
  private
    Fis_blocked: boolean;
    Fis_watched: boolean;
  published
    property is_blocked: boolean read Fis_blocked write Fis_blocked;
    property is_watched: boolean read Fis_watched write Fis_watched;
  end;

  { TEVEContactCharacterList }

  TEVEContactCharacterList = class(TCollection)
  private
    function GetItems(Index: integer): TEVEContactCharacter;
  public
    property Items[Index: integer]: TEVEContactCharacter read GetItems;
  end;

  TEVEContactCorporation = class(TEVEContactCharacter);

  { TEVEContactCorporationList }

  TEVEContactCorporationList = class(TCollection)
  private
    function GetItems(Index: integer): TEVEContactCorporation;
  public
    property Items[Index: integer]: TEVEContactCorporation read GetItems;
  end;

  { TESIContacts }

  TESIContacts = class(TESIBase)
  private
    Fproperty_name: string;
  protected
    procedure RestorePropertyNotify(Sender: TObject; AObject: TObject; Info: PPropInfo; AValue: TJSONData; var Handled: boolean); override;
  public
    function GetAllianceContacts(AAccessToken: string; AAllianceID, APage: integer): TEVEContactAllianceList;
    //function GetAllianceContactLabels(AAccessToken: string; AAllianceID: integer): string;
    //function DeleteCharacterContact(AAccessToken: string; AChatacterID: integer): string;
    function GetCharacterContacts(AAccessToken: string; ACharacterID, APage: integer): TEVEContactCharacterList;
    //function AddCharacterContact(AAccessToken: string; ACharacterID: integer): string;
    //function GetCharacterContactLabels(AAccessToken: string; ACharacterID: integer): string;
    function GetCorporationContacts(AAccessToken: string; ACorporationID, APage: integer): TEVEContactCorporationList;
    //function GetCorporationContactLabels(AAccessToken: string; ACorporationID: integer): string;
  end;

implementation

{ TEVEContactCorporationList }

function TEVEContactCorporationList.GetItems(Index: integer): TEVEContactCorporation;
begin
  Result := TEVEContactCorporation(inherited Items[Index]);
end;

{ TEVEContactAlliance }

constructor TEVEContactAlliance.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  Flabel_ids := TEVEContactLabelID.Create;
end;

destructor TEVEContactAlliance.Destroy;
begin
  FreeAndNil(Flabel_ids);
  inherited Destroy;
end;

{ TEVEContactCharacterList }

function TEVEContactCharacterList.GetItems(Index: integer): TEVEContactCharacter;
begin
  Result := TEVEContactCharacter(inherited Items[Index]);
end;

{ TESIContacts }

procedure TESIContacts.RestorePropertyNotify(Sender: TObject; AObject: TObject; Info: PPropInfo; AValue: TJSONData; var Handled: boolean);
var
  i: integer;
begin
  if Info^.Name = Fproperty_name then
    for i := 0 to TJSONArray(AValue).Count - 1 do
      TEVEContactCharacter(AObject).label_ids.Add(TJSONArray(AValue).Items[i].AsInteger)
  else
    inherited RestorePropertyNotify(Sender, AObject, Info, AValue, Handled);
end;

function TESIContacts.GetAllianceContacts(AAccessToken: string; AAllianceID, APage: integer): TEVEContactAllianceList;
const
  URL = 'https://esi.evetech.net/latest/alliances/%s/contacts/?datasource=%s&page=%d';
begin
  Fproperty_name := 'label_ids';
  Result := TEVEContactAllianceList.Create(TEVEContactAlliance);
  DeStreamerArray(Get(AAccessToken, Format(URL, [AAllianceID.ToString, DataSource, APage])), TCollection(Result));
  Fproperty_name := '';
end;

function TESIContacts.GetCharacterContacts(AAccessToken: string; ACharacterID, APage: integer): TEVEContactCharacterList;
const
  URL = 'https://esi.evetech.net/latest/characters/%s/contacts/?datasource=%s&page=%d';
begin
  Fproperty_name := 'label_ids';
  Result := TEVEContactCharacterList.Create(TEVEContactCharacter);
  DeStreamerArray(Get(AAccessToken, Format(URL, [ACharacterId.ToString, DataSource, APage])), TCollection(Result));
  Fproperty_name := '';
end;

function TESIContacts.GetCorporationContacts(AAccessToken: string; ACorporationID, APage: integer): TEVEContactCorporationList;
const
  URL = 'https://esi.evetech.net/latest/corporations/%s/contacts/?datasource=%s&page=%d';
begin
  Fproperty_name := 'label_ids';
  Result := TEVEContactCorporationList.Create(TEVEContactCorporation);
  DeStreamerArray(Get(AAccessToken, Format(URL, [ACorporationID.ToString, DataSource, APage])), TCollection(Result));
  Fproperty_name := '';
end;

{ TEVEContactAllianceList }

function TEVEContactAllianceList.GetItems(Index: integer): TEVEContactAlliance;
begin
  Result := TEVEContactAlliance(inherited Items[Index]);
end;

end.
